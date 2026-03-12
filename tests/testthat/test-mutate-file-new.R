test_that("mutate_file creates mutations", {
  # Create a temporary R script for testing
  temp_file <- tempfile(fileext = ".R")
  mutation_dir <- tempfile("mutations_")
  dir.create(mutation_dir)
  on.exit(unlink(c(temp_file, mutation_dir), recursive = TRUE), add = TRUE)

  writeLines("square <- function(x) {
    return(x * x)
  }

  add <- function(a, b) {
    return(a + b)
  }", temp_file)

  # Run mutation
  mutated_files <- mutate_file(temp_file, out_dir = mutation_dir)

  # Check that mutations were created
  expect_true(is.list(mutated_files))
  expect_true(length(mutated_files) > 0)

  # Check that mutation files exist
  for (mutant in mutated_files) {
    expect_true(file.exists(mutant$path))
    expect_true(!is.null(mutant$info))
  }
})

test_that("mutate_file handles empty files", {
  # Create an empty R script
  temp_file <- tempfile(fileext = ".R")
  mutation_dir <- tempfile("mutations_")
  dir.create(mutation_dir)
  on.exit(unlink(c(temp_file, mutation_dir), recursive = TRUE), add = TRUE)

  writeLines("", temp_file)

  # Run mutation
  mutated_files <- expect_warning(
    mutate_file(temp_file, out_dir = mutation_dir),
    "No valid lines to delete"
  )

  # Only string-level deletion mutations should be attempted
  expect_true(is.list(mutated_files))
})

test_that("mutate_file honors max_mutants cap", {
  temp_file <- tempfile(fileext = ".R")
  out_dir_all <- tempfile("mutations_all_")
  out_dir_limited <- tempfile("mutations_limited_")
  out_dir_zero <- tempfile("mutations_zero_")
  dir.create(out_dir_all)
  dir.create(out_dir_limited)
  dir.create(out_dir_zero)
  on.exit(unlink(c(temp_file, out_dir_all, out_dir_limited, out_dir_zero), recursive = TRUE), add = TRUE)

  writeLines("f <- function(x) {
  if (x < 0) {
    return(-x)
  }
  x + 1
}", temp_file)

  all_mutants <- mutate_file(temp_file, out_dir = out_dir_all)
  expect_true(length(all_mutants) > 0)

  limited_mutants <- mutate_file(temp_file, out_dir = out_dir_limited, max_mutants = 2)
  expect_length(limited_mutants, min(2, length(all_mutants)))

  uncapped_mutants <- mutate_file(temp_file, out_dir = out_dir_limited, max_mutants = 999)
  expect_length(uncapped_mutants, length(all_mutants))

  zero_mutants <- mutate_file(temp_file, out_dir = out_dir_zero, max_mutants = 0)
  expect_equal(zero_mutants, list())
})
