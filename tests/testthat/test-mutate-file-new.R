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
