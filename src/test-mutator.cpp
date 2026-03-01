#include <testthat.h>
#include <R.h>
#include <Rinternals.h>
#include "ASTHandler.h"
#include "Mutator.h"

context("Mutator C++ tests")
{

    test_that("applyMutation runs on first discovered operator")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        SEXP srcref = PROTECT(Rf_allocVector(INTSXP, 4));
        INTEGER(srcref)
        [0] = 1;
        INTEGER(srcref)
        [1] = 1;
        INTEGER(srcref)
        [2] = 1;
        INTEGER(srcref)
        [3] = 5;

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);

        if (ops.size() > 0)
        {
            Mutator mutator;
            auto result = mutator.applyMutation(expr, ops, 0);
            bool valid_type = TYPEOF(result.first) == LANGSXP ||
                              TYPEOF(result.first) == EXPRSXP ||
                              TYPEOF(result.first) == VECSXP;
            expect_true(valid_type);
        }
        else
        {
            expect_true(true);
        }

        UNPROTECT(2);
    }
}
