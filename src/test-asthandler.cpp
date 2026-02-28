#include <testthat.h>
#include <R.h>
#include <Rinternals.h>
#include "ASTHandler.hpp"

context("ASTHandler C++ tests")
{

    test_that("gatherOperators runs on a basic call")
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

        expect_true(ops.size() >= 0);

        UNPROTECT(2);
    }
}
