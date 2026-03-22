// ASTHandler.cpp

#include <map>
#include <functional>
#include <iostream>
#include <Rversion.h>
#include "ASTHandler.h"
#include "PlusOperator.h"
#include "MinusOperator.h"
#include "DivideOperator.h"
#include "MultiplyOperator.h"
#include "EqualOperator.h"
#include "NotEqualOperator.h"
#include "LessThanOperator.h"
#include "MoreThanOperator.h"
#include "LessThanOrEqualOperator.h"
#include "MoreThanOrEqualOperator.h"
#include "AndOperator.h"
#include "OrOperator.h"
#include "LogicalOrOperator.h"
#include "LogicalAndOperator.h"
#include "DeleteOperator.h"

static struct CachedSyms
{
    SEXP s_lbrace = Rf_install("{");
    SEXP s_rbrace = Rf_install("}");
    SEXP s_filename = Rf_install("filename");
    SEXP s_plus = Rf_install("+");
    SEXP s_minus = Rf_install("-");
    SEXP s_mul = Rf_install("*");
    SEXP s_div = Rf_install("/");
    SEXP s_eq = Rf_install("==");
    SEXP s_neq = Rf_install("!=");
    SEXP s_lt = Rf_install("<");
    SEXP s_gt = Rf_install(">");
    SEXP s_le = Rf_install("<=");
    SEXP s_ge = Rf_install(">=");
    SEXP s_and = Rf_install("&");
    SEXP s_or = Rf_install("|");
    SEXP s_land = Rf_install("&&");
    SEXP s_lor = Rf_install("||");
    SEXP s_srcref = Rf_install("srcref");
    SEXP s_mutinfo = Rf_install("mutation_info");
} SYM;

static SEXP getVarFromFrame(SEXP env, SEXP name)
{
#if R_VERSION >= R_Version(4, 5, 0)
    return R_getVar(name, env, FALSE);
#else
    return Rf_findVarInFrame(env, name);
#endif
}

static bool extractSrcrefBounds(SEXP srcref, int &start_line, int &start_col, int &end_line, int &end_col)
{
    if (TYPEOF(srcref) != INTSXP || LENGTH(srcref) < 4)
        return false;
    const int *p = INTEGER(srcref);
    start_line = p[0];
    start_col = p[1];
    end_line = p[2];
    end_col = p[3];
    return true;
}

bool ASTHandler::isDeletable(SEXP expr)
{
    if (!_is_inside_block)
        return false;
    if (TYPEOF(expr) != LANGSXP)
        return true;

    SEXP head = CAR(expr);
    if (TYPEOF(head) == SYMSXP && (head == SYM.s_lbrace || head == SYM.s_rbrace))
        return false;
    return true;
}

std::vector<OperatorPos> ASTHandler::gatherOperators(SEXP expr, SEXP src_ref,
                                                     bool is_inside_block)
{
    if (TYPEOF(src_ref) != INTSXP || LENGTH(src_ref) < 4)
        Rf_error("src_ref must be an integer vector of length 4");

    const int *p = INTEGER(src_ref);
    _start_line = p[0];
    _start_col = p[1];
    _end_line = p[2];
    _end_col = p[3];
    _file_path.clear();

    SEXP srcfile = Rf_getAttrib(src_ref, Rf_install("srcfile"));
    if (srcfile != R_NilValue)
    {
        SEXP filename = Rf_getAttrib(srcfile, SYM.s_filename);
        if (TYPEOF(filename) == STRSXP && LENGTH(filename) > 0)
        {
            _file_path = CHAR(STRING_ELT(filename, 0));
        }
        else if (TYPEOF(srcfile) == ENVSXP)
        {
            SEXP env_name = getVarFromFrame(srcfile, SYM.s_filename);
            if (env_name != R_UnboundValue && TYPEOF(env_name) == STRSXP && LENGTH(env_name) > 0)
            {
                _file_path = CHAR(STRING_ELT(env_name, 0));
            }
        }
    }

    _is_inside_block = is_inside_block;

    std::vector<OperatorPos> ops;
    std::vector<int> path;
    gatherOperatorsRecursive(expr, path, ops);
    return ops;
}

void ASTHandler::gatherOperatorsRecursive(SEXP expr, std::vector<int> path,
                                          std::vector<OperatorPos> &ops)
{
    if (TYPEOF(expr) != LANGSXP)
        return;

    int node_start_line = _start_line;
    int node_start_col = _start_col;
    int node_end_line = _end_line;
    int node_end_col = _end_col;

    SEXP node_srcref = Rf_getAttrib(expr, SYM.s_srcref);
    extractSrcrefBounds(node_srcref,
                        node_start_line,
                        node_start_col,
                        node_end_line,
                        node_end_col);

    SEXP fun = CAR(expr);

    /* operator map – keys are the cached symbols */
    static const std::map<SEXP, std::function<std::unique_ptr<Operator>()>> op_map = {
        {SYM.s_plus, []
         { return std::make_unique<PlusOperator>(); }},
        {SYM.s_minus, []
         { return std::make_unique<MinusOperator>(); }},
        {SYM.s_mul, []
         { return std::make_unique<MultiplyOperator>(); }},
        {SYM.s_div, []
         { return std::make_unique<DivideOperator>(); }},
        {SYM.s_eq, []
         { return std::make_unique<EqualOperator>(); }},
        {SYM.s_neq, []
         { return std::make_unique<NotEqualOperator>(); }},
        {SYM.s_lt, []
         { return std::make_unique<LessThanOperator>(); }},
        {SYM.s_gt, []
         { return std::make_unique<MoreThanOperator>(); }},
        {SYM.s_le, []
         { return std::make_unique<LessThanOrEqualOperator>(); }},
        {SYM.s_ge, []
         { return std::make_unique<MoreThanOrEqualOperator>(); }},
        {SYM.s_and, []
         { return std::make_unique<AndOperator>(); }},
        {SYM.s_or, []
         { return std::make_unique<OrOperator>(); }},
        {SYM.s_land, []
         { return std::make_unique<LogicalAndOperator>(); }},
        {SYM.s_lor, []
         { return std::make_unique<LogicalOrOperator>(); }}};

    if (auto it = op_map.find(fun); it != op_map.end())
    {
        auto op = it->second();
        ops.push_back({path, std::move(op), node_start_line, node_start_col,
                       node_end_line, node_end_col, fun, _file_path});
    }

    // add delete operator if allowed
    if (isDeletable(expr))
    {
        auto del = std::make_unique<DeleteOperator>(expr);
        ops.push_back({path, std::move(del), node_start_line, node_start_col,
                       node_end_line, node_end_col, fun, _file_path});
    }

    // recurse into children (block or not)
    int idx = 0;
    for (SEXP next = CDR(expr); next != R_NilValue; next = CDR(next), ++idx)
    {
        auto child_path = path;
        child_path.push_back(idx);
        gatherOperatorsRecursive(CAR(next), child_path, ops);
    }
}
