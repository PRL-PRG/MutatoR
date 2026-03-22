#ifndef MUTATION_MODEL_H
#define MUTATION_MODEL_H

#include <R.h>
#include <Rinternals.h>

#include <string>
#include <vector>

enum class NodeShape
{
    Call,
    Symbol,
    Constant,
    Pairlist,
    Other
};

enum class CallArity
{
    None,
    Unary,
    Binary,
    Variadic
};

enum class SemanticKind
{
    ArithmeticCall,
    ComparisonCall,
    LogicalCall,
    BlockCall,
    ControlCall,
    NumericLiteral,
    LogicalLiteral,
    StringLiteral,
    SymbolReference,
    Unknown
};

enum class StructuralRole
{
    Root,
    CallHead,
    CallArgument,
    BlockChild,
    Condition,
    Other
};

enum class MutationFamily
{
    OperatorReplacement,
    ValueMutation,
    DecisionMutation,
    StatementMutation
};

enum class MutationAction
{
    ReplaceCallHeadSymbol,
    ReplaceNode,
    DeleteChild,
    SwapChildren
};

struct NodeOccurrence
{
    std::vector<int> path;

    int start_line = 0;
    int start_col = 0;
    int end_line = 0;
    int end_col = 0;
    std::string file_path;

    NodeShape shape = NodeShape::Other;
    CallArity arity = CallArity::None;
    SemanticKind kind = SemanticKind::Unknown;
    StructuralRole role = StructuralRole::Other;

    SEXP node = R_NilValue;
    SEXP parent = R_NilValue;
    SEXP original_symbol = R_NilValue;

    bool removable = false;
    bool swappable = false;
};

struct MutationCandidate
{
    MutationFamily family = MutationFamily::OperatorReplacement;
    MutationAction action = MutationAction::ReplaceCallHeadSymbol;
    std::vector<int> path;

    SEXP replacement_symbol = R_NilValue;
    SEXP replacement_node = R_NilValue;

    int child_index_a = -1;
    int child_index_b = -1;

    SemanticKind source_kind = SemanticKind::Unknown;
    CallArity source_arity = CallArity::None;
};

#endif // MUTATION_MODEL_H