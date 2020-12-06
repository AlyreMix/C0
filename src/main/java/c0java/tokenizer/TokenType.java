package c0java.tokenizer;

public enum TokenType {
    /**
     * FN_KW     -> 'fn'
     * LET_KW    -> 'let'
     * CONST_KW  -> 'const'
     * AS_KW     -> 'as'
     * WHILE_KW  -> 'while'
     * IF_KW     -> 'if'
     * ELSE_KW   -> 'else'
     * RETURN_KW -> 'return'
     * digit -> [0-9]
     * UINT_LITERAL -> digit+
     * PLUS      -> '+'
     * MINUS     -> '-'
     * MUL       -> '*'
     * DIV       -> '/'
     * ASSIGN    -> '='
     * EQ        -> '=='
     * NEQ       -> '!='
     * LT        -> '<'
     * GT        -> '>'
     * LE        -> '<='
     * GE        -> '>='
     * L_PAREN   -> '('
     * R_PAREN   -> ')'
     * L_BRACE   -> '{'
     * R_BRACE   -> '}'
     * ARROW     -> '->'
     * COMMA     -> ','
     * COLON     -> ':'
     * SEMICOLON -> ';'
     * 64 位有符号整数 int
     * 空类型 void
     *  */

    VOID,
    INT,
    FN_KW,
    LET_KW,
    CONST_KW,
    AS_KW,
    WHILE_KW,
    IF_KW,
    ELSE_KW,
    RETURN_KW,
    PLUS,
    MINUS,
    MUL,
    DIV,
    ASSIGN,
    EQ,
    NEQ,
    LT,
    GT,
    LE,
    GE,
    L_PAREN,
    R_PAREN,
    L_BRACE,
    R_BRACE,
    ARROW,
    COMMA,
    COLON,
    SEMICOLON,
    IDENT,
    UINT_LITERAL,
    /** 文件末尾 */
    EOF;

    @Override
    public String toString() {
        switch (this) {
            case VOID      :return "void";
            case INT       :return "int";
            case FN_KW     :return "fn";
            case LET_KW    :return "let";
            case CONST_KW  :return "const";
            case AS_KW     :return "as";
            case WHILE_KW  :return "while";
            case IF_KW     :return "if";
            case ELSE_KW   :return "else";
            case RETURN_KW :return "return";
            case PLUS      :return "+";
            case MINUS     :return "-";
            case MUL       :return "*";
            case DIV       :return "/";
            case ASSIGN    :return "=";
            case EQ        :return "==";
            case NEQ       :return "!=";
            case LT        :return "<";
            case GT        :return ">";
            case LE        :return "<=";
            case GE        :return ">=";
            case L_PAREN   :return "(";
            case R_PAREN   :return ")";
            case L_BRACE   :return "{";
            case R_BRACE   :return "}";
            case ARROW     :return "->";
            case COMMA     :return ",";
            case COLON     :return ":";
            case SEMICOLON :return ";";
            case IDENT     :return "ident";
            case UINT_LITERAL: return "uint_literal";
            default:
                return "InvalidToken";
        }
    }
}
