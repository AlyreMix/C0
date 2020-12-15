# SSSCompiler-C0

SingleSele's Simple Compler for C0
# 词法分析
## 单词类别表
     * FN_KW     -> 'fn'
     * LET_KW    -> 'let'
     * CONST_KW  -> 'const'
     * AS_KW     -> 'as'
     * WHILE_KW  -> 'while'
     * IF_KW     -> 'if'
     * ELSE_KW   -> 'else'
     * RETURN_KW -> 'return'
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
     * IDENT     -> 'ident'
     * UINT_LITERAL -> 'uint_literal'

 # 语法分析
 ```text
 expr -> 
       operator_expr
     | negate_expr
     | assign_expr
     | call_expr
     | literal_expr
     | ident_expr
     | group_expr
 
 binary_operator -> '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>='
 operator_expr -> expr binary_operator expr
 
 negate_expr -> '-' expr
 
 assign_expr -> l_expr '=' expr
 
 call_param_list -> expr (',' expr)*
 call_expr -> IDENT '(' call_param_list? ')'
 
 literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL | CHAR_LITERAL
 
 ident_expr -> IDENT
 
 group_expr -> '(' expr ')'
 
 // ## 左值表达式
 l_expr -> IDENT
 
 // ## 类型
 ty -> IDENT
 
 // # 语句
 stmt ->
       expr_stmt
     | decl_stmt
     | if_stmt
     | while_stmt
     | return_stmt
     | block_stmt
     | empty_stmt
 
 expr_stmt -> expr ';'
 
 let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'
 const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
 decl_stmt -> let_decl_stmt | const_decl_stmt
 
 if_stmt -> 'if' expr block_stmt ('else' 'if' expr block_stmt)* ('else' block_stmt)?
 
 while_stmt -> 'while' expr block_stmt
 
 return_stmt -> 'return' expr? ';'
 
 block_stmt -> '{' stmt* '}'
 
 empty_stmt -> ';'
 
 // # 函数
 function_param -> 'const'? IDENT ':' ty
 function_param_list -> function_param (',' function_param)*
 function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
 
 // # 程序
 item -> function | decl_stmt
 program -> item*
 ```

## First集
```text
binary_operator='+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>='
operator_expr -> '-'|IDENT| UINT_LITERAL |'('
assign_expr -> IDENT
negate_expr -> '-'
call_param_list -> '-'|IDENT| UINT_LITERAL |'('
call_expr -> IDENT
expr_stmt -> '-'|IDENT| UINT_LITERAL |'('
ty=IDENT
l_expr=IDENT
group_expr='('
ident_expr=IDENT
literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL | CHAR_LITERAL
empty_stmt -> ';'
let_decl_stmt -> 'let'
const_decl_stmt -> 'const' 
decl_stmt =‘let'|'const'
if_stmt -> 'if'
while_stmt -> 'while'
return_stmt -> 'return'
block_stmt -> '{'
stmt='-'|IDENT| UINT_LITERAL |'('|‘let'|'const'|'while'|'return'| '{'|';'
function_param -> 'const'|ident
 function_param_list -> 'const'|ident
function -> 'fn'
expr='-'|IDENT| UINT_LITERAL |'('
item -> 'fn'|decl
```