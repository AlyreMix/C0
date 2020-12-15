package c0java.analyser;

import c0java.error.AnalyzeError;
import c0java.error.CompileError;
import c0java.error.ErrorCode;
import c0java.error.ExpectedTokenError;
import c0java.error.TokenizeError;
import c0java.instruction.Instruction;
import c0java.instruction.Operation;
import c0java.tokenizer.Token;
import c0java.tokenizer.TokenType;
import c0java.tokenizer.Tokenizer;
import c0java.util.Pos;

import java.util.*;

public final class Analyser {

    Tokenizer tokenizer;
    ArrayList<Instruction> instructions;

    /** 当前偷看的 token */
    Token peekedToken = null;

    /** 符号表 */
    HashMap<String, SymbolEntry> symbolTable = new HashMap<>();

    /** 下一个变量的栈偏移 */
    int nextOffset = 0;

    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
        this.instructions = new ArrayList<>();
    }

    public List<Instruction> analyse() throws CompileError {
        analyseProgram();
        return instructions;
    }

    /**
     * 查看下一个 Token
     * 
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 获取下一个 Token
     * 
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            var token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     * 
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        var token = peek();
        return token.getTokenType() == tt;
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
     * 
     * @param tt 类型
     * @return 如果匹配则返回这个 token，否则返回 null
     * @throws TokenizeError
     */
    private Token nextIf(TokenType tt) throws TokenizeError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            return null;
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     * 
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * 获取下一个变量的栈偏移
     * 
     * @return
     */
    private int getNextVariableOffset() {
        return this.nextOffset++;
    }

    /**
     * 添加一个符号
     * 
     * @param name          名字
     * @param isInitialized 是否已赋值
     * @param isConstant    是否是常量
     * @param curPos        当前 token 的位置（报错用）
     * @throws AnalyzeError 如果重复定义了则抛异常
     */
    private void addSymbol(String name, boolean isInitialized, boolean isConstant, Pos curPos) throws AnalyzeError {
        if (this.symbolTable.get(name) != null) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        } else {
            this.symbolTable.put(name, new SymbolEntry(isConstant, isInitialized, getNextVariableOffset()));
        }
    }

    /**
     * 设置符号为已赋值
     * 
     * @param name   符号名称
     * @param curPos 当前位置（报错用）
     * @throws AnalyzeError 如果未定义则抛异常
     */
    private void initializeSymbol(String name, Pos curPos) throws AnalyzeError {
        var entry = this.symbolTable.get(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            entry.setInitialized(true);
        }
    }

    /**
     * 获取变量在栈上的偏移
     * 
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 栈偏移
     * @throws AnalyzeError
     */
    private int getOffset(String name, Pos curPos) throws AnalyzeError {
        var entry = this.symbolTable.get(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            return entry.getStackOffset();
        }
    }

    /**
     * 获取变量是否是常量
     * 
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 是否为常量
     * @throws AnalyzeError
     */
    private boolean isConstant(String name, Pos curPos) throws AnalyzeError {
        var entry = this.symbolTable.get(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            return entry.isConstant();
        }
    }

    private void analyseProgram() throws CompileError {
        // program -> item*
        while(peekedToken.getTokenType()!=TokenType.EOF)
        {
            analyseItem();
        }
    }

    private void analyseItem() throws CompileError {
        if(peekedToken.getTokenType()==TokenType.FN_KW)
            analyseFunction();
        else if(peekedToken.getTokenType()==TokenType.LET_KW||peekedToken.getTokenType()==TokenType.CONST_KW)
            analyseDeclStmt();
        else
            throw new ExpectedTokenError(List.of(TokenType.FN_KW, TokenType.LET_KW, TokenType.CONST_KW), next());
    }

    private void analyseFunction() throws CompileError {
        expect(TokenType.FN_KW);
        String functionName = expect(TokenType.IDENT).getValueString();
        expect(TokenType.L_PAREN);
        if(peekedToken.getTokenType()==TokenType.CONST_KW||peekedToken.getTokenType()==TokenType.IDENT)
            analyseFunctionParamList();// Todo:实现list功能
        expect(TokenType.R_PAREN);
        expect(TokenType.ARROW);
        analyseTy();// Todo:实现返回值功能
        analyseBlockStmt();// Todo:实现函数内容
    }

    private void analyseDeclStmt() throws CompileError {
        if(peekedToken.getTokenType()==TokenType.LET_KW){
            analyseLetDeclStmt();
        }
        else if(peekedToken.getTokenType()==TokenType.CONST_KW){
            analyseConstDeclStmt();
        }
        else
            throw new ExpectedTokenError(List.of(TokenType.LET_KW, TokenType.CONST_KW), next());
    }

    private void analyseFunctionParamList() throws CompileError {
        analyseFunctionParam();// Todo:实现功能
        while(peekedToken.getTokenType()==TokenType.COMMA){
            expect(TokenType.COMMA);
            analyseFunctionParam();// Todo:实现功能
        }
    }

    private void analyseTy() throws CompileError{
        // Todo:实现功能
        expect(TokenType.IDENT);
    }

    private boolean isStmt(Token tt){
        TokenType[] stmt_first = {
                TokenType.MINUS,
                TokenType.IDENT,
                TokenType.UINT_LITERAL,
                TokenType.L_PAREN,
                TokenType.LET_KW,
                TokenType.CONST_KW,
                TokenType.WHILE_KW,
                TokenType.RETURN_KW,
                TokenType.L_BRACE,
                TokenType.SEMICOLON
        };
        TokenType tt_type = tt.getTokenType();
        for(TokenType type: stmt_first){
            if(tt_type==type)
                return true;
        }
        return false;
    }

    private void analyseBlockStmt() throws CompileError{
        // Todo:实现功能
        expect(TokenType.L_BRACE);

        while(isStmt(peekedToken)){
            analyseStmt();
        }

        expect(TokenType.R_BRACE);
    }

    private void analyseLetDeclStmt() throws CompileError{
        expect(TokenType.LET_KW);
        String varName = expect(TokenType.IDENT).getValueString();
        expect(TokenType.COLON);
        analyseTy();// Todo:获取类型
        if(peekedToken.getTokenType()==TokenType.ASSIGN)        {
            expect(TokenType.ASSIGN);
            analyseExpr();// Todo:获取返回值
        }
        expect(TokenType.SEMICOLON);
    }

    private void analyseConstDeclStmt() throws CompileError{
        expect(TokenType.CONST_KW);
        String varName = expect(TokenType.IDENT).getValueString();
        expect(TokenType.COLON);
        analyseTy();// Todo:获取类型
        expect(TokenType.ASSIGN);
        analyseExpr();// Todo:获取返回值
        expect(TokenType.SEMICOLON);
    }

    private void analyseFunctionParam() throws CompileError{
        //Todo:实现功能
        if(peekedToken.getTokenType()==TokenType.CONST_KW){
            expect(TokenType.CONST_KW);
        }
        String varName = expect(TokenType.IDENT).getValueString();
        expect(TokenType.COLON);
        analyseTy();// Todo:获取返回值
    }

    private void analyseStmt() throws CompileError{
        TokenType nextType = peekedToken.getTokenType();
        if(nextType==TokenType.LET_KW||nextType==TokenType.CONST_KW)
            analyseDeclStmt();
        else if(nextType==TokenType.IF_KW)
            analyseIfStmt();
        else if(nextType==TokenType.WHILE_KW)
            analyseWhileStmt();
        else if(nextType==TokenType.RETURN_KW)
            analyseReturnStmt();
        else if(nextType==TokenType.L_BRACE)
            analyseBlockStmt();
        else if(nextType==TokenType.SEMICOLON)
            analyseEmptyStmt();
        else if(nextType==TokenType.MINUS||nextType==TokenType.IDENT||nextType==TokenType.UINT_LITERAL||nextType==TokenType.L_PAREN)
            analyseExprStmt();
        else
            throw new ExpectedTokenError(List.of(), next()); // 偷懒了，希望别出岔子
    }

    private void analyseExpr() throws CompileError {
        // Todo:获取返回值
        if(peekedToken.getTokenType()==TokenType.MINUS){
            // negate expr
            expect(TokenType.MINUS);
            int retv = -(Integer)(expect(TokenType.UINT_LITERAL).getValue());
            // Todo:retv
        }
        else if(peekedToken.getTokenType()==TokenType.IDENT){
            // assign or ident or call
            String lValue = expect(TokenType.IDENT).getValueString();
            if(peekedToken.getTokenType()==TokenType.ASSIGN){
                // assign
                expect(TokenType.ASSIGN);
                analyseExpr();// Todo: 赋值
            }
            else if(peekedToken.getTokenType()==TokenType.L_PAREN){
                // call
                expect(TokenType.L_PAREN);
                if(peekedToken.getTokenType()==TokenType.R_PAREN)
                    expect(TokenType.R_PAREN);
                else{
                    analyseCallParamList();// Todo:参数列表
                    expect(TokenType.R_PAREN);
                }
            }
            else{
                // ident
                expect(TokenType.IDENT).getValueString();
                // Todo: 返回引用值
            }
        }
        else if(peekedToken.getTokenType()==TokenType.L_PAREN){
            // group expr
            expect(TokenType.L_PAREN);
            analyseExpr();
            expect(TokenType.R_PAREN);
        }
        else if(peekedToken.getTokenType()==TokenType.UINT_LITERAL){
            // literal
            int value = (Integer)expect(TokenType.UINT_LITERAL).getValue();
        }
        else
            throw new ExpectedTokenError(List.of(), next()); // 偷懒了，希望别出岔子
        TokenType nextType = peekedToken.getTokenType();
        if(
                nextType==TokenType.PLUS ||
                nextType==TokenType.MINUS ||
                nextType==TokenType.MUL ||
                nextType==TokenType.DIV ||
                nextType==TokenType.EQ ||
                nextType==TokenType.NEQ ||
                nextType==TokenType.LT ||
                nextType==TokenType.GT ||
                nextType==TokenType.LE ||
                nextType==TokenType.GE
        ){
            TokenType opr = next().getTokenType();
            analyseExpr();// Todo:获取返回值
            // Todo: 实现功能
        }
        // Todo:返回值
    }

    private void analyseIfStmt() throws CompileError{
        // Todo: 判断分支
        expect(TokenType.IF_KW);
        analyseExpr();// Todo:判断条件
        analyseBlockStmt();
        if(peekedToken.getTokenType()==TokenType.ELSE_KW){
            expect(TokenType.ELSE_KW);
            if(peekedToken.getTokenType()==TokenType.IF_KW){
                analyseIfStmt();
            }
            else{
                analyseBlockStmt();
            }
        }
    }

    private void analyseWhileStmt() throws CompileError{
        // Todo:判断分支
        expect(TokenType.WHILE_KW);
        analyseExpr(); // Todo:判断条件
        analyseBlockStmt();
    }

    private void analyseReturnStmt() throws CompileError{
        expect(TokenType.RETURN_KW);
        if(peekedToken.getTokenType()==TokenType.SEMICOLON){
            expect(TokenType.SEMICOLON);
        }
        else{
            analyseExpr();
            expect(TokenType.SEMICOLON);
        }
    }

    private void analyseEmptyStmt() throws CompileError{
        expect(TokenType.SEMICOLON);
    }

    private void analyseExprStmt() throws CompileError{
        analyseExpr();
        expect(TokenType.SEMICOLON);
    }

    private void analyseCallParamList() throws CompileError{
        // Todo: 存储参数列表
        analyseExpr();
        while (peekedToken.getTokenType()==TokenType.COMMA){
            expect(TokenType.COMMA);
            analyseExpr();
        }
    }

//    private void analyseProgram() throws CompileError {
//        // 程序 -> 'begin' 主过程 'end'
//        // 示例函数，示例如何调用子程序
//        // 'begin'
//        expect(TokenType.Begin);
//
//        analyseMain();
//
//        // 'end'
//        expect(TokenType.End);
//        expect(TokenType.EOF);
//    }
//
//    /**
//     * <主过程> ::= <常量声明><变量声明><语句序列>
//     * @throws CompileError
//     */
//    private void analyseMain() throws CompileError {
//        analyseConstantDeclaration();
//        analyseVariableDeclaration();
//        analyseStatementSequence();
//    }
//
//    /**
//     * <常量声明> ::= {<常量声明语句>}
//     * <常量声明语句> ::= 'const'<标识符>'='<常表达式>';'
//     * @throws CompileError
//     */
//    private void analyseConstantDeclaration() throws CompileError {
//        // 示例函数，示例如何解析常量声明
//        // 常量声明 -> 常量声明语句*
//
//        // 如果下一个 token 是 const 就继续
//        while (nextIf(TokenType.Const) != null) {
//            // 常量声明语句 -> 'const' 变量名 '=' 常表达式 ';'
//
//            // 变量名
//            var nameToken = expect(TokenType.Ident);
//
//            // 加入符号表
//            String name = (String) nameToken.getValue();
//            addSymbol(name, true, true, nameToken.getStartPos());
//
//            // 等于号
//            expect(TokenType.Equal);
//
//            // 常表达式
//            var value = analyseConstantExpression();
//
//            // 分号
//            expect(TokenType.Semicolon);
//
//            // 这里把常量值直接放进栈里，位置和符号表记录的一样。
//            // 更高级的程序还可以把常量的值记录下来，遇到相应的变量直接替换成这个常数值，
//            // 我们这里就先不这么干了。
//            instructions.add(new Instruction(Operation.LIT, value));
//        }
//    }
//
//    /**
//     * <变量声明> ::= {<变量声明语句>}
//     * <变量声明语句> ::= 'var'<标识符>['='<表达式>]';'
//     * @throws CompileError
//     */
//    private void analyseVariableDeclaration() throws CompileError {
////<<<<<<< HEAD
////        // 如果下一个 token 是 var 就继续
////        while (nextIf(TokenType.Var) != null) {
////            // 变量名
////            var nameToken = expect(TokenType.Ident);
////
////            // 等于号
////            expect(TokenType.Equal);
////
////            // 表达式
////            analyseExpression();
////
////            // 分号
////            expect(TokenType.Semicolon);
////=======
//        // 变量声明 -> 变量声明语句*
//
//        // 如果下一个 token 是 var 就继续
//        while (nextIf(TokenType.Var) != null) {
//            // 变量名
//            var nameToken = expect(TokenType.Ident);
//            // 变量初始化了吗
//            boolean initialized = false;
//
//            // 下个 token 是等于号吗？如果是的话分析初始化
//            expect(TokenType.Equal);
//            // 分析初始化的表达式
//            analyseExpression();
//
//            // 分号
//            expect(TokenType.Semicolon);
//
//            // 加入符号表，请填写名字和当前位置（报错用）
//            String name = nameToken.getValueString();
//            addSymbol(name, false, false, /* 当前位置 */ null);
//
//            // 如果没有初始化的话在栈里推入一个初始值
//            if (!initialized) {
//                instructions.add(new Instruction(Operation.LIT, 0));
//            }
//        }
//    }
//
//    /**
//     * <语句序列> ::= {<语句>}
//     * @throws CompileError
//     */
//    private void analyseStatementSequence() throws CompileError {
//        // 语句序列 -> 语句*
//        // 语句 -> 赋值语句 | 输出语句 | 空语句
//
//        while (true) {
//            // 如果下一个 token 是……
//            var peeked = peek().getTokenType();
//            if (peeked == TokenType.Ident) {
//                // 调用相应的分析函数
//                analyseAssignmentStatement();
//                // 如果遇到其他非终结符的 FIRST 集呢？
//            }
//            else if(peeked == TokenType.Print){
//                analyseOutputStatement();
//            }
//            else if(peeked == TokenType.Semicolon){
//                expect(TokenType.Semicolon);
//            }
//            else {
//                // 都不是，摸了
//                break;
//            }
//        }
//    }
//
//    /**
//          * <常表达式> ::= [<符号>]<无符号整数>
//          * @throws CompileError
//          */
//    private int analyseConstantExpression() throws CompileError {
//        boolean negative = false;
//        if (nextIf(TokenType.Plus) != null) {
//            negative = false;
//        } else if (nextIf(TokenType.Minus) != null) {
//            negative = true;
//        }
//
//        var token = expect(TokenType.Uint);
//
//        int value = (int) token.getValue();
//        if (negative) {
//            value = -value;
//        }
//
//        return value;
//    }
//
//    /**
//     * <表达式> ::= <项>{<加法型运算符><项>}
//     * @throws CompileError
//     */
//    private void analyseExpression() throws CompileError {
//        // 表达式 -> 项 (加法运算符 项)*
//        // 项
//        analyseItem();
//
//        while (true) {
//            // 预读可能是运算符的 token
//            var op = peek();
//            if (op.getTokenType() != TokenType.Plus && op.getTokenType() != TokenType.Minus) {
//                break;
//            }
//
//            // 运算符
//            next();
//
//            // 项
//            analyseItem();
//
//            // 生成代码
//            if (op.getTokenType() == TokenType.Plus) {
//                instructions.add(new Instruction(Operation.ADD));
//            } else if (op.getTokenType() == TokenType.Minus) {
//                instructions.add(new Instruction(Operation.SUB));
//            }
//        }
//    }
//
//    /**
//     * <赋值语句> ::= <标识符>'='<表达式>';'
//     * @throws CompileError
//     */
//    private void analyseAssignmentStatement() throws CompileError {
//
//        // 赋值语句 -> 标识符 '=' 表达式 ';'
//
//        // 分析这个语句
//        var nameToken = expect(TokenType.Ident);
//        expect(TokenType.Equal);
//        analyseExpression();
//
//        // 标识符是什么？
//        String name = nameToken.getValueString();
//        var symbol = symbolTable.get(name);
//        if (symbol == null) {
//            // 没有这个标识符
//            throw new AnalyzeError(ErrorCode.NotDeclared, /* 当前位置 */ null);
//        } else if (symbol.isConstant) {
//            // 标识符是常量
//            throw new AnalyzeError(ErrorCode.AssignToConstant, /* 当前位置 */ null);
//        }
//        // 设置符号已初始化
//        initializeSymbol(name, null);
//
//        // 把结果保存
//        var offset = getOffset(name, null);
//        instructions.add(new Instruction(Operation.STO, offset));
//    }
//
//    /**
//     * <输出语句> ::= 'print' '(' <表达式> ')' ';'
//     * @throws CompileError
//     */
//    private void analyseOutputStatement() throws CompileError {
//        // 输出语句 -> 'print' '(' 表达式 ')' ';'
//
//        expect(TokenType.Print);
//        expect(TokenType.LParen);
//
//        analyseExpression();
//
//        expect(TokenType.RParen);
//        expect(TokenType.Semicolon);
//
//        instructions.add(new Instruction(Operation.WRT));
//    }
//
//    /**
//     * <项> ::= <因子>{<乘法型运算符><因子>}
//     * @throws CompileError
//     */
//    private void analyseItem() throws CompileError {
//        // 项 -> 因子 (乘法运算符 因子)*
//
//        // 因子
//        analyseFactor();
//
//        while (true) {
//            // 预读可能是运算符的 token
//            Token op = null;
//
//            // 运算符
//            if(check(TokenType.Mult)||check(TokenType.Div))
//                op=next();
//            else
//                break;
//
//            // 因子
//            analyseFactor();
//
//            // 生成代码
//            if (op.getTokenType() == TokenType.Mult) {
//                instructions.add(new Instruction(Operation.MUL));
//            } else if (op.getTokenType() == TokenType.Div) {
//                instructions.add(new Instruction(Operation.DIV));
//            }
//        }
//    }
//
//    /**
//     * <因子> ::= [<符号>]( <标识符> | <无符号整数> | '('<表达式>')' )
//     * @throws CompileError
//     */
//    private void analyseFactor() throws CompileError {
//        // 因子 -> 符号? (标识符 | 无符号整数 | '(' 表达式 ')')
//
//        boolean negate;
//        if (nextIf(TokenType.Minus) != null) {
//            negate = true;
//            // 计算结果需要被 0 减
//            instructions.add(new Instruction(Operation.LIT, 0));
//        } else {
//            nextIf(TokenType.Plus);
//            negate = false;
//        }
//
//        if (check(TokenType.Ident)) {
//            // 是标识符
//
//            // 加载标识符的值
//            String name = next().getValueString();
//            var symbol = symbolTable.get(name);
//            if (symbol == null) {
//                // 没有这个标识符
//                throw new AnalyzeError(ErrorCode.NotDeclared, /* 当前位置 */ null);
//            } else if (!symbol.isInitialized) {
//                // 标识符没初始化
//                throw new AnalyzeError(ErrorCode.NotInitialized, /* 当前位置 */ null);
//            }
//            var offset = getOffset(name, null);
//            instructions.add(new Instruction(Operation.LOD, offset));
//        } else if (check(TokenType.Uint)) {
//            // 是整数
//            // 加载整数值
//            int value = (Integer)(next().getValue());
//            instructions.add(new Instruction(Operation.LIT, value));
//        } else if (check(TokenType.LParen)) {
//            // 是表达式
//            // 调用相应的处理函数
//            analyseExpression();
//        } else {
//            // 都不是，摸了
//            throw new ExpectedTokenError(List.of(TokenType.Ident, TokenType.Uint, TokenType.LParen), next());
//        }
//
//        if (negate) {
//            instructions.add(new Instruction(Operation.SUB));
//        }
//    }
}
