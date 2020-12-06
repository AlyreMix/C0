package c0java.tokenizer;

import c0java.error.TokenizeError;
import c0java.error.ErrorCode;
import c0java.util.Pos;

public class Tokenizer {

    private StringIter it;

    public Tokenizer(StringIter it) {
        this.it = it;
    }

    // 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了
    /**
     * 获取下一个 Token
     * 
     * @return
     * @throws TokenizeError 如果解析有异常则抛出
     */
    public Token nextToken() throws TokenizeError {
        it.readAll();

        // 跳过之前的所有空白字符
        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();
        if (Character.isDigit(peek)) {
            return lexUInt();
        } else if (Character.isAlphabetic(peek)) {
            return lexIdentOrKeyword();
        } else {
            return lexOperatorOrUnknown();
        }
    }

    /**
     * 无符号整数字面量
     * @return TokenType.UINT_LITERAL
     * @throws TokenizeError
     */
    private Token lexUInt() throws TokenizeError {
        // 请填空：
        StringBuffer sb = new StringBuffer();
        Pos startPos = it.currentPos();
        // 直到查看下一个字符不是数字为止:
        while(Character.isDigit(it.peekChar()))
        // -- 前进一个字符，并存储这个字符
            sb.append(it.nextChar());

        // 解析存储的字符串为无符号整数
        // 解析成功则返回无符号整数类型的token，否则返回编译错误
        try{
            int tokenValue = Integer.parseInt(sb.toString());
            // Token 的 Value 应填写数字的值
            return new Token(TokenType.UINT_LITERAL, tokenValue,  startPos, it.currentPos());
        }
        catch(Exception e){
            throw new TokenizeError(ErrorCode.InvalidInput, it.currentPos());
        }
    }

    private Token lexIdentOrKeyword() throws TokenizeError {
        // 请填空：
        StringBuffer sb = new StringBuffer();
        Pos startPos = it.currentPos();
        // 直到查看下一个字符不是数字或字母为止:
        while(Character.isDigit(it.peekChar())||Character.isLetter(it.peekChar()))
        // -- 前进一个字符，并存储这个字符
            sb.append(it.nextChar());
        //
        // 尝试将存储的字符串解释为关键字
        TokenType tt = Token.identifyKeyword(sb.toString());
        // -- 如果是关键字，则返回关键字类型的 token
        // -- 否则，返回标识符
        if(tt != null)
            // Token 的 Value 应填写标识符或关键字的字符串
            return new Token(tt, sb.toString(), startPos, it.currentPos());
        else
            return new Token(TokenType.IDENT, sb.toString(), startPos, it.currentPos());
    }

    private Token lexOperatorOrUnknown() throws TokenizeError {
        switch (it.nextChar()) {
            case '+':
                return new Token(TokenType.PLUS, '+', it.previousPos(), it.currentPos());

            case '-':
                if(it.nextChar()=='>')
                    return new Token(TokenType.ARROW, "->", it.previousPos(), it.currentPos());
                else
                    return new Token(TokenType.MINUS, '-', it.previousPos(), it.currentPos());

            case '*':
                return new Token(TokenType.MUL, '*', it.previousPos(), it.currentPos());

            case '/':
                return new Token(TokenType.DIV, '/', it.previousPos(), it.currentPos());

            case '=':
                if(it.nextChar()=='=')
                    return new Token(TokenType.EQ, "==", it.previousPos(), it.currentPos());
                else
                    return new Token(TokenType.ASSIGN, '=', it.previousPos(), it.currentPos());

            case '!':
                if(it.nextChar()=='=')
                    return new Token(TokenType.NEQ, "!=", it.previousPos(), it.currentPos());
                else
                    // 不认识这个输入，摸了
                    throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());

            case '<':
                if(it.nextChar()=='=')
                    return new Token(TokenType.LE, "<=", it.previousPos(), it.currentPos());
                else
                    return new Token(TokenType.LT, '<', it.previousPos(), it.currentPos());

            case '>':
                if(it.nextChar()=='=')
                    return new Token(TokenType.GE, ">=", it.previousPos(), it.currentPos());
                else
                    return new Token(TokenType.GT, '>', it.previousPos(), it.currentPos());

            case '(':
                return new Token(TokenType.L_PAREN, '(', it.previousPos(), it.currentPos());
            case ')':
                return new Token(TokenType.R_PAREN, ')', it.previousPos(), it.currentPos());

            case '{':
                return new Token(TokenType.L_BRACE, '{', it.previousPos(), it.currentPos());
            case '}':
                return new Token(TokenType.R_BRACE, '}', it.previousPos(), it.currentPos());

            case ',':
                return new Token(TokenType.COMMA, ',', it.previousPos(), it.currentPos());

            case ':':
                return new Token(TokenType.COLON, ':', it.previousPos(), it.currentPos());
            case ';':
                return new Token(TokenType.SEMICOLON, ';', it.previousPos(), it.currentPos());
            default:
                // 不认识这个输入，摸了
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }

    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }
}
