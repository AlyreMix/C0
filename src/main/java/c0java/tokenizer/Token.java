package c0java.tokenizer;

import c0java.util.Pos;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Token {
    private TokenType tokenType;
    private Object value;
    private Pos startPos;
    private Pos endPos;
    private static Map<String, TokenType> keywords = new HashMap<String, TokenType>();
    static{
        keywords.put("fn", TokenType.FN_KW);
        keywords.put("let", TokenType.LET_KW);
        keywords.put("const", TokenType.CONST_KW);
        keywords.put("as", TokenType.AS_KW);
        keywords.put("while", TokenType.WHILE_KW);
        keywords.put("if", TokenType.IF_KW);
        keywords.put("else", TokenType.ELSE_KW);
        keywords.put("return", TokenType.RETURN_KW);
        }

    public Token(TokenType tokenType, Object value, Pos startPos, Pos endPos) {
        this.tokenType = tokenType;
        this.value = value;
        this.startPos = startPos;
        this.endPos = endPos;
    }

    public Token(Token token) {
        this.tokenType = token.tokenType;
        this.value = token.value;
        this.startPos = token.startPos;
        this.endPos = token.endPos;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Token token = (Token) o;
        return tokenType == token.tokenType && Objects.equals(value, token.value)
                && Objects.equals(startPos, token.startPos) && Objects.equals(endPos, token.endPos);
    }

    @Override
    public int hashCode() {
        return Objects.hash(tokenType, value, startPos, endPos);
    }

    public String getValueString() {
        if (value instanceof Integer || value instanceof String || value instanceof Character) {
            return value.toString();
        }
        throw new Error("No suitable cast for token value.");
    }

    public TokenType getTokenType() {
        return tokenType;
    }

    public void setTokenType(TokenType tokenType) {
        this.tokenType = tokenType;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public Pos getStartPos() {
        return startPos;
    }

    public void setStartPos(Pos startPos) {
        this.startPos = startPos;
    }

    public Pos getEndPos() {
        return endPos;
    }

    public void setEndPos(Pos endPos) {
        this.endPos = endPos;
    }

    @Override
    public String toString() {
        var sb = new StringBuilder();
        sb.append("Line: ").append(this.startPos.row).append(' ');
        sb.append("Column: ").append(this.startPos.col).append(' ');
        sb.append("Type: ").append(this.tokenType).append(' ');
        sb.append("Value: ").append(this.value);
        return sb.toString();
    }

    public String toStringAlt() {
        return new StringBuilder().append("Token(").append(this.tokenType).append(", value: ").append(value)
                .append("at: ").append(this.startPos).toString();
    }

    /**
     * 返回字符串对应的关键字，若不为关键字，返回null
     * @param string
     * @return TokenType
     */
    public static TokenType identifyKeyword(String string){
        return keywords.get(new String(string).toUpperCase());
    }
}
