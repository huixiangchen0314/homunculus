package top.kzre.homunculus.core.ir2.pass;

import java.util.ArrayList;
import java.util.List;

/**
 * 类型字符串的 token 器。
 *
 * <p>把类型表达式字符串切分成 token 序列，供 Clojure 侧的
 * {@code parse-type-symbol} 消费。
 *
 * <h2>token 种类</h2>
 * <ul>
 *   <li>{@code IDENT} —— 标识符：{@code float4} / {@code aa.bb.c} / {@code aa.bb/c} / {@code foo-bar}</li>
 *   <li>{@code TYPE_VAR} —— 类型变量：{@code ?a} 的 {@code text} 是 {@code "a"}（不含 {@code ?}）</li>
 *   <li>{@code ARROW} —— 函数箭头 {@code ->}</li>
 *   <li>{@code INT} —— 整数（数组长度）：{@code 13}</li>
 *   <li>{@code LBRACKET} / {@code RBRACKET} —— 数组括号</li>
 *   <li>{@code LANGLE} / {@code RANGLE} —— 泛型尖括号</li>
 *   <li>{@code LPAREN} / {@code RPAREN} —— 分组括号</li>
 *   <li>{@code COMMA} —— 泛型参数分隔</li>
 *   <li>{@code EOF} —— 结束哨兵</li>
 * </ul>
 *
 * <h2>标识符字符集</h2>
 *
 * <p>标识符首字符：{@code [a-zA-Z_*+!$%&=]}；后续字符：{@code [a-zA-Z0-9_./!?*+=]}；
 * 中间连字符：{@code -} 后必须是标识符后续字符且不能是 {@code >}——
 * {@code foo-bar} 是一个标识符，{@code foo->bar} 切成 {@code foo} / {@code ->} / {@code bar}。
 *
 * <p>这样 {@code aa.bb.c} 是一个 IDENT token（不是 {@code aa} / {@code .} / {@code bb} / {@code .} / {@code c}），
 * {@code composite.tile-layout/uTileSize} 也是一个 token。
 *
 * <h2>空白与错误</h2>
 *
 * <p>空白字符（空格、制表、换行）被跳过。遇到不在任何 token 类内的字符，
 * 抛出 {@link IllegalArgumentException}，消息含字符和位置。
 *
 * <p>返回的列表以 {@code EOF} 结尾——调用方不需要单独判断 {@code pos < size}。
 *
 * <h2>线程契约</h2>
 * <p>无状态（除实例内部的 {@code pos}），每次 {@link #tokenize(String)} 新建实例。
 * 线程安全。
 */
public final class TypeTokenizer {

    public enum TokenType {
        IDENT, TYPE_VAR, INT, ARROW,
        LBRACKET, RBRACKET, LANGLE, RANGLE,
        LPAREN, RPAREN, COMMA, EOF
    }

    public static final class Token {
        public final TokenType type;
        public final String    text;
        public final int       position;

        public Token(TokenType type, String text, int position) {
            this.type     = type;
            this.text     = text;
            this.position = position;
        }

        @Override
        public String toString() {
            return type + "(" + text + ")@" + position;
        }
    }

    private final String src;
    private int pos;

    private TypeTokenizer(String src) {
        if (src == null) {
            throw new IllegalArgumentException("src must not be null");
        }
        this.src = src;
        this.pos = 0;
    }

    /**
     * 把类型表达式字符串切分为 token 列表。列表以 {@code EOF} 结尾。
     *
     * @param src 类型表达式字符串，非 null
     * @return token 列表，最后一项是 {@code EOF}
     * @throws IllegalArgumentException {@code src} 为 null 或含非法字符
     */
    public static List<Token> tokenize(String src) {
        return new TypeTokenizer(src).tokenizeAll();
    }

    // ═══════════════════════════════════════════
    // 主循环
    // ═══════════════════════════════════════════

    private List<Token> tokenizeAll() {
        List<Token> tokens = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= src.length()) {
                tokens.add(new Token(TokenType.EOF, "", pos));
                return tokens;
            }
            tokens.add(nextToken());
        }
    }

    private Token nextToken() {
        int  start = pos;
        char c     = src.charAt(pos);

        if (c == '?') {
            return readTypeVar();
        }
        if (c == '-' && pos + 1 < src.length() && src.charAt(pos + 1) == '>') {
            pos += 2;
            return new Token(TokenType.ARROW, "->", start);
        }
        if (isIdentStart(c)) {
            return readIdent();
        }
        if (isDigit(c)) {
            return readInt();
        }
        return readPunct();
    }

    // ═══════════════════════════════════════════
    // 各 token 的读取
    // ═══════════════════════════════════════════

    private Token readTypeVar() {
        int start = pos;
        pos++; // 消费 ?
        if (pos >= src.length() || !isIdentStart(src.charAt(pos))) {
            throw new IllegalArgumentException(
                    "expected identifier after '?' at position " + start);
        }
        readIdentBody();
        return new Token(TokenType.TYPE_VAR,
                src.substring(start + 1, pos),
                start);
    }

    private Token readIdent() {
        int start = pos;
        readIdentBody();
        return new Token(TokenType.IDENT, src.substring(start, pos), start);
    }

    /**
     * 读取标识符体（不含首字符——调用方已经确认 pos 指向一个合法首字符）。
     *
     * <p>循环规则：
     * <ul>
     *   <li>标识符字符（字母 / 数字 / {@code .} / {@code /} / {@code ?} / {@code *+!$%&=}）→ 消费</li>
     *   <li>{@code -} 且后面不是 {@code >} 且后面是标识符字符 → 消费 {@code -}（连字符中间态）</li>
     *   <li>其他 → 停止</li>
     * </ul>
     */
    private void readIdentBody() {
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (isIdentBody(c)) {
                pos++;
            } else if (c == '-'
                    && pos + 1 < src.length()
                    && src.charAt(pos + 1) != '>'
                    && isIdentBody(src.charAt(pos + 1))) {
                pos++;
            } else {
                break;
            }
        }
    }

    private Token readInt() {
        int start = pos;
        while (pos < src.length() && isDigit(src.charAt(pos))) {
            pos++;
        }
        return new Token(TokenType.INT, src.substring(start, pos), start);
    }

    private Token readPunct() {
        int  start = pos;
        char c     = src.charAt(pos);
        switch (c) {
            case '[': pos++; return new Token(TokenType.LBRACKET, "[", start);
            case ']': pos++; return new Token(TokenType.RBRACKET, "]", start);
            case '<': pos++; return new Token(TokenType.LANGLE,   "<", start);
            case '>': pos++; return new Token(TokenType.RANGLE,   ">", start);
            case '(': pos++; return new Token(TokenType.LPAREN,   "(", start);
            case ')': pos++; return new Token(TokenType.RPAREN,   ")", start);
            case ',': pos++; return new Token(TokenType.COMMA,    ",", start);
            default:
                throw new IllegalArgumentException(
                        "Unexpected character '" + c + "' at position " + start);
        }
    }

    // ═══════════════════════════════════════════
    // 字符分类
    // ═══════════════════════════════════════════

    private void skipWhitespace() {
        while (pos < src.length() && Character.isWhitespace(src.charAt(pos))) {
            pos++;
        }
    }

    private static boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    /** 标识符首字符：字母、下划线、Clojure 特殊字符（不含数字、点、斜杠、问号）。 */
    private static boolean isIdentStart(char c) {
        return (c >= 'a' && c <= 'z')
                || (c >= 'A' && c <= 'Z')
                || c == '_' || c == '*' || c == '+'
                || c == '!' || c == '$' || c == '%'
                || c == '&' || c == '=';
    }

    /** 标识符后续字符：首字符集 + 数字 + 点 + 斜杠 + 问号。不含 {@code -}——单独处理。 */
    private static boolean isIdentBody(char c) {
        return isIdentStart(c)
                || isDigit(c)
                || c == '.' || c == '/' || c == '?';
    }
}