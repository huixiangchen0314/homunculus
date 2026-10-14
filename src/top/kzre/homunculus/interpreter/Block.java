package top.kzre.homunculus.interpreter;

public interface Block extends Expr{
    /**
     * 执行表达式，返回 表达式结果.
     */
    Object execute(Expr expr);
}
