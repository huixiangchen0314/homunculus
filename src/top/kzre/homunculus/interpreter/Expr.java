package top.kzre.homunculus.interpreter;

public interface Expr {
    /**
     * 在给定栈帧执行表达式
     */
    Expr eval(Frame frame);
}
