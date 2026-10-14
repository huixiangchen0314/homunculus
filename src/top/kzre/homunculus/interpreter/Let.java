package top.kzre.homunculus.interpreter;

import top.kzre.homunculus.lang.Symbol;
import top.kzre.homunculus.lang.Var;

public interface Let {
    /**
     * 绑定符号到 Var 上
     */
    Let bind(Symbol symbol, Var var);

    /**
     * 进入 Let 作用域
     */
    Block execute();
}
