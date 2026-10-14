package top.kzre.homunculus.interpreter;

import top.kzre.homunculus.lang.Symbol;

public interface Def {
    /**
     * 绑定符号到 Lambda 上
     */
    LambdaDef lambda(Symbol symbol);

}
