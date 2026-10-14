package top.kzre.homunculus.backend.mock;

import top.kzre.homunculus.lang.IFn;

public final class HMulFloatFloat implements IFn {
    public HMulFloatFloat() {}
    @Override
    public Object invoke(Object... params) {
        int length = params.length;
        if (length != 2){
            throw new IllegalArgumentException();
        }
        Object lhs = params[0];
        Object rhs = params[1];
        if (lhs instanceof HFloat && rhs instanceof HFloat) {
            return ((HFloat) lhs).getValue() * ((HFloat) rhs).getValue();
        }
        throw new IllegalArgumentException();
    }
}
