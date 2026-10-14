package top.kzre.homunculus.lang;

/**
 * 函数。所有可调用对象统一用这个接口。
 * <br>
 * 内建函数、用户函数、mock 函数都实现它。
 */
public interface IFn {
    Object invoke(Object... params);
}