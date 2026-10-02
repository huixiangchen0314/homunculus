package top.kzre.homunculus.core;

import clojure.lang.IExceptionInfo;
import clojure.lang.IPersistentMap;
import clojure.lang.PersistentArrayMap;
import clojure.lang.Keyword;

/**
 * 编译错误，用户异常。
 *
 * 表示「用户代码有问题，编译无法继续」。
 * 其它异常（含普通 ex-info）默认视为编译器内部 bug。
 *
 * 调用方典型处理：
 *   (try ...
 *     (catch CompileException e (report-user-error e))
 *     (catch Throwable e (report-internal-bug e)))
 *
 * 通过 ex-data 获取结构化信息：
 *   :category  错误类别关键字，如 :type-mismatch
 *   :node      相关 IR2 节点（如果可定位）
 *   :expected  期望信息（可选）
 *   :actual    实际信息（可选）
 */
public class CompileException extends RuntimeException implements IExceptionInfo {

    private final IPersistentMap data;

    public CompileException(String message, Keyword category) {
        this(message, category, PersistentArrayMap.EMPTY, null);
    }

    public CompileException(String message, Keyword category, IPersistentMap data) {
        this(message, category, data, null);
    }

    public CompileException(String message, Keyword category, IPersistentMap data, Throwable cause) {
        super(message, cause);
        this.data = (IPersistentMap) data.assoc(
                Keyword.intern("category"), category
        );
    }

    @Override
    public IPersistentMap getData() {
        return data;
    }
}