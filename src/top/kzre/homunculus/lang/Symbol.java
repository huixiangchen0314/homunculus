package top.kzre.homunculus.lang;

public final class Symbol implements Named {
    private final String namespace;
    private final String name;

    public Symbol(String namespace, String name) {
        this.namespace = namespace;
        this.name = name;
    }

    @Override
    public String getNamespace() {
        return namespace;
    }

    @Override
    public String getName() {
        return name;
    }
}
