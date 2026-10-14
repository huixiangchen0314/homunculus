package top.kzre.homunculus.backend.mock;

public final class HInt {
    private final int value;

    public HInt(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public static HInt of(int i) { return new HInt(i); }
    public int toInt() { return value; }
}