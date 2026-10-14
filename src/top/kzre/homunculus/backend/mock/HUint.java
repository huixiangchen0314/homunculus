package top.kzre.homunculus.backend.mock;

public final class HUint {
    private final int bits;   // 位模式存 uint32

    public HUint(int bits) {
        this.bits = bits;
    }

    public long getValue(){
        return toLong();
    }

    public static HUint of(long v) { return new HUint((int) v); }
    public long toLong() { return Integer.toUnsignedLong(bits); }
}