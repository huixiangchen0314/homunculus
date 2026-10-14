package top.kzre.homunculus.backend.mock;

/**
 * IEEE 754 binary16 (half)。
 * 内部存位模式，所有构造都按 binary16 规则舍入。
 */
public final class HHalf {
    private final short bits;

    private HHalf(short bits) {
        this.bits = bits;
    }

    public static HHalf of(float f) {
        return new HHalf(floatToHalfBits(f));
    }

    public static HHalf fromBits(short bits) {
        return new HHalf(bits);
    }

    public float getValue(){
        return toFloat();
    }

    public float toFloat() {
        return halfBitsToFloat(bits);
    }

    public short bits() {
        return bits;
    }

    @Override
    public String toString() {
        return "Half(" + toFloat() + ")";
    }

    // ═══════════════════════════════════════════
    // float32 → float16
    // ═══════════════════════════════════════════

    private static short floatToHalfBits(float f) {
        int fbits = Float.floatToIntBits(f);
        int sign  = (fbits >>> 31) & 0x1;
        int exp   = (fbits >>> 23) & 0xFF;
        int mant  = fbits & 0x7FFFFF;

        // NaN / Inf
        if (exp == 0xFF) {
            if (mant == 0) {
                return (short) ((sign << 15) | 0x7C00);          // Inf
            }
            // NaN：保留 1 个尾数位，避免变成 Inf
            int halfMant = (mant >>> 13) | 0x1;
            return (short) ((sign << 15) | 0x7C00 | halfMant);
        }

        // 指数重新偏置：127 → 15
        int newExp = exp - 127 + 15;

        // 上溢 → Inf
        if (newExp >= 31) {
            return (short) ((sign << 15) | 0x7C00);
        }

        // 下溢 / 非规格化数
        if (newExp <= 0) {
            if (newExp < -10) {
                return (short) (sign << 15);                     // ±0
            }
            mant |= 0x800000;                                    // 隐含的 1
            int shift = 14 - newExp;
            int halfMant  = mant >>> shift;
            int remainder = mant & ((1 << shift) - 1);
            int halfway   = 1 << (shift - 1);
            // 舍入到最近偶数
            if (remainder > halfway
                    || (remainder == halfway && (halfMant & 1) == 1)) {
                halfMant++;
            }
            return (short) ((sign << 15) | halfMant);
        }

        // 规格化数
        int halfMant  = mant >>> 13;
        int remainder = mant & 0x1FFF;
        int halfway   = 0x1000;
        if (remainder > halfway
                || (remainder == halfway && (halfMant & 1) == 1)) {
            halfMant++;
            if (halfMant == 0x400) {
                halfMant = 0;
                newExp++;
                if (newExp >= 31) {
                    return (short) ((sign << 15) | 0x7C00);
                }
            }
        }

        return (short) ((sign << 15) | (newExp << 10) | halfMant);
    }

    // ═══════════════════════════════════════════
    // float16 → float32
    // ═══════════════════════════════════════════

    private static float halfBitsToFloat(short h) {
        int hbits = h & 0xFFFF;
        int sign  = (hbits >>> 15) & 0x1;
        int exp   = (hbits >>> 10) & 0x1F;
        int mant  = hbits & 0x3FF;

        int fsign = sign << 31;

        // 零 / 非规格化数
        if (exp == 0) {
            if (mant == 0) {
                return Float.intBitsToFloat(fsign);              // ±0
            }
            // 规格化非规格化数
            int e = -1;
            int m = mant;
            while ((m & 0x400) == 0) {
                m <<= 1;
                e--;
            }
            m &= 0x3FF;
            int fexp  = e + 127 - 15 + 1;
            int fmant = m << 13;
            return Float.intBitsToFloat(fsign | (fexp << 23) | fmant);
        }

        // Inf / NaN
        if (exp == 0x1F) {
            if (mant == 0) {
                return Float.intBitsToFloat(fsign | 0x7F800000); // ±Inf
            }
            return Float.intBitsToFloat(fsign | 0x7F800000 | (mant << 13));
        }

        // 规格化数
        int fexp  = exp - 15 + 127;
        int fmant = mant << 13;
        return Float.intBitsToFloat(fsign | (fexp << 23) | fmant);
    }
}