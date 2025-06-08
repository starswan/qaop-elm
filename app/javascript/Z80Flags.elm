module Z80Flags exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import Utils exposing (BitTest, bitMaskFromBit, shiftLeftBy1, shiftLeftBy8, shiftRightBy1, shiftRightBy8)
import Z80Byte exposing (Z80Byte)
import Z80Word exposing (Z80Word, toZ80Word, z80wordToInt)


type alias FlagRegisters =
    { a : Z80Byte
    , ff : Int
    , fr : Int
    , fa : Int
    , fb : Int
    }


type alias IntWithFlags =
    { value : Int
    , flags : FlagRegisters
    }


type alias WordWithFlags =
    { value : Z80Word
    , flags : FlagRegisters
    }


type alias Z80ByteWithFlags =
    { value : Z80Byte
    , flags : FlagRegisters
    }


type FlagFunc
    = AdcA
    | AddA
    | SubA
    | SbcA
    | AndA
    | XorA
    | OrA
    | CpA



--/*
-- lazy flag evaluation:
--
-- state is stored in four variables: Ff, Fr, Fa, Fb
--
--   Z: Fr==0
--   P: parity of Fr&0xFF
--   V: (Fa.7^Fr.7) & (Fb.7^Fr.7)
--   X: Fa.8
-- P/V: X ? P : V
--   N: Fb.9
--   H: Fr.4 ^ Fa.4 ^ Fb.4 ^ Fb.12
--
--		FEDCBA98 76543210
--	Ff	.......C S.5.3...
--	Fr	........ V..H....
--	Fa	.......X V..H....
--	Fb	...H..N. V..H....
--*/


c_FC =
    0x01


c_FN =
    0x02


c_FP =
    0x04


c_F3 =
    0x08


c_FH =
    0x10


c_F5 =
    0x20


c_FZ =
    0x40


c_FS =
    0x80


c_F53 =
    0x28



--private int flags() {
--	int f = Ff, a = Fa, b = Fb, r = Fr;
--	f = f&(FS|F53) | f>>>8&FC;		// S.5.3..C
--	int u = b >> 8;
--	if(r == 0) f |= FZ;			// .Z......
--	int ra = r ^ a;
--	f |= u & FN;				// ......N.
--	f |= (ra ^ b ^ u) & FH;			// ...H....
--	if((a&~0xFF)==0) {
--		a = ra & (b ^ r);
--		b = 5;				// .....V..
--	} else {
--		a = 0x9669*FP;
--		b = (r ^ r>>>4)&0xF;		// .....P..
--	}
--	return f | a>>>b & FP;
--}


get_flags : FlagRegisters -> Z80Byte
get_flags the_flags =
    let
        f1 =
            the_flags.ff

        a1 =
            the_flags.fa

        b1 =
            the_flags.fb

        r =
            the_flags.fr

        lhs_f =
            Bitwise.and f1 (Bitwise.or c_FS c_F53)

        rhs_f =
            Bitwise.and (shiftRightBy8 f1) c_FC

        f2 =
            Bitwise.or lhs_f rhs_f

        u =
            shiftRightBy8 b1

        f3 =
            if r == 0 then
                Bitwise.or f2 c_FZ

            else
                f2

        ra =
            Bitwise.xor r a1

        f4 =
            Bitwise.or f3 (Bitwise.and u c_FN)

        f5 =
            Bitwise.or f4 (Bitwise.and (Bitwise.xor (Bitwise.xor ra b1) u) c_FN)

        ( a, b ) =
            if Bitwise.and a1 0xFFFFFF00 == 0 then
                ( Bitwise.and ra (Bitwise.xor b1 r), 5 )

            else
                ( 0x9669 * c_FP, Bitwise.and (Bitwise.xor r (shiftRightBy 4 r)) 0x0F )
    in
    Bitwise.or f5 (Bitwise.and (shiftRightBy b a) c_FP) |> Z80Byte.fromInt



--	private void flags(int f) {
--		Fr = ~f&FZ;
--		Ff = (f |= f<<8);
--		Fa = 0xFF & (Fb = f&~0x80 | (f&FP)<<5);
--	}


set_flags : Z80Byte -> Z80Byte -> FlagRegisters
set_flags in_flags a =
    let
        flags =
            in_flags |> Z80Byte.toInt

        fr =
            Bitwise.and (Bitwise.complement flags) c_FZ

        ff =
            Bitwise.or flags (shiftLeftBy8 flags)

        fb =
            Bitwise.or (Bitwise.and ff (Bitwise.complement 0x80)) (shiftLeftBy (Bitwise.and ff c_FP) 5)

        fa =
            Bitwise.and 0xFF fb
    in
    { a = a, ff = ff, fr = fr, fa = fa, fb = fb }



--private void add(int b)
--{
--	A = Fr = (Ff = (Fa = A) + (Fb = b)) & 0xFF;
--}


z80_add : Z80Byte -> FlagRegisters -> FlagRegisters
z80_add byte_b the_flags =
    let
        b =
            byte_b |> Z80Byte.toInt

        fa =
            the_flags.a |> Z80Byte.toInt

        fb =
            b

        ff =
            fa + fb

        fr =
            Bitwise.and ff 0xFF
    in
    { the_flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr |> Z80Byte.fromInt }



--private void adc(int b)
--{
--	A = Fr = (Ff = (Fa = A) + (Fb = b) + (Ff>>>8 & FC)) & 0xFF;
--}


adc : Z80Byte -> FlagRegisters -> FlagRegisters
adc byte_b the_flags =
    let
        b =
            byte_b |> Z80Byte.toInt

        fa =
            the_flags.a |> Z80Byte.toInt

        fb =
            b

        ff =
            fa + fb + Bitwise.and (shiftRightBy8 the_flags.ff) c_FC

        fr =
            Bitwise.and ff 0xFF
    in
    { the_flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr |> Z80Byte.fromInt }



--private void sub(int b)
--{
--    Fb = ~b;
--    A = Fr = (Ff = (Fa = A) - b) & 0xFF;
--}


z80_sub : Z80Byte -> FlagRegisters -> FlagRegisters
z80_sub byte_b flagRegs =
    let
        b =
            byte_b |> Z80Byte.toInt

        fb =
            complement b

        fa =
            flagRegs.a |> Z80Byte.toInt

        ff =
            fa - b

        fr =
            Bitwise.and ff 0xFF
    in
    { flagRegs | fa = fa, fb = fb, ff = ff, fr = fr, a = fr |> Z80Byte.fromInt }



--private void sbc(int b)
--{
--    Fb = ~b;
--    A = Fr = (Ff = (Fa = A) - b - (Ff>>>8 & FC)) & 0xFF;
--}


sbc : Z80Byte -> FlagRegisters -> FlagRegisters
sbc byte_b flagRegs =
    let
        b =
            byte_b |> Z80Byte.toInt

        fb =
            complement b

        fa =
            flagRegs.a |> Z80Byte.toInt

        ff =
            fa - b - Bitwise.and (shiftRightBy8 flagRegs.ff) c_FC

        fr =
            Bitwise.and ff 0xFF
    in
    { flagRegs | fa = fa, fb = fb, ff = ff, fr = fr, a = fr |> Z80Byte.fromInt }



--private void cp(int b)
--{
--    int r = (Fa = A) - b;
--    Fb = ~b;
--    Ff = r&~F53 | b&F53;
--    Fr = r&0xFF;
--}


z80_cp : Z80Byte -> FlagRegisters -> FlagRegisters
z80_cp byte_b flagRegs =
    let
        b =
            byte_b |> Z80Byte.toInt

        fa =
            flagRegs.a |> Z80Byte.toInt

        r =
            fa - b

        fb =
            complement b

        ff =
            Bitwise.or (Bitwise.and r (complement c_F53)) (Bitwise.and b c_F53)

        fr =
            Bitwise.and r 0xFF
    in
    { flagRegs | fr = fr, ff = ff, fb = fb, fa = fa }



--private void and(int b)
--{
--    Fa = ~(A = Ff = Fr = A & b); Fb = 0;
--}


z80_and : Z80Byte -> FlagRegisters -> FlagRegisters
z80_and byte_b flagRegs =
    let
        b =
            byte_b |> Z80Byte.toInt

        fr =
            flagRegs.a |> Z80Byte.toInt |> Bitwise.and b

        ff =
            fr

        a =
            ff

        fa =
            complement a
    in
    { flagRegs | fa = fa, fb = 0, ff = ff, fr = fr, a = a |> Z80Byte.fromInt }



--private void or(int b)
--{
--    Fa = (A = Ff = Fr = A | b) | 0x100;
--    Fb = 0;
--}


z80_or : Z80Byte -> FlagRegisters -> FlagRegisters
z80_or byte_b flagRegs =
    let
        b =
            byte_b |> Z80Byte.toInt

        fr =
            flagRegs.a |> Z80Byte.toInt |> Bitwise.or b

        ff =
            fr

        a =
            ff

        fa =
            Bitwise.or a 0x0100
    in
    { flagRegs | fa = fa, fb = 0, ff = ff, fr = fr, a = a |> Z80Byte.fromInt }



--private void xor(int b)
--{
--    Fa = (A = Ff = Fr = A ^ b) | 0x100;
--    Fb = 0
--}


z80_xor : Z80Byte -> FlagRegisters -> FlagRegisters
z80_xor byte_b flagRegs =
    let
        b =
            byte_b |> Z80Byte.toInt

        fr =
            flagRegs.a |> Z80Byte.toInt |> Bitwise.xor b

        ff =
            fr

        a =
            ff

        fa =
            Bitwise.or a 0x0100
    in
    { flagRegs | fa = fa, fb = 0, ff = ff, fr = fr, a = a |> Z80Byte.fromInt }



--private void cpl()
--{
--    Ff = Ff&~F53 | (A ^= 0xFF)&F53;
--    Fb |= ~0x80; Fa = Fa&~FH | ~Fr&FH; // set H, N
--}


cpl : FlagRegisters -> FlagRegisters
cpl flagRegs =
    let
        new_a =
            flagRegs.a |> Z80Byte.toInt |> Bitwise.xor 0xFF

        ff =
            Bitwise.or (Bitwise.and flagRegs.ff (complement c_F53)) (Bitwise.and new_a c_F53)

        fb =
            Bitwise.or flagRegs.fb (complement 0x80)

        fa =
            Bitwise.or (Bitwise.and flagRegs.fa (complement c_FH)) (Bitwise.and (complement flagRegs.fr) c_FH)
    in
    { flagRegs | a = new_a |> Z80Byte.fromInt, ff = ff, fb = fb, fa = fa }


inc : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
inc v_byte flagRegs =
    let
        v =
            v_byte |> Z80Byte.toInt

        ff =
            Bitwise.and flagRegs.ff 0x0100

        vv =
            Bitwise.and (v + 1) 0xFF
    in
    Z80ByteWithFlags (vv |> Z80Byte.fromInt) { flagRegs | ff = Bitwise.or ff vv, fb = 1, fa = v, fr = vv }


dec : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
dec v_byte flagRegs =
    let
        v =
            v_byte |> Z80Byte.toInt

        ff =
            Bitwise.and flagRegs.ff 0x0100

        vv =
            Bitwise.and (v - 1) 0xFF
    in
    Z80ByteWithFlags (vv |> Z80Byte.fromInt) { flagRegs | ff = Bitwise.or ff vv, fb = -1, fa = v, fr = vv }



--private void bit(int n, int v)
--{
--    int m = v & 1<<n;
--    Ff = Ff&~0xFF | v&F53 | m;
--    Fa = ~(Fr = m);
--    Fb = 0;
--}


bit : Int -> Int -> FlagRegisters -> FlagRegisters
bit n v flagRegs =
    -- used by group_xy_cb only
    let
        m =
            Bitwise.and v (shiftLeftBy n 1)

        -- This one is correct - see https://introcs.cs.princeton.edu/java/11precedence/
        -- Bitwise and(&) has higher precedence than Bitwise or(|)
        ff =
            --Bitwise.or (Bitwise.and flagRegs.ff (complement 0xFF)) (Bitwise.or (Bitwise.and v c_F53) m)
            flagRegs.ff |> Bitwise.and (complement 0xFF) |> Bitwise.or (v |> Bitwise.and c_F53) |> Bitwise.or m
    in
    { flagRegs | ff = ff, fr = m, fa = complement m, fb = 0 }


testBit : BitTest -> Z80Byte -> FlagRegisters -> FlagRegisters
testBit testType v_in flagRegs =
    --private void bit(int n, int v)
    --{
    --    int m = v & 1<<n;
    --    Ff = Ff&~0xFF | v&F53 | m;
    --    Fa = ~(Fr = m);
    --    Fb = 0;
    --}
    let
        v =
            v_in |> Z80Byte.toInt

        m =
            testType |> bitMaskFromBit |> Bitwise.and v

        -- This one is correct - see https://introcs.cs.princeton.edu/java/11precedence/
        -- Bitwise and(&) has higher precedence than Bitwise or(|)
        ff =
            --Bitwise.or (Bitwise.and flagRegs.ff (complement 0xFF)) (Bitwise.or (Bitwise.and (v |> Z80Byte.toInt) c_F53) m)
            flagRegs.ff |> Bitwise.and (complement 0xFF) |> Bitwise.or (v |> Bitwise.and c_F53) |> Bitwise.or m
    in
    { flagRegs | ff = ff, fr = m, fa = complement m, fb = 0 }


rot : Int -> FlagRegisters -> FlagRegisters
rot a flagRegs =
    --private void rot(int a)
    --{
    --    Ff = Ff&0xD7 | a&0x128;
    --    Fb &= 0x80; Fa = Fa&~FH | Fr&FH; // reset H, N
    --    A = a&0xFF;
    --}
    let
        ff =
            Bitwise.or (Bitwise.and flagRegs.ff 0x07) (Bitwise.and a 0x0128)

        fb =
            Bitwise.and flagRegs.fb 0x80

        fa =
            Bitwise.or (Bitwise.and flagRegs.fa (Bitwise.complement c_FH)) (Bitwise.and flagRegs.fr c_FH)
    in
    { flagRegs | ff = ff, fb = fb, fa = fa, a = Bitwise.and a 0xFF |> Z80Byte.fromInt }


shifter : Int -> Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter o v_in flagRegs =
    case Bitwise.and o 7 of
        0 ->
            flagRegs |> shifter0 v_in

        1 ->
            flagRegs |> shifter1 v_in

        2 ->
            flagRegs |> shifter2 v_in

        3 ->
            flagRegs |> shifter3 v_in

        4 ->
            flagRegs |> shifter4 v_in

        5 ->
            flagRegs |> shifter5 v_in

        6 ->
            flagRegs |> shifter6 v_in

        _ ->
            flagRegs |> shifter7 v_in


shifter_v : Int -> FlagRegisters -> Z80ByteWithFlags
shifter_v v flagRegs =
    let
        fr =
            Bitwise.and 0xFF v
    in
    Z80ByteWithFlags (fr |> Z80Byte.fromInt) { flagRegs | ff = v, fr = fr, fb = 0, fa = Bitwise.or 0x0100 fr }


shifter0 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter0 v_in flagRegs =
    flagRegs |> shifter_v (shiftRightBy 7 ((v_in |> Z80Byte.toInt) * 0x0101))


shifter1 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter1 v_in flagRegs =
    flagRegs |> shifter_v (shiftRightBy 24 ((v_in |> Z80Byte.toInt) * 0x80800000))


shifter2 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter2 v_in flagRegs =
    flagRegs |> shifter_v (Bitwise.or (shiftLeftBy1 (v_in |> Z80Byte.toInt)) (Bitwise.and (shiftRightBy8 flagRegs.ff) 1))


shifter3 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter3 v_in flagRegs =
    flagRegs |> shifter_v (shiftRightBy1 (Bitwise.or ((v_in |> Z80Byte.toInt) * 0x0201) (Bitwise.and flagRegs.ff 0x0100)))


shifter4 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter4 v_in flagRegs =
    flagRegs |> shifter_v (shiftLeftBy1 (v_in |> Z80Byte.toInt))


shifter5 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter5 v_byte flagRegs =
    let
        v_in =
            v_byte |> Z80Byte.toInt
    in
    flagRegs |> shifter_v (Bitwise.or (Bitwise.or (shiftRightBy1 v_in) (Bitwise.and v_in 0x80)) (shiftLeftBy8 v_in))


shifter6 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter6 v_in flagRegs =
    flagRegs |> shifter_v (Bitwise.or (shiftLeftBy1 (v_in |> Z80Byte.toInt)) 1)


shifter7 : Z80Byte -> FlagRegisters -> Z80ByteWithFlags
shifter7 v_in flagRegs =
    flagRegs |> shifter_v (shiftRightBy1 ((v_in |> Z80Byte.toInt) * 0x0201))



--private int add16(int a, int b)
--{
--	int r = a + b;
--	Ff = Ff & FS | r>>>8 & 0x128;
--	Fa &= ~FH;
--	Fb = Fb&0x80 | ((r ^ a ^ b)>>>8 ^ Fr) & FH;
--	MP = a+1;
--	time += 7;
--	return (char)r;
--}


add16 : Z80Word -> Z80Word -> FlagRegisters -> WordWithFlags
add16 in_a in_b main_flags =
    let
        a =
            in_a |> z80wordToInt

        b =
            in_b |> z80wordToInt

        r =
            a + b

        ff =
            Bitwise.or (Bitwise.and main_flags.ff c_FS) (Bitwise.and (shiftRightBy8 r) 0x0128)

        fa =
            Bitwise.and main_flags.fa (complement c_FH)

        shiftright =
            shiftRightBy8 (Bitwise.xor (Bitwise.xor r a) b)

        shiftrightxor =
            Bitwise.xor shiftright main_flags.fr

        fb_rhs =
            Bitwise.and shiftrightxor c_FH

        fb =
            Bitwise.or (Bitwise.and main_flags.fb 0x80) fb_rhs

        new_flags =
            { main_flags | ff = ff, fa = fa, fb = fb }
    in
    --IntWithFlags (Bitwise.and r 0xFFFF) new_flags
    WordWithFlags (r |> toZ80Word) new_flags



--	/* instructions */
--
--	// may be wrong. see http://scratchpad.wikia.com/wiki/Z80
--	private void scf_ccf(int x)
--	{
--		Fa &= ~FH;
--		Fb = Fb&0x80 | (x>>>4 ^ Fr) & FH;
--		Ff = 0x100 ^ x | Ff&FS | A&F53;
--	}


scf_ccf : Int -> FlagRegisters -> FlagRegisters
scf_ccf x flagRegs =
    let
        fa =
            Bitwise.and flagRegs.fa (complement c_FH)

        fb =
            Bitwise.or (Bitwise.and flagRegs.fb 0x80) (Bitwise.and (Bitwise.xor (shiftRightBy 4 x) flagRegs.fr) c_FH)

        ff =
            Bitwise.or (Bitwise.or (Bitwise.xor 0x0100 x) (Bitwise.and flagRegs.ff c_FS)) (flagRegs.a |> Z80Byte.toInt |> Bitwise.and c_F53)
    in
    { flagRegs | fa = fa, fb = fb, ff = ff }



--
--	private void daa()
--	{
--		int h = (Fr ^ Fa ^ Fb ^ Fb>>8) & FH;
--
--		int d = 0;
--		if((A | Ff&0x100) > 0x99) d = 0x160;
--		if((A&0xF | h) > 9) d += 6;
--
--		Fa = A | 0x100; // parity
--		if((Fb & 0x200)==0)
--			A += (Fb = d);
--		else {
--			A -= d; Fb = ~d;
--		}
--		Ff = (Fr = A &= 0xFF) | d&0x100;
--	}


daa : FlagRegisters -> FlagRegisters
daa flagRegs =
    let
        h =
            Bitwise.and (Bitwise.xor (Bitwise.xor (Bitwise.xor flagRegs.fr flagRegs.fa) flagRegs.fb) (shiftRightBy8 flagRegs.fb)) c_FH

        d0 =
            if Bitwise.or (flagRegs.a |> Z80Byte.toInt) (Bitwise.and flagRegs.ff 0x0100) > 0x99 then
                0x0160

            else
                0

        d =
            if Bitwise.or (Bitwise.and (flagRegs.a |> Z80Byte.toInt) 0x0F) h > 9 then
                d0 + 6

            else
                d0

        fa =
            Bitwise.or (flagRegs.a |> Z80Byte.toInt) 0x0100

        ( a0, fb ) =
            if Bitwise.and flagRegs.fb 0x0200 == 0 then
                ( (flagRegs.a |> Z80Byte.toInt) + d, d )

            else
                ( (flagRegs.a |> Z80Byte.toInt) - d, complement d )

        a =
            Bitwise.and a0 0xFF

        fr =
            a

        ff =
            Bitwise.or fr (Bitwise.and d 0x0100)
    in
    { flagRegs | fr = fr, a = a |> Z80Byte.fromInt, fb = fb, fa = fa, ff = ff }



--	void af(int v) {A = v>>>8; flags(v&0xFF);}


set_af : Z80Word -> FlagRegisters
set_af v =
    let
        a =
            --shiftRightBy8 v |> Z80Byte.fromInt
            v.high

        flagRegs =
            --Bitwise.and v 0xFF
            v.low
    in
    set_flags flagRegs a


get_af : FlagRegisters -> Z80Word
get_af z80_flags =
    --Bitwise.or (z80_flags.a |> Z80Byte.toInt |> shiftLeftBy8) (get_flags z80_flags)
    { high = z80_flags.a, low = get_flags z80_flags }


f_szh0n0p : Z80Byte -> FlagRegisters -> FlagRegisters
f_szh0n0p r flags =
    let
        fr =
            r |> Z80Byte.toInt

        ff =
            Bitwise.or (Bitwise.and flags.ff (complement 0xFF)) fr

        fa =
            Bitwise.or fr 0x0100
    in
    { flags | fr = fr, ff = ff, fa = fa, fb = 0 }


changeFlags : FlagFunc -> Z80Byte -> FlagRegisters -> FlagRegisters
changeFlags flagFunc int flags =
    case flagFunc of
        AdcA ->
            flags |> adc int

        AddA ->
            flags |> z80_add int

        SubA ->
            flags |> z80_sub int

        SbcA ->
            flags |> sbc int

        AndA ->
            flags |> z80_and int

        XorA ->
            flags |> z80_xor int

        OrA ->
            flags |> z80_or int

        CpA ->
            flags |> z80_cp int
