module Z80Flags exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import Utils exposing (BitTest, bitMaskFromBit, shiftLeftBy1, shiftLeftBy8, shiftRightBy1, shiftRightBy8)


type alias FlagRegisters =
    { a : Int
    , ff : Int
    , fr : Int
    , fa : Int
    , fb : Int
    }


type alias IntWithFlags =
    { value : Int
    , flags : FlagRegisters
    }



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


getFlags : FlagRegisters -> Int
getFlags flags =
    let
        ini =
            { f = flags.ff, a = flags.fa, b = flags.fb, r = flags.fr }

        lhs_f =
            Bitwise.and ini.f (Bitwise.or c_FS c_F53)

        rhs_f =
            Bitwise.and (shiftRightBy8 ini.f) c_FC

        old_f =
            Bitwise.or lhs_f rhs_f

        u =
            shiftRightBy8 ini.b

        new_f =
            if ini.r == 0 then
                Bitwise.or old_f c_FZ

            else
                old_f

        ra =
            Bitwise.xor ini.r ini.a

        new_new_f =
            new_f |> Bitwise.or (Bitwise.and u c_FN)

        ra_b_u =
            ra |> Bitwise.xor ini.b |> Bitwise.xor u

        really_new_f =
            new_new_f |> Bitwise.or (Bitwise.and ra_b_u c_FH)

        ( a, b ) =
            if Bitwise.and ini.a (complement 0xFF) == 0 then
                ( Bitwise.and ra (Bitwise.xor ini.b ini.r), 5 )

            else
                ( 0x9669 * c_FP, Bitwise.xor ini.r (ini.r |> shiftRightBy 4) |> Bitwise.and 0x0F )
    in
    really_new_f |> Bitwise.or (a |> shiftRightBy b |> Bitwise.and c_FP)



--	private void flags(int f) {
--		Fr = ~f&FZ;
--		Ff = (f |= f<<8);
--		Fa = 0xFF & (Fb = f&~0x80 | (f&FP)<<5);
--	}


setFlags : Int -> FlagRegisters -> FlagRegisters
setFlags f_in flags =
    let
        -- Bitwise not is higher precedence than bitwise and
        fr =
            f_in |> Bitwise.complement |> Bitwise.and c_FZ

        ff =
            Bitwise.or f_in (shiftLeftBy8 f_in)

        fb_lhs =
            ff |> Bitwise.and (Bitwise.complement 0x80)

        fb_rhs =
            ff |> Bitwise.and c_FP |> shiftLeftBy 5

        fb =
            Bitwise.or fb_lhs fb_rhs

        fa =
            Bitwise.and 0xFF fb
    in
    { flags | ff = ff, fr = fr, fa = fa, fb = fb }



--private void add(int b)
--{
--	A = Fr = (Ff = (Fa = A) + (Fb = b)) & 0xFF;
--}


z80_add : Int -> FlagRegisters -> FlagRegisters
z80_add b the_flags =
    let
        fa =
            the_flags.a

        fb =
            b

        ff =
            fa + fb

        fr =
            Bitwise.and ff 0xFF
    in
    { the_flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }



--private void adc(int b)
--{
--	A = Fr = (Ff = (Fa = A) + (Fb = b) + (Ff>>>8 & FC)) & 0xFF;
--}


adc : Int -> FlagRegisters -> FlagRegisters
adc b the_flags =
    let
        fa =
            the_flags.a

        fb =
            b

        ff =
            fa + fb + Bitwise.and (shiftRightBy8 the_flags.ff) c_FC

        fr =
            Bitwise.and ff 0xFF
    in
    { the_flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }



--private void sub(int b)
--{
--    Fb = ~b;
--    A = Fr = (Ff = (Fa = A) - b) & 0xFF;
--}


z80_sub : Int -> FlagRegisters -> FlagRegisters
z80_sub b flagRegs =
    let
        fb =
            complement b

        fa =
            flagRegs.a

        ff =
            fa - b

        fr =
            Bitwise.and ff 0xFF
    in
    { flagRegs | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }



--private void sbc(int b)
--{
--    Fb = ~b;
--    A = Fr = (Ff = (Fa = A) - b - (Ff>>>8 & FC)) & 0xFF;
--}


sbc : Int -> FlagRegisters -> FlagRegisters
sbc b flagRegs =
    let
        fb =
            complement b

        fa =
            flagRegs.a

        ff =
            fa - b - Bitwise.and (shiftRightBy8 flagRegs.ff) c_FC

        fr =
            Bitwise.and ff 0xFF
    in
    { flagRegs | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }



--private void cp(int b)
--{
--    int r = (Fa = A) - b;
--    Fb = ~b;
--    Ff = r&~F53 | b&F53;
--    Fr = r&0xFF;
--}


z80_cp : Int -> FlagRegisters -> FlagRegisters
z80_cp b flagRegs =
    let
        fa =
            flagRegs.a

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


z80_and : Int -> FlagRegisters -> FlagRegisters
z80_and b flagRegs =
    let
        fr =
            Bitwise.and flagRegs.a b

        ff =
            fr

        a =
            ff

        fa =
            complement a
    in
    { flagRegs | fa = fa, fb = 0, ff = ff, fr = fr, a = a }



--private void or(int b)
--{
--    Fa = (A = Ff = Fr = A | b) | 0x100;
--    Fb = 0;
--}


z80_or : Int -> FlagRegisters -> FlagRegisters
z80_or b flagRegs =
    let
        fr =
            Bitwise.or flagRegs.a b

        ff =
            fr

        a =
            ff

        fa =
            Bitwise.or a 0x0100
    in
    { flagRegs | fa = fa, fb = 0, ff = ff, fr = fr, a = a }



--private void xor(int b)
--{
--    Fa = (A = Ff = Fr = A ^ b) | 0x100;
--    Fb = 0
--}


z80_xor : Int -> FlagRegisters -> FlagRegisters
z80_xor b flagRegs =
    let
        fr =
            Bitwise.xor flagRegs.a b

        ff =
            fr

        a =
            ff

        fa =
            Bitwise.or a 0x0100
    in
    { flagRegs | fa = fa, fb = 0, ff = ff, fr = fr, a = a }



--private void cpl()
--{
--    Ff = Ff&~F53 | (A ^= 0xFF)&F53;
--    Fb |= ~0x80; Fa = Fa&~FH | ~Fr&FH; // set H, N
--}


cpl : FlagRegisters -> FlagRegisters
cpl flagRegs =
    let
        new_a =
            Bitwise.xor flagRegs.a 0xFF

        ff =
            Bitwise.or (Bitwise.and flagRegs.ff (complement c_F53)) (Bitwise.and new_a c_F53)

        fb =
            Bitwise.or flagRegs.fb (complement 0x80)

        fa =
            Bitwise.or (Bitwise.and flagRegs.fa (complement c_FH)) (Bitwise.and (complement flagRegs.fr) c_FH)
    in
    { flagRegs | a = new_a, ff = ff, fb = fb, fa = fa }


inc : Int -> FlagRegisters -> IntWithFlags
inc v flagRegs =
    let
        ff =
            Bitwise.and flagRegs.ff 0x0100

        vv =
            Bitwise.and (v + 1) 0xFF
    in
    IntWithFlags vv { flagRegs | ff = Bitwise.or ff vv, fb = 1, fa = v, fr = vv }


dec : Int -> FlagRegisters -> IntWithFlags
dec v flagRegs =
    let
        ff =
            Bitwise.and flagRegs.ff 0x0100

        vv =
            Bitwise.and (v - 1) 0xFF
    in
    IntWithFlags vv { flagRegs | ff = Bitwise.or ff vv, fb = -1, fa = v, fr = vv }



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


testBit : BitTest -> Int -> FlagRegisters -> FlagRegisters
testBit testType v flagRegs =
    --private void bit(int n, int v)
    --{
    --    int m = v & 1<<n;
    --    Ff = Ff&~0xFF | v&F53 | m;
    --    Fa = ~(Fr = m);
    --    Fb = 0;
    --}
    let
        m =
            testType |> bitMaskFromBit |> Bitwise.and v

        -- This one is correct - see https://introcs.cs.princeton.edu/java/11precedence/
        -- Bitwise and(&) has higher precedence than Bitwise or(|)
        ff =
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
            Bitwise.or (Bitwise.and flagRegs.ff 0xD7) (Bitwise.and a 0x0128)

        fb =
            Bitwise.and flagRegs.fb 0x80

        fa_lhs =
            Bitwise.and flagRegs.fa (Bitwise.complement c_FH)

        fa_rhs =
            Bitwise.and flagRegs.fr c_FH

        fa =
            Bitwise.or fa_lhs fa_rhs
    in
    { flagRegs | ff = ff, fb = fb, fa = fa, a = Bitwise.and a 0xFF }


shifter_v : Int -> FlagRegisters -> IntWithFlags
shifter_v v flagRegs =
    let
        fr =
            Bitwise.and 0xFF v
    in
    IntWithFlags fr { flagRegs | ff = v, fr = fr, fb = 0, fa = Bitwise.or 0x0100 fr }


shifter0 : Int -> FlagRegisters -> IntWithFlags
shifter0 v_in flagRegs =
    let
        v =
            v_in + (v_in |> shiftLeftBy8)
    in
    flagRegs |> shifter_v (v |> shiftRightBy 7)


shifter1 : Int -> FlagRegisters -> IntWithFlags
shifter1 v_in flagRegs =
    flagRegs |> shifter_v (shiftRightBy 24 (v_in * 0x80800000))


shifter2 : Int -> FlagRegisters -> IntWithFlags
shifter2 v_in flagRegs =
    flagRegs |> shifter_v (Bitwise.or (shiftLeftBy1 v_in) (Bitwise.and (shiftRightBy8 flagRegs.ff) 1))


shifter3 : Int -> FlagRegisters -> IntWithFlags
shifter3 v_in flagRegs =
    flagRegs |> shifter_v (shiftRightBy1 (Bitwise.or (v_in * 0x0201) (Bitwise.and flagRegs.ff 0x0100)))


shifter4 : Int -> FlagRegisters -> IntWithFlags
shifter4 v_in flagRegs =
    flagRegs |> shifter_v (shiftLeftBy1 v_in)


shifter5 : Int -> FlagRegisters -> IntWithFlags
shifter5 v_in flagRegs =
    flagRegs |> shifter_v (Bitwise.or (Bitwise.or (shiftRightBy1 v_in) (Bitwise.and v_in 0x80)) (shiftLeftBy8 v_in))


shifter6 : Int -> FlagRegisters -> IntWithFlags
shifter6 v_in flagRegs =
    flagRegs |> shifter_v (Bitwise.or (shiftLeftBy1 v_in) 1)


shifter7 : Int -> FlagRegisters -> IntWithFlags
shifter7 v_in flagRegs =
    flagRegs |> shifter_v (shiftRightBy1 (v_in * 0x0201))



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


add16 : Int -> Int -> FlagRegisters -> IntWithFlags
add16 a b main_flags =
    let
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
    IntWithFlags (Bitwise.and r 0xFFFF) new_flags



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
            Bitwise.or (Bitwise.or (Bitwise.xor 0x0100 x) (Bitwise.and flagRegs.ff c_FS)) (Bitwise.and flagRegs.a c_F53)
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
            if Bitwise.or flagRegs.a (Bitwise.and flagRegs.ff 0x0100) > 0x99 then
                0x0160

            else
                0

        d =
            if Bitwise.or (Bitwise.and flagRegs.a 0x0F) h > 9 then
                d0 + 6

            else
                d0

        fa =
            Bitwise.or flagRegs.a 0x0100

        ( a0, fb ) =
            if Bitwise.and flagRegs.fb 0x0200 == 0 then
                ( flagRegs.a + d, d )

            else
                ( flagRegs.a - d, complement d )

        a =
            Bitwise.and a0 0xFF

        fr =
            a

        ff =
            Bitwise.or fr (Bitwise.and d 0x0100)
    in
    { flagRegs | fr = fr, a = a, fb = fb, fa = fa, ff = ff }



--	void af(int v) {A = v>>>8; flags(v&0xFF);}


set_af : Int -> FlagRegisters
set_af v =
    let
        a =
            shiftRightBy8 v

        flagRegs =
            Bitwise.and v 0xFF

        blankFlags =
            { a = a, fr = 0, ff = 0, fa = 0, fb = 0 }
    in
    blankFlags |> setFlags flagRegs


get_af : FlagRegisters -> Int
get_af z80_flags =
    Bitwise.or (shiftLeftBy8 z80_flags.a) (getFlags z80_flags)


f_szh0n0p : Int -> FlagRegisters -> FlagRegisters
f_szh0n0p r flags =
    let
        fr =
            r

        ff =
            Bitwise.or (Bitwise.and flags.ff (complement 0xFF)) fr

        fa =
            Bitwise.or r 0x0100
    in
    { flags | fr = fr, ff = ff, fa = fa, fb = 0 }


jump_nz : FlagRegisters -> Bool
jump_nz z80_flags =
    z80_flags.fr /= 0


jump_z : FlagRegisters -> Bool
jump_z z80_flags =
    z80_flags.fr == 0


jump_nc : FlagRegisters -> Bool
jump_nc z80_flags =
    Bitwise.and z80_flags.ff 0x0100 == 0


jump_c : FlagRegisters -> Bool
jump_c z80_flags =
    Bitwise.and z80_flags.ff 0x0100 /= 0


jump_po : FlagRegisters -> Bool
jump_po z80_flags =
    Bitwise.and (z80_flags |> getFlags) c_FP == 0


jump_pe : FlagRegisters -> Bool
jump_pe z80_flags =
    Bitwise.and (z80_flags |> getFlags) c_FP /= 0


jump_p : FlagRegisters -> Bool
jump_p z80_flags =
    Bitwise.and z80_flags.ff c_FS == 0


jump_m : FlagRegisters -> Bool
jump_m z80_flags =
    Bitwise.and z80_flags.ff c_FS /= 0
