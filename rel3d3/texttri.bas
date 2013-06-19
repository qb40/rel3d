'Code supplement for the vector article
'Texture triangle filler
'Original code by CGI Joe(Generic)

'''Relsoft 2004
'''Rel.Betterwebber.com


DECLARE SUB TextureTri (ox1%, oy1%, ou1%, ov1%, ox2%, oy2%, ou2%, ov2%, ox3%, oy3%, ou3%, ov3%, TSEG%, TOFF%)
DECLARE SUB GradColor (col1%, r1%, g1%, b1%, col2%, r2%, g2%, b2%)

DEFINT A-Z

RANDOMIZE TIMER
Size% = ((32 * 32) + 4) \ 2
DIM SHARED Texture%(Size%)

CLS
SCREEN 13

GradColor 0, 0, 0, 0, 128, 32, 63, 32
GradColor 128, 32, 63, 32, 255, 63, 0, 63

FOR y = 0 TO 31
FOR x = 0 TO 31
    PSET (x, y), (x * 15) XOR (y * 15)
NEXT x
NEXT y
GET (0, 0)-(31, 31), Texture%

TSEG% = VARSEG(Texture%(0))
TOFF% = VARPTR(Texture%(0))

C$ = INPUT$(1)

DO

x1 = INT(RND * 320)
y1 = INT(RND * 200)

x2 = INT(RND * 320)
y2 = INT(RND * 200)

x3 = INT(RND * 320)
y3 = INT(RND * 200)

u1 = INT(RND * 33)
u2 = INT(RND * 33)
u3 = INT(RND * 33)

v1 = INT(RND * 33)
v2 = INT(RND * 33)
v3 = INT(RND * 33)

TextureTri x1, y1, u1, v1, x2, y2, u2, v2, x3, y3, u3, v3, TSEG%, TOFF%

LOOP UNTIL INKEY$ <> ""

SUB GradColor (col1, r1, g1, b1, col2, r2, g2, b2)
'Makes a gradient color by interpolating the RGB values of the first
'color index (col1) and col2 by the number of cols.
'Only use this in screen 13

R! = r1
G! = g1
B! = b1
cols = (col2 - col1 + 1)
Rstep! = (r2 - r1 + 1) / cols
Gstep! = (g2 - g1 + 1) / cols
Bstep! = (b2 - b1 + 1) / cols
FOR Col = col1 TO col2
    R! = R! + Rstep!
    G! = G! + Gstep!
    B! = B! + Bstep!
    IF R! > 63 THEN R! = 63
    IF R! < 0 THEN R! = 0
    IF G! > 63 THEN G! = 63
    IF G! < 0 THEN G! = 0
    IF B! > 63 THEN B! = 63
    IF B! < 0 THEN B! = 0
    OUT &H3C8, Col
    OUT &H3C9, FIX(R!)
    OUT &H3C9, FIX(G!)
    OUT &H3C9, FIX(B!)
NEXT Col

END SUB

SUB TextureTri (ox1%, oy1%, ou1%, ov1%, ox2%, oy2%, ou2%, ov2%, ox3%, oy3%, ou3%, ov3%, TSEG%, TOFF%)
'
'
'             /
'        d1 /  |
'         /    |
'         \    |
'          \   |d3
'        d2 \  |
'            \ |
'             \|

'This implementation of the Textured triangle routine is
'the the fast FIXPOINT point version.
'Almost the same as a Flat Triangle routine with an added interpolation
'of the U and V Texture coordinates.  Note the comments with '***. It means
'Addition from the Flat filler code to make it a Texture filler code.

CONST FIXPOINT = 65536

DIM x1 AS INTEGER, y1 AS INTEGER
DIM x2 AS INTEGER, y2 AS INTEGER
DIM x3 AS INTEGER, y3 AS INTEGER

DIM u1 AS INTEGER, v1 AS INTEGER               '***texture coords
DIM u2 AS INTEGER, v2 AS INTEGER
DIM u3 AS INTEGER, v3 AS INTEGER


DIM dx1 AS INTEGER, dy1 AS INTEGER, du1 AS INTEGER, dv1  AS INTEGER      '***
DIM dx2 AS INTEGER, dy2 AS INTEGER, du2 AS INTEGER, dv2  AS INTEGER      '***
DIM dx3 AS INTEGER, dy3 AS INTEGER, du3 AS INTEGER, dv3  AS INTEGER      '***


DIM delta1&, delta2&, delta3&
DIM UDelta1&, UDelta2&, UDelta3&        '***
DIM VDelta1&, VDelta2&, VDelta3&        '***

DIM Lx&, Rx&
DIM Lu&, Ru&                            '***
DIM Lv&, Rv&                            '***

x1 = ox1%
y1 = oy1%
u1 = ou1%                               '***
v1 = ov1%                               '***

x2 = ox2%
y2 = oy2%
u2 = ou2%                               '***
v2 = ov2%                               '***

x3 = ox3%
y3 = oy3%
u3 = ou3%                               '***
v3 = ov3%                               '***



DEF SEG = TSEG%
TEXTUREWID% = (PEEK(TOFF%) + PEEK(TOFF% + 1) * 256) \ 8
TWM1% = TEXTUREWID% - 1

IF y2 < y1 THEN
    SWAP y1, y2
    SWAP x1, x2
    SWAP u1, u2
    SWAP v1, v2
END IF
IF y3 < y1 THEN
    SWAP y3, y1
    SWAP x3, x1
    SWAP u3, u1
    SWAP v3, v1
END IF

IF y3 < y2 THEN
    SWAP y3, y2
    SWAP x3, x2
    SWAP u3, u2
    SWAP v3, v2
END IF

dx1 = x2 - x1
dy1 = y2 - y1
du1 = u2 - u1                           '***
dv1 = v2 - v1
IF dy1 <> 0 THEN
    delta1& = dx1 * FIXPOINT \ dy1
    UDelta1& = du1 * FIXPOINT \ dy1     '***
    VDelta1& = dv1 * FIXPOINT \ dy1     '***
ELSE
    delta1& = 0
    UDelta1& = 0                        '***
    VDelta1& = 0                        '***
END IF

dx2 = x3 - x2
dy2 = y3 - y2
du2 = u3 - u2                           '***
dv2 = v3 - v2
IF dy2 <> 0 THEN
    delta2& = dx2 * FIXPOINT \ dy2
    UDelta2& = du2 * FIXPOINT \ dy2     '***
    VDelta2& = dv2 * FIXPOINT \ dy2     '***
ELSE
    delta2& = 0
    UDelta2& = 0                        '***
    VDelta2& = 0                        '***
END IF


dx3 = x1 - x3
dy3 = y1 - y3
du3 = u1 - u3                           '***
dv3 = v1 - v3
IF dy3 <> 0 THEN
    delta3& = dx3 * FIXPOINT \ dy3
    UDelta3& = du3 * FIXPOINT \ dy3     '***
    VDelta3& = dv3 * FIXPOINT \ dy3     '***
ELSE
    delta3& = 0
    UDelta3& = 0                        '***
    VDelta3& = 0                        '***
END IF


'Flat bottom
'Tup part of triangle

Lx& = x1 * FIXPOINT
Rx& = Lx&

Lu& = u1 * FIXPOINT                     '***Left U
Ru& = Lu&                               '***Right U
Lv& = v1 * FIXPOINT                     '***Left V
Rv& = Lv&                               '***Right V


FOR y% = y1 TO y2 - 1
    Tx1% = Lx& \ FIXPOINT   '\          '***
    Tx2% = Rx& \ FIXPOINT   ' \   Parameters for TextureHline subroutine
    TLu1& = Lu&             ' /         '***
    TLu2& = Ru&             '/
    TLv1& = Lv&            '/           '***
    TLv2& = Rv&
    GOSUB TextureHline                  '***
    Lx& = Lx& + delta1&
    Rx& = Rx& + delta3&
    Lu& = Lu& + UDelta1&                '***
    Ru& = Ru& + UDelta3&                '***
    Lv& = Lv& + VDelta1&                '***
    Rv& = Rv& + VDelta3&                '***
NEXT y%

'Flat top
'Lower part of triangle

Lx& = x2 * FIXPOINT
Lu& = u2 * FIXPOINT                     '***
Lv& = v2 * FIXPOINT                     '***

FOR y% = y2 TO y3
    Tx1% = Lx& \ FIXPOINT   '\          '***
    Tx2% = Rx& \ FIXPOINT   ' \   Parameters for TextureHline subroutine
    TLu1& = Lu&             ' /         '***
    TLu2& = Ru&             '/
    TLv1& = Lv&            '/           '***
    TLv2& = Rv&
    GOSUB TextureHline                  '***
    Lx& = Lx& + delta2&
    Rx& = Rx& + delta3&
    Lu& = Lu& + UDelta2&                '***
    Ru& = Ru& + UDelta3&                '***
    Lv& = Lv& + VDelta2&                '***
    Rv& = Rv& + VDelta3&                '***
NEXT y%

EXIT SUB

'**************************************************************************
'Draws a Textured horizontal line interpolated from u1 to u2, v1 to v2
'Needed variables.
'   Tx1% = integer x1 coordinate
'   Tx2% = integer x2 coordinate
'   y% = integer y coordinate
'   u1& = Long int of u1 *Fixpoint. In this case 2^16
'   u2& = Ditto.
'   v1& = Long int of v1 *Fixpoint. In this case 2^16
'   v2& = Ditto.
'**************************************************************************
TextureHline:    

    Gx1% = Tx1%             'Save values to be safe
    Gx2% = Tx2%
    yy% = y%

    Tu1& = TLu1&
    Tu2& = TLu2&
    Tv1& = TLv1&
    Tv2& = TLv2&

    IF Gx1% > Gx2% THEN             'Sort values
        SWAP Gx1%, Gx2%
        SWAP Tu1&, Tu2&
        SWAP Tv1&, Tv2&
    END IF
    Gdx% = (Gx2% - Gx1%) + 1       'Get Xdelta(+1) for the Div by 0 error
    Udx& = Tu2& - Tu1&             'U delta
    deltaU& = Udx& \ Gdx%          'Interpolate
    Vdx& = Tv2& - Tv1&             'V delta
    deltaV& = Vdx& \ Gdx%          'Interpolate


    u& = Tu1&                       'save values to be safe
    v& = Tv1&
    FOR l% = Gx1% TO Gx2%                   'Rasterizer loop
        u% = (u& \ FIXPOINT) AND TWM1%
        v% = (v& \ FIXPOINT) AND TWM1%
        Pix = PEEK(4 + u% + v% * TEXTUREWID%)
        PSET (l%, yy%), Pix                 'Use poke for speed
        u& = u& + deltaU&                   'DDA
        v& = v& + deltaV&                   'DDA
    NEXT l%

RETURN

END SUB

