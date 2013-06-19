'Texture mapping (affine) supplement for the vector article
'SetVideoSeg By Plasma
'Orig code for Polyfiller by CGI Joe
'Cube generation by SCM

'''Relsoft 2004
'''Rel.Betterwebber.com

DECLARE SUB LoadCube (Model() AS ANY, Poly() AS ANY, radius%)
DECLARE SUB CalcTextures (Poly() AS ANY, TextSize%)
DECLARE SUB TextureTri (ox1%, oy1%, ou1%, ov1%, ox2%, oy2%, ou2%, ov2%, ox3%, oy3%, ou3%, ov3%, TSEG%, TOFF%)
DECLARE SUB GradColor (col1%, r1%, g1%, b1%, col2%, r2%, g2%, b2%)
DECLARE SUB DrawCube (Model() AS ANY, Vector() AS ANY)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB RotateAndProject (Model() AS ANY, AngleX%, AngleY%, AngleZ%)
DEFINT A-Z
REM $DYNAMIC

TYPE Point3d
        x       AS SINGLE                   'Normal 3d coords
        y       AS SINGLE
        z       AS SINGLE
        xr      AS SINGLE                   'Rotated coords
        yr      AS SINGLE
        Zr      AS SINGLE
        scrx    AS INTEGER                  'Translated and projected
        scry    AS INTEGER                  '2d Coords
END TYPE

TYPE PolyType
        p1      AS INTEGER              'vertex
        p2      AS INTEGER
        p3      AS INTEGER
        u1      AS INTEGER          '\
        v1      AS INTEGER          '  \
        u2      AS INTEGER          '    \ texture coordinates
        v2      AS INTEGER          '    /
        u3      AS INTEGER          '  /
        v3      AS INTEGER          '/
END TYPE

TYPE VectorType                         'Normals are vectors
        x      AS SINGLE
        y      AS SINGLE
        z      AS SINGLE
END TYPE


CONST LENS = 256                            'Z
CONST XCENTER = 160                         '??
CONST YCENTER = 100                         '??


CONST PI = 3.14151693#

REDIM SHARED Vpage(32009)  AS INTEGER       'Virtual page
DIM SHARED Lcos(359) AS SINGLE              'Lookup tables for speed
DIM SHARED Lsin(359) AS SINGLE

REDIM SHARED CubeModel(1) AS Point3d
REDIM SHARED CubePoly(1) AS PolyType
DIM SHARED ThetaX, ThetaY, ThetaZ           'angle of rotation
DIM SHARED camx%, camy%, camz%              'camera offset

Size% = ((32 * 32) + 4) \ 2         'Calc array big enough for 32*32 texture
DIM SHARED Texture%(Size%)
DIM SHARED TSEG%, TOFF%             'Segment:offset of textures(needed by
                                    'TextureTri sub

'PreCalc sin and cos lookuptable

FOR i = 0 TO 359
    A! = i * PI / 180
    Lcos(i) = COS(A!)
    Lsin(i) = SIN(A!)
NEXT i


LoadCube CubeModel(), CubePoly(), 50
RotateAndProject CubeModel(), 0, 0, 0           'may not be  needed
CalcTextures CubePoly(), 32

CLS
SCREEN 13
RANDOMIZE TIMER

'Generate out texture
FOR y = 0 TO 31
FOR x = 0 TO 31
    PSET (x, y), x * 16 XOR y * 16
NEXT x
NEXT y

'Get our texture
GET (0, 0)-(31, 31), Texture%

'Set up texture variables for easy referencing
TSEG% = VARSEG(Texture%(0))
TOFF% = VARPTR(Texture%(0))

'Grey/Purple Scale the Palette
GradColor 0, 0, 0, 0, 128, 32, 63, 32
GradColor 128, 32, 63, 32, 255, 63, 0, 63



ThetaX = INT(RND * 360)
ThetaY = INT(RND * 360)
ThetaZ = INT(RND * 360)

camx% = 0                   'camera
camy% = 0
camz% = 128


Vpage(6) = 2560                    'set up buffer
Vpage(7) = 200
Layer = VARSEG(Vpage(0)) + 1
SetVideoSeg Layer

DO

     'Increment angles
     ThetaX = (ThetaX + 1) MOD 360
     ThetaY = (ThetaY + 1) MOD 360
     ThetaZ = (ThetaZ + 1) MOD 360

     'Rotate model
     RotateAndProject CubeModel(), ThetaX, ThetaY, ThetaZ

     'Set draw to buffer
     SetVideoSeg Layer
     LINE (0, 0)-(319, 199), 0, BF          'cls

     'Draw
     DrawCube CubeModel(), CubePoly()

     'Set draw to screen
     SetVideoSeg &HA000
     PUT (0, 0), Vpage(6), PSET     'Pcopy

LOOP UNTIL INKEY$ <> ""


END

'''Cube polygon connecting points in Quad form
CUBECONNECT:
DATA 0, 1, 2, 3
DATA 1, 5, 6, 2
DATA 5, 4, 7, 6
DATA 4, 0, 3, 7
DATA 4, 5, 1, 0
DATA 3, 2, 6, 7

REM $STATIC
SUB CalcTextures (Poly() AS PolyType, TextSize%)
'calculate vertex coords centering it
'Not needed when loading files from 3d objs as they store their UV's

FOR j = 1 TO UBOUND(Poly)
    u1 = 0
    v1 = 0
    u2 = TextSize%
    v2 = TextSize%
    u3 = TextSize%
    v3 = 0
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
    j = j + 1
    u1 = 0
    v1 = 0
    u2 = 0
    v2 = TextSize%
    u3 = TextSize%
    v3 = TextSize%
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
NEXT j


END SUB

SUB DrawCube (Model() AS Point3d, Poly() AS PolyType) STATIC

FOR i = 1 TO UBOUND(Poly)
    x1 = Model(Poly(i).p1).scrx       'Get triangles from "projected"
    x2 = Model(Poly(i).p2).scrx       'X and Y coords since Znormal
    x3 = Model(Poly(i).p3).scrx       'Does not require a Z coord
    y1 = Model(Poly(i).p1).scry       'V1= Point1 connected to V2 then
    y2 = Model(Poly(i).p2).scry       'V2 to V3 and so on...
    y3 = Model(Poly(i).p3).scry

    znormal = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
    IF znormal < 0 THEN
        u1 = Poly(i).u1         'Texture Coords
        v1 = Poly(i).v1
        u2 = Poly(i).u2
        v2 = Poly(i).v2
        u3 = Poly(i).u3
        v3 = Poly(i).v3
        TextureTri x1, y1, u1, v1, x2, y2, u2, v2, x3, y3, u3, v3, TSEG%, TOFF%
    END IF

NEXT i

END SUB

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
FOR col = col1 TO col2
    R! = R! + Rstep!
    G! = G! + Gstep!
    B! = B! + Bstep!
    IF R! > 63 THEN R! = 63
    IF R! < 0 THEN R! = 0
    IF G! > 63 THEN G! = 63
    IF G! < 0 THEN G! = 0
    IF B! > 63 THEN B! = 63
    IF B! < 0 THEN B! = 0
    OUT &H3C8, col
    OUT &H3C9, FIX(R!)
    OUT &H3C9, FIX(G!)
    OUT &H3C9, FIX(B!)
NEXT col

END SUB

SUB LoadCube (Model() AS Point3d, Poly() AS PolyType, radius)
'Generation code by:
'by Steve McCarthy(SCM)
'Tesselation by me. :*)

REDIM Model(1 TO 8) AS Point3d
Theta! = PI / 4
dTheta! = PI / 2

FOR P = 1 TO 8
  Model(P).x = radius * SGN(COS(Theta!))     ' x
  Model(P).y = radius * SGN(SIN(Theta!))      ' y
  Model(P).z = radius - (radius * 2) * ((P - 1) \ 4)  ' z
  Theta! = Theta! + dTheta!
NEXT P

'Tesselate
REDIM Poly(1 TO 12) AS PolyType
RESTORE CUBECONNECT
j = 1
FOR i = 1 TO 6
    READ p1, p2, p3, p4
    Poly(j).p1 = p4 + 1         'Added 1 cuz I'm too lazy to reedit
    Poly(j).p2 = p2 + 1         'the zero based vertex connection
    Poly(j).p3 = p1 + 1
    j = j + 1
    Poly(j).p1 = p4 + 1
    Poly(j).p2 = p3 + 1
    Poly(j).p3 = p2 + 1
    j = j + 1
NEXT i


END SUB

SUB RotateAndProject (Model() AS Point3d, AngleX, AngleY, AngleZ) STATIC

''Right handed system
''x=goes right
''y=up
''z=goes into you(out of the screen)

'''rotation: counter-clockwise of each axis
''ei.  make yourself perpenicular to the axis
''wave your hand from the center of your body to the left.
''or nake a fist and touch your thumb to your nose, the fingers
''curl counter clocwise.
''That's how it rotates. ;*)


'Precalculate the SIN and COS of each angle
cx! = Lcos(AngleX)
sx! = Lsin(AngleX)
cy! = Lcos(AngleY)
sy! = Lsin(AngleY)
cz! = Lcos(AngleZ)
sz! = Lsin(AngleZ)

'''After2 hours of work, I was able to weed out the constants from
'''Rotate and project N to reduce my muls to 9 instead of 12. woot!!!!

xx! = cy! * cz!
xy! = sx! * sy! * cz! - cx! * sz!
xz! = cx! * sy! * cz! + sx! * sz!

yx! = cy! * sz!
yy! = cx! * cz! + sx! * sy! * sz!
yz! = -sx! * cz! + cx! * sy! * sz!

zx! = -sy!
zy! = sx! * cy!
zz! = cx! * cy!

FOR i = 1 TO UBOUND(Model)

        x! = Model(i).x
        y! = Model(i).y
        z! = Model(i).z

        RotX! = (x! * xx! + y! * xy! + z! * xz!) - camx%
        RotY! = (x! * yx! + y! * yy! + z! * yz!) - camy%
        RotZ! = (x! * zx! + y! * zy! + z! * zz!) - camz%

        Model(i).xr = RotX!
        Model(i).yr = RotY!
        Model(i).Zr = RotZ!


        'Project
        Distance% = (LENS - RotZ!)
        IF Distance% THEN
            Model(i).scrx = XCENTER + (LENS * RotX! / Distance%)
            Model(i).scry = YCENTER - (LENS * RotY! / Distance%)
        ELSE
        END IF
NEXT i

END SUB

SUB SetVideoSeg (Segment) STATIC

DEF SEG

IF VideoAddrOff& = 0 THEN ' First time the sub is called

' We need to find the location of b$AddrC, which holds the graphics
' offset (b$OffC) and segment (b$SegC). Since b$AddrC is in the default
' segment, we can find it by setting it to a certain value, and then
' searching for that value.

SCREEN 13 ' Set b$SegC to A000 (00A0 in memory)
PSET (160, 100), 0 ' Set b$OffC to 7DA0 (not needed in the IDE)

FOR Offset& = 0 TO 32764 ' Search for b$AddrC, which is
IF PEEK(Offset&) = &HA0 THEN ' in the default segment and
IF PEEK(Offset& + 1) = &H7D THEN ' should have a value of
IF PEEK(Offset& + 2) = &H0 THEN ' A0 7D 00 A0.
IF PEEK(Offset& + 3) = &HA0 THEN
VideoAddrOff& = Offset& + 2 ' If we found it, record the
EXIT FOR ' offset of b$SegC and quit
END IF ' looking. (Oddly, changing
END IF ' the b$OffC doesn't seem to
END IF ' do anything, so this is why
END IF ' this sub only changes b$SegC)
NEXT

END IF

' Change b$SegC to the specified Segment

POKE VideoAddrOff&, Segment AND &HFF
POKE VideoAddrOff& + 1, (Segment AND &HFF00&) \ &H100



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

