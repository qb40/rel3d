'Environment Mapping supplement for the vector article
'SetVideoSeg By Plasma
'Orig code for Polyfiller by CGI Joe

'Relsoft 2004
'Rel.Betterwebber.com

DECLARE SUB TextureTri (ox1%, oy1%, ou1%, ov1%, ox2%, oy2%, ou2%, ov2%, ox3%, oy3%, ou3%, ov3%, TSEG%, TOFF%)
DECLARE SUB CalcTextures (Poly() AS ANY, v() AS ANY, Textsize%)
DECLARE SUB CalcNormals (Model() AS ANY, Poly() AS ANY, v() AS ANY, v2() AS ANY)
DECLARE SUB DrawCube (Model() AS ANY, Vector() AS ANY)
DECLARE SUB LoadCube (Model() AS ANY, Vector() AS ANY)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB RotateAndProject (Model() AS ANY, AngleX%, AngleY%, AngleZ%)
DECLARE SUB RotNormals (v() AS ANY, v2() AS ANY, AngleX%, AngleY%, AngleZ%)
DEFINT A-Z
REM $DYNAMIC

TYPE Point3d
        x       AS SINGLE                   'Normal 3d coords
        y       AS SINGLE
        Z       AS SINGLE
        xr      AS SINGLE                   'Rotated coords
        yr      AS SINGLE
        Zr      AS SINGLE
        scrx    AS INTEGER                  'Translated and projected
        scry    AS INTEGER                  '2d Coords
END TYPE

TYPE PolyType
        P1      AS INTEGER              'vertex
        P2      AS INTEGER
        P3      AS INTEGER
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
        Z      AS SINGLE
END TYPE

CONST TSIZE% = 64                           'Size of texture
CONST LENS = 256                            'Z
CONST XCENTER = 160                         '??
CONST YCENTER = 100                         '??


CONST PI = 3.14151693#

REDIM SHARED Vpage(32009)  AS INTEGER       'Virtual page
DIM SHARED Lcos(359) AS SINGLE              'Lookup tables for speed
DIM SHARED Lsin(359) AS SINGLE

REDIM SHARED CubeModel(1) AS Point3d
REDIM SHARED CubePoly(1) AS PolyType
REDIM SHARED CubeNormal(1) AS VectorType          'Orig Face normal
REDIM SHARED CubeVTXNormal(1) AS VectorType       'Orig Vertex normal
REDIM SHARED CubeVTXNormal2(1) AS VectorType      'Rotated Vertex normal
DIM SHARED ThetaX, ThetaY, ThetaZ           'angle of rotation
DIM SHARED camx%, camy%, camz%              'camera offset

Size% = ((TSIZE% * TSIZE%) + 4) \ 2         'Calc array big enough for 64*64 texture
DIM SHARED Texture%(Size%)
DIM SHARED TSEG%, TOFF%             'Segment:offset of textures(needed by
                                    'TextureTri sub

'PreCalc sin and cos lookuptable

FOR i = 0 TO 359
    a! = i * PI / 180
    Lcos(i) = COS(a!)
    Lsin(i) = SIN(a!)
NEXT i

'Setup cube including normals

LoadCube CubeModel(), CubePoly()
RotateAndProject CubeModel(), 0, 0, 0           'may not be  needed
CalcNormals CubeModel(), CubePoly(), CubeNormal(), CubeVTXNormal()

CLS
SCREEN 13
RANDOMIZE TIMER

'Make a nice wrap-around pal (I use this for Plasmas) :*)

j! = 255 / 360 * 1
k! = 255 / 360 * 3
l! = 255 / 360 * 2
FOR i% = 0 TO 255
    OUT &H3C8, i%
    m% = INT(a!)
    n% = INT(b!)
    o% = INT(c!)
    r% = 63 * ABS(SIN(m% * PI / 180))
    g% = 63 * ABS(SIN(n% * PI / 180))
    b% = 63 * ABS(SIN(o% * PI / 180))
    a! = a! + j!
    b! = b! + k!
    c! = c! + l!
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
NEXT


'do some nice texture
'Generate texture
FOR y% = -TSIZE% \ 2 TO TSIZE% \ 2 - 1
    yy% = ABS(y%)
    FOR x% = -TSIZE% \ 2 TO TSIZE% \ 2 - 1
        xx% = ABS(x%)
        c% = SIN(xx% / 12) * 132 + SIN(yy% / 12) * 256 + SIN((yy% + xx%) / 8) * 64
        PSET (x% + TSIZE% \ 2, y% + TSIZE% \ 2), c%
    NEXT x%
NEXT y%

'Get our texture
GET (0, 0)-(TSIZE% - 1, TSIZE% - 1), Texture%

'Set up texture variables for easy referencing
TSEG% = VARSEG(Texture%(0))
TOFF% = VARPTR(Texture%(0))


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

     'Rotate normals
     'CubeVTXNormal()=Orig
     'CubeVTXNormal2()=Rotated. This are the normal used in env mapping
     RotNormals CubeVTXNormal(), CubeVTXNormal2(), ThetaX, ThetaY, ThetaZ

     'Calculate the texture using normals
     CalcTextures CubePoly(), CubeVTXNormal2(), TSIZE%

     'Set draw to buffer
     SetVideoSeg Layer
     LINE (0, 0)-(319, 199), 0, BF          'cls

     'Draw
     DrawCube CubeModel(), CubePoly()
     PUT (0, 0), Texture(0), PSET   'Draw texture

     'Set draw to screen
     SetVideoSeg &HA000
     PUT (0, 0), Vpage(6), PSET     'Pcopy

LOOP UNTIL INKEY$ <> ""


END

'numPoints
NumPoints:
DATA 8


'vertices of Cube
VertexData:
DATA -50,50,50
DATA 50,50,50
DATA 50,50,-50
DATA -50,50,-50
DATA -50,-50,50
DATA 50,-50,50
DATA 50,-50,-50
DATA -50,-50,-50
     
NumPoly:
DATA 12
ConnectData:
DATA 5,4,0, 5,0,1
DATA 6,2,3, 3,7,6
DATA 6,5,1, 6,1,2
DATA 7,0,4, 7,3,0
DATA 6,7,4, 6,4,5
DATA 0,3,2, 1,0,2

REM $STATIC
SUB CalcNormals (Model() AS Point3d, Poly() AS PolyType, v() AS VectorType, v2() AS VectorType)
'Calculates the face and vertex normals of all the polygons of our model

'Face normals
FOR i = 1 TO UBOUND(v)

    P1 = Poly(i).P1             'vertex
    P2 = Poly(i).P2
    P3 = Poly(i).P3
    x1 = Model(P1).x            'coords
    x2 = Model(P2).x
    x3 = Model(P3).x
    y1 = Model(P1).y
    y2 = Model(P2).y
    y3 = Model(P3).y
    Z1 = Model(P1).Z
    Z2 = Model(P2).Z
    Z3 = Model(P3).Z

    ax! = x2 - x1               'vectors
    bx! = x3 - x2
    ay! = y2 - y1
    by! = y3 - y2
    az! = Z2 - Z1
    bz! = Z3 - Z2

    'Cross product
    xnormal! = ay! * bz! - az! * by!
    ynormal! = az! * bx! - ax! * bz!
    znormal! = ax! * by! - ay! * bx!

    'Normalize
    Mag! = SQR(xnormal! ^ 2 + ynormal! ^ 2 + znormal! ^ 2)
    IF Mag! <> 0 THEN
        xnormal! = xnormal! / Mag!
        ynormal! = ynormal! / Mag!
        znormal! = znormal! / Mag!
    END IF

    v(i).x = xnormal!                  'final face normal
    v(i).y = ynormal!
    v(i).Z = znormal!

NEXT i


'VertexNormals
'Algo: since we cannot find a normal to a point(doh?!!!) we find adjacent
        'planes(faces) of the polyhedra that a vertex is located then adding
        'all the facenormals of all the faces that a particular vertex is
        'located.
FOR i = 1 TO UBOUND(Model)
    xnormal! = 0
    ynormal! = 0
    znormal! = 0
    FaceFound = 0
    FOR j = 1 TO UBOUND(Poly)
        IF Poly(j).P1 = i OR Poly(j).P2 = i OR Poly(j).P3 = i THEN
            xnormal! = xnormal! + v(j).x
            ynormal! = ynormal! + v(j).y
            znormal! = znormal! + v(j).Z
            FaceFound = FaceFound + 1
        END IF
    NEXT j
    xnormal! = xnormal! / FaceFound
    ynormal! = ynormal! / FaceFound
    znormal! = znormal! / FaceFound

    'Normalize
    Mag! = SQR(xnormal! ^ 2 + ynormal! ^ 2 + znormal! ^ 2)
    IF Mag! <> 0 THEN
        xnormal! = xnormal! / Mag!
        ynormal! = ynormal! / Mag!
        znormal! = znormal! / Mag!
    END IF

    v2(i).x = xnormal!              'Vertex normals
    v2(i).y = ynormal!
    v2(i).Z = znormal!
NEXT i

END SUB

SUB CalcTextures (Poly() AS PolyType, v() AS VectorType, Textsize%)
'Calculates the texture coordinates of the cube every frame
'using vertex normals for a relective surface.

'TextureCoord = TSize/2+Vertexnormal*Tsize/2

Tdiv2! = Textsize% / 2
FOR i = 1 TO UBOUND(Poly)
        u1! = Tdiv2! + v(Poly(i).P1).x * Tdiv2!     'Vertex1
        v1! = Tdiv2! + v(Poly(i).P1).y * Tdiv2!
        u2! = Tdiv2! + v(Poly(i).P2).x * Tdiv2!      'Vertex2
        v2! = Tdiv2! + v(Poly(i).P2).y * Tdiv2!
        u3! = Tdiv2! + v(Poly(i).P3).x * Tdiv2!      'Vertex3
        v3! = Tdiv2! + v(Poly(i).P3).y * Tdiv2!
        Poly(i).u1 = u1!
        Poly(i).v1 = v1!
        Poly(i).u2 = u2!
        Poly(i).v2 = v2!
        Poly(i).u3 = u3!
        Poly(i).v3 = v3!
NEXT i

END SUB

SUB DrawCube (Model() AS Point3d, Poly() AS PolyType) STATIC

FOR i = 1 TO UBOUND(Poly)
    x1 = Model(Poly(i).P1).scrx       'Get triangles from "projected"
    x2 = Model(Poly(i).P2).scrx       'X and Y coords since Znormal
    x3 = Model(Poly(i).P3).scrx       'Does not require a Z coord
    y1 = Model(Poly(i).P1).scry       'V1= Point1 connected to V2 then
    y2 = Model(Poly(i).P2).scry       'V2 to V3 and so on...
    y3 = Model(Poly(i).P3).scry

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

SUB LoadCube (Model() AS Point3d, Tri() AS PolyType) STATIC

RESTORE NumPoints
READ MaxVertex
REDIM Model(1 TO MaxVertex) AS Point3d


RESTORE VertexData
FOR v = 1 TO UBOUND(Model)
    READ Xt, Yt, Zt
    Model(v).x = Xt
    Model(v).y = Yt
    Model(v).Z = Zt
NEXT v

RESTORE NumPoly
READ MaxPoly
REDIM Tri(MaxPoly) AS PolyType

RESTORE ConnectData

FOR v = 1 TO UBOUND(Tri)
        READ T1, T2, T3
        Tri(v).P1 = T1 + 1
        Tri(v).P2 = T2 + 1
        Tri(v).P3 = T3 + 1
NEXT v

'Face normals
REDIM CubeNormal(1 TO UBOUND(Tri)) AS VectorType

'Vertex normals
REDIM CubeVTXNormal(1 TO UBOUND(Model)) AS VectorType
REDIM CubeVTXNormal2(1 TO UBOUND(Model)) AS VectorType




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
        Z! = Model(i).Z

        RotX! = (x! * xx! + y! * xy! + Z! * xz!) - camx%
        RotY! = (x! * yx! + y! * yy! + Z! * yz!) - camy%
        RotZ! = (x! * zx! + y! * zy! + Z! * zz!) - camz%

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

SUB RotNormals (v() AS VectorType, v2() AS VectorType, AngleX, AngleY, AngleZ)

'We don't have to calculate normals in real time but we could instead
'rotate them just as we would rotate our points.

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


FOR i = 1 TO UBOUND(v)

        x! = v(i).x       'Load Original normals
        y! = v(i).y
        Z! = v(i).Z

        RotX! = (x! * xx! + y! * xy! + Z! * xz!)
        RotY! = (x! * yx! + y! * yy! + Z! * yz!)
        RotZ! = (x! * zx! + y! * zy! + Z! * zz!)

        v2(i).x = RotX!             'Rotated normals
        v2(i).y = RotY!
        v2(i).Z = RotZ!
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

