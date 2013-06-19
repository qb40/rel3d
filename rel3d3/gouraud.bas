'Gouraud shading supplement for the vector article
'SetVideoSeg By Plasma
'Orig code for Polyfiller by CGI Joe

'Relsoft 2004
'Rel.Betterwebber.com

DECLARE SUB GouraudTri (ox1%, oy1%, oc1%, ox2%, oy2%, oc2%, ox3%, oy3%, oc3%)
DECLARE SUB GradColor (col1%, r1%, g1%, b1%, col2%, r2%, g2%, b2%)
DECLARE SUB CalcNormals (Model() AS ANY, Poly() AS ANY, v() AS ANY, v2() AS ANY)
DECLARE SUB DrawCube (Model() AS ANY, Vector() AS ANY)
DECLARE SUB LoadCube (Model() AS ANY, Vector() AS ANY)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB RotateAndProject (Model() AS ANY, AngleX%, AngleY%, AngleZ%)
DECLARE SUB RotNormals (v() AS ANY, v2() AS ANY, AngleX%, AngleY%, AngleZ%)
DEFINT A-Z
REM $DYNAMIC

TYPE Point3d
        X       AS SINGLE                   'Normal 3d coords
        Y       AS SINGLE
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
END TYPE

TYPE VectorType                         'Normals are vectors
        X      AS SINGLE
        Y      AS SINGLE
        Z      AS SINGLE
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
REDIM SHARED CubeNormal(1) AS VectorType          'Orig Face normal
REDIM SHARED CubeVTXNormal(1) AS VectorType       'Orig Vertex normal
REDIM SHARED CubeVTXNormal2(1) AS VectorType      'Rotated Vertex normal
DIM SHARED ThetaX, ThetaY, ThetaZ           'angle of rotation
DIM SHARED LightNormal AS VectorType        'our light normal
DIM SHARED camx%, camy%, camz%              'camera offset

'PreCalc sin and cos lookuptable

FOR i = 0 TO 359
    A! = i * PI / 180
    Lcos(i) = COS(A!)
    Lsin(i) = SIN(A!)
NEXT i

LightNormal.X = 0               'Light normal
LightNormal.Y = -.3
LightNormal.Z = -.6


'Setup cube including normals

LoadCube CubeModel(), CubePoly()
RotateAndProject CubeModel(), 0, 0, 0           'may not be  needed
CalcNormals CubeModel(), CubePoly(), CubeNormal(), CubeVTXNormal()

CLS
SCREEN 13
RANDOMIZE TIMER


'Grey/Purple Scale the Palette
GradColor 0, 0, 0, 0, 255, 63, 50, 24


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
     'CubeVTXNormal2()=Rotated. This are the normal used in shading
     RotNormals CubeVTXNormal(), CubeVTXNormal2(), ThetaX, ThetaY, ThetaZ

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
    x1 = Model(P1).X            'coords
    x2 = Model(P2).X
    x3 = Model(P3).X
    y1 = Model(P1).Y
    y2 = Model(P2).Y
    y3 = Model(P3).Y
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

    v(i).X = xnormal!                  'final face normal
    v(i).Y = ynormal!
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
    FOR J = 1 TO UBOUND(Poly)
        IF Poly(J).P1 = i OR Poly(J).P2 = i OR Poly(J).P3 = i THEN
            xnormal! = xnormal! + v(J).X
            ynormal! = ynormal! + v(J).Y
            znormal! = znormal! + v(J).Z
            FaceFound = FaceFound + 1
        END IF
    NEXT J
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

    v2(i).X = xnormal!              'Vertex normals
    v2(i).Y = ynormal!
    v2(i).Z = znormal!
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
        nx1! = CubeVTXNormal2(Poly(i).P1).X         'Vertex1
        ny1! = CubeVTXNormal2(Poly(i).P1).Y
        nz1! = CubeVTXNormal2(Poly(i).P1).Z
        nx2! = CubeVTXNormal2(Poly(i).P2).X         'Vertex2
        ny2! = CubeVTXNormal2(Poly(i).P2).Y
        nz2! = CubeVTXNormal2(Poly(i).P2).Z
        nx3! = CubeVTXNormal2(Poly(i).P3).X         'Vertex3
        ny3! = CubeVTXNormal2(Poly(i).P3).Y
        nz3! = CubeVTXNormal2(Poly(i).P3).Z

        lx! = LightNormal.X
        ly! = LightNormal.Y
        lz! = LightNormal.Z

        'Calculate dot-products of vertex normals
        Dot1! = (nx1! * lx!) + (ny1! * ly!) + (nz1! * lz!)
        IF Dot1! < 0 THEN           'Limit
            Dot1! = 0
        ELSEIF Dot1! > 1 THEN
            Dot1! = 1
        END IF
        Dot2! = (nx2! * lx!) + (ny2! * ly!) + (nz2! * lz!)
        IF Dot2! < 0 THEN
            Dot2! = 0
        ELSEIF Dot2! > 1 THEN
            Dot2! = 1
        END IF
        Dot3! = (nx3! * lx!) + (ny3! * ly!) + (nz3! * lz!)
        IF Dot3! < 0 THEN
            Dot3! = 0
        ELSEIF Dot3! > 1 THEN
            Dot3! = 1
        END IF

        'multiply by color range
        clr1 = Dot1! * 255
        clr2 = Dot2! * 255
        clr3 = Dot3! * 255

        GouraudTri x1, y1, clr1, x2, y2, clr2, x3, y3, clr3
    END IF

NEXT i

END SUB

SUB GouraudTri (ox1%, oy1%, oc1%, ox2%, oy2%, oc2%, ox3%, oy3%, oc3%)
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

'This implementation of the Gouraud triangle routine is
'the the fast FIXPOINT point version.
'Almost the same as a Flat Triangle routine with an added interpolation
'of the color gradients.  Note the comments with '***. It means
'Addition from the Flat filler code to make it a gouraud filler code.

CONST FIXPOINT = 65536

DIM x1 AS INTEGER, y1 AS INTEGER
DIM x2 AS INTEGER, y2 AS INTEGER
DIM x3 AS INTEGER, y3 AS INTEGER

DIM c1 AS INTEGER               '***colors
DIM c2 AS INTEGER
DIM c3 AS INTEGER


DIM dx1 AS INTEGER, dy1 AS INTEGER, dc1 AS INTEGER      '***
DIM dx2 AS INTEGER, dy2 AS INTEGER, dc2 AS INTEGER      '***
DIM dx3 AS INTEGER, dy3 AS INTEGER, dc3 AS INTEGER      '***


DIM delta1&, delta2&, delta3&
DIM CDelta1&, CDelta2&, CDelta3&        '***

DIM lx&, Rx&
DIM Lc&, Rc&                            '***

x1 = ox1%
y1 = oy1%
c1 = oc1%                               '***

x2 = ox2%
y2 = oy2%
c2 = oc2%                               '***

x3 = ox3%
y3 = oy3%
c3 = oc3%                               '***

IF y2 < y1 THEN
    SWAP y1, y2
    SWAP x1, x2
    SWAP c1, c2                         '***
END IF
IF y3 < y1 THEN
    SWAP y3, y1
    SWAP x3, x1
    SWAP c3, c1                         '***
END IF

IF y3 < y2 THEN
    SWAP y3, y2
    SWAP x3, x2
    SWAP c3, c2                         '***
END IF

dx1 = x2 - x1
dy1 = y2 - y1
dc1 = c2 - c1                           '***
IF dy1 <> 0 THEN
    delta1& = dx1 * FIXPOINT \ dy1
    CDelta1& = dc1 * FIXPOINT \ dy1     '***
ELSE
    delta1& = 0
    CDelta1& = 0                        '***
END IF

dx2 = x3 - x2
dy2 = y3 - y2
dc2 = c3 - c2                           '***
IF dy2 <> 0 THEN
    delta2& = dx2 * FIXPOINT \ dy2
    CDelta2& = dc2 * FIXPOINT \ dy2     '***
ELSE
    delta2& = 0
    CDelta2& = 0                        '***
END IF


dx3 = x1 - x3
dy3 = y1 - y3
dc3 = c1 - c3                          '***
IF dy3 <> 0 THEN
    delta3& = dx3 * FIXPOINT \ dy3
    CDelta3& = dc3 * FIXPOINT \ dy3     '***
ELSE
    delta3& = 0
    CDelta3& = 0                        '***
END IF


'Flat bottom
'Tup part of triangle

lx& = x1 * FIXPOINT
Rx& = lx&

Lc& = c1 * FIXPOINT                     '***Left color
Rc& = Lc&                               '***Right Color

FOR Y% = y1 TO y2 - 1
    Tx1% = lx& \ FIXPOINT   '\          '***
    Tx2% = Rx& \ FIXPOINT   ' \   Parameters for GourHline subroutine
    col1& = Lc&             ' /         '***
    col2& = Rc&             '/          '***
    GOSUB GourHline                     '***
    lx& = lx& + delta1&
    Rx& = Rx& + delta3&
    Lc& = Lc& + CDelta1&                '***    DDA the color grad
    Rc& = Rc& + CDelta3&                '***
NEXT Y%

'Flat top
'Lower part of triangle

lx& = x2 * FIXPOINT
Lc& = c2 * FIXPOINT                     '***

FOR Y% = y2 TO y3
    Tx1% = lx& \ FIXPOINT               '***
    Tx2% = Rx& \ FIXPOINT               '***
    col1& = Lc&                         '***
    col2& = Rc&                         '***
    GOSUB GourHline                     '***
    lx& = lx& + delta2&
    Rx& = Rx& + delta3&
    Lc& = Lc& + CDelta2&                '***
    Rc& = Rc& + CDelta3&                '***
NEXT Y%

EXIT SUB

'**************************************************************************
'Draws a color gradiated horizontal line interpolated from col1 to col2
'Needed variables.
'   Tx1% = integer x1 coordinate
'   Tx2% = integer x2 coordinate
'   y% = integer y coordinate
'   Col1& = Long int of color1 *Fixpoint. In this case 2^16
'   Col2& = Ditto.
'**************************************************************************
GourHline:             

    Gx1% = Tx1%             'Save values to be safe
    Gx2% = Tx2%
    yy% = Y%

    clr1& = col1&
    clr2& = col2&

    IF Gx1% > Gx2% THEN             'Sort values
        SWAP Gx1%, Gx2%
        SWAP clr1&, clr2&
    END IF
    Gdx% = (Gx2% - Gx1%) + 1       'Get Xdelta(+1) for the Div by 0 error
    Cdx& = clr2& - clr1&           'Color delta
    deltac& = Cdx& \ Gdx%          'Interpolate
    col& = clr1&                   'save orig color to be safe

    FOR l% = Gx1% TO Gx2%                   'Rasterizer loop
        PSET (l%, yy%), col& \ FIXPOINT     'Use poke for speed
        col& = col& + deltac&               'DDA
    NEXT l%

RETURN

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

SUB LoadCube (Model() AS Point3d, Tri() AS PolyType) STATIC

RESTORE NumPoints
READ MaxVertex
REDIM Model(1 TO MaxVertex) AS Point3d


RESTORE VertexData
FOR v = 1 TO UBOUND(Model)
    READ Xt, Yt, Zt
    Model(v).X = Xt
    Model(v).Y = Yt
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

        X! = Model(i).X
        Y! = Model(i).Y
        Z! = Model(i).Z

        RotX! = (X! * xx! + Y! * xy! + Z! * xz!) - camx%
        RotY! = (X! * yx! + Y! * yy! + Z! * yz!) - camy%
        RotZ! = (X! * zx! + Y! * zy! + Z! * zz!) - camz%

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

        X! = v(i).X       'Load Original normals
        Y! = v(i).Y
        Z! = v(i).Z

        RotX! = (X! * xx! + Y! * xy! + Z! * xz!)
        RotY! = (X! * yx! + Y! * yy! + Z! * yz!)
        RotZ! = (X! * zx! + Y! * zy! + Z! * zz!)

        v2(i).X = RotX!             'Rotated normals
        v2(i).Y = RotY!
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

