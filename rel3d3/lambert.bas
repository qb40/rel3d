'Lambert shading supplement for the vector article
'SetVideoSeg By Plasma
'Orig code for Polyfiller by CGI Joe

'''Relsoft 2004
'''Rel.Betterwebber.com

DECLARE SUB FixFlatTri (ox1%, oy1%, ox2%, oy2%, ox3%, oy3%, clr%)
DECLARE SUB CalcNormals (Model() AS ANY, Poly() AS ANY, V() AS ANY)
DECLARE SUB FlatTri (x1%, y1%, x2%, y2%, x3%, y3%, Col%)
DECLARE SUB DrawCube (Model() AS ANY, Vector() AS ANY)
DECLARE SUB LoadCube (Model() AS ANY, Vector() AS ANY)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB RotateAndProject (Model() AS ANY, AngleX%, AngleY%, AngleZ%)
DECLARE SUB RotNormals (V() AS ANY, V2() AS ANY, AngleX%, AngleY%, AngleZ%)
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
REDIM SHARED CubeNormal(1) AS VectorType      'Orig normal
REDIM SHARED CubeNormal2(1) AS VectorType     'rotated normal
DIM SHARED ThetaX, ThetaY, ThetaZ           'angle of rotation
DIM SHARED LightNormal AS VectorType        'our light normal
DIM SHARED camx%, camy%, camz%              'camera offset

'PreCalc sin and cos lookuptable

FOR i = 0 TO 359
    A! = i * PI / 180
    Lcos(i) = COS(A!)
    Lsin(i) = SIN(A!)
NEXT i

LightNormal.x = 0               'Light normal
LightNormal.y = -.3
LightNormal.z = -.6


'Setup cube including normals

LoadCube CubeModel(), CubePoly()
RotateAndProject CubeModel(), 0, 0, 0           'may not be  needed
CalcNormals CubeModel(), CubePoly(), CubeNormal()

CLS
SCREEN 13
RANDOMIZE TIMER

'Grey Scale the Palette
 FOR i = 0 TO 255
  OUT &H3C8, i
  OUT &H3C9, (i \ 4)
  OUT &H3C9, (i \ 4) * .9
  OUT &H3C9, (i \ 4) * .1
 NEXT i


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
     'CubeNormal()=Orig
     'CubeNormal2()=Rotated. This are the normal used in shading
     RotNormals CubeNormal(), CubeNormal2(), ThetaX, ThetaY, ThetaZ

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
SUB CalcNormals (Model() AS Point3d, Poly() AS PolyType, V() AS VectorType)
'Calculates the face normal of all the polygons of our model

FOR i = 1 TO UBOUND(V)

    p1 = Poly(i).p1             'vertex
    p2 = Poly(i).p2
    p3 = Poly(i).p3
    x1 = Model(p1).x            'coords
    x2 = Model(p2).x
    x3 = Model(p3).x
    y1 = Model(p1).y
    y2 = Model(p2).y
    y3 = Model(p3).y
    Z1 = Model(p1).z
    Z2 = Model(p2).z
    Z3 = Model(p3).z

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

    V(i).x = xnormal!                  'final face normal
    V(i).y = ynormal!
    V(i).z = znormal!

NEXT i
END SUB

SUB DrawCube (Model() AS Point3d, Poly() AS PolyType) STATIC

FOR i = 1 TO UBOUND(Poly)
    x1 = Model(Poly(i).p1).scrx       'Get triangles from "projected"
    x2 = Model(Poly(i).p2).scrx       'X and Y coords since Znormal
    x3 = Model(Poly(i).p3).scrx       'Does not require a Z coord
    y1 = Model(Poly(i).p1).scry       'V1= Point1 connected to V2 then
    y2 = Model(Poly(i).p2).scry       'V2 to V3 and so on...
    y3 = Model(Poly(i).p3).scry


    'Use the Znormal,the Ray perpendicular(Orthogonal) to the XY plane
    'Defined by the Triangle (X1,Y1,X2,Y2,X3,Y3)
    'if Less(>) 0 then its facing in the opposite direction so
    'don't plot. If <0 then its facing towards you so Plot.
    znormal = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
    IF znormal < 0 THEN
        nx! = CubeNormal2(i).x
        ny! = CubeNormal2(i).y
        nz! = CubeNormal2(i).z
        Lx! = LightNormal.x
        ly! = LightNormal.y
        lz! = LightNormal.z
        Dot! = (nx! * Lx!) + (ny! * ly!) + (nz! * lz!)  'dot product
        IF Dot! < 0 OR Dot! > 1 THEN        'Limit
            Dot! = 0
        END IF
            clr = Dot! * 255                'multiply by color range
            FixFlatTri x1, y1, x2, y2, x3, y3, clr
    END IF

NEXT i

END SUB

SUB FixFlatTri (ox1, oy1, ox2, oy2, ox3, oy3, clr%)

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

'This implementation of the filled triangle routine is
'the the fast FIXPOINT point version.
'Look at the 2 loops below. It first calculate the derivatives and uses DDA
'to get LX&,RX&.  It works because of the fact that multiplication is just
'repeated addition.

CONST FIXPOINT = 65536

DIM x1 AS INTEGER, y1 AS INTEGER
DIM x2 AS INTEGER, y2 AS INTEGER
DIM x3 AS INTEGER, y3 AS INTEGER

DIM dx1 AS INTEGER, dy1 AS INTEGER
DIM dx2 AS INTEGER, dy2 AS INTEGER
DIM dx3 AS INTEGER, dy3 AS INTEGER


DIM delta1&, delta2&, delta3&
DIM Lx&, Rx&

x1 = ox1
y1 = oy1

x2 = ox2
y2 = oy2

x3 = ox3
y3 = oy3

IF y2 < y1 THEN
    SWAP y1, y2
    SWAP x1, x2
END IF
IF y3 < y1 THEN
    SWAP y3, y1
    SWAP x3, x1
END IF

IF y3 < y2 THEN
    SWAP y3, y2
    SWAP x3, x2
END IF

dx1 = x2 - x1
dy1 = y2 - y1
IF dy1 <> 0 THEN
    delta1& = dx1 * FIXPOINT \ dy1
ELSE
    delta1& = 0
END IF

dx2 = x3 - x2
dy2 = y3 - y2
IF dy2 <> 0 THEN
    delta2& = dx2 * FIXPOINT \ dy2
ELSE
    delta2& = 0
END IF


dx3 = x1 - x3
dy3 = y1 - y3
IF dy3 <> 0 THEN
    delta3& = dx3 * FIXPOINT \ dy3
ELSE
    delta3& = 0
END IF


'Flat bottom
'Tup part of triangle

Lx& = x1 * FIXPOINT
Rx& = x1 * FIXPOINT

FOR y = y1 TO y2 - 1
    LINE (Lx& \ FIXPOINT, y)-(Rx& \ FIXPOINT, y), clr
    Lx& = Lx& + delta1&
    Rx& = Rx& + delta3&
NEXT y

'Flat top
'Lower part of triangle

Lx& = x2 * FIXPOINT
FOR y = y2 TO y3 - 1
    LINE (Lx& \ FIXPOINT, y)-(Rx& \ FIXPOINT, y), clr
    Lx& = Lx& + delta2&
    Rx& = Rx& + delta3&
NEXT y

END SUB

SUB LoadCube (Model() AS Point3d, Tri() AS PolyType) STATIC

RESTORE NumPoints
READ MaxVertex
REDIM Model(1 TO MaxVertex) AS Point3d


RESTORE VertexData
FOR V = 1 TO UBOUND(Model)
    READ Xt, Yt, Zt
    Model(V).x = Xt
    Model(V).y = Yt
    Model(V).z = Zt
NEXT V

RESTORE NumPoly
READ MaxPoly
REDIM Tri(MaxPoly) AS PolyType

RESTORE ConnectData

FOR V = 1 TO UBOUND(Tri)
        READ T1, T2, T3
        Tri(V).p1 = T1 + 1
        Tri(V).p2 = T2 + 1
        Tri(V).p3 = T3 + 1
NEXT V


REDIM CubeNormal(1 TO UBOUND(Tri)) AS VectorType
REDIM CubeNormal2(1 TO UBOUND(Tri)) AS VectorType




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

SUB RotNormals (V() AS VectorType, V2() AS VectorType, AngleX, AngleY, AngleZ)

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


FOR i = 1 TO UBOUND(V)

        x! = V(i).x       'Load Original normals
        y! = V(i).y
        z! = V(i).z

        RotX! = (x! * xx! + y! * xy! + z! * xz!)
        RotY! = (x! * yx! + y! * yy! + z! * yz!)
        RotZ! = (x! * zx! + y! * zy! + z! * zz!)

        V2(i).x = RotX!             'Rotated normals
        V2(i).y = RotY!
        V2(i).z = RotZ!
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

