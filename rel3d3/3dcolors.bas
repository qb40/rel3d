
'''Vector article supplement
'''Playing with colors!!!
'''Gouraud+Phong in 16 color gradient
'''Cube by SCM
'''Torus by Biskbart
'''All others by Relsoft
'''SetVideoSeg by Plasma357
'''Tritexture modified from CGI Joe's code.
'''PhongPal by Bobby3999

'''Rel.betterwebber.com

DECLARE SUB SetPhong16Grad ()
DECLARE SUB GouraudTri (ox1%, oy1%, oc1%, ox2%, oy2%, oc2%, ox3%, oy3%, oc3%)
DECLARE SUB LoadTorus (Rings%, Bands%, RINGRADIUS%, BandRadius%, Model() AS ANY, Poly() AS ANY)
DECLARE SUB SetPolyBaseColor (Poly() AS ANY)
DECLARE SUB Rotnormals (V() AS ANY, v2() AS ANY, M AS ANY)
DECLARE SUB RotateAndProject (Model() AS ANY, M AS ANY)
DECLARE SUB CalcNormals (Model() AS ANY, ModelConnect() AS ANY, V() AS ANY, v2() AS ANY)
DECLARE SUB RotMatrix (M AS ANY, anglex%, angley%, anglez%)
DECLARE SUB DrawModelGour (Model() AS ANY, Poly() AS ANY, VtxNormal() AS ANY)
DECLARE SUB SortPolys (Model() AS ANY, Poly() AS ANY)
DECLARE SUB ShellSort (Poly() AS ANY, Min%, Max%)
DECLARE SUB LoadCube (Model() AS ANY, Poly() AS ANY, radius%)
DECLARE SUB LoadPlane (Model() AS ANY, Poly() AS ANY, radius%)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB LoadCylinder (Model() AS ANY, Poly() AS ANY, radius%, Slices%, Bands%, zdist%)
DECLARE SUB LoadSphere (Model() AS ANY, Poly() AS ANY, radius%, Slices%, Bands%)
DECLARE SUB PhongPal (Ra!, Rd!, Rs!, Ga!, Gd!, Gs!, Ba!, Bd!, Bs!, N%, col1%, col2%)

DEFINT A-Z
REM $DYNAMIC


TYPE Point3d
        x       AS SINGLE       'our 3d point
        y       AS SINGLE
        z       AS SINGLE
        xr      AS SINGLE       'Rotated 3d point
        yr      AS SINGLE       'not needed really
        Zr      AS SINGLE
        ScrX    AS INTEGER      'projected x/y coords
        ScrY    AS INTEGER      'for display on screen
END TYPE

TYPE PolyType
        p1      AS INTEGER      'vertex 1 of our triangle
        p2      AS INTEGER      'huh?
        p3      AS INTEGER
        Clr     AS INTEGER      'color for flat shading
        u1      AS INTEGER      'Texture U and V
        v1      AS INTEGER
        u2      AS INTEGER
        v2      AS INTEGER
        u3      AS INTEGER
        v3      AS INTEGER
        zcenter AS INTEGER      'everage z coord of a poly
        idx     AS INTEGER      'index used for sorting
END TYPE

TYPE VectorType
        x      AS SINGLE
        y      AS SINGLE
        z      AS SINGLE
END TYPE

TYPE MatrixType
    xx     AS SINGLE
    xy     AS SINGLE
    xz     AS SINGLE
    yx     AS SINGLE
    yy     AS SINGLE
    yz     AS SINGLE
    zx     AS SINGLE
    zy     AS SINGLE
    zz     AS SINGLE
END TYPE





CONST FALSE = 0, TRUE = NOT FALSE
CONST LENS = 256                'our multiplier
CONST XCENTER = 160             'mid x and y of screen 13
CONST YCENTER = 100


CONST PI = 3.14151693#


CONST TORNUMRINGS = 16          'Number of Rings outside TORUS
CONST TORNUMBANDS = 10          'Number of PIXEL per RING
CONST TORRINGRADIUS = 65       'Radius of the Ring
CONST TORBANDRADIUS = 20        'Radius of the BAND


REDIM SHARED Vpage(32009) AS INTEGER        'out buffer

DIM SHARED Lcos(359) AS SINGLE      'LUTS for fast rotation
DIM SHARED Lsin(359) AS SINGLE

REDIM SHARED Model(1) AS Point3d   'our 3d object
REDIM SHARED Tri(1) AS PolyType    'its polygons
REDIM SHARED Normal(1) AS VectorType        '"Normal" normal
REDIM SHARED Normal2(1) AS VectorType       'Rotated face normal
REDIM SHARED VertexNormal(1) AS VectorType  'Vertex normal
REDIM SHARED VertexNormal2(1) AS VectorType 'rotated


DIM SHARED Thetax, Thetay, Thetaz           'angle of rotation
DIM SHARED TextSeg%, Textoff%, ImgSize%     'easy reference
DIM SHARED camz%, camy%, camx%      'our cartesian camera
DIM SHARED Matrix AS MatrixType
DIM SHARED LightNormal AS VectorType

RANDOMIZE TIMER
FOR i = 0 TO 359            'prefcalc our Lookup tables
    a! = i * PI / 180
    Lcos(i) = COS(a!)
    Lsin(i) = SIN(a!)
NEXT i


LightNormal.x = 0                 'Light is from the camera
LightNormal.y = -.1
LightNormal.z = -.7

'Normalize
Mag! = SQR(LightNormal.x ^ 2 + LightNormal.y ^ 2 + LightNormal.z ^ 2)
IF Mag! <> 0 THEN
    LightNormal.x = LightNormal.x / Mag!
    LightNormal.y = LightNormal.y / Mag!
    LightNormal.z = LightNormal.z / Mag!
END IF


CLS
SCREEN 0
WIDTH 80

LOCATE 1, 1                 'get input
PRINT "Choose Model:"
PRINT "1. Cube"
PRINT "2. Cylinder"
PRINT "3. Sphere"
PRINT "4. Torus[Default]"
DO
    K$ = INKEY$
LOOP UNTIL K$ <> ""

Cull = TRUE             'if backface culing enabled
SELECT CASE ASC(K$)
        CASE 49
            LoadCube Model(), Tri(), 45
        CASE 50
            LoadCylinder Model(), Tri(), 50, 8, 8, 20
        CASE 51
            LoadSphere Model(), Tri(), 70, 10, 10
        CASE 52
            LoadTorus TORNUMRINGS, TORNUMBANDS, TORRINGRADIUS, TORBANDRADIUS, Model(), Tri()
        CASE ELSE
            LoadTorus TORNUMRINGS, TORNUMBANDS, TORRINGRADIUS, TORBANDRADIUS, Model(), Tri()
END SELECT


''Set up new normals
REDIM Normal(0 TO UBOUND(Tri)) AS VectorType
REDIM Normal2(0 TO UBOUND(Tri)) AS VectorType
REDIM VertexNormal(0 TO UBOUND(Model)) AS VectorType
REDIM VertexNormal2(0 TO UBOUND(Model)) AS VectorType

'Set base colors for polygons
SetPolyBaseColor Tri()


'init rotation just in case, for use with calc normals
RotMatrix Matrix, 0, 0, 0
RotateAndProject Model(), Matrix
CalcNormals Model(), Tri(), Normal(), VertexNormal()



CLS
SCREEN 13

'Set up nice phong pal
SetPhong16Grad

Thetax = INT(RND * 360)         'random starting angles
Thetay = INT(RND * 360)
Thetaz = INT(RND * 360)

REDIM Vpage(32009) AS INTEGER        'Clear offscreen buffer
Vpage(6) = 2560                      'Width 320*8
Vpage(7) = 200                       'Height
LAYER = VARSEG(Vpage(0)) + 1         'Buffer Seg(Ask Plasma)
SetVideoSeg LAYER                    'Set Draw to Buffer


T# = TIMER
frame& = 0              'frame counter for FPS

DO

         K$ = INKEY$                'user control of camera
         SELECT CASE UCASE$(K$)
            CASE "A"
                camz% = camz% + 1
            CASE "Z"
                camz% = camz% - 1
            CASE "S"
                camy% = camy% + 1
            CASE "X"
                camy% = camy% - 1
            CASE "D"
                camx% = camx% + 1
            CASE "C"
                camx% = camx% - 1
            CASE ELSE
        END SELECT

        frame& = frame& + 1
        SetVideoSeg LAYER                   'Set Draw to Buffer
        LINE (0, 0)-(319, 199), 0, BF       'cls
        Thetax = (Thetax + 1) MOD 360       'increase angles of rotation
        Thetay = (Thetay + 1) MOD 360
        Thetaz = (Thetaz + 1) MOD 360
        '''rotate object
        RotMatrix Matrix, Thetax, Thetay, Thetaz
        RotateAndProject Model(), Matrix
        Rotnormals VertexNormal(), VertexNormal2(), Matrix
        ''sort em by distance
        SortPolys Model(), Tri()
        ''draw our textured model
        DrawModelGour Model(), Tri(), VertexNormal2()
        SetVideoSeg &HA000              'set draw tfo screen
        WAIT &H3DA, 8                   'vsynch
        PUT (0, 0), Vpage(6), PSET      'BitBlit

LOOP UNTIL K$ = CHR$(27)        'escape"?

DEF SEG         'restore current seg

CLS
SCREEN 0
WIDTH 80
PRINT "Fps:"; frame& / (TIMER - T#)    'print FPS

C$ = INPUT$(1)          'wait for keypress...

END

'''Cube polygon connecting poifnts in Quad form
CUBECONNECT:
DATA 0, 1, 2, 3
DATA 1, 5, 6, 2
DATA 5, 4, 7, 6
DATA 4, 0, 3, 7
DATA 4, 5, 1, 0
DATA 3, 2, 6, 7

REM $STATIC
SUB CalcNormals (Model() AS Point3d, ModelConnect() AS PolyType, V() AS VectorType, v2() AS VectorType)

FOR i = 0 TO UBOUND(V)

    p1 = ModelConnect(i).p1
    p2 = ModelConnect(i).p2
    p3 = ModelConnect(i).p3
    x1 = Model(p1).x
    x2 = Model(p2).x
    x3 = Model(p3).x
    y1 = Model(p1).y
    y2 = Model(p2).y
    y3 = Model(p3).y
    z1 = Model(p1).z
    z2 = Model(p2).z
    Z3 = Model(p3).z

    ax! = x2 - x1
    bx! = x3 - x2
    ay! = y2 - y1
    by! = y3 - y2
    az! = z2 - z1
    bz! = Z3 - z2

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


    V(i).x = xnormal!
    V(i).y = ynormal!
    V(i).z = znormal!

NEXT i

'VertexNormal
FOR i = 0 TO UBOUND(Model) - 1
    nx! = 0
    ny! = 0
    nz! = 0
    count = 0
    FOR j = 0 TO UBOUND(ModelConnect)
        IF ModelConnect(j).p1 = i OR ModelConnect(j).p2 = i OR ModelConnect(j).p3 = i THEN
            nx! = nx! + V(j).x
            ny! = ny! + V(j).y
            nz! = nz! + V(j).z
            count = count + 1
        END IF
    NEXT j


    nx! = nx! / count
    ny! = ny! / count
    nz! = nz! / count

    'Normalize
    Mag! = SQR(xnormal! ^ 2 + ynormal! ^ 2 + znormal! ^ 2)
    IF Mag! <> 0 THEN
        xnormal! = xnormal! / Mag!
        ynormal! = ynormal! / Mag!
        znormal! = znormal! / Mag!
    END IF

    v2(i).x = nx!
    v2(i).y = ny!
    v2(i).z = nz!
NEXT i

END SUB

SUB DrawModelGour (Model() AS Point3d, Poly() AS PolyType, VtxNormal() AS VectorType) STATIC

FOR j = 0 TO UBOUND(Poly)
    i = Poly(j).idx
    x1 = Model(Poly(i).p1).ScrX       'Get triangles from "projected"
    x2 = Model(Poly(i).p2).ScrX       'X and Y coords since Znormal
    x3 = Model(Poly(i).p3).ScrX       'Does not require a Z coord
    y1 = Model(Poly(i).p1).ScrY       'V1= Point1 connected to V2 then
    y2 = Model(Poly(i).p2).ScrY       'V2 to V3 and so on...
    y3 = Model(Poly(i).p3).ScrY


    'Use the Znormal,the Ray perpendicular(Orthogonal) to the XY plane
    'Defined by the Triangle (X1,Y1,X2,Y2,X3,Y3)
    'if Less(<) 0 then its facing in the opposite direction so
    'don't plot. If =>0 then its facing towards you so Plot.
    znormal = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
    IF znormal < 0 THEN
        nx1! = VtxNormal(Poly(i).p1).x
        ny1! = VtxNormal(Poly(i).p1).y
        nz1! = VtxNormal(Poly(i).p1).z
        nx2! = VtxNormal(Poly(i).p2).x
        ny2! = VtxNormal(Poly(i).p2).y
        nz2! = VtxNormal(Poly(i).p2).z
        nx3! = VtxNormal(Poly(i).p3).x
        ny3! = VtxNormal(Poly(i).p3).y
        nz3! = VtxNormal(Poly(i).p3).z
        Lx! = LightNormal.x
        Ly! = LightNormal.y
        lz! = LightNormal.z
        Dot1! = (nx1! * Lx!) + (ny1! * Ly!) + (nz1! * lz!)
        IF Dot1! < 0 THEN
            Dot1! = 0
        ELSEIF Dot1! > 1 THEN
            Dot1! = 1
        END IF

        Dot2! = (nx2! * Lx!) + (ny2! * Ly!) + (nz2! * lz!)
        IF Dot2! < 0 THEN
            Dot2! = 0
        ELSEIF Dot2! > 1 THEN
            Dot2! = 1
        END IF

        Dot3! = (nx3! * Lx!) + (ny3! * Ly!) + (nz3! * lz!)
        IF Dot3! < 0 THEN
            Dot3! = 0
        ELSEIF Dot3! > 1 THEN
            Dot3! = 1
        END IF

            Clr1 = (Dot1! * 15)
            Clr1 = Clr1 + Poly(i).Clr
            Clr2 = (Dot2! * 15)
            Clr2 = Clr2 + Poly(i).Clr
            Clr3 = (Dot3! * 15)
            Clr3 = Clr3 + Poly(i).Clr
            GouraudTri x1, y1, Clr1, x2, y2, Clr2, x3, y3, Clr3
    END IF


NEXT j

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

DIM Lx&, Rx&
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

Lx& = x1 * FIXPOINT
Rx& = Lx&

Lc& = c1 * FIXPOINT                     '***Left color
Rc& = Lc&                               '***Right Color

FOR y% = y1 TO y2 - 1
    Tx1% = Lx& \ FIXPOINT   '\          '***
    Tx2% = Rx& \ FIXPOINT   ' \   Parameters for GourHline subroutine
    col1& = Lc&             ' /         '***
    col2& = Rc&             '/          '***
    GOSUB GourHline                     '***
    Lx& = Lx& + delta1&
    Rx& = Rx& + delta3&
    Lc& = Lc& + CDelta1&                '***    DDA the color grad
    Rc& = Rc& + CDelta3&                '***
NEXT y%

'Flat top
'Lower part of triangle

Lx& = x2 * FIXPOINT
Lc& = c2 * FIXPOINT                     '***

FOR y% = y2 TO y3
    Tx1% = Lx& \ FIXPOINT               '***
    Tx2% = Rx& \ FIXPOINT               '***
    col1& = Lc&                         '***
    col2& = Rc&                         '***
    GOSUB GourHline                     '***
    Lx& = Lx& + delta2&
    Rx& = Rx& + delta3&
    Lc& = Lc& + CDelta2&                '***
    Rc& = Rc& + CDelta3&                '***
NEXT y%

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
    yy% = y%

    Clr1& = col1&
    Clr2& = col2&

    IF Gx1% > Gx2% THEN             'Sort values
        SWAP Gx1%, Gx2%
        SWAP Clr1&, Clr2&
    END IF
    Gdx% = (Gx2% - Gx1%) + 1       'Get Xdelta(+1) for the Div by 0 error
    Cdx& = Clr2& - Clr1&           'Color delta
    deltac& = Cdx& \ Gdx%          'Interpolate
    col& = Clr1&                   'save orig color to be safe

    FOR l% = Gx1% TO Gx2%                   'Rasterizer loop
        PSET (l%, yy%), col& \ FIXPOINT     'Use poke for speed
        col& = col& + deltac&               'DDA
    NEXT l%

RETURN

END SUB

SUB LoadCube (Model() AS Point3d, Poly() AS PolyType, radius)
'Generation code by:
'by Steve McCarthy(SCM)
'Tesselation by me. :*)

REDIM Model(7) AS Point3d
Theta! = PI / 4
dTheta! = PI / 2

FOR P = 0 TO 7
  Model(P).x = radius * SGN(COS(Theta!))     ' x
  Model(P).y = radius * SGN(SIN(Theta!))      ' y
  Model(P).z = radius - (radius * 2) * (P \ 4)    ' z
  Theta! = Theta! + dTheta!
NEXT P

'Tesselate
REDIM Poly(11) AS PolyType
RESTORE CUBECONNECT

j = 0
FOR i = 0 TO 5
    READ p1, p2, p3, p4
    Poly(j).p1 = p4
    Poly(j).p2 = p2
    Poly(j).p3 = p1
    j = j + 1
    Poly(j).p1 = p4
    Poly(j).p2 = p3
    Poly(j).p3 = p2
    j = j + 1
NEXT i


'calculate vertex coords centering it
FOR j = 0 TO UBOUND(Poly)
    u1 = 0
    v1 = 0
    u2 = TSIZEM1%
    v2 = TSIZEM1%
    u3 = TSIZEM1%
    v3 = 0
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
    Poly(j).idx = j
    j = j + 1
    u1 = 0
    v1 = 0
    u2 = 0
    v2 = TSIZEM1%
    u3 = TSIZEM1%
    v3 = TSIZEM1%
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
    Poly(j).idx = j
NEXT j
END SUB

SUB LoadCylinder (Model() AS Point3d, Poly() AS PolyType, radius, Slices, Bands, zdist)
'Generation code by:
'by Relsoft=me .;*)

'I. Cylindrical to cartesian
'///  x = COS(theta)
'///  y = SIN(theta)
'///  z = z

REDIM Model((Slices * Bands) - 1) AS Point3d
i = 0
z! = -zdist * Slices / 2
FOR Slice = 0 TO Slices - 1
FOR Band = 0 TO Bands - 1
    Theta! = (2 * PI / Bands) * Band
    Model(i).x = radius * COS(Theta!)
    Model(i).y = radius * SIN(Theta!)
    Model(i).z = z!
    i = i + 1
NEXT Band
    z! = z! + zdist
NEXT Slice

i = 0
MaxPoly = 0
FOR Slice = 0 TO Slices - 1
    FOR Band = 0 TO Bands - 1
        i = i + 2
        MaxPoly = MaxPoly + 2
    NEXT Band
NEXT Slice

REDIM Poly(MaxPoly) AS PolyType

MaxVertex = (Slices * Bands)
i = 0
FOR Slice = 0 TO Slices - 1
    FOR Band = 0 TO Bands - 1
        Poly(i).p1 = (Slice * Slices + Band + Slices) MOD MaxVertex
        Poly(i).p2 = Slice * Slices + (Band + 1) MOD Slices
        Poly(i).p3 = Slice * Slices + Band
        i = i + 1
        Poly(i).p1 = (Slice * Slices + Band + Slices) MOD MaxVertex
        Poly(i).p2 = (Slice * Slices + (Band + 1) MOD Slices + Slices) MOD MaxVertex
        Poly(i).p3 = Slice * Slices + (Band + 1) MOD Slices
        i = i + 1
    NEXT Band
NEXT Slice

'calculate vertex coords centering it
FOR j = 0 TO UBOUND(Poly) - 1
    u1 = 0
    v1 = 0
    u2 = TSIZEM1%
    v2 = TSIZEM1%
    u3 = TSIZEM1%
    v3 = 0
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
    Poly(j).idx = j
    j = j + 1
    u1 = 0
    v1 = 0
    u2 = 0
    v2 = TSIZEM1%
    u3 = TSIZEM1%
    v3 = TSIZEM1%
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
    Poly(j).idx = j
NEXT j



END SUB

SUB LoadSphere (Model() AS Point3d, Poly() AS PolyType, radius, Slices, Bands)
'Generation code by:
'by Relsoft=me .;*)

'///    x =  p SIN(Phi) COS(theta)
'///    y =  p SIN(Phi) SIN(theta)
'///    z =  p COS(Phi)

MaxVertex = (1 + Slices) * (Bands + 1) - Bands
REDIM Model(MaxVertex)  AS Point3d

i = 0
FOR SliceLoop = 0 TO Slices
    Phi! = PI / Slices * SliceLoop
    FOR BandLoop = 0 TO Bands - 1
        Theta! = 2 * PI / Bands * BandLoop
        Model(i).x = -INT(radius * SIN(Phi!) * COS(Theta!))
        Model(i).y = -INT(radius * SIN(Phi!) * SIN(Theta!))
        Model(i).z = -INT(radius * COS(Phi!))
        i = i + 1
    NEXT BandLoop
NEXT SliceLoop


i = 0
MaxPoly = 0
FOR Slice = 0 TO Slices
    FOR Band = 0 TO Bands - 1
        i = i + 2
        MaxPoly = MaxPoly + 2
    NEXT Band
NEXT Slice

REDIM Poly(MaxPoly) AS PolyType

i = 0
FOR Slice = 0 TO Slices
    FOR Band = 0 TO Bands - 1
        Poly(i).p1 = (Slice * Slices + Band + Slices) MOD MaxVertex
        Poly(i).p2 = Slice * Slices + (Band + 1) MOD Slices
        Poly(i).p3 = Slice * Slices + Band
        i = i + 1
        Poly(i).p1 = (Slice * Slices + Band + Slices) MOD MaxVertex
        Poly(i).p2 = (Slice * Slices + (Band + 1) MOD Slices + Slices) MOD MaxVertex
        Poly(i).p3 = Slice * Slices + (Band + 1) MOD Slices
        i = i + 1
    NEXT Band
NEXT Slice

'calculate vertex coords centering it
FOR j = 0 TO UBOUND(Poly) - 1
    u1 = 0
    v1 = 0
    u2 = TSIZEM1%
    v2 = TSIZEM1%
    u3 = TSIZEM1%
    v3 = 0
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
    Poly(j).idx = j
    j = j + 1
    u1 = 0
    v1 = 0
    u2 = 0
    v2 = TSIZEM1%
    u3 = TSIZEM1%
    v3 = TSIZEM1%
    Poly(j).u1 = u1
    Poly(j).v1 = v1
    Poly(j).u2 = u2
    Poly(j).v2 = v2
    Poly(j).u3 = u3
    Poly(j).v3 = v3
    Poly(j).idx = j
NEXT j


END SUB

SUB LoadTorus (Rings, Bands, RINGRADIUS, BandRadius, Model() AS Point3d, Poly() AS PolyType)

'Generation code by Biskbart.

MaxPoint% = Rings * Bands
REDIM Model((MaxPoint%)) AS Point3d

  A1! = 2 * PI / Rings: A2! = 2 * PI / Bands
  i% = 0
 FOR S2% = 0 TO Bands - 1
  FOR S1% = 0 TO Rings - 1
    x1! = COS(S1% * A1!) * RINGRADIUS
    y1! = SIN(S1% * A1!) * RINGRADIUS
     Model(i%).x = x1! + COS(S1% * A1!) * SIN(S2% * A2!) * BandRadius
     Model(i%).y = y1! + SIN(S1% * A1!) * SIN(S2% * A2!) * BandRadius
     Model(i%).z = COS(S2% * A2!) * BandRadius
    i% = i% + 1
  NEXT S1%
 NEXT S2%


  i% = 0
  MaxTri% = 0
  FOR S1% = Bands - 1 TO 0 STEP -1
   FOR S2% = Rings - 1 TO 0 STEP -1
    i% = i% + 1
    MaxTri% = MaxTri% + 1
    i% = i% + 1
    MaxTri% = MaxTri% + 1
   NEXT S2%
  NEXT S1%

  REDIM Poly(MaxTri%) AS PolyType
  TT2% = 0
  i% = 0
  FOR S2% = Rings - 1 TO 0 STEP -1
  FOR S1% = Bands - 1 TO 0 STEP -1
    Poly(i%).p3 = (S1% * Rings + S2% + Rings) MOD MaxPoint%
    Poly(i%).p2 = S1% * Rings + (S2% + 1) MOD Rings
    Poly(i%).p1 = S1% * Rings + S2%
    C% = INT(RND * 255) + 1
    Poly(i%).Clr = C%
    Poly(i%).u1 = TSIZE%
    Poly(i%).v1 = 0
    Poly(i%).u2 = 0
    Poly(i%).v2 = 0
    Poly(i%).u3 = TSIZE%
    Poly(i%).v3 = TSIZE%
    i% = i% + 1
    Poly(i%).p3 = (S1% * Rings + S2% + Rings) MOD MaxPoint%
    Poly(i%).p2 = (S1% * Rings + (S2% + 1) MOD Rings + Rings) MOD MaxPoint%
    Poly(i%).p1 = S1% * Rings + (S2% + 1) MOD Rings
    Poly(i%).Clr = C%
    Poly(i%).u1 = 0
    Poly(i%).v1 = 0
    Poly(i%).u2 = 0
    Poly(i%).v2 = TSIZE%
    Poly(i%).u3 = TSIZE%
    Poly(i%).v3 = TSIZE%
    T% = (T% + 1) AND 1
    i% = i% + 1
   NEXT S1%
   TT2% = ((TT2% + 1) AND 1)
  NEXT S2%

END SUB

SUB PhongPal (Ra!, Rd!, Rs!, Ga!, Gd!, Gs!, Ba!, Bd!, Bs!, N%, col1%, col2%)
  Range% = col2% - col1%
  Angle! = PI / 2!
  AngleStep! = (PI / 2!) / Range%

  FOR i% = col1% TO col2%
    CosineOfAngle! = COS(Angle!)
    Diffuse! = Rd! * CosineOfAngle!
    Specular! = Rs! * (CosineOfAngle! ^ N%)
    red% = Ra! + Diffuse! + Specular!
    Diffuse! = Gd! * CosineOfAngle!
    Specular! = Gs! * (CosineOfAngle! ^ N%)
    green% = Ga! + Diffuse! + Specular!
    Diffuse! = Bd! * CosineOfAngle!
    Specular! = Bs! * (CosineOfAngle! ^ N%)
    blue% = Ba! + Diffuse! + Specular!
    IF red% > 63 THEN red% = 63
    IF green% > 63 THEN green% = 63
    IF blue% > 63 THEN blue% = 63
    IF red% < 0 THEN red% = 0
    IF green% < 0 THEN green% = 0
    IF blue% < 0 THEN blue% = 0

    OUT &H3C8, i%
    OUT &H3C9, red%
    OUT &H3C9, green%
    OUT &H3C9, blue%
    Angle! = Angle! - AngleStep!
  NEXT

END SUB

SUB RotateAndProject (Model() AS Point3d, M AS MatrixType) STATIC


FOR i = 0 TO UBOUND(Model)

        x! = Model(i).x
        y! = Model(i).y
        z! = Model(i).z

        RotX! = (x! * M.xx + y! * M.xy + z! * M.xz) - camx%
        RotY! = (x! * M.yx + y! * M.yy + z! * M.yz) - camy%
        RotZ! = (x! * M.zx + y! * M.zy + z! * M.zz) - camz%

        Model(i).xr = RotX!
        Model(i).yr = RotY!
        Model(i).Zr = RotZ!


        'Project
        Distance% = (LENS - RotZ!)
        IF Distance% THEN
            Model(i).ScrX = (LENS * RotX! / Distance%) + XCENTER
            Model(i).ScrY = -(LENS * RotY! / Distance%) + YCENTER
        ELSE
        END IF
NEXT i


END SUB

SUB RotMatrix (M AS MatrixType, anglex%, angley%, anglez%)
'Precalculate the SIN and COS of each angle
cx! = Lcos(anglex%)
sx! = Lsin(anglex%)
cy! = Lcos(angley%)
sy! = Lsin(angley%)
cz! = Lcos(anglez%)
sz! = Lsin(anglez%)


M.xx = cy! * cz!
M.xy = sx! * sy! * cz! - cx! * sz!
M.xz = cx! * sy! * cz! + sx! * sz!

M.yx = cy! * sz!
M.yy = cx! * cz! + sx! * sy! * sz!
M.yz = -sx! * cz! + cx! * sy! * sz!

M.zx = -sy!
M.zy = sx! * cy!
M.zz = cx! * cy!

END SUB

SUB Rotnormals (V() AS VectorType, v2() AS VectorType, M AS MatrixType)


FOR i = 0 TO UBOUND(V)

        x! = V(i).x       'Load Original vector
        y! = V(i).y
        z! = V(i).z


        RotX! = (x! * M.xx + y! * M.xy + z! * M.xz) - camx%
        RotY! = (x! * M.yx + y! * M.yy + z! * M.yz) - camy%
        RotZ! = (x! * M.zx + y! * M.zy + z! * M.zz) - camz%

        v2(i).x = RotX!
        v2(i).y = RotY!
        v2(i).z = RotZ!
NEXT i

END SUB

SUB SetPhong16Grad
'Sets a 16 color gradient phong palette
'change the constants if you aren't happy with the colors
FOR i = 0 TO 15
Ra! = 0: Rd! = INT(RND * 150) + 25: Rs! = INT(RND * 150) + 20
Ga! = 0: Gd! = INT(RND * 150) + 25: Gs! = INT(RND * 150) + 20
Ba! = 0: Bd! = INT(RND * 150) + 25: Bs! = INT(RND * 150) + 20
N% = 8 + INT(RND * 64)
PhongPal Ra!, Rd!, Rs!, Ga!, Gd!, Gs!, Ba!, Bd!, Bs!, N%, i * 16, (i * 16) + 15
NEXT i

END SUB

SUB SetPolyBaseColor (Poly() AS PolyType)

    FOR i% = 0 TO UBOUND(Poly) - 1
        C% = ((i%) AND 15) * 16
        Poly(i%).Clr = C%
        i% = i% + 1
        Poly(i%).Clr = C%
    NEXT i%

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

FOR offset& = 0 TO 32764 ' Search for b$AddrC, which is
IF PEEK(offset&) = &HA0 THEN ' in the default segment and
IF PEEK(offset& + 1) = &H7D THEN ' should have a value of
IF PEEK(offset& + 2) = &H0 THEN ' A0 7D 00 A0.
IF PEEK(offset& + 3) = &HA0 THEN
VideoAddrOff& = offset& + 2 ' If we found it, record the
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

SUB ShellSort (Poly() AS PolyType, Min, Max)
    'Shell sort Algorithm
    ' Set comparison offset to half the number of records.
    offset = Max \ 2

    ' Loop until offset gets to zero.
    DO WHILE offset > 0

        Limit = Max - offset

        DO

            ' Assume no switches at this offset.
            Switch = FALSE

            ' Compare elements for the specified field and switch
            ' any that are out of order.
            FOR i = Min TO Limit - 1
                Ti = Poly(i).zcenter
                Tj = Poly(i + offset).zcenter
                        IF Ti > Tj THEN
                            SWAP Poly(i).idx, Poly(i + offset).idx
                            SWAP Poly(i).zcenter, Poly(i + offset).zcenter
                            Switch = i
                        END IF

            NEXT i

            ' Sort on next pass only to location where last switch was made.
            Limit = Switch

        LOOP WHILE Switch

        ' No switches at last offset. Try an offset half as big.
        offset = offset \ 2
    LOOP


END SUB

SUB SortPolys (Model() AS Point3d, Poly() AS PolyType)

 FOR i% = 0 TO UBOUND(Poly)
  Poly(i%).zcenter = Model(Poly(i%).p1).Zr + Model(Poly(i%).p2).Zr + Model(Poly(i%).p3).Zr
  Poly(i%).idx = i%
 NEXT i%

 ShellSort Poly(), 0, UBOUND(Poly) + 1

END SUB

