
'''Texturing and tesselation of 3d models for QBCM
'''Cube by SCM
'''All others by Relsoft
'''SetvideoSeg by Plasma357
'''Tritexture modified from CGI Joe's code.
'''Tesselation also of 2nd degree shapes by Biskbart

'''Rel.betterwebber.com

DECLARE SUB FFIX (Mode%)
DECLARE SUB SortPolys (Model() AS ANY, Poly() AS ANY)
DECLARE SUB ShellSort (Poly() AS ANY, Min%, Max%)
DECLARE SUB LoadCube (Model() AS ANY, Poly() AS ANY, radius%)
DECLARE SUB AF.Print (Xpos%, Ypos%, Text$, Col%)
DECLARE SUB DrawModel (Model() AS ANY, Poly() AS ANY, Cull%)
DECLARE SUB LoadPlane (Model() AS ANY, Poly() AS ANY, radius%)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB Tritexture (xx1%, yy1%, xx2%, yy2%, xx3%, yy3%, uu1%, vv1%, uu2%, vv2%, uu3%, vv3%, TSEG%, TOFF%)
DECLARE SUB HTLine (xx1%, xx2%, yy%, uu1&, uu2&, vv1&, vv2&, Twidth%, TOFF%)
DECLARE SUB RotateAndProject (Model() AS ANY, AngleX%, AngleY%, AngleZ%)
DECLARE SUB LoadCylinder (Model() AS ANY, Poly() AS ANY, radius%, Slices%, Bands%, zdist%)
DECLARE SUB LoadSphere (Model() AS ANY, Poly() AS ANY, radius%, Slices%, Bands%)
DEFINT A-Z
REM $DYNAMIC


TYPE Point3d
        x       AS SINGLE       'our 3d point
        y       AS SINGLE
        z       AS SINGLE
        xr      AS SINGLE       'Rotated 3d point
        yr      AS SINGLE       'not needed really
        Zr      AS SINGLE
        scrx    AS INTEGER      'projected x/y coords
        scry    AS INTEGER      'for display on screen
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



CONST FALSE = 0, TRUE = NOT FALSE
CONST LENS = 256                'our multiplier
CONST XCENTER = 160             'mid x and y of screen 13
CONST YCENTER = 100


CONST TSIZE% = 32               'our texture size
CONST TSIZEM1% = TSIZE% - 1     'speed issues used for tiling(AND)

CONST PI = 3.14151693#

REDIM SHARED Vpage(32009) AS INTEGER        'out buffer

REDIM SHARED Texture%(((4 + (TSIZE% * TSIZE%)) \ 2))    'dimiension or
                                                        'texture array

DIM SHARED Lcos(359) AS SINGLE      'LUTS for fast rotation
DIM SHARED Lsin(359) AS SINGLE

REDIM SHARED Model(1) AS Point3d   'our 3d object
REDIM SHARED Tri(1) AS PolyType    'its polygons

DIM SHARED Thetax, Thetay, Thetaz           'angle of rotation
DIM SHARED TextSeg%, Textoff%, ImgSize%     'easy reference
DIM SHARED camz%, camy%, camx%      'our cartesian camera

FFIX 0
RANDOMIZE TIMER
FOR i = 0 TO 359            'prefcalc our Lookup tables
    a! = i * PI / 180
    Lcos(i) = COS(a!)
    Lsin(i) = SIN(a!)
NEXT i


CLS
SCREEN 0
WIDTH 80

LOCATE 1, 1                 'get input
PRINT "Choose Model:"
PRINT "1. Plane"
PRINT "2. Cube [Default]"
PRINT "3. Cylinder"
PRINT "4. Sphere"
DO
    K$ = INKEY$
LOOP UNTIL K$ <> ""

Cull = TRUE             'if backface culing enabled
SELECT CASE ASC(K$)
        CASE 49
            LoadPlane Model(), Tri(), 80
            Cull = FALSE                    'Plane has only one face
        CASE 50
            LoadCube Model(), Tri(), 45
        CASE 51
            LoadCylinder Model(), Tri(), 50, 8, 8, 20
        CASE 52
            LoadSphere Model(), Tri(), 70, 10, 10
        CASE ELSE
            LoadCube Model(), Tri(), 45
END SELECT

CLS
SCREEN 13


'generate our palette

j! = 255 / 360 * 3          'Maxcolor/2PI*Frequency
K! = 255 / 360 * 1
l! = 255 / 360 * 2
FOR i% = 0 TO 255
    OUT &H3C8, i%
    m% = INT(a!)
    n% = INT(b!)
    o% = INT(c!)
    r% = 63 * ABS(SIN(m% * PI / 180))       'The sine-wwave
    g% = 63 * ABS(SIN(n% * PI / 180))
    b% = 63 * ABS(SIN(o% * PI / 180))
    a! = a! + j!
    b! = b! + K!
    c! = c! + l!
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
NEXT

'Generate texture
FOR y% = -TSIZE% \ 2 TO TSIZE% \ 2 - 1
    yy% = ABS(y%)
    FOR x% = -TSIZE% \ 2 TO TSIZE% \ 2 - 1
        xx% = ABS(x%)
        c% = SIN(xx% / 12) * 32 + SIN(yy% / 12) * 256 + SIN((yy% + xx%) / 8) * 64
        PSET (x% + TSIZE% \ 2, y% + TSIZE% \ 2), c%
    NEXT x%
NEXT y%

'get texture
ImgSize% = (4 + TSIZE% * TSIZE%) \ 2
GET (0, 0)-STEP(TSIZE% - 1, TSIZE% - 1), Texture%(0)

TextSeg% = VARSEG(Texture%(0))      'easy reference
Textoff% = VARPTR(Texture%(0))


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
        RotateAndProject Model(), Thetax, Thetay, Thetaz
        ''sort em by distance
        SortPolys Model(), Tri()
        ''show our original texture
        PUT (0, 0), Texture%, PSET
        ''draw our textured model
        DrawModel Model(), Tri(), Cull
        SetVideoSeg &HA000              'set draw to screen
        WAIT &H3DA, 8                   'vsynch
        PUT (0, 0), Vpage(6), PSET      'BitBlit

LOOP UNTIL K$ = CHR$(27)        'escape"?

DEF SEG         'restore current seg

CLS
SCREEN 0
WIDTH 80
PRINT frame& / (TIMER - T#)     'print FPS

c$ = INPUT$(1)          'wait for keypress...
FFIX -1

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
SUB AF.Print (Xpos%, Ypos%, Text$, Col%)
'Prints the standard 8*8 CGA font
'Paramenters:
'Segment=the Layer to print to
'Xpos,Ypos=the coordinates of the text
'Text$=the string to print
'col= is the color to print(gradient)

x% = Xpos%
y% = Ypos%
Spacing% = 8
  FOR i% = 0 TO LEN(Text$) - 1
    x% = x% + Spacing%
    offset% = 8 * ASC(MID$(Text$, i% + 1, 1)) + 14
    FOR j% = 0 TO 7
      DEF SEG = &HFFA6
      Bit% = PEEK(offset% + j%)
      IF Bit% AND 1 THEN PSET (x%, y% + j%), Col% + j%
      IF Bit% AND 2 THEN PSET (x% - 1, y% + j%), Col% + j%
      IF Bit% AND 4 THEN PSET (x% - 2, y% + j%), Col% + j%
      IF Bit% AND 8 THEN PSET (x% - 3, y% + j%), Col% + j%
      IF Bit% AND 16 THEN PSET (x% - 4, y% + j%), Col% + j%
      IF Bit% AND 32 THEN PSET (x% - 5, y% + j%), Col% + j%
      IF Bit% AND 64 THEN PSET (x% - 6, y% + j%), Col% + j%
      IF Bit% AND 128 THEN PSET (x% - 7, y% + j%), Col% + j%
    NEXT j%
  NEXT i%

END SUB

SUB DrawModel (Model() AS Point3d, Poly() AS PolyType, Cull) STATIC
'if its a plane,
'we don't need to backface cull thre polys as the planes
'are supposed to be displayed. ;*)
FOR i = 0 TO UBOUND(Poly)
    j = Poly(i).idx
    x1 = Model(Poly(j).p1).scrx       'Get triangles from "projected"
    x2 = Model(Poly(j).p2).scrx       'X and Y coords since Znormal
    x3 = Model(Poly(j).p3).scrx       'Does not require a Z coord
    y1 = Model(Poly(j).p1).scry       'V1= Point1 connected to V2 then
    y2 = Model(Poly(j).p2).scry       'V2 to V3 and so on...
    y3 = Model(Poly(j).p3).scry
    IF Cull THEN
        Znormal = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
        IF (Znormal < 0) THEN
            u1 = Poly(j).u1
            v1 = Poly(j).v1
            u2 = Poly(j).u2
            v2 = Poly(j).v2
            u3 = Poly(j).u3
            v3 = Poly(j).v3
            TOFF% = VARPTR(Texture%(0))
            Tritexture x1, y1, x2, y2, x3, y3, u1, v1, u2, v2, u3, v3, TextSeg%, TOFF%
        END IF
    ELSE
            u1 = Poly(j).u1
            v1 = Poly(j).v1
            u2 = Poly(j).u2
            v2 = Poly(j).v2
            u3 = Poly(j).u3
            v3 = Poly(j).v3
            TOFF% = VARPTR(Texture%(0))
            Tritexture x1, y1, x2, y2, x3, y3, u1, v1, u2, v2, u3, v3, TextSeg%, TOFF%
    END IF
NEXT i

END SUB

SUB FFIX (Mode%) STATIC
IF Mode% = 0 THEN
    DIM isr(0 TO 5) AS LONG                     'FFix by Dav,Plasma and v1ctor
    isr(0) = &H53EC8B55: isr(1) = &H83025E8B
    isr(2) = &H8E0602EB: isr(3) = &HC7260446
    isr(4) = &H79B9007: isr(5) = &HCF9B5D5B
    DEF SEG = 0
    OldISR1 = PEEK(&HF4)
    OldISR2 = PEEK(&HF5)
    OldISR3 = PEEK(&HF6)
    OldISR4 = PEEK(&HF7)
    POKE &HF4, VARPTR(isr(0)) AND 255
    POKE &HF5, (CLNG(VARPTR(isr(0))) AND &HFF00&) \ 256
    POKE &HF6, VARSEG(isr(0)) AND 255
    POKE &HF7, (CLNG(VARSEG(isr(0))) AND &HFF00&) \ 256

ELSE

DEF SEG = 0
POKE &HF4, OldISR1
POKE &HF5, OldISR2
POKE &HF6, OldISR3
POKE &HF7, OldISR4


END IF

END SUB

SUB HTLine (xx1%, xx2%, yy%, uu1&, uu2&, vv1&, vv2&, Twidth%, TOFF%)


x1% = xx1%
x2% = xx2%
y% = yy%
u1& = uu1&
v1& = vv1&
u2& = uu2&
v2& = vv2&


IF x1% > x2% THEN SWAP x1%, x2%: SWAP u1&, u2&: SWAP v1&, v2&
xdiff% = 1 + x2% - x1%

du& = (u2& - u1&) \ xdiff%
dv& = (v2& - v1&) \ xdiff%

u& = u1&
v& = v1&

Twidthm1% = Twidth% - 1

FOR l% = x1% TO x2%
u% = (u& \ 65536) AND Twidthm1%
v% = (v& \ 65536) AND Twidthm1%
Col% = PEEK(TOFF% + (u% + v% * Twidth%))
PSET (l%, y%), Col%
u& = u& + du&
v& = v& + dv&
NEXT l%

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

SUB LoadPlane (Model() AS Point3d, Poly() AS PolyType, radius)
'Generation code by:
'by Relsoft = me ;*)

REDIM Model(3) AS Point3d
REDIM Poly(1) AS PolyType
Theta! = 90 * PI / 180
FOR i = 0 TO 3
  Model(i).x = radius * (COS(Theta!))
  Model(i).y = radius * (SIN(Theta!))
  Model(i).z = 0
  Theta! = Theta! + PI / 2
NEXT i


'set poly num(Tesselate)
Poly(0).p1 = 3
Poly(0).p2 = 1
Poly(0).p3 = 0

Poly(1).p1 = 3
Poly(1).p2 = 2
Poly(1).p3 = 1

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

SUB RotateAndProject (Model() AS Point3d, AngleX, AngleY, AngleZ) STATIC

''Right handed system
''when camera components increase:
''x=goes left
''y=goes down
''z goes into the screen

'''rotation: counter-clockwise of each axis
''ei.  make yourself perpenicular to the axis
''wave your hand from the center of your body to the left.
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

FOR i = 0 TO UBOUND(Model)

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
            Model(i).scrx = (LENS * RotX! / Distance%) + XCENTER
            Model(i).scry = -(LENS * RotY! / Distance%) + YCENTER
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

 ShellSort Poly(), 0, UBOUND(Poly)

END SUB

SUB Tritexture (xx1%, yy1%, xx2%, yy2%, xx3%, yy3%, uu1%, vv1%, uu2%, vv2%, uu3%, vv3%, TSEG%, TOFF%)

x1% = xx1%
y1% = yy1%
u1% = uu1%
v1% = vv1%

x2% = xx2%
y2% = yy2%
u2% = uu2%
v2% = vv2%

x3% = xx3%
y3% = yy3%
u3% = uu3%
v3% = vv3%




DEF SEG = TSEG%
TSIZ% = (PEEK(TOFF%) + PEEK(TOFF% + 1) * 256) \ 8
IF y1% > y2% THEN SWAP y1%, y2%: SWAP x1%, x2%: SWAP u1%, u2%: SWAP v1%, v2%
IF y1% > y3% THEN SWAP y1%, y3%: SWAP x1%, x3%: SWAP u1%, u3%: SWAP v1%, v3%
IF y2% > y3% THEN SWAP y2%, y3%: SWAP x2%, x3%: SWAP u2%, u3%: SWAP v2%, v3%


d1& = 0: dg1 = 0

ydiffa% = y2% - y1%
IF ydiffa% THEN
d1& = ((x2% - x1%) * 65536) \ ydiffa%
ud1& = ((u2% - u1%) * 65536) \ ydiffa%
vd1& = ((v2% - v1%) * 65536) \ ydiffa%
END IF

ydiffb% = y3% - y2%
IF ydiffb% THEN
d2& = ((x3% - x2%) * 65536) \ ydiffb%
ud2& = ((u3% - u2%) * 65536) \ ydiffb%
vd2& = ((v3% - v2%) * 65536) \ ydiffb%
END IF

ydiffc% = y3% - y1%
IF ydiffc% THEN
d3& = ((x3% - x1%) * 65536) \ ydiffc%
ud3& = ((u3% - u1%) * 65536) \ ydiffc%
vd3& = ((v3% - v1%) * 65536) \ ydiffc%
END IF

lx& = x1% * 65536
rx& = x1% * 65536

lu& = u1% * 65536: ru& = lu&
lv& = v1% * 65536: rv& = lv&

FOR y% = y1% TO y2% - 1
HTLine (lx& \ 65536), (rx& \ 65536), y%, (lu&), (ru&), (lv&), (rv&), TSIZ%, TOFF% + 4
  lx& = lx& + d1&
  rx& = rx& + d3&
  lu& = lu& + ud1&
  ru& = ru& + ud3&
  lv& = lv& + vd1&
  rv& = rv& + vd3&
NEXT

lx& = (x2% * 65536)
lu& = u2% * 65536
lv& = v2% * 65536

' HERE d1 has been added (y2%-y1%)+1 times to lx  = x2
'      d3 has been added (y2%-y1%)+1 times to rx

FOR y% = y2% TO y3%

  HTLine (lx& \ 65536), (rx& \ 65536), y%, (lu&), (ru&), (lv&), (rv&), TSIZ%, TOFF% + 4

  lx& = lx& + d2&
  rx& = rx& + d3&
  lu& = lu& + ud2&
  ru& = ru& + ud3&
  lv& = lv& + vd2&
  rv& = rv& + vd3&
NEXT y%

END SUB

