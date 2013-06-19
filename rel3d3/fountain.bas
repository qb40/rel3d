'///A lil particle demo I made using WuPixels in colors!!!
'///Y-axis rotation but could rotate on any axis
'///tried to add wind but the fx sucked terribly.
'///SetVideoSeg by Plasma
'///FFIX by v1ctor, Plasma and Dav
'///
'///Funny how I get a *lot* done using someone else's comp that using mine. ;*)
'///Relsoft
'///Rel.BetterWebber.com

DECLARE SUB WuPixelGrad16 (x!, y!, col%)
DECLARE SUB GradColor (col1%, r1%, g1%, b1%, col2%, r2%, g2%, b2%)
DECLARE SUB FFIX (Mode%)
DECLARE SUB SetVideoSeg (Segment%)
DEFINT A-Z

TYPE point3d
    x       AS SINGLE
    y       AS SINGLE
    z       AS SINGLE
    xv      AS SINGLE
    yv      AS SINGLE
    zv      AS SINGLE
    counter AS INTEGER
    clr     AS INTEGER
END TYPE

CONST NUMPARTS = 300
CONST LENS = 256
CONST xMID = 160, yMID = 100
CONST PI = 3.141593
CONST GRAV = .01
CONST WIND = 0

'Floor

CONST XMAX = 25, YMAX = 25

RANDOMIZE TIMER
REDIM SHARED Vpage(32009)  AS INTEGER
DIM Parts(NUMPARTS) AS point3d
DIM Floor(XMAX * YMAX) AS point3d

DIM SHARED Lcos(359) AS SINGLE
DIM SHARED Lsin(359) AS SINGLE


FFIX 0  'Secret formula that makes Floating Point cals faster. :*)

'Spherical coordinate system

'///    x =  p SIN(Phi) COS(theta)
'///    y =  p SIN(Phi) SIN(theta)
'///    z =  p COS(Phi)

FOR i = 0 TO NUMPARTS
    Parts(i).x = 0
    Parts(i).y = -50
    Parts(i).z = 0
    theta! = INT(RND * 360) * PI / 180
    Phi! = INT(RND * 360) * PI / 180
    Speed! = .1 + RND
    Parts(i).xv = SIN(Phi!) * COS(theta!) * (Speed! / 3)
    Parts(i).yv = ABS(SIN(Phi!) * SIN(theta!) * Speed! * 2)
    Parts(i).zv = COS(Phi!) * (Speed! / 3)
    Parts(i).counter = 0
    Parts(i).counter = 0
    c% = ((c% MOD 15) + 1) * 16
    Parts(i).clr = c%
NEXT i


'Floor model
FScale! = 10
xm = XMAX \ 2
ym = YMAX \ 2
i = 0
FOR x = -xm TO xm - 1
    c% = ((c% MOD 15) + 1) * 16
    FOR z = -ym TO ym + 1
        Floor(i).x = x * FScale!
        Floor(i).z = z * FScale!
        Floor(i).y = -50
        Floor(i).clr = c%
        i = i + 1
    NEXT z
NEXT x


FOR i = 0 TO 359
    a! = i * PI / 180
    Lcos(i) = COS(a!)
    Lsin(i) = SIN(a!)
NEXT i





CLS
SCREEN 13

'Read ending grad colors and set gradient pal
RESTORE RGB
FOR i = 0 TO 15
    READ R%, G%, B%
    GradColor i * 16, 0, 0, 0, (i * 16) + 15, R%, G%, B%
NEXT i


Vpage(6) = 2560
Vpage(7) = 200
Layer = VARSEG(Vpage(0)) + 1
SetVideoSeg Layer

DO
    SetVideoSeg Layer
    LINE (0, 0)-(319, 199), 0, BF
    AngleY = (AngleY + 1) MOD 360
    cx! = Lcos(AngleX)
    sx! = Lsin(AngleX)
    cy! = Lcos(AngleY)
    sy! = Lsin(AngleY)
    cz! = Lcos(AngleZ)
    sz! = Lsin(AngleZ)

    xx! = cy! * cz!
    xy! = sx! * sy! * cz! - cx! * sz!
    xz! = cx! * sy! * cz! + sx! * sz!

    'Optimization trick!!!
    'Y axis is not rotated
    REM yx! = cy! * sz!
    REM yy! = cx! * cz! + sx! * sy! * sz!
    REM yz! = -sx! * cz! + cx! * sy! * sz!

    zx! = -sy!
    zy! = sx! * cy!
    zz! = cx! * cy!

        'Floor
    FOR i = 0 TO UBOUND(Floor) - 2

        'Optimization trick!!!
        'Y axis is not rotated

        RotX! = (Floor(i).x * xx! + Floor(i).y * xy! + Floor(i).z * xz!) - camx%
        RotY! = Floor(i).y - camy%
        RotZ! = (Floor(i).x * zx! + Floor(i).y * zy! + Floor(i).z * zz!) - camz%
        'Project
        Distance% = (LENS - RotZ!)
        IF Distance% THEN
            x2d! = xMID + (LENS * RotX! / Distance%)
            y2d! = yMID - (LENS * RotY! / Distance%)
        END IF
        WuPixelGrad16 x2d!, y2d!, Floor(i).clr
    NEXT i

        'particles
    FOR i = 0 TO NUMPARTS
        Parts(i).x = Parts(i).x + Parts(i).xv
        Parts(i).y = Parts(i).y + Parts(i).yv
        Parts(i).z = Parts(i).z + Parts(i).zv
        Parts(i).yv = Parts(i).yv - GRAV

        IF Parts(i).y < -51 THEN
            Parts(i).xv = 0
            Parts(i).yv = 0
            Parts(i).zv = 0
            Parts(i).counter = Parts(i).counter + 1
        ELSE
            Parts(i).x = Parts(i).x + WIND
        END IF

        IF Parts(i).counter > 100 THEN
            Parts(i).x = 0
            Parts(i).y = -50
            Parts(i).z = 0
            theta! = INT(RND * 360) * PI / 180
            Phi! = INT(RND * 360) * PI / 180
            Speed! = .1 + RND
            Parts(i).xv = SIN(Phi!) * COS(theta!) * (Speed! / 3)
            Parts(i).yv = ABS(SIN(Phi!) * SIN(theta!) * Speed! * 2)
            Parts(i).zv = COS(Phi!) * (Speed! / 3)
            Parts(i).counter = 0
            c% = ((c% MOD 15) + 1) * 16
            Parts(i).clr = c%
        END IF


        'Optimization trick!!!
        'Y axis is not rotated
        RotX! = (Parts(i).x * xx! + Parts(i).y * xy! + Parts(i).z * xz!) - camx%
        RotY! = Parts(i).y - camy%
        RotZ! = (Parts(i).x * zx! + Parts(i).y * zy! + Parts(i).z * zz!) - camz%

        'Project
        Distance% = (LENS - RotZ!)
        IF Distance% THEN
            x2d! = xMID + (LENS * RotX! / Distance%)
            y2d! = yMID - (LENS * RotY! / Distance%)
        END IF
        WuPixelGrad16 x2d!, y2d!, Parts(i).clr
    NEXT i

    SetVideoSeg &HA000
    WAIT &H3DA, 8
    PUT (0, 0), Vpage(6), PSET
LOOP UNTIL INKEY$ <> ""

FFIX -1

END

RGB:
DATA 63,63,63               : 'WHITE
DATA 63,0,0                 : 'RED
DATA 0,63,0                 : 'GREEN
DATA 0,0,63                 : 'BLUE
DATA 0,63,63                : 'BLUE/GREEN
DATA 63,63,0                : 'RED/GREEN
DATA 63,0,63                : 'RED/BLUE
DATA 32,25,63               : '
DATA 63,0,45                : '
DATA 16,63,20               : '
DATA 45,45,63               : '
DATA 63,45,20               : '
DATA 25,63,45               : '
DATA 56,63,34               : '
DATA 45,25,50               : '
DATA 63,25,11               : '

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

SUB SetVideoSeg (Segment) STATIC
'By Plasma 357 (Jon Petrosky)

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

SUB WuPixelGrad16 (x!, y!, col)

TempC = col AND 15
BaseColor = col - TempC
colx = 15

x1 = FIX(x!)
y1 = FIX(y!)

x2 = x1 + 1
y2 = y1 + 1

xm! = ABS(x! - x1)
ym! = ABS(y! - y1)

xm2! = (1 - xm!)
ym2! = (1 - ym!)

c1 = xm2! * ym2! * colx
c2 = xm! * ym2! * colx
c3 = xm2! * ym! * colx
c4 = xm! * ym! * colx

PSET (x1, y1), c1 + BaseColor
PSET (x2, y1), c2 + BaseColor
PSET (x1, y2), c3 + BaseColor
PSET (x2, y2), c4 + BaseColor

END SUB

