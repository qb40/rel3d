'Code supplement for the projection and rotation article at QBCM
'3d projection and rotation test
'applied to spacefield
'SetVideoSEG by Plasma357
'Relsoft 2004
'rel.betterwebber.com

DECLARE SUB RotateAndProject (Model() AS ANY, AngleX%, AngleY%, AngleZ%)
DECLARE SUB AF.Print (Xpos%, Ypos%, Text$, col%)
DECLARE SUB SetVideoSeg (Segment%)

DEFINT A-Z
REM $DYNAMIC


TYPE Point3d
        x       AS SINGLE           '3d x coord
        y       AS SINGLE           'ditto
        z       AS SINGLE           'ditto
        scrx    AS INTEGER
        scry    AS INTEGER
        Visible AS INTEGER
        clr     AS INTEGER
END TYPE

CONST FALSE = 0, TRUE = NOT FALSE
CONST PI = 3.141593
CONST LENS = 256                    'camera lens(FOV)
CONST XCENTER = 160                 'middle coords of screen 13
CONST YCENTER = 100                 'our center of projection
CONST MAXSTARS = 560                'number of stars


RANDOMIZE TIMER
REDIM SHARED Vpage(32009) AS INTEGER            'our buffer

DIM SHARED stars(MAXSTARS) AS Point3d           'the stars
DIM SHARED Lcos(359) AS SINGLE
DIM SHARED Lsin(359) AS SINGLE

DIM SHARED camx%, camy%, camz%

FOR i = 0 TO 359
    A! = i * PI / 180
    Lcos(i) = COS(A!)
    Lsin(i) = SIN(A!)
NEXT i

'////Initialize the starting values of our stars
FOR i = 0 TO UBOUND(stars)
    ax = RND * 360
    ay = RND * 360
    az = RND * 360
    rad = 64
    stars(i).x = rad * SIN(ay * PI / 180)
    stars(i).y = rad * COS(ax * PI / 180)
    stars(i).z = rad * SIN(az * PI / 180)
NEXT i


CLS
SCREEN 13

'Grey Scale the Palette
 FOR i = 0 TO 255
  OUT &H3C8, i
  OUT &H3C9, i \ 4
  OUT &H3C9, i \ 4
  OUT &H3C9, i \ 4
 NEXT i


REDIM Vpage(32009) AS INTEGER        'Clear offscreen buffer
Vpage(6) = 2560                      'Width 320*8
Vpage(7) = 200                       'Height
LAYER = VARSEG(Vpage(0)) + 1         'Buffer Seg(Ask Plasma)
SetVideoSeg LAYER                    'Set Draw to Buffer

ThetaX = 180
ThetaY = 0
ThetaZ = 0

T# = TIMER
Frame& = 0
camz% = -255
zdir = 1                                'Direction of camz%
DO
    Frame& = Frame& + 1
    SetVideoSeg LAYER                    'Set Draw to Buffer
    LINE (0, 0)-(319, 199), 0, BF       'clear the screen
    camz% = camz% + zdir
    IF camz% > 255 THEN
        zdir = -zdir
    ELSEIF camz% < -255 THEN
        zdir = -zdir
    END IF

    RotateAndProject stars(), ThetaX, ThetaY, ThetaZ
    FOR i = 0 TO UBOUND(stars)

        IF stars(i).Visible THEN                    'if dist>0 then
            PSET (stars(i).scrx, stars(i).scry), stars(i).clr
        ELSE
                                    'do nothing
                                    'you wouldn't wan't to
                                    'divide by 0 would ya? :*)
        END IF
    NEXT i
    SetVideoSeg &HA000              'Set Draw to screen
    WAIT &H3DA, 8                   'Vsynch
    PUT (0, 0), Vpage(6), PSET      'Blit out buffer to screen

    ThetaX = (ThetaX + 1) MOD 360
    ThetaY = (ThetaY + 1) MOD 360
    ThetaZ = (ThetaZ + 1) MOD 360

LOOP UNTIL INKEY$ <> ""

DEF SEG

CLS
SCREEN 0
WIDTH 80
PRINT Frame& / (TIMER - T#)
c$ = INPUT$(1)


END

REM $STATIC
SUB AF.Print (Xpos%, Ypos%, Text$, col%)
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
    Offset% = 8 * ASC(MID$(Text$, i% + 1, 1)) + 14
    FOR j% = 0 TO 7
      DEF SEG = &HFFA6
      Bit% = PEEK(Offset% + j%)
      IF Bit% AND 1 THEN PSET (x%, y% + j%), col% + j%
      IF Bit% AND 2 THEN PSET (x% - 1, y% + j%), col% + j%
      IF Bit% AND 4 THEN PSET (x% - 2, y% + j%), col% + j%
      IF Bit% AND 8 THEN PSET (x% - 3, y% + j%), col% + j%
      IF Bit% AND 16 THEN PSET (x% - 4, y% + j%), col% + j%
      IF Bit% AND 32 THEN PSET (x% - 5, y% + j%), col% + j%
      IF Bit% AND 64 THEN PSET (x% - 6, y% + j%), col% + j%
      IF Bit% AND 128 THEN PSET (x% - 7, y% + j%), col% + j%
    NEXT j%
  NEXT i%

END SUB

SUB RotateAndProject (Model() AS Point3d, AngleX, AngleY, AngleZ) STATIC


'Precalculate the SIN and COS of each angle
cx! = Lcos(AngleX)
sx! = Lsin(AngleX)
cy! = Lcos(AngleY)
sy! = Lsin(AngleY)
cz! = Lcos(AngleZ)
sz! = Lsin(AngleZ)


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


        'Project
        distance% = (LENS - RotZ!)
        IF distance% > 0 THEN
            Model(i).scrx = (LENS * RotX! / distance%) + XCENTER
            Model(i).scry = -(LENS * RotY! / distance%) + YCENTER
            Model(i).Visible = TRUE
            d% = 255 - distance%
            IF d% < 64 THEN d% = 64
            Model(i).clr = d%
        ELSE                            'out of screen
            Model(i).Visible = FALSE
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

