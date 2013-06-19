'Code supplement for the rotation article at QBCM
'2d rotation test
'SetVideoSEG by Plasma357
'Relsoft 2004
'rel.betterwebber.com

DECLARE SUB AF.Print (Xpos%, Ypos%, Text$, col%)
DECLARE SUB SetVideoSeg (Segment%)

DEFINT A-Z
REM $DYNAMIC


TYPE Point2d
        x       AS SINGLE           'cartesion x coord
        y       AS SINGLE           'y coord
END TYPE

CONST PI = 3.141593
CONST XCENTER = 160                 'middle coords of screen 13
CONST YCENTER = 100                 'our center of rotation


RANDOMIZE TIMER
REDIM SHARED Vpage(32009) AS INTEGER            'our buffer

DIM Points(3) AS Point2d            'points we have to rotate

CLS
SCREEN 13

'Init Points
'formula:
'//x=COS(i% * 90 * PI / 180) * Radius  
'//sets angle increments at 90 degree intervals (I*90) then
'//converts it to radain by multiplying the angle by PI/180
'//so that COS and SIN would work as they only accept radian measure.
'//multiplying it again by the set radius scales it bigger 50 units
'//away from center.  Using 90 degrees interval makes it a sure way
'//to have  values of -1 and 1 as all points are either perpendicular or
'//collinear. ;*)
xRadius = 50                 'Distance of point from center
yRadius = 30
FOR i = 0 TO UBOUND(Points)
    Points(i).x = COS(i% * 90 * PI / 180) * xRadius      'Polar to cartesian
    Points(i).y = SIN(i% * 90 * PI / 180) * yRadius      'conversion
    'test points
    PSET (XCENTER + Points(i).x, YCENTER + Points(i).y), 15
NEXT i

LOCATE 1, 1
PRINT "This is our points(Model) to rotate on  the z-axis(Normal 2d rottion)."
PRINT
PRINT "Press any key to rotate..."
C$ = INPUT$(1)



REDIM Vpage(32009) AS INTEGER        'Clear offscreen buffer
Vpage(6) = 2560                      'Width 320*8
Vpage(7) = 200                       'Height
LAYER = VARSEG(Vpage(0)) + 1         'Buffer Seg(Ask Plasma)
SetVideoSeg LAYER                    'Set Draw to Buffer

Angle = 0                   'start from 0
DO
        SetVideoSeg LAYER                   'Set Draw to Buffer
        LINE (0, 0)-(319, 199), 0, BF       'clear the screen
        radAngle! = Angle * PI / 180        'convert degrees to radians
        '''rotate the each point
        FOR i = 0 TO UBOUND(Points)
            x = Points(i).x                 'store
            y = Points(i).y
                '2d cartesian rotation formula
            xrot = (x * COS(radAngle!)) - (y * SIN(radAngle!))
            yrot = (y * COS(radAngle!)) + (x * SIN(radAngle!))
                'draw
            PSET (XCENTER + xrot, YCENTER + yrot), 15
        NEXT i
        AF.Print 0, 0, "Angle=" + STR$(Angle), 23       'print angle
        SetVideoSeg &HA000              'Set Draw to screen
        WAIT &H3DA, 8                   'Vsynch
        PUT (0, 0), Vpage(6), PSET      'Blit out buffer to screen
        Angle = (Angle + 1) MOD 360     'increment angle and MOD it to be
                                        'sure its values are between 0 and 360

LOOP UNTIL INKEY$ <> ""

DEF SEG

CLS
SCREEN 0
WIDTH 80

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

