'''Translucent boxes supplement for my vector article
'''SetVideoSeg by Plasma

'Relsoft 2004
DECLARE SUB GradColor (col1%, r1%, g1%, b1%, col2%, r2%, g2%, b2%)
DECLARE SUB TransBox (x1%, y1%, x2%, y2%, clr%)
DECLARE SUB AF.Print (Xpos%, Ypos%, Text$, col%)
DECLARE SUB SetVideoSeg (Segment%)
DEFINT A-Z

RANDOMIZE TIMER

REDIM Vpage(32009) AS INTEGER        ' Clear offscreen buffer
Vpage(6) = 2560
Vpage(7) = 200
Layer = VARSEG(Vpage(0)) + 1


CLS

SCREEN 13


'Read ending grad colors and set gradient pal
RESTORE RGB
FOR i = 0 TO 15
    READ R%, g%, B%
    GradColor i * 16, 0, 0, 0, (i * 16) + 15, R%, g%, B%
NEXT i



''draw an example of the 16 color gradient pal in 16 rows
AF.Print 100, 20, "This is the PAL in 16", 23
AF.Print 100, 30, "color grad(arranged).", 23
FOR i = 0 TO 15
    FOR j = 0 TO 15
        LINE (j * 5, i * 5)-STEP(5, 5), (i * 16) + j, BF
    NEXT j
NEXT i

''Draw continous lines
AF.Print 0, 100, "This is whole 256 colors sequentially.", 23
FOR i = 0 TO 255
    LINE (i, 120)-STEP(0, 50), i, BF
NEXT i

C$ = INPUT$(1)

CLS

x1 = 50
y1 = 50
x2 = x1 + 100
y2 = y1 + 100
clr = 15 + (RND * 240)

DIM Bg(32001) AS INTEGER        'Backgroud

FOR y = 20 TO 199               'draw some nice BG
FOR x = 0 TO 319
    PSET (x, y), x XOR y
NEXT x
NEXT y
GET (0, 0)-(319, 199), Bg

DO

    x2 = x1 + 100
    y2 = y1 + 100

    SetVideoSeg Layer
    LINE (0, 0)-(319, 199), 0, BF   'cls
    PUT (0, 0), Bg, PSET            'BG
    AF.Print 0, 0, "Use arrows to move box.", 150
    AF.Print 0, 10, "Space to change color(Color=" + STR$(clr) + ")", 40
        K$ = INKEY$
        SELECT CASE K$
            CASE CHR$(0) + "H"
                y1 = y1 - 1
            CASE CHR$(0) + "P"
                y1 = y1 + 1
            CASE CHR$(0) + "K"
                x1 = x1 - 1
            CASE CHR$(0) + "M"
                x1 = x1 + 1
            CASE CHR$(32)
                clr = (clr + 1) AND 255'Rotate color
            CASE ELSE
        END SELECT

    TransBox x1, y1, x2, y2, clr
    SetVideoSeg &HA000
    PUT (0, 0), Vpage(6), PSET

LOOP UNTIL K$ = CHR$(27)






END
'RGB colors for our Grad
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

SUB GradColor (col1, r1, g1, b1, col2, r2, g2, b2)
'Makes a gradient color by interpolating the RGB values of the first
'color index (col1) and col2 by the number of cols.
'Only use this in screen 13

R! = r1
g! = g1
B! = b1
cols = (col2 - col1 + 1)
Rstep! = (r2 - r1 + 1) / cols
Gstep! = (g2 - g1 + 1) / cols
Bstep! = (b2 - b1 + 1) / cols
FOR col = col1 TO col2
    R! = R! + Rstep!
    g! = g! + Gstep!
    B! = B! + Bstep!
    IF R! > 63 THEN R! = 63
    IF R! < 0 THEN R! = 0
    IF g! > 63 THEN g! = 63
    IF g! < 0 THEN g! = 0
    IF B! > 63 THEN B! = 63
    IF B! < 0 THEN B! = 0
    OUT &H3C8, col
    OUT &H3C9, FIX(R!)
    OUT &H3C9, FIX(g!)
    OUT &H3C9, FIX(B!)
NEXT col

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

SUB TransBox (x1, y1, x2, y2, clr)

bx1 = x1            'save values in temp vars(BYVAL emulation)
bx2 = x2
by1 = y1
by2 = y2

'Swap values if necessary
IF bx1 > bx2 THEN SWAP bx1, bx2
IF by1 > by2 THEN SWAP by1, by2

TempC = clr AND 15              'and color of box by 15
BaseColor = clr - TempC         'get basecolor

FOR ya = by1 TO by2
FOR xa = bx1 TO bx2
        DestCol = POINT(xa, ya) AND 15      'and screen color by 15
        C = ((TempC + DestCol) \ 2) + BaseColor     'average and add
                                                    'to base color
        PSET (xa, ya), C                    'plot
NEXT xa
NEXT ya

END SUB

