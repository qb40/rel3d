'Code supplement for the vector article
'Gouraud triangle filler
'Original code by CGI Joe(Gneric)

'''Relsoft 2004
'''Rel.Betterwebber.com


DECLARE SUB GradColor (col1%, r1%, g1%, b1%, col2%, r2%, g2%, b2%)
DECLARE SUB GouraudTri (ox1%, oy1%, oc1%, ox2%, oy2%, oc2%, ox3%, oy3%, oc3%)

DEFINT A-Z

RANDOMIZE TIMER

CLS
SCREEN 13

GradColor 0, 0, 0, 0, 128, 32, 63, 32
GradColor 128, 32, 63, 32, 255, 63, 0, 63

DO

x1 = INT(RND * 320)
y1 = INT(RND * 200)

x2 = INT(RND * 320)
y2 = INT(RND * 200)

x3 = INT(RND * 320)
y3 = INT(RND * 200)

c1 = INT(RND * 255)
c2 = INT(RND * 255)
c3 = INT(RND * 255)

GouraudTri x1, y1, c1, x2, y2, c2, x3, y3, c3

LOOP UNTIL INKEY$ <> ""

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

SUB HLine (x1%, x2%, y%, col1&, col2&)
'Generic's routine..
Xx1% = x1%
Xx2% = x2%
yy% = y%
Clr1& = col1&
Clr2& = col2&

IF Xx1% > Xx2% THEN SWAP Xx1%, Xx2%: SWAP Clr1&, Clr2&
xdiff% = 1 + Xx2% - Xx1%

dc& = (Clr2& - Clr1&) \ xdiff%
col& = Clr1&

FOR l% = Xx1% TO Xx2%
PSET (l%, yy%), col& \ 65536
col& = col& + dc&
NEXT l%

END SUB

