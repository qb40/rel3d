'Code supplement for the vector article
'Flat triangle filler
'one-algo, 3 methods
'Slow tri is the functional approach
'Flatri is the DDA approach
'FixFlatTri is the same as flattri but in fixpoint math.

'Original code(Fixpoint method) by CGI Joe(Gneric)

'''Relsoft 2004
'''Rel.Betterwebber.com


DECLARE SUB FlatTri (ox1%, oy1%, ox2%, oy2%, ox3%, oy3%, clr%)
DECLARE SUB SlowFlatTri (ox1%, oy1%, ox2%, oy2%, ox3%, oy3%, clr%)
DECLARE SUB FixFlatTri (ox1%, oy1%, ox2%, oy2%, ox3%, oy3%, clr%)
DEFINT A-Z

RANDOMIZE TIMER

CLS
SCREEN 13

DO

x1 = INT(RND * 320)
y1 = INT(RND * 200)

x2 = INT(RND * 320)
y2 = INT(RND * 200)

x3 = INT(RND * 320)
y3 = INT(RND * 200)

clr = INT(RND * 255)

FixFlatTri x1, y1, x2, y2, x3, y3, clr

LOOP UNTIL INKEY$ <> ""

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
Rx& = Lx&

FOR y = y1 TO y2 - 1
    LINE (Lx& \ FIXPOINT, y)-(Rx& \ FIXPOINT, y), clr
    Lx& = Lx& + delta1&
    Rx& = Rx& + delta3&
NEXT y

'Flat top
'Lower part of triangle

Lx& = x2 * FIXPOINT
FOR y = y2 TO y3
    LINE (Lx& \ FIXPOINT, y)-(Rx& \ FIXPOINT, y), clr
    Lx& = Lx& + delta2&
    Rx& = Rx& + delta3&
NEXT y

END SUB

SUB FlatTri (ox1, oy1, ox2, oy2, ox3, oy3, clr%)

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
'the the fast floating point version.
'Look at the 2 loops below. It first calculate the derivatives and uses DDA
'to get LX!,RX!.  It works because of the fact that multiplication is just
'repeated addition.

DIM x1 AS INTEGER, y1 AS INTEGER        'These are the coodinates of
DIM x2 AS INTEGER, y2 AS INTEGER        'the triangle
DIM x3 AS INTEGER, y3 AS INTEGER

DIM dx1 AS INTEGER, dy1 AS INTEGER      'used for interpolation
DIM dx2 AS INTEGER, dy2 AS INTEGER      'dx1 is the difference in x1
DIM dx3 AS INTEGER, dy3 AS INTEGER      'and so forth
                                                    
DIM delta1!, delta2!, delta3!           'These are used for incrementing the
                                        'inverse-slope of the line that
                                        'composes the triangle

x1 = ox1            'simulate the BYVAL keyword by storing
y1 = oy1            'the vertices in temporary variables

x2 = ox2
y2 = oy2

x3 = ox3
y3 = oy3

IF y2 < y1 THEN         'sort vertices is not in-order
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

dx1 = x2 - x1               'calculate difference in X
dy1 = y2 - y1               'difference in Y
IF dy1 <> 0 THEN            'check for divide by zero
    delta1! = dx1 / dy1     'interpolate Top-Left side
ELSE
    delta1! = 0
END IF

dx2 = x3 - x2               'Lower left side
dy2 = y3 - y2
IF dy2 <> 0 THEN
    delta2! = dx2 / dy2
ELSE
    delta2! = 0
END IF


dx3 = x1 - x3               'Right side
dy3 = y1 - y3
IF dy3 <> 0 THEN
    delta3! = dx3 / dy3
ELSE
    delta3! = 0
END IF


'Flat bottom
'Tup part of triangle
''code: x1 + (y - y1) * delta1!
'is changed to:
'Lx! = x1
'For y=y1 to y2
    Lx! = Lx! + delta1!     '(y - y1) * Delta1! = (Lx!+ Delta1!) * NumberOfLoops
'nexft y

Lx! = x1
Rx! = x1

FOR y = y1 TO y2 - 1
    LINE (Lx!, y)-(Rx!, y), clr
    Lx! = Lx! + delta1!             'increment derivatives
    Rx! = Rx! + delta3!
NEXT y

'Flat top
'Lower part of triangle

Lx! = x2
FOR y = y2 TO y3
    LINE (Lx!, y)-(Rx!, y), clr
    Lx! = Lx! + delta2!
    Rx! = Rx! + delta3!
NEXT y


END SUB

SUB SlowFlatTri (ox1, oy1, ox2, oy2, ox3, oy3, clr%)

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
'the slow one. I included it here for learning purposes only.
'Look at the 2 loops below. It calculates the startx and endx
'by multiplying the distance with the deltas.  Bad way!!!

DIM x1 AS INTEGER, y1 AS INTEGER        'These are the coodinates of
DIM x2 AS INTEGER, y2 AS INTEGER        'the triangle
DIM x3 AS INTEGER, y3 AS INTEGER

DIM dx1 AS INTEGER, dy1 AS INTEGER      'used for interpolation
DIM dx2 AS INTEGER, dy2 AS INTEGER      'dx1 is the difference in x1
DIM dx3 AS INTEGER, dy3 AS INTEGER      'and so forth
                                                     
DIM delta1!, delta2!, delta3!           'These are used for incrementing the
                                        'inverse-slope of the line that
                                        'composes the triangle

x1 = ox1            'simulate the BYVAL keyword by storing
y1 = oy1            'the vertices in temporary variables

x2 = ox2
y2 = oy2

x3 = ox3
y3 = oy3

IF y2 < y1 THEN         'sort vertices is not in-order
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

dx1 = x2 - x1               'calculate difference in X
dy1 = y2 - y1               'difference in Y
IF dy1 <> 0 THEN            'check for divide by zero
    delta1! = dx1 / dy1     'interpolate Top-Left side
ELSE
    delta1! = 0
END IF

dx2 = x3 - x2               'Lower left side
dy2 = y3 - y2
IF dy2 <> 0 THEN
    delta2! = dx2 / dy2
ELSE
    delta2! = 0
END IF


dx3 = x1 - x3               'Right side
dy3 = y1 - y3
IF dy3 <> 0 THEN
    delta3! = dx3 / dy3
ELSE
    delta3! = 0
END IF


'Flat bottom
'Tup part of triangle
'code: x1 + (y - y1) * delta1!
    'interpolates distance(y-y1) with delta1!
    'pretty dlow way of doing things.

FOR y = y1 TO y2
    Lx! = x1 + (y - y1) * delta1!       'left-x of tri
    Rx! = x1 + (y - y1) * delta3!       'right-x of tri
    LINE (Lx!, y)-(Rx!, y), clr
NEXT y

'Flat top
'Lower part of triangle

FOR y = y2 TO y3
    Lx! = x2 + (y - y2) * delta2!       'left-x of tri
    Rx! = x1 + (y - y1) * delta3!       'left-x of tri
    LINE (Lx!, y)-(Rx!, y), clr
NEXT y

END SUB

