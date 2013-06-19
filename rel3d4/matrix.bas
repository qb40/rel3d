'''3d rotation code supplement using matrices
'''Relsoft June 4, 2004
'''Thanks to:
'''Plasma for SetVideoSeg
'''Mark Feldman for that matrix doc
'''Biskbart for the torus generator
'''Hugo Elias for the WuPixel tute

'''Rel.betterwebber.com

DECLARE SUB FindCentroid (Model() AS ANY)
DECLARE SUB WuPixel (x!, y!, col%)
DECLARE SUB LoadTorus (Rings%, Bands%, RINGRADIUS%, BandRadius%, Model() AS ANY)
DECLARE SUB LoadSphere (Model() AS ANY, radius%, Slices%, Bands%)
DECLARE SUB TransFormPoints (Model() AS ANY, M!())
DECLARE SUB Matrix.SetRotateZ (M!(), Angle%)
DECLARE SUB Matrix.MulVector (M!(), ox!, oy!, oz!, nx!, ny!, nz!)
DECLARE SUB Matrix.MulMatrix (M!(), TM!())
DECLARE SUB Matrix.SetRotateX (M!(), Angle%)
DECLARE SUB Matrix.SetRotateY (M!(), Angle%)
DECLARE SUB Matrix.SetScale (M!(), sx!, sy!, sz!)
DECLARE SUB Matrix.SetTranslate (M!(), Tx!, Ty!, Tz!)
DECLARE SUB Matrix.Clear (M!())
DECLARE SUB Matrix.SetIdentity (M!())
DECLARE SUB LoadSpace (Model() AS ANY, radius%, Numstars%)
DECLARE SUB LoadCubeSolid (Model() AS ANY, xmax%, ymax%, zMax%, Scale!)
DECLARE SUB DrawModel (Model() AS ANY, clr%)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB LoadPlaneSolid (Model() AS ANY, xmax%, ymax%, Scale!)

DEFINT A-Z
REM $DYNAMIC



TYPE Point3d
        x       AS SINGLE                   'Normal 3d coords
        y       AS SINGLE
        z       AS SINGLE
        scrx    AS SINGLE                   'Translated and projected
        scry    AS SINGLE                   '2d Coords
        cull    AS INTEGER                  'visibility check
END TYPE

TYPE Vector
        x       AS SINGLE
        y       AS SINGLE
        z       AS SINGLE
END TYPE

CONST FALSE = 0, TRUE = NOT FALSE

CONST LENS = 256                            'Z
CONST XCENTER = 160                         '??
CONST YCENTER = 100                         '??

CONST PI = 3.14151693#

REDIM SHARED Vpage(32009)  AS INTEGER           'our Video buffer

'Polyhedra stuff
REDIM SHARED Model(1) AS Point3d               '3d  Coords
DIM SHARED Thetax, Thetay, Thetaz              'Angle of rotation
DIM SHARED camx%, camy%, camz%                 'camera

'Matrix
DIM SHARED Matrix!(1 TO 4, 1 TO 4)          'transformation matrix
DIM SHARED TMatrix!(1 TO 4, 1 TO 4)         'temporary matrix to be used
                                            'for multiplication



CLS
SCREEN 0
WIDTH 80


LOCATE 1, 1
PRINT "Choose Model:"
PRINT "1. Solid Cube"
PRINT "2. PlaneSolid"
PRINT "3. Space"
PRINT "4. Sphere"
PRINT "5. Torus"

PRINT
PRINT "Controls:"
PRINT "Camera:<x,y,z)  [A,Z / S,X/ D,C]"

PRINT "Scale:<x,y,z)   [F,V / G,B/ H,N]"
DO
    k$ = INKEY$
LOOP UNTIL k$ <> ""

'Initialize 3d Models

SELECT CASE ASC(k$)
        CASE 49                 '1
            LoadCubeSolid Model(), 9, 9, 9, 14
            FindCentroid Model()
        CASE 50                 '2
            LoadPlaneSolid Model(), 16, 16, 7
            FindCentroid Model()
        CASE 51                 '3
            LoadSpace Model(), 64, 300
        CASE 52                 '4
            LoadSphere Model(), 50, 20, 18
        CASE 53                 '5
            LoadTorus 32, 18, 80, 20, Model()
        CASE ELSE
            LoadCubeSolid Model(), 9, 9, 9, 14
END SELECT


CLS
SCREEN 13
RANDOMIZE TIMER

camx% = 0
camy% = 0
camz% = 0

Thetax = INT(RND * 360)
Thetay = INT(RND * 360)
Thetaz = INT(RND * 360)

Vpage(6) = 2560                 'GET/PUT stuuff
Vpage(7) = 200
Layer = VARSEG(Vpage(0)) + 1    'Easy buffer reference
SetVideoSeg Layer               'Set Draw to buffer
Finished = 0

'Grey Scale the Palette
FOR i = 0 TO 255
  OUT &H3C8, i
  OUT &H3C9, i \ 4
  OUT &H3C9, i \ 4
  OUT &H3C9, i \ 4
NEXT i

scalex! = 1
scaley! = 1
scalez! = 1

DO

     k$ = INKEY$
     SELECT CASE UCASE$(k$)             'Camera control
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
        CASE "F"
            scalex! = scalex! + .02
        CASE "V"
            scalex! = scalex! - .02
        CASE "G"
            scaley! = scaley! + .02
        CASE "B"
            scaley! = scaley! - .02
        CASE "H"
            scalez! = scalez! + .02
        CASE "N"
            scalez! = scalez! - .02
            CASE ELSE

    END SELECT
            

    Thetax = (Thetax + 1) MOD 360          'rotate our angles
    Thetay = (Thetay + 1) MOD 360
    Thetaz = (Thetaz + 1) MOD 360

    cx! = -camx%        'camera as inverse translation matrix
    cy! = -camy%
    cz! = -camz%

    Matrix.SetIdentity Matrix!()            'set up our transform matrix

    'scaling
    Matrix.SetScale TMatrix!(), scalex!, scaley!, scalez!
    Matrix.MulMatrix Matrix!(), TMatrix!()

    'RotateX
    Matrix.SetRotateX TMatrix!(), Thetax
    Matrix.MulMatrix Matrix!(), TMatrix!()

    'RotateY
    Matrix.SetRotateY TMatrix!(), Thetay
    Matrix.MulMatrix Matrix!(), TMatrix!()

    'RotateZ
    Matrix.SetRotateZ TMatrix!(), Thetaz
    Matrix.MulMatrix Matrix!(), TMatrix!()

    'translate
    Matrix.SetTranslate TMatrix!(), cx!, cy!, cz!
    Matrix.MulMatrix Matrix!(), TMatrix!()


    TransFormPoints Model(), Matrix!()
    SetVideoSeg Layer              'set draw to Buffer
    LINE (0, 0)-(319, 199), 0, BF  'CLS
    DrawModel Model(), 255         'Draw 3d model
    SetVideoSeg &HA000             'Set Draw to buffer
    WAIT &H3DA, 8                  'Vsynch
    PUT (0, 0), Vpage(6), PSET     'Blit

LOOP UNTIL k$ = CHR$(27)


CLS
SCREEN 0
WIDTH 80

END

REM $STATIC
SUB DrawModel (Model() AS Point3d, clr%) STATIC

'Draws the 3dmodel in color clr

FOR i = 0 TO UBOUND(Model)
    x! = (Model(i).scrx)             'get projected coords
    y! = (Model(i).scry)
    IF NOT Model(i).cull THEN           'if visible then plot
        WuPixel x!, y!, clr%
    END IF
NEXT i

END SUB

SUB FindCentroid (Model() AS Point3d)
'Centers the model at (0,0,0)

NP = UBOUND(Model)
x! = 0
y! = 0
z! = 0
FOR i = 1 TO NP
    x! = x! + Model(i).x
    y! = y! + Model(i).y
    z! = z! + Model(i).z
NEXT i
xc! = x! / NP
yc! = y! / NP
zc! = z! / NP

FOR i = 0 TO NP - 1
    Model(i).x = Model(i).x - xc!
    Model(i).y = Model(i).y - yc!
    Model(i).z = Model(i).z - zc!
NEXT i

END SUB

SUB LoadCubeSolid (Model() AS Point3d, xmax, ymax, zMax, Scale!)
'Loads a cobe using the cartesian coordinate system
'Parameters:
'Model() = the 3d Model
'xmax = x distance
'ymax = y distance
'zmax =huh?
'Scale!=how big or small the size of the model is. :*)


xm = xmax \ 2           'Center our cube at origin(0,0,0)
ym = ymax \ 2
zm = zMax \ 2
REDIM Model((xmax * ymax * zMax) - 1) AS Point3d    'make it big enough
FOR x = -xm TO xm - 1                   'loop sequentiealy
    FOR y = -ym TO ym - 1
        FOR z = -zm TO zm - 1
            Model(i).x = x * Scale!     'scale it according to
            Model(i).y = y * Scale!     'the parameter
            Model(i).z = z * Scale!
            i = i + 1
        NEXT z
    NEXT y
NEXT x

END SUB

SUB LoadPlaneSolid (Model() AS Point3d, xmax, ymax, Scale!)

'Parameters:
'Model() = the 3d Model
'xmax = x distance
'ymax = y distance
'Scale!=how big or small the size of the model is. :*)

                 
REDIM Model((xmax * ymax))  AS Point3d

xm = xmax \ 2
ym = ymax \ 2
i = 0
FOR x = -xm TO xm - 1
    FOR y = -ym TO ym - 1
        Model(i).x = x * Scale!
        Model(i).y = y * Scale!
        Model(i).z = 0
        i = i + 1
    NEXT y
NEXT x

END SUB

SUB LoadSpace (Model() AS Point3d, radius, Numstars)
'Model() = the 3d Model
'radius the distance of each star from the origin
'Numstars = number of stars

'////Initialize the starting values of our stars
REDIM Model(Numstars) AS Point3d
FOR i = 0 TO UBOUND(Model)
    ax = RND * 360
    ay = RND * 360
    az = RND * 360
    Model(i).x = radius * SIN(ay * PI / 180)
    Model(i).y = radius * COS(ax * PI / 180)
    Model(i).z = radius * SIN(az * PI / 180)
NEXT i

END SUB

SUB LoadSphere (Model() AS Point3d, radius, Slices, Bands)
'Generation code by:
'by Relsoft=me .;*)

'///    x =  p SIN(Phi) COS(theta)
'///    y =  p SIN(Phi) SIN(theta)
'///    z =  p COS(Phi)

REDIM Model((Slices * Bands) - 1) AS Point3d

i = 0
FOR SliceLoop = 0 TO Slices - 1
    Phi! = PI / Slices * SliceLoop
    FOR BandLoop = 0 TO Bands - 1
        Theta! = 2 * -PI / Bands * BandLoop
        Model(i).x = -INT(radius * SIN(Phi!) * COS(Theta!))
        Model(i).y = -INT(radius * SIN(Phi!) * SIN(Theta!))
        Model(i).z = -INT(radius * COS(Phi!))
        i = i + 1
    NEXT BandLoop
NEXT SliceLoop

END SUB

SUB LoadTorus (Rings, Bands, RINGRADIUS, BandRadius, Model() AS Point3d)

'Generation code by:
'by BiskBart

MaxPoint% = Rings * Bands
REDIM Model((MaxPoint%) - 1) AS Point3d

A1! = 2 * PI / Rings: A2! = 2 * PI / Bands
i% = 0
FOR S2% = 0 TO Bands - 1
    FOR S1% = 0 TO Rings - 1
        x1! = COS(S1% * A1!) * RINGRADIUS
        y1! = SIN(S1% * A1!) * RINGRADIUS
        Model(i%).x = x1! + COS(S1% * A1!) * COS(S2% * A2!) * BandRadius
        Model(i%).y = y1! + SIN(S1% * A1!) * COS(S2% * A2!) * BandRadius
        Model(i%).z = SIN(S2% * A2!) * BandRadius
        i% = i% + 1
    NEXT S1%
NEXT S2%


END SUB

SUB Lookat (M!(), V AS Vector)

END SUB

SUB Matrix.Clear (M!())

'sets all elements of the matrix to 0

'   [   0   0   0   0   ]
'   [   0   0   0   0   ]
'   [   0   0   0   0   ]
'   [   0   0   0   0   ]

FOR row = 1 TO UBOUND(M!, 1)
FOR col = 1 TO UBOUND(M!, 2)
    M!(row, col) = 0
NEXT col
NEXT row

END SUB

SUB Matrix.MulMatrix (M!(), TM!())

'Combines 2 matrices M!() and TM!()
'ie. Result = TM x M
'Warning matrix multiplication is not commutative.
'M x TM <> TM x M


DIM Result!(1 TO 4, 1 TO 4)     'resultant matrix to be copied to M!()

Matrix.Clear Result!()

FOR i = 1 TO 3              'since we know the last row = 0,0,0,1
    FOR j = 1 TO 4
        Result!(i, j) = 0
        FOR k = 1 TO 4
            Result!(i, j) = Result!(i, j) + TM!(i, k) * M!(k, j)
        NEXT k
    NEXT j
NEXT i

Result!(4, 1) = 0           'we don't need this yet. :*)
Result!(4, 2) = 0
Result!(4, 3) = 0
Result!(4, 4) = 1

'copy to our original matrix
FOR row = 1 TO UBOUND(M!, 1)
FOR col = 1 TO UBOUND(M!, 2)
    M!(row, col) = Result!(row, col)
NEXT col
NEXT row


END SUB

SUB Matrix.MulVector (M!(), ox!, oy!, oz!, nx!, ny!, nz!)
'Transforms a 3d point using M!()

nx! = ox! * M!(1, 1) + oy! * M!(1, 2) + oz! * M!(1, 3) + M!(1, 4)
ny! = ox! * M!(2, 1) + oy! * M!(2, 2) + oz! * M!(2, 3) + M!(2, 4)
nz! = ox! * M!(3, 1) + oy! * M!(3, 2) + oz! * M!(3, 3) + M!(3, 4)

END SUB

SUB Matrix.Print (M!())
'for testing purposes

FOR row = 1 TO UBOUND(M!, 1)
FOR col = 1 TO UBOUND(M!, 2)
        LOCATE row, col * 15
        PRINT M!(row, col)
NEXT col
NEXT row

END SUB

SUB Matrix.SetIdentity (M!())

'Our initial "do-nothing" matrix

'   [   1   0   0   0   ]
'   [   0   1   0   0   ]
'   [   0   0   1   0   ]
'   [   0   0   0   1   ]

FOR row = 1 TO UBOUND(M!, 1)
FOR col = 1 TO UBOUND(M!, 2)
    IF row = col THEN
        M!(row, col) = 1
    ELSE
        M!(row, col) = 0
    END IF
NEXT col
NEXT row


END SUB

SUB Matrix.SetRotateX (M!(), Angle%)

'rotate in x axis
'ca = COS(angle)
'sa = sin(angle)

'   [   1   0   0   0   ]       x' = x
'   [   0   ca -sa  0   ]       y' = ca * y - sa * z
'   [   0   sa  ca  0   ]       z' = sa * y + ca * z
'   [   0   0   0   1   ]

Theta! = Angle% * PI / 180
ca! = COS(Theta!)
sa! = SIN(Theta!)

Matrix.SetIdentity M!()

M!(2, 2) = ca!
M!(2, 3) = -sa!
M!(3, 2) = sa!
M!(3, 3) = ca!

END SUB

SUB Matrix.SetRotateY (M!(), Angle%)

'rotate in y axis
'ca = COS(angle)
'sa = sin(angle)

'   [   ca  0   sa  0   ]       x' = ca * x + sa * z
'   [   0   1   0   0   ]       y' = y
'   [  -sa  0   ca  0   ]       z' = -sa * x + ca * z
'   [   0   0   0   1   ]

Theta! = Angle% * PI / 180
ca! = COS(Theta!)
sa! = SIN(Theta!)

Matrix.SetIdentity M!()

M!(1, 1) = ca!
M!(1, 3) = sa!
M!(3, 1) = -sa!
M!(3, 3) = ca!

END SUB

SUB Matrix.SetRotateZ (M!(), Angle%)

'rotate in z axis
'ca = COS(angle)
'sa = sin(angle)

'   [   ca -sa  0   0   ]       x' = ca * x - sa * y
'   [   sa  ca  0   0   ]       y' = sa * x + ca * y
'   [   0   0   1   0   ]       z' = z
'   [   0   0   0   1   ]

Theta! = Angle% * PI / 180
ca! = COS(Theta!)
sa! = SIN(Theta!)

Matrix.SetIdentity M!()

M!(1, 1) = ca!
M!(1, 2) = -sa!
M!(2, 1) = sa!
M!(2, 2) = ca!

END SUB

SUB Matrix.SetScale (M!(), sx!, sy!, sz!)


'scales the matrix

'   [   sx  0   0   0   ]
'   [   0   sy  0   0   ]
'   [   0   0   sz  0   ]
'   [   0   0   0   1   ]

Matrix.SetIdentity M!()

M!(1, 1) = sx!
M!(2, 2) = sy!
M!(3, 3) = sz!


END SUB

SUB Matrix.SetTranslate (M!(), Tx!, Ty!, Tz!)

'translates or moves the points

'   [   1   0   0   tx  ]
'   [   0   1   0   ty  ]
'   [   0   0   1   tz  ]
'   [   0   0   0   1   ]

Matrix.SetIdentity M!()

M!(1, 4) = Tx!
M!(2, 4) = Ty!
M!(3, 4) = Tz!

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

SUB TransFormPoints (Model() AS Point3d, M!()) STATIC


FOR i = 0 TO UBOUND(Model)

        x! = Model(i).x
        y! = Model(i).y
        z! = Model(i).z

        Matrix.MulVector M!(), x!, y!, z!, Rotx!, Roty!, Rotz!

        Model(i).cull = FALSE

        'Project
        Distance% = (LENS - Rotz!)
        IF Distance% > 0 THEN
            Model(i).scrx = XCENTER + (LENS * Rotx! / Distance%)
            Model(i).scry = YCENTER - (LENS * Roty! / Distance%)
        ELSE
            Model(i).cull = TRUE
        END IF
NEXT i


END SUB

SUB WuPixel (x!, y!, col)

x1 = FIX(x!)
y1 = FIX(y!)

x2 = x1 + 1
y2 = y1 + 1

xm! = x! - x1
ym! = y! - y1

xm2! = (1 - xm!)
ym2! = (1 - ym!)

c1 = xm2! * ym2! * col
c2 = xm! * ym2! * col
c3 = xm2! * ym! * col
c4 = xm! * ym! * col

PSET (x1, y1), c1
PSET (x2, y1), c2
PSET (x1, y2), c3
PSET (x2, y2), c4

END SUB

