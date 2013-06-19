'''3d model generators using rectangular, cylindrical and spherical
'''coordinate systems
'''Cube by SCM
'''Torus by Biskbart
'''All others by Relsoft
'''SetvideoSeg by Plasma357

'''Relsoft 2004
'''Rel.betterwebber.com

DECLARE SUB SaveModel (File$, Model() AS ANY)
DECLARE SUB LoadCubeSolid (Model() AS ANY, xmax%, ymax%, zMax%, Scale!)
DECLARE SUB DrawModel (Model() AS ANY, clr%)
DECLARE SUB SetVideoSeg (Segment%)
DECLARE SUB RotateAndProject (Model() AS ANY, AngleX%, AngleY%, AngleZ%)
DECLARE SUB LoadCylinder (Model() AS ANY, radius%, Slices%, Bands%, zdist%)
DECLARE SUB LoadCube (Model() AS ANY, radius%)
DECLARE SUB LoadHelix (Model() AS ANY, radius%, NumPoints%, Cycles%, zdist%)
DECLARE SUB LoadSphere (Model() AS ANY, radius%, Slices%, Bands%)
DECLARE SUB LoadTorus (Rings%, Bands%, RINGRADIUS%, BandRadius%, Model() AS ANY)
DECLARE SUB LoadPlane (Model() AS ANY, radius%)
DECLARE SUB LoadPlaneSolid (Model() AS ANY, xmax%, ymax%, Scale!)
DEFINT A-Z
REM $DYNAMIC



TYPE Point3d
        x       AS SINGLE                   'Normal 3d coords
        y       AS SINGLE
        z       AS SINGLE
        xr      AS SINGLE                   'Rotated  3d coords
        yr      AS SINGLE
        zr      AS SINGLE
        scrx    AS INTEGER                  'Translated and projected
        scry    AS INTEGER                  '2d Coords
        cull    AS INTEGER                   'visibility check
END TYPE

CONST FALSE = 0, TRUE = NOT FALSE

CONST LENS = 256                            'Z
CONST XCENTER = 160                         '??
CONST YCENTER = 100                         '??

CONST TORNUMRINGS = 20          'Number of Rings outside TORUS
CONST TORNUMBANDS = 10          'Number of PIXEL per RING
CONST TORRINGRADIUS = 65       'Radius of the Ring
CONST TORBANDRADIUS = 20        'Radius of the BAND

CONST PI = 3.14151693#

REDIM SHARED Vpage(32009)  AS INTEGER
DIM SHARED Lcos(359) AS SINGLE
DIM SHARED Lsin(359) AS SINGLE

'Polyhedra stuff
REDIM SHARED Model(1) AS Point3d               '3d  Coords
DIM SHARED Thetax, Thetay, Thetaz
DIM SHARED zcenter, camx%, camy%, camz%


'PreCalc sin and cos lookuptable

FOR i = 0 TO 359
    a! = i * PI / 180
    Lcos(i) = COS(a!)
    Lsin(i) = SIN(a!)
NEXT i



'Initialize 3d stuff
CLS
SCREEN 0
WIDTH 80

LOCATE 1, 1
PRINT "Choose Model:"
PRINT "1. Cube"
PRINT "2. Solid Cube"
PRINT "3. Sphere [Default]"
PRINT "4. Helix"
PRINT "5. Cylinder"
PRINT "6. Torus"
PRINT "7. Plane"
PRINT "8. PlaneSolid"
DO
    K$ = INKEY$
LOOP UNTIL K$ <> ""

SELECT CASE ASC(K$)
        CASE 49
            LoadCube Model(), 50
        CASE 50
            LoadCubeSolid Model(), 6, 8, 6, 14          'cyrix 686 pr233 woot!
        CASE 51
            LoadSphere Model(), 50, 10, 10
        CASE 52
            LoadHelix Model(), 50, 200, 8, 1
        CASE 53
            LoadCylinder Model(), 40, 15, 15, 15
        CASE 54
            LoadTorus TORNUMRINGS, TORNUMBANDS, TORRINGRADIUS, TORBANDRADIUS, Model()
        CASE 55
            LoadPlane Model(), 50
        CASE 56
            LoadPlaneSolid Model(), 16, 16, 7
        CASE ELSE
            LoadSphere Model(), 50, 10, 20
END SELECT
SaveModel "C:\Qbasic\3dTute\Cube.Dat", Model()
CLS
SCREEN 13
RANDOMIZE TIMER

camx% = 0
camy% = 0
camz% = 0

Thetax = INT(RND * 360)
Thetay = INT(RND * 360)
Thetaz = INT(RND * 360)

Vpage(6) = 2560
Vpage(7) = 200
Layer = VARSEG(Vpage(0)) + 1
SetVideoSeg Layer
Finished = 0

DO

     K$ = INKEY$
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
        CASE CHR$(0) + "H"
            Thetay = (Thetay + 1) MOD 360
        CASE CHR$(0) + "P"
            Thetay = (Thetay - 1)
                   IF Thetay < 0 THEN Thetay = 359
        CASE CHR$(0) + "K"
            Thetax = (Thetax - 1)
                   IF Thetax < 0 THEN Thetax = 359
        CASE CHR$(0) + "M"
            Thetax = (Thetax + 1) MOD 360
        CASE CHR$(0) + CHR$(&H53)
            Thetaz = (Thetaz - 1)
                   IF Thetaz < 0 THEN Thetaz = 359
        CASE CHR$(0) + CHR$(&H52)
            Thetaz = (Thetaz + 1) MOD 360
        CASE ELSE
    END SELECT
            

     Thetax = (Thetax + 1) MOD 360
     Thetay = (Thetay + 1) MOD 360
     Thetaz = (Thetaz + 1) MOD 360
     RotateAndProject Model(), Thetax, Thetay, Thetaz
     SetVideoSeg Layer
     LINE (0, 0)-(319, 199), 0, BF
     DrawModel Model(), 12
     SetVideoSeg &HA000
     WAIT &H3DA, 8
     PUT (0, 0), Vpage(6), PSET

LOOP UNTIL K$ = CHR$(27)


CLS
SCREEN 0
WIDTH 80

END

REM $STATIC
SUB DrawModel (Model() AS Point3d, clr%) STATIC

FOR i = 0 TO UBOUND(Model)
    x% = INT(Model(i).scrx)
    y% = INT(Model(i).scry)
    IF NOT Model(i).cull THEN
        PSET (x%, y%), clr%
    END IF
NEXT i

END SUB

SUB LoadCube (Model() AS Point3d, radius)
'Generation code by:
'by Steve McCarthy(SCM)

REDIM Model(7) AS Point3d
Theta! = PI / 4
dTheta! = PI / 2

FOR P = 0 TO 7
  Model(P).x = radius * SGN(COS(Theta!))          ' x
  Model(P).y = radius * SGN(SIN(Theta!))          ' y
  Model(P).z = radius - (radius * 2) * (P \ 4)    ' z
  Theta! = Theta! + dTheta!
NEXT P


END SUB

SUB LoadCubeSolid (Model() AS Point3d, xmax, ymax, zMax, Scale!)
'Generation code by:
'by Relsoft=me .;*)
                   
xm = xmax \ 2
ym = ymax \ 2
zm = zMax \ 2
REDIM Model((xmax * ymax * zMax) - 1) AS Point3d
FOR x = -xm TO xm - 1
    FOR y = -ym TO ym - 1
        FOR z = -zm TO zm - 1
            Model(i).x = x * Scale!
            Model(i).y = y * Scale!
            Model(i).z = -z * Scale!
            i = i + 1
        NEXT z
    NEXT y
NEXT x

END SUB

SUB LoadCylinder (Model() AS Point3d, radius, Slices, Bands, zdist)
'Generation code by:
'by Relsoft=me .;*)

'I. Cylindrical to cartesian
'///  x = COS(theta)
'///  y = SIN(theta)
'///  z = z

REDIM Model((Slices * Bands) - 1) AS Point3d
i = 0
z! = zdist * Slices / 2
FOR Slice = 0 TO Slices - 1
FOR Band = 0 TO Bands - 1
    Theta! = (2 * PI / Bands) * Band
    Model(i).x = radius * COS(Theta!)
    Model(i).y = radius * SIN(Theta!)
    Model(i).z = -z!
    i = i + 1
NEXT Band
    z! = z! - zdist
NEXT Slice

END SUB

SUB LoadHelix (Model() AS Point3d, radius, NumPoints, Cycles, zdist)
'Generation code by:
'by Relsoft=me .;*)

'I. Cylindrical to cartesian
'///  x = COS(theta)
'///  y = SIN(theta)
'///  z = z

REDIM Model(NumPoints)  AS Point3d

ADelta! = Cycles * NumPoints / 180
FOR i = 0 TO NumPoints
    angle! = angle! + ADelta!
    Model(i).x = radius * COS(angle! * PI / 180)
    Model(i).y = radius * SIN(angle! * PI / 180)
    Model(i).z = i * zdist - ((zdist * NumPoints) / 2)
NEXT i

END SUB

SUB LoadPlane (Model() AS Point3d, radius)
'Generation code by:
'by Relsoft = me ;*)

REDIM Model(3) AS Point3d
Theta! = 0
FOR i = 0 TO 3
  Model(i).x = radius * (COS(Theta!))
  Model(i).y = radius * (SIN(Theta!))
  Model(i).z = 0
  Theta! = Theta! + PI / 2
NEXT i

END SUB

SUB LoadPlaneSolid (Model() AS Point3d, xmax, ymax, Scale!)
'Generation code by:
'by Relsoft=me .;*)
                 
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
        X1! = COS(S1% * A1!) * RINGRADIUS
        Y1! = SIN(S1% * A1!) * RINGRADIUS
        Model(i%).x = X1! + COS(S1% * A1!) * COS(S2% * A2!) * BandRadius
        Model(i%).y = Y1! + SIN(S1% * A1!) * COS(S2% * A2!) * BandRadius
        Model(i%).z = SIN(S2% * A2!) * BandRadius
        i% = i% + 1
    NEXT S1%
NEXT S2%



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
        Model(i).zr = RotZ!
        Model(i).cull = FALSE

        'Project
        Distance% = (LENS - RotZ!)
        IF Distance% > 0 THEN
            Model(i).scrx = XCENTER + (LENS * RotX! / Distance%)
            Model(i).scry = YCENTER - (LENS * RotY! / Distance%)
        ELSE
            Model(i).cull = TRUE
        END IF
NEXT i

END SUB

SUB SaveModel (File$, Model() AS Point3d)

F = FREEFILE
OPEN File$ FOR OUTPUT AS #F
FOR i = 0 TO UBOUND(Model)
    a% = Model(i).x
    b% = Model(i).y
    c% = Model(i).z
    PRINT #F, "DATA " + LTRIM$(STR$(a%)) + "," + LTRIM$(STR$(b%)) + "," + LTRIM$(STR$(c%))
NEXT i

CLOSE #F

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

