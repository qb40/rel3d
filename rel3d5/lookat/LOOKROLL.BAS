'****************************************************************************
'RelSoft 2004
'Http://Rel.Betterwebber.com
'
'Lookat code supplement
'This also supports roll angle (Puke cam)
'Controls:  Mouse: "Free look"
'           W/S:  Move Player
'           A/D: Rotate camera(Roll)
'           MouseButtons: Forward and back
'           Enter:  switch Rendermode
'
'Special Thanks to:
'Dr. Davidstein for making this cool Lightwave3d model
'v1ctor, Plasma, and Dav for FFIX
'Cornell Huth for the Iterative Quicksort algo(I modified it a lil)
'Milo Sedlaceck for Multikey
'Toshi for some help

'Load QB with the /L option. Ie. QB/L
'****************************************************************************
DECLARE SUB Matrix.Lookat (M!(), Target AS ANY, Camera AS ANY, RollAngle%)
DECLARE SUB FFix (Mode%)
DECLARE SUB Mouse.Reset (Regs AS ANY)
DECLARE SUB Mouse.SetXY (Mousex%, Mousey%, Regs AS ANY)
DECLARE SUB Mouse.Stats (Mousex%, Mousey%, RB%, LB%, Regs AS ANY)
DECLARE SUB Rel.LoadBMP (File$, SwitchPal%)
DECLARE SUB AF.TriT (DestSeg%, X1%, Y1%, U1%, V1%, X2%, Y2%, U2%, V2%, X3%, Y3%, U3%, V3%, TSEG%, TOff%)
DECLARE SUB Matrix.SetScale (M!(), sx!, sy!, sz!)
DECLARE FUNCTION MULTIKEY% (T%)
DECLARE SUB AF.Pcopy (DestSeg%, SRCSEG%)
DECLARE SUB AF.Cls (DestSeg%, c%)
DECLARE SUB AF.TriF (DestSeg%, X1%, Y1%, X2%, Y2%, X3%, Y3%, c%)
DECLARE SUB LoadLightWaveQ (M() AS ANY, P() AS ANY, File$, Textsize%)
DECLARE SUB ClipPolys (Model() AS ANY, Tri() AS ANY)
DECLARE SUB Qsort (SortArray() AS ANY, Lower%, Upper%)
DECLARE SUB LoadDataSyn9 (Model() AS ANY, Poly() AS ANY)
DECLARE SUB FindCentroid (Model() AS ANY)
DECLARE SUB GetTextures (Textures%())
DECLARE SUB Matrix.SetTranslate (M!(), tx!, Ty!, Tz!)
DECLARE SUB Matrix.MulVector (M!(), ox!, oy!, oz!, nx!, ny!, nz!)
DECLARE SUB TransFormPoints (Model() AS ANY, M!())
DECLARE SUB Matrix.MulMatrix (M!(), TM!())
DECLARE SUB Vector.Normalize (v AS ANY)
DECLARE FUNCTION Vector.Dot! (V1 AS ANY, V2 AS ANY)
DECLARE SUB Vector.Cross (v AS ANY, V1 AS ANY, V2 AS ANY)
DECLARE SUB Matrix.SetIdentity (M!())
DECLARE SUB Matrix.SetCamera (M!(), R AS ANY, U AS ANY, F AS ANY)
DECLARE SUB DrawL3dT (Model() AS ANY, Tri() AS ANY)
DECLARE SUB SortPolys (Model() AS ANY, Poly() AS ANY)
DECLARE SUB ScaleModel (M() AS ANY, Scale!)
DEFINT A-Z
REM $DYNAMIC

TYPE RegType
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
END TYPE

TYPE BMPHeaderType                              'All values are for 320*200*256 bmp
        ID              AS STRING * 2           '"BM"
        size            AS LONG                 'width*heigth+1078= 65078
        RSV1            AS INTEGER              '0 Reserved
        RSV2            AS INTEGER              '0 Reserved
        offset          AS LONG                 '1078  First Pixel(Scanline order)
        HORZ            AS LONG                 '40
        WID             AS LONG                 '320
        HEI             AS LONG                 '200
        PLANES          AS INTEGER              '1   num of Planes
        BPP             AS INTEGER              '8   Bits per Plane
        COMPRESSION     AS LONG                 '0
        IMAGESIZE       AS LONG                 '64000  Width *Height
        XRES            AS LONG                 '3790  X pels
        YRES            AS LONG                 '3780  Y pels
        CLRUSED         AS LONG                 '0 Colors used
        CLRIMPORT       AS LONG                 '0 Colors Important
        Pal             AS STRING * 1024        'Order=Blue*4, Green*4, Red*4, 0 * 4
END TYPE

TYPE Point3d
        x       AS SINGLE                   'Normal 3d coords
        y       AS SINGLE
        z       AS SINGLE
        xr      AS SINGLE                   'Rotated  3d coords
        yr      AS SINGLE
        zr      AS SINGLE
        scrx    AS INTEGER                  'Translated and projected
        scry    AS INTEGER                  '2d Coords
END TYPE

TYPE PolyType
        p1      AS INTEGER                  'vertex 1
        p2      AS INTEGER
        p3      AS INTEGER
        U1      AS INTEGER          'Texture U and V
        V1      AS INTEGER
        U2      AS INTEGER
        V2      AS INTEGER
        U3      AS INTEGER
        V3      AS INTEGER
        Clr     AS INTEGER                  'color
        idx     AS INTEGER                  'index for later use
        zcenter AS INTEGER
        Clip    AS INTEGER                  '0=draw
        tx      AS INTEGER
        v       AS INTEGER
END TYPE

TYPE Vector
        x       AS SINGLE
        y       AS SINGLE
        z       AS SINGLE
END TYPE


'KEY CONSTANTS
CONST KEYESC = 1, KEYENTER = 28, KEYSPACE = 57, KEYTAB = &HF
CONST KEYUP = 72, KEYDOWN = 80, KEYLEFT = 75, KEYRIGHT = 77

'Of course!!!!! :)
CONST FALSE = 0, TRUE = NOT FALSE
CONST VIDEO = &HA000

CONST KEYW = &H11, KEYA = &H1E, KEYSS = &H1F, KEYD = &H20

CONST LENS = 256
CONST XCENTER = 160                         '??
CONST YCENTER = 100                         '??

CONST PI = 3.14151693#
CONST cTsize% = 64                          'TextureSize



DIM SHARED Vpage(31999)  AS INTEGER           'our Video buffer
DIM SHARED Layer

'Polyhedra stuff
REDIM SHARED Model(1) AS Point3d               '3d  Coords
REDIM SHARED ModelPoly(1) AS PolyType          'faces/Polys
REDIM SHARED NewPoly(1) AS PolyType            'faces/Polys
DIM SHARED MAXPOLY AS INTEGER                  'For Newpoly fast sort

'Matrix stuff

DIM SHARED Campos AS Vector                 'Origin
DIM SHARED CamLookAT AS Vector              'Target
'Matrix
DIM SHARED Matrix!(1 TO 4, 1 TO 4)          'transformation matrix
DIM SHARED TMatrix!(1 TO 4, 1 TO 4)         'temporary matrix to be used
                                            'for multiplication

'Mouse stuff
DIM Regs AS RegType

DIM SHARED TextSeg%, Tsize%
Tsize% = ((cTsize% * cTsize%) + 4) \ 2
DIM SHARED Texture%(Tsize%, 3)
TextSeg% = VARSEG(Texture%(0, 0))


DIM SHARED Path$
DIM SHARED Textured             'if textured or flat


'FFix 0                         'Unrem this when you compile
CLS

Path$ = ""
Path$ = "C:\Qbasic\3dtute\Chapter5\LandLook\"
File$ = "Land1.L3d"
LoadLightWaveQ Model(), ModelPoly(), Path$ + File$, cTsize%
REDIM NewPoly(UBOUND(ModelPoly)) AS PolyType
FindCentroid Model()

CLS
SCREEN 13

GetTextures Texture%()
Mouse.Reset Regs
Campos.x = 0
Campos.y = -27
Campos.z = 0

RollAngle = 90              '90 degrees is perpendicular to floor


Textured = TRUE
Layer = VARSEG(Vpage(0))
Finished = 0
Vsynch = FALSE
Frame& = 0
T# = TIMER
Dummy = MULTIKEY(-1)
DO

    Frame& = Frame& + 1

    Mouse.Stats Mousex%, Mousey%, RB%, LB%, Regs
   
    Controlx = Mousex%
    IF Controlx < 1 THEN
        Controlx = 319
        Mouse.SetXY 319, Controly, Regs
    END IF
    IF Controlx > 319 THEN
        Controlx = 0
        Mouse.SetXY 0, Controly, Regs
    END IF
    Controly = Mousey%
    IF Controly < 1 THEN
        Controly = 199
        Mouse.SetXY Controlx, 199, Regs
    END IF
    IF Controly > 199 THEN
        Controly = 0
        Mouse.SetXY Controlx, 0, Regs
    END IF

    IF MULTIKEY(KEYESC) THEN
            Finished = TRUE
    END IF
    IF MULTIKEY(KEYSPACE) THEN
            Vsynch = NOT Vsynch
    END IF
    IF MULTIKEY(KEYENTER) THEN
            Textured = NOT Textured
    END IF


    Tmx = Controlx + 1
    Tmy = Controly + 1

    Theta! = 2 * -PI * (Tmx) / 320             'Azimuth
    Phi! = PI * Tmy / 200                      'elevation

    CamLookAT.x = SIN(Phi!) * COS(Theta!)     'Spherical system
    CamLookAT.z = SIN(Phi!) * SIN(Theta!)
    CamLookAT.y = COS(Phi!)

    IF MULTIKEY(KEYA) THEN                      'Rotate left
        RollAngle = (RollAngle + 1) MOD 360
    END IF
    IF MULTIKEY(KEYD) THEN                      'Rotate right
        RollAngle = (RollAngle - 1) MOD 360
    END IF


    IF MULTIKEY(KEYW) OR LB% OR MULTIKEY(KEYUP) THEN               'Forward
        Campos.x = Campos.x - CamLookAT.x * 3
        Campos.y = Campos.y - CamLookAT.y * 3   'rem this to walk on the plane
        Campos.z = Campos.z - CamLookAT.z * 3
    END IF

    IF MULTIKEY(KEYSS) OR RB% OR MULTIKEY(KEYDOWN) THEN              'Back
        Campos.x = Campos.x + CamLookAT.x * 3
        Campos.y = Campos.y + CamLookAT.y * 3   'rem this to walk on the plane
        Campos.z = Campos.z + CamLookAT.z * 3
    END IF


    'Move Target vector just a lil forward of camera
    CamLookAT.x = CamLookAT.x + Campos.x
    CamLookAT.y = CamLookAT.y + Campos.y
    CamLookAT.z = CamLookAT.z + Campos.z

    'Do the matrix transforms
    Matrix.SetIdentity Matrix!()            'set up our transform matrix
    Matrix.SetScale TMatrix!(), 10!, 10!, 10!
    Matrix.MulMatrix Matrix!(), TMatrix!()

    'Movement/Translation
    tx! = Campos.x
    Ty! = Campos.y
    Tz! = Campos.z
    Matrix.SetTranslate TMatrix!(), tx!, Ty!, Tz!
    Matrix.MulMatrix Matrix!(), TMatrix!()


    'Camera
    Matrix.Lookat TMatrix!(), CamLookAT, Campos, RollAngle
    Matrix.MulMatrix Matrix!(), TMatrix!()

    TransFormPoints Model(), Matrix!()

    ClipPolys Model(), ModelPoly()
    SortPolys Model(), NewPoly()


    AF.Cls Layer, 0
    DrawL3dT Model(), NewPoly()      'WireFrame


    IF Vsynch THEN
        WAIT &H3DA, 8                  'Vsynch
    END IF
    AF.Pcopy VIDEO, Layer

LOOP UNTIL Finished

Dummy = MULTIKEY(-2)

CLS
SCREEN 0
WIDTH 80

PRINT "FPS:"; Frame& / (TIMER - T#)


'FFix -1                    'Unrem this when you compile
c$ = INPUT$(1)


END

REM $STATIC
SUB AF.Cls (DestSeg%, c%)
'Clears the Layer to a specified color
'Parameters:
'Destseg=Layer or page to clear(use VARSEG or VIDEO/&HA000)

STATIC Asm.Cls%(), InitDone%

IF InitDone% = 0 THEN
    Asm$ = ""
    Asm$ = Asm$ + "5589E58B46088CDA8EC031FF8A460688C489C166C1E01089C8B9803EF366AB8EDA5DCA0400"

    CodeLen% = LEN(Asm$)
    IF (CodeLen% MOD 2) <> 0 THEN CodeLen% = CodeLen% + 1
    size% = CodeLen% \ 4
    REDIM Asm.Cls%(size%)

    DEF SEG = VARSEG(Asm.Cls%(0))
    FOR i% = 0 TO CodeLen% \ 2
            Byte% = VAL("&H" + MID$(Asm$, (i% * 2) + 1, 2))
            POKE VARPTR(Asm.Cls%(0)) + i%, Byte%
    NEXT i%
    DEF SEG
    InitDone% = 1
END IF


    DEF SEG = VARSEG(Asm.Cls%(0))
    CALL ABSOLUTE(BYVAL DestSeg%, BYVAL c%, VARPTR(Asm.Cls%(0)))
    DEF SEG
    Asm$ = ""

END SUB

SUB AF.Pcopy (DestSeg%, SRCSEG%)
'copies the sourceseg to destseg
'this acheives double buffering to eliminate flicker
'same as QB's PCOPY command
'Parameters:
'Destseg=Layer or page to copy to(usually VIDEO)
'srcseg=the source layer to copy from



STATIC Asm.Pcopy%(), InitDone%

IF InitDone% = 0 THEN
    Asm$ = ""
    Asm$ = Asm$ + "5589E58CD88B4E088B56068EC18EDA31FF31F6B9803EF366A58ED85DCA0400"

    CodeLen% = LEN(Asm$)
    IF (CodeLen% MOD 2) <> 0 THEN CodeLen% = CodeLen% + 1
    size% = CodeLen% \ 4
    REDIM Asm.Pcopy%(size%)

    DEF SEG = VARSEG(Asm.Pcopy%(0))
    FOR i% = 0 TO CodeLen% \ 2
            Byte% = VAL("&H" + MID$(Asm$, (i% * 2) + 1, 2))
            POKE VARPTR(Asm.Pcopy%(0)) + i%, Byte%
    NEXT i%
    DEF SEG
    InitDone% = 1
END IF

    DEF SEG = VARSEG(Asm.Pcopy%(0))
    CALL ABSOLUTE(BYVAL DestSeg%, BYVAL SRCSEG%, VARPTR(Asm.Pcopy%(0)))
    DEF SEG
    Asm$ = ""

END SUB

SUB AF.Pset (DestSeg%, x%, y%, c%)
'Same as QB's PSET command
'Paramenters:
'Dest seg=See AF.Box
'X,Y:coordinates of the pixel
'C:color of the pixel to draw

STATIC Asm.Pset%(), InitDone%

IF InitDone% = 0 THEN
    Asm$ = ""
    Asm$ = "8BC58BEC8E460A8B560683FA007C2E909081FAC7007F2690908B5E0883FB"
    Asm$ = Asm$ + "007C1C909081FB3F017F14909066678D1492C1E2068BFA03FB8A4E"
    Asm$ = Asm$ + "0426880D8BE8CA0800"

    CodeLen% = LEN(Asm$)
    IF (CodeLen% MOD 2) <> 0 THEN CodeLen% = CodeLen% + 1
    size% = CodeLen% \ 4
    REDIM Asm.Pset%(size%)

    DEF SEG = VARSEG(Asm.Pset%(0))
    FOR i% = 0 TO CodeLen% \ 2
            Byte% = VAL("&H" + MID$(Asm$, (i% * 2) + 1, 2))
            POKE VARPTR(Asm.Pset%(0)) + i%, Byte%
    NEXT i%
    DEF SEG
    InitDone% = 1
END IF

    DEF SEG = VARSEG(Asm.Pset%(0))
    CALL ABSOLUTE(BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL c%, VARPTR(Asm.Pset%(0)))
    DEF SEG
    Asm$ = ""

END SUB

SUB AF.TriF (DestSeg%, X1%, Y1%, X2%, Y2%, X3%, Y3%, c%)
'Draws a Filled triangle in a Layer(DestSeg%)
'Clipping supported
'parameters:
'Destseg=Layer to draw the Box(use VARSEG)
'X1,Y1,X2,Y2,X3,Y3=Coordinates of the box
'C=Color
' RELTRIF                            115 (0073h)

STATIC Asm.TriF%(), InitDone%
IF InitDone% = 0 THEN
    Asm$ = ""
    Asm$ = "C80200008956FE3BC37E039090933D3F017F4A90903D00007C45909083FB"
    Asm$ = Asm$ + "007C3C909081FB3F017F4590908BD3422BD0742B909086E98BF9C1"
    Asm$ = Asm$ + "EF0203F903F88B46FE8AE08BCA8BD066C1E0108BC28BD9C1E90283"
    Asm$ = Asm$ + "E303F366AB8BCBF3AAC9C38BD32BD003C23D00007CF333C0EBAEBB"
    Asm$ = Asm$ + "3F01EBB8C80C000006578B46108B5E0C3BD87D179090938B4E128B"
    Asm$ = Asm$ + "560E87CA894610895E0C894E1289560E8B46088B5E103BC37D1790"
    Asm$ = Asm$ + "90938B4E0A8B561287CA894608895E10894E0A8956128B46088B5E"
    Asm$ = Asm$ + "0C3BC37D179090938B4E0A8B560E87CA894608895E0C894E0A8956"
    Asm$ = Asm$ + "0E66C746FC000000008B5E0C2B5E10741990908B460E2B461266C1"
    Asm$ = Asm$ + "E0106699660FBFCB66F7F9668946FC66C746F8000000008B5E082B"
    Asm$ = Asm$ + "5E0C741990908B460A2B460E66C1E0106699660FBFCB66F7F96689"
    Asm$ = Asm$ + "46F866C746F4000000008B5E102B5E08741990908B46122B460A66"
    Asm$ = Asm$ + "C1E0106699660FBFCB66F7F9668946F48E46148B461266C1E01066"
    Asm$ = Asm$ + "8BD88B4E1083F9007C22909081F9C7007F1A9090665066535166C1"
    Asm$ = Asm$ + "F81066C1FB108B5606E877FE59665B6658660346FC66035EF4413B"
    Asm$ = Asm$ + "4E0C7ECB662B5EF48B460E66C1E0108B4E0C83F9007C22909081F9"
    Asm$ = Asm$ + "C7007F1A9090665066535166C1F81066C1FB108B5606E834FE5966"
    Asm$ = Asm$ + "5B6658660346F866035EF4413B4E087CCB5F07C9CA1000"

    CodeLen% = LEN(Asm$)
    IF (CodeLen% MOD 2) <> 0 THEN CodeLen% = CodeLen% + 1
    size% = CodeLen% \ 4
    REDIM Asm.TriF%(size%)

    DEF SEG = VARSEG(Asm.TriF%(0))
    FOR i% = 0 TO CodeLen% \ 2
            Byte% = VAL("&H" + MID$(Asm$, (i% * 2) + 1, 2))
            POKE VARPTR(Asm.TriF%(0)) + i%, Byte%
    NEXT i%
    DEF SEG
    InitDone% = 1
END IF

    DEF SEG = VARSEG(Asm.TriF%(0))
    CALL ABSOLUTE(BYVAL DestSeg%, BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL X3%, BYVAL Y3%, BYVAL c%, VARPTR(Asm.TriF%(0)) + 115)
    DEF SEG
    Asm$ = ""

END SUB

SUB AF.TriT (DestSeg%, X1%, Y1%, U1%, V1%, X2%, Y2%, U2%, V2%, X3%, Y3%, U3%, V3%, TSEG%, TOff%)
'Draws a Textured triangle in a Layer(DestSeg%)
'Clipping supported
' RELTRIT                            612 (0264h)

STATIC Asm.TriT%(), InitDone%
IF InitDone% = 0 THEN
    Asm$ = ""
    Asm$ = "C81800008B461A8B5E183BC37E2C93668B4E12668B560E6687CA89461A89"
    Asm$ = Asm$ + "5E18668B460A668B5E06669366894E126689560E6689460A66895E"
    Asm$ = Asm$ + "06668B4E12668B560A66894EF4668956F08B461A8B5E18668B560E"
    Asm$ = Asm$ + "668B4E12662BD12BD80F848401895EEE8BC36698668BCA66916699"
    Asm$ = Asm$ + "66F7F9668946FC668B5606668B4E0A662BD18B46EE6698668BCA66"
    Asm$ = Asm$ + "91669966F7F9668946F88B461A8B5E183D3F010F8F44013D00000F"
    Asm$ = Asm$ + "8C410183FB000F8C360181FB3F010F8F63018B4E048BD34249894E"
    Asm$ = Asm$ + "EC2BD00F841E018B4E1686E98BF9C1EF0203F903F88C66EA8BDA83"
    Asm$ = Asm$ + "FB040F8E3F0180E303C1EA028AEB8956E88CE88AC8668B5EF4668B"
    Asm$ = Asm$ + "56F0668BC266C1E8102346ECD3E08BF00376EA668BC366C1E81023"
    Asm$ = Asm$ + "46EC03F08A440426880566035EFC660356F8668BC266C1E8102346"
    Asm$ = Asm$ + "ECD3E08BF00376EA668BC366C1E8102346EC03F08A440426884501"
    Asm$ = Asm$ + "66035EFC660356F8668BC266C1E8102346ECD3E08BF00376EA668B"
    Asm$ = Asm$ + "C366C1E8102346EC03F08A44042688450266035EFC660356F8668B"
    Asm$ = Asm$ + "C266C1E8102346ECD3E08BF00376EA668BC366C1E8102346EC03F0"
    Asm$ = Asm$ + "8A44042688450366035EFC660356F883C704FF4EE80F8547FF84ED"
    Asm$ = Asm$ + "7430668BC266C1E8102346ECD3E08BF00376EA668BC366C1E81023"
    Asm$ = Asm$ + "46EC03F08A440426880566035EFC660356F847FECD75D0C9C21800"
    Asm$ = Asm$ + "8BC8F7D98BD32BD003C23D00007CED6633C08BC1660FAF46FC6601"
    Asm$ = Asm$ + "46F46633C08BC1660FAF46F8660146F06633C0E98EFEBB3F01E997"
    Asm$ = Asm$ + "FE8956E88CE88AC8668B5EF4668B56F0668BC266C1E8102346ECD3"
    Asm$ = Asm$ + "E08BF00376EA668BC366C1E8102346EC03F08A440426880566035E"
    Asm$ = Asm$ + "FC660356F847FF4EE875CFC9C21800C842000006571E568B461E8B"
    Asm$ = Asm$ + "5E163BD87D31938B4E208B561887CA89461E895E16894E20895618"
    Asm$ = Asm$ + "8B4E1C8B561487CA894E1C8956148B4E1A8B561287CA894E1A8956"
    Asm$ = Asm$ + "128B460E8B5E1E3BC37D31938B4E108B562087CA89460E895E1E89"
    Asm$ = Asm$ + "4E108956208B4E0C8B561C87CA894E0C89561C8B4E0A8B561A87CA"
    Asm$ = Asm$ + "894E0A89561A8B460E8B5E163BC37D31938B4E108B561887CA8946"
    Asm$ = Asm$ + "0E895E16894E108956188B4E0C8B561487CA894E0C8956148B4E0A"
    Asm$ = Asm$ + "8B561287CA894E0A89561266C746FC0000000066C746E800000000"
    Asm$ = Asm$ + "66C746D4000000008B5E162B5E1E743D8B46182B462066C1E01066"
    Asm$ = Asm$ + "99660FBFCB66F7F9668946FC8B46142B461C66C1E010669966F7F9"
    Asm$ = Asm$ + "668946E88B46122B461A66C1E010669966F7F9668946D466C746F8"
    Asm$ = Asm$ + "0000000066C746E40000000066C746D0000000008B5E0E2B5E1674"
    Asm$ = Asm$ + "3D8B46102B461866C1E0106699660FBFCB66F7F9668946F88B460C"
    Asm$ = Asm$ + "2B461466C1E010669966F7F9668946E48B460A2B461266C1E01066"
    Asm$ = Asm$ + "9966F7F9668946D066C746F40000000066C746E00000000066C746"
    Asm$ = Asm$ + "CC000000008B5E1E2B5E0E743D8B46202B461066C1E0106699660F"
    Asm$ = Asm$ + "BFCB66F7F9668946F48B461C2B460C66C1E010669966F7F9668946"
    Asm$ = Asm$ + "E08B461A2B460A66C1E010669966F7F9668946CC8E46228E5E088B"
    Asm$ = Asm$ + "46068EE08BF08B1CC1EB03895EC08BC33D02007505BB0100EB463D"
    Asm$ = Asm$ + "04007505BB0200EB3C3D08007505BB0300EB323D10007505BB0400"
    Asm$ = Asm$ + "EB283D20007505BB0500EB1E3D40007505BB0600EB143D80007505"
    Asm$ = Asm$ + "BB0700EB0A3D00010F858601BB08008EEB8B4620669866C1E01066"
    Asm$ = Asm$ + "8BD8668946F066895EEC8B461C669866C1E010668BD8668946DC66"
    Asm$ = Asm$ + "895ED88B461A669866C1E010668BD8668946C866895EC48B4E1E89"
    Asm$ = Asm$ + "4EC2837EC2007C3C817EC2C7007F35668B46F0668B5EEC66C1F810"
    Asm$ = Asm$ + "66C1FB105053668B4EDC668B56D88B46C25066516652668B46C866"
    Asm$ = Asm$ + "8B5EC4665066538B4EC051E8DFFA668B46FC668B5EF4668B4EE866"
    Asm$ = Asm$ + "8B56E0660146F066015EEC66014EDC660156D8668B46D4668B5ECC"
    Asm$ = Asm$ + "660146C866015EC4FF46C28B4EC23B4E167E83668B5EF466295EEC"
    Asm$ = Asm$ + "668B56E0662956D8668B46CC662946C48B4618669866C1E0106689"
    Asm$ = Asm$ + "46F08B4614669866C1E010668946DC8B4612669866C1E010668946"
    Asm$ = Asm$ + "C88B4E16894EC2837EC2007C3C817EC2C7007F35668B46F0668B5E"
    Asm$ = Asm$ + "EC66C1F81066C1FB105053668B4EDC668B56D88B46C25066516652"
    Asm$ = Asm$ + "668B46C8668B5EC4665066538B4EC051E81DFA668B46F8668B5EF4"
    Asm$ = Asm$ + "668B4EE4668B56E0660146F066015EEC66014EDC660156D8668B46"
    Asm$ = Asm$ + "D0668B5ECC660146C866015EC4FF46C28B4EC23B4E0E7C835E1F5F"
    Asm$ = Asm$ + "07C9CA1E00"

    CodeLen% = LEN(Asm$)
    IF (CodeLen% MOD 2) <> 0 THEN CodeLen% = CodeLen% + 1
    size% = CodeLen% \ 4
    REDIM Asm.TriT%(size%)

    DEF SEG = VARSEG(Asm.TriT%(0))
    FOR i% = 0 TO CodeLen% \ 2
            Byte% = VAL("&H" + MID$(Asm$, (i% * 2) + 1, 2))
            POKE VARPTR(Asm.TriT%(0)) + i%, Byte%
    NEXT i%
    DEF SEG
    InitDone% = 1
END IF

    DEF SEG = VARSEG(Asm.TriT%(0))
    CALL ABSOLUTE(BYVAL DestSeg%, BYVAL X1%, BYVAL Y1%, BYVAL U1%, BYVAL V1%, BYVAL X2%, BYVAL Y2%, BYVAL U2%, BYVAL V2%, BYVAL X3%, BYVAL Y3%, BYVAL U3%, BYVAL V3%, BYVAL TSEG%, BYVAL TOff%, VARPTR(Asm.TriT%(0)) + 612)
    DEF SEG
    Asm$ = ""

END SUB

SUB ClipPolys (Model() AS Point3d, Tri() AS PolyType) STATIC
'Clips polygons using standard 2d+nearZ clipping
'If polygon is visible add it to our draw array so that
'we only sort visible polygons.
'Bacface culling supported

Newi% = 0
FOR i = 1 TO UBOUND(Tri)
    X1 = Model(Tri(i).p1).scrx       'Get triangles from "projected"
    X2 = Model(Tri(i).p2).scrx       'X and Y coords since Znormal
    X3 = Model(Tri(i).p3).scrx       'Does not require a Z coord
    Y1 = Model(Tri(i).p1).scry       'V1= Point1 connected to V2 then
    Y2 = Model(Tri(i).p2).scry       'V2 to V3 and so on...
    Y3 = Model(Tri(i).p3).scry
    z1 = Model(Tri(i).p1).zr
    z2 = Model(Tri(i).p2).zr
    z3 = Model(Tri(i).p3).zr

    'Clip
    Znormal& = CLNG(X2 - X1) * CLNG(Y1 - Y3) - CLNG(Y2 - Y1) * CLNG(X1 - X3)
    IF (Znormal& < 0) THEN               '<0 so vector facing us

    IF z1 > 1 THEN
     IF z2 > 1 THEN
      IF z3 > 1 THEN
        TxL% = (X1 > -1) + (X2 > -1) + (X3 > -1)
        TxR% = (X1 < 320) + (X2 < 320) + (X3 < 320)
        TyL% = (Y1 > -1) + (Y2 > -1) + (Y3 > -1)
        TyR% = (Y1 < 200) + (Y2 < 200) + (Y3 < 200)
        IF TxL% <> 0 OR TxR% <> 0 OR TyL% <> 0 OR TyR% <> 0 THEN
            Newi% = Newi% + 1
            NewPoly(Newi%) = Tri(i)
        END IF
      END IF
     END IF
    END IF

    END IF
NEXT i

MAXPOLY = Newi%

END SUB

SUB DrawL3dT (Model() AS Point3d, Tri() AS PolyType) STATIC

FOR i = 1 TO MAXPOLY
    j = Tri(i).idx
    X1 = Model(Tri(j).p1).scrx       'Get triangles from "projected"
    X2 = Model(Tri(j).p2).scrx       'X and Y coords since Znormal
    X3 = Model(Tri(j).p3).scrx       'Does not require a Z coord
    Y1 = Model(Tri(j).p1).scry       'V1= Point1 connected to V2 then
    Y2 = Model(Tri(j).p2).scry       'V2 to V3 and so on...
    Y3 = Model(Tri(j).p3).scry
   
    U1 = Tri(j).U1
    V1 = Tri(j).V1
    U2 = Tri(j).U2
    V2 = Tri(j).V2
    U3 = Tri(j).U3
    V3 = Tri(j).V3
    T% = Tri(j).tx
    TOff% = VARPTR(Texture%(0, T%))
    IF Textured THEN
        AF.TriT Layer%, X1, Y1, U1, V1, X2, Y2, U2, V2, X3, Y3, U3, V3, TextSeg%, TOff%
    ELSE
        AF.TriF Layer%, X1, Y1, X2, Y2, X3, Y3, Tri(j).Clr
    END IF
NEXT i


END SUB

SUB FFix (Mode%) STATIC
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

SUB FindCentroid (Model() AS Point3d)
NP = UBOUND(Model)
x! = 0
y! = 0
z! = 0
FOR i = 1 TO NP
    x! = x! + Model(i).x
    y! = y! + Model(i).y
    z! = z! + Model(i).z
NEXT i
XC! = x! / NP
YC! = y! / NP
zc! = z! / NP

FOR i = 1 TO NP
    Model(i).x = Model(i).x - XC!
    Model(i).y = Model(i).y - YC!
    Model(i).z = Model(i).z - zc!
NEXT i

END SUB

SUB GetTextures (Textures())


File$ = Path$ + "Land1.bmp"
Rel.LoadBMP File$, TRUE
GET (0, 0)-(cTsize - 1, cTsize - 1), Textures(0, 0)
File$ = Path$ + "bark.bmp"
Rel.LoadBMP File$, 0
GET (0, 0)-(cTsize - 1, cTsize - 1), Textures(0, 1)

File$ = Path$ + "leaf1.bmp"
Rel.LoadBMP File$, 0
GET (0, 0)-(cTsize - 1, cTsize - 1), Textures(0, 2)



END SUB

SUB LoadLightWaveQ (M() AS Point3d, P() AS PolyType, File$, Textsize%)
Textured = -1
DIM utx!(1 TO 4)
DIM vtx!(1 TO 4)

NumTris = 0
NumPoints = 0

currentVert = 0
currentFace = 0
vtxcounter = 0

F = FREEFILE
OPEN File$ FOR INPUT AS #F
DO WHILE NOT EOF(F)
        LINE INPUT #F, Temp$
        SELECT CASE MID$(Temp$, 1, 1)
                CASE "f"
                        NumTris = NumTris + 1
                        GOSUB CountVertsQ
                CASE "#"
                        PRINT Temp$
                        GOSUB GetHeaderQ
                CASE ELSE
        END SELECT
LOOP
CLOSE #F

PRINT "Numverts:"; NumPoints
PRINT "NumFace:"; NumTris
c$ = INPUT$(1)
REDIM P(1 TO NumTris) AS PolyType
F = FREEFILE
OPEN File$ FOR INPUT AS #F
DO WHILE NOT EOF(F)
        LINE INPUT #F, Temp$
        GOSUB RemodelQ
        SELECT CASE LTRIM$(RTRIM$(MID$(Temp$, 1, 2)))
                CASE "v"
                        GOSUB GetVertexQ
                CASE "vt"
                        vtxcounter = vtxcounter + 1
                        GOSUB GetUV
                CASE "f"
                        IF NOT Textured THEN
                                GOSUB GetFacesQ
                        ELSE
                                GOSUB GetFacesQT
                        END IF
                        UVcounter = 0
                CASE "us"
                        mtlTx% = VAL(MID$(Temp$, 7, 2))
                CASE ELSE
        END SELECT
LOOP
CLOSE #F

EXIT SUB


'========================Header=======================================
GetHeaderQ:

'///Vertex
Vnum = INSTR(Temp$, "Vertices:")
IF Vnum <> 0 THEN
        Vnum = Vnum + LEN("Vertices:")
        NumPoints = VAL(MID$(Temp$, Vnum + 1, LEN(Temp$) - Vnum))
        REDIM M(1 TO NumPoints) AS Point3d
END IF

'///Polys
Tnum = INSTR(Temp$, "Faces:")
IF Tnum <> 0 THEN
        Tnum = Tnum + LEN("Faces:")
END IF


RETURN

'=========================Vertex====================================

GetVertexQ:
currentVert = currentVert + 1
spc1 = INSTR(Temp$, " ") + 1        '1st space
spc2 = INSTR(spc1, Temp$, " ") + 1  '2nd
spc3 = INSTR(spc2, Temp$, " ") + 1  '3rd
Leng1 = (spc2 - spc1)               'length of value
Leng2 = (spc3 - spc2)
Leng3 = (LEN(Temp$) - spc3) + 2

M(currentVert).x = -VAL(MID$(Temp$, spc1, Leng1))
M(currentVert).y = VAL(MID$(Temp$, spc2, Leng2))
M(currentVert).z = -VAL(MID$(Temp$, spc3, Leng3))

RETURN


'=========================Faces======================================
GetFacesQ:

spacecnt = 0
FOR T = 1 TO LEN(Temp$)
        IF MID$(Temp$, T, 1) = " " THEN
                spacecnt = spacecnt + 1
        END IF
NEXT T

IF spacecnt < 4 THEN
        currentFace = currentFace + 1
        spc1 = INSTR(Temp$, " ") + 1       '1st space
        spc2 = INSTR(spc1, Temp$, " ") + 1 '2nd
        spc3 = INSTR(spc2, Temp$, " ") + 1 '3rd

        Leng1 = (spc2 - spc1)           'length of value
        Leng2 = (spc3 - spc2)
        Leng3 = (LEN(Temp$) - spc3) + 2

        P(currentFace).p1 = VAL(MID$(Temp$, spc1, Leng1))        'value of Face 1
        P(currentFace).p2 = VAL(MID$(Temp$, spc2, Leng2))        'ditto
        P(currentFace).p3 = VAL(MID$(Temp$, spc3, Leng3))
        P(currentFace).idx = currentFace
        P(currentFace).Clr = 20 + INT(RND * 128)
        P(currentFace).tx = mtlTx%
ELSE
        spc1 = INSTR(Temp$, " ") + 1       '1st space
        spc2 = INSTR(spc1, Temp$, " ") + 1 '2nd
        spc3 = INSTR(spc2, Temp$, " ") + 1 '3rd
        spc4 = INSTR(spc3, Temp$, " ") + 1 '3rd

        Leng1 = (spc2 - spc1)           'length of value
        Leng2 = (spc3 - spc2)
        Leng3 = (spc4 - spc3)
        Leng4 = (LEN(Temp$) - spc4) + 2

        p1 = VAL(MID$(Temp$, spc1, Leng1))        'value of Face 1
        p2 = VAL(MID$(Temp$, spc2, Leng2))        'ditto
        p3 = VAL(MID$(Temp$, spc3, Leng3))
        p4 = VAL(MID$(Temp$, spc4, Leng4))

        currentFace = currentFace + 1
        P(currentFace).p1 = p1
        P(currentFace).p2 = p2
        P(currentFace).p3 = p4
        P(currentFace).idx = currentFace
        P(currentFace).Clr = 20 + INT(RND * 128)
        P(currentFace).tx = mtlTx%

        currentFace = currentFace + 1
        P(currentFace).p1 = p2
        P(currentFace).p2 = p3
        P(currentFace).p3 = p4
        P(currentFace).idx = currentFace
        P(currentFace).Clr = 20 + INT(RND * 128)
        P(currentFace).tx = mtlTx%

END IF

RETURN


'=========================Faces and UV============================
GetFacesQT:
spacecnt = 0
FOR T = 1 TO LEN(Temp$)
        IF MID$(Temp$, T, 1) = " " THEN
                spacecnt = spacecnt + 1
        END IF
NEXT T

IF spacecnt < 4 THEN
        currentFace = currentFace + 1
        spc1 = INSTR(Temp$, " ") + 1       '1st space
        spc2 = INSTR(spc1, Temp$, " ") + 1 '2nd
        spc3 = INSTR(spc2, Temp$, " ") + 1 '3rd

        Leng1 = (spc2 - spc1)           'length of value
        Leng2 = (spc3 - spc2)
        Leng3 = (LEN(Temp$) - spc3) + 2

        Char1$ = MID$(Temp$, spc1, Leng1)
        Char2$ = MID$(Temp$, spc2, Leng2)
        Char3$ = MID$(Temp$, spc3, Leng3)


        Spc1x = INSTR(Char1$, "/") - 1    '1st slash
        Spc2x = INSTR(Char2$, "/") - 1    '1st slash
        Spc3x = INSTR(Char3$, "/") - 1    '1st slash

        Face1$ = LEFT$(Char1$, Spc1x)
        Face2$ = LEFT$(Char2$, Spc2x)
        Face3$ = LEFT$(Char3$, Spc3x)

        vtx1$ = RIGHT$(Char1$, Leng1 - Spc1x - 1)
        vtx2$ = RIGHT$(Char2$, Leng2 - Spc2x - 1)
        vtx3$ = RIGHT$(Char3$, Leng3 - Spc3x - 2)

        vtx1 = ABS(VAL(vtx1$))
        vtx2 = ABS(VAL(vtx2$))
        vtx3 = ABS(VAL(vtx3$))

        P(currentFace).p1 = VAL(Face1$)        'value of Face 1
        P(currentFace).p2 = VAL(Face2$)
        P(currentFace).p3 = VAL(Face3$)
        P(currentFace).idx = currentFace
        P(currentFace).Clr = 20 + INT(RND * 128)
        P(currentFace).tx = mtlTx%

        P(currentFace).U1 = utx!(3) * Textsize%
        P(currentFace).V1 = vtx!(3) * Textsize%
        P(currentFace).U2 = utx!(2) * Textsize%
        P(currentFace).V2 = vtx!(2) * Textsize%
        P(currentFace).U3 = utx!(1) * Textsize%
        P(currentFace).V3 = vtx!(1) * Textsize%


ELSE
        spc1 = INSTR(Temp$, " ") + 1       '1st space
        spc2 = INSTR(spc1, Temp$, " ") + 1 '2nd
        spc3 = INSTR(spc2, Temp$, " ") + 1 '3rd
        spc4 = INSTR(spc3, Temp$, " ") + 1 '4th

        Leng1 = (spc2 - spc1)           'length of value
        Leng2 = (spc3 - spc2)
        Leng3 = (spc4 - spc3)
        Leng4 = (LEN(Temp$) - spc4) + 2

        Char1$ = MID$(Temp$, spc1, Leng1)
        Char2$ = MID$(Temp$, spc2, Leng2)
        Char3$ = MID$(Temp$, spc3, Leng3)
        Char4$ = MID$(Temp$, spc4, Leng4)


        Spc1x = INSTR(Char1$, "/") - 1    '1st slash
        Spc2x = INSTR(Char2$, "/") - 1    '1st slash
        Spc3x = INSTR(Char3$, "/") - 1    '1st slash
        Spc4x = INSTR(Char4$, "/") - 1    '1st slash

        Face1$ = LEFT$(Char1$, Spc1x)
        Face2$ = LEFT$(Char2$, Spc2x)
        Face3$ = LEFT$(Char3$, Spc3x)
        Face4$ = LEFT$(Char4$, Spc4x)

        vtx1$ = RIGHT$(Char1$, Leng1 - Spc1x - 1)
        vtx2$ = RIGHT$(Char2$, Leng2 - Spc2x - 1)
        vtx3$ = RIGHT$(Char3$, Leng3 - Spc3x - 1)
        vtx4$ = RIGHT$(Char4$, Leng4 - Spc4x - 2)

        vtx1 = ABS(VAL(vtx1$))
        vtx2 = ABS(VAL(vtx2$))
        vtx3 = ABS(VAL(vtx3$))
        vtx4 = ABS(VAL(vtx4$))

        p1 = VAL(Face1$)        'value of Face 1
        p2 = VAL(Face2$)
        p3 = VAL(Face3$)
        p4 = VAL(Face4$)


        'First tri
        currentFace = currentFace + 1
        P(currentFace).p1 = p1
        P(currentFace).p2 = p2
        P(currentFace).p3 = p4
        P(currentFace).idx = currentFace
        P(currentFace).Clr = 20 + INT(RND * 128)
        P(currentFace).tx = mtlTx%

        P(currentFace).U1 = utx!(4) * Textsize%
        P(currentFace).V1 = vtx!(4) * Textsize%
        P(currentFace).U2 = utx!(2) * Textsize%
        P(currentFace).V2 = vtx!(2) * Textsize%
        P(currentFace).U3 = utx!(1) * Textsize%
        P(currentFace).V3 = vtx!(1) * Textsize%

        'Second tri
        currentFace = currentFace + 1
        P(currentFace).p1 = p2
        P(currentFace).p2 = p3
        P(currentFace).p3 = p4
        P(currentFace).idx = currentFace
        P(currentFace).Clr = 20 + INT(RND * 128)

        P(currentFace).U1 = utx!(4) * Textsize%
        P(currentFace).V1 = vtx!(4) * Textsize%
        P(currentFace).U2 = utx!(3) * Textsize%
        P(currentFace).V2 = vtx!(3) * Textsize%
        P(currentFace).U3 = utx!(2) * Textsize%
        P(currentFace).V3 = vtx!(2) * Textsize%
        P(currentFace).tx = mtlTx%




END IF


RETURN


'//==============================Remodel==========================
RemodelQ:
Temp2$ = ""
Leng = LEN(Temp$)
FOR i = 1 TO Leng
        Char$ = MID$(Temp$, i, 2)
        IF Char$ = "- " THEN
                Temp2$ = Temp2$ + LEFT$(Char$, 1)
                i = i + 1
        ELSE
                Temp2$ = Temp2$ + LEFT$(Char$, 1)
        END IF
NEXT i
Temp$ = Temp2$
RETURN


CountVertsQ:
        spacecnt = 0
        FOR T = 1 TO LEN(Temp$)
                IF MID$(Temp$, T, 1) = " " THEN
                        spacecnt = spacecnt + 1
                END IF
                IF MID$(Temp$, T, 1) = "/" THEN
                        Textured = -1
                END IF
        NEXT T
        IF spacecnt > 3 THEN NumTris = NumTris + 1
RETURN


'//==============================UV=============================

GetUV:
UVcounter = UVcounter + 1
'Vertex U,V
spc1 = INSTR(Temp2$, " ") + 1       '1st space
spc2 = INSTR(spc1, Temp2$, " ") + 1 '2nd

Leng1 = (spc2 - spc1)           'length of value
Leng2 = (LEN(Temp2$) - spc2) + 2

U$ = MID$(Temp2$, spc1, Leng1)
v$ = MID$(Temp2$, spc2, Leng2)

UText! = VAL(U$)
VText! = VAL(v$)


utx!(UVcounter) = UText!
vtx!(UVcounter) = 1! - VText!

RETURN

END SUB

SUB Matrix.Lookat (M!(), Target AS Vector, Camera AS Vector, RollAngle%) STATIC
'This sub returns a trasformation matrix defined fom 3 vectors U,F,R
'This type of viewing system is perfect for FPS's. ;*)
'I intentionally left out the roll angle since I have really no use for it.


DIM F AS Vector         'Forward vector
DIM U AS Vector         'Up vector
DIM R AS Vector         'Right vector


F.x = Target.x - Camera.x
F.y = Target.y - Camera.y
F.z = Target.z - Camera.z

Vector.Normalize F         'normalize forward vector

'Calculate roll angle
ra! = RollAngle% * 3.141593 / 180
U.x = COS(ra!)
U.y = SIN(ra!)
U.z = 0


Vector.Normalize U

Dot! = Vector.Dot!(F, U)

U.x = U.x - F.x * Dot!        'Align U to F
U.y = U.y - F.y * Dot!
U.z = U.z - F.z * Dot!


Vector.Normalize U        'normalize the Up vector

Vector.Cross R, U, F      'R = normal to plane f and u

Vector.Normalize R

'Set up camera matrix
Matrix.SetCamera M!(), R, U, F

END SUB

SUB Matrix.MulMatrix (M!(), TM!())

'Combines 2 matrices M!() and TM!()
'ie. Result = TM x M
'Warning matrix multiplication is not commutative.
'M x TM <> TM x M


DIM Result!(1 TO 4, 1 TO 4)     'resultant matrix to be copied to M!()


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
FOR Col = 1 TO UBOUND(M!, 2)
    M!(row, Col) = Result!(row, Col)
NEXT Col
NEXT row


END SUB

SUB Matrix.MulVector (M!(), ox!, oy!, oz!, nx!, ny!, nz!)
'Transforms a 3d point using M!()

nx! = ox! * M!(1, 1) + oy! * M!(1, 2) + oz! * M!(1, 3) + M!(1, 4)
ny! = ox! * M!(2, 1) + oy! * M!(2, 2) + oz! * M!(2, 3) + M!(2, 4)
nz! = ox! * M!(3, 1) + oy! * M!(3, 2) + oz! * M!(3, 3) + M!(3, 4)

END SUB

SUB Matrix.SetCamera (M!(), R AS Vector, U AS Vector, F AS Vector)
'   [   Rx  Uy  Fz  0   ]
'   [   Rx  Uy  Fz  0   ]
'   [   Rx  Uy  Fz  0   ]
'   [   0   0   0   1   ]

Matrix.SetIdentity M!()

M!(1, 1) = R.x
M!(1, 2) = R.y
M!(1, 3) = R.z

M!(2, 1) = U.x
M!(2, 2) = U.y
M!(2, 3) = U.z

M!(3, 1) = F.x
M!(3, 2) = F.y
M!(3, 3) = F.z

END SUB

SUB Matrix.SetIdentity (M!())

'Our initial "do-nothing" matrix

'   [   1   0   0   0   ]
'   [   0   1   0   0   ]
'   [   0   0   1   0   ]
'   [   0   0   0   1   ]

FOR row = 1 TO UBOUND(M!, 1)
FOR Col = 1 TO UBOUND(M!, 2)
    IF row = Col THEN
        M!(row, Col) = 1
    ELSE
        M!(row, Col) = 0
    END IF
NEXT Col
NEXT row


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

SUB Matrix.SetTranslate (M!(), tx!, Ty!, Tz!)

'translates or moves the points

'   [   1   0   0   tx  ]
'   [   0   1   0   ty  ]
'   [   0   0   1   tz  ]
'   [   0   0   0   1   ]

Matrix.SetIdentity M!()

M!(1, 4) = tx!
M!(2, 4) = Ty!
M!(3, 4) = Tz!

END SUB

SUB Mouse.Hide (Regs AS RegType)
Regs.ax = 2
CALL Interrupt(&H33, Regs, Regs)
END SUB

SUB Mouse.Reset (Regs AS RegType)
Regs.ax = 0
CALL Interrupt(&H33, Regs, Regs)
END SUB

SUB Mouse.SetXY (Mousex%, Mousey%, Regs AS RegType)
Regs.ax = 4
Regs.cx = Mousex% * 2
Regs.dx = Mousey%
CALL Interrupt(&H33, Regs, Regs)
END SUB

SUB Mouse.Show (Regs AS RegType)
Regs.ax = 1
CALL Interrupt(&H33, Regs, Regs)
END SUB

SUB Mouse.Stats (Mousex%, Mousey%, RB%, LB%, Regs AS RegType)
Regs.ax = 3
CALL Interrupt(&H33, Regs, Regs)
LB% = Regs.bx AND 1
RB% = (Regs.bx AND 2) \ 2
Mousex% = Regs.cx \ 2
Mousey% = Regs.dx
END SUB

FUNCTION MULTIKEY (T)
'Milo Sedlacek's multikey

STATIC kbcontrol%(), kbmatrix%(), Firsttime, StatusFlag

IF Firsttime = 0 THEN          'Initalize
 DIM kbcontrol%(128)
 DIM kbmatrix%(128)
 code$ = ""
 code$ = code$ + "E91D00E93C00000000000000000000000000000000000000000000000000"
 code$ = code$ + "00001E31C08ED8BE24000E07BF1400FCA5A58CC38EC0BF2400B85600FAAB"
 code$ = code$ + "89D8ABFB1FCB1E31C08EC0BF2400BE14000E1FFCFAA5A5FB1FCBFB9C5053"
 code$ = code$ + "51521E560657E460B401A8807404B400247FD0E088C3B700B0002E031E12"
 code$ = code$ + "002E8E1E100086E08907E4610C82E661247FE661B020E6205F075E1F5A59"
 code$ = code$ + "5B589DCF"
 DEF SEG = VARSEG(kbcontrol%(0))
 FOR i% = 0 TO 155                     ' Load ASM
     d% = VAL("&h" + MID$(code$, (i% * 2) + 1, 2))
     POKE VARPTR(kbcontrol%(0)) + i%, d%
 NEXT i%
 i& = 16       ' I think this stuff connects the interrupt with kbmatrix%()
 n& = VARSEG(kbmatrix%(0)): l& = n& AND 255: h& = ((n& AND &HFF00) \ 256): POKE i&, l&: POKE i& + 1, h&: i& = i& + 2
 n& = VARPTR(kbmatrix%(0)): l& = n& AND 255: h& = ((n& AND &HFF00) \ 256): POKE i&, l&: POKE i& + 1, h&: i& = i& + 2
 DEF SEG
 Firsttime = 1
END IF

SELECT CASE T
 CASE -1
  IF StatusFlag = 0 THEN
   DEF SEG = VARSEG(kbcontrol%(0))
   CALL ABSOLUTE(0)                     ' Run interrupt
   DEF SEG
   StatusFlag = 1
  END IF
 CASE -2
  IF StatusFlag = 1 THEN
   DEF SEG = VARSEG(kbcontrol%(0))      ' Turn off interrupt
   CALL ABSOLUTE(3)
   DEF SEG
   StatusFlag = 0
  END IF
 CASE 1 TO 128
  MULTIKEY = kbmatrix%(T)               ' Return status
 CASE ELSE
  MULTIKEY = 0                          ' User Supidity Error
END SELECT

code$ = ""

END FUNCTION

SUB Qsort (SortArray() AS PolyType, Lower%, Upper%) STATIC

  'QuickSort iterative (rather than recursive) by Cornel Huth
  DIM Stack(1 TO 256, 1)   AS INTEGER   'Low=0,Hi=1
  DIM Sp AS INTEGER                   'out stack pointer
  Sp = 1
  'maxsp = sp
  Stack(Sp, 0) = Lower%
  Stack(Sp, 1) = Upper%
  Sp = Sp + 1
  DO
    Sp = Sp - 1
    Low = Stack(Sp, 0)
    Hi = Stack(Sp, 1)
    DO
      i = Low
      j = Hi
      mid = (Low + Hi) \ 2
      Compare = SortArray(mid).zcenter
      DO
        DO WHILE SortArray(i).zcenter > Compare
          i = i + 1
        LOOP
        DO WHILE SortArray(j).zcenter < Compare
          j = j - 1
        LOOP
        IF i <= j THEN
          SWAP SortArray(i).zcenter, SortArray(j).zcenter
          SWAP SortArray(i).idx, SortArray(j).idx
          i = i + 1
          j = j - 1
        END IF
      LOOP WHILE i <= j
      IF j - Low < Hi - i THEN
        IF i < Hi THEN

          Stack(Sp, 0) = i
          Stack(Sp, 1) = Hi
          Sp = Sp + 1
        END IF
        Hi = j
      ELSE
        IF Low < j THEN
          Stack(Sp, 0) = Low
          Stack(Sp, 1) = j
          Sp = Sp + 1
        END IF
        Low = i
      END IF
    LOOP WHILE Low < Hi
  LOOP WHILE Sp <> 1

END SUB

SUB Rel.LoadBMP (File$, SwitchPal%) STATIC
'Loads a BMP to a layer or directly to the screen
'if you want it to be on screen pass &HA000 as DestSeg%
'only supports 256 color BMPs.
DIM BMP AS BMPHeaderType

F% = FREEFILE                   'Get free filenum
OPEN File$ FOR BINARY AS #F%    'Binary read
GET #F%, , BMP                   'Get header

'Our File Pointer points to 55 byte seek 55, first byte
'Pal should be 1024 in length
Pall$ = BMP.Pal
IF SwitchPal% THEN              'if we switch to pal then
        IF LEN(Pall$) = 1024 THEN
                OUT &H3C8, 0              'color zero start
                FOR i% = 1 TO 1024 STEP 4
                        B% = ASC(MID$(Pall$, i%, 1)) \ 4     'div by 4
                        g% = ASC(MID$(Pall$, i% + 1, 1)) \ 4 'div by 4
                        R% = ASC(MID$(Pall$, i% + 2, 1)) \ 4 'div by 4
                        'Byte 4 unused. Just for padding 32 bit regs
                        OUT &H3C9, R%
                        OUT &H3C9, g%
                        OUT &H3C9, B%
                NEXT i%
        END IF
END IF

'Read and Write time!!!
'Notes: Bad MS(I don't get it why they stored it backwards?)

Byte$ = SPACE$(BMP.WID)
DEF SEG = DestSeg%

Wide% = BMP.WID - 1             'Sub 1 since we start at zero
Hite% = BMP.HEI - 1
FOR y% = Hite% TO 0 STEP -1
        GET #F%, , Byte$
FOR x% = 0 TO Wide%
        c% = ASC(MID$(Byte$, x% + 1, 1))
        PSET (x%, y%), c%
NEXT x%
NEXT y%

END SUB

SUB ScaleModel (M() AS Point3d, Scale!)
FOR i = 1 TO UBOUND(M)
        M(i).x = M(i).x * Scale!
        M(i).y = M(i).y * Scale!
        M(i).z = M(i).z * Scale!
NEXT i

END SUB

SUB SortPolys (Model() AS Point3d, Poly() AS PolyType)

 FOR i% = 1 TO MAXPOLY
  Poly(i%).zcenter = Model(Poly(i%).p1).zr + Model(Poly(i%).p2).zr + Model(Poly(i%).p3).zr
  Poly(i%).idx = i%
 NEXT i%

 Qsort Poly(), 1, MAXPOLY
END SUB

SUB TransFormPoints (Model() AS Point3d, M!()) STATIC


FOR i = 1 TO UBOUND(Model)

        x! = Model(i).x
        y! = Model(i).y
        z! = Model(i).z

        Matrix.MulVector M!(), x!, y!, z!, rotx!, roty!, rotz!
        
        Model(i).xr = rotx!
        Model(i).yr = roty!
        Model(i).zr = rotz!

        'Project
        Distance% = rotz!
        IF Distance% > 0 THEN
            Model(i).scrx = XCENTER + (LENS * rotx! \ Distance%)
            Model(i).scry = YCENTER - (LENS * roty! \ Distance%)
        END IF

NEXT i

END SUB

SUB Vector.Cross (v AS Vector, V1 AS Vector, V2 AS Vector)
'gets the cross product between 2 vectors v1 and v2
'v is the return value

v.x = V1.y * V2.z - V2.y * V1.z
v.y = V1.z * V2.x - V2.z * V1.x
v.z = V1.x * V2.y - V2.x * V1.y

END SUB

FUNCTION Vector.Dot! (V1 AS Vector, V2 AS Vector)
    Vector.Dot! = V1.x * V2.x + V1.y * V2.y + V1.z * V2.z
END FUNCTION

SUB Vector.Normalize (v AS Vector)
'Makes v a unit vector

mag! = SQR(v.x * v.x + v.y * v.y + v.z * v.z)

IF mag! <> 0 THEN
    v.x = v.x / mag!
    v.y = v.y / mag!
    v.z = v.z / mag!
ELSE
    v.x = 0
    v.y = 0
    v.z = 1
END IF

END SUB

