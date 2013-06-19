'Code supplement for the vector chapter.
'Felix the Helix
'By Jelly(Relsoft)

'''Relsoft 2004
'''Rel.Betterwebber.com


1 IF I% = 0 THEN SCREEN 7, , 0, 1 ELSE PCOPY 0, 1
2 A! = (A! + .03) * -(A! <= (6.283186 - .03))
3 FOR I% = 0 TO 200
4 x! = ((50 * COS(4 * 3.141593 / 200 * I%)) * (COS(A!) * COS(A!)) + (50 * SIN(8 * 3.141593 / 200 * I%)) * (SIN(A!) * SIN(A!) * COS(A!) - COS(A!) * SIN(A!)) + (I% - (200 / 2)) * (COS(A!) * SIN(A!) * COS(A!) + SIN(A!) * SIN(A!)))
5 y! = ((50 * COS(8 * 3.141593 / 200 * I%)) * (COS(A!) * SIN(A!)) + (50 * SIN(8 * 3.141593 / 200 * I%)) * (COS(A!) * COS(A!) + SIN(A!) * SIN(A!) * SIN(A!)) + (I% - (200 / 2)) * (-SIN(A!) * COS(A!) + COS(A!) * SIN(A!) * SIN(A!)))
6 z! = ((50 * COS(8 * 3.141593 / 200 * I%)) * (-SIN(A!)) + (50 * SIN(8 * 3.141593 / 200 * I%)) * (SIN(A!) * COS(A!)) + (I% - (200 / 2)) * COS(A!) * COS(A!))
7 IF I% > 1 THEN CIRCLE ((160 + (256 * x! \ (256 - z!))), (100 - (256 * y! \ (256 - z!)))), 512 \ (256 - z!), (A! * 57.29577 + I%) AND 15 ELSE LINE (0, 0)-(319, 199), 0, BF
8 NEXT I%
9 IF INKEY$ = "" THEN 1

