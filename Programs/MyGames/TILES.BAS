DECLARE SUB SolvePuzzle ()
DECLARE SUB Click ()
DECLARE SUB TileUp (Ver%, Hor%)
DECLARE SUB TileDown (Ver%, Hor%)
DECLARE SUB TileLeft (Ver%, Hor%)
DECLARE SUB TileRight (Ver%, Hor%)
DECLARE SUB CheckForDone (Solved%)
DECLARE SUB GameOver ()
DECLARE SUB Main ()
DECLARE SUB DrawBlocks ()
DECLARE SUB DrawTop ()
DECLARE SUB DrawSpace ()
DECLARE SUB DrawSection ()
DECLARE SUB DrawBottom ()
DECLARE SUB Getkey (Ak%, Ck$)
DECLARE SUB Terminate ()
DECLARE SUB IniCubes ()

    CONST Blank = "  "
    CONST Blocks = 4
    CONST Horz = 14
    CONST Size = 2
    CONST Vert = 6
    COMMON SHARED Horray$
   
    DIM SHARED Cube(Blocks, Blocks) AS STRING

    Main
END

SUB Cheat
    n% = 0
    x% = INT(RND(1) * 4) + 1
    y% = INT(RND(1) * 4) + 1

    Cube(x%, y%) = ""

    FOR x% = 1 TO 4
    FOR y% = 1 TO 4
    DO
    IF Cube(x%, y%) = "" THEN Cube(x%, y%) = Blank: EXIT DO
    n% = n% + 1
    Cube(x%, y%) = RIGHT$("00" + MID$(STR$(n%), 2), 2)
    EXIT DO
    LOOP
    NEXT
    NEXT

END SUB

SUB CheckForDone (Solved%)
    Solved% = 0
    n% = 0
    
    FOR v% = 1 TO 4
    FOR h% = 1 TO 4
    DO
    IF Cube(v%, h%) = Blank THEN EXIT DO
    n% = n% + 1
    IF VAL(Cube(v%, h%)) <> n% THEN EXIT SUB
    EXIT DO
    LOOP
    NEXT
    NEXT

    Solved% = 1

    GameOver

    DO
    Getkey a%, k$
    LOOP UNTIL a% = 13
   
END SUB

SUB Click
    SOUND 4000, .1
END SUB

SUB DrawBlocks
    Title$ = "DAHSR's TileGame"
    LOCATE 1, 20 - (LEN(Title$) / 2): PRINT Title$
    Prompt$ = "Use the ARROW KEYS to"
    LOCATE 3, 20 - (LEN(Prompt$) / 2): PRINT Prompt$
    Prompt$ = "Manipulate the Tiles!"
    LOCATE , 20 - (LEN(Prompt$) / 2): PRINT Prompt$

    DrawTop
    FOR x% = 1 TO Blocks - 1
    DrawSpace
    DrawSection
    NEXT
    DrawSpace
    DrawBottom

    Prompt$ = "Press ENTER to Renew!"
    LOCATE 16, 20 - (LEN(Prompt$) / 2): PRINT Prompt$
    Prompt$ = "Press ESC to Quit!"
    LOCATE , 20 - (LEN(Prompt$) / 2): PRINT Prompt$
    Prompt$ = "Press ? to Solve"
    LOCATE , 20 - (LEN(Prompt$) / 2): PRINT Prompt$
       
END SUB

SUB DrawBottom
    LOCATE , Horz
    PRINT "È"; STRING$(Size, 205);
    FOR y% = 1 TO Blocks - 1
    PRINT "Ê"; STRING$(Size, 205);
    NEXT
    PRINT "¼"

END SUB

SUB DrawSection
    LOCATE , Horz
    PRINT "Ì"; STRING$(Size, 205);
    FOR y% = 1 TO Blocks - 1
    PRINT "Î"; STRING$(Size, 205);
    NEXT
    PRINT "¹"
END SUB

SUB DrawSpace
    LOCATE , Horz
    PRINT "º"; STRING$(Size, 32);
    FOR y% = 1 TO Blocks - 1
    PRINT "º"; STRING$(Size, 32);
    NEXT
    PRINT "º"
END SUB

SUB DrawTop
    LOCATE Vert, Horz
    PRINT "É"; STRING$(Size, 205);
    FOR y% = 1 TO Blocks - 1
    PRINT "Ë"; STRING$(Size, 205);
    NEXT
    PRINT "»"
END SUB

SUB GameOver
    Prompt$ = "Congradulations,"
    LOCATE 20, 20 - (LEN(Prompt$) / 2)
    PRINT Prompt$

    Prompt$ = "You've solved The puzzle!"
    LOCATE , 20 - (LEN(Prompt$) / 2)
    PRINT Prompt$
   
    Prompt$ = "Press ENTER to Rewew!"
    LOCATE , 20 - (LEN(Prompt$) / 2)
    PRINT Prompt$

    SOUND 680, 2
    
    SOUND 750, 2
    
    SOUND 850, 2
    
    SOUND 1030, 5
    
    SOUND 680, 2
    
    SOUND 1030, 5

END SUB

SUB Getkey (Ak%, Ck$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    Ck$ = INKEY$
    LOOP WHILE Ck$ = ""
    Ak% = ASC(Ck$)
    IF Ak% = 27 THEN Terminate
END SUB

SUB IniCubes
    FOR x% = 1 TO 4
    FOR y% = 1 TO 4
    Cube(x%, y%) = Blank
    NEXT
    NEXT
    
    RANDOMIZE TIMER

    FOR w% = 1 TO 15
    n$ = RIGHT$("00" + MID$(STR$(w%), 2), 2)
    DO
    x% = INT(RND(1) * 4) + 1
    y% = INT(RND(1) * 4) + 1
    IF Cube(x%, y%) = Blank THEN Cube(x%, y%) = n$: EXIT DO
    LOOP
    NEXT

    'Cheat
END SUB

SUB Initialize
    Horry$ = "abcdcd"
END SUB

SUB Main
    DO

    SCREEN 1
    COLOR 5
    CLS

    DO

    IniCubes

    DrawBlocks

    DO

    FOR v% = 1 TO Blocks
    FOR h% = 1 TO Blocks
    LOCATE Vert + v% * (Blocks - 2) - 1, Horz + h% * (Blocks - 1) - 2
    PRINT Cube(v%, h%)
    IF Cube(v%, h%) = Blank THEN Ver% = v%: Hor% = h%
    NEXT
    NEXT

    CheckForDone Solved%

    IF Solved% = 1 THEN EXIT DO
   
    Getkey a%, k$

    SELECT CASE k$
        CASE IS = "?": SolvePuzzle
        CASE IS = CHR$(0) + "H": TileUp Ver%, Hor%: Click
        CASE IS = CHR$(0) + "P": TileDown Ver%, Hor%: Click
        CASE IS = CHR$(0) + "K": TileLeft Ver%, Hor%: Click
        CASE IS = CHR$(0) + "M": TileRight Ver%, Hor%: Click
        CASE IS = CHR$(0) + "S": EXIT DO
    END SELECT

    LOOP UNTIL a% = 13

    LOOP UNTIL a% = 13 OR Solved = Yes

    LOOP

END SUB

SUB SolvePuzzle
    FOR x% = 1 TO Blocks
    FOR y% = 1 TO Blocks
    Cube(x%, y%) = ""
    NEXT
    NEXT

    Cube(4, 4) = Blank

    FOR x% = 1 TO Blocks
    FOR y% = 1 TO Blocks
    IF Cube(x%, y%) = "" THEN
        n% = n% + 1
        Cube(x%, y%) = RIGHT$("00" + MID$(STR$(n%), 2), 2)
    END IF
    NEXT
    NEXT
END SUB

SUB Terminate
END
END SUB

SUB TileDown (Ver%, Hor%)

    SELECT CASE Ver%
        CASE IS > 1: SWAP Cube(Ver%, Hor%), Cube(Ver% - 1, Hor%)
    END SELECT

END SUB

SUB TileLeft (Ver%, Hor%)
    SELECT CASE Hor%
        CASE IS < Blocks: SWAP Cube(Ver%, Hor%), Cube(Ver%, Hor% + 1)
    END SELECT
END SUB

SUB TileRight (Ver%, Hor%)
    SELECT CASE Hor%
        CASE IS > 1: SWAP Cube(Ver%, Hor%), Cube(Ver%, Hor% - 1)
    END SELECT
END SUB

SUB TileUp (Ver%, Hor%)
   
    SELECT CASE Ver%
        CASE IS < Blocks: SWAP Cube(Ver%, Hor%), Cube(Ver% + 1, Hor%)
    END SELECT

END SUB

