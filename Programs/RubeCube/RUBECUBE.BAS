DECLARE SUB Initialize ()
DECLARE SUB VScan (w%)
DECLARE SUB HScan (w%)
DECLARE SUB Scan ()
DECLARE SUB Award ()
DECLARE SUB Test (k$)
DECLARE SUB Cuckoo ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
DECLARE SUB Mask ()
DECLARE SUB MoveLeft (r%)
DECLARE SUB MoveRight (r%)
DECLARE SUB MoveUp (c%)
DECLARE SUB MoveDown (c%)
DECLARE SUB Mix ()
DECLARE SUB Display (c%, r%)
DECLARE SUB Say (i$, v%, h%, c%)
    CONST Size = 80, Border = Size * .025
    CONST Cell = 3, Edge = 310 - ((Size * Cell) / 2)
    CONST Top = 180 - ((Size * Cell) / 2)
    COMMON SHARED Flt%, Prompt$, Title$, Txt%
    DIM SHARED Cube(Cell, Cell) AS INTEGER
    Initialize
    DO
    Main
    LOOP
END
Trap: Flt% = 1: RESUME NEXT

SUB Award
    DIM a%, k$
    Display 2, 2
    PLAY "O3L16CEA O4L4C O3L16A O4L4C"
    Say "You", 12, 20, 14
    Say "Win!", 12, 60, 14
    Say Prompt$, 27, 0, 13
    DO
    Getkey a%, k$
    LOOP UNTIL a% = 13
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1200, 4
    SOUND 0, 2
END SUB

SUB Display (c%, r%)
    DIM a%, b%, d%, e%, s%, t%, x%, y%
'Draw Cubes:
    FOR y% = 1 TO Cell
    FOR x% = 1 TO Cell
    a% = Edge + (Size * x% - Size + Border)
    b% = Top + (Size * y% - Size + Border)
    d% = a% + Size - Border
    e% = b% + Size - Border
    LINE (a%, b%)-(d%, e%), Cube(x%, y%), BF
    NEXT
    NEXT
'Draw Smiley Face:
    'Head.
    FOR x% = 1 TO (Size * .2)
    CIRCLE (Edge + c% * Size - (Size * .5), Top + r% * Size - (Size * .5)), x%, 14
    NEXT
    FOR x% = 1 TO (Size * .03)
    'Left Eye.
    CIRCLE (Edge + c% * Size - (Size * .58), Top + r% * Size - (Size * .58)), x%, 0
    'Right Eye.
    CIRCLE (Edge + c% * Size - (Size * .42), Top + r% * Size - (Size * .58)), x%, 0
    'Nose.
    CIRCLE (Edge + c% * Size - (Size * .5), Top + r% * Size - (Size * .5)), x%, 0
    NEXT
    FOR x% = (Size * .04) TO (Size * .08)
    'Mouth.
    CIRCLE (Edge + c% * Size - (Size * .5), Top + r% * Size - (Size * .43)), x%, 0, 3.4, 6
    NEXT
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN SCREEN 0: END
END SUB

SUB HScan (w%)
    w% = 0
    DIM c%, r%
    FOR r% = 1 TO Cell
    FOR c% = 1 TO Cell - 1
    IF Cube(c%, r%) <> Cube(c% + 1, r%) THEN EXIT SUB
    NEXT
    NEXT
    w% = 1
    Award
END SUB

SUB Initialize
    Txt% = 15
    Title$ = "The Rube's Cube of " + RIGHT$(DATE$, 4)
    Prompt$ = "ENTER a New Puzzle or ESC to end."
END SUB

SUB Main
    DIM a%, c%, k$, r%, w%
    DO
    c% = 2
    r% = 2
    Mask
    DO
    IF c% > Cell THEN c% = 1: r% = r% + 1: IF r% > Cell THEN r% = 1
    IF c% < 1 THEN c% = Cell: r% = r% - 1: IF r% < 1 THEN r% = Cell
    Display c%, r%
    HScan w%: IF w% = 1 THEN EXIT DO
    VScan w%: IF w% = 1 THEN EXIT DO
    DO
    Getkey a%, k$
    SELECT CASE k$
        CASE IS = CHR$(13): Mix: c% = 2: r% = 2: EXIT DO
        CASE IS = CHR$(0) + "R": Mix: c% = 2: r% = 2: EXIT DO
        CASE IS = CHR$(0) + "S": Mix: c% = 2: r% = 2: EXIT DO
        'CASE "H", "h", "V", "v": Test k$: EXIT DO
        CASE CHR$(0) + "I": c% = c% - 1: EXIT DO
        CASE CHR$(0) + "Q": c% = c% + 1: EXIT DO
        CASE IS = CHR$(0) + "H": MoveUp c%: EXIT DO
        CASE IS = CHR$(0) + "P": MoveDown c%: EXIT DO
        CASE IS = CHR$(0) + "K": MoveLeft r%: EXIT DO
        CASE IS = CHR$(0) + "M": MoveRight r%: EXIT DO
        CASE IS = CHR$(0) + "G": c% = 2: r% = 2: EXIT DO
        CASE IS = CHR$(0) + "O": c% = Cell: r% = Cell: EXIT DO
        CASE IS = "0": c% = 2: r% = 2: EXIT DO
        CASE IS = "1": c% = 1: r% = 3: EXIT DO
        CASE IS = "2": c% = 2: r% = 3: EXIT DO
        CASE IS = "3": c% = 3: r% = 3: EXIT DO
        CASE IS = "4": c% = 1: r% = 2: EXIT DO
        CASE IS = "5": c% = 2: r% = 2: EXIT DO
        CASE IS = "6": c% = 3: r% = 2: EXIT DO
        CASE IS = "7": c% = 1: r% = 1: EXIT DO
        CASE IS = "8": c% = 2: r% = 1: EXIT DO
        CASE IS = "9": c% = 3: r% = 1: EXIT DO
    END SELECT
    Cuckoo
    LOOP
    LOOP
    LOOP
END SUB

SUB Mask
    DIM a%, b%, c%, d%, x%
    SCREEN 12
    CLS
    'Print the board text.
    Say Title$, 3, 0, 11
    x% = 21
    Say "Align Each Color in" + STR$(Cell) + " Separate Rows of Cubes, Horizontally or Vertically.", x%, 0, Txt%
    Say "Each Cube Position Corresponds to the Numberpad Keys.", x%, 0, Txt%
    Say "The Position of the Smiley Face Determines the Row or Column Moved.", x%, 0, Txt%
    Say "Press The Corresponding Keypad Number to Place the Smiley Face on a Cube.", x%, 0, Txt%
    Say "Move the Row of Cubes with the (" + CHR$(27) + CHR$(26) + ") Arrow Keys.", x%, 0, Txt%
    Say "Move the Column of Cubes with the (" + CHR$(24) + CHR$(25) + ") Arrow Keys.", x%, 0, Txt%
    Say Prompt$, 27, 0, Txt%
    'Draw the white board background.
    a% = Edge
    b% = Top
    c% = a% + (Size * Cell + Border)
    d% = b% + (Size * Cell + Border)
    LINE (a%, b%)-(c%, d%), Txt%, BF
    Mix
END SUB

SUB Mix
    DIM a%, b%, c%, x%, y%
    c% = 2
    'Initialize Cubes
    FOR a% = 1 TO Cell
    FOR b% = 1 TO Cell
    Cube(a%, b%) = 0
    NEXT
    NEXT
    'Assign Values to Cubes.
    FOR a% = 1 TO Cell
    FOR b% = 1 TO Cell
    DO
    RANDOMIZE TIMER
    x% = INT(RND(1) * Cell) + 1
    y% = INT(RND(1) * Cell) + 1
    IF Cube(x%, y%) = 0 AND Cube(x%, y%) <> c% THEN Cube(x%, y%) = c%: EXIT DO
    LOOP
    c% = c% + 1
    IF c% > 4 THEN c% = 2
    NEXT
    NEXT
END SUB

SUB MoveDown (c%)
    DIM x%
    Cube(c%, 0) = Cube(c%, Cell)
    FOR x% = Cell TO 2 STEP -1
    Cube(c%, x%) = Cube(c%, x% - 1)
    NEXT
    Cube(c%, 1) = Cube(c%, 0)
END SUB

SUB MoveLeft (r%)
    DIM x%
    Cube(0, r%) = Cube(1, r%)
    FOR x% = 1 TO Cell - 1
    Cube(x%, r%) = Cube(x% + 1, r%)
    NEXT
    Cube(Cell, r%) = Cube(0, r%)
END SUB

SUB MoveRight (r%)
    DIM x%
    Cube(0, r%) = Cube(Cell, r%)
    FOR x% = Cell TO 2 STEP -1
    Cube(x%, r%) = Cube(x% - 1, r%)
    NEXT
    Cube(1, r%) = Cube(0, r%)
END SUB

SUB MoveUp (c%)
    DIM x%
    Cube(c%, 0) = Cube(c%, 1)
    FOR x% = 1 TO Cell - 1
    Cube(c%, x%) = Cube(c%, x% + 1)
    NEXT
    Cube(c%, Cell) = Cube(c%, 0)
END SUB

SUB Say (i$, v%, h%, c%)
    IF h% = 0 THEN h% = 40
    h% = h% - (LEN(i$) / 2)
    COLOR c%
    LOCATE v%, h%
    PRINT i$
    COLOR Txt%
    v% = v% + 1
    IF v% > 27 THEN v% = 27
END SUB

SUB Test (k$)
    DIM c%, r%
    FOR c% = 1 TO Cell
    FOR r% = 1 TO Cell
    SELECT CASE k$
        CASE "H", "h": Cube(c%, r%) = r% + 1
        CASE "V", "v": Cube(r%, c%) = r% + 1
    END SELECT
    NEXT
    NEXT
END SUB

SUB VScan (w%)
    w% = 0
    DIM c%, r%
    FOR c% = 1 TO Cell
    FOR r% = 1 TO Cell - 1
    IF Cube(c%, r%) <> Cube(c%, r% + 1) THEN EXIT SUB
    NEXT
    NEXT
    w% = 1
    Award
END SUB

