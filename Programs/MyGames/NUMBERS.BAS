DECLARE SUB Cuckoo ()
DECLARE SUB Initialize ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%)
DECLARE SUB Main ()
DECLARE SUB Getkey (a%, k$)
    CONST MXL = 40, MXN = 4
    COMMON SHARED Blk$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 1
    SOUND 1200, 4
    SOUND 0, 1
END SUB

SUB Generate (n$, g$)
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Initialize
    Blk$ = STRING$(MXN, 45)
END SUB

SUB Main
    SCREEN 1
    DIM a%, d$, g$, k$, n$, r$, s$, x%, y%, z%
    s$ = "0"
    z% = 5
    DO
    COLOR 9
    Mask
    RANDOMIZE TIMER
    r$ = RIGHT$(STRING$(MXN, "0") + MID$(STR$(INT(RND(1) * 9999) + 1), 2), 4)
    g$ = ""
    SayIt "Guess the Random Number by", z%
    SayIt "Pressing Numbers 0 to 9 to Form", z% + 2
    SayIt "a" + STR$(MXN) + " Digit Number String.", z% + 4
    DO
    SELECT CASE LEFT$(g$, 1)
        CASE "0" TO "9"
            SayIt RIGHT$(Blk$ + g$, MXN), z% + 6
            SELECT CASE VAL(g$)
                CASE IS > VAL(r$): SayIt "The Random Number is Lower!", z% + 8
                CASE IS = VAL(r$)
                    COLOR 2
                    SayIt CHR$(2) + " " + RIGHT$(Blk$ + g$, MXN) + " " + CHR$(2), z% + 6
                    SayIt "is the Correct Random Number!", z% + 8
                    s$ = STR$(VAL(s$) + 1)
                CASE IS < VAL(r$): SayIt "The Random Number is Higher!", z% + 8
            END SELECT
        CASE ELSE
            SayIt STRING$(MXN, 63), z% + 6
            SayIt STRING$(MXN, 45), z% + 8
    END SELECT
    REDIM ar(9) AS INTEGER
    n$ = "0"
    d$ = "0"
    FOR x% = 1 TO MXN
    FOR y% = 1 TO MXN
    IF MID$(r$, x%, 1) = MID$(g$, y%, 1) THEN
        IF x% = y% THEN d$ = MID$(STR$(VAL(d$) + 1), 2)
        IF ar(y%) = 0 THEN
            n$ = MID$(STR$(VAL(n$) + 1), 2)
            ar(y%) = 1
        END IF
    END IF
    NEXT
    NEXT
    SayIt "Numbers Correct: " + n$ + ".", z% + 10
    SayIt "Digits  Correct: " + d$ + ".", z% + 12
    SayIt "Score: " + s$ + ".", z% + 14
    Getkey a%, k$
    SELECT CASE k$
        CASE CHR$(13): EXIT DO
        CASE "="
            s$ = STR$(VAL(s$) - 1)
            g$ = r$
        CASE "?", "/"
            COLOR 4
            Mask
            s$ = STR$(VAL(s$) - 1)
            SayIt "I'm Sorry,", z% + 3
            SayIt "the Random Number Was " + r$ + "!", z% + 5
            SayIt "You Lose 1 Point.", z% + 7
            SayIt "Your Score is Now" + s$ + ".", z% + 9
            SayIt "ENTER a New Game!", z% + 11
            Cuckoo
            DO
            Getkey a%, k$
            LOOP UNTIL a% = 13
            EXIT DO
        CASE "0" TO "9"
            g$ = RIGHT$(g$ + k$, MXN)
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
    LOCATE 1, 1
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(MXL - 2, 205); "»"
        CASE 3, 21: PRINT "Ç"; STRING$(MXL - 2, 196); "¶"
        CASE 23: PRINT "È"; STRING$(MXL - 2, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(MXL - 2, 32); "º"
    END SELECT
    NEXT
    SayIt "DAHSR's " + CHR$(34) + "Number Game" + CHR$(34) + " ù " + RIGHT$(DATE$, 4), 2
    SayIt "ENTER a New Game or ESC to End", 22
END SUB

SUB SayIt (i$, v%)
    DIM h%, l$, r$, w%, x%
    w% = MXL / 2 - 1
    h% = 2
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    LOCATE v%, h%
    PRINT l$; r$
END SUB

