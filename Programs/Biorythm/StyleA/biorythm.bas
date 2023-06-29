DECLARE SUB Scroll (s%, e%)
DECLARE SUB Bufferize ()
DECLARE SUB FormatDate (b$)
DECLARE SUB Display (i$, t%, f%, b%)
DECLARE SUB Calculate (b$, t$, n)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
    CONST BGC = 1, BOOT = "biorythm.bas", FGC = 15
    CONST MXL = 77, PI = 3.14, PRGM = "biorythm", WSZ = 11
    COMMON SHARED Flt%
    COMMON SHARED Today$
    DIM SHARED Cycle(7) AS STRING, Month(12) AS INTEGER
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA 31,28,31,30,31,30,31,31,30,31,30,31
DATA "23Physical"
DATA "33Intellectual"
DATA "28Emotional"
DATA "53Spiritual"
DATA "43Aesthetics"
DATA "48Awareness"
DATA "38Intuitiveness"

SUB Bufferize
    DIM b$, c$, i$, n, t$, v%, x%
    CLOSE
    OPEN PRGM + ".txt" FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    LINE INPUT #1, b$
    Calculate b$, t$, n
    i$ = b$ + "³" + t$ + "³"
    FOR x% = 1 TO 7
    v% = INT(SIN(2 * PI * n / VAL(Cycle(x%))) * 100) + 100
    SELECT CASE v%
        CASE IS < 1: c$ = "A"
        CASE 1 TO 44: c$ = "B"
        CASE 45 TO 55: c$ = "C"
        CASE 56 TO 99: c$ = "D"
        CASE 100: c$ = "E"
        CASE 101 TO 144: c$ = "F"
        CASE 145 TO 155: c$ = "G"
        CASE 156 TO 199: c$ = "H"
        CASE IS > 199: c$ = "I"
        CASE ELSE: c$ = "?"
    END SELECT
    i$ = i$ + STRING$(8, c$)
    IF x% < 7 THEN i$ = i$ + "³"
    NEXT
    PRINT #2, LEFT$(i$ + STRING$(MXL, 46), MXL)
    WEND
END SUB

SUB Calculate (b$, t$, n)
    DIM d%, i$, j$, m%, o$, x%, y%
    n = 0
    o$ = CHR$(32)
    t$ = Today$
    FOR x% = 1 TO LEN(b$)
    IF MID$(b$, x%, 1) = "," THEN
        t$ = RIGHT$(b$, x% + 1)
        b$ = LEFT$(b$, x% - 1)
        o$ = CHR$(249)
        EXIT FOR
    END IF
    NEXT
    FormatDate b$
    FormatDate t$
    DO
    FOR y% = VAL(RIGHT$(b$, 4)) TO VAL(RIGHT$(t$, 4)) + 100
    SELECT CASE y%
        CASE 1600, 2000, 2400, 2800: Month(2) = 29
        CASE ELSE
            SELECT CASE VAL(RIGHT$(STR$(y%), 2))
                CASE 4, 8, 12, 16, 20, 24, 28, 32, 36, 40: Month(2) = 29
                CASE 44, 48, 52, 56, 60, 64, 68, 72, 76: Month(2) = 29
                CASE 80, 84, 88, 92, 96: Month(2) = 29
                CASE ELSE: Month(2) = 28
            END SELECT
    END SELECT
    FOR m% = 1 TO 12
    FOR d% = 1 TO Month(m%)
    i$ = RIGHT$("00" + MID$(STR$(m%), 2), 2)
    i$ = i$ + RIGHT$("00" + MID$(STR$(d%), 2), 2)
    i$ = i$ + RIGHT$("0000" + MID$(STR$(y%), 2), 4)
    IF i$ = b$ THEN j$ = "+"
    IF j$ = "+" THEN n = n + 1
    IF i$ = t$ THEN EXIT DO
    NEXT
    NEXT
    NEXT
    EXIT DO
    LOOP
    b$ = o$ + LEFT$(b$, 4) + RIGHT$(b$, 2)
    t$ = LEFT$(t$, 4) + RIGHT$(t$, 2)
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Display (i$, t%, f%, b%)

END SUB

SUB FormatDate (b$)
    DIM i$, x%
    FOR x% = 1 TO LEN(b$)
    SELECT CASE MID$(b$, x%, 1)
        CASE "0" TO "9": i$ = i$ + MID$(b$, x%, 1)
    END SELECT
    NEXT
    b$ = RIGHT$(STRING$(8, "0") + i$, 8)
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
    DIM i$, x%
    Flt% = 0
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(LEFT$(i$, x%))
        END SELECT
        NEXT
        WEND
    END IF
    CLOSE
    FOR x% = 1 TO 12: READ Month(x%): NEXT
    FOR x% = 1 TO 7: READ Cycle(x%): NEXT
END SUB

SUB Main
    DIM a%, e%, k$, s%
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    s% = 1
    e% = LOF(1) / (MXL + 2)
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey a%, k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM i$, x%, y%
    CLS
    COLOR FGC, BGC
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3
            PRINT "Ç"; STRING$(7, 196); "Â"; STRING$(6, 196); "Â";
            FOR y% = 1 TO 6
            PRINT STRING$(8, 196); "Â";
            NEXT
            PRINT STRING$(9, 196); "¶"
        CASE 5
            PRINT "Ç"; STRING$(7, 196); "Å"; STRING$(6, 196); "Å";
            FOR y% = 1 TO 6
            PRINT STRING$(8, 196); "Å";
            NEXT
            PRINT STRING$(9, 196); "¶"
        CASE WSZ + 6
            PRINT "Ç"; STRING$(7, 196); "Á"; STRING$(6, 196); "Á";
            FOR y% = 1 TO 6
            PRINT STRING$(8, 196); "Á";
            NEXT
            PRINT STRING$(9, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE
            PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt "Biorythm Indicators ù Today is " + Today$, 2, 11, BGC
    i$ = "DOB's ³EnDate"
    FOR x% = 1 TO 7
    i$ = i$ + "³" + MID$(Cycle(x%), 3, 8)
    NEXT
    SayIt i$, 4, FGC, BGC
    i$ = "(" + CHR$(24) + ")Positive Phase Crest, (" + CHR$(26) + ")Start Cycle"
    SayIt i$, WSZ + 7, 10, BGC
    i$ = "Positive Phase: (" + CHR$(24) + ")Cycle Increasing, (" + CHR$(25) + ")Cycle Decreasing, (" + CHR$(18) + ")Mid Cycle"
    SayIt i$, WSZ + 8, 11, BGC
    i$ = "Negative Phase: (" + CHR$(25) + ")Cycle Increasing, (" + CHR$(24) + ")Cycle Decreasing, (" + CHR$(27) + ")End Cycle"
    SayIt i$, WSZ + 9, 14, BGC
    i$ = "(" + CHR$(25) + ")Negative Phase Trough"
    SayIt i$, WSZ + 10, 13, BGC
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù TAB to Edit ù ESC to End ù", 22, FGC, BGC
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + l$, 38)
    r$ = LEFT$(r$ + STRING$(38, 32), 38)
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$; r$
    COLOR FGC, BGC
END SUB

SUB Scroll (s%, e%)
    DIM b%, d AS STRING * MXL, f%, i$, j$, o$, x%, y%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, d
        CASE e% + 1
            i$ = STRING$(7, 196) + "Å" + STRING$(6, 196) + "Å"
            FOR y% = 1 TO 6
            i$ = i$ + STRING$(8, 196) + "Å"
            NEXT
            i$ = i$ + STRING$(9, 196)
            d = i$
        CASE ELSE
            i$ = STRING$(7, 32) + "³" + STRING$(6, 32) + "³"
            FOR y% = 1 TO 6
            i$ = i$ + STRING$(8, 32) + "³"
            NEXT
            i$ = i$ + STRING$(9, 32)
            d = i$
    END SELECT
    i$ = LEFT$(d, MXL)
    FOR y% = 1 TO LEN(i$)
    SELECT CASE UCASE$(MID$(i$, y%, 1))
        CASE "0" TO "9": f% = FGC: b% = BGC: j$ = MID$(i$, y%, 1)
        CASE "A": f% = 10: b% = BGC: j$ = CHR$(26)
        CASE "B": f% = 11: b% = BGC: j$ = CHR$(24)
        CASE "C": f% = 10: b% = BGC: j$ = CHR$(24)
        CASE "D": f% = 11: b% = BGC: j$ = CHR$(25)
        CASE "E": f% = 14: b% = BGC: j$ = CHR$(18)
        CASE "F": f% = 14: b% = BGC: j$ = CHR$(25)
        CASE "G": f% = 13: b% = BGC: j$ = CHR$(25)
        CASE "H": f% = 14: b% = BGC: j$ = CHR$(24)
        CASE "I": f% = 11: b% = BGC: j$ = CHR$(27)
        CASE ELSE: f% = FGC: b% = BGC: j$ = MID$(i$, y%, 1)
    END SELECT
    LOCATE x% + 5, y% + 1
    COLOR f%, b%
    PRINT j$
    NEXT
    PRINT
    NEXT
    COLOR FGC, BGC
END SUB

