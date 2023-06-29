DECLARE SUB SetClocks ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Scroll (s%)
DECLARE SUB Getkey (a%, k$, s%)
DECLARE SUB Initialize ()
DECLARE SUB Main ()
DECLARE SUB Mask ()
    CONST BOOT = "wtclocks.bas", ZONE = 7, NOZ = 25
    CONST PRGM = "tzlabels", MXL = 76, WSZ = 17
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    DIM SHARED Tz(1, NOZ) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = 0: RESUME NEXT

SUB Getkey (a%, k$, s%)
    DIM x%
    DO: LOOP UNTIL INKEY$ = ""
    x% = VAL(MID$(TIME$, 4, 2))
    DO
    k$ = INKEY$
    IF x% <> VAL(MID$(TIME$, 4, 2)) THEN Scroll s%
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Initialize
    DIM i$, x%, y%
    Bgc% = 1
    Dbse$ = PRGM + ".txt"
    Fgc% = 15
    Flt% = 0
    CLOSE
    OPEN Dbse$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        FOR x% = 1 TO NOZ
        IF Flt% = 0 THEN
            LINE INPUT #1, i$
            FOR y% = 1 TO LEN(i$)
            IF MID$(i$, y%, 1) = "," AND MID$(i$, y% + 1, 1) <> " " THEN i$ = LEFT$(i$, y%) + " " + MID$(i$, y% + 1)
            NEXT
            Tz(1, x%) = LEFT$(i$ + STRING$(MXL, 46), MXL)
        ELSE
            Tz(1, x%) = STRING$(MXL, ".")
        END IF
        NEXT
    END IF
    CLOSE
END SUB

SUB Main
    DIM a%, k$
    Mask
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > NOZ - WSZ + 1 THEN s% = NOZ - WSZ + 1
    Scroll s%
    DO
    Getkey a%, k$, s%
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "I": s% = 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "O": s% = NOZ: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = NOZ: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
    CLS
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, 21: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt "World Time Clocks", 2, 11, Bgc%
    SayIt "Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù ESC to End", 22, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$, s$, w%
    w% = 38
    s$ = STRING$(w%, 32)
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(s$ + l$, w%)
    r$ = LEFT$(r$ + s$, w%)
    LOCATE v%, 3
    COLOR f%, b%
    PRINT l$; r$
END SUB

SUB Scroll (s%)
    SetClocks
    DIM b%, f%, i$, x%, z$
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO NOZ: i$ = LEFT$(Tz(0, s% + x% - 1) + "|" + Tz(1, s% + x% - 1), MXL)
        CASE NOZ + 1: i$ = STRING$(MXL, 196)
        CASE ELSE: i$ = STRING$(MXL, 32)
    END SELECT
    SELECT CASE LCASE$(MID$(i$, 6, 2))
        CASE "nn": f% = 0: b% = 15
        CASE "mm": f% = 15: b% = 0
        CASE ELSE
            IF s% + x% - 1 = 13 THEN
                f% = 15: b% = 11
            ELSE
                f% = Fgc%: b% = Bgc%
            END IF
    END SELECT
    IF s% + x% - 2 = ZONE THEN f% = 15: b% = 13
    SayIt i$, x% + 3, f%, b%
    NEXT
END SUB

SUB SetClocks
    DIM h$, t%, o$, x%
    t% = VAL(LEFT$(TIME$, 2))
    FOR x% = 1 TO ZONE
    t% = t% - 1
    IF t% < 0 THEN t% = 23
    NEXT
    FOR x% = 1 TO NOZ
    IF t% > 23 THEN t% = 0
    SELECT CASE t%
        CASE 0: h$ = "00": o$ = "mm"
        CASE 1 TO 11: h$ = RIGHT$("00" + MID$(STR$(t%), 2), 2): o$ = "am"
        CASE 12: h$ = RIGHT$("00" + MID$(STR$(t%), 2), 2): o$ = "nn"
        CASE ELSE: h$ = RIGHT$("00" + MID$(STR$(t% - 12), 2), 2): o$ = "pm"
    END SELECT
    i$ = h$ + ":" + MID$(TIME$, 4, 2) + o$
    Tz(0, x%) = i$
    t% = t% + 1
    NEXT
END SUB

