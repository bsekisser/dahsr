DECLARE SUB Initialize ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Getkey (k$)
DECLARE SUB Calculate (a%, b%, g%, e%, f%)
DECLARE SUB Main ()
DECLARE SUB Mask ()
    CONST WSZ = 14
    COMMON SHARED Bgc%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    DIM SHARED Mth$(12)
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
Calendar:
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"
Presets:
DATA 6,21,180
DATA 6,22,180
DATA 6,23,180
DATA 6,24,180
DATA 6,25,180
DATA 12,20,270
DATA 12,21,270
DATA 12,22,270
DATA 12,23,270
DATA 12,24,270
DATA 12,25,270
DATA 12,26,270
DATA 0

SUB Calculate (a%, b%, g%, e%, f%)
    m% = a%
    d% = b%
    FOR x% = 1 TO g%
    SELECT CASE m%
        CASE 4, 6, 9, 11: y% = 30
        CASE 2: y% = 28
        CASE ELSE: y% = 31
    END SELECT
    d% = d% + 1
    IF d% > y% THEN d% = 1: m% = m% + 1
    IF m% > 12 THEN m% = 1
    NEXT
    e% = m%
    f% = d%
END SUB

SUB Getkey (k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF k$ = CHR$(27) THEN END
END SUB

SUB Initialize
    Bgc% = 1
    Fgc% = 15
    FOR x% = 1 TO 12
    READ Mth$(x%)
    NEXT
END SUB

SUB Main
    Mask
    m% = 12
    d% = 25
    g% = 270
    DO
    IF m% < 1 THEN m% = 1
    IF m% > 12 THEN m% = 12
    IF d% < 1 THEN d% = 1
    IF d% > 31 THEN d% = 31
    IF g% < 1 THEN g% = 1
    IF g% > 365 THEN g% = 365
    Calculate m%, d%, g%, a%, b%
    i$ = "Conception:" + LEFT$(MID$(STR$(d%), 2) + " " + Mth$(m%) + STRING$(40, 46), 16)
    i$ = i$ + "Gestation:" + RIGHT$("000" + MID$(STR$(g%), 2), 3) + " (Days)"
    i$ = i$ + RIGHT$(STRING$(40, 46) + "Birth:" + MID$(STR$(b%), 2) + " " + Mth$(a%), 29)
    SayIt i$, WSZ + 5, 10, Bgc%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(0) + "G": m% = 1: d% = 1: g% = 270: EXIT DO
        CASE CHR$(0) + "H": d% = d% - 1: EXIT DO
        CASE CHR$(0) + "I": m% = m% - 1: EXIT DO
        CASE CHR$(0) + "K": d% = d% - 2: EXIT DO
        CASE CHR$(0) + "M": d% = d% + 2: EXIT DO
        CASE CHR$(0) + "O": m% = 12: d% = 31: g% = 270: EXIT DO
        CASE CHR$(0) + "P": d% = d% + 1: EXIT DO
        CASE CHR$(0) + "Q": m% = m% + 1: EXIT DO
        CASE "+": g% = g% + 1: EXIT DO
        CASE "-": g% = g% - 1: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    COLOR 15, 1
    CLS
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, WSZ + 4, WSZ + 6: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt "Gestation Calculator", 2, 11, Bgc%
    SayIt "Presets", 4, 11, Bgc%
    SayIt STRING$(76, 45), 5, Fgc%, Bgc%
    FOR x% = 1 TO WSZ
    READ m%
    IF m% = 0 THEN
        EXIT FOR
    ELSE
        READ d%, g%
        Calculate m%, d%, g%, a%, b%
        i$ = "Conception:" + LEFT$(MID$(STR$(d%), 2) + " " + Mth$(m%) + STRING$(40, 46), 16)
        i$ = i$ + "Gestation:" + RIGHT$("000" + MID$(STR$(g%), 2), 3) + " (Days)"
        i$ = i$ + RIGHT$(STRING$(40, 46) + "Birth:" + MID$(STR$(b%), 2) + " " + Mth$(a%), 29)
        SayIt i$, x% + 5, 10, Bgc%
    END IF
    NEXT
    SayIt "These Calculations are Dependant Upon Normal Gestations", 21, 14, Bgc%
    SayIt "PAGE Month ù Scroll Days(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù Adjust Gestation(-+) ù ESC to End", 22, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + l$, 38)
    r$ = LEFT$(r$ + STRING$(38, 32), 38)
    SELECT CASE LEFT$(i$, 1)
        CASE "Ç", "º", "É", "È"
            PRINT #4, i$
        CASE ELSE
            LOCATE v%, 3
            COLOR f%, b%
            PRINT l$; r$
            COLOR Fgc%, Bgc%
    END SELECT
END SUB

