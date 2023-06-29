DECLARE SUB Scroll (s%, e%)
DECLARE SUB Display (r%, k$)
DECLARE SUB Initialize ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB Mask ()
    CONST MXL = 76
    CONST WSZ = 15
    COMMON SHARED Bgc%
    COMMON SHARED Echo%
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED q AS STRING * 1
    DIM SHARED Mth$(12)
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
Months:
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"
Gestations:
DATA 180,286,0

SUB Bufferize
    DIM d%, m%, x%, z%
    RESTORE Gestations
    DO
    READ p%
    IF p% <= 0 THEN
        EXIT DO
    ELSE
        Tr% = Tr% + 1
        CLOSE : OPEN "fbse\" + MID$(STR$(Tr%), 2) + ".txt" FOR OUTPUT AS #2
        FOR m% = 1 TO 12
        FOR d% = 1 TO 30
        NEXT
        NEXT
    END IF
END SUB

SUB Display (r%, k$)
    DIM e%, s%
    Mask
    CLOSE : OPEN "fbse\" + MID$(STR$(r%), 2) + ".txt" FOR RANDOM AS #1
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE ELSE: EXIT SUB
    END SELECT
    LOOP
    LOOP
END SUB

SUB Getkey (k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF k$ = CHR$(27) THEN END
END SUB

SUB Initialize
    DIM i$
    RESTORE Months
    FOR x% = 1 TO 12
    READ Mth$(x%)
    NEXT
    Bgc% = 1
    Fgc% = 15
    Echo% = WSZ + 5
    q = CHR$(34)
END SUB

SUB Main
    DIM k$, r%
    r% = 0
    DO
    IF r% < 0 THEN r% = Tr%
    IF r% > Tr% THEN r% = 0
    Display r%, k$
    SELECT CASE k$
        CASE CHR$(0) + "K": r% = r% - 1
        CASE CHR$(0) + "M": r% = r% + 1
    END SELECT
    LOOP
END SUB

SUB Mask
    DIM i$, x%
    STATIC o$
    IF o$ = "" THEN
        o$ = "+"
        CLS
    ELSE
        LOCATE 1, 1, 0
    END IF
    COLOR 15, 1
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, Echo% - 1: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt "Gestation Calculations", 2, 11, Bgc%
    x% = Echo%
    i$ = "Scroll (" + CHR$(24) + CHR$(25) + ") Lines"
    i$ = i$ + " ù Scroll (" + CHR$(27) + CHR$(26) + ") Gestations"
    SayIt i$, x%, Fgc%, Bgc%
    SayIt "Calculations are based on a " + q + "30" + q + " Day Month", x%, 14, Bgc%
    SayIt "ESC to End", x%, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + "ù " + l$, 38)
    r$ = LEFT$(r$ + " ù" + STRING$(38, 32), 38)
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 22
    LOCATE v%, 3
    COLOR f%, b%
    PRINT l$; r$
    v% = v% + 1
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, x%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE e% + 1: Dln = STRING$(MXL, 205)
        CASE ELSE: Dln = STRING$(MXL, 32)
    END SELECT
    i$ = LEFT$(Dln, MXL)
    b% = Bgc%
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z": f% = 10
        CASE ELSE: f% = Fgc%
    END SELECT
    SayIt i$, x% + 3, f%, b%
    NEXT
END SUB

