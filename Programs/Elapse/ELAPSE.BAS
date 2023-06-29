DECLARE SUB FormatDate (i$)
DECLARE SUB Calculate (i$, j$)
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
    CONST BOOT = "elapse.bas", MXL = 76, PRGM = "elapse", WSZ = 15
    COMMON SHARED Bgc%
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Title$
    COMMON SHARED Today$
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Bufferize
    DIM d$, e$, j$
    Mask
    tm = 0
    Flt% = 0
    CLOSE
    OPEN PRGM + ".txt" FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        INPUT #1, d$
        LINE INPUT #1, e$
        FormatDate d$
        SayIt LEFT$(d$ + "³" + STRING$(MXL, 46), MXL - LEN(e$)) + e$, 22, 26, Bgc%
        Calculate (d$), j$
        PRINT #2, LEFT$(d$ + "³" + e$ + "." + STRING$(MXL, 46), MXL - LEN(j$)); j$
        WEND
    ELSE
        Flt% = 0
        PRINT #2, LEFT$(Today$ + STRING$(MXL, 46), MXL - 7); "No Data"
    END IF
    CLOSE
END SUB

SUB Calculate (i$, j$)
    DIM c, d%, m%, t$, y%, z%
    t$ = RIGHT$(Today$, 4) + LEFT$(Today$, 2) + MID$(Today$, 4, 2)
    j$ = "0"
    m% = VAL(i$)
    d% = VAL(MID$(i$, 4))
    y% = VAL(RIGHT$(i$, 4))
    DO
    SELECT CASE m%
        CASE 2
            SELECT CASE VAL(RIGHT$("00" + MID$(STR$(y%), 2), 2))
                CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48: z% = 29
                CASE 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: z% = 29
                CASE ELSE: z% = 28
            END SELECT
        CASE 4, 6, 9, 11: z% = 30
        CASE ELSE: z% = 31
    END SELECT
    c = c + 1
    d% = d% + 1
    IF d% > z% THEN
        d% = 1
        m% = m% + 1
        IF m% > 12 THEN
            m% = 1
            y% = y% + 1
        END IF
    END IF
    i$ = RIGHT$("0000" + MID$(STR$(y%), 2), 4)
    i$ = i$ + RIGHT$("00" + MID$(STR$(m%), 2), 2)
    i$ = i$ + RIGHT$("00" + MID$(STR$(d%), 2), 2)
    LOOP WHILE VAL(i$) < VAL(t$)
    t$ = " (" + MID$(STR$(c), 2) + " Days" + ")"
    y% = INT(c / 365)
    c = c - (y% * 365)
    m% = INT(c / 30)
    c = c - (m% * 30)
    d% = c
    j$ = MID$(STR$(y%), 2) + " Yrs, " + MID$(STR$(m%), 2) + " Mths and " + MID$(STR$(d%), 2) + " Days" + t$
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB FormatDate (i$)
    DIM d$, m$, y$, x%
    m$ = MID$(STR$(VAL(i$)), 2)
    FOR x% = 1 TO LEN(i$)
    IF MID$(i$, x%, 1) = "/" THEN
        IF d$ = "" THEN
            d$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
        ELSE
            y$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
            EXIT FOR
        END IF
    END IF
    NEXT
    i$ = RIGHT$("00" + m$, 2) + "/"
    i$ = i$ + RIGHT$("00" + d$, 2) + "/"
    i$ = i$ + RIGHT$(LEFT$(RIGHT$(DATE$, 4), 4 - LEN(y$)) + y$, 4)
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN
        END
    ELSEIF k$ = "*" THEN
        RUN BOOT
        END
    END IF
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    Fgc% = 15
    Flt% = 0
    Title$ = "My QBasic Time Elapse Calculator"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
END SUB

SUB Main
    DIM a%, e%, k$, s%
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
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
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
    STATIC o$
    IF o$ = "" THEN
        o$ = "+"
        CLS
    ELSE
        LOCATE 1, 1, 0
    END IF
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, 5, 21: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 4: PRINT LEFT$("º Date......³Event" + STRING$(MXL, 46), MXL - 10); "Time Elapsed º"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " ù " + Today$, 2, 11, Bgc%
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù TAB to Edit ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(39, 32) + l$, 39)
    r$ = LEFT$(r$ + STRING$(39, 32), 39)
    COLOR f%, b%
    LOCATE v%, 2
    PRINT l$; r$
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
    i$ = Dln
    SELECT CASE LEFT$(i$, 1)
        CASE CHR$(32) TO CHR$(122): f% = 10: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    SayIt i$, x% + 5, f%, b%
    NEXT
END SUB

