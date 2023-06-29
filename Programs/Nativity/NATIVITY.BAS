DECLARE SUB CalCulate (a%, b%, c%, e%, i$)
DECLARE SUB MtTrash ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "nativity.bas", MXL = 77, PRGM = "nativity", WSZ = 4
    COMMON SHARED Bgc%
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Edit$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Mdy%
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Src$
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Mth(12) AS STRING * 3
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"

SUB CalCulate (a%, b%, c%, e%, i$)
    DIM d%, m%, o%, x%, z%, y%
    m% = a%
    d% = b%
    y% = c%
    o% = 0
    FOR x% = 1 TO e%
    IF o% = 0 THEN
        SELECT CASE m%
            CASE 2: z% = 28
                SELECT CASE y%
                    CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: z% = 29
                    CASE ELSE: z% = 28
                END SELECT
            CASE 4, 6, 9, 11: z% = 30
            CASE ELSE: z% = 31
        END SELECT
        o% = 1
    END IF
    d% = d% + 1
    IF d% > z% THEN
        o% = 0
        d% = 1
        m% = m% + 1
        IF m% > 12 THEN
            m% = 1
            y% = y% + 1
            IF y% = 0 THEN y% = y% + 1
        END IF
    END IF
    NEXT
    i$ = RIGHT$("00" + MID$(STR$(d%), 2), 2)
    i$ = i$ + " " + Mth(m%)
    i$ = i$ + " " + STR$(y%)
    IF y% <= 0 THEN
        i$ = i$ + " BC"
    ELSE
        i$ = i$ + " AD"
    END IF
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Getkey (k$)
    DO
    k$ = INKEY$
    LOOP UNTIL k$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF ASC(k$) = 27 THEN
        MtTrash
        END
    ELSEIF k$ = "*" THEN
        MtTrash
        RUN BOOT
    END IF
END SUB

SUB Initialize
    DIM i$, x%
    FOR x% = 1 TO 12: READ Mth(x%): NEXT
    Bgc% = 1
    Dbse$ = "dbse\"
    Edit$ = "notepad.exe "
    Fgc% = 15
    Mdy% = 30
    q = CHR$(34)
    Title$ = "The Nativity Calculator"
    Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
    DO
    Flt% = 0: CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "EDITOR=": Edit$ = MID$(i$, x% + 1): EXIT FOR
                CASE "MDAYS=": Mdy% = VAL(MID$(i$, x% + 1)): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #2
        PRINT #2, "EDITOR="; Edit$
        PRINT #2, "GESTATION=30"
    END IF
    LOOP
    CLOSE
    SHELL "dir > src.shl"
    OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "src.shl"
    Src$ = LCASE$(MID$(i$, 15)) + "\"
    FOR x% = 1 TO LEN(Src$)
    SELECT CASE UCASE$(MID$(Src$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
    'CLS
    'PRINT Src$
    'END
END SUB

SUB Main
    DIM d%, k$, m%, y%
    Mask
    d% = 1
    m% = 1
    y% = -5
    DO
    SELECT CASE y%
        CASE 2: z% = 28
        CASE 4, 6, 9, 11: z% = 30
        CASE ELSE: z% = 31
    END SELECT
    IF d% < 1 THEN d% = z%: m% = m% - 1
    IF d% > z% THEN d% = 1: m% = m% + 1
    IF m% < 1 THEN m% = 12: y% = y% - 1: IF y% = 0 THEN y% = y% - 1
    IF m% > 12 THEN m% = 1: y% = y% + 1: IF y% = 0 THEN y% = y% + 1
    IF y% < -10 THEN y% = 10
    IF y% > 10 THEN y% = -10
    i$ = RIGHT$("00" + MID$(STR$(d%), 2), 2)
    i$ = i$ + " " + Mth(m%)
    i$ = i$ + " " + MID$(STR$(y%), 2)
    IF y% < 0 THEN
        i$ = i$ + " BC"
    ELSE
        i$ = i$ + " AD"
    END IF
    SayIt LEFT$("John's Conception" + STRING$(MXL, 46), MXL - LEN(i$) - 1) + i$, 4, 10, Bgc%
    CalCulate m%, d%, y%, 6 * Mdy%, i$
    SayIt LEFT$("Jesus' Conception (6 Mths After John's Conception)" + STRING$(MXL, 46), MXL - LEN(i$) - 1) + i$, 5, 11, Bgc%
    CalCulate m%, d%, y%, 9 * Mdy%, i$
    SayIt LEFT$("John's Birth (9 Mths After John's Conception)" + STRING$(MXL, 46), MXL - LEN(i$) - 1) + i$, 6, 14, Bgc%
    CalCulate m%, d%, y%, 15 * Mdy%, i$
    SayIt LEFT$("Jesus' Birth (15 Mths After John's Conception)" + STRING$(MXL, 46), MXL - LEN(i$) - 1) + i$, 7, 13, Bgc%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL Edit$ + PRGM + ".ini": RUN BOOT
        CASE CHR$(13): d% = 13: m% = 6: y% = -4: EXIT DO
        CASE CHR$(0) + "G": d% = 1: m% = 1: y% = -4: EXIT DO
        CASE CHR$(0) + "H": d% = d% - 1: EXIT DO
        CASE CHR$(0) + "I"
            y% = y% + 1
            IF y% = 0 THEN y% = y% + 1
            EXIT DO
        CASE CHR$(0) + "K": m% = m% - 1: EXIT DO
        CASE CHR$(0) + "M": m% = m% + 1: EXIT DO
        CASE CHR$(0) + "O": d% = 31: m% = 12: y% = y% - 3: EXIT DO
        CASE CHR$(0) + "Q"
            y% = y% - 1
            IF y% = 0 THEN y% = y% - 1
            EXIT DO
        CASE CHR$(0) + "P": d% = d% + 1: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM i$, x%
    CLS
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, 8: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt "ù " + Title$ + " ù", 2, 11, Bgc%
    x% = 9
    i$ = ""
    i$ = i$ + " ù The Calculation is Based on " + q + MID$(STR$(Mdy%), 2) + q + " Days a Month"
    i$ = i$ + " ù"
    SayIt i$, x%, 14, Bgc%
    i$ = ""
    i$ = i$ + " ù Scroll (" + CHR$(24) + CHR$(25) + ") Days"
    i$ = i$ + " ù Scroll (" + CHR$(27) + CHR$(26) + ") Months"
    i$ = i$ + " ù PAGE UP/DOWN Years"
    i$ = i$ + " ù"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = ""
    i$ = i$ + " ù HOME to Jan 1st"
    i$ = i$ + " ù ENTER June 13th"
    i$ = i$ + " ù END to Dec 31st"
    i$ = i$ + " ù"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = ""
    i$ = i$ + " ù TAB to Edit " + q + LCASE$(PRGM) + ".ini" + q
    i$ = i$ + " ù ESC to End"
    i$ = i$ + " ù"
    SayIt i$, x%, Fgc%, Bgc%
END SUB

SUB MtTrash
    SHELL "dir > " + PRGM + ".can /B"
    
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM h%, l$, r$, w%
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 2
    w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$; r$
    v% = v% + 1
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, m$, o$, x%
    FOR x% = 1 TO 4
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE e% + 1: Dln = STRING$(MXL, 205)
        CASE ELSE: Dln = STRING$(MXL, 32)
    END SELECT
    o$ = LEFT$(Dln, 1)
    i$ = MID$(Dln, 2)
    SELECT CASE o$
        CASE "*": f% = 11: b% = Bgc%
        CASE "+": f% = 10: b% = Bgc%
        CASE "-": f% = 14: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    SayIt LEFT$(i$ + STRING$(MXL, o$), MXL), x% + 3, f%, b%
    NEXT
END SUB

