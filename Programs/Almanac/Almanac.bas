DECLARE SUB Search (s%, e%, k$)
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Bufferize ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
TYPE dahsr
    op AS STRING * 1
    da AS STRING * 7
    li AS STRING * 68
END TYPE
    CONST BOOT = "almanac.bas", GENESIS = -4004, MXL = 76
    CONST PRGM = "almanac", WSZ = 17
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED MyFile$
    COMMON SHARED Src$
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Dln AS dahsr, Mth(12) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"

SUB Bufferize
    DIM d$, h$, i$, o$, x%, y%
    CLOSE : OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    FOR x% = VAL(RIGHT$(DATE$, 4)) TO GENESIS STEP -1
    IF x% = 0 THEN x% = x% - 1
    o$ = "-"
    d$ = RIGHT$("0000" + MID$(STR$(x%), 2), 4)
    i$ = ""
    IF x% < 0 THEN
        d$ = d$ + " BC"
    ELSE
        d$ = d$ + " AD"
    END IF
    Dln.op = o$
    Dln.da = d$
    Flt% = 0
    CLOSE #1: OPEN PRGM + ".txt" FOR INPUT AS #1
    IF Flt% = 0 THEN
        DO WHILE NOT EOF(1)
        LINE INPUT #1, h$
        IF MID$(h$, 5, 1) <> " " THEN h$ = LEFT$(h$, 4) + " " + MID$(h$, 5)
        IF UCASE$(LEFT$(d$, LEN(Dln.da))) = UCASE$(LEFT$(h$, LEN(Dln.da))) THEN
            Dln.op = "+"
            i$ = MID$(h$, LEN(Dln.da) + 2)
            EXIT DO
        END IF
        LOOP
    END IF
    FOR y% = 1 TO LEN(i$)
    IF MID$(i$, y%, 1) = "," AND MID$(i$, y% + 1, 1) <> " " THEN
        i$ = LEFT$(i$, y%) + " " + MID$(i$, y% + 1)
    END IF
    NEXT
    Dln.li = LEFT$(i$ + STRING$(MXL, 46), MXL)
    PRINT #2, Dln.op; Dln.da; Dln.li
    NEXT
    CLOSE
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Getkey (k$)
    DIM a%
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Initialize
    DIM i$, x%
    FOR x% = 1 TO 12
    READ Mth(x%)
    NEXT
    Bgc% = 1
    Dbse$ = "dbse\"
    Fgc% = 15
    Flt% = 0
    MyFile$ = "myfiles\myfiles" + RIGHT$(BOOT, 4)
    Title$ = "DAHSR's Almanac from " + MID$(STR$(GENESIS), 2) + " BC to " + RIGHT$(DATE$, 4) + " AD"
    Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
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
    SHELL "dir > src.shl"
    OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    Src$ = LCASE$(MID$(i$, 15)) + "\"
    MyFile$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM) - 1) + MyFile$
    EXIT SUB
    CLS
    PRINT Src$
    PRINT MyFile$
    END
END SUB

SUB Main
    DIM k$
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% - WSZ + 1 THEN s% = e% - WSZ + 1
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(8)
            CHDIR LEFT$(MyFile$, LEN(MyFile$) - 12)
            RUN MyFile$
            CHDIR LEFT$(Src$, LEN(Src$) - 1)
            RUN BOOT
        CASE CHR$(9): SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "I": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = s% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "R": SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT
        CASE CHR$(0) + "S": SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT
        CASE "+", "-": Search s%, e%, k$: EXIT DO
        CASE "*": RUN BOOT
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
        CASE 3: PRINT "ÇÄ"; STRING$(LEN(Dln.da), 196); "Â"; STRING$(LEN(Dln.li), 196); "Ä¶"
        CASE 4 TO WSZ + 3: PRINT "º "; STRING$(LEN(Dln.da), 32); "³"; STRING$(LEN(Dln.li), 32); " º"
        CASE WSZ + 4: PRINT "ÇÄ"; STRING$(LEN(Dln.da), 196); "Á"; STRING$(LEN(Dln.li), 196); "Ä¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " ù Today is " + Today$, 2, 11, Bgc%
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù BACKSPACE to Exit ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM h%, l$, r$, w%
    w% = 38
    SELECT CASE w%
        CASE 38: h% = 3
        CASE 39: h% = 2
        CASE ELSE: h% = 1
    END SELECT
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, h%, 0
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, m$, x%
    m$ = CHR$(28)
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE e% + 1
            Dln.op = STRING$(MXL, 205)
            Dln.da = STRING$(MXL, 205)
            Dln.li = STRING$(MXL, 205)
        CASE ELSE
            Dln.op = STRING$(MXL, 32)
            Dln.da = STRING$(MXL, 32)
            Dln.li = STRING$(MXL, 32)
    END SELECT
    LOCATE x% + 3, 3
    IF Dln.op = "+" THEN
        COLOR 15, 4
    ELSEIF Dln.op = "-" THEN
        COLOR 10, Bgc%
    ELSE
        COLOR Fgc%, Bgc%
    END IF
    PRINT Dln.da; m$;
    IF Dln.op = "+" THEN
        COLOR 10, Bgc%
    ELSEIF Dln.op = "-" THEN
        COLOR 2, Bgc%
    ELSE
        COLOR Fgc%, Bgc%
    END IF
    PRINT Dln.li
    NEXT
    COLOR Fgc%, Bgc%
END SUB

SUB Search (s%, e%, k$)
    DIM x%
    x% = s%
    DO
    SELECT CASE k$
        CASE "+": x% = x% + 1
        CASE "-": x% = x% - 1
    END SELECT
    IF x% < 1 THEN x% = e%
    IF x% > e% THEN x% = 1
    IF x% = s% THEN EXIT DO
    GET #1, x%, Dln
    IF Dln.op = "+" THEN EXIT DO
    LOOP
    s% = x%
END SUB

