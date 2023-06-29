DECLARE SUB Decant (c$, i$)
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "scanner.bas", MXC = 200, MXL = 76, PRGM = "scanner", WSZ = 16
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Echo%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Myfile$
    COMMON SHARED Src$
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Grid(20, 10) AS STRING * MXL, Mth(12) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
Months:
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"

SUB Bufferize
    DIM c$, i$
    Flt% = 0
    CLOSE : OPEN Dbse$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        DO
        IF Flt% = 0 THEN
            INPUT #1, c$, i$
            Grid(VAL(c$)) = i$
        ELSE
            EXIT DO
        END IF
        LOOP
    ELSE
        Flt% = 0
    END IF
    CLOSE
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
        END
    ELSEIF k$ = "*" THEN
        RUN BOOT
    END IF
END SUB

SUB Initialize
    DIM i$, x%
    RESTORE Months
    FOR x% = 1 TO 12: READ Mth(x%): NEXT
    FOR x% = 1 TO MXC: Grid(x%) = "No Data": NEXT
    Bgc% = 1
    Dbse$ = PRGM + ".txt"
    Echo% = WSZ + 5
    Fgc% = 15
    Myfile$ = "myfiles\myfiles" + LCASE$(RIGHT$(BOOT, 4))
    Flt% = 0
    Title$ = "Radio Scanner Frequencies"
    Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(LEFT$(i$, x%))
            CASE "FILEBOX=", "FILEMANAGER=", "MYFILES=": myfiles$ = MID$(i$, x% + 1): EXIT FOR
        END SELECT
        NEXT
        WEND
    ELSE
        Flt% = 0
        CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #1
    END IF
    CLOSE
    SHELL "dir > src.shl"
    OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "src.shl"
    Src$ = LCASE$(MID$(i$, 15)) + "\"
    myfiles$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM)) + myfiles$
    FOR x% = 1 TO LEN(Src$)
    SELECT CASE UCASE$(MID$(Src$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
    'CLS
    'PRINT Src$
    'PRINT myfiles$
    'END
END SUB

SUB Main
    DIM e%, k$, p%, s%
    Mask
    e% = MXC
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + Dbse$: RUN BOOT
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
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
        CASE 3, WSZ + 4: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$, 2, 11, Bgc%
    x% = Echo%
    i$ = ""
    i$ = i$ + "Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Lines"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = ""
    i$ = i$ + "ESC to End"
    SayIt i$, x%, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM e$, h%, l$, r$, w%
    IF i$ = "" THEN e$ = "" ELSE e$ = " ù "
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 2
    h% = 3: w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + RIGHT$(e$, 2) + l$, w%)
    r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, h%
    PRINT l$; r$
    v% = v% + 1
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, j$, x%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: i$ = "Channel #" + MID$(STR$(s% + x% - 1), 2): j$ = Grid(s% + x% - 1)
        CASE e% + 1: i$ = "": j$ = STRING$(MXL, 205)
        CASE ELSE: i$ = "": j$ = ""
    END SELECT
    SELECT CASE UCASE$(LEFT$(j$, 1))
        CASE "0" TO "9", "A" TO "Z": f% = 10: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    LOCATE x% + 3, 3
    COLOR f%, b%
    PRINT LEFT$(i$ + STRING$(MXL, 46), MXL - LEN(j$)); j$
    'SayIt LEFT$(i$ + STRING$(MXL, 46), MXL - LEN(j$)) + j$, x% + 3, f%, b%
    NEXT
END SUB

