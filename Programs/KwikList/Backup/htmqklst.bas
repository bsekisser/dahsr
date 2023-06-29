DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%, p%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "htmqklst.bas", MXL = 76, PRGM = "htmqklst", WSZ = 16
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Flmngr$
    COMMON SHARED Src$
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Mth(12) AS STRING
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
    DIM i$
    Flt% = 0
    CLOSE : OPEN Dbse$ FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        PRINT #2, LEFT$(i$ + STRING$(MXL, 32), MXL)
        WEND
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
    Bgc% = 1
    Dbse$ = "dbse\"
    Echo% = WSZ + 5
    Fgc% = 15
    Flmngr$ = "flmngr\flmngr" + LCASE$(RIGHT$(BOOT, 4))
    Flt% = 0
    Title$ = "New Mask Program"
    Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(LEFT$(i$, x%))
            CASE "FILEBOX=", "FILEMANAGER=", "MYFILES=": Flmngr$ = MID$(i$, x% + 1): EXIT FOR
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
    Flmngr$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM)) + Flmngr$
    FOR x% = 1 TO LEN(Src$)
    SELECT CASE UCASE$(MID$(Src$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
    'CLS
    'PRINT Src$
    'PRINT Flmngr$
    'END
END SUB

SUB Main
    DIM e%, k$, p%, s%
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    p% = 1
    s% = 1
    DO
    IF p% < 1 THEN p% = 1: s% = s% - 1
    IF p% > WSZ THEN p% = WSZ: s% = s% + 1
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%, p%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe": RUN BOOT
        CASE CHR$(0) + "G": p% = 1: s% = 1: EXIT DO
        CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
        CASE CHR$(0) + "K": p% = p% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "M": p% = p% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "O": p% = WSZ: s% = 1: EXIT DO
        CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
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
    'h% = 1: w% = 40
    'h% = 2: w% = 39
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

SUB Scroll (s%, e%, p%)
    DIM b%, f%, i$, m$, o$, x%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln: o$ = CHR$(46)
        CASE e% + 1: Dln = STRING$(MXL, 205): o$ = CHR$(205)
        CASE ELSE: Dln = STRING$(MXL, 32): o$ = CHR$(32)
    END SELECT
    i$ = LEFT$(Dln, MXL)
    j$ = i$
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z": f% = 10: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    IF x% = p% THEN f% = 15: b% = 13
    SayIt LEFT$(i$ + STRING$(MXL, o$), MXL - LEN(j$)) + j$, x% + 3, f%, b%
    NEXT
END SUB

