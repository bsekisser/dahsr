DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%, p%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB GetKey (k$)
DECLARE SUB Main ()
    CONST BOOT = "getkey.bas", MXL = 76, PRGM = "getkey", WSZ = 17
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED MyFile$
    COMMON SHARED Src$
    COMMON SHARED Title$
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB GetKey (k$)
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    Dbse$ = "dbse\"
    Echo% = WSZ + 5
    Fgc% = 15
    MyFile$ = "myfiles\myfiles" + LCASE$(RIGHT$(BOOT, 4))
    Flt% = 0
    Title$ = CHR$(34) + "GetKey" + CHR$(34) + " Analizer"
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(LEFT$(i$, x%))
            CASE "FILEBOX=", "FILEMANAGER=", "MYFILES=": MyFile$ = MID$(i$, x% + 1): EXIT FOR
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
    MyFile$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM)) + MyFile$
    FOR x% = 1 TO LEN(Src$)
    SELECT CASE UCASE$(MID$(Src$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
    'CLS
    'PRINT Src$
    'PRINT MyFile$
    'END
END SUB

SUB Main
    DIM k$
    k$ = ""
    Mask
    DO
    SELECT CASE ASC(k$)
        CASE 8: k$ = "BACKSPACE"
        CASE 9: k$ = "TAB"
        CASE 13: k$ = ""
        CASE 32: k$ = "SPACEBAR"
    END SELECT
    IF k$ = "" THEN
        SayIt "Press a Key!", 12, 14, Bgc%
    ELSE
        SayIt "Key Pressed: " + CHR$(34) + k$ + CHR$(34), 11, Fgc%, Bgc%
        SayIt "Length of key Pressed: (" + MID$(STR$(LEN(k$)), 2) + ")", 12, Fgc%, Bgc%
        SayIt "ASC of key Pressed; (" + MID$(STR$(ASC(k$)), 2) + ")", 13, Fgc%, Bgc%
    END IF
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
    LOOP
END SUB

SUB Mask
    DIM i$
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
    i$ = "ESC to End"
    SayIt i$, Echo%, Fgc%, Bgc%
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

