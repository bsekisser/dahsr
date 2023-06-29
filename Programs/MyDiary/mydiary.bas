DECLARE SUB Insert ()
DECLARE SUB Delete ()
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%, p%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB Enter ()
    CONST BOOT = "mydiary.bas"
    CONST COLS = 5
    CONST MXL = 12
    CONST PRGM = "mydiary"
    CONST ROWS = 16
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Edit$
    COMMON SHARED Ext$
    COMMON SHARED Fgc%
    COMMON SHARED Filebox$
    COMMON SHARED Filename$
    COMMON SHARED Flt%
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Span%
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
HelpTxt:
DATA "If the PRESENT DATE file is not in the database then it will be automatically created when the application is run."
DATA ""
DATA "If the PRESENT DATE file is in the database, and if data is entered therein, then any data will not be lost when the application is run because the file will already exist."
DATA ""
DATA "If the PRESENT DATE file is deleted on it's PRESENT DATE then the file will be recreated when the application is run on the SAME DATE of startup, but all former entered data will be lost."
DATA "*"

SUB Bufferize
    DIM i$
    Flt% = 0
    SHELL "dir " + LEFT$(Dbse$, 4) + " > temp.txt /B"
    CLOSE : OPEN "temp.txt" FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        PRINT #2, LEFT$(LCASE$(i$) + STRING$(MXL, 32), MXL)
        WEND
        DO
        i$ = LCASE$(RIGHT$(DATE$, 4) + LEFT$(DATE$, 2) + MID$(DATE$, 4, 2) + Ext$)
        CLOSE #1: OPEN Dbse$ + i$ FOR INPUT AS #1
        IF Flt% = 0 THEN
            EXIT DO
        ELSE
            Flt% = 0
            CLOSE #1: OPEN Dbse$ + i$ FOR OUTPUT AS #1
            PRINT #1, Title$
            PRINT #2, LEFT$(LCASE$(i$) + STRING$(MXL, 32), MXL)
            EXIT DO
        END IF
        LOOP
    ELSE
        Flt% = 0
    END IF
    CLOSE
    KILL "temp.txt"
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Delete
    DIM k$
    DO
    IF Filename$ = "" THEN
        EXIT DO
    ELSE
        SayIt "DELETE This File (Y/N) ?", Echo%, 14, Bgc%
        SayIt q + Filename$ + q, Echo% + 1, 14, Bgc%
        DO
        Getkey k$
        SELECT CASE UCASE$(k$)
            CASE "Y", CHR$(0) + "S"
                KILL Dbse$ + Filename$
                RUN BOOT
            CASE "N": EXIT DO
        END SELECT
        LOOP
        EXIT DO
    END IF
    LOOP
END SUB

SUB Enter
    Flt% = 0
    IF Filename$ = "" THEN
        Insert
    ELSE
        DO
        CLOSE #1: OPEN Dbse$ + Filename$ FOR INPUT AS #1
        IF Flt% = 0 THEN
            SHELL Edit$ + Dbse$ + Filename$
            RUN BOOT
        ELSE
            Flt% = 0
            CLOSE #1: OPEN Dbse$ + Filename$ FOR OUTPUT AS #1
            PRINT #1, Title$
        END IF
        LOOP
    END IF
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
    FOR x% = 1 TO 12: READ Mth(x%): NEXT
    Bgc% = 1
    Dbse$ = "dbse\": MKDIR "dbse"
    Echo% = ROWS + 5
    Edit$ = "notepad.exe "
    Ext$ = ".txt"
    Fgc% = 15
    Filebox$ = "filebox\filebox" + LCASE$(RIGHT$(BOOT, 4))
    Flt% = 0
    q = CHR$(34)
    Span% = COLS * ROWS
    Title$ = "My Diary"
    Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "EDITOR=": Edit$ = MID$(i$, x% + 1): EXIT FOR
                CASE "EXT=", "EXTENTION=": Ext$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
    ELSE
        Flt% = 0
        CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #1
        PRINT #1, "EDITOR="; Edit$
    END IF
    DO
    CLOSE : OPEN "help.txt" FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        Flt% = 0
        CLOSE : OPEN "help.txt" FOR OUTPUT AS #1
        PRINT #1, Title$ + " Help:"
        RESTORE HelpTxt
        DO
        READ i$
        IF Flt% = 0 THEN
            IF i$ = "*" THEN
                EXIT DO
            ELSE
                PRINT #1, i$
            END IF
        ELSE
            Flt% = 0
            EXIT DO
        END IF
        LOOP
    END IF
    LOOP
    CLOSE
    SHELL "dir > src.shl"
    OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "src.shl"
    Src$ = LCASE$(MID$(i$, 15)) + "\"
    Filebox$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM)) + Filebox$
    FOR x% = 1 TO LEN(Src$)
    SELECT CASE UCASE$(MID$(Src$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
    'CLS
    'PRINT Src$
    'PRINT Filebox$
    'END
END SUB

SUB Insert
    DIM x%
    FOR x% = 0 TO 9999
    DO
    Filename$ = RIGHT$(DATE$, 4) + RIGHT$(STRING$(4, "0") + MID$(STR$(x%), 2), 4) + Ext$
    CLOSE #1: OPEN PRGM + ".bfr" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    IF i$ = Filename$ THEN EXIT DO
    WEND
    CLOSE #1: OPEN Dbse$ + Filename$ FOR OUTPUT AS #1
    PRINT #1, Title$
    EXIT FOR
    LOOP
    NEXT
    RUN BOOT
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
    IF p% > Span% THEN p% = Span%: s% = s% + 1
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%, p%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe": RUN BOOT
        CASE CHR$(13): Enter: EXIT DO
        CASE CHR$(0) + "G": p% = 1: s% = 1: EXIT DO
        CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
        CASE CHR$(0) + "K": p% = p% - ROWS: EXIT DO
        CASE CHR$(0) + "M": p% = p% + ROWS: EXIT DO
        CASE CHR$(0) + "O": p% = Span%: s% = e%: EXIT DO
        CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
        CASE CHR$(0) + "R": Insert: EXIT DO
        CASE CHR$(0) + "S": Delete: EXIT DO
        CASE ELSE: SHELL Edit$ + "help.txt": RUN BOOT
    END SELECT
    LOOP
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
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, Echo% - 1: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " ù Today is " + Today$, 2, 11, Bgc%
    i$ = "Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") to Select a File"
    i$ = i$ + " ù BACKSPACE to Exit"
    i$ = i$ + " ù ESC to End"
    SayIt i$, Echo% + 1, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM e$, h%, l$, r$, w%
    IF i$ = "" THEN e$ = "" ELSE e$ = " ù "
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 22
    h% = 3: w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + RIGHT$(e$, 2) + l$, w%)
    r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, h%
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%, p%)
    DIM b%, f%, g%, h%, i$, v%, x%
    Filename$ = ""
    h% = 9
    v% = 1
    FOR x% = 1 TO Span%
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE ELSE: Dln = STRING$(MXL, 32)
    END SELECT
    i$ = RTRIM$(LEFT$(Dln, MXL))
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z"
            f% = 10
            b% = Bgc%
            IF x% = p% THEN Filename$ = i$
        CASE ELSE: f% = 10: b% = Bgc%
    END SELECT
    IF x% = p% THEN f% = 15: b% = 13
    LOCATE v% + 3, h%
    COLOR f%, b%
    PRINT LEFT$(i$ + STRING$(MXL, 46), MXL)
    v% = v% + 1: IF v% > ROWS THEN v% = 1: h% = h% + MXL + 1
    NEXT
    IF Filename$ = "" THEN
        SayIt "ENTER or INSERT a New File", Echo%, 14, Bgc%
    ELSE
        SayIt "ENTER Selected File ù DELETE Selected File ù INSERT a New File ", Echo%, 14, Bgc%
    END IF
    COLOR Fgc%, Bgc%
END SUB

