DECLARE SUB Default (i$)
DECLARE SUB Decant (i$, j$)
DECLARE SUB FormatDate ()
DECLARE SUB PurgeDbse ()
DECLARE SUB FindNote (s%, e%, k$)
DECLARE SUB FindToday (s%, e%)
DECLARE SUB ReverseDate (i$)
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Bufferize ()
DECLARE SUB Mask ()
DECLARE SUB Cuckoo ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
TYPE dahsr
    day AS STRING * 11
    entry AS STRING * 64
END TYPE
    CONST BOOT = "notebook.bas"
    CONST MXL = 76
    CONST PRGM = "notebook"
    CONST WSZ = 16
    COMMON SHARED Bfr$
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Echo%
    COMMON SHARED Editor$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Nts%
    COMMON SHARED Path$
    COMMON SHARED Prompt$
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Syear%
    COMMON SHARED Title$
    COMMON SHARED Today$
    COMMON SHARED Work$
    DIM SHARED Notebook AS dahsr, Mth(12) AS STRING * 3
    ON ERROR GOTO Trap
    Initialize
    FormatDate
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
Months:
DATA "Jan","Feb","Mar","Apr","May","Jun"
DATA "Jul","Aug","Sep","Oct","Nov","Dec"
DaysOfWeek:
DATA "Sun","Mon","Tue","Wed","Thu","Fri","Sat"

SUB Bufferize
    DIM d%, i$, j$, k$, m%, n$, o%, x%, y%, z%
    Flt% = 0
    CLOSE : OPEN Work$ FOR INPUT AS #1
    OPEN Bfr$ FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        FOR y% = Syear% - 1 TO Syear% + 1
        FOR m% = 1 TO 12
        SELECT CASE m%
            CASE 2
                SELECT CASE VAL(RIGHT$(MID$(STR$(y%), 2), 2))
                    CASE 0, 4, 8, 12, 16, 20, 24, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: z% = 29
                    CASE ELSE: z% = 28
                END SELECT
            CASE 4, 6, 9, 11: z% = 30
            CASE ELSE: z% = 31
        END SELECT
        FOR d% = 1 TO z%
        n$ = RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/"
        n$ = n$ + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/"
        n$ = n$ + RIGHT$("0000" + MID$(STR$(y%), 2), 4)
        Notebook.day = RIGHT$("00" + MID$(STR$(d%), 2), 2) + " " + Mth(m%) + " " + RIGHT$("0000" + MID$(STR$(y%), 2), 4)
        Notebook.entry = STRING$(LEN(Notebook.entry), 46)
        o% = 0
        CLOSE #1: OPEN Work$ FOR INPUT AS #1
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        SayIt TIME$, 14, 10, Bgc%
        k$ = INKEY$
        IF k$ = CHR$(9) THEN
            SHELL Editor$ + Dbse$
            RUN BOOT
        ELSEIF k$ = CHR$(13) THEN
            CLS
            RUN BOOT
        ELSEIF k$ = CHR$(27) THEN
            END
        END IF
        IF i$ = "" THEN
            i$ = ""
        ELSE
            SELECT CASE LEFT$(i$, 1)
                CASE "0" TO "9"
                    Decant i$, j$
                    IF i$ = LEFT$(n$, LEN(i$)) THEN
                        o% = 1
                        Nts% = Nts% + 1
                        Notebook.entry = RIGHT$(STRING$(MXL, 46) + j$, LEN(Notebook.entry))
                        PRINT #2, Notebook.day; Notebook.entry
                    END IF
            END SELECT
        END IF
        WEND
        IF o% = 0 THEN PRINT #2, Notebook.day; Notebook.entry
        NEXT
        NEXT
        NEXT
    ELSE
        Flt% = 0
        Default "Cannot Find the " + q + Work$ + q + " File"
    END IF
    CLOSE
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Decant (i$, j$)
    CLOSE #9: OPEN "decant.txt" FOR OUTPUT AS #9
    PRINT #9, i$
    CLOSE #9: OPEN "decant.txt" FOR INPUT AS #9
    INPUT #9, i$
    LINE INPUT #9, j$
    CLOSE #9
    KILL "decant.txt"
END SUB

SUB Default (i$)
    SayIt i$, Echo%, 15, 4
    SayIt "Press TAB to Edit or ESC to End", Echo% + 1, 15, 4
    DO
    Getkey k$
    IF k$ = CHR$(9) THEN
        SHELL "notepad.exe " + Dbse$
        RUN BOOT
    END IF
    LOOP
END SUB

SUB FindNote (s%, e%, k$)
    DIM x%
    x% = s%
    DO
    SELECT CASE UCASE$(k$)
        CASE "+": x% = x% + 1
        CASE "-": x% = x% - 1
    END SELECT
    IF x% = s% THEN EXIT DO
    IF x% < 1 THEN x% = e%
    IF x% > e% THEN x% = 1
    GET #1, x%, Notebook
    SELECT CASE UCASE$(RIGHT$(Notebook.entry, 1))
        CASE CHR$(32) TO CHR$(45), CHR$(47) TO CHR$(126): EXIT DO
    END SELECT
    LOOP UNTIL x% = s%
    s% = x%
END SUB

SUB FindToday (s%, e%)
    DIM x%
    x% = s%
    DO
    x% = x% + 1
    IF x% > e% THEN
        x% = 1
    ELSEIF x% = s% THEN
        EXIT DO
    END IF
    GET #1, x%, Notebook
    IF RIGHT$(Notebook.day, LEN(Today$)) = Today$ THEN EXIT DO
    LOOP
    s% = x%
END SUB

SUB FormatDate
    DIM a$, b$, i$, j$, l$, x%, y$
    Mask
    y$ = LEFT$(RIGHT$(DATE$, 4), 2)
    Flt% = 0
    CLOSE : OPEN Dbse$ FOR INPUT AS #1
    OPEN Work$ FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        SELECT CASE LEFT$(i$, 1)
            CASE "0" TO "9"
                Decant i$, j$
                l$ = MID$(STR$(VAL(l$) + 1), 2)
                a$ = ""
                FOR x% = 1 TO LEN(i$)
                SELECT CASE MID$(i$, x%, 1)
                    CASE "/", "\": a$ = a$ + "/"
                    CASE "0" TO "9": a$ = a$ + "x"
                    CASE ELSE: Default "Character Error on Line #" + l$ + " (" + q + i$ + q + ")"
                END SELECT
                NEXT
                b$ = ""
                SELECT CASE a$
                    CASE "x/x/xx": b$ = "0" + LEFT$(i$, 2) + "0" + MID$(i$, 3, 2) + y$ + RIGHT$(i$, 2)
                    CASE "xx/x/xx": b$ = LEFT$(i$, 3) + "0" + MID$(i$, 4, 2) + y$ + RIGHT$(i$, 2)
                    CASE "x/xx/xx": b$ = "0" + LEFT$(i$, 5) + y$ + RIGHT$(i$, 2)
                    CASE "xx/xx/xx": b$ = LEFT$(i$, 6) + y$ + RIGHT$(i$, 2)
                    CASE "x/x/xxxx": b$ = "0" + LEFT$(i$, 2) + "0" + MID$(i$, 3, 2) + y$ + RIGHT$(i$, 4)
                    CASE "xx/x/xxxx": b$ = LEFT$(i$, 3) + "0" + MID$(i$, 4, 2) + y$ + RIGHT$(i$, 4)
                    CASE "x/xx/xxxx": b$ = "0" + LEFT$(i$, 5) + y$ + RIGHT$(i$, 4)
                    CASE ELSE: Default "Cannot Determine the Date Format of " + q + i$ + q + " on Line #" + l$
                END SELECT
                i$ = b$
                PRINT #2, i$; ","; j$
        END SELECT
        WEND
    END IF
    CLOSE
END SUB

SUB Getkey (k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF k$ = "*" THEN
        RUN BOOT
    ELSEIF k$ = CHR$(27) THEN
        END
    END IF
END SUB

SUB Initialize
    DIM i$, x%
    RESTORE Months: FOR x% = 1 TO 12: READ Mth(x%): NEXT
    Bfr$ = PRGM + ".bfr"
    Bgc% = 1
    Dbse$ = PRGM + ".txt"
    Echo% = WSZ + 5
    Editor$ = "notepad.exe "
    Fgc% = 15
    q = CHR$(34)
    Syear% = VAL(RIGHT$(DATE$, 4))
    Title$ = "The Window of Notebook Entries Span from 01 Jan " + MID$(STR$(Syear% - 1), 2) + " to 31 Dec " + MID$(STR$(Syear% + 1), 2)
    Today$ = MID$(DATE$, 4, 2) + " " + Mth$(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
    Work$ = PRGM + ".wrk"
    DO
    Flt% = 0
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "EDITOR=": Editor$ = MID$(i$, x% + 1): EXIT FOR
                CASE "SYEAR=": Syear% = VAL(MID$(i$, x% + 1)): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #2
        PRINT #2, "EDITOR="; Editor$
        PRINT #2, "SYEAR="; MID$(STR$(Syear%), 2)
    END IF
    LOOP
END SUB

SUB Justify (i$, j$)

END SUB

SUB Main
    DIM a%, e%, k$, s%
    Mask
    CLOSE : OPEN Bfr$ FOR RANDOM AS #1 LEN = LEN(Notebook) + 2
    e% = LOF(1) / (LEN(Notebook) + 2)
    s% = 1
    FindToday s%, e%
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE "+", "-": FindNote s%, e%, k$: EXIT DO
        CASE CHR$(8): s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(9): SHELL "notepad.exe " + Dbse$: RUN BOOT
        CASE CHR$(13): FindToday s%, e%: EXIT DO
        CASE CHR$(0) + CHR$(15): SHELL "notepad.exe " + PRGM + ".ini": RUN BOOT
        CASE CHR$(0) + "G": s% = t%: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "I": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = s% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
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
        o$ = "-"
        LOCATE 1, 1, 0
    END IF
    i$ = "[ Notes: " + MID$(STR$(Nts%), 2) + " ]"
    COLOR Fgc%, Bgc%
    IF o$ = "+" THEN
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3, Echo%: PRINT "Ç"; STRING$(78, 196); "¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º" + STRING$(78, 32); "º"
        END SELECT
        NEXT
    ELSE
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3: PRINT "ÇÄ"; STRING$(LEN(Notebook.day), 196); "Â"; STRING$(LEN(Notebook.entry), 196); "Ä¶"
            CASE 4 TO Echo% - 2: PRINT "º "; STRING$(LEN(Notebook.day), 32); "³"; STRING$(LEN(Notebook.entry), 32); " º"
            CASE Echo% - 1: PRINT "ÇÄ"; STRING$(LEN(Notebook.day), 196); "Á"; STRING$(LEN(Notebook.entry), 196); "Ä¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
        NEXT
    END IF
    SayIt Title$, 2, 11, Bgc%
    IF o$ = "+" THEN
        SayIt "Standby, Loading " + q + Dbse$ + q, 10, 14, Bgc%
        SayIt "(Loading Time Determined by the File Size)", 12, 11, Bgc%
        SayIt "Press TAB to Edit Data, ENTER to Exit or ESC to End", 22, Fgc%, Bgc%
    ELSE
        LOCATE WSZ + 4, 40 - (LEN(i$) / 2): COLOR Fgc%, Bgc%: PRINT i$
        SayIt "Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Lines ù ENTER Today's Date (" + q + Today$ + q + ")", Echo%, Fgc%, Bgc%
        SayIt "TAB: Edit " + q + Dbse$ + q + " ù (+)Next Entry ù (-)Last Entry ù ESC to End", Echo% + 1, Fgc%, Bgc%
    END IF
END SUB

SUB ReverseDate (i$)
    DIM x%
    d$ = ""
    FOR x% = 1 TO LEN(i$)
    SELECT CASE MID$(i$, x%, 1)
        CASE "0" TO "9": d$ = d$ + MID$(i$, x%, 1)
    END SELECT
    NEXT
    i$ = RIGHT$(STRING$(10, "0") + d$, LEN(d$))
    d$ = RIGHT$(i$, 2) + LEFT$(i$, 4)
    i$ = d$
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM e$, l$, r$, w%
    IF i$ = "" THEN e$ = "" ELSE e$ = " ù "
    w% = 39
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + RIGHT$(e$, 2) + l$, w%)
    r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(w%, 32), w%)
    IF v% < 2 THEN v% = 2
    IF v% = 3 THEN v% = 4
    IF v% = Echo% - 1 THEN v% = Echo%
    IF v% > 22 THEN v% = 22
    COLOR f%, b%
    LOCATE v%, 2, 0
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, j$, m$, x%
    m$ = CHR$(28)
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Notebook
        CASE e% + 1
            Notebook.day = STRING$(MXL, 205)
            Notebook.entry = STRING$(MXL, 205)
        CASE ELSE
            Notebook.day = STRING$(MXL, 32)
            Notebook.entry = STRING$(MXL, 32)
    END SELECT
    SELECT CASE UCASE$(RIGHT$(Notebook.entry, 1))
        CASE "*"
            f% = 11
            b% = Bgc%
        CASE CHR$(32) TO CHR$(126)
            f% = 10
            b% = Bgc%
        CASE ELSE
            f% = Fgc%
            b% = Bgc%
    END SELECT
    IF RIGHT$(Notebook.day, LEN(Today$)) = Today$ THEN f% = 15: b% = 3
    LOCATE x% + 3, 3
    COLOR f%, b%
    PRINT Notebook.day + m$ + Notebook.entry
    NEXT
    COLOR Fgc%, Bgc%
END SUB

