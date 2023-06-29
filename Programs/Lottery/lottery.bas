DECLARE SUB Purge ()
DECLARE SUB Backup ()
DECLARE SUB MsgBox (i$)
DECLARE SUB ValiData ()
DECLARE SUB Update ()
DECLARE SUB Validate (i$, j$)
DECLARE SUB DataFault (i$, j$)
DECLARE SUB Cuckoo ()
DECLARE SUB Edit (f$)
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Display (f$, k$)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "lottery.bas", FORWARD = "sort"
    CONST MXD = 90, MXL = 80, WSZ = 20
    COMMON SHARED Bgc%
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Mxg%
    COMMON SHARED q$
    COMMON SHARED Spx$
    COMMON SHARED Title$
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Backup
    DIM f$, i$
    MsgBox "Backing Up Database!"
    MKDIR "backup"
    Flt% = 0
    CLOSE
    OPEN "games.txt" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, f$
        CLOSE #3: OPEN "dbse\" + f$ FOR INPUT AS #3
        CLOSE #4: OPEN "backup\" + f$ FOR OUTPUT AS #4
        WHILE NOT EOF(3)
        LINE INPUT #3, i$
        IF LEN(i$) > 0 THEN PRINT #4, i$
        WEND
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

SUB DataFault (f$, j$)
    STATIC x$
    IF x$ = "" THEN
        Mask
        e$ = MID$(STR$(VAL(e$) + 1), 2)
        SayIt "Data Error in " + q$ + f$ + q$ + " File:", 21, 14, 4
        SayIt j$, 22, 14, 4
        SayIt "ù ENTER to Continue ù END Datacheck ù TAB to Edit ù ESC to End ù", 23, 14, 4
        DO
        Getkey k$
        SELECT CASE UCASE$(k$)
            CASE CHR$(9)
                SHELL "notepad.exe dbse\" + f$
                RUN BOOT
            CASE CHR$(0) + "O"
                x$ = "+"
                EXIT DO
        END SELECT
        LOOP UNTIL k$ = CHR$(13)
    END IF
END SUB

SUB Display (f$, k$)
    DIM a%, e%, i$, s%, x%
    Mask
    CLOSE : OPEN "text\" + f$ + ".txt" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(8): EXIT SUB
        CASE CHR$(9): EXIT SUB
        CASE CHR$(13): Update: RUN BOOT
        CASE "+", "-": EXIT SUB
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "I": EXIT SUB
        CASE CHR$(0) + "K": EXIT SUB
        CASE CHR$(0) + "M": EXIT SUB
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "Q": EXIT SUB
        CASE "0" TO "9": EXIT SUB
        CASE "P": Purge: RUN BOOT
    END SELECT
    'Cuckoo
    LOOP
    LOOP
END SUB

SUB Getkey (k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    IF UCASE$(RIGHT$(BOOT, 4)) = ".EXE" THEN Spx$ = ".mod" ELSE Spx$ = ".sub"
    Fgc% = 15
    Mxg% = 0
    q$ = CHR$(34)
    Skip$ = ""
    Title$ = "DAHSR's Lottery Games Number Statistics"
    Flt% = 0
    CLOSE : OPEN "lottery.ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                
            END SELECT
        END IF
        NEXT
        WEND
    ELSE
        Flt% = 0
    END IF
    SHELL "dir dbse > games.txt /B"
    Flt% = 0
    CLOSE
    OPEN "games.txt" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        IF LEN(i$) > 0 THEN Mxg% = Mxg% + 1
        WEND
    ELSE
        Flt% = 0
        Mxg% = 0
    END IF
    CLOSE
    ValiData
END SUB

SUB Main
    DIM e$, f%, k$, x%
    CLS
    f% = 0
    DO
    IF e$ = "" THEN
        e$ = "s"
        Fgc% = 15
        Bgc% = 1
    ELSEIF e$ = "s" THEN
        Fgc% = 15
        Bgc% = 1
    ELSEIF e$ = "h" THEN
        Fgc% = 15
        Bgc% = 0
    ELSE
        Fgc% = 15
        Bgc% = 1
    END IF
    IF f% < 0 THEN f% = Mxg%
    IF f% > Mxg% THEN f% = 0
    Display e$ + MID$(STR$(f%), 2), k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(8): f% = f% - 1
        CASE CHR$(9)
            IF f% > 0 THEN
                CLOSE : OPEN "games.txt" FOR INPUT AS #1
                FOR x% = 1 TO f%
                INPUT #1, k$
                NEXT
                SHELL "notepad.exe " + "dbse\" + k$
                RUN BOOT
            END IF
        CASE "-", CHR$(0) + "K": f% = f% - 1
        CASE CHR$(0) + "I", CHR$(0) + "Q"
            IF e$ = "" THEN
                e$ = "s"
            ELSEIF e$ = "s" THEN
                e$ = "h"
            ELSEIF e$ = "h" THEN
                e$ = "s"
            ELSE
                e$ = "s"
            END IF
        CASE "+", CHR$(0) + "M": f% = f% + 1
        CASE "0" TO "9"
            IF k$ = "0" THEN k$ = "10"
            f% = f% + VAL(k$)
            IF f% > Mxg% THEN f% = f% - Mxg%
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
    COLOR 0, 0
    FOR x% = 1 TO 23
    SayIt "", x%, 0, 0
    NEXT
    i$ = "ù Scroll(" + CHR$(24) + CHR$(25) + ") Lines "
    i$ = i$ + "ù Scroll(-" + CHR$(27) + CHR$(26) + "+) Displays ù"
    i$ = i$ + " PAGE Toggles Displays ù"
    SayIt i$, 21, 0, 15
    i$ = "ù ENTER to Update Text and html Displays ù"
    SayIt i$, 22, 0, 15
    i$ = "ù TAB to Edit the Selected File ù (P)urge Database ù ESC to End ù"
    SayIt i$, 23, 0, 15
END SUB

SUB MsgBox (i$)
    i$ = LEFT$("º " + STRING$(MXL / 2 - (LEN(i$) / 2), 32) + i$ + STRING$(MXL / 2, 32), MXL - 2) + " º"
    SayIt "É" + STRING$(MXL - 2, 205) + "»", WSZ + 1, 15, 4
    SayIt i$, WSZ + 2, 15, 4
    SayIt "È" + STRING$(MXL - 2, 205) + "¼", WSZ + 3, 15, 4
END SUB

SUB Purge
    DIM f$, i$, k$, t%
    MsgBox "Purge the Database and Save the Most Recent" + STR$(MXD) + " Drawings (Y) ?"
    DO
    Getkey k$
    IF UCASE$(k$) = "Y" THEN
        MsgBox "Standyby. . . Purging the Database!"
        Backup
        CLOSE : OPEN "games.txt" FOR INPUT AS #1
        WHILE NOT EOF(1)
        LINE INPUT #1, f$
        t% = 0
        CLOSE #3: OPEN "dbse\" + f$ FOR INPUT AS #3
        WHILE NOT EOF(3)
        LINE INPUT #3, i$
        t% = t% + 1
        WEND
        CLOSE #2: OPEN "temp.txt" FOR OUTPUT AS #2
        CLOSE #3: OPEN "dbse\" + f$ FOR INPUT AS #3
        LINE INPUT #3, i$
        PRINT #2, i$
        WHILE NOT EOF(3)
        LINE INPUT #3, i$
        t% = t% - 1
        IF t% <= MXD THEN PRINT #2, i$
        WEND
        CLOSE #2
        CLOSE #3
        KILL "dbse\" + f$
        NAME "temp.txt" AS "dbse\" + f$
        WEND
        CLOSE
        EXIT DO
    ELSE
        EXIT DO
    END IF
    LOOP
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(40, 32) + l$, 40)
    r$ = LEFT$(r$ + STRING$(40, 32), 40)
    COLOR f%, b%
    LOCATE v%, 1, 0
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM i$, x%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE ELSE: Dln = ""
    END SELECT
    i$ = LEFT$(Dln, MXL)
    SayIt i$, x%, Fgc%, Bgc%
    NEXT
END SUB

SUB Update
    Flt% = 0
    RUN FORWARD + Spx$
    IF Flt% = 53 THEN
        i$ = "ù Cannot Find " + q$ + FORWARD + Spx$ + q$
    ELSE
        i$ = "ù Error #" + STR$(Flt%)
    END IF
    i$ = i$ + " ù ENTER to Continue ù ESC to End ù"
    MsgBox i$
    DO
    Getkey k$
    LOOP UNTIL k$ = CHR$(13)
    RUN BOOT
END SUB

SUB ValiData
    DIM d$, k$, l$, n$, s$, x%, y%
    Flt% = 0
    CLOSE
    OPEN "games.txt" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, f$
    CLOSE #3: OPEN "dbse\" + f$ FOR INPUT AS #3
    INPUT #3, Ln$, Ty$, Px$, Lo$, Hi$
    IF VAL(Ty$) = 3 THEN INPUT #3, Xp$, Xl$, Xh$
    l$ = "0"
    WHILE NOT EOF(3)
    INPUT #3, d$
    LINE INPUT #3, s$
    l$ = MID$(STR$(VAL(l$) + 1), 2)
    y% = VAL(Px$)
    IF VAL(Ty$) = 3 THEN y% = y% + 1
    s$ = "," + s$
    FOR x% = 1 TO LEN(s$)
    IF MID$(s$, x%, 1) = "," THEN y% = y% - 1
    NEXT
    s$ = MID$(s$, 2)
    IF y% = 0 THEN
        CLOSE #2: OPEN "temp.txt" FOR OUTPUT AS #2: PRINT #2, s$: CLOSE #2
        CLOSE #5: OPEN "temp.txt" FOR INPUT AS #5
        FOR x% = 1 TO VAL(Px$)
        INPUT #5, n$
        SELECT CASE VAL(n$)
            CASE IS < VAL(Lo$): DataFault f$, "Draw Date " + d$ + ", on Line #" + l$ + ", the Number " + n$ + " is Less Than " + Lo$ + "."
            CASE IS > VAL(Hi$): DataFault f$, "Draw Date " + d$ + ", on Line #" + l$ + ", the Number " + n$ + " is Greater Than " + Hi$ + "."
        END SELECT
        NEXT
        IF VAL(Ty$) = 3 THEN
            INPUT #5, n$
            SELECT CASE VAL(n$)
                CASE IS < VAL(Xl$): DataFault f$, q$ + Xp$ + q$ + ": Draw Date " + d$ + ", on Line #" + l$ + ", the Number " + n$ + " is Less Than " + Xl$ + "."
                CASE IS > VAL(Xh$): DataFault f$, q$ + Xp$ + q$ + ": Draw Date " + d$ + ", on Line #" + l$ + ", the Number " + n$ + " is Greater Than " + Xh$ + "."
            END SELECT
        END IF
        CLOSE #5
    ELSE
        DataFault f$, "Draw Date:" + d$ + ": Data Count Error on Line #" + l$ + "."
    END IF
    WEND
    WEND
    CLOSE
END SUB

