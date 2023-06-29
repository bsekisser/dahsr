DECLARE SUB Display (f$)
DECLARE SUB Search (s%, e%)
DECLARE SUB AddLine (o$, i$, j$)
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Bufferize ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "bckpasst.bas", MXL = 77
    CONST PRGM = "bckpasst", WSZ = 15
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED MyFile$
    COMMON SHARED q$
    COMMON SHARED Src$
    COMMON SHARED Title$
    COMMON SHARED Today$
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB AddLine (o$, i$, j$)
    DIM l$
    IF o$ = "*" THEN
        l$ = LEFT$(o$ + i$ + " " + STRING$(MXL, 249), MXL - LEN(j$) - 1) + " " + j$
    ELSEIF i$ = "=" AND j$ = "=" THEN
        l$ = STRING$(MXL, 205)
    ELSEIF i$ = "-" AND j$ = "-" THEN
        l$ = STRING$(MXL, 196)
    ELSEIF i$ = "-" AND j$ = " " OR i$ = " " AND j$ = "-" THEN
        l$ = " " + STRING$(MXL - 1, 45)
    ELSEIF LEN(i$) > 0 AND j$ = "" THEN
        l$ = LEFT$(o$ + STRING$(MXL / 2 - (LEN(i$) / 2), 32) + i$ + STRING$(MXL, 32), MXL)
    ELSEIF LEN(j$) > 0 AND i$ = "" THEN
        l$ = LEFT$(o$ + STRING$(MXL / 2 - (LEN(j$) / 2), 32) + j$ + STRING$(MXL, 32), MXL)
    ELSEIF LEN(i$) > 0 AND j$ = "." THEN
        l$ = LEFT$(o$ + i$ + STRING$(MXL, 46), MXL)
    ELSEIF LEN(j$) > 0 AND i$ = "." THEN
        l$ = RIGHT$(o$ + STRING$(MXL, 46) + j$, MXL)
    ELSE
        l$ = LEFT$(o$ + i$ + STRING$(MXL, 46), MXL - LEN(j$)) + j$
    END IF
    PRINT #2, l$
END SUB

SUB Bufferize
    DIM a$, b$, i$, j$, o$, t1$, t2$, x%
    Flt% = 0
    CLOSE
    OPEN Dbse$ FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "MAIN=", "MAIN:": a$ = MID$(i$, x% + 1): EXIT FOR
                CASE "BACKUP=", "BACKUP:": b$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        END IF
        NEXT
        IF LEN(a$) > 0 AND LEN(b$) > 0 THEN
            t1$ = "0": t2$ = "0"
            CLOSE #3: OPEN a$ FOR INPUT AS #3
            IF Flt% = 0 THEN
                WHILE NOT EOF(3)
                LINE INPUT #3, i$
                IF LEN(i$) > 0 THEN t1$ = MID$(STR$(VAL(t1$) + 1), 2)
                WEND
                CLOSE #3: OPEN b$ FOR INPUT AS #3
                IF Flt% = 0 THEN
                    o$ = "+"
                    WHILE NOT EOF(3)
                    LINE INPUT #3, i$
                    IF LEN(i$) > 0 THEN t2$ = MID$(STR$(VAL(t2$) + 1), 2)
                    WEND
                ELSE
                    Flt% = 0
                    o$ = ">"
                END IF
            ELSE
                Flt% = 0
                o$ = "<"
            END IF
            AddLine "*", a$, b$
            AddLine o$, "Files:" + t1$, "Files:" + t2$
            AddLine o$, "-", "-"
            SELECT CASE VAL(t1$) - VAL(t2$)
                CASE 0: AddLine o$, "No Backup Necessary", ""
                CASE ELSE
                    IF o$ = "<" THEN
                        AddLine o$, "Cannot Find Main Directory!", ""
                    ELSEIF o$ = ">" THEN
                        AddLine o$, "Cannot Find Backup Directory!", ""
                    ELSEIF VAL(t1$) > VAL(t2$) THEN
                        AddLine o$, "Main Directory Files Not in Backup Directory", ""
                        AddLine o$, "-", " "
                        CLOSE #3: OPEN a$ FOR INPUT AS #3
                        WHILE NOT EOF(3)
                        LINE INPUT #3, i$
                        DO
                        CLOSE #4: OPEN b$ FOR INPUT AS #4
                        WHILE NOT EOF(4)
                        LINE INPUT #4, j$
                        IF i$ = j$ THEN EXIT DO
                        WEND
                        AddLine o$, i$, "."
                        EXIT DO
                        LOOP
                        WEND
                    ELSEIF VAL(t1$) < VAL(t2$) THEN
                        SWAP a$, b$
                        AddLine o$, "Backup Directory Files Not in Main Directory", ""
                        AddLine o$, "-", " "
                        CLOSE #3: OPEN a$ FOR INPUT AS #3
                        WHILE NOT EOF(3)
                        LINE INPUT #3, i$
                        DO
                        CLOSE #4: OPEN b$ FOR INPUT AS #4
                        WHILE NOT EOF(4)
                        LINE INPUT #4, j$
                        IF i$ = j$ THEN EXIT DO
                        WEND
                        AddLine o$, i$, "."
                        EXIT DO
                        LOOP
                        WEND
                    END IF
            END SELECT
            IF NOT EOF(1) THEN AddLine o$, "=", "="
            a$ = "": b$ = ""
        END IF
        WEND
    ELSE
        FOR x% = 1 TO WSZ
        SELECT CASE x%
            CASE 3: AddLine "!", "O o p s !", ""
            CASE 5: AddLine "!", "I Cannot Find the Database Directory Named:", ""
            CASE 6: AddLine "!", "" + q$ + Dbse$ + q$, ""
            CASE 8: AddLine "!", "You Need to Create and Populate", ""
            CASE 9: AddLine "!", "a Database Directory!", ""
            CASE 11: AddLine "!", "NOTE: Pressing TAB Will Create the File", ""
            CASE 12: AddLine "!", "and Then Data Can Be Entered!", ""
            CASE ELSE: AddLine "!", "" + STRING$(MXL, 32), ""
        END SELECT
        NEXT
    END IF
    CLOSE
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Display (f$)
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
    DIM f$, i$, x%
    Bgc% = 1
    Dbse$ = PRGM + ".txt"
    Echo% = WSZ + 5
    Fgc% = 15
    Flt% = 0
    MyFile$ = LCASE$("myfiles\myfiles" + RIGHT$(BOOT, 4))
    q$ = CHR$(34)
    Src$ = ""
    Title$ = "Directory Backup Assistant"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 2)
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "BGC=": Bgc% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "FGC=": Fgc% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "TITLE=": Title$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
    END IF
    CLOSE
    SHELL "dir > src.shl"
    OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "src.shl"
    Src$ = LCASE$(MID$(i$, 15)) + "\"
    MyFile$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM) - 1) + MyFile$
END SUB

SUB Main
    DIM e%, k$, s%
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(8)
            CHDIR LEFT$(MyFile$, LEN(MyFile$) - 12)
            RUN MyFile$
            CHDIR LEFT$(Src$, LEN(Src$) - 1)
            RUN BOOT
        CASE CHR$(9): SHELL "notepad.exe " + Dbse$: RUN BOOT
        CASE CHR$(13): Search s%, e%: EXIT DO
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "I": EXIT SUB
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "Q": EXIT SUB
        CASE "*": RUN BOOT
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
    STATIC o%
    IF o% = 0 THEN
        CLS
        o% = 1
    ELSE
        LOCATE 1, 1, 0
    END IF
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, Echo% - 1: PRINT "Ì"; STRING$(78, 205); "¹"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$, 2, 11, Bgc%
    x% = Echo%
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Lines ù PAGE UP/DOWN " + MID$(STR$(WSZ - 1), 2) + " Lines ù", x%, Fgc%, Bgc%
    x% = x% + 1
    SayIt "ù Press TAB to View the Directory Backup List ù", x%, Fgc%, Bgc%
    x% = x% + 1
    SayIt "ù BACKSPACE to Exit ù ESC to End ù", x%, Fgc%, Bgc%
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
    LOCATE v%, h%
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, o$, x%
    m$ = CHR$(28)
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%
            GET #1, s% + x% - 1, Dln
        CASE e% + 1
            Dln = STRING$(MXL, 205)
        CASE ELSE
            Dln = STRING$(MXL, 32)
    END SELECT
    o$ = LEFT$(Dln, 1)
    i$ = MID$(Dln, 2)
    SELECT CASE o$
        CASE "*": f% = 11: b% = Bgc%
        CASE "+": f% = 14: b% = Bgc%
        CASE "-": f% = 11: b% = Bgc%
        CASE "=": f% = 10: b% = Bgc%
        CASE ">": f% = 15: b% = 12
        CASE "<": f% = 15: b% = 12
        CASE ":": f% = 10: b% = Bgc%
        CASE "!": f% = 15: b% = 10
        CASE "?": f% = 15: b% = 12
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    SayIt i$, x% + 3, f%, b%
    NEXT
END SUB

SUB Search (s%, e%)
    DIM x%
    x% = s%
    DO
    x% = x% + 1
    IF x% > e% THEN x% = 1
    IF x% = s% THEN EXIT DO
    GET #1, x%, Dln
    IF LEFT$(Dln, 1) = "*" THEN EXIT DO
    LOOP
    s% = x%
END SUB

