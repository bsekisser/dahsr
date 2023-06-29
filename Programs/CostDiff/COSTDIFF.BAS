DECLARE SUB Decant (a$, b$, c$)
DECLARE SUB AddLine (a$, b$, c$, d$)
DECLARE SUB Summarize (a$, b$, c$)
DECLARE SUB Process (r%, k$)
DECLARE SUB Delete (r%)
DECLARE SUB Edit (r%)
DECLARE SUB Display (r%, k$)
DECLARE SUB Purge ()
DECLARE SUB AddHead (a$, b$, f$)
DECLARE SUB Convert (i$)
DECLARE SUB Insert ()
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "costdiff.bas"
    CONST DLR = "$**###.##"
    CONST INI = "costdiff.ini"
    CONST MXL = 76
    CONST PRGM = "costdiff"
    CONST WSZ = 15
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Fbse$
    COMMON SHARED Fgc%
    COMMON SHARED Filebox$
    COMMON SHARED Flt%
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Title$
    COMMON SHARED Today$
    COMMON SHARED Trc$
    DIM SHARED Pl(4) AS INTEGER, Mth(12) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Purge
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
Months:
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"

SUB AddHead (a$, b$, f$)
    PRINT #2, LEFT$(STRING$(MXL / 2 - (LEN(f$) / 2), 32) + f$ + STRING$(MXL, 32), MXL)
    PRINT #2, STRING$(MXL, 196)
    IF LEFT$(a$, 1) = "*" THEN
        a$ = MID$(a$, 2)
    END IF
    PRINT #2, LEFT$("Items" + STRING$(MXL, 46), Pl(1));
    PRINT #2, RIGHT$(STRING$(MXL, 46) + a$, Pl(2));
    PRINT #2, RIGHT$(STRING$(MXL, 46) + b$, Pl(3));
    PRINT #2, RIGHT$(STRING$(MXL, 46) + "Difference", Pl(4))
    PRINT #2, STRING$(MXL, 45)
END SUB

SUB AddLine (a$, b$, c$, d$)
    PRINT #2, LEFT$(a$ + STRING$(MXL, 46), Pl(1));
    PRINT #2, RIGHT$(STRING$(MXL, 46) + "$" + b$, Pl(2));
    PRINT #2, RIGHT$(STRING$(MXL, 46) + "$" + c$, Pl(3));
    PRINT #2, RIGHT$(STRING$(MXL, 46) + "$" + d$, Pl(4))
END SUB

SUB Bufferize
    DIM a$, b$, c$, d$, f$, i$, t$, u$, v$, w$, r$
    r$ = "0"
    Flt% = 0
    CLOSE : OPEN Fbse$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, f$
        CLOSE #3: OPEN Dbse$ + f$ FOR INPUT AS #3
        IF Flt% = 0 THEN
            r$ = MID$(STR$(VAL(r$) + 1), 2)
            t$ = "0": u$ = "0": v$ = "0": w$ = "0"
            CLOSE #2: OPEN "text\r" + r$ + ".txt" FOR OUTPUT AS #2
            WHILE NOT EOF(3)
            Decant a$, b$, c$
            SELECT CASE UCASE$(LEFT$(a$, 1))
                CASE "*"
                    AddHead a$, b$, LEFT$("Record #" + r$ + " of " + Trc$ + STRING$(MXL, 32), MXL - LEN(f$) - 2) + q + f$ + q
                CASE "-"
                    PRINT #2, STRING$(MXL, 45)
                    Summarize "SubTotals", t$, u$
                    PRINT #2, STRING$(MXL, 196)
                    t$ = "0": u$ = "0"
                CASE "="
                    PRINT #2, STRING$(MXL, 45)
                    Summarize "SubTotals", t$, u$
                    PRINT #2, STRING$(MXL, 196)
                    Summarize "Totals", v$, w$
                    PRINT #2, STRING$(MXL, 205)
                    t$ = "0": u$ = "0": v$ = "0": w$ = "0"
                CASE "0" TO "9", "A" TO "Z"
                    t$ = STR$(VAL(t$) + VAL(b$))
                    u$ = STR$(VAL(u$) + VAL(c$))
                    v$ = STR$(VAL(v$) + VAL(b$))
                    w$ = STR$(VAL(w$) + VAL(c$))
                    IF VAL(b$) > VAL(c$) THEN
                        d$ = STR$(VAL(b$) - VAL(c$))
                    ELSEIF VAL(b$) < VAL(c$) THEN
                        d$ = STR$(VAL(c$) - VAL(b$))
                    ELSE
                        d$ = "0"
                    END IF
                    Convert b$
                    Convert c$
                    Convert d$
                    AddLine a$, b$, c$, d$
                END SELECT
            WEND
        ELSE
            Flt% = 0
        END IF
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE
END SUB

SUB Convert (i$)
    DIM c$, d$, v AS DOUBLE
    v = VAL(i$) * 100
    IF v - INT(v) < .5 THEN v = v ELSE v = v + 1
    i$ = MID$(STR$(INT(v)), 2)
    c$ = "." + RIGHT$("00" + i$, 2)
    v = INT(v / 100)
    d$ = STR$(v)
    i$ = d$ + c$
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Decant (a$, b$, c$)
    LINE INPUT #3, i$
    CLOSE #9: OPEN "temp.txt" FOR OUTPUT AS #9: PRINT #9, i$
    CLOSE #9: OPEN "temp.txt" FOR INPUT AS #9: INPUT #9, a$, b$, c$
    CLOSE #9: KILL "temp.txt"
    IF Flt% = 0 THEN Flt% = 0 ELSE Flt% = 0
END SUB

SUB Delete (r%)
    DIM i$, x%
    CLOSE : OPEN Fbse$ FOR INPUT AS #1
    FOR x% = 1 TO r%
    LINE INPUT #1, i$
    NEXT
    SayIt "Delete " + i$ + " (Y/N) ?", Echo%, 14, Bgc%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE "Y": KILL Dbse$ + i$: EXIT DO
        CASE "N": EXIT DO
    END SELECT
    LOOP
    RUN BOOT
END SUB

SUB Display (r%, k$)
    DIM e%, i$, p%, s%
    Mask
    CLOSE : OPEN "text\r" + MID$(STR$(r%), 2) + ".txt" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    p% = 1
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(13): EXIT SUB
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "K": EXIT SUB
        CASE CHR$(0) + "M": EXIT SUB
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE ELSE: Process r%, k$: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Edit (r%)
    DIM i$, x%
    CLOSE : OPEN Fbse$ FOR INPUT AS #1
    FOR x% = 1 TO r%
    LINE INPUT #1, i$
    NEXT
    SHELL "notepad.exe " + Dbse$ + i$
    RUN BOOT
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
    MKDIR "dbse"
    MKDIR "text"
    RESTORE Months
    FOR x% = 1 TO 12: READ Mth(x%): NEXT
    Bgc% = 0
    Dbse$ = "dbse\"
    Echo% = WSZ + 5
    Fbse$ = PRGM + ".txt"
    Fgc% = 15
    Filebox$ = "filebox\filebox" + LCASE$(RIGHT$(BOOT, 4))
    x% = 2
    Pl(1) = 76 - ((LEN(DLR) * x%) * 3)
    Pl(2) = LEN(DLR) * x%
    Pl(3) = LEN(DLR) * x%
    Pl(4) = LEN(DLR) * x%
    q = CHR$(34)
    Title$ = "The Cost Differences of Various Items Between Two Entities"
    Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
    Trc$ = "0"
    Flt% = 0: CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(LEFT$(i$, x%))
        END SELECT
        NEXT
        WEND
    ELSE
        Flt% = 0
        CLOSE : OPEN INI FOR OUTPUT AS #1
    END IF
    CLOSE
    SHELL "dir dbse > " + Fbse$ + " /B"
    OPEN Fbse$ FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    IF i$ = "" THEN
        i$ = ""
    ELSE
        Trc$ = STR$(VAL(Trc$) + 1)
    END IF
    WEND
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
            Fgc% = 14
            EXIT FOR
    END SELECT
    NEXT
END SUB

SUB Insert
    DIM f$, i$, x%
    f$ = "untitled.txt"
    DO
    Flt% = 0
    CLOSE : OPEN Dbse$ + f$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        CLOSE : OPEN Dbse$ + f$ FOR OUTPUT AS #2
        PRINT #2, "Entity #1,Entity #2"
        FOR x% = 1 TO 9
        PRINT #2, "Item #"; STR$(x%); ",0,0"
        NEXT
        PRINT #2, "="
    END IF
    LOOP
    RUN BOOT
END SUB

SUB Main
    DIM k$, r%
    r% = 1
    DO
    IF r% < 1 THEN
        r% = VAL(Trc$)
    ELSEIF r% > VAL(Trc$) THEN
        r% = 1
    END IF
    Display r%, k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(0) + "K": r% = r% - 1
        CASE CHR$(0) + "M": r% = r% + 1
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
    i$ = i$ + "Scroll (" + CHR$(24) + CHR$(25) + ") Lines"
    i$ = i$ + " ù Scroll (" + CHR$(27) + CHR$(26) + ") Files"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = ""
    i$ = i$ + "TAB: Edit This File"
    i$ = i$ + " ù DELETE This File"
    i$ = i$ + " ù INSERT a New File"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = "ESC to End"
    SayIt i$, x%, Fgc%, Bgc%
END SUB

SUB Process (r%, k$)
    SELECT CASE k$
        CASE CHR$(0) + "R": Insert
        CASE ELSE
            DO
            IF r% < 1 OR r% > VAL(Trc$) THEN
                EXIT DO
            ELSE
                SELECT CASE k$
                    CASE CHR$(9): Edit r%
                    CASE CHR$(0) + "S": Delete r%
                END SELECT
            END IF
            LOOP
    END SELECT
END SUB

SUB Purge
    DIM i$
    SHELL "dir text > temp.txt /B"
    CLOSE : OPEN "temp.txt" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z": KILL "text\" + i$
    END SELECT
    WEND
    CLOSE
    KILL "temp.txt"
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

SUB Scroll (s%, e%)
    DIM b%, f%, i$, m$, o$, p%, r%, t%, x%, y%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE ELSE: Dln = STRING$(MXL, 32)
    END SELECT
    i$ = LEFT$(Dln, MXL)
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE " ": f% = 11: b% = Bgc%
        CASE "0" TO "9", "A" TO "Z": f% = 10: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    SayIt i$, x% + 3, f%, b%
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z"
            j$ = "": k$ = "": l$ = ""
            FOR y% = 1 TO LEN(i$)
            SELECT CASE UCASE$(MID$(i$, y%, 1))
                CASE "0" TO "9"
                    COLOR 10, Bgc%
                    LOCATE x% + 3, y% + 2
                    PRINT MID$(i$, y%, 1)
                CASE "A" TO "Z"
                    COLOR 11, Bgc%
                    LOCATE x% + 3, y% + 2
                    PRINT MID$(i$, y%, 1)
                CASE ELSE: COLOR Fgc%, Bgc%
            END SELECT
            IF MID$(i$, y%, 1) = "$" THEN
                IF j$ = "" THEN
                    j$ = STR$(VAL(MID$(i$, y% + 1)))
                    p% = y% + 2
                ELSEIF k$ = "" THEN
                    k$ = STR$(VAL(MID$(i$, y% + 1)))
                    r% = y% + 2
                ELSEIF l$ = "" THEN
                    l$ = STR$(VAL(MID$(i$, y% + 1)))
                    t% = y% + 2
                END IF
            END IF
            NEXT
            IF j$ = "" THEN
                j$ = ""
            ELSE
                Convert j$
                LOCATE x% + 3, p%
                IF VAL(j$) > VAL(k$) THEN
                    COLOR 14, Bgc%
                ELSE
                    COLOR 10, Bgc%
                END IF
                PRINT "$"; j$
            END IF
            IF k$ = "" THEN
                k$ = ""
            ELSE
                Convert k$
                LOCATE x% + 3, r%
                IF VAL(j$) < VAL(k$) THEN
                    COLOR 14, Bgc%
                ELSE
                    COLOR 10, Bgc%
                END IF
                PRINT "$"; k$
            END IF
            IF l$ = "" THEN
                l$ = ""
            ELSE
                Convert l$
                LOCATE x% + 3, t%
                IF VAL(l$) > 0 THEN
                    COLOR 13, Bgc%
                ELSE
                    COLOR 10
                END IF
                PRINT "$"; l$
            END IF
    END SELECT
    NEXT
    COLOR Fgc%, Bgc%
END SUB

SUB Summarize (a$, b$, c$)
    DIM d$
    IF VAL(b$) > VAL(c$) THEN
        d$ = STR$(VAL(b$) - VAL(c$))
    ELSEIF VAL(b$) < VAL(c$) THEN
        d$ = STR$(VAL(c$) - VAL(b$))
    ELSE
        d$ = "0"
    END IF
    Convert b$
    Convert c$
    Convert d$
    AddLine a$, b$, c$, d$
END SUB

