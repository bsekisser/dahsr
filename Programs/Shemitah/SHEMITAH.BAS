DECLARE SUB FindStart (s%, e%)
DECLARE SUB Htmlize ()
DECLARE SUB Table (a$, b$, p$, s$, w$)
DECLARE SUB Td (a$, c$, s$, t$, i$)
DECLARE SUB Header ()
DECLARE SUB Footer ()
DECLARE SUB Mask ()
DECLARE SUB Getkey (k$)
DECLARE SUB Initialize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Bufferize ()
DECLARE SUB Main ()
TYPE dahsr
    op AS STRING * 1
    yb AS STRING * 7
    in AS STRING * 60
    ye AS STRING * 7
END TYPE
    CONST BFR = "shemitah.bfr"
    CONST BOOT = "shemitah.bas"
    CONST HTM = "shemitah.htm"
    CONST INI = "shemitah.ini"
    CONST MXL = 75
    CONST PRGM = "shemitah"
    CONST TCM = "157015"
    CONST TXT = "shemitah.txt"
    CONST WSZ = 15
    COMMON SHARED Bgc%
    COMMON SHARED Echo%
    COMMON SHARED Fgc%
    COMMON SHARED Finale%
    COMMON SHARED Flt%
    COMMON SHARED Jst%
    COMMON SHARED Sst%
    COMMON SHARED Origin%
    COMMON SHARED q$
    DIM SHARED Shem AS dahsr
    ON ERROR GOTO Trap
    Initialize
    Mask
    Bufferize
    Htmlize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Bufferize
    DIM a%, h$, i$, j$, x%, y%, z%
    Flt% = 0
    CLOSE #1: OPEN TXT FOR INPUT AS #1
    OPEN BFR FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        x% = Finale%
        y% = Sst%
        z% = Jst%
        DO UNTIL x% < Origin%
        IF x% = 0 THEN x% = x% - 1
        i$ = RIGHT$("0000" + MID$(STR$(x%), 2), 4)
        j$ = RIGHT$("0000" + MID$(STR$(x% + 1), 2), 4)
        IF x% < 0 THEN
            IF x% = -1 THEN
                i$ = i$ + " BC"
                j$ = j$ + " AD"
            ELSE
                i$ = i$ + " BC"
                j$ = j$ + " BC"
            END IF
        ELSE
            i$ = i$ + " AD"
            j$ = j$ + " AD"
        END IF
        Shem.yb = i$
        Shem.ye = j$
        Shem.op = "-"
        y% = y% - 1: IF y% < 1 THEN y% = 7: Shem.op = "s"
        z% = z% - 1: IF z% < 1 THEN z% = 49: Shem.op = "j"
        i$ = ""
        CLOSE #1: OPEN TXT FOR INPUT AS #1
        WHILE NOT EOF(1)
        LINE INPUT #1, h$
        IF MID$(h$, 5, 1) = " " THEN h$ = h$ ELSE h$ = LEFT$(h$, 4) + " " + MID$(h$, 5)
        IF UCASE$(Shem.yb) = UCASE$(LEFT$(h$, LEN(Shem.yb))) THEN
            i$ = i$ + MID$(h$, 9) + ", "
        END IF
        WEND
        IF RIGHT$(i$, 2) = ", " THEN i$ = LEFT$(i$, LEN(i$) - 2)
        DO WHILE w% < LEN(i$)
        FOR w% = 1 TO LEN(i$)
        IF MID$(i$, w%, 1) = "," AND MID$(i$, w% + 1, 1) <> " " THEN
            i$ = LEFT$(i$, w%) + " " + MID$(i$, w% + 1)
            EXIT FOR
        END IF
        NEXT
        LOOP
        SELECT CASE Shem.op
            CASE "s": Shem.in = LEFT$("Shemitah." + i$ + STRING$(MXL, 46), MXL)
            CASE "j": Shem.in = LEFT$("Jubilee." + i$ + STRING$(MXL, 46), MXL)
            CASE ELSE: Shem.in = LEFT$(i$ + STRING$(MXL, 46), MXL)
        END SELECT
        PRINT #2, Shem.op; Shem.yb; Shem.in; Shem.ye
        x% = x% - 1
        LOOP
    ELSE
        Flt% = 0
    END IF
    CLOSE
END SUB

SUB FindStart (s%, e%)
    DIM x%
    FOR x% = 1 TO e%
    GET #1, x%, Shem
    IF LEFT$(Shem.yb, 4) = RIGHT$(DATE$, 4) THEN
        IF UCASE$(RIGHT$(Shem.yb, 2)) = "AD" THEN
            s% = x% - (WSZ / 2)
            EXIT FOR
        END IF
    END IF
    NEXT
END SUB

SUB Footer
    PRINT #2, "</body>"
    PRINT #2, "</html>"
END SUB

SUB Getkey (k$)
    DO
    k$ = INKEY$
    LOOP UNTIL k$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF k$ = "*" THEN
        RUN BOOT
    ELSEIF k$ = CHR$(27) THEN
        END
    END IF
END SUB

SUB Header
    PRINT #2, "<!doctype html>"
    PRINT #2, "<html lang="; q$; "en"; q$; ">"
    PRINT #2, "<head>"
    PRINT #2, "<meta charset="; q$; "utf-8"; q$; ">"
    PRINT #2, "<title>Shemitah</title>"
    PRINT #2, "<style type="; q$; "text/css"; q$; ">"
    PRINT #2, "BODY"
    PRINT #2, "     {"
    PRINT #2, "     font-family:georgia,"; q$; "times new roman"; q$; ",trebuchet,serif;"
    PRINT #2, "     font-style:normal;"
    PRINT #2, "     font-weight:bold;"
    PRINT #2, "     }"
    PRINT #2, " .mhdr {color:#fff; background:#00f; font-size:20px;}"
    PRINT #2, " .mtxt {color:#fff; background:#00f; font-size:18px;}"
    PRINT #2, " .stxt {color:#00f; font-size:16px;}"
    PRINT #2, " .text {color:#000; font-size:14px;}"
    PRINT #2, " .shem {color:#fff; background:#f00; font-size:16px;}"
    PRINT #2, " .sbkg {color:#000; background:#fd0; font-size:14px;}"
    PRINT #2, " .jubi {color:#fff; background:#00f; font-size:16px;}"
    PRINT #2, " .jbkg {color:#000; background:#0f0; font-size:14px;}"
    PRINT #2, "</style>"
    PRINT #2, "</head>"
    PRINT #2, "<body>"
    Table "", "1", "3", "", ""
    Td "<", "mtxt", LEFT$(TCM, 2), "<", "The Period<br>Begins in<br>September<br>(Elul 29)"
    Td "", "mhdr", MID$(TCM, 3, 2), "", "Shemitah Periods from " + MID$(STR$(Origin%), 2) + " BC to " + MID$(STR$(Finale%), 2) + " AD"
    Td ">", "mtxt", RIGHT$(TCM, 2), ">", "The Period<br>Ends in<br>September<br>(Elul 28)"
    PRINT #2, "</table>"
END SUB

SUB Htmlize
    DIM c$, d$, h$, i$, j$, k$, w%, x%, y%, z%
    x% = Finale%
    y% = Sst%
    z% = Jst%
    CLOSE
    OPEN HTM FOR OUTPUT AS #2
    Header
    Table "", "1", "3", "", ""
    DO UNTIL x% < Origin%
    IF x% = 0 THEN x% = x% - 1
    i$ = RIGHT$("0000" + MID$(STR$(x%), 2), 4)
    j$ = RIGHT$("0000" + MID$(STR$(x% + 1), 2), 4)
    IF x% < 0 THEN
        IF x% = -1 THEN
            i$ = i$ + " BC"
            j$ = j$ + " AD"
        ELSE
            i$ = i$ + " BC"
            j$ = j$ + " BC"
        END IF
    ELSE
        i$ = i$ + " AD"
        j$ = j$ + " AD"
    END IF
    o$ = "-"
    y% = y% - 1: IF y% < 1 THEN y% = 7: o$ = "s"
    z% = z% - 1: IF z% < 1 THEN z% = 49: o$ = "j"
    CLOSE #1: OPEN TXT FOR INPUT AS #1
    k$ = "&nbsp;"
    WHILE NOT EOF(1)
    LINE INPUT #1, h$
    IF MID$(h$, 5, 1) = " " THEN h$ = h$ ELSE h$ = LEFT$(h$, 4) + " " + MID$(h$, 5)
    IF UCASE$(i$) = UCASE$(LEFT$(h$, LEN(i$))) THEN
        IF k$ = "&nbsp;" THEN
            k$ = MID$(h$, 9) + ", "
        ELSE
            k$ = k$ + MID$(h$, 9) + ", "
        END IF
    END IF
    WEND
    IF o$ = "s" THEN
        IF k$ = "&nbsp;" THEN
            k$ = "Shemitah, "
        ELSE
            k$ = k$ + "Shemitah, "
        END IF
    ELSEIF o$ = "j" THEN
        IF k$ = "&nbsp;" THEN
            k$ = "Jubilee, "
        ELSE
            k$ = k$ + "Jubilee, "
        END IF
    END IF
    IF k$ = "&nbsp;" THEN
        k$ = k$
    ELSE
        IF RIGHT$(k$, 2) = ", " THEN k$ = LEFT$(k$, LEN(k$) - 2)
        IF RIGHT$(k$, 1) = "." THEN k$ = k$ ELSE k$ = k$ + "."
        DO WHILE w% < LEN(k$)
        FOR w% = 1 TO LEN(k$)
        IF MID$(k$, w%, 1) = "," AND MID$(k$, w% + 1, 1) <> " " THEN
            k$ = LEFT$(k$, w%) + " " + MID$(k$, w% + 1)
            EXIT FOR
        END IF
        NEXT
        LOOP
    END IF
    SELECT CASE o$
        CASE "s": c$ = "shem": d$ = "sbkg"
        CASE "j": c$ = "jubi": d$ = "jbkg"
        CASE ELSE: c$ = "stxt": d$ = "text"
    END SELECT
    Td "<", c$, LEFT$(TCM, 2), "<", i$
    Td "<", d$, MID$(TCM, 3, 2), "", k$
    Td ">", c$, RIGHT$(TCM, 2), ">", j$
    x% = x% - 1
    LOOP
    PRINT #2, "</table>"
    Footer
    CLOSE
END SUB

SUB Initialize
    Bgc% = 1
    Echo% = WSZ + 5
    Fgc% = 15
    Finale% = 4004
    Jst% = 30
    Sst% = 3
    Origin% = -4004
    q$ = CHR$(34)
END SUB

SUB Main
    DIM e%, s%, t$, k$, x%
    t$ = RIGHT$(DATE$, 4)
    Mask
    CLOSE : OPEN BFR FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    FindStart s%, e%
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% - WSZ + 1 THEN s% = e% - WSZ + 1
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT
        CASE CHR$(13): FindStart s%, e%: EXIT DO
        CASE CHR$(0) + "G": IF s% > 1 THEN s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "I": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "O": IF s% < e% THEN s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = s% + WSZ - 1: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
    STATIC o$
    IF o$ = "" THEN
        o$ = "+"
        CLS
    ELSE
        o$ = "-"
        LOCATE 1, 1, 0
    END IF
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    IF o$ = "+" THEN
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
    ELSE
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3: PRINT "Ç"; STRING$(LEN(Shem.op), 196); STRING$(LEN(Shem.yb), 196); "Â"; STRING$(LEN(Shem.in), 196); "Â"; STRING$(LEN(Shem.ye), 196); "Ä¶"
            CASE 4 TO WSZ + 3: PRINT "º "; STRING$(LEN(Shem.op), 32); STRING$(LEN(Shem.yb), 196); "³"; STRING$(LEN(Shem.in), 196); "³"; STRING$(LEN(Shem.ye), 32); "º"
            CASE WSZ + 4: PRINT "Ç"; STRING$(LEN(Shem.op), 196); STRING$(LEN(Shem.yb), 196); "Á"; STRING$(LEN(Shem.in), 196); "Á"; STRING$(LEN(Shem.ye), 196); "Ä¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
    END IF
    NEXT
    SayIt "Shemitahs & Jubilees from " + MID$(STR$(Origin%), 2) + " BC to " + MID$(STR$(Finale%), 2) + " AD", 2, 11, Bgc%
    IF o$ = "+" THEN
        FOR x% = 2 TO 22
        SayIt "Standby, Loading Data. . .", x%, 11, Bgc%
        NEXT
    ELSE
        LOCATE 2, 3: PRINT "From September"
        LOCATE 2, 67: PRINT "To September"
        SayIt "ù Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Years ù PAGE Years ù", Echo%, Fgc%, Bgc%
        SayIt "ù ENTER Present Year ù HOME: Origin Year ù END: Finale Year ù", Echo% + 1, Fgc%, Bgc%
        SayIt "ù TAB: Edit Text File ù ESC to End ù", Echo% + 2, Fgc%, Bgc%
    END IF
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$, w%
    w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 22
    LOCATE v%, 3
    COLOR f%, b%
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, j$, k$, m$, o$, x%, y%
    m$ = CHR$(179)
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Shem
        CASE e% + 1
            Shem.op = STRING$(MXL, 205)
            Shem.yb = STRING$(MXL, 205)
            Shem.in = STRING$(MXL, 205)
            Shem.ye = STRING$(MXL, 205)
        CASE ELSE
        CASE e% + 1
            Shem.op = STRING$(MXL, 32)
            Shem.yb = STRING$(MXL, 32)
            Shem.in = STRING$(MXL, 32)
            Shem.ye = STRING$(MXL, 32)
    END SELECT
    LOCATE x% + 3, 3
    SELECT CASE LCASE$(Shem.op)
        CASE "s": f% = 15: b% = 13
        CASE "j": f% = 31: b% = 13
        CASE ELSE: f% = 10: b% = Bgc%
    END SELECT
    COLOR f%, b%
    PRINT Shem.yb;
    COLOR Fgc%, Bgc%
    PRINT m$; Shem.in; m$;
    COLOR f%, b%
    PRINT Shem.ye
    NEXT
    COLOR Fgc%, Bgc%
END SUB

SUB Table (a$, b$, p$, s$, w$)
    IF a$ = "" THEN a$ = "center"
    IF b$ = "" THEN b$ = "0"
    IF p$ = "" THEN p$ = "0"
    IF s$ = "" THEN s$ = "0"
    IF w$ = "" THEN w$ = "100"
    PRINT #2, "<table";
    PRINT #2, " align="; q$; a$; q$;
    PRINT #2, " border="; q$; b$; q$;
    PRINT #2, " cellpadding="; q$; p$; q$;
    PRINT #2, " cellspacing="; q$; s$; q$;
    PRINT #2, " width="; q$; w$; "%"; q$; ">"
END SUB

SUB Td (a$, c$, s$, t$, i$)
    IF a$ = "" THEN a$ = "center"
    IF a$ = "<" THEN a$ = "left"
    IF a$ = ">" THEN a$ = "right"
    IF c$ = "" THEN c$ = "text"
    IF s$ = "" THEN s$ = "100"
    IF LEFT$(t$, 1) = "<" THEN PRINT #2, "<tr>";
    PRINT #2, "<td";
    PRINT #2, " align="; q$; a$; q$;
    PRINT #2, " class="; q$; c$; q$;
    PRINT #2, " colspan="; q$; s$; q$;
    PRINT #2, " width="; q$; s$; "%"; q$; ">"
    IF LEN(i$) > 0 THEN
        PRINT #2, i$; "</td>";
        IF RIGHT$(t$, 1) = ">" THEN
            PRINT #2, "</tr>"
        ELSE
            PRINT #2, ""
        END IF
    END IF
END SUB

