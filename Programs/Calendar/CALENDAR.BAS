DECLARE SUB Td (a$, c$, h$, i$, v$, l$)
DECLARE SUB CreateMenu ()
DECLARE SUB Purge ()
DECLARE SUB CreateCalendar ()
DECLARE SUB Footer (i$)
DECLARE SUB Header (t$)
DECLARE SUB Generate ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
DECLARE SUB Events (d$, l$)
DECLARE SUB CreateIndex ()
    CONST BOOT = "calendar.bas"
    CONST INI = "calendar.ini"
    CONST PRGM = "calendar"
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dow%
    COMMON SHARED Family$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED FSize%
    COMMON SHARED Hgt$
    COMMON SHARED Html$
    COMMON SHARED Index$
    COMMON SHARED Path$
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Temp$
    COMMON SHARED Title$
    COMMON SHARED Today$
    COMMON SHARED Year$
    DIM SHARED Httl(1, 1) AS STRING, Mth$(1, 13), Wkd$(7)
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
WeekDays:
DATA "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
Months:
DATA "December",31,"January",31,"February",28,"March",31,"April",30,"May",31,"June",30
DATA "July",31,"August",31,"September",30,"October",31,"November",30,"December",31,"January",31

SUB CheckData
    DIM c$, i%, l$, x%
    l$ = "0"
    CLOSE : OPEN Dbse$ FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    l$ = LTRIM$(STR$(VAL(l$) + 1))
    c$ = "0"
    FOR x% = 1 TO LEN(i$)
    IF MID$(i$, x%, 1) = "," THEN c$ = LTRIM$(STR$(VAL(c$) + 1))
    NEXT
    IF VAL(c$) < 2 OR VAL(c$) > 3 THEN
        SayIt i$, 12, 14, 4
        SayIt "Data Error: Line #" + l$ + ": Commas:" + c$, 13, 14, 4
        SayIt "INSERT to Edit " + CHR$(34) + Dbse$ + CHR$(34) + " ù TAB to Edit " + CHR$(34) + INI + CHR$(34) + " ù ESC to End", 22, 14, 4
        DO
        Getkey a%, k$
        SELECT CASE UCASE$(k$)
            CASE CHR$(9): SHELL "notepad.exe " + INI: RUN BOOT
            CASE CHR$(0) + "R": SHELL "notepad.exe " + Dbse$: RUN BOOT
        END SELECT
        LOOP
    END IF
    WEND
END SUB

SUB CreateCalendar
    DIM c%, d%, i$, j$, l$, m%, n%, s%, w%, x%, y%, z%
    w% = Dow%
    FOR m% = 1 TO 12
    Title$ = Mth$(0, m%) + " " + Year$
    i$ = "<a class=" + q + "shad" + q + " href=" + q + LEFT$(Mth$(0, m% - 1), 3) + ".htm" + q + ">" + Mth$(0, m% - 1) + "</a>&nbsp;"
    i$ = i$ + "&nbsp;<a href=" + q + Html$ + q + ">" + Mth$(0, m%) + "&nbsp;" + Year$ + "</a>&nbsp;"
    i$ = i$ + "&nbsp;<a class=" + q + "shad" + q + "href=" + q + LEFT$(Mth$(0, m% + 1), 3) + ".htm" + q + ">" + Mth$(0, m% + 1) + "</a>"
    CLOSE #2: OPEN Year$ + "\" + LEFT$(Mth$(0, m%), 3) + ".htm" FOR OUTPUT AS #2
    Header i$
    PRINT #2, "<tr>"
    Td "", "16", "", "wkdy", "", "Sunday"
    Td "", "14", "", "wkdy", "", "Monday"
    Td "", "14", "", "wkdy", "", "Tuesday"
    Td "", "14", "", "wkdy", "", "Wednesday"
    Td "", "14", "", "wkdy", "", "Thursday"
    Td "", "14", "", "wkdy", "", "Friday"
    Td "", "14", "", "wkdy", "", "Saturday"
    PRINT #2, "</tr>"
    d% = 0
    s% = 0
    SELECT CASE VAL(Mth$(1, m%)) + w% - 1
        CASE IS < 29: c% = 4
        CASE 29 TO 35: c% = 5
        CASE ELSE: c% = 6
    END SELECT
    FOR y% = 1 TO c%
    PRINT #2, "<tr>"
    FOR x% = 1 TO 7
    s% = s% + 1
    IF s% < w% THEN
        i$ = "<br>"
    ELSE
        d% = d% + 1
        SELECT CASE d%
            CASE 1 TO VAL(Mth$(1, m%)): i$ = MID$(STR$(d%), 2)
            CASE ELSE: i$ = "<br>"
        END SELECT
    END IF
    l$ = i$
    IF FSize% <= 20 THEN Events RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/" + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/" + RIGHT$(Year$, 2), l$
    IF x% = 1 THEN
        Td "left", "16", Hgt$, "date", "top", l$
    ELSE
        Td "left", "14", Hgt$, "date", "top", l$
    END IF
    NEXT
    PRINT #2, "</tr>"
    PRINT #2, ""
    NEXT
    Footer ""   'Mth$(0, m%) + " " + Year$
    FOR z% = 1 TO VAL(Mth$(1, m%))
    w% = w% + 1: IF w% > 7 THEN w% = 1
    NEXT
    NEXT
    CLOSE
END SUB

SUB CreateIndex
    DIM i$, j$
    CLOSE : OPEN Html$ FOR OUTPUT AS #2
    PRINT #2, "<!doctype html>"
    PRINT #2, "<html lang="; q; "en"; q; ">"
    PRINT #2, "<head>"
    PRINT #2, "<meta charset="; q; "utf-8"; q; ">"
    PRINT #2, "<title>Calendar Index</title>"
    PRINT #2, "<style type="; q; "text/css"; q; ">"
    PRINT #2, "A        {color:#000;font-size:26px;text-decoration:none;}"
    'PRINT #2, "A:hover  {color:#fff;background:#000;}"
    PRINT #2, "BODY     {"
    PRINT #2, "         font-family:georgia,"; q; "times new roman"; q; ",trebuchet,serif;"
    PRINT #2, "         font-style:normal;"
    PRINT #2, "         font-type:normal;"
    PRINT #2, "         font-weight:bold;"
    PRINT #2, "         font-size:26px;"
    PRINT #2, "         padding;5px;"
    PRINT #2, "         text-align:center;"
    PRINT #2, "         }"
    PRINT #2, ".mhdr"
    PRINT #2, "         {"
    PRINT #2, "         color:#fff;"
    PRINT #2, "         background:#00f;"
    PRINT #2, "         font-size:36px;"
    PRINT #2, "         }"
    PRINT #2, "</style>"
    PRINT #2, "</head>"
    PRINT #2, "<body>"
    PRINT #2, "         <div class="; q; "mhdr"; q; ">Calendar Index</div>"
    PRINT #2, "         <p>"
    SHELL "dir > temp.txt /B"
    CLOSE #1: OPEN "temp.txt" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    SELECT CASE LEFT$(i$, 1)
        CASE "0" TO "9"
            IF j$ = "" THEN
                j$ = "+"
                PRINT #2, TAB(8); "<a href="; q; i$; "\calendar.htm"; q; ">"; i$; "</a>"
            ELSE
                PRINT #2, TAB(8); "- <a href="; q; i$; "\calendar.htm"; q; ">"; i$; "</a>"
            END IF
    END SELECT
    WEND
    PRINT #2, "         </p>"
    PRINT #2, "</body>"
    PRINT #2, "</html>"
    CLOSE
END SUB

SUB CreateMenu
    CLOSE : OPEN Year$ + "\" + Html$ FOR OUTPUT AS #2
    Title$ = "<a href=" + q + "..\calendar.htm" + q + ">"
    Title$ = Title$ + "Calendar for " + Year$ + "</a>"
    Header Title$
    PRINT #2, "<tr>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "jan.htm" + q + ">January</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "apr.htm" + q + ">April</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "jul.htm" + q + ">July</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "oct.htm" + q + ">October</a>"
    PRINT #2, "</tr><tr>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "feb.htm" + q + ">February</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "may.htm" + q + ">May</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "aug.htm" + q + ">August</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "nov.htm" + q + ">November</a>"
    PRINT #2, "</tr><tr>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "mar.htm" + q + ">March</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "jun.htm" + q + ">June</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "sep.htm" + q + ">September</a>"
    Td "", "25", "", "sttl", "", "<a href=" + q + "dec.htm" + q + ">December</a>"
    PRINT #2, "</tr>"
    Footer ""   'Year$
END SUB

SUB Events (d$, l$)
    DIM i$, j$, x%, y%
    Flt% = 0
    CLOSE #1: OPEN Dbse$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        INPUT #1, i$
        LINE INPUT #1, j$
        IF i$ = LEFT$(d$, LEN(i$)) THEN
            l$ = l$ + "<br><a class=" + q + "item" + q + ">" + j$ + "</a>"
        END IF
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE #1
END SUB

SUB Footer (i$)
    IF LEN(i$) > 0 THEN
        PRINT #2, "<tr>"
        Td "", "", "", "ital", "", i$
        PRINT #2, "</tr>"
    END IF
    PRINT #2, "</table>"
    PRINT #2, "</body>"
    PRINT #2, "</html>"
END SUB

SUB Generate
    DIM a%, k$, x%
    Purge
    CreateMenu
    CreateCalendar
    CreateIndex
    x% = 4
    DO
    SayIt "", x%, Fgc%, Bgc%
    LOOP WHILE x% < 21
    SayIt "The " + q + "html" + q + " Calendar for " + Year$ + " was Generated", 12, 10, Bgc%
    SayIt "Press ENTER to Continue or ESC to End", 22, 11, Bgc%
    DO
    Getkey a%, k$
    LOOP UNTIL a% = 13
    RUN BOOT
END SUB

SUB Getkey (a%, k$)
    DO: k$ = INKEY$: LOOP UNTIL k$ = ""
    DO: k$ = INKEY$: LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF k$ = "*" THEN
        RUN BOOT
    ELSEIF a% = 27 THEN
        END
    END IF
END SUB

SUB Header (t$)
    DIM x%
    'The next little FOR/NEXT loop
    'creates the space at the top of the calendar page.
    FOR x% = 1 TO 10: PRINT #2, "<br>": NEXT
    PRINT #2, "<!doctype html>"
    PRINT #2, "<html lang="; q; "en"; q; ">"
    PRINT #2, "<head>"
    PRINT #2, "<title>"; Title$; "</title>"
    PRINT #2, "<style type="; q; "text/css"; q; ">"
    PRINT #2, "A    {text-decoration:none; color:#00f;}"
    PRINT #2, "BODY {"
    PRINT #2, "     background:#fff;"
    PRINT #2, "     font-family:"; Family$; ";"
    PRINT #2, "     font-style:normal;"
    PRINT #2, "     }"
    PRINT #2, ".ttle {color:#00f;font-weight:bold;font-size:40;}"
    PRINT #2, ".sttl {color:#00f;font-weight:bold;font-size:30;}"
    PRINT #2, ".shad {color:#666;font-weight:bold;font-size:26;}"
    PRINT #2, ".wkdy {color:#00f;font-weight:bold;font-size:20;}"
    PRINT #2, ".date {color:#000;font-weight:bold;font-size:16;}"
    PRINT #2, ".tday {color:#f00;background:#ff0;font-style:italic;font-weight:bold;font-size:16;}"
    PRINT #2, ".item {color:#f00;font-weight:bold;font-size:12;}"
    PRINT #2, ".ital {color:#fff;background:#000;font-style:italic;font-weight:bold;font-size:10;}"
    PRINT #2, "</style>"
    PRINT #2, "</head>"
    PRINT #2, "<body>"
    PRINT #2, "<table";
    PRINT #2, " align="; q; "center"; q;
    PRINT #2, " border="; q; "1"; q;
    PRINT #2, " cellpadding="; q; "5"; q;
    PRINT #2, " cellspacing="; q; "0"; q;
    PRINT #2, " width="; q; "100%"; q; ">"
    Td "", "", "", "ttle", "", t$
END SUB

SUB Initialize
    DIM i$, x%
    RESTORE WeekDays
    FOR x% = 1 TO 7
    READ Wkd$(x%)
    NEXT
    RESTORE Months
    FOR x% = 0 TO 13
    READ Mth$(0, x%), Mth$(1, x%)
    NEXT
    Bgc% = 1
    Dbse$ = PRGM + ".txt"
    Dow% = 1
    Family$ = "georgia," + CHR$(34) + "times new roman" + CHR$(34) + ",trebuchet,serif"
    Fgc% = 15
    FSize% = 20
    Hgt$ = "120"
    Html$ = PRGM + ".htm"
    Index$ = PRGM + ".sub"
    q = CHR$(34)
    Temp$ = "temp.txt"
    Today$ = LEFT$(DATE$, 2) + " / " + MID$(DATE$, 4, 2) + " / " + RIGHT$(DATE$, 4)
    Year$ = RIGHT$(DATE$, 4)
    DO
    Flt% = 0: CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "DAY="
                    Dow% = VAL(MID$(i$, x% + 1))
                    IF Dow% < 1 OR Dow% > 7 THEN Dow% = 1
                    EXIT FOR
                CASE "FONT-FAMILY=": Family$ = MID$(i$, x% + 1): EXIT FOR
                CASE "FONT-SIZE=": FSize% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "YEAR=": Year$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        Flt% = 0
        CLOSE : OPEN INI FOR OUTPUT AS #2
        PRINT #2, "DAY=1"
        PRINT #2, "FONT-FAMILY="; Family$
        PRINT #2, "FONT-SIZE=20"
        PRINT #2, "YEAR="; Year$
    END IF
    LOOP
    CLOSE
    SELECT CASE VAL(RIGHT$(Year$, 2))
        CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: Mth$(1, 2) = "29"
    END SELECT
    IF FSize% < 10 THEN FSize% = 10
    IF FSize% > 20 THEN FSize% = 20
END SUB

SUB Justify (i$, j$)

END SUB

SUB Main
    DIM a%, i$, k$
    Mask
    DO
    Getkey a%, k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + Dbse$: RUN BOOT
        CASE CHR$(13): Generate: END
        CASE CHR$(0) + CHR$(15): SHELL "notepad.exe " + INI: RUN BOOT
        CASE CHR$(0) + "<"
            CreateIndex
            SayIt "A Calendar Index was Created", 22, 31, Bgc%
            SLEEP 2
            RUN BOOT
    END SELECT
    LOOP
END SUB

SUB Mask
    DIM x%
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
        CASE 3, 21: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt q + "html" + q + " Calendar Generator", 2, 11, Bgc%
    x% = 8
    SayIt "January 1, " + Year$ + " Begins on " + Wkd$(Dow%), x%, 10, Bgc%
    SayIt "Press ENTER to Generate the Calendar", x%, 14, Bgc%
    SayIt "TAB: Edit " + CHR$(34) + Dbse$ + CHR$(34), x%, Fgc%, Bgc%
    SayIt "SHIFT/TAB: Edit " + CHR$(34) + INI + CHR$(34), x%, Fgc%, Bgc%
    SayIt "F2: Create A Calendar Index", x%, Fgc%, Bgc%
    SayIt "ESC to End", 22, Fgc%, Bgc%
END SUB

SUB Purge
    DIM i$, x%
    MKDIR Year$
    SHELL "dir " + Year$ + " > " + Temp$ + " /B"
    Flt% = 0
    CLOSE #1: OPEN Temp$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        SELECT CASE UCASE$(LEFT$(i$, 1))
            CASE "0" TO "9", "A" TO "Z": KILL Year$ + "\" + i$
        END SELECT
        WEND
    END IF
    CLOSE #1
    KILL Temp$
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM e$, l$, r$
    IF i$ = "" THEN e$ = "" ELSE e$ = " ù "
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(39, 32) + RIGHT$(e$, 2) + l$, 39)
    r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(39, 32), 39)
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 22
    COLOR f%, b%
    LOCATE v%, 2
    PRINT l$; r$
    COLOR Fgc%, Bgc%
    v% = v% + 2
END SUB

SUB Td (a$, c$, h$, i$, v$, l$)
    DIM w$
    IF a$ = "" THEN
        a$ = "center"
    ELSEIF a$ = "^" THEN
        a$ = "center"
    ELSEIF a$ = "<" THEN
        a$ = "left"
    ELSEIF a$ = ">" THEN
        a$ = "right"
    END IF
    IF c$ = "" THEN c$ = "100"
    IF i$ = "" THEN i$ = "text"
    w$ = c$ + "%"
    PRINT #2, "<td";
    PRINT #2, " align="; q; a$; q;
    PRINT #2, " colspan="; q; c$; q;
    IF LEN(h$) > 0 THEN PRINT #2, " height="; q; h$; q;
    PRINT #2, " class="; q; i$; q;
    IF LEN(v$) > 0 THEN PRINT #2, " valign="; q; v$; q;
    PRINT #2, " width="; q; w$; q; ">";
    IF LEN(l$) > 0 THEN PRINT #2, l$; "</td>"
END SUB

