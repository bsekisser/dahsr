DECLARE SUB Delete ()
DECLARE SUB Cuckoo ()
DECLARE SUB SortDbse ()
DECLARE SUB PurgeDbse ()
DECLARE SUB CreateCSS ()
DECLARE SUB FormatTextLine (i$)
DECLARE SUB FormatHtmlLine (i$)
DECLARE SUB Div (c$, i$)
DECLARE SUB Table (a$, b$, p$, s$, w$)
DECLARE SUB Tr (a$, b$, c$)
DECLARE SUB Td (a$, c$, s$, t$, i$)
DECLARE SUB Header ()
DECLARE SUB Footer ()
DECLARE SUB SetStart ()
DECLARE SUB MonthDays (m%, d%, y%)
DECLARE SUB MtTrash ()
DECLARE SUB Bufferize ()
DECLARE SUB Scroll ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB LoadEvents ()
DECLARE SUB FormatDates ()
DECLARE SUB Rst ()
TYPE dahsr
    aa AS STRING * 3
    bb AS STRING * 11
    cc AS STRING * 60
END TYPE
    CONST BOOT = "events.bas"
    CONST CSS = "events.css"
    CONST INI = "events.ini"
    CONST MXL = 76
    CONST PRGM = "events"
    CONST ROWS = 16
    COMMON SHARED Bgc%
    COMMON SHARED Dcw%
    COMMON SHARED Dbse$
    COMMON SHARED Display AS INTEGER
    COMMON SHARED Ebse$
    COMMON SHARED Echo%
    COMMON SHARED Edit$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Mark AS INTEGER
    COMMON SHARED Mhdr$
    COMMON SHARED Pdb AS INTEGER
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Span$
    COMMON SHARED Swd%
    COMMON SHARED Syr%
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Event AS dahsr, Mth(12) AS STRING, Wd(7) AS STRING * 3
    ON ERROR GOTO Trap
    Initialize
    FormatDates
    IF Pdb THEN PurgeDbse
    SortDbse
    SetStart
    LoadEvents
    Bufferize
    IF Mark AND Display THEN Main
    MtTrash
END
Trap: Flt% = ERR: RESUME NEXT
DB1:
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"
DB2:
DATA "Sun","Mon","Tue","Wed","Thu","Fri","Sat"
DB3:
DATA ".bas",".css",".exe",".htm",".ini",".obj",".txt","*"

SUB Bufferize
    DIM a$, b$, c$
    Flt% = 0
    CLOSE : OPEN Ebse$ FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    OPEN PRGM + ".htm" FOR OUTPUT AS #4
    Header
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        INPUT #1, a$, b$, c$
        Event.aa = a$
        Event.bb = b$
        Event.cc = c$
        PRINT #2, Event.aa; Event.bb; Event.cc
        IF Mark > 0 THEN Tr a$, b$, c$
        WEND
    ELSE
        Flt% = 0
    END IF
    Footer
    CLOSE
END SUB

SUB CreateCSS
    CLOSE : OPEN CSS FOR OUTPUT AS #2
    PRINT #2, "BODY"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "font-family:georgia,"; q; "times new roman"; q; ",tahoma,serif;"
    PRINT #2, TAB(4); "font-style:normal;"
    PRINT #2, TAB(4); "font-weight:bold;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".mhdr"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "font-size:30px;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".shdr"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "font-size:20px;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".span"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "font-size:20px;"
    PRINT #2, TAB(4); "font-style:italic;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".mhdr, .shdr, .span"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#00f;"
    PRINT #2, TAB(4); "color:#fff;"
    PRINT #2, TAB(4); "padding:5px;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".text"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#fff;"
    PRINT #2, TAB(4); "color:#000;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".rbkg"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#f00;"
    PRINT #2, TAB(4); "color:#fff;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".bbkg"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#00f;"
    PRINT #2, TAB(4); "color:#fff;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".ybkg"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#ff0;"
    PRINT #2, TAB(4); "color:#000;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".text, .rbkg, .bbkg, .ybkg"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "font-size:18px;"
    PRINT #2, TAB(4); "}"
    CLOSE
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Delete
    DIM i$, k$
    i$ = "ù DELETE and Reset " + q + "CSS" + q + " & " + q + "INI" + q + " Files "
    i$ = i$ + "ù ENTER to Exit "
    i$ = i$ + "ù ESC to End ù"
    SayIt i$, 22, 15, 4
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(13): EXIT SUB
        CASE CHR$(0) + "S"
            KILL PRGM + ".css"
            KILL PRGM + ".ini"
            SayIt "ù " + q + "CSS" + q + " & " + q + "INI" + q + " Files Deleted ù Will AutoCreate on Startup ù", 22, 26, Bgc%
            SLEEP 3
            RUN BOOT
    END SELECT
    Cuckoo
    LOOP
END SUB

SUB Div (c$, i$)
    PRINT #4, "<div class="; q; c$; q; ">"; i$; "</div>"
END SUB

SUB Footer
    PRINT #4, "</table>"
    PRINT #4, "</body>"
    PRINT #4, "</html>"
END SUB

SUB FormatDates
    DIM d$, i$, j$, m$, x%, y$
    CLOSE : OPEN Dbse$ FOR INPUT AS #1
    OPEN "temp.txt" FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    INPUT #1, i$
    m$ = RIGHT$("00" + (MID$(STR$(VAL(i$)), 2)), 2)
    d$ = ""
    y$ = ""
    FOR x% = 1 TO LEN(i$)
    IF MID$(i$, x%, 1) = "/" THEN
        IF d$ = "" THEN
            d$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
        ELSEIF y$ = "" THEN
            y$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
            EXIT FOR
        END IF
    END IF
    NEXT
    LINE INPUT #1, j$
    PRINT #2, RIGHT$("00" + m$, 2); "/";
    PRINT #2, RIGHT$("00" + d$, 2); "/";
    PRINT #2, LEFT$(RIGHT$(DATE$, 4), 4 - LEN(y$)) + y$;
    PRINT #2, ","; j$
    WEND
    CLOSE
    KILL Dbse$
    NAME "temp.txt" AS Dbse$
END SUB

SUB FormatHtmlLine (i$)
    DIM o%, x%
    DO
    o% = 0
    FOR x% = 1 TO LEN(i$)
    IF MID$(i$, x%, 1) = "," THEN
        IF MID$(i$, x% + 1, 1) = " " THEN
            i$ = LEFT$(i$, x% - 1) + ";" + MID$(i$, x% + 1)
        ELSE
            i$ = LEFT$(i$, x% - 1) + "; " + MID$(i$, x% + 1)
        END IF
        o% = o% + 1
        EXIT FOR
    END IF
    IF MID$(i$, x%, 3) = " & " THEN
        i$ = LEFT$(i$, x% - 1) + " &#38; " + MID$(i$, x% + 3)
        o% = o% + 1
        EXIT FOR
    END IF
    NEXT
    LOOP WHILE o% > 0
END SUB

SUB FormatTextLine (i$)
    DIM o%, x%
    DO
    o% = 0
    FOR x% = 1 TO LEN(i$)
    IF MID$(i$, x%, 1) = "," THEN
        i$ = LEFT$(i$, x% - 1) + ";" + MID$(i$, x% + 1)
        IF MID$(i$, x% + 1, 1) = CHR$(32) THEN
            i$ = i$
        ELSE
            i$ = LEFT$(i$, x%) + " " + MID$(i$, x% + 1)
        END IF
        o% = o% + 1
        EXIT FOR
    END IF
    NEXT
    LOOP WHILE o% > 0
END SUB

SUB Getkey (k$)
    DO: k$ = INKEY$: LOOP UNTIL k$ = ""
    DO: k$ = INKEY$: LOOP WHILE k$ = ""
    IF ASC(k$) = 27 THEN
        MtTrash
        END
    ELSEIF k$ = "*" THEN
        MtTrash
        RUN BOOT
    END IF
END SUB

SUB Header
    PRINT #4, "<!doctype html>"
    PRINT #4, "<html lang="; q; "en"; q; ">"
    PRINT #4, "<head>"
    PRINT #4, "<meta charset="; q; "utf-8"; q; ">"
    PRINT #4, "<title>Events</title>"
    PRINT #4, "<link href="; q; CSS; q; " rel="; q; "stylesheet"; q; " type="; q; "text/css"; q; ">"
    PRINT #4, "</head>"
    PRINT #4, "<body>"
    Table "", "3", "3", "", "90"
    Td "", "", "", "<", ""
    Div "mhdr", "Events&#44; Apointments &#38; Reminders"
    IF Mark = 0 THEN
        Div "span", "&#34;There Are No Scheduled Events<br>" + Span$ + ",<br>Have a Nice Day!&#34;"
    ELSE
        Div "shdr", Span$
    END IF
    PRINT #4, "</td></tr>"
END SUB

SUB Initialize
    DIM i$, x%
    RESTORE DB1
    FOR x% = 1 TO 12: READ Mth(x%): NEXT
    RESTORE DB2
    FOR x% = 1 TO 7: READ Wd(x%): NEXT
    Bgc% = 1
    Dbse$ = PRGM + ".txt"
    Dcw% = 15
    Display = 1
    Edit$ = "notepad.exe "
    Echo% = ROWS + 6
    Fgc% = 15
    Ebse$ = "wbse.txt"
    Mark = 0
    Pdb = 0
    q = CHR$(34)
    Swd% = 7
    Syr% = 2000
    Title$ = "ù Events, Apointments & Reminders ù"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    DO
    Flt% = 0: CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "DATECOLUMNWIDTH=": Dcw% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "DISPLAYMODE=": Display = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "EDITORPATHNAME=": Edit$ = MID$(i$, x% + 1): EXIT FOR
                CASE "PURGEDBSE=": Pdb% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "STARTWEEKDAY="
                    Swd% = VAL(MID$(i$, x% + 1))
                    IF Swd% < 1 THEN Swd% = 1
                    IF Swd% > 7 THEN Swd% = 7
                    EXIT FOR
                CASE "STARTYEAR="
                    Syr% = VAL(MID$(i$, x% + 1))
                    IF Syr% < 2000 THEN Syr% = 2000
                    IF Syr% > VAL(RIGHT$(DATE$, 4)) THEN Syr% = VAL(RIGHT$(DATE$, 4))
                    EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #2
        PRINT #2, "DATECOLUMNWIDTH="; MID$(STR$(Dcw%), 2)
        PRINT #2, "DISPLAYMODE="; MID$(STR$(Display), 2)
        PRINT #2, "EDITORPATHNAME="; Edit$
        PRINT #2, "PURGEDBSE="; MID$(STR$(Pdb%), 2)
        PRINT #2, "STARTWEEKDAY=7"
        PRINT #2, "STARTYEAR=2000"
    END IF
    LOOP
    DO
    Flt% = 0: CLOSE : OPEN CSS FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        CreateCSS
    END IF
    LOOP
    SHELL "dir > dir.shl"
    OPEN "dir.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "dir.shl"
    FOR x% = 1 TO LEN(i$)
    SELECT CASE UCASE$(MID$(i$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
END SUB

SUB LoadEvents
    DIM d%, i$, j$, l$, m%, t$, u$, x%, y%, z%
    m% = VAL(Today$)
    d% = VAL(MID$(Today$, 4))
    y% = VAL(RIGHT$(Today$, 4))
    CLOSE : OPEN Ebse$ FOR OUTPUT AS #2
    FOR x% = 1 TO ROWS
    t$ = RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/"
    t$ = t$ + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/"
    t$ = t$ + RIGHT$("0000" + MID$(STR$(y%), 2), 4)
    u$ = RIGHT$("00" + MID$(STR$(d%), 2), 2) + " "
    u$ = u$ + LEFT$(Mth(m%), 3) + " "
    u$ = u$ + RIGHT$("0000" + MID$(STR$(y%), 2), 4)
    v$ = Mth(m%) + " "
    v$ = v$ + RIGHT$("00" + MID$(STR$(d%), 2), 2) + ", "
    v$ = v$ + RIGHT$("0000" + MID$(STR$(y%), 2), 4)
    IF x% = 1 THEN Span$ = "from " + v$
    IF x% = ROWS THEN Span$ = Span$ + " to " + v$
    l$ = ""
    CLOSE #1: OPEN Dbse$ FOR INPUT AS #1
    WHILE NOT EOF(1)
    INPUT #1, i$
    LINE INPUT #1, j$
    IF i$ = t$ THEN
        IF Mark = 0 THEN Mark = 1
        IF l$ = "" THEN
            l$ = j$
        ELSE
            l$ = l$ + "," + j$
        END IF
    END IF
    WEND
    FormatTextLine l$
    PRINT #2, Wd(Swd%); ","; u$; ","; l$
    MonthDays m%, z%, y%
    Swd% = Swd% + 1: IF Swd% > 7 THEN Swd% = 1
    d% = d% + 1
    IF d% > z% THEN
        d% = 1
        m% = m% + 1
        IF m% > 12 THEN
            m% = 1
            y% = y% + 1
        END IF
    END IF
    NEXT
END SUB

SUB Main
    DIM e%, i$, k$, s%
    DO
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = LEN(Event) + 2
    e% = LOF(1) / (LEN(Event) + 2)
    s% = 1
    Scroll
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL Edit$ + Dbse$: RUN BOOT
        CASE CHR$(0) + CHR$(15): SHELL Edit$ + INI: RUN BOOT
        CASE CHR$(0) + "<": SHELL Edit$ + CSS: RUN BOOT
        CASE CHR$(0) + "S": Delete: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM c$, h$, i$, x%, y%
    STATIC o$
    IF o$ = "" THEN
        o$ = "+'"
        CLS
    ELSE
        LOCATE 1, 1, 0
    END IF
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 4, Echo% - 1: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$, 2, 11, Bgc%
    IF Mark THEN
        SayIt "ù " + Span$ + " ù", 3, 11, Bgc%
    ELSE
        SayIt "ù There Are No Scheduled Events for the Period ù", 3, 11, Bgc%
    END IF
    h% = 3
    DO
    FOR y% = 1 TO 3
    SELECT CASE y%
        CASE 1: h% = h% + LEN(Event.aa)
        CASE 2: h% = h% + LEN(Event.bb) + 1
        CASE ELSE: EXIT DO
    END SELECT
    FOR x% = 4 TO 21
    SELECT CASE x%
        CASE 4: c$ = CHR$(194)
        CASE Echo% - 1: c$ = CHR$(193)
        CASE ELSE: c$ = CHR$(179)
    END SELECT
    LOCATE x%, h%: PRINT c$
    NEXT
    NEXT
    LOOP
    i$ = ""
    i$ = i$ + "ù TAB to Edit Data "
    i$ = i$ + "ù SHIFT/TAB to Edit INI "
    i$ = i$ + "ù ESC to End "
    i$ = i$ + "ù"
    SayIt i$, 22, Fgc%, Bgc%
    IF Pdb THEN
        LOCATE 2, 2
        PRINT "* "
        LOCATE 2, 79
        PRINT "*"
        LOCATE 22, 2
        PRINT "* "
        LOCATE 22, 79
        PRINT "*"
    END IF
END SUB

SUB MonthDays (m%, d%, y%)
    SELECT CASE m%
        CASE 2
            SELECT CASE VAL(RIGHT$(STR$(y%), 2))
                CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: d% = 29
                CASE ELSE: d% = 28
            END SELECT
        CASE 4, 6, 9, 11: d% = 30
        CASE ELSE: d% = 31
    END SELECT
END SUB

SUB MtTrash
    DIM i$
    SHELL "dir > trash.can /B"
    CLOSE : OPEN "trash.can" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    RESTORE DB3
    DO
    READ j$
    IF j$ = "*" THEN
        KILL i$
        EXIT DO
    ELSEIF LCASE$(LEFT$(i$, LEN(PRGM))) = LCASE$(PRGM) AND LCASE$(RIGHT$(i$, LEN(j$))) = LCASE$(j$) THEN
        EXIT DO
    END IF
    LOOP
    WEND
    CLOSE
    KILL "trash.can"
END SUB

SUB PurgeDbse
    DIM a$, d$, i$, t$
    t$ = RIGHT$(DATE$, 2) + LEFT$(DATE$, 2) + MID$(DATE$, 4, 2)
    CLOSE : OPEN Dbse$ FOR INPUT AS #1
    OPEN "temp.txt" FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    INPUT #1, d$
    LINE INPUT #1, i$
    a$ = RIGHT$(d$, 2) + LEFT$(d$, 2) + MID$(d$, 4, 2)
    IF VAL(t$) <= VAL(a$) THEN PRINT #2, d$; ","; i$
    WEND
    CLOSE
    KILL Dbse$
    NAME "temp.txt" AS Dbse$
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM h%, l$, r$, w%
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 22
    w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$; r$
    v% = v% + 1
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll
    DIM b%, f%, i$, m$, o$, x%
    m$ = CHR$(28)
    FOR x% = 1 TO ROWS
    GET #1, x%, Event
    b% = Bgc%
    IF RTRIM$(Event.cc) = "" THEN
        f% = 10
    ELSE
        SELECT CASE UCASE$(RIGHT$(RTRIM$(Event.cc), 1))
            CASE "*"
                Event.cc = "* " + LEFT$(RTRIM$(Event.cc), LEN(RTRIM$(Event.cc)) - 1) + " *"
                f% = 11
            CASE "0" TO "9"
                IF LEFT$(RTRIM$(Event.cc), 3) = "Dr." OR MID$(Event.cc, LEN(RTRIM$(Event.cc)) - 4, 1) = "@" THEN
                    f% = 14
                ELSE
                    f% = 10
                END IF
            CASE "A" TO "Z"
                IF LEFT$(Event.cc, 4) = "[Rx]" THEN
                    f% = 14
                    Event.cc = "! ReFill " + UCASE$(LTRIM$(MID$(RTRIM$(Event.cc), 5))) + " !"
                ELSE
                    f% = 11
                END IF
            CASE "!"
                f% = 14
                Event.cc = "! ReFill " + UCASE$(LEFT$(RTRIM$(Event.cc), LEN(RTRIM$(Event.cc)) - 1)) + " !"
            CASE ELSE: f% = Fgc%: b% = Bgc%
        END SELECT
    END IF
    COLOR f%, b%
    LOCATE x% + 4, 3
    PRINT Event.aa; m$;
    PRINT Event.bb; m$;
    PRINT RIGHT$(STRING$(LEN(Event.cc), 46) + RTRIM$(Event.cc), LEN(Event.cc))
    NEXT
    COLOR Fgc%, Bgc%
END SUB

SUB SetStart
    DIM d%, i$, l%, m%, t$, w%
    d% = 1
    m% = 1
    DO
    MonthDays m%, l%, Syr%
    Swd% = Swd% + 1: IF Swd% > 7 THEN Swd% = 1
    d% = d% + 1
    IF d% > l% THEN
        d% = 1
        m% = m% + 1
        IF m% > 12 THEN
            m% = 1
            Syr% = Syr% + 1
        END IF
    END IF
    t$ = RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/"
    t$ = t$ + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/"
    t$ = t$ + RIGHT$("0000" + MID$(STR$(Syr%), 2), 4)
    LOOP UNTIL t$ = Today$
END SUB

SUB SortDbse
    DIM d$, i$, j$, m$, y$
    CLOSE : OPEN Dbse$ FOR INPUT AS #1
    OPEN "temp.txt" FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    INPUT #1, i$
    LINE INPUT #1, j$
    m$ = LEFT$(i$, 2)
    d$ = MID$(i$, 4, 2)
    y$ = RIGHT$(i$, 4)
    PRINT #2, y$; m$; d$; ","; j$
    WEND
    CLOSE
    SHELL "sort temp.txt > sort.txt"
    OPEN "sort.txt" FOR INPUT AS #1
    OPEN Dbse$ FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    INPUT #1, i$
    LINE INPUT #1, j$
    m$ = MID$(i$, 5, 2)
    d$ = RIGHT$(i$, 2)
    y$ = LEFT$(i$, 4)
    PRINT #2, m$; "/"; d$; "/"; y$; ","; j$
    WEND
    CLOSE
END SUB

SUB Table (a$, b$, p$, s$, w$)
    IF a$ = "<" THEN
        a$ = "left"
    ELSEIF a$ = ">" THEN
        a$ = "right"
    ELSE
        a$ = "center"
    END IF
    IF b$ = "" THEN b$ = "0"
    IF p$ = "" THEN p$ = "0"
    IF s$ = "" THEN s$ = "0"
    IF w$ = "" THEN w$ = "100"
    PRINT #4, "<table";
    PRINT #4, " align="; q; a$; q;
    PRINT #4, " border="; q; b$; q;
    PRINT #4, " cellpadding="; q; p$; q;
    PRINT #4, " cellspacing="; q; s$; q;
    PRINT #4, " width="; q; w$; "%"; q; ">"
END SUB

SUB Td (a$, c$, s$, t$, i$)
    IF a$ = "<" THEN
        a$ = "left"
    ELSEIF a$ = ">" THEN
        a$ = "right"
    ELSE
        a$ = "center"
    END IF
    IF c$ = "" THEN c$ = "text"
    IF s$ = "" THEN s$ = "100"
    IF LEFT$(t$, 1) = "<" THEN PRINT #4, "<tr>";
    PRINT #4, "<td";
    PRINT #4, " align="; q; a$; q;
    PRINT #4, " class="; q; c$; q;
    PRINT #4, " colspan="; q; s$; q;
    PRINT #4, " width="; q; s$; "%"; q; ">";
    IF i$ = "" THEN
        PRINT #4, ""
    ELSEIF i$ = "</>" THEN
        PRINT #4, "</td>"
    ELSEIF i$ = "<//>" THEN
        PRINT #4, "</td></tr>"
    ELSE
        PRINT #4, i$; "</td>";
        IF RIGHT$(t$, 1) = ">" THEN
            PRINT #4, "</tr>"
        ELSE
            PRINT #4, ""
        END IF
    END IF
END SUB

SUB Tr (a$, b$, c$)
    DIM d$, i$, w%, x%
    w% = Dcw%
    IF c$ = "" THEN
        c$ = "</>"
        d$ = "text"
    ELSEIF LEFT$(c$, 3) = "Dr." OR MID$(c$, LEN(c$) - 4, 1) = "@" THEN
        d$ = "ybkg"
    ELSEIF UCASE$(LEFT$(c$, 4)) = "[RX]" OR RIGHT$(c$, 1) = "!" THEN
        d$ = "rbkg"
        c$ = "! Refill " + UCASE$(LTRIM$(MID$(c$, 5))) + " !"
    ELSEIF LEFT$(c$, 1) = "*" OR RIGHT$(c$, 1) = "*" THEN
        d$ = "bbkg"
        c$ = "* " + LEFT$(c$, LEN(c$) - 1) + " *"
    END IF
    FormatHtmlLine c$
    Td "<", d$, "5", "<", a$
    Td "", d$, MID$(STR$(w%), 2), "", b$
    Td ">", d$, MID$(STR$(100 - 5 - w%), 2), ">", c$
END SUB

