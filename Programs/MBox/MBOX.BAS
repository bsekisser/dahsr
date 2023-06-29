DECLARE SUB Deslash (i$)
DECLARE SUB ValidateDirectory (i$)
DECLARE SUB Tr (n%, a$, c$, s$, v$, i$)
DECLARE SUB Table (n%, a$, b$, p$, s$, w$)
DECLARE SUB Td (n%, a$, c$, s$, v$, i$)
DECLARE SUB Hsmil (n%)
DECLARE SUB CreateRequestsPage (g$, p$)
DECLARE SUB PurgeTarget ()
DECLARE SUB CreateMenuPage ()
DECLARE SUB CreateGroupPage (g$, p$)
DECLARE SUB LoadGroups ()
DECLARE SUB FormatLine (i$)
DECLARE SUB Fsmil (n%)
DECLARE SUB Header (n%)
DECLARE SUB Footer (n%)
DECLARE SUB Format ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
DECLARE SUB CreateDirectories ()
    CONST BOOT = "mbox.bas", CSS = "mbox.css", INI = "mbox.ini"
    COMMON SHARED Bgc%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED q$
    COMMON SHARED Library$
    COMMON SHARED Request$
    COMMON SHARED Source$
    COMMON SHARED Target$
    COMMON SHARED Title$
    COMMON SHARED Tmed$
    COMMON SHARED Today$
    DIM SHARED Mth$(12)
    ON ERROR GOTO Trap
    Initialize
    DO
    Main
    LOOP
END
Trap: Flt% = ERR: RESUME NEXT
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"

SUB AddLine (i$, j$, m$, p$, q$)
END SUB

SUB CreateGroupPage (g$, p$)
    DIM i$, j$, m$, x%
    CLOSE #3: OPEN Library$ FOR INPUT AS #3
    CLOSE #4: OPEN Target$ + "\p" + p$ + ".htm" FOR OUTPUT AS #4
    Header 4
    Table 4, "", "1", "5", "", ""
    Td 4, "", "text", "", "", ""
    PRINT #4, "<div align="; q$; center; ; q$; " class="; q$; "btxt"; q$; ">"
    PRINT #4, "<a href=" + q$ + "mbox.htm" + q$ + ">Return to Menu<hr width="; q$; "25%"; q$; "></a></div>"
    PRINT #4, "<div align="; q$; center; ; q$; " class="; q$; "mtxt"; q$; ">"
    PRINT #4, q$; g$; q$; "</a></div>"
    PRINT #4, "</td>"
    Tr 4, "<", "text", "", "", "<a href=" + q$ + "p" + p$ + ".wpl" + q$ + ">Play All</a>"
    CLOSE #5: OPEN Target$ + "\p" + p$ + ".wpl" FOR OUTPUT AS #5
    Hsmil 5
    WHILE NOT EOF(3)
    LINE INPUT #3, i$
    IF i$ = "" THEN
        i$ = ""
    ELSE
        IF UCASE$(LEFT$(i$, LEN(g$))) = UCASE$(g$) THEN
            j$ = MID$(i$, LEN(g$) + 2)
            m$ = MID$(STR$(VAL(m$) + 1), 2)
            FOR x% = LEN(j$) TO 1 STEP -1
            IF MID$(j$, x%, 1) = "." THEN
                j$ = LEFT$(j$, x%)
                EXIT FOR
            END IF
            NEXT
            FOR x% = 1 TO LEN(j$)
            IF MID$(j$, x%, 1) = "," THEN
                IF MID$(j$, x% + 1, 1) = " " THEN
                    MID$(j$, x% + 1, 1) = " "
                ELSE
                    j$ = LEFT$(j$, x%) + " " + MID$(j$, x% + 1)
                END IF
            END IF
            NEXT
            Tr 4, "<", "text", "", "", "<a href=" + q$ + Source$ + "\" + i$ + q$ + ">" + RIGHT$(STRING$(LEN(Tmed$), "0") + m$, LEN(Tmed$)) + "</a>:&nbsp;" + j$
            FormatLine i$
            PRINT #5, "<media src="; q$; Source$; "\"; i$; q$; "/>"'; "<br>"
        END IF
    END IF
    WEND
    PRINT #4, ""
    PRINT #4, "</table>"
    PRINT #4, ""
    Footer 4
    Fsmil 5
END SUB

SUB CreateMenuPage
    DIM g$, p$
    p$ = "0"
    Flt% = 0
    CLOSE
    OPEN "groups.txt" FOR INPUT AS #1
    OPEN Target$ + "\mbox.htm" FOR OUTPUT AS #2
    Header 2
    PRINT #2, ""
    Table 2, "", "", "3", "", ""
    Td 2, "^", "mtxt", "", "", ""
    PRINT #2, "<div align="; q$; center; ; q$; " class="; q$; "btxt"; q$; ">";
    PRINT #2, "Total Selections: "; Tmed$; "<hr width="; q$; "25%"; q$; "></div>"
    PRINT #2, ""
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        INPUT #1, g$
        p$ = MID$(STR$(VAL(p$) + 1), 2)
        PRINT #2, "<a href="; q$; "p"; p$; ".htm"; q$; ">"; g$; "</a><br>"
        CreateGroupPage g$, p$
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE #1: OPEN "requests.txt" FOR INPUT AS #1
    IF Flt% = 0 THEN
        PRINT #2, "<div align="; q$; "center"; q$; " class="; q$; "stxt"; q$; ">"
        PRINT #2, "<hr><u>Special Requests</u></div>"
        WHILE NOT EOF(1)
        INPUT #1, g$
        p$ = MID$(STR$(VAL(p$) + 1), 2)
        PRINT #2, "<a href="; q$; "p"; p$; ".htm"; q$; ">"; g$; "</a><br>"
        CreateRequestsPage g$, p$
        WEND
    ELSE
        Flt% = 0
    END IF
    PRINT #2, "<hr>"
    PRINT #2, "<div align="; q$; "center"; q$; " class="; q$; "btxt"; q$; ">"
    PRINT #2, "If you've added any media into or changed names or data within the source"
    PRINT #2, "directory then you must update the " + q$ + Library$ + q$ + " file through the"
    PRINT #2, q$ + "COMMAND PROMPT" + q$ + " and then run " + CHR$(34) + BOOT + CHR$(34) + " to reflect any changes...<br>"
    PRINT #2, "...and don't forget to incorporate the " + CHR$(34) + "/B" + CHR$(34) + " switch on the command line!"
    PRINT #2, "</div>"
    PRINT #2, ""
    PRINT #2, "</td></table>"
    PRINT #2, ""
    Footer 2
    CLOSE
END SUB

SUB CreateRequestsPage (g$, p$)
    DIM i$, j$, m$, x%, y%
    CLOSE #3: OPEN Library$ FOR INPUT AS #3
    CLOSE #4: OPEN Target$ + "\p" + p$ + ".htm" FOR OUTPUT AS #4
    Header 4
    Table 4, "", "1", "5", "", ""
    Td 4, "", "text", "", "", ""
    PRINT #4, "<div align="; q$; center; ; q$; " class="; q$; "btxt"; q$; ">"
    PRINT #4, "<a href=" + q$ + "mbox.htm" + q$ + ">Return to Menu<hr width="; q$; "25%"; q$; "></a></div>"
    PRINT #4, "<div align="; q$; center; ; q$; " class="; q$; "mtxt"; q$; ">"
    PRINT #4, "by Special Request:<br>"; q$; g$; q$; "</a></div>"
    PRINT #4, "</td>"
    Tr 4, "<", "text", "", "", "<a href=" + q$ + "p" + p$ + ".wpl" + q$ + ">Play All</a>"
    CLOSE #5: OPEN Target$ + "\p" + p$ + ".wpl" FOR OUTPUT AS #5
    Hsmil 5
    WHILE NOT EOF(3)
    i$ = ""
    LINE INPUT #3, i$
    IF i$ = "" THEN
        i$ = ""
    ELSE
        FOR y% = 1 TO LEN(i$) - LEN(g$)
        IF UCASE$(MID$(i$, y%, LEN(g$))) = UCASE$(g$) THEN
            j$ = i$
            m$ = MID$(STR$(VAL(m$) + 1), 2)
            FOR x% = LEN(j$) TO LEN(j$) - 5 STEP -1
            IF MID$(j$, x%, 1) = "." THEN
                j$ = LEFT$(j$, x%)
                EXIT FOR
            END IF
            NEXT
            FOR x% = 1 TO LEN(j$)
            IF MID$(j$, x%, 1) = "," THEN
                IF MID$(j$, x% + 1, 1) = " " THEN
                    MID$(j$, x% + 1, 1) = " "
                ELSE
                    j$ = LEFT$(j$, x%) + " " + MID$(j$, x% + 1)
                END IF
            END IF
            NEXT
            Tr 4, "<", "text", "", "", "<a href=" + q$ + Source$ + "\" + i$ + q$ + ">" + RIGHT$(STRING$(LEN(Tmed$), "0") + m$, LEN(Tmed$)) + "</a>:&nbsp;" + j$
            FormatLine i$
            PRINT #5, "<media src="; q$; Source$; "\"; i$; q$; "/>"'; "<br>"
        END IF
        NEXT
    END IF
    WEND
    PRINT #4, ""
    PRINT #4, "</table>"
    PRINT #4, ""
    Footer 4
    Fsmil 5
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Footer (n%)
    Table n%, "", "", "5", "", ""
    Td n%, "", "ital", "", "", "Last Update: " + Today$
    PRINT #n%, "</table>"
    PRINT #n%, ""
    PRINT #n%, "</td>"
    PRINT #n%, "</table>"
    PRINT #n%, "</body>"
    PRINT #n%, "</html>"
END SUB

SUB Format
    DIM a%, k$
    Flt% = 0
    CLOSE : OPEN Library$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        SayIt "Creating " + CHR$(34) + "mbox.htm" + CHR$(34) + "!", 22, 14, Bgc%
        PurgeTarget
        LoadGroups
        CreateMenuPage
        SayIt "ù Done ù Press ENTER to Exit ù ESC to End ù", 22, 11, Bgc%
    ELSE
        Flt% = 0
        SayIt "ù Cannot Find " + CHR$(34) + Library$ + CHR$(34) + " ù ENTER to Exit ù ESC to End ù", 22, 14, 4
    END IF
    DO
    Getkey a%, k$
    LOOP UNTIL a% = 13
END SUB

SUB FormatLine (i$)
    DIM j$, l$, x%
    FOR x% = 1 TO LEN(i$)
    l$ = MID$(i$, x%, 1)
    SELECT CASE l$
        CASE CHR$(34): l$ = "&quot;"
        CASE CHR$(38): l$ = "&amp;"
        CASE CHR$(39): l$ = "&apos;"
    END SELECT
    j$ = j$ + l$
    NEXT
    i$ = j$
END SUB

SUB Fsmil (n%)
    PRINT #n%, "</seq>"
    PRINT #n%, "</body>"
    PRINT #n%, "</smil>"
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Header (n%)
    DIM f$
    PRINT #n%, "<html>"
    PRINT #n%, "<head><title>"; Title$; "</title>"
    PRINT #n%, "<style type="; q$; "text/css"; q$; ">"
    PRINT #n%, "A:link, A:visited, A:active"
    PRINT #n%, "    {color:#0000ff;}"
    PRINT #n%, "A:link, A:visited, A:active, A:hover"
    PRINT #n%, "    {text-decoration:none; font-style:normal; font-weight:bold;}"
    PRINT #n%, "A:hover {background-color:#0000ff; color:#ffffff;}"
    PRINT #n%, ""
    PRINT #n%, "A:link, A:visited, A:active, A:hover, .mhdr, .shdr, .mtxt, .stxt, .text, .rtxt, .gtxt, .btxt, .ytxt, .rbkg, gbkg, .bbkg, .ybkg, .shdd, .ital"
    PRINT #n%, "    {font-family:georgia, times new roman;}"
    PRINT #n%, "A:link, A:visited, A:active, A:hover"
    PRINT #n%, "    {font-size:18;}"
    PRINT #n%, ".text, .rtxt, .gtxt, .btxt, .ytxt, .rbkg, .gbkg, .bbkg, .ybkg, .shdd, .ital"
    PRINT #n%, "    {font-size:16;}"
    PRINT #n%, ""
    PRINT #n%, ".mhdr   {color:#ffffff; background:#0000ff; font-type:normal; font-weight:bold; font-size:30;}"
    PRINT #n%, ".shdr   {color:#ffffff; background:#0000ff; font-type:normal; font-weight:normal; font-size:14;}"
    PRINT #n%, ".mtxt   {color:#0000ff; font-type:normal; font-weight:bold; font-size:22;}"
    PRINT #n%, ".stxt   {color:#0000ff; font-type:normal; font-weight:bold; font-size:20;}"
    PRINT #n%, ".text   {color:#000000; font-type:normal; font-weight:normal;}"
    PRINT #n%, ".rtxt   {color:#ff0000; font-type:normal; font-weight:normal;}"
    PRINT #n%, ".gtxt   {color:#006600; font-type:normal; font-weight:normal;}"
    PRINT #n%, ".btxt   {color:#0000ff; font-type:normal; font-weight:normal;}"
    PRINT #n%, ".ytxt   {color:#ffff00; font-type:normal; font-weight:normal;}"
    PRINT #n%, ".rbkg   {color:#ffff00; background:#ff0000; font-type:bold; font-weight:bold;}"
    PRINT #n%, ".gbkg   {color:#ffffff; background:#006600; font-type:bold; font-weight:bold;}"
    PRINT #n%, ".bbkg   {color:#ffffff; background:#0000ff; font-type:bold; font-weight:bold;}"
    PRINT #n%, ".ybkg   {color:#000000; background:#ffff00; font-type:bold; font-weight:bold;}"
    PRINT #n%, ".shdd   {color:#000000; background:#dddddd; font-type:normal; font-weight:normal;}"
    PRINT #n%, ".ital   {color:#ffffff; background:#000000; font-type:italic; font-weight:normal; font-size:;12;}"
    PRINT #n%, ""
    PRINT #n%, "TABLE   {background:#ffffff;}"
    PRINT #n%, "TD      {background:#ffffff;}"
    PRINT #n%, "BODY    {background:#bbbbbb;|"
    PRINT #n%, "</style>"
    PRINT #n%, "</head>"
    PRINT #n%, "<body bgcolor="; q$; "#ffffff"; q$; ">"
    Table n%, "", "5", "", "", "80"
    PRINT #n%, "<td>"
    PRINT #n%, ""
    Table n%, "", "", "5", "", ""
    Td n%, "", "mhdr", "", "", Title$
    PRINT #n%, "</table>"
END SUB

SUB Hsmil (n%)
    PRINT #n%, "<smil>"
    PRINT #n%, "<head><title>"; Title$; "</title></head>"
    PRINT #n%, "<body>"
    PRINT #n%, "<seq>"
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    Fgc% = 15
    Flt% = 0
    q$ = CHR$(34)
    Library$ = "library.txt"
    Request$ = "requests.txt"
    Source$ = "source"
    Target$ = "target"
    Title$ = "Media Box"
    CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "LIBRARY=": Library$ = MID$(i$, x% + 1): EXIT FOR
                CASE "REQUESTS=": Request$ = MID$(i$, x% + 1): EXIT FOR
                CASE "SOURCE=": Source$ = MID$(i$, x% + 1): EXIT FOR
                CASE "TARGET=": Target$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
    END IF
    FOR x% = 1 TO 12
    READ Mth$(x%)
    NEXT
    Today$ = Mth$(VAL(LEFT$(DATE$, 2))) + " " + MID$(DATE$, 4, 2) + ", " + RIGHT$(DATE$, 4)
    DO
    SELECT CASE RIGHT$(i$, 1)
        CASE "/", "\": i$ = LEFT$(i$, LEN(i$) - 1)
        CASE ELSE: EXIT DO
    END SELECT
    LOOP
    FOR x% = 1 TO LEN(Target$)
    SELECT CASE MID$(Target$, x%, 1)
        CASE "/", "\": MKDIR LEFT$(Target$, x% - 1)
    END SELECT
    NEXT
END SUB

SUB LoadGroups
    DIM g$, i$
    Flt% = 0
    CLOSE
    OPEN Library$ FOR INPUT AS #1
    OPEN "groups.txt" FOR OUTPUT AS #2: CLOSE #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, g$
        IF LEN(g$) > 0 THEN
            Tmed$ = MID$(STR$(VAL(Tmed$) + 1), 2)
            FOR x% = 1 TO LEN(g$)
            IF MID$(g$, x%, 1) = "," THEN
                g$ = LEFT$(g$, x% - 1)
                SELECT CASE UCASE$(LEFT$(g$, 1))
                    CASE "0" TO "9", "A" TO "Z"
                        CLOSE #2: OPEN "groups.txt" FOR INPUT AS #2
                        DO
                        WHILE NOT EOF(2)
                        LINE INPUT #2, i$
                        IF UCASE$(LEFT$(i$, LEN(g$))) = UCASE$(g$) THEN EXIT DO
                        WEND
                        CLOSE #2: OPEN "groups.txt" FOR APPEND AS #2
                        PRINT #2, g$
                        EXIT DO
                        LOOP
                END SELECT
                EXIT FOR
            END IF
            NEXT
        END IF
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE
END SUB

SUB Main
    DIM a%, i$, k$, x%
    Mask
    IF Library$ = "" OR Source$ = "" OR Target$ = "" THEN
        x% = 10
        i$ = "Cannot Find"
        IF Library$ = "" THEN
            i$ = i$ + " the Library Directory"
        ELSEIF Source$ = "" THEN
            i$ = i$ + " the Source Directory"
        ELSEIF Target$ = "" THEN
            i$ = i$ + " the Target Directory"
        END IF
        i$ = i$ + "!"
        SayIt i$, x%, 14, Bgc%
        x% = x% + 2
        SayIt "Press the TAB Key to Edit the " + q$ + INI + q$ + " File:", x%, 14, Bgc%
        x% = x% + 2
        SayIt "Press the ESC Key to End!", x%, 14, Bgc%
        DO
        Getkey a%, k$
        IF k$ = CHR$(9) THEN
            SHELL "notepad.exe " + INI
            RUN BOOT
        END IF
        LOOP
    ELSE
        x% = 6
        SayIt "Library Filename: " + CHR$(34) + Library$ + CHR$(34), x%, 10, Bgc%
        x% = x% + 1
        SayIt "Source Directory: " + CHR$(34) + Source$ + CHR$(34), x%, 10, Bgc%
        x% = x% + 1
        SayIt "Target Directory: " + CHR$(34) + Target$ + CHR$(34), x%, 10, Bgc%
        x% = x% + 2
        SayIt "ù ENTER to Create MBox html Files ù", x%, 14, Bgc%
        x% = x% + 2
        SayIt "ù TAB Edits the " + CHR$(34) + INI + CHR$(34) + " File ù INSERT Edits the Special Requests ù", x%, 14, Bgc%
        x% = x% + 2
        SayIt "If you've added any media into or changed names or data within the source", x%, 11, Bgc%
        x% = x% + 1
        SayIt "directory then you must update the " + q$ + Library$ + q$ + " file through the", x%, 11, Bgc%
        x% = x% + 1
        SayIt q$ + "COMMAND PROMPT" + q$ + " and then run " + CHR$(34) + BOOT + CHR$(34) + " to reflect any changes...", x%, 11, Bgc%
        x% = x% + 1
        SayIt "...and don't forget to incorporate the " + CHR$(34) + "/B" + CHR$(34) + " switch on the command line!", x%, 14, Bgc%
        x% = x% + 1
        DO
        Getkey a%, k$
        SELECT CASE UCASE$(k$)
            CASE CHR$(9)
                SHELL "notepad.exe mbox.ini"
                RUN BOOT
            CASE CHR$(13)
                Format
                RUN BOOT
            CASE CHR$(0) + ";"
                SHELL "notepad.exe " + Library$
                RUN BOOT
            CASE CHR$(0) + "R"
                SHELL "notepad.exe " + Request$
                RUN BOOT
            CASE "*": RUN BOOT
        END SELECT
        LOOP
    END IF
END SUB

SUB Mask
    DIM x%
    CLS
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, 21: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " Generator ù Today is " + Today$, 2, 11, Bgc%
    SayIt "ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB PurgeTarget
    DIM i$
    SHELL "dir " + Target$ + " > purge.shl /B"
    CLOSE : OPEN "purge.shl" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z": KILL Target$ + "\" + i$
    END SELECT
    WEND
    CLOSE
    KILL "purge.shl"
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(39, 32) + l$, 39)
    r$ = LEFT$(r$ + STRING$(39, 32), 39)
    COLOR f%, b%
    LOCATE v%, 2
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Table (n%, a$, b$, p$, s$, w$)
    SELECT CASE a$
        CASE "", "^", "c": a$ = "center"
        CASE "<", "l": a$ = "left"
        CASE ">", "r": a$ = "right"
    END SELECT
    IF b$ = "" THEN b$ = "0"
    IF p$ = "" THEN p$ = "0"
    IF s$ = "" THEN s$ = "0"
    IF w$ = "" THEN w$ = "100"
    PRINT #n%, "<table";
    PRINT #n%, " align="; q$; a$; q$;
    PRINT #n%, " border="; q$; b$; q$;
    PRINT #n%, " cellpadding="; q$; p$; q$;
    PRINT #n%, " cellspacing="; q$; s$; q$;
    PRINT #n%, " width="; q$; w$; "%"; q$; ">"
END SUB

SUB Td (n%, a$, c$, s$, v$, i$)
    SELECT CASE a$
        CASE "", "^", "c": a$ = "center"
        CASE "<", "l": a$ = "left"
        CASE ">", "r": a$ = "right"
    END SELECT
    IF c$ = "" THEN c$ = "text"
    IF s$ = "" THEN s$ = "100"
    SELECT CASE v$
        CASE "t": v$ = "top"
        CASE "m": v$ = "middle"
        CASE "b": v$ = "bottom"
    END SELECT
    PRINT #n%, "<td";
    PRINT #n%, " align="; q$; a$; q$;
    PRINT #n%, " class="; q$; c$; q$;
    PRINT #n%, " colspan="; q$; s$; q$;
    IF v$ = "" THEN v$ = "" ELSE PRINT #n%, " valign="; q$; v$; q$;
    PRINT #n%, " width="; q$; s$; "%"; q$; ">"
    IF LEN(i$) > 0 THEN PRINT #n%, i$; "</td>"
END SUB

SUB Tr (n%, a$, c$, s$, v$, i$)
    SELECT CASE a$
        CASE "", "^", "c": a$ = "center"
        CASE "<", "l": a$ = "left"
        CASE ">", "r": a$ = "right"
    END SELECT
    IF c$ = "" THEN c$ = "text"
    IF s$ = "" THEN s$ = "100"
    SELECT CASE v$
        CASE "t": v$ = "top"
        CASE "m": v$ = "middle"
        CASE "b": v$ = "bottom"
    END SELECT
    PRINT #n%, "<tr><td";
    PRINT #n%, " align="; q$; a$; q$;
    PRINT #n%, " class="; q$; c$; q$;
    PRINT #n%, " colspan="; q$; s$; q$;
    IF v$ = "" THEN v$ = "" ELSE PRINT #n%, " valign="; q$; v$; q$;
    PRINT #n%, " width="; q$; s$; "%"; q$; ">"
    IF LEN(i$) > 0 THEN PRINT #n%, i$; "</td></tr>"
END SUB

