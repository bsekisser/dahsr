DECLARE SUB Cuckoo ()
DECLARE SUB Edit (k$)
DECLARE SUB Footer ()
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Header ()
DECLARE SUB Td (a$, c$, i$, l$)
DECLARE SUB Generate ()
DECLARE SUB Table (a$, b$, p$, s$, w$)
DECLARE SUB SolarDay (s%, l$)
DECLARE SUB GregorianDay (m%, d%, l$)
    CONST BOOT = "solar.bas", INI = "solar.ini"
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dwt$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Gregorian$
    COMMON SHARED Html$
    COMMON SHARED Iwt$
    COMMON SHARED Solar$
    COMMON SHARED Spring%
    COMMON SHARED Title$
    COMMON SHARED Year$
    DIM SHARED Mth(12) AS INTEGER
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = 1: RESUME NEXT
DATA 31,28,31,30,31,30,31,31,30,31,30,31

SUB Cuckoo
    SOUND 1400, 2
    SOUND 0, 0
    SOUND 1200, 4
    SOUND 0, 0
END SUB

SUB Edit (k$)
    SELECT CASE UCASE$(k$)
        CASE "G": SHELL "notepad.exe " + Gregorian$
        CASE CHR$(9): SHELL "notepad.exe " + INI
        CASE "S": SHELL "notepad.exe " + Solar$
    END SELECT
    RUN BOOT
END SUB

SUB Footer
    PRINT #2, "<tr>"
    Td "", "", "ital", "The Sun Don't Shine on East Side of the House All Day!"
    PRINT #2, "</tr>"
    PRINT #2, "</table>"
    PRINT #2, "</body>"
    PRINT #2, "</html>"
END SUB

SUB Generate
    DIM a%, d%, g$, k$, m%, s$, x%, y%
    DIM gd$, sd$
    MKDIR "solar"
    CLOSE : OPEN Html$ FOR OUTPUT AS #2
    Header
    m% = 3
    d% = Spring%
    y% = VAL(Year$)
    FOR x% = 0 TO 400
    g$ = RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/"
    g$ = g$ + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/"
    g$ = g$ + RIGHT$("00" + MID$(STR$(y%), 2), 2)
    gd$ = "&nbsp;"
    s$ = "Day " + RIGHT$("000" + MID$(STR$(x%), 2), 3)
    sd$ = "&nbsp;"
    GregorianDay m%, d%, gd$
    SolarDay x%, sd$
    PRINT #2, "<tr>"
    Td "", "8", "date", g$
    Td "<", MID$(STR$(50 - 8), 2), "item", gd$
    Td ">", MID$(STR$(50 - 8), 2), "item", sd$
    Td "", "8", "date", s$
    PRINT #2, "</tr>"
    SELECT CASE y%
        CASE 2004, 2008, 2012, 2016, 2020, 2024, 2028: Mth(2) = 29
        CASE ELSE: Mth(2) = 28
    END SELECT
    d% = d% + 1
    IF d% > Mth(m%) THEN d% = 1: m% = m% + 1
    IF m% > 12 THEN m% = 1: y% = y% + 1
    IF m% = 3 AND d% = Spring% THEN EXIT FOR
    NEXT
    Footer
    CLOSE
    SayIt "ù Calendar Generated: Press ENTER to ReSet or ESC to End ù", 22, 14, Bgc%
    DO
    Getkey a%, k$
    LOOP UNTIL a% = 13
    RUN BOOT
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB GregorianDay (m%, d%, l$)
    DIM i$, o%
    l$ = "&nbsp;"
    o% = 2
    Flt% = 0
    CLOSE #1: OPEN Gregorian$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        IF m% = VAL(i$) THEN
            i$ = MID$(i$, LEN(MID$(STR$(m%), 2)) + o%)
            IF d% = VAL(i$) THEN
                i$ = MID$(i$, LEN(MID$(STR$(d%), 2)) + o%)
                IF l$ = "&nbsp;" THEN
                    l$ = i$
                ELSE
                    l$ = l$ + ", " + i$
                END IF
            END IF
        END IF
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE #1
END SUB

SUB Header
    DIM q$
    q$ = CHR$(34)
    PRINT #2, "<html>"
    PRINT #2, "<head>"
    PRINT #2, "<title>Gregorian/Solar Calendar</title>"
    PRINT #2, "<style type="; q$; "text/css"; q$; ">"
    PRINT #2, "   #ttle {color:#ffffff; background:#0000ff; font-style:normal; font-family:times new roman; font-weight:bold; font-size:40;}"
    PRINT #2, "   #sttl {color:#0000ff; background:#ccccff; font-style:normal; font-family:times new roman; font-weight:bold; font-size:24;}"
    PRINT #2, "   #date {color:#0000ff; background:#bbffbb; font-style:normal; font-family:times new roman; font-weight:bold; font-size:18;}"
    PRINT #2, "   #item {color:#000000; background:#ffff00; font-style:normal; font-family:times new roman; font-weight:normal; font-size:16;}"
    PRINT #2, "   #ital {color:#ffffff; background:#000000; font-style:italic; font-family:times new roman; font-weight:bold; font-size:14;}"
    PRINT #2, "</style>"
    PRINT #2, "</head>"
    PRINT #2, "<body bgcolor="; q$; "#eeffee"; q$; ">"
    Table "", "5", "2", "", "90"
    Td "", "", "ttle", "Calendar"
    PRINT #2, "<tr>"
    Td "", "50", "sttl", "Gregorian"
    Td "", "50", "sttl", "Solar"
    PRINT #2, "</tr>"
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    Dbse$ = "dbse"
    Fgc% = 15
    Gregorian$ = "gregrian.txt"
    Html$ = "solar.htm"
    Solar$ = "solar.txt"
    Spring% = 21
    Title$ = "Gregorian/Solar html Calendar Generator"
    Year$ = MID$(STR$(VAL(RIGHT$(DATE$, 4))), 2)
    CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF LEN(i$) > x% THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "SPRING=": Spring% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "YEAR=": Year$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        ELSE
            EXIT FOR
        END IF
        NEXT
        WEND
    END IF
    CLOSE
    FOR x% = 1 TO 12
    READ Mth(x%)
    NEXT
END SUB

SUB Main
    DIM a%, k$
    Mask
    DO
    Getkey a%, k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9), "G", "S": Edit k$: END
        CASE CHR$(13): Generate: END
    END SELECT
    Cuckoo
    LOOP
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
    SayIt Title$, 2, 11, Bgc%
    SayIt "Press ENTER to Generate the Solar Calendar for " + Year$ + " /" + STR$(VAL(Year$) + 1), 13, 14, Bgc%
    SayIt "Edit (G)regorian or (S)olar Calendar ù TAB to Edit " + CHR$(34) + INI + CHR$(34) + " ù ESC to End", 22, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + l$, 38)
    r$ = LEFT$(r$ + STRING$(38, 32), 38)
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB SolarDay (s%, l$)
    DIM i$, o%
    l$ = "&nbsp;"
    o% = 2
    Flt% = 0
    CLOSE #1: OPEN Solar$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        IF s% = VAL(i$) THEN
            i$ = MID$(i$, LEN(MID$(STR$(s%), 2)) + o%)
            IF l$ = "&nbsp;" THEN
                l$ = i$
            ELSE
                l$ = l$ + ", " + i$
            END IF
        END IF
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE #1
END SUB

SUB Table (a$, b$, p$, s$, w$)
    DIM q$
    q$ = CHR$(34)
    IF a$ = "" THEN
        a$ = "center"
    ELSEIF a$ = "<" THEN
        a$ = "left"
    ELSEIF a$ = "^" THEN
        a$ = "center"
    ELSEIF a$ = ">" THEN
        a$ = "right"
    END IF
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

SUB Td (a$, c$, i$, l$)
    DIM q$
    q$ = CHR$(34)
    IF a$ = "" THEN
        a$ = "center"
    ELSEIF a$ = "<" THEN
        a$ = "left"
    ELSEIF a$ = "^" THEN
        a$ = "center"
    ELSEIF a$ = ">" THEN
        a$ = "right"
    END IF
    IF c$ = "" THEN c$ = "100"
    IF i$ = "" THEN t$ = "item"
    PRINT #2, "<td";
    PRINT #2, " align="; q$; a$; q$;
    PRINT #2, " colspan="; q$; c$; q$;
    PRINT #2, " id="; q$; i$; q$;
    PRINT #2, " width="; q$; c$; "%"; q$; ">"
    IF LEN(l$) > 0 THEN PRINT #2, l$;
    PRINT #2, "</td>"
END SUB

