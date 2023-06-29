DECLARE SUB ResetStarts ()
DECLARE SUB Table (a$, b$, p$, s$, w$)
DECLARE SUB Td (a$, c$, s$, t$, i$)
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask ()
DECLARE SUB Cuckoo ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
    CONST BOOT = "wasteind.bas", MXL = 76, PRGM = "wasteind", WDW = 17
    COMMON SHARED Bgc%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Mxw%
    COMMON SHARED MyFile$
    COMMON SHARED q$
    COMMON SHARED Sday%
    COMMON SHARED Sweekday%
    COMMON SHARED Smonth%
    COMMON SHARED Src$
    COMMON SHARED Syear%
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Md%(12), Wd$(7), Worker$(20)
    ON ERROR GOTO Trap
    Initialize
    ResetStarts
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA 31,28,31,30,31,30,31,31,30,31,30,31
DATA "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"

SUB Bufferize
    DIM c$, d%, k%, m%, o$, t$, w%, x%, y%
    d% = Sday%
    k% = Sweekday%
    m% = Smonth%
    o$ = ""
    y% = Syear%
    w% = 1
    IF k% < 1 THEN k% = 1
    CLOSE : OPEN "wasteind.bfr" FOR OUTPUT AS #2
    OPEN "wasteind.htm" FOR OUTPUT AS #4
    'PRINT #4, "<!doctype html>"
    PRINT #4, "<html>"
    PRINT #4, "<head>"
    PRINT #4, "<title>Waste Industies Schedule</title>"
    'PRINT #4, "<meta charset="; q$; "utf-8"; q$; " name="; q$; "Waste Industries"; q$; " description="; q$; "Work Schedule"; q$; ">"
    PRINT #4, "<style type="; q$; "text/css"; q$; ">"
    PRINT #4, " .mhdr, .text, .rtxt, .gtxt, .btxt {font-family:georgia,"; q$; "times new roman"; q$; ",serif;}"
    PRINT #4, " .mhdr {color:#fff; background:#00f; font-style:normal; font-weight:bold; font-size:30;}"
    PRINT #4, " .text {color:#000; font-style:terminal; font-weight:normal; font-size:20;}"
    PRINT #4, " body {background:#fff;}"
    PRINT #4, "</style>"
    PRINT #4, "</head>"
    PRINT #4, "<body bgcolor="; q$; "#00f"; q$; ">"
    Table "", "1", "5", "", ""
    Td "", "mhdr", "", "", "Waste Industries Work Schedule"
    FOR x% = 1 TO (365 * 5)
    t$ = RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/" + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/" + MID$(STR$(y%), 2)
    PRINT #2, LEFT$(LEFT$(Wd$(k%), 3) + " " + RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/" + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/" + MID$(STR$(y%), 2) + ": " + Worker$(w%) + STRING$(MXL, 46), MXL)
    Td "<", "", "12", "<", Wd$(k%)
    Td "", "", "12", "", RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/" + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/" + MID$(STR$(y%), 2)
    Td "<", "", "76", ">", Worker$(w%)
    'PRINT #4, Wd$(k%); " "; RIGHT$("00" + MID$(STR$(m%), 2), 2); "/"; RIGHT$("00" + MID$(STR$(d%), 2), 2); "/" + MID$(STR$(y%), 2); ": "; Worker$(w%) + "<br>"
    SELECT CASE VAL(RIGHT$(MID$(STR$(y%), 2), 2))
        CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: Md%(2) = 29
        CASE ELSE: Md%(2) = 28
    END SELECT
    d% = d% + 1
    IF d% > Md%(m%) THEN d% = 1: m% = m% + 1
    IF m% > 12 THEN m% = 1: y% = y% + 1
    w% = w% + 1: IF w% > Mxw% THEN w% = 1
    k% = k% + 1: IF k% > 7 THEN k% = 1
    NEXT
    PRINT #4, "</table>"
    PRINT #4, "</body>"
    PRINT #4, "</html>"
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Default (i$)
    DIM a%, k$
    SayIt "Error: " + i$ + "!", Echo%, 14, Bgc%
    SayIt Prompt$, 22, Fgc%, Bgc%
    Cuckoo
    DO
    Getkey a%, k$
    LOOP
    END
END SUB

SUB Getkey (a%, k$)
    DO
    k$ = INKEY$
    LOOP UNTIL k$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN
        END
    ELSEIF k$ = "*" THEN
        RUN BOOT
    END IF
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    Fgc% = 15
    Mxw% = 0
    MyFile$ = "myfiles\myfiles" + LCASE$(RIGHT$(BOOT, 4))
    Path$ = ""
    q$ = CHR$(34)
    'Sday% = 1
    'Sweekday% = 3
    'Smonth% = 1
    'Syear% = 2008
    Title$ = "Waste Industries Schedule"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    Flt% = 0
    CLOSE : OPEN "wasteind.ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "WORKER="
                    Mxw% = Mxw% + 1
                    SELECT CASE Mxw%
                        CASE 1 TO 20: Worker$(Mxw%) = MID$(i$, x% + 1)
                        CASE ELSE: Mxw% = 20
                    END SELECT
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
    FOR x% = 1 TO 12: READ Md%(x%): NEXT
    FOR x% = 1 TO 7: READ Wd$(x%): NEXT
END SUB

SUB Main
    DIM a%, e%, k$, s%
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    Mask
    CLOSE : OPEN "wasteind.bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey a%, k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(8)
            CHDIR LEFT$(MyFile$, LEN(MyFile$) - 12)
            RUN MyFile$
            CHDIR LEFT$(Src$, LEN(Src$) - 1)
            RUN BOOT
        CASE CHR$(9): SHELL "notepad.exe wasteind.ini": RUN BOOT
        CASE CHR$(13): s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - WDW + 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + WDW - 1: EXIT DO
        CASE CHR$(0) + "I": s% = s% - WDW + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = s% + WDW - 1: EXIT DO
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
    CLS
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, WDW + 4: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " ù " + Today$, 2, 11, Bgc%
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù BACKSPACE to Exit ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB ResetStarts
    DIM d%, k%, m%, x%, y%
    d% = 1
    k% = 3
    m% = 1
    o$ = ""
    y% = 2008
    IF k% < 1 THEN k% = 1
    DO
    SELECT CASE VAL(RIGHT$(MID$(STR$(y%), 2), 2))
        CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: Md%(2) = 29
        CASE ELSE: Md%(2) = 28
    END SELECT
    d% = d% + 1
    IF d% > Md%(m%) THEN d% = 1: m% = m% + 1
    IF m% > 12 THEN m% = 1: y% = y% + 1
    k% = k% + 1: IF k% > 7 THEN k% = 1
    IF m% = VAL(DATE$) AND d% = VAL(MID$(DATE$, 4)) AND y% = VAL(MID$(DATE$, 7)) THEN EXIT DO
    LOOP
    Sday% = d%
    Sweekday% = k%
    Smonth% = m%
    Syear% = y%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$, w%
    w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, d AS STRING * MXL, f%, i$, x%
    FOR x% = 1 TO WDW
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, d
        CASE e% + 1: d = STRING$(MXL, 205)
        CASE ELSE: d = STRING$(MXL, 32)
    END SELECT
    i$ = LEFT$(d, MXL)
    f% = 10
    b% = Bgc%
    SayIt i$, x% + 3, f%, b%
    NEXT
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
    PRINT #4, " align="; q$; a$; q$;
    PRINT #4, " border="; q$; b$; q$;
    PRINT #4, " cellpadding="; q$; p$; q$;
    PRINT #4, " cellspacing="; q$; s$; q$;
    PRINT #4, " width="; q$; w$; "%"; q$; ">"
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
    PRINT #4, " align="; q$; a$; q$;
    PRINT #4, " class="; q$; c$; q$;
    PRINT #4, " colspan="; q$; s$; q$;
    PRINT #4, " width="; q$; s$; "%"; q$; ">"
    IF LEN(i$) > 0 THEN
        PRINT #4, i$; "</td>"
        IF RIGHT$(t$, 1) = ">" THEN PRINT #4, "</tr>"
    END IF
END SUB

