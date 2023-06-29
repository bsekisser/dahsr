DECLARE SUB Htmlize ()
DECLARE SUB Header ()
DECLARE SUB Footer ()
DECLARE SUB Display (k$)
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s AS DOUBLE, e AS DOUBLE)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB GetKey (k$)
DECLARE SUB Main ()
DECLARE SUB Search (s AS DOUBLE, e AS DOUBLE)
    CONST APP = "decoder.bas"
    CONST BFR = "decoder.bfr"
    CONST KWL = "decoder.kwd"
    CONST INI = "decoder.ini"
    CONST LIB = "decoder.lib"
    CONST MXW = 76
    CONST WSZ = 10
    CONST ECHO = WSZ + 5
    COMMON SHARED Bgc%
    COMMON SHARED Bkg$
    COMMON SHARED Byte AS STRING * 2
    COMMON SHARED Alphabet$
    COMMON SHARED Dbse$
    COMMON SHARED Hfam$
    COMMON SHARED Htm$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED High%
    COMMON SHARED Interval%
    COMMON SHARED Kwd$
    COMMON SHARED Mxe AS DOUBLE
    COMMON SHARED Opcode%
    COMMON SHARED Src$
    COMMON SHARED Span%
    COMMON SHARED Start%
    COMMON SHARED Title$
    COMMON SHARED Today$
    COMMON SHARED Url$
    COMMON SHARED Wide%
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DG1:
DATA "INSERT the Source Document into the Buffer"
DATA "(A Large File Will Take a Few Minutes!)"
DATA ""

DG2:
DATA "The Document Source Label (SOURCE=) Cannot be Determined."
DATA "The Label May Not Have Been Assigned a Source or"
DATA "The Label May Have Been DeSelected (') in the 'INI' File."
DATA "Edit the 'INI' File to Make Any Corrections!"
DATA ""

DG3:
DATA "Cannot Find The Source Document in the Library!"
DATA "Press F2 to View Source Library, Make a Note of"
DATA "the Files and Then Exit the Editor and Return"
DATA "to the Decoder Application. Then Press F1"
DATA "to Enter a Correct Library Source Directory/Name."
DATA ""

SUB Bufferize
    DIM i%, h$, l$, s%, x%, y%, z%
    KILL LIB
    Flt% = 0
    Mxe = 0
    IF Src$ = "" THEN Src$ = "?"
    IF Src$ = "?" THEN
        SHELL "dir > " + LIB
        CLOSE : OPEN BFR FOR OUTPUT AS #2
        RESTORE DG2
        DO
        READ l$
        IF l$ = "" THEN
            EXIT DO
        ELSE
            FOR x% = 1 TO LEN(l$)
            PRINT #2, RIGHT$("?" + MID$(l$, x%, 1), LEN(Byte))
            NEXT
        END IF
        LOOP
    ELSEIF LEN(Src$) > 0 THEN
        Flt% = 0
        CLOSE : OPEN Src$ FOR INPUT AS #1
        IF Flt% = 0 THEN
            FOR x% = LEN(Src$) TO 1 STEP -1
            SELECT CASE MID$(Src$, x%, 1)
                CASE "/", "\"
                    SHELL "dir " + LEFT$(Src$, x% - 1) + " > " + LIB + " /B"
                    EXIT FOR
            END SELECT
            NEXT
            i% = 0
            s% = 0
            SayIt "Standby, Buffering Source!", ECHO, 30, Bgc%
            Flt% = 0
            CLOSE
            OPEN Src$ FOR INPUT AS #1
            OPEN BFR FOR OUTPUT AS #2
            WHILE NOT EOF(1)
                LINE INPUT #1, l$
                FOR x% = 1 TO LEN(l$)
                s% = s% + 1
                IF s% >= Start% THEN
                    s% = Start%
                    FOR y% = 1 TO LEN(Alphabet$)
                    SELECT CASE UCASE$(MID$(l$, x%, 1))
                        CASE UCASE$(MID$(Alphabet$, y%, 1))
                            i% = i% + 1
                            o$ = "<"
                            IF i% = Interval% THEN
                                Mxe = Mxe + 1
                                i% = 0
                                FOR z% = 1 TO LEN(Kwd$)
                                IF UCASE$(MID$(l$, x%, 1)) = UCASE$(MID$(Kwd$, z%, 1)) THEN o$ = ">": EXIT FOR
                                NEXT
                                SELECT CASE Opcode%
                                    CASE 0: PRINT #2, RIGHT$(o$ + UCASE$(MID$(l$, x%, 1)), LEN(Byte))
                                    CASE 1: PRINT #2, RIGHT$(o$ + LCASE$(MID$(l$, x%, 1)), LEN(Byte))
                                    CASE ELSE: PRINT #2, RIGHT$(o$ + MID$(l$, x%, 1), LEN(Byte))
                                END SELECT
                            END IF
                            EXIT FOR
                    END SELECT
                    NEXT
                END IF
                NEXT
            WEND
            Htmlize
        ELSE
            Flt% = 0
            FOR x% = LEN(Src$) TO 1 STEP -1
            SELECT CASE MID$(Src$, x%, 1)
                CASE "/", "\"
                    SHELL "dir " + LEFT$(Src$, x% - 1) + " > " + LIB + " /B"
                    EXIT FOR
            END SELECT
            NEXT
            CLOSE : OPEN BFR FOR OUTPUT AS #2
            RESTORE DG3
            DO
            READ l$
            IF l$ = "" THEN
                EXIT DO
            ELSE
                l$ = LEFT$(STRING$(Wide% / 2 - (LEN(l$) / 2), 46) + l$ + STRING$(Wide%, 46), Wide%)
                FOR x% = 1 TO LEN(l$)
                PRINT #2, RIGHT$("#" + MID$(l$, x%, 1), LEN(Byte))
                NEXT
            END IF
            LOOP
        END IF
    END IF
    CLOSE
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Display (k$)
    DIM a%, e AS DOUBLE, s AS DOUBLE
    CLOSE : OPEN BFR FOR RANDOM AS #1 LEN = LEN(Byte) + 2
    e = LOF(1) / (LEN(Byte) + 2)
    s = 1
DL1:
    SayIt "ù Document Source:" + CHR$(34) + Src$ + CHR$(34) + " ù", ECHO, 11, Bgc%
    SayIt "ù Keyword (Keyphrase): " + CHR$(34) + Kwd$ + CHR$(34) + " ù", ECHO + 1, 11, Bgc%
    DO
    IF s > e THEN s = e
    IF s < 1 THEN s = 1
    IF Wide% < 1 THEN Wide% = 1
    IF Wide% > MXW THEN Wide% = MXW
    Span% = High% * Wide%
    SayIt "ù Window <" + MID$(STR$(High%), 2) + "/" + MID$(STR$(Wide%), 2) + "> Spans from " + MID$(STR$(s), 2) + " to " + MID$(STR$(s + Span% - 1), 2) + " ù Document Length: " + MID$(STR$(e), 2) + " ù", ECHO + 2, 11, Bgc%
    SayIt "ù Character Start: " + MID$(STR$(Start%), 2) + " ù Character Interval: " + MID$(STR$(Interval%), 2) + " ù", ECHO + 3, 11, Bgc%
    Scroll s, e
    DO
    GetKey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(8): s = s - Span% + 1: EXIT DO
        CASE CHR$(9)
            IF Bkg$ = "ON" THEN
                Bkg$ = "OFF"
            ELSEIF Bkg$ = "OFF" THEN
                Bkg$ = "ON"
            ELSE
                Bkg$ = "ON"
            END IF
            EXIT DO
        CASE CHR$(13): Search s, e: GOTO DL1
        CASE ",", "<": Wide% = Wide% - 1: EXIT DO
        CASE ".", ">": Wide% = Wide% + 1: EXIT DO
        CASE CHR$(0) + ";": SHELL "notepad.exe " + INI: RUN APP
        CASE CHR$(0) + "<": SHELL "notepad.exe " + LIB: EXIT DO
        CASE "-": s = s - 1: EXIT DO
        CASE "+": s = s + 1: EXIT DO
        CASE CHR$(0) + "G": s = 1: GOTO DL1
        CASE CHR$(0) + "H": s = s - Wide%: EXIT DO
        CASE CHR$(0) + "K": s = s - 1: EXIT DO
        CASE CHR$(0) + "M": s = s + 1: EXIT DO
        CASE CHR$(0) + "O": s = e: EXIT DO
        CASE CHR$(0) + "P": s = s + Wide%: EXIT DO
        CASE CHR$(0) + "I": s = s - Span% + Wide%: EXIT DO
        CASE CHR$(0) + "Q": s = s + Span% - Wide%: EXIT DO
        CASE CHR$(0) + "R": EXIT SUB
    END SELECT
    LOOP
    LOOP
END SUB

SUB Footer
    DIM q$
    PRINT #2, "</td></tr>"
    IF LEN(Url$) > 0 THEN
        PRINT #2, "<tr><td";
        PRINT #2, " align="; q$; "center"; q$;
        PRINT #2, " colspan="; q$; MID$(STR$(Wide%), 2); q$;
        PRINT #2, " class="; q$; "itl"; q$;
        PRINT #2, " width="; q$; "100%"; q$; ">"
        PRINT #2, Url$
        PRINT #2, "</td></tr>"
    END IF
    PRINT #2, "</table>"
    PRINT #2, "</body>"
    PRINT #2, "</html>"
END SUB

SUB GetKey (k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF k$ = CHR$(27) THEN
        END
    END IF
END SUB

SUB Header
    DIM q$
    q$ = CHR$(34)
    PRINT #2, "<html>"
    PRINT #2, "<head>"
    PRINT #2, "<title>"; Title$; "</title>"
    PRINT #2, "<link href="; q$; "bible.css"; q$; " rel=stylesheet type="; q$; "text/css"; q$; ">"
    'PRINT #2, "<style type="; q$; "text/css"; q$; ">"
    'PRINT #2, "   #hdr  {"
    'PRINT #2, "         color:#0000ff;"
    'PRINT #2, "         font-style:normal;"
    'PRINT #2, "         font-family:"; q$; Hfam$; q$; ";"
    'PRINT #2, "         font-weight:bold;"
    'PRINT #2, "         font-size:30; }"
    'PRINT #2, "   #txt {"
    'PRINT #2, "         color:#000000;"
    'PRINT #2, "         font-style:normal;"
    'PRINT #2, "         font-family:terminal;"
    'PRINT #2, "         font-weight:bold;"
    'PRINT #2, "         font-size:22; }"
    'PRINT #2, "   #itl {"
    'PRINT #2, "         color:#000000;"
    'PRINT #2, "         font-style:italic;"
    'PRINT #2, "         font-family:terminal;"
    'PRINT #2, "         font-weight:normal;"
    'PRINT #2, "         font-size:12; }"
    'PRINT #2, "</style>"
    PRINT #2, "</head>"
    PRINT #2, "<body color="; q$; "#ddff00"; q$; ">"
    PRINT #2, ""
    PRINT #2, "<table";
    PRINT #2, " border="; q$; "0"; q$;
    PRINT #2, " cellpadding="; q$; "10"; q$;
    PRINT #2, " cellspacing="; q$; "10"; q$;
    PRINT #2, " width="; q$; "100%"; q$; ">"
    PRINT #2, ""
    PRINT #2, "<td";
    PRINT #2, " align="; q$; "center"; q$;
    PRINT #2, " class="; q$; "hdr"; q$;
    PRINT #2, " width="; q$; "100%"; q$; ">"
    PRINT #2, "Document Source:<br>"
    PRINT #2, CHR$(34) + Src$ + CHR$(34) + "<br>"
    PRINT #2, "Character Start: " + MID$(STR$(Start%), 2) + ", Character Interval: " + MID$(STR$(Interval%), 2); "<br>"
    PRINT #2, "Line Length: " + MID$(STR$(Wide%), 2); " Characters<br>"
    PRINT #2, "Document Length: " + MID$(STR$(Mxe), 2) + " Characters."
    PRINT #2, "</td>"
    PRINT #2, "<tr><td";
    PRINT #2, " align="; q$; "center"; q$;
    PRINT #2, " class="; q$; "txt"; q$;
    PRINT #2, " width="; q$; "100%"; q$; ">"
END SUB

SUB Htmlize
    DIM h$, t$, x%
    SayIt "Standby, Creating html Page!", ECHO, 30, Bgc%
    CLOSE
    OPEN BFR FOR INPUT AS #1
    OPEN Htm$ FOR OUTPUT AS #2
    Header
    WHILE NOT EOF(1)
    h$ = ""
    FOR x% = 1 TO Wide%
    INPUT #1, Byte
    IF Flt% = 0 THEN
        h$ = h$ + RIGHT$(Byte, 1)
    ELSE
        Byte$ = ".."
    END IF
    NEXT
    PRINT #2, LEFT$(h$ + STRING$(Wide%, 46), Wide%); "<br>"
    WEND
    Footer
    CLOSE
    Flt% = 0
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    Bkg$ = "ON"
    Dbse$ = "dbse\"
    Fgc% = 15
    Flt% = 0
    Alphabet$ = "abcdefghijklmnopqrstuvwxyz"
    Hfam$ = "times new roman"
    High% = WSZ
    Htm$ = "decoder.htm"
    Kwd$ = "?"
    Interval% = 1
    Opcode% = 0
    Start% = 1
    Src$ = "?"
    Title$ = "Basic Document Decoder"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 2)
    Url$ = ""
    Wide% = 76
    CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "ALPHABET=": Alphabet$ = MID$(i$, x% + 1): EXIT FOR
                CASE "FAMILY=": Hfam$ = MID$(i$, x% + 1): EXIT FOR
                CASE "HIGH=": High% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "HTML=": Htm$ = MID$(i$, x% + 1): EXIT FOR
                CASE "INTERVAL=": Interval% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "KEYWORD=": Kwd$ = MID$(i$, x% + 1): EXIT FOR
                CASE "OPCODE=": Opcode% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "SOURCE=": Src$ = MID$(i$, x% + 1): EXIT FOR
                CASE "START=": Start% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "TITLE=": Title$ = MID$(i$, x% + 1): EXIT FOR
                CASE "URL=": Url$ = MID$(i$, x% + 1): EXIT FOR
                CASE "WIDE=": Wide% = VAL(MID$(i$, x% + 1)): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE
    IF Interval% < 1 THEN Interval% = 1
    IF High% < 1 OR High% > WSZ THEN High% = WSZ
    IF Wide% < 1 OR Wide% > 76 THEN Wide% = 76
    IF Start% < 1 THEN Start% = 1
    IF LCASE$(RIGHT$(Htm$, 4)) <> ".htm" THEN Htm$ = Htm$ + ".htm"
    IF LCASE$(Htm$) = LCASE$(APP) THEN Htm$ = "decoder.htm"
    OPEN BFR FOR OUTPUT AS #2
    RESTORE DG1
    DO
    READ i$
    IF i$ = "" THEN
        EXIT DO
    ELSE
        i$ = LEFT$(STRING$(Wide% / 2 - (LEN(i$) / 2), 46) + i$ + STRING$(Wide%, 46), Wide%)
        FOR x% = 1 TO LEN(i$)
        PRINT #2, RIGHT$("!" + MID$(i$, x%, 1), LEN(Byte))
        NEXT
    END IF
    LOOP
    CLOSE
END SUB

SUB Main
    DIM k$
    Mask
    DO
    SELECT CASE k$
        CASE CHR$(0) + "R": Bufferize
    END SELECT
    Display k$
    LOOP
END SUB

SUB Mask
    DIM x%
    COLOR 0, 0
    CLS
    COLOR Fgc%, Bgc%
    LOCATE 1, 1, 0
    FOR x% = 1 TO 23
    LOCATE x%, 1, 0
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, WSZ + 4, ECHO + 4: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " ù " + Today$, 2, 11, Bgc%
    SayIt "ù Document Source: " + CHR$(34) + Src$ + CHR$(34) + " ù", ECHO, 11, Bgc%
    SayIt "ù Keyword (Keyphrase): ? ù", ECHO + 1, 11, Bgc%
    SayIt "ù Window <?/?> Spans from ? to ? ù Document Length: ? ù", ECHO + 2, 11, Bgc%
    SayIt "ù Character Start: ? ù Character Interval: ? ù", ECHO + 3, 11, Bgc%
    SayIt "ù Scroll Window (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù PAGE Window ù", 20, Fgc%, Bgc%
    SayIt "ù ENTER Keyword Search ù Press INSERT to Load Source ù", 21, Fgc%, Bgc%
    SayIt "ù TAB Toggles Highlight ù F1:Edit " + CHR$(34) + INI + CHR$(34) + " ù F2:Library ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB MsgBox
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + l$, 38)
    r$ = LEFT$(r$ + STRING$(38, 32), 38)
    COLOR f%, b%
    LOCATE v%, 3, 0
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s AS DOUBLE, e AS DOUBLE)
    DIM b%, f%, h%, v%, x%, y%
    x% = 0
    FOR v% = 1 TO High%
    FOR h% = 1 TO Wide%
    x% = x% + 1
    SELECT CASE s + x% - 1
        CASE 1 TO e: GET #1, s + x% - 1, Byte
        CASE ELSE: Byte = ".."
    END SELECT
    IF Bkg$ = "ON" THEN
        f% = 10
        b% = Bgc%
    ELSEIF Bkg$ = "OFF" THEN
        f% = Bgc%
        b% = Bgc%
    ELSE
        f% = 10
        b% = Bgc%
    END IF
    SELECT CASE UCASE$(LEFT$(Byte, 1))
        CASE "<"
            IF Bkg$ = "ON" THEN
                f% = 10
                b% = Bgc%
            ELSEIF Bkg$ = "OFF" THEN
                f% = Bgc%
                b% = Bgc%
            END IF
        CASE ">"
            IF Bkg$ = "ON" THEN
                f% = 15
                b% = 4
            ELSEIF Bkg$ = "OFF" THEN
                f% = 15
                b% = Bgc%
            END IF
        CASE "!": f% = 11: b% = Bgc%
        CASE "?": f% = 14: b% = Bgc%
        CASE "#": f% = 15: b% = 4
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    COLOR f%, b%
    LOCATE v% + 3, h% + 2
    PRINT RIGHT$(Byte, 1);
    COLOR Fgc%, Bgc%
    SELECT CASE MXW - Wide%
        CASE 0: PRINT " º"
        CASE MXW - 1: PRINT " º>" + STRING$(MXW - Wide% - 1, 32)
        CASE ELSE: PRINT "<º>" + STRING$(MXW - Wide% - 1, 32)
    END SELECT
    NEXT
    NEXT
END SUB

SUB Search (s AS DOUBLE, e AS DOUBLE)
    DIM k$, x AS DOUBLE, y%, z%
    IF Kwd$ = "?" THEN EXIT SUB
    SayIt "ù Searching for Keyword (Keyphrase): " + CHR$(34) + Kwd$ + CHR$(34) + " ù", ECHO + 1, 30, Bgc%
    x = s
    DO
    k$ = INKEY$
    IF k$ = CHR$(13) THEN
        EXIT DO
    ELSEIF k$ = CHR$(27) THEN
        END
    END IF
    x = x + 1
    IF x < 1 THEN x = e
    IF x > e THEN x = 1
    IF x = s THEN EXIT DO
    GET #1, x, Byte
    IF UCASE$(RIGHT$(Byte, 1)) = UCASE$(LEFT$(Kwd$, 1)) THEN
        DO
        FOR y% = 1 TO LEN(Kwd$)
        GET #1, x + y% - 1, Byte
        IF UCASE$(RIGHT$(Byte, 1)) <> UCASE$(MID$(Kwd$, y%, 1)) THEN EXIT DO
        NEXT
        s = x
        EXIT SUB
        LOOP
    END IF
    LOOP
END SUB

