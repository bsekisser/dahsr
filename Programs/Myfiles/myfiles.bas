DECLARE SUB Display ()
DECLARE SUB Help ()
DECLARE SUB MtTrash ()
DECLARE SUB Delete ()
DECLARE SUB ReplaceDataFiles ()
DECLARE SUB LoadLink ()
DECLARE SUB LoadStyle (m$)
DECLARE SUB ValidateINIFile ()
DECLARE SUB CreateIniFile ()
DECLARE SUB CreateRemoteFile ()
DECLARE SUB CreateTextFile ()
DECLARE SUB CreateCssFile ()
DECLARE SUB ValidateREMOTEFile ()
DECLARE SUB ValidateCSSFile ()
DECLARE SUB ValidateTEXTFile ()
DECLARE SUB Htmlize ()
DECLARE SUB Search (k$, s%, e%)
DECLARE SUB Launch ()
DECLARE SUB Bufferize ()
DECLARE SUB Populate (s$, m%)
DECLARE SUB Scroll (s%, e%, p%)
DECLARE SUB Mask (m%)
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB LoadRemotes ()
    CONST BOOT = "myfiles.bas"
    CONST CSS = "myfiles.css"
    CONST HTML = "myfiles.htm"
    CONST INI = "myfiles.ini"
    CONST MXL = 76
    CONST PRGM = "myfiles"
    CONST REMOTE = "myfiles.rem"
    CONST TEXT = "myfiles.txt"
    CONST WSZ = 15
    COMMON SHARED Bgc%
    COMMON SHARED Ca$
    COMMON SHARED Cb$
    COMMON SHARED Dbse$
    COMMON SHARED Directory$
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Ext$
    COMMON SHARED Fgc%
    COMMON SHARED Filename$
    COMMON SHARED Flt%
    COMMON SHARED Mhdr$
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Src$
    COMMON SHARED Shdr$
    COMMON SHARED Tf$
    COMMON SHARED Th$
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Mth(12) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Htmlize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DBA:
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"

SUB Bufferize
    DIM x%
    Mask 1
    FOR x% = 4 TO 20
    SayIt "ù ù Populating File List ù ù", x%, 14, Bgc%
    NEXT
    MKDIR "temp"
    CLOSE
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    Populate Src$, 0
    PRINT #2, STRING$(MXL, 196)
    PRINT #2, LEFT$("Total Files" + STRING$(MXL, 46), MXL - LEN(Tf$)); Tf$
    CLOSE
    KILL "temp\*.*"
    RMDIR "temp"
END SUB

SUB CreateCssFile
    CLOSE #2: OPEN CSS FOR OUTPUT AS #2
    LoadStyle ""
    CLOSE #2
END SUB

SUB CreateIniFile
    CLOSE #2: OPEN INI FOR OUTPUT AS #2
    PRINT #2, "EXT="; Ext$
    PRINT #2, "MAINHEADER="; Mhdr$
    PRINT #2, "SUBHEADER="; Shdr$
    PRINT #2, "TITLE="; Title$
    CLOSE #2
END SUB

SUB CreateRemoteFile
    CLOSE #2: OPEN REMOTE FOR OUTPUT AS #2
REM Add "html" address' here!
    CLOSE #2
END SUB

SUB CreateTextFile
    CLOSE #2: OPEN TEXT FOR OUTPUT AS #2
    PRINT #2, "<!doctype html>"
    PRINT #2, "<html lang="; q; "en"; q; ">"
    PRINT #2, "<head>"
    PRINT #2, "<meta charset="; q; "utf-8"; q; ">"
    PRINT #2, "<title>"
    PRINT #2, "[TITLE]"
    PRINT #2, "</title>"
    PRINT #2, "[LINK]"
    PRINT #2, "[STYLE]"
    PRINT #2, "</head>"
    PRINT #2, "<body>"
    PRINT #2, "<div id="; q; "shell"; q; ">"
    PRINT #2, TAB(4); "<div class="; q; "mhdr"; q; ">"
    PRINT #2, "[MHDR]"
    PRINT #2, TAB(4); "</div>"
    PRINT #2, TAB(4); "<div class="; q; "shdr"; q; ">"
    PRINT #2, TAB(4); "Source Directory &quot;"
    PRINT #2, "[SOURCE]"
    PRINT #2, TAB(4); "&quot;</div>"
    PRINT #2, TAB(4); "<div id="; q; "lpane"; q; ">"
    PRINT #2, TAB(4); "<div class="; q; "local"; q; ">Local</div>"
    PRINT #2, TAB(8); "<div class="; q; "text"; q; ">"
    PRINT #2, TAB(8); "<ul>"
    PRINT #2, "[POPULATE]"
    PRINT #2, TAB(8); "</ul>"
    PRINT #2, TAB(8); "</div>"
    PRINT #2, TAB(4); "</div>"
    PRINT #2, TAB(4); "<div id="; q; "rpane"; q; ">"
    PRINT #2, TAB(4); "<div class="; q; "remote"; q; ">Remote</div>"
    PRINT #2, TAB(8); "<div class="; q; "text"; q; ">"
    PRINT #2, TAB(8); "<ul>"
    PRINT #2, "[REMOTE]"
    PRINT #2, TAB(8); "</ul>"
    PRINT #2, TAB(8); "</div>"
    PRINT #2, TAB(4); "</div>"
    PRINT #2, TAB(4); "<div class="; q; "date"; q; ">"
    'PRINT #2, "[DATE]"
    'PRINT #2, "[TIME]"
    PRINT #2, "[DATE/TIME]"
    PRINT #2, TAB(4); "</div>"
    PRINT #2, "</div>"
    PRINT #2, "</body>"
    PRINT #2, "</html>"
    CLOSE #2
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Delete
    KILL CSS
    KILL INI
    KILL REMOTE
    KILL TEXT
END SUB

SUB Display
    DIM e%, p%, s%
    Mask 0
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    p% = 1
    s% = 1
    DO
    IF p% < 1 THEN p% = 1: s% = s% - 1
    IF p% > WSZ THEN p% = WSZ: s% = s% + 1
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%, p%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + TEXT: RUN BOOT
        CASE CHR$(13): Launch: EXIT DO
        CASE CHR$(0) + CHR$(15): SHELL "notepad.exe " + INI: RUN BOOT
        CASE CHR$(0) + ";": Help: EXIT SUB
        CASE CHR$(0) + "<": SHELL "notepad.exe " + REMOTE: RUN BOOT
        CASE CHR$(0) + "=": SHELL "notepad.exe " + CSS: RUN BOOT
        CASE CHR$(0) + ">": ReplaceDataFiles: RUN BOOT
        CASE CHR$(0) + "G": p% = 1: s% = 1: EXIT DO
        CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
        CASE CHR$(0) + "I": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "K": p% = 1: s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "M": p% = 1: s% = s% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "O": p% = 1: s% = e%: EXIT DO
        CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = s% + WSZ - 1: EXIT DO
        CASE "A" TO "Z": Search k$, s%, e%: p% = 1: EXIT DO
        CASE ELSE: Help: EXIT SUB
    END SELECT
    LOOP
    LOOP
END SUB

SUB Getkey (k$)
    DO
    k$ = INKEY$
    LOOP UNTIL k$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF ASC(k$) = 27 THEN
        MtTrash
        END
    ELSEIF k$ = "*" THEN
        MtTrash
        RUN BOOT
    END IF
END SUB

SUB Help
    DIM k$, m%, x%
    Mask 1
    SayIt Title$ + " General Information", 2, 11, Bgc%
    SayIt LEFT$("Key Press : Function" + STRING$(MXL, 46), MXL - 4) + "File", 4, 11, Bgc%
    SayIt STRING$(MXL, 45), 5, Fgc%, Bgc%
    SayIt LEFT$("TAB       : Edit" + STRING$(MXL, 46), MXL - LEN(TEXT) - 2) + q + TEXT + q, 6, 10, Bgc%
    SayIt LEFT$("SHIFT/TAB : Edit" + STRING$(MXL, 46), MXL - LEN(INI) - 2) + q + INI + q, 7, 10, Bgc%
    SayIt LEFT$("F1        : Help and General Information" + STRING$(MXL, 46), MXL), 8, Fgc%, Bgc%
    SayIt LEFT$("F2        : Edit" + STRING$(MXL, 46), MXL - LEN(REMOTE) - 2) + q + REMOTE + q, 9, 10, Bgc%
    SayIt LEFT$("F3        : Edit" + STRING$(MXL, 46), MXL - LEN(CSS) - 2) + q + CSS + q, 10, 10, Bgc%
    SayIt LEFT$("F4        : Reinitialize Data Files" + STRING$(MXL, 46), MXL), 11, 14, Bgc%
    
    SayIt "Press Any Key to Exit or ESC to End", 22, Fgc%, Bgc%
    DO
    Getkey k$
    SELECT CASE k$
        CASE CHR$(9): SHELL "notepad.exe " + TEXT: RUN BOOT
        CASE CHR$(0) + CHR$(15): SHELL "notepad.exe " + INI: RUN BOOT
        CASE CHR$(0) + "<": SHELL "notepad.exe " + REMOTE: RUN BOOT
        CASE CHR$(0) + "=": SHELL "notepad.exe " + CSS: RUN BOOT
        CASE CHR$(0) + ">": ReplaceDataFiles: RUN BOOT
        CASE ELSE: EXIT DO
    END SELECT
    LOOP
END SUB

SUB Htmlize
    DIM i$, x%
    FOR x% = 4 TO 20
    SayIt "ù ù Updating " + q + LCASE$(PRGM) + ".htm" + q + " Page ù ù", x%, 14, Bgc%
    NEXT
    MKDIR "temp"
    CLOSE : OPEN TEXT FOR INPUT AS #1
    OPEN HTML FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    i$ = RTRIM$(i$)
    SELECT CASE UCASE$(i$)
        CASE "[TITLE]": PRINT #2, Title$
        CASE "[MHDR]": PRINT #2, Mhdr$
        CASE "[LINK]": LoadLink
        CASE "[STYLE]", "[STYLES]": LoadStyle "+"
        CASE "[REMOTE]", "[REMOTES]": LoadRemotes
        CASE "[SHDR]": PRINT #2, Shdr$
        CASE "[SOURCE]": PRINT #2, Src$
        CASE "[LOCAL]", "[LOCALS]", "[POPULATE]": Populate Src$, 1
        CASE "[DATE]": PRINT #2, Today$
        CASE "[TIME]": PRINT #2, LEFT$(TIME$, 5)
        CASE "[DATE/TIME]": PRINT #2, Today$; " at "; LEFT$(TIME$, 5)
        CASE "[TIME/DATE]": PRINT #2, LEFT$(TIME$, 5); " on "; Today$
        CASE ELSE: PRINT #2, i$
    END SELECT
    WEND
    CLOSE
    KILL "temp\*.*"
    RMDIR "temp"
END SUB

SUB Initialize
    DIM i$, x%, y%
    RESTORE DBA
    FOR x% = 1 TO 12: READ Mth(x%): NEXT
    Bgc% = 1
    Ca$ = "83"
    Cb$ = "17"
    Dbse$ = "dbse\"
    Echo% = WSZ + 5
    Ext$ = ""
    Fgc% = 15
    q = CHR$(34)
    Shdr$ = "My DOS Files"
    Src$ = ""
    Tf$ = "0"
    Title$ = "My DOS Files"
    Mhdr$ = "My DOS Files"
    Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
    ValidateCSSFile
    ValidateINIFile
    ValidateREMOTEFile
    ValidateTEXTFile
    SHELL "dir > src.shl"
    'SHELL "notepad.exe src.shl": END
    OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE : KILL "src.shl"
    i$ = MID$(i$, 15)
    Src$ = LCASE$(LEFT$(i$, LEN(i$) - LEN(PRGM) - 1))
    FOR x% = 1 TO LEN(i$)
    SELECT CASE UCASE$(MID$(i$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
    IF LCASE$(Ext$) = LCASE$(RIGHT$(BOOT, 3)) THEN Ext$ = ""
    EXIT SUB
    CLS
    PRINT "Source: "; Src$
    END
END SUB

SUB Launch
    DO
    IF Src$ = "" OR Filename$ = "" THEN
        EXIT DO
    ELSEIF LCASE$(RIGHT$(Filename$, 4)) = ".txt" THEN
        SHELL "notepad.exe " + Src$ + "\" + Directory$ + "\" + Filename$
        RUN BOOT
    ELSEIF LCASE$(RIGHT$(Filename$, 4)) = LCASE$(RIGHT$(BOOT, 4)) THEN
        CHDIR Src$ + "\" + Directory$
        RUN Src$ + "\" + Directory$ + "\" + Filename$
        CHDIR Src$ + "\" + PRGM
        EXIT DO
    ELSE
        EXIT DO
    END IF
    LOOP
END SUB

SUB LoadLink
    PRINT #2, "<link href="; q; CSS; q; " rel="; q; "stylesheet"; q; " type="; q; "text/css"; q; ">"
END SUB

SUB LoadRemotes
    DIM i$
    CLOSE #3: OPEN REMOTE FOR INPUT AS #3
    WHILE NOT EOF(3)
    LINE INPUT #3, i$
    IF i$ = "" THEN
        i$ = ""
    ELSE
        PRINT #2, "<li>"; i$; "</li>"
    END IF
    WEND
    CLOSE #3
END SUB

SUB LoadStyle (m$)
    DIM dt$, lr$, mf$, sf$, tx$
    dt$ = "14": lr$ = "20": mf$ = "30": sf$ = "18": tx$ = "16"
    IF m$ = "+" THEN PRINT #2, "<style type="; q; "text/css"; q; ">"
    PRINT #2, "A {color:#00f; text-decoration:none; font-size:"; tx$; "px;}"
    PRINT #2, "A:hover {color:#fff; background:#00f;}"
    PRINT #2, "BODY"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#ddd;"
    PRINT #2, TAB(4); "font-family:georgia,"; q; "times new roman"; q; ",trebuchet,serif;"
    PRINT #2, TAB(4); "font-type:normal;"
    PRINT #2, TAB(4); "}"
    PRINT #2, "LI"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "indent:25px;"
    PRINT #2, TAB(4); "}"
    PRINT #2, "#shell"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#fff;"
    PRINT #2, TAB(4); "border:1px solid #000;"
    PRINT #2, TAB(4); "margin:20px auto;"
    PRINT #2, TAB(4); "width:900px;"
    PRINT #2, TAB(4); "}"
    PRINT #2, "#lpane"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "border-right:1px solid #000f;"
    PRINT #2, TAB(4); "float:left;"
    PRINT #2, TAB(4); "align:left;"
    PRINT #2, TAB(4); "padding:3px;"
    PRINT #2, TAB(4); "width:443px;"
    PRINT #2, TAB(4); "}"
    PRINT #2, "#rpane"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "float:right;"
    PRINT #2, TAB(4); "align:left;"
    PRINT #2, TAB(4); "padding:3px;"
    PRINT #2, TAB(4); "width:444px;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".mhdr"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#00f;"
    PRINT #2, TAB(4); "color:#fff;"
    PRINT #2, TAB(4); "font-size:"; mf$; "px;"
    PRINT #2, TAB(4); "text-align:center;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".shdr"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#00f;"
    PRINT #2, TAB(4); "color:#fff;"
    PRINT #2, TAB(4); "font-size:"; sf$; "px;"
    PRINT #2, TAB(4); "text-align:center;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".date"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "clear:both;"
    PRINT #2, TAB(4); "background:#000;"
    PRINT #2, TAB(4); "color:#fff;"
    PRINT #2, TAB(4); "font-size:"; dt$; "px;"
    PRINT #2, TAB(4); "text-align:center;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".local"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#eef;"
    PRINT #2, TAB(4); "color:#00f;"
    PRINT #2, TAB(4); "font-size:"; lr$; "px;"
    PRINT #2, TAB(4); "text-align:center;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".remote"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "background:#eef;"
    PRINT #2, TAB(4); "color:#00f;"
    PRINT #2, TAB(4); "font-size:"; lr$; "px;"
    PRINT #2, TAB(4); "text-align:center;"
    PRINT #2, TAB(4); "}"
    PRINT #2, ".text"
    PRINT #2, TAB(4); "{"
    PRINT #2, TAB(4); "color:#000;"
    PRINT #2, TAB(4); "font-size:"; tx$; "px;"
    PRINT #2, TAB(4); "text-align:left;"
    PRINT #2, TAB(4); "}"
    IF m$ = "+" THEN PRINT #2, "</style>"
END SUB

SUB Main
    DO
    Display
    LOOP
END SUB

SUB Mask (m%)
    DIM i$, x%
    STATIC o$
    IF o$ = "" THEN
        o$ = "+"
        CLS
    ELSE
        LOCATE 1, 1, 0
    END IF
    COLOR Fgc%, Bgc%
    IF m% = 0 THEN
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3, WSZ + 4: PRINT "Ç"; STRING$(78, 196); "¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
        NEXT
        SayIt Title$, 2, 11, Bgc%
        LOCATE 2, 3: PRINT "Sub Directory"
        LOCATE 2, 71: PRINT "Filename"
        IF Src$ = "" THEN
            SayIt "Source Directory Not Specified", Echo%, 15, 4
        ELSE
            SayIt "Source Directory: " + CHR$(34) + Src$ + CHR$(34), Echo%, 11, Bgc%
        END IF
        i$ = "Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Cursor Line to Select"
        SayIt i$, Echo% + 2, Fgc%, Bgc%
    ELSEIF m% = 1 THEN
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3, 21: PRINT "Ç"; STRING$(78, 196); "¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
        NEXT
        SayIt Title$, 2, 11, Bgc%
        SayIt Title$, 22, 11, Bgc%
    ELSEIF m% = 2 THEN
        COLOR 15, 4
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3, 21: PRINT "Ç"; STRING$(78, 196); "¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
        NEXT
        SayIt Title$, 2, 11, Bgc%
        SayIt Title$, 22, 11, Bgc%
    END IF
END SUB

SUB MtTrash
    DIM i$
    SHELL "dir > trash.can /B"
    Flt% = 0
    CLOSE : OPEN "trash.can" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    DO
    IF LCASE$(PRGM) = LCASE$(LEFT$(i$, LEN(PRGM))) THEN
        EXIT DO
    ELSEIF i$ = "trash.can" THEN
        EXIT DO
    ELSE
        KILL i$
        EXIT DO
    END IF
    LOOP
    WEND
    CLOSE
    KILL "trash.can"
END SUB

SUB Populate (s$, m%)
    IF LCASE$(RIGHT$(s$, LEN(PRGM))) = LCASE$(PRGM) THEN
        'We don't want to search for files within the
        'application's subdirectory.
        EXIT SUB
    ELSE
        'We only want to search for files within the
        'subdirectories of the application's source directory.
        DIM f$, i$
        STATIC n%
        DO
        n% = n% + 1
        LOOP WHILE n% < 4
        SHELL "dir " + s$ + " > temp\d" + MID$(STR$(n%), 2) + ".txt"
        Flt% = 0
        CLOSE #n%: OPEN "temp\d" + MID$(STR$(n%), 2) + ".txt" FOR INPUT AS #n%
        IF Flt% = 0 THEN
            WHILE NOT EOF(n%)
            LINE INPUT #n%, i$
            SELECT CASE UCASE$(LEFT$(i$, 1))
                CASE "0" TO "9", "A" TO "Z"
                    DO
                    FOR x% = 1 TO LEN(i$)
                    SELECT CASE UCASE$(MID$(i$, x%, 1))
                        CASE CHR$(32), "/", ":", "<", ">", "\": i$ = i$
                        CASE "0" TO "9", "A" TO "Z": i$ = i$
                        CASE ELSE: EXIT DO
                    END SELECT
                    NEXT
                    IF UCASE$(MID$(i$, 14, 5)) = "<DIR>" THEN
                        Populate s$ + "\" + RTRIM$(LEFT$(i$, 13)), m%
                    ELSE
                        f$ = LCASE$(RTRIM$(LEFT$(i$, 8)))
                        IF m% = 0 THEN
                            IF LCASE$(MID$(i$, 10, 3)) = LCASE$(RIGHT$(BOOT, 3)) THEN
                                f$ = LCASE$(f$ + "." + RTRIM$(MID$(i$, 10, LEN(RIGHT$(BOOT, 3)))))
                                Tf$ = MID$(STR$(VAL(Tf$) + 1), 2)
                                PRINT #2, LCASE$(LEFT$(MID$(s$, LEN(Src$) + 2) + STRING$(MXL, 32), MXL - LEN(f$))); f$
                            ELSEIF LEN(Ext$) > 0 THEN
                                IF LCASE$(MID$(i$, 10, LEN(Ext$))) = LCASE$(Ext$) THEN
                                    f$ = LCASE$(f$ + "." + RTRIM$(MID$(i$, 10, LEN(Ext$))))
                                    Tf$ = MID$(STR$(VAL(Tf$) + 1), 2)
                                    PRINT #2, LCASE$(LEFT$(MID$(s$, LEN(Src$) + 2) + STRING$(MXL, 32), MXL - LEN(f$))); f$
                                END IF
                            END IF
                        ELSEIF m% = 1 THEN
                            IF LCASE$(MID$(i$, 10, 3)) = "htm" THEN
                                f$ = LCASE$(f$ + ".htm")
                                Th$ = MID$(STR$(VAL(Th$) + 1), 2)
                                i$ = "<a href=" + q + "file:\\\" + s$ + "\" + f$ + q + ">" + MID$(s$, LEN(Src$) + 2) + "\" + f$ + "</a>"
                                PRINT #2, "<li>"; LCASE$(i$); "</li>"
                            END IF
                        END IF
                    END IF
                    EXIT DO
                    LOOP
            END SELECT
            WEND
        ELSE
            Flt% = 0
        END IF
        CLOSE #n%
        KILL "temp\d" + MID$(STR$(n%), 2) + ".txt"
        n% = n% - 1
        IF n% < 4 THEN n% = 4
    END IF
END SUB

SUB ReplaceDataFiles
    DIM b%, f%, k$, x%
    f% = 15: b% = 4
    Mask 2
    SayIt "Replace All " + q + Title$ + q + " Data Files", 2, f%, b%
    x% = 6
    SayIt "This Function Will Delete the Following Files from the Directory", x%, f%, b%
    x% = x% + 1
    SayIt "and Replace Them with the Original Default Files;", x%, f%, b%
    x% = x% + 2
    SayIt q + TEXT + q, x%, f%, b%
    x% = x% + 1
    SayIt q + REMOTE + q, x%, f%, b%
    x% = x% + 1
    SayIt q + INI + q, x%, f%, b%
    x% = x% + 1
    SayIt q + CSS + q, x%, f%, b%
    x% = x% + 3
    SayIt "Do You Wish to Proceed (Y)es or (N)o ?", x%, f%, b%
    x% = x% + 3
    SayIt "** CAUTION: All Previous Editing by Any User Will be Deleted **", x%, f%, b%
    SayIt "ESC to End (Aborts Action)", 22, f%, b%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE "Y", CHR$(0) + "S"
            Delete
            SayIt "Action Complete", 2, f%, b%
            SLEEP 1
            EXIT DO
        CASE "N"
            SayIt "Action Aborted", 2, f%, b%
            SLEEP 1
            EXIT DO
    END SELECT
    LOOP
    RUN BOOT
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM e$, h%, l$, r$, w%
    IF i$ = "" THEN e$ = "" ELSE e$ = " ù "
    h% = 3: w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + RIGHT$(e$, 2) + l$, w%)
    r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(w%, 32), w%)
    COLOR f%, b%
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 22
    LOCATE v%, h%
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%, p%)
    DIM b%, f%, i$, j$, o$, x%
    Directory$ = ""
    Filename$ = ""
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln: o$ = CHR$(46)
        CASE e% + 1: Dln = STRING$(MXL, 205): o$ = CHR$(205)
        CASE ELSE: Dln = STRING$(MXL, 32): o$ = CHR$(32)
    END SELECT
    i$ = RTRIM$(LEFT$(Dln, MXL - 13))
    j$ = LTRIM$(RIGHT$(Dln, 12))
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z"
            SELECT CASE LCASE$(RIGHT$(j$, 4))
                CASE LCASE$(RIGHT$(BOOT, 4))
                    f% = 10
                    b% = Bgc%
                CASE ".txt"
                    f% = 14
                    b% = Bgc%
                CASE ".err"
                    f% = 13
                    b% = Bgc%
                CASE ELSE
                    f% = Fgc%
                    b% = Bgc%
            END SELECT
            IF x% = p% THEN
                Directory$ = i$
                Filename$ = j$
            END IF
        CASE ELSE
            f% = Fgc%
            b% = Bgc%
            o$ = LEFT$(Dln, 1)
    END SELECT
    IF x% = p% THEN f% = 15: b% = 13
    SayIt LEFT$(i$ + STRING$(MXL, o$), MXL - LEN(j$)) + j$, x% + 3, f%, b%
    NEXT
    SELECT CASE LCASE$(RIGHT$(Filename$, 4))
        CASE ".txt"
            SayIt "Press ENTER to Edit " + q + LCASE$(Directory$ + "\" + Filename$) + q, Echo% + 1, 14, Bgc%
        CASE LCASE$(RIGHT$(BOOT, 4))
            SayIt "Press ENTER to Run " + q + LCASE$(Directory$ + "\" + Filename$) + q, Echo% + 1, 10, Bgc%
        CASE ELSE
            SayIt "???", Echo% + 1, 14, Bgc%
    END SELECT
END SUB

SUB Search (k$, s%, e%)
    x% = s%
    DO
    x% = x% + 1
    IF x% = e% THEN x% = 1
    IF x% = s% THEN EXIT DO
    GET #1, x%, Dln
    IF UCASE$(k$) = UCASE$(LEFT$(Dln, 1)) THEN EXIT DO
    LOOP
    s% = x%
END SUB

SUB Table (b$, p$, s$, w$)
    IF b$ = "" THEN b$ = "0"
    IF p$ = "" THEN p$ = "0"
    IF s$ = "" THEN s$ = "0"
    IF w$ = "" THEN w$ = "100"
    PRINT #2, "<table";
    PRINT #2, " border="; q; b$; q;
    PRINT #2, " cellpadding="; q; p$; q;
    PRINT #2, " cellspacing="; q; s$; q;
    PRINT #2, " width="; q; w$; "%"; q; ">"
END SUB

SUB Td (a$, c$, s$, t$, l$)
    IF a$ = "<" THEN
        a$ = "left"
    ELSEIF a$ = ">" THEN
        a$ = "right"
    ELSE
        a$ = "center"
    END IF
    IF c$ = "" THEN c$ = "text"
    IF s$ = "" THEN s$ = "100"
    IF LEFT$(t$, 1) = "<" THEN PRINT #2, "<tr>";
    PRINT #2, "<td";
    PRINT #2, " align="; q; a$; q;
    IF LEFT$(c$, 1) = "." THEN
        PRINT #2, " class="; q; MID$(c$, 2); q;
    ELSEIF LEFT$(c$, 1) = "#" THEN
        PRINT #2, " id="; q; MID$(c$, 2); q;
    ELSE
        PRINT #2, " class="; q; c$; q;
    END IF
    PRINT #2, " colspan="; q; s$; q;
    PRINT #2, " width="; q; s$; "%"; q; ">";
    IF l$ = "" THEN
        PRINT #2, ""
    ELSE
        PRINT #2, l$; "</td>"
        IF RIGHT$(t$, 1) = ">" THEN
            PRINT #2, "</tr>"
        END IF
    END IF
END SUB

SUB ValidateCSSFile
    DO
    Flt% = 0: CLOSE : OPEN CSS FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        CreateCssFile
    END IF
    LOOP
    CLOSE
END SUB

SUB ValidateINIFile
    DO
    Flt% = 0: CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "EXT=", "EXTENTION=": Ext$ = MID$(i$, x% + 1): EXIT FOR
                CASE "MAINHEADER=": Mhdr$ = MID$(i$, x% + 1): EXIT FOR
                CASE "SUBHEADER=": Shdr$ = MID$(i$, x% + 1): EXIT FOR
                CASE "TITLE=": Title$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        CreateIniFile
    END IF
    LOOP
    CLOSE
END SUB

SUB ValidateREMOTEFile
    DO
    Flt% = 0: CLOSE : OPEN REMOTE FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        CreateRemoteFile
    END IF
    LOOP
    CLOSE
END SUB

SUB ValidateTEXTFile
    DO
    Flt% = 0: CLOSE : OPEN TEXT FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        CreateTextFile
    END IF
    LOOP
    CLOSE
END SUB

