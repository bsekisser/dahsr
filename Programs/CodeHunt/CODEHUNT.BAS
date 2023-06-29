DECLARE SUB Search (s!, e!, k$)
DECLARE SUB Delete ()
DECLARE SUB Start (k$)
DECLARE SUB Interval (k$)
DECLARE SUB Insert ()
DECLARE SUB Launch ()
DECLARE SUB Scroll (m%, s!, e!, p%)
DECLARE SUB Getkey (k$)
DECLARE SUB Display (k$)
DECLARE SUB Bufferize (m%)
DECLARE SUB Mask (m%)
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Main ()
    CONST BFR = "codehunt.bfr", BOOT = "codehunt.bas", MXK = 72, MXD = 2
    CONST DCM = "document.bfr", MXL = 77, PRGM = "codehunt", ROWS = 12
    CONST NUM = "0000", TOP = 5
    COMMON SHARED Bgc%
    COMMON SHARED Bkg$
    COMMON SHARED Echo%
    COMMON SHARED Document$
    COMMON SHARED Fgc%
    COMMON SHARED Filename$
    COMMON SHARED Flt%
    COMMON SHARED Ivl%
    COMMON SHARED Kst$
    COMMON SHARED MyFile$
    COMMON SHARED q$
    COMMON SHARED Src$
    COMMON SHARED Sta AS DOUBLE
    COMMON SHARED Tdc$
    COMMON SHARED Title$
    COMMON SHARED Tog%
    ON ERROR GOTO Trap
    Initialize
    Bufferize 0
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Bufferize (m%)
    DIM a, b, c$, d$, i$, f$, x%
    SELECT CASE m%
        CASE 0
            CLS
            Mask 0
            SayIt "ù Loading Document List ù", TOP + (ROWS / 2), 11, Bgc%
            CLOSE
            SHELL "dir dbse > dbse.shl"
            OPEN "dbse.shl" FOR INPUT AS #1
            OPEN BFR FOR OUTPUT AS #2
            WHILE NOT EOF(1)
            LINE INPUT #1, i$
            SELECT CASE UCASE$(LEFT$(i$, 1))
                CASE "0" TO "9", "A" TO "Z"
                    IF UCASE$(MID$(i$, 14, 5)) = "<DIR>" THEN
                        d$ = RTRIM$(LEFT$(i$, 13))
                        CLOSE #3
                        KILL "temp.shl"
                        SHELL "dir dbse\" + d$ + " > temp.shl /B"
                        OPEN "temp.shl" FOR INPUT AS #3
                        WHILE NOT EOF(3)
                        LINE INPUT #3, i$
                        SELECT CASE UCASE$(LEFT$(i$, 1))
                            CASE "0" TO "9", "A" TO "Z"
                                f$ = i$
                                PRINT #2, LEFT$("+" + d$ + STRING$(MXL, 32), MXL - LEN(f$)); f$
                                Tdc$ = MID$(STR$(VAL(Tdc$) + 1), 2)
                        END SELECT
                        WEND
                    END IF
            END SELECT
            WEND
            CLOSE
            KILL "temp.shl"
            KILL "dbse.shl"
        CASE 1
            a = 0
            b = 0
            CLOSE #2: OPEN DCM FOR OUTPUT AS #2
            Flt% = 0
            CLOSE #3: OPEN "dbse\" + Filename$ FOR INPUT AS #3
            IF Flt% = 0 THEN
                WHILE NOT EOF(3)
                LINE INPUT #3, c$
                IF c$ = "" THEN
                    c$ = ""
                ELSE
                    FOR x% = 1 TO LEN(c$)
                    SELECT CASE UCASE$(MID$(c$, x%, 1))
                        CASE "0" TO "9", "A" TO "Z"
                            a = a + 1
                            IF a >= Sta THEN
                                b = b + 1
                                IF b = Ivl% THEN
                                    b = 0
                                    PRINT #2, LEFT$(MID$(c$, x%, 1) + STRING$(MXD, "*"), MXD)
                                END IF
                            END IF
                    END SELECT
                    NEXT
                END IF
                WEND
            ELSE
                i$ = "Cannot Find " + q$ + Filename$ + q$ + "!"
                FOR x% = 1 TO LEN(i$)
                PRINT #2, LEFT$(MID$(i$, x%, 1) + STRING$(MXD, "*"), MXD)
                NEXT
            END IF
            CLOSE #2
            CLOSE #3
    END SELECT
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Display (k$)
    DIM e, i$, s, v
    STATIC w%
    Mask 2
    CLOSE #3: OPEN DCM FOR RANDOM AS #3 LEN = MXD + 2
    e = LOF(3) / (MXD + 2)
    s = 1
    IF w% = 0 THEN w% = 76
    DO
    IF w% < 1 THEN w% = 1
    IF w% > 76 THEN w% = 76
    IF s < 1 THEN s = 1
    IF s > e THEN s = e
    IF Sta > e THEN Sta = e
    Scroll 1, s, e, w%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(13): EXIT SUB
        CASE CHR$(32)
            Tog% = Tog% + 1
            IF Tog% > 3 THEN Tog% = 0
            EXIT DO
        CASE CHR$(0) + ";": Start k$: EXIT SUB
        CASE CHR$(0) + "<": Interval k$: EXIT SUB
        CASE CHR$(0) + "="
            w% = 76
            Kst$ = ""
            Sta = 1
            Ivl% = 1
            EXIT SUB
        CASE CHR$(0) + "G": s = 1: EXIT DO
        CASE CHR$(0) + "H": s = s - w%: EXIT DO
        CASE CHR$(0) + "I": s = s - (w% * ROWS) + w%: EXIT DO
        CASE CHR$(0) + "K": s = s - 1: EXIT DO
        CASE CHR$(0) + "M": s = s + 1: EXIT DO
        CASE CHR$(0) + "O": s = e: EXIT DO
        CASE CHR$(0) + "P": s = s + w%: EXIT DO
        CASE CHR$(0) + "Q": s = s + (w% * ROWS) - w%: EXIT DO
        CASE CHR$(0) + "R": Insert: EXIT SUB
        CASE "-": w% = w% - 1: EXIT DO
        CASE "+": w% = w% + 1: EXIT DO
        CASE "*": RUN BOOT
    END SELECT
    LOOP
    LOOP
END SUB

SUB FormatDCM
    DIM c$, i, s, x%
    i = 0
    s = 0
    Flt% = 0
    CLOSE #2: OPEN DCM FOR OUTPUT AS #2
    CLOSE #3: OPEN "dbse\" + Filename$ FOR INPUT AS #3
    IF Flt% = 0 THEN
        WHILE NOT EOF(3)
        LINE INPUT #3, c$
        IF LEN(c$) > 0 THEN
            FOR x% = 1 TO LEN(c$)
            SELECT CASE UCASE$(MID$(c$, x%, 1))
                CASE "0" TO "9", "A" TO "Z"
                    s = s + 1
                    IF s >= Sta THEN
                        s = Sta
                        i = i + 1
                        IF i = Ivl% THEN
                            PRINT #2, LEFT$(MID$(c$, x%, 1) + STRING$(MXD, 46), MXD)
                            i = 0
                        END IF
                    END IF
            END SELECT
            NEXT
        END IF
        WEND
    ELSE
        PRINT #2, "Error #" + MID$(STR$(Flt%), 2)
    END IF
    CLOSE #2
    CLOSE #3
END SUB

SUB Getkey (k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    MKDIR "dbse"
    Echo% = TOP + ROWS + 2
    Fgc% = 15
    Flt% = 0
    Kst$ = ""
    Ivl% = 1
    MyFile$ = "myfiles\myfiles.bas"
    w% = 76
    q$ = CHR$(34)
    Src$ = ""
    Sta = 1
    Title$ = "Coded Strings Search Within Document Text"
    Tog% = 0
    CLOSE
    SHELL "dir > source.shl"
    OPEN "source.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "source.shl"
    Src$ = LCASE$(MID$(i$, 15)) + "\"
    MyFile$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM) - 1) + MyFile$
    'CLS
    'PRINT Src$
    'PRINT MyFile$
    'END
END SUB

SUB Insert
    DIM b$, x%
    IF Kst$ = "" THEN
        b$ = "?????"
        SayIt "ù Press DELETE to Erase the String ù", Echo% + 1, 14, Bgc%
        SayIt "ù Press BACKSPACE to Delete Last Character ù", Echo% + 2, 14, Bgc%
        SayIt "ù Press ENTER to Exit ù Press ESC to End ù", Echo% + 3, 14, Bgc%
        DO
        IF Kst$ = "" THEN Kst$ = b$
        SayIt q$ + UCASE$(Kst$) + q$, 4, 30, Bgc%
        Getkey k$
        IF Kst$ = b$ THEN Kst$ = ""
        SELECT CASE UCASE$(k$)
            CASE CHR$(8)
                IF Kst$ = "" THEN
                    Kst$ = ""
                ELSE
                    Kst$ = LEFT$(Kst$, LEN(Kst$) - 1)
                END IF
            CASE CHR$(13), CHR$(0) + "O": EXIT DO
            CASE CHR$(0) + "S": Kst$ = ""
            CASE " ", "0" TO "9", "A" TO "Z"
                IF LEN(Kst$) < MXK THEN
                    Kst$ = Kst$ + k$
                ELSE
                    Kst$ = MID$(Kst$, 2) + k$
                END IF
        END SELECT
        LOOP
    ELSE
        Kst$ = ""
    END IF
END SUB

SUB Interval (k$)
    DIM i$
    i$ = NUM
    COLOR 30, Bgc%
    DO
    LOCATE 2, 78 - LEN(NUM) + 1
    PRINT RIGHT$(NUM + i$, LEN(NUM))
    Getkey k$
    SELECT CASE k$
        CASE CHR$(13), CHR$(0) + "R"
            k$ = "+"
            Ivl% = VAL(i$)
            EXIT DO
        CASE CHR$(27): EXIT DO
        CASE CHR$(0) + "O": EXIT DO
        CASE CHR$(0) + "S": i$ = ""
        CASE "0" TO "9"
            IF LEN(i$) > LEN(NUM) THEN
                i$ = MID$(i$, 2) + k$
            ELSE
                i$ = i$ + k$
            END IF
    END SELECT
    LOOP
END SUB

SUB Launch
    DIM k$
    k$ = ""
    IF Filename$ = "" THEN
        EXIT SUB
    ELSE
        DO
        'Mask 0
        SayIt "ù Loading " + q$ + Document$ + q$ + " Document ù", 4, 14, Bgc%
        IF Sta < 1 THEN Sta = 1
        IF Ivl% < 1 THEN Ivl% = 1
        Bufferize 1
        Display k$
        CLOSE #2
        CLOSE #3
        LOOP UNTIL k$ = CHR$(13) OR k$ = CHR$(27)
        Mask 1
    END IF
END SUB

SUB Main
    DIM e, k$, p%, s
    Mask 1
    CLOSE : OPEN BFR FOR RANDOM AS #1 LEN = MXL + 2
    e = LOF(1) / (MXL + 2)
    p% = 1
    s = 1
    DO
    IF p% < 1 THEN p% = 1: s = s - 1
    IF p% > ROWS THEN p% = ROWS: s = s + 1
    IF s < 1 THEN s = 1
    IF s > e THEN s = e
    Scroll 0, s, e, p%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(8)
            CHDIR LEFT$(MyFile$, LEN(MyFile$) - 12)
            RUN MyFile$
            CHDIR LEFT$(Src$, LEN(Src$) - 1)
            RUN BOOT
        CASE CHR$(9): SHELL "notepad.exe " + Src$ + "dbse\" + Filename$: RUN BOOT
        CASE CHR$(13): Launch: EXIT DO
        CASE CHR$(0) + "G": p% = 1: s = 1: EXIT DO
        CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
        CASE CHR$(0) + "I": p% = 1: s = s - ROWS + 1: EXIT DO
        CASE CHR$(0) + "K": s = s - ROWS + 1: EXIT DO
        CASE CHR$(0) + "M": s = s + ROWS - 1: EXIT DO
        CASE CHR$(0) + "O": p% = 1: s = e: EXIT DO
        CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
        CASE CHR$(0) + "Q": p% = 1: s = s + ROWS - 1: EXIT DO
        CASE "0" TO "9", "A" TO "Z": Search s, e, k$: EXIT DO
        CASE "*": RUN BOOT
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask (m%)
    DIM a%, i$, k$, x%
    SELECT CASE m%
        CASE 0
            LOCATE 1, 1
            COLOR Fgc%, Bgc%
            FOR x% = 1 TO 23
            SELECT CASE x%
                CASE 1: PRINT "É"; STRING$(78, 205); "»"
                CASE 3, TOP, TOP + ROWS + 1: PRINT "Ç"; STRING$(78, 196); "¶"
                CASE 23: PRINT "È"; STRING$(78, 205); "¼"
                CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
            END SELECT
            NEXT
        CASE 1
            SayIt Title$, 2, 11, Bgc%
            SayIt LEFT$("Directory" + STRING$(76, 32), 76 - LEN("Filename")) + "Filename", 4, 10, Bgc%
            SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ")Lines or Press " + q$ + "A" + q$ + " to " + q$ + "Z" + q$ + " to Select a Document ù", Echo%, 11, Bgc%
            SayIt "ù Press ENTER to View the Text of the Selected Document ù", Echo% + 1, 14, Bgc%
            SayIt "ù HOME ù END ù PAGEUP ù PAGEDOWN ù", Echo% + 2, Fgc%, Bgc%
            SayIt "ù BACKSPACE to Exit ù ESC to End ù", 22, Fgc%, Bgc%
        CASE 2
            SayIt q$ + Document$ + q$, 2, 11, Bgc%
            SayIt q$ + UCASE$(Kst$) + q$, 4, 11, Bgc%
            SayIt "ù Scroll(" + CHR$(24) + CHR$(25) + ")Lines ù Scroll(" + CHR$(27) + CHR$(26) + ")Text ù", Echo%, Fgc%, Bgc%
            SayIt "ù Size(-+)Window ù PAGE Window ù SPACEBAR Toggles Display ù", Echo% + 1, Fgc%, Bgc%
            SayIt "ù F1:Enter Start Value ù F2:Enter Interval Value ù F3:Reset Default Values ù", Echo% + 2, Fgc%, Bgc%
            i$ = "ù ENTER to Exit "
            IF Kst$ = "" THEN
                Kst$ = ""
            ELSE
                i$ = i$ + "ù INSERT Deletes String "
            END IF
            i$ = i$ + "ù ESC to End ù"
            SayIt i$, Echo% + 3, Fgc%, Bgc%
    END SELECT
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + l$, 38)
    r$ = LEFT$(r$ + STRING$(38, 32), 38)
    COLOR f%, b%
    LOCATE v%, 3, 0
    PRINT l$; r$;
    COLOR Fgc%, Bgc%
    SELECT CASE v%
        CASE TOP + 1 TO TOP + ROWS: PRINT " º"
        CASE ELSE: PRINT ""
    END SELECT
END SUB

SUB Scroll (m%, s, e, p%)
    DIM c AS STRING * MXL, d AS STRING * MXD
    DIM a%, b%, f%, h%, i$, j$, k%, l%, o$, v%, x%, y%, z
    SELECT CASE m%
        CASE 0
            COLOR Fgc%, Bgc%
            Document$ = ""
            Filename$ = ""
            FOR x% = 1 TO ROWS
            SELECT CASE s + x% - 1
                CASE 1 TO e: GET #1, s + x% - 1, c: o$ = "."
                CASE 1 TO e + 1: c = STRING$(MXL, 196): o$ = CHR$(196)
                CASE 1 TO e + 2: c = LEFT$("*Total Documents" + STRING$(MXL, 46), MXL - LEN(Tdc$)) + Tdc$: o$ = "."
                CASE 1 TO e + 3: c = STRING$(MXL, 205): o$ = CHR$(205)
                CASE ELSE: c = STRING$(MXL, 32): o$ = " "
            END SELECT
            i$ = RTRIM$(LEFT$(c, MXL - 12))
            j$ = LTRIM$(RIGHT$(c, 12))
            SELECT CASE LEFT$(i$, 1)
                CASE "+", "-"
                    IF LEFT$(i$, 1) = "+" THEN
                        f% = 10
                        b% = Bgc%
                    ELSEIF LEFT$(i$, 1) = "-" THEN
                        f% = 14
                        b% = Bgc%
                    ELSE
                        f% = Fgc%
                        b% = Bgc%
                    END IF
                    IF x% = p% THEN
                        Filename$ = MID$(i$, 2) + "\" + j$
                        Document$ = j$
                    END IF
                CASE "*": f% = 11: b% = Bgc%
                CASE ELSE: f% = Fgc%: b% = Bgc%
            END SELECT
            IF x% = p% THEN f% = 15: b% = 14
            SayIt LEFT$(MID$(i$, 2) + STRING$(MXL, o$), MXL - LEN(j$) - 1) + j$, TOP + x%, f%, b%
            NEXT
        CASE 1
            COLOR Fgc%, Bgc%
            LOCATE 2, 3: PRINT "Start:"; RIGHT$(STRING$(LEN(NUM), LEFT$(NUM, 1)) + MID$(STR$(Sta), 2), LEN(NUM))
            LOCATE 2, 18: PRINT "Width:" + RIGHT$("00" + MID$(STR$(p%), 2), 2)
            LOCATE 2, 53: PRINT "Span:"; RIGHT$(STRING$(LEN(NUM), LEFT$(NUM, 1)) + MID$(STR$(ROWS * p%), 2), LEN(NUM))
            LOCATE 2, 66: PRINT "Interval:"; RIGHT$(STRING$(LEN(NUM), LEFT$(NUM, 1)) + MID$(STR$(Ivl%), 2), LEN(NUM))
            IF Kst$ = "" THEN
                SayIt "ù INSERT a Search String ù", 4, 11, Bgc%
            ELSE
                SayIt "ù " + q$ + UCASE$(Kst$) + q$ + " ù", 4, 10, Bgc%
            END IF
            z = 0
            FOR x% = 1 TO ROWS
            FOR y% = 1 TO 76
            SELECT CASE y%
                CASE 1 TO p%
                    z = z + 1
                    SELECT CASE z
                        CASE 1 TO e: GET #3, z + s - 1, d
                        CASE ELSE: d = STRING$(MXD, 32)
                    END SELECT
                CASE ELSE: d = STRING$(MXD, 32)
            END SELECT
            SELECT CASE UCASE$(LEFT$(d, 1))
                CASE "0" TO "9", "A" TO "Z"
                    SELECT CASE UCASE$(LEFT$(d, 1))
                        CASE "A" TO "Z"
                            IF Kst$ = "" THEN
                                f% = 10
                                b% = Bgc%
                            ELSE
                                IF Tog% = 2 OR Tog% = 3 THEN
                                    f% = Bgc%
                                    b% = Bgc%
                                ELSE
                                    f% = 10
                                    b% = Bgc%
                                END IF
                            END IF
                        CASE ELSE
                            IF Kst$ = "" THEN
                                f% = 14
                                b% = Bgc%
                            ELSE
                                IF Tog% = 2 OR Tog% = 3 THEN
                                    f% = Bgc%
                                    b% = Bgc%
                                ELSE
                                    f% = 14
                                    b% = Bgc%
                                END IF
                            END IF
                    END SELECT
                    IF LEN(Kst$) > 1 THEN
                        FOR k% = 1 TO LEN(Kst$)
                        IF UCASE$(LEFT$(d, 1)) = UCASE$(MID$(Kst$, k%, 1)) THEN
                            f% = 15
                            b% = 13
                        END IF
                        NEXT
                    END IF
                CASE ELSE: f% = Fgc%: b% = Bgc%
            END SELECT
            LOCATE TOP + x%, y% + 2
            COLOR f%, b%
            SELECT CASE Tog%
                CASE 0, 2: PRINT UCASE$(LEFT$(d, 1))
                CASE 1, 3: PRINT LCASE$(LEFT$(d, 1))
                CASE ELSE: PRINT LEFT$(d, 1)
            END SELECT
            NEXT
            FOR a% = 1 TO ROWS
            COLOR Fgc%, Bgc%
            LOCATE TOP + a%, p% + 3
            SELECT CASE p%
                CASE 1: PRINT "+º"
                CASE 75: PRINT "-+º"
                CASE 76: PRINT "-º"
                CASE ELSE: PRINT "-+º "
            END SELECT
            NEXT
            NEXT
    END SELECT
END SUB

SUB Search (s, e, k$)
    DIM i$, l AS STRING * MXL, x
    x = s
    DO
    x = x + 1
    IF x < 1 THEN x = e
    IF x > e THEN x = 1
    IF x = s THEN EXIT DO
    GET #1, x, l
    i$ = LTRIM$(RIGHT$(l, 12))
    IF UCASE$(k$) = UCASE$(LEFT$(i$, 1)) THEN
        EXIT DO
    END IF
    LOOP
    s = x
END SUB

SUB Start (k$)
    DIM i$
    i$ = NUM
    COLOR 30, Bgc%
    DO
    LOCATE 2, 9
    PRINT RIGHT$(NUM + i$, LEN(NUM))
    Getkey k$
    SELECT CASE k$
        CASE CHR$(13), CHR$(0) + "R"
            k$ = "+"
            Sta = VAL(i$)
            EXIT DO
        CASE CHR$(27): EXIT DO
        CASE CHR$(0) + "O": EXIT DO
        CASE CHR$(0) + "S": i$ = ""
        CASE "0" TO "9"
            IF LEN(i$) > LEN(NUM) THEN
                i$ = MID$(i$, 2) + k$
            ELSE
                i$ = i$ + k$
            END IF
    END SELECT
    LOOP
END SUB

