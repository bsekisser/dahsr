DECLARE SUB Insert ()
DECLARE SUB Delete ()
DECLARE SUB Decant (i$, a$, b$, c$, d$, e$)
DECLARE SUB Display (r%, k$)
DECLARE SUB Purge ()
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB AddCoin ()
TYPE dahsr
    op AS STRING * 4
    aa AS STRING * 4
    bb AS STRING * 1
    cc AS STRING * 12
    dd AS STRING * 5
    ee AS STRING * 50
    ff AS STRING * 12
    gg AS STRING * 76
END TYPE
    CONST BOOT = "coinbox.bas"
    CONST CNTS = "$#.##"
    CONST DCM = "#####"
    CONST DLRS = "$**##.##"
    CONST INI = "coinbox.ini"
    CONST PRGM = "coinbox"
    CONST WSZ = 12
    COMMON SHARED Bgc%
    COMMON SHARED Echo%
    COMMON SHARED Editor$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Title$
    COMMON SHARED Today$
    COMMON SHARED Trc%
    DIM SHARED Coin AS dahsr
    ON ERROR GOTO Trap
    Initialize
    Purge
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DBCC:
DATA "$","Dollar","1"
DATA "?","Unknown","?"
DATA "c","Cent",".01"
DATA "d","Dime",".1"
DATA "f","Farthing","?"
DATA "h","Half Dlr",".5"
DATA "l","Pound","?"
DATA "lb","Pound","?"
DATA "n","Nickel",".05"
DATA "p","Penny",".01"
DATA "q","Quarter Dlr",".25"
DATA "s","Shilling","?"
DATA "*"
DBEE:
DATA "?","Nationality Unknown"
DATA "c","Circulated"
DATA "dd","Double Die"
DATA "ddo","Double Die Obverse"
DATA "ddr","Double Die Reverse"
DATA "d/p","D over P Mint Mark"
DATA "d/s","D over S Mint Mark"
DATA "f","Fine Condition"
DATA "fa","Fair Condition"
DATA "fi","Fine Condition"
DATA "g","Good Condition"
DATA "m","Mint Condition"
DATA "mm","Mint Mark"
DATA "p/d","P over D Mint Mark"
DATA "p/s","P over S Mint Mark"
DATA "p","Poor Condition"
DATA "pf","Proof"
DATA "pr","Proof"
DATA "s/d","S over D Mint Mark"
DATA "s/p","S over P Mint Mark"
DATA "u","Uncirculated"
DATA "*"

SUB AddCoin
    DIM i$, t$, v$, x%, y AS DOUBLE
    PRINT #2, Coin.op; Coin.aa; Coin.bb; Coin.cc;
    SELECT CASE LEFT$(Coin.dd, 1)
        CASE ".", "0" TO "9"
            PRINT #2, USING (CNTS); VAL(Coin.dd);
        CASE ELSE
            PRINT #2, Coin.dd;
    END SELECT
    IF LEFT$(Coin.ee, 1) = "#" THEN
        i$ = RTRIM$(Coin.ee)
        t$ = ""
        v$ = ""
        FOR x% = 1 TO LEN(i$)
        IF MID$(i$, x%, 1) = "#" THEN t$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
        IF MID$(i$, x%, 1) = "$" THEN v$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
        NEXT
        y = VAL(v$) * 100
        IF y - INT(y) < .5 THEN y = y ELSE y = y + 1
        y = INT(y)
        v$ = RIGHT$("00" + MID$(STR$(y), 2), 2)
        y = INT(y / 100)
        v$ = MID$(STR$(y), 2) + "." + v$
        Coin.ee = STRING$(12 - ((LEN(t$) / 2) + (LEN(v$) / 2)), 249) + " Coins: #" + t$ + " ש Face Value $" + v$ + " " + STRING$(80, 249)
        PRINT #2, Coin.ee;
    ELSE
        PRINT #2, Coin.ee;
    END IF
    PRINT #2, Coin.ff; Coin.gg
END SUB

SUB Bufferize
    DIM a$, b$, c$, d$, e$, f$, i$, j$, k$, t, v, x%, y%
    Trc% = 0
    SHELL "dir dbse > temp.txt /B"
    Flt% = 0
    CLOSE : OPEN "temp.txt" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, f$
    CLOSE #3: OPEN "dbse\" + f$ FOR INPUT AS #3
    IF Flt% = 0 THEN
        LINE INPUT #3, i$
        SELECT CASE LCASE$(LEFT$(i$, 1))
            CASE "a" TO "z"
                t = 0
                v = 0
                IF RIGHT$(i$, 1) = ")" THEN
                    i$ = LEFT$(i$, LEN(i$) - 1)
                    FOR x% = LEN(i$) TO 1 STEP -1
                    IF MID$(i$, x%, 1) = "(" THEN
                        a$ = MID$(i$, x% + 1)
                        i$ = LEFT$(i$, x% - 1)
                        EXIT FOR
                    END IF
                    NEXT
                    SELECT CASE LEFT$(a$, 1)
                        CASE "0" TO "9"
                            SELECT CASE LEN(a$)
                                CASE 1: a$ = a$ + "0" + a$ + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                                CASE 2: a$ = a$ + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                                CASE 3: a$ = LEFT$(a$, 2) + "0" + RIGHT$(a$, 1)
                                CASE ELSE: a$ = LEFT$(a$, 2) + RIGHT$(a$, 2)
                            END SELECT
                            IF VAL(LEFT$(a$, 2)) > 15 THEN a$ = "15" + RIGHT$(a$, 2)
                            IF VAL(RIGHT$(a$, 2)) > 15 THEN a$ = LEFT$(a$, 2) + "15"
                            IF VAL(LEFT$(a$, 2)) = VAL(RIGHT$(a$, 2)) THEN a$ = RIGHT$("00" + MID$(STR$(Fgc%), 2), 2) + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                        CASE ELSE
                            a$ = RIGHT$("00" + MID$(STR$(Fgc%), 2), 2) + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                    END SELECT
                ELSE
                    a$ = RIGHT$("00" + MID$(STR$(Fgc%), 2), 2) + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                END IF
                Coin.op = a$
                Coin.ff = f$
                Coin.gg = q + i$ + q$
                Trc% = Trc% + 1
                CLOSE #2: OPEN "text\r" + MID$(STR$(Trc%), 2) + ".txt" FOR OUTPUT AS #2
                WHILE NOT EOF(3)
                LINE INPUT #3, i$
                SELECT CASE LEFT$(i$, 1)
                    CASE "0" TO "9"
                    Decant i$, a$, b$, c$, d$, e$
                    t = t + 1
                    Coin.aa = a$
                    Coin.bb = b$
                    IF c$ = "" THEN
                        c$ = "Cent"
                        IF d$ = "" THEN d$ = ".01"
                    ELSE
                        CLOSE #5: OPEN "coinbox.cc" FOR INPUT AS #5
                        DO WHILE NOT EOF(5)
                        INPUT #5, i$, j$, k$
                        IF i$ = c$ THEN
                            c$ = j$
                            IF d$ = "" THEN d$ = k$
                            EXIT DO
                        END IF
                        LOOP
                        CLOSE #5
                    END IF
                    Coin.cc = c$
                    Coin.dd = d$
                    SELECT CASE LEFT$(d$, 1)
                        CASE ".", "0" TO "9": v = v + VAL(d$)
                    END SELECT
                    IF e$ = "" THEN
                        e$ = "Circulated"
                    ELSE
                        DO
                        IF LEFT$(e$, 1) = "*" THEN
                            e$ = MID$(e$, 2) + "*"
                        ELSE
                            EXIT DO
                        END IF
                        LOOP
                        IF RIGHT$(e$, 2) = "**" THEN
                            e$ = LEFT$(e$, LEN(e$) - 2) + " (Gold)"
                        ELSEIF RIGHT$(e$, 1) = "*" THEN
                            e$ = LEFT$(e$, LEN(e$) - 1) + " (Silver)"
                        END IF
                        CLOSE #5: OPEN "coinbox.ee" FOR INPUT AS #5
                        DO WHILE NOT EOF(5)
                        INPUT #5, i$, j$
                        IF i$ = e$ THEN
                            e$ = j$
                            EXIT DO
                        ELSE
                            i$ = i$ + " "
                            IF i$ = LEFT$(e$, LEN(i$)) THEN
                                e$ = j$ + MID$(e$, LEN(i$))
                                EXIT DO
                            END IF
                        END IF
                        LOOP
                        CLOSE #5
                    END IF
                    Coin.ee = e$
                    AddCoin
                END SELECT
                WEND
                Coin.op = RIGHT$("00" + MID$(STR$(Fgc%), 2), 2) + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                Coin.aa = STRING$(80, 196)
                Coin.bb = STRING$(80, 196)
                Coin.cc = STRING$(80, 196)
                Coin.dd = STRING$(80, 196)
                Coin.ee = STRING$(80, 196)
                AddCoin
                Coin.op = "11" + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                Coin.aa = STRING$(80, 249)
                Coin.bb = STRING$(80, 249)
                Coin.cc = STRING$(80, 249)
                Coin.dd = STRING$(80, 249)
                Coin.ee = "#" + MID$(STR$(t), 2) + "$" + MID$(STR$(v), 2)
                AddCoin
            CASE ELSE
                t = 0
                v = 0
                Coin.gg = q + LEFT$(i$, LEN(i$) - 2) + q
                Coin.op = RIGHT$("00" + i$, 2) + RIGHT$("00" + MID$(STR$(Bgc%), 2), 2)
                Coin.ff = f$
                Trc% = Trc% + 1
                CLOSE #2: OPEN "text\r" + MID$(STR$(Trc%), 2) + ".txt" FOR OUTPUT AS #2
                Coin.op = "1504"
                Coin.aa = STRING$(80, 249)
                Coin.bb = STRING$(80, 249)
                Coin.dd = STRING$(80, 249)
                Coin.cc = "ש Title " + STRING$(80, 249)
                Coin.ee = STRING$(1, 249) + " All Files MUST Begin with a Title Name and All " + STRING$(80, 249)
                AddCoin
                Coin.op = "1504"
                Coin.aa = STRING$(80, 249)
                Coin.bb = STRING$(80, 249)
                Coin.cc = STRING$(80, 249)
                Coin.dd = STRING$(80, 249)
                Coin.ee = STRING$(3, 249) + " Title Names MUST Begin with the Following " + STRING$(80, 249)
                AddCoin
                Coin.op = "1504"
                Coin.aa = STRING$(80, 249)
                Coin.bb = STRING$(80, 249)
                Coin.cc = "ששש Name " + STRING$(80, 249)
                Coin.dd = STRING$(80, 249)
                Coin.ee = STRING$(4, 249) + " Characters (Either Upper or Lower Case) " + STRING$(80, 249)
                AddCoin
                Coin.op = "1504"
                Coin.aa = STRING$(80, 249)
                Coin.bb = STRING$(80, 249)
                Coin.cc = STRING$(80, 249)
                Coin.dd = STRING$(80, 249)
                Coin.ee = ""
                FOR x% = 1 TO 26
                Coin.ee = RTRIM$(Coin.ee) + CHR$(x% + 64)
                NEXT
                Coin.ee = STRING$(10, 249) + " " + q$ + RTRIM$(Coin.ee) + q$ + " " + STRING$(80, 249)
                AddCoin
                Coin.op = "1504"
                Coin.aa = STRING$(80, 249)
                Coin.bb = STRING$(80, 249)
                Coin.cc = "שששש Error " + STRING$(80, 249)
                Coin.dd = STRING$(80, 249)
                Coin.ee = ""
                FOR x% = 1 TO 26
                Coin.ee = RTRIM$(Coin.ee) + CHR$(x% + 96)
                NEXT
                Coin.ee = STRING$(10, 249) + " " + q$ + RTRIM$(Coin.ee) + q$ + " " + STRING$(80, 249)
                AddCoin
        END SELECT
    ELSE
        Flt% = 0
    END IF
    WEND
    CLOSE
    KILL "temp.txt"
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Decant (i$, a$, b$, c$, d$, e$)
    a$ = "": b$ = "": c$ = "": d$ = "": e$ = ""
    CLOSE #9: OPEN "decant.tmp" FOR OUTPUT AS #9
    PRINT #9, i$
    CLOSE #9: OPEN "decant.tmp" FOR INPUT AS #9
    INPUT #9, a$, c$, d$, e$
    CLOSE #9
    KILL "decant.tmp"
    SELECT CASE RIGHT$(a$, 1)
        CASE "a" TO "z": b$ = UCASE$(RIGHT$(a$, 1))
        CASE ELSE: b$ = CHR$(32)
    END SELECT
END SUB

SUB Delete
    DIM k$
    DO
    IF RTRIM$(Coin.ff) = "" THEN
        EXIT DO
    ELSE
        SayIt "DELETE " + q + RTRIM$(Coin.gg) + q + " (Y)es/(N)o ?", 2, 14, 4
        DO
        Getkey k$
        SELECT CASE UCASE$(k$)
            CASE CHR$(0) + "S", "Y"
                KILL "dbse\" + RTRIM$(Coin.ff)
                EXIT DO
            CASE "N": EXIT DO
        END SELECT
        LOOP UNTIL k$ = CHR$(13)
        RUN BOOT
    END IF
    LOOP
END SUB

SUB Display (r%, k$)
    DIM e%, p%, s%
    CLOSE : OPEN "text\r" + MID$(STR$(r%), 2) + ".txt" FOR RANDOM AS #1 LEN = LEN(Coin) + 2
    GET #1, 1, Coin
    Mask
    e% = LOF(1) / (LEN(Coin) + 2)
    p% = 1
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL Editor$ + "dbse\" + RTRIM$(Coin.ff): RUN BOOT
        CASE CHR$(0) + CHR$(15): SHELL Editor$ + INI: RUN BOOT
        CASE CHR$(0) + ";": SHELL Editor$ + "coinbox.cc": RUN BOOT
        CASE CHR$(0) + "<": SHELL Editor$ + "coinbox.ee": RUN BOOT
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE ELSE: EXIT SUB
    END SELECT
    LOOP
    LOOP
END SUB

SUB Edit (i$)
    DO
    IF i$ = "" THEN
        EXIT DO
    ELSE
        SHELL Editor$ + i$
        EXIT DO
    END IF
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
        END
    ELSEIF k$ = "*" THEN
        RUN BOOT
    END IF
END SUB

SUB Initialize
    DIM i$, j$, k$, x%
    MKDIR "dbse"
    MKDIR "html"
    MKDIR "text"
    Bgc% = 1
    Echo% = WSZ + 7
    Editor$ = "notepad.exe "
    Fgc% = 15
    q = CHR$(34)
    Title$ = "The Coin Box"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    DO
    Flt% = 0: CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "BGC=", "BACKGROUNDCOLOR=": Bgc% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "EDITOR=": Editor$ = MID$(i$, x% + 1): EXIT FOR
                CASE "FGC=", "FOREGROUNDCOLOR=": Bgc% = VAL(MID$(i$, x% + 1)): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #2
        PRINT #2, "BACKGROUNDCOLOR="; MID$(STR$(Bgc%), 2)
        PRINT #2, "EDITOR="; Editor$
        PRINT #2, "FOREGROUNDCOLOR="; MID$(STR$(Fgc%), 2)
    END IF
    LOOP
    DO
    Flt% = 0: CLOSE : OPEN PRGM + ".cc" FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        CLOSE : OPEN PRGM + ".cc" FOR OUTPUT AS #2
        RESTORE DBCC
        DO
        READ i$
        IF i$ = "*" THEN
            EXIT DO
        ELSE
            READ j$, k$
            PRINT #2, i$; ","; j$; ","; k$
        END IF
        LOOP
    END IF
    LOOP
    DO
    Flt% = 0: CLOSE : OPEN PRGM + ".ee" FOR INPUT AS #1
    IF Flt% = 0 THEN
        EXIT DO
    ELSE
        CLOSE : OPEN PRGM + ".ee" FOR OUTPUT AS #2
        RESTORE DBEE
        DO
        READ i$
        IF i$ = "*" THEN
            EXIT DO
        ELSE
            READ j$
            PRINT #2, i$; ","; j$
        END IF
        LOOP
    END IF
    LOOP
    CLOSE
    SHELL "dir > src.shl"
    OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "src.shl"
    FOR x% = 1 TO LEN(i$)
    SELECT CASE UCASE$(MID$(i$, x%, 4))
        CASE "/FIX", "FIX/", "\FIX", "FIX\"
            Bgc% = 0
            EXIT FOR
    END SELECT
    NEXT
END SUB

SUB Insert
    DIM f$
    f$ = "newfile.txt"
    CLOSE : OPEN "dbse\" + f$ FOR OUTPUT AS #2
    PRINT #2, "Each line of data represents a coin."
    PRINT #2, "There is four pieces of data per line."
    PRINT #2, "Enter the data (as displayed below) below the dashed line:"
    PRINT #2, "*"; RIGHT$(DATE$, 4); "(mint mark), Denomination, Value, Condition)"
    PRINT #2, "*All data entities may be empty EXCEPT the YEAR, ";
    PRINT #2, "the YEAR MUST be included!"
    PRINT #2, "However the data is entered, ";
    PRINT #2, "there MUST be FOUR PIECES of DATA per LINE."
    PRINT #2, ""
    PRINT #2, "Delete all information above the dashed line, ";
    PRINT #2, "save this file under a different name and exit."
    PRINT #2, ""
    PRINT #2, "WARNING: SAVE UNDER ANOTHER NAME because this file, ";
    PRINT #2, q + f$ + q; ", WILL BE DELETED UPON EXIT!"
    PRINT #2, STRING$(80, 45)
    CLOSE
    SHELL Editor$ + "dbse\" + f$
    KILL "dbse\" + f$
    RUN BOOT
END SUB

SUB Main
    DIM k$, r%
    r% = 1
    DO
    IF r% < 1 THEN r% = Trc%
    IF r% > Trc% THEN r% = 1
    Display r%, k$
    SELECT CASE k$
        CASE CHR$(8): r% = r% - 1
        CASE CHR$(13): r% = r% + 1
        CASE CHR$(0) + "K": r% = r% - 1
        CASE CHR$(0) + "M": r% = r% + 1
        CASE CHR$(0) + "R": Insert
        CASE CHR$(0) + "S": Delete
    END SELECT
    LOOP
END SUB

SUB Mask
    DIM b%, h%, i$, x%, y%
    b% = Echo% - 1
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
        CASE 1: PRINT "ֹ"; STRING$(78, 205); "»"
        CASE 3, 5, b%: PRINT "ַ"; STRING$(78, 196); "¶"
        CASE 23: PRINT "ָ"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "÷"; STRING$(78, 32); "÷"
    END SELECT
    NEXT
    SayIt Title$ + " ש " + RTRIM$(Coin.gg), 2, 11, Bgc%
    h% = 3
    FOR y% = 1 TO 10
    SELECT CASE y%
        CASE 1: h% = h% + LEN(Coin.aa)
        CASE 2: h% = h% + LEN(Coin.bb) + 1
        CASE 3: h% = h% + LEN(Coin.cc) + 1
        CASE 4: h% = h% + LEN(Coin.dd) + 1
        CASE ELSE: EXIT FOR
    END SELECT
    FOR x% = 3 TO b%
    LOCATE x%, h%
    SELECT CASE x%
        CASE 3: PRINT "ֲ"
        CASE 5: PRINT "ֵ"
        CASE b%: PRINT "ֱ"
        CASE ELSE: PRINT "³"
    END SELECT
    NEXT
    NEXT
    Coin.aa = "Year"
    Coin.bb = "M"
    Coin.cc = "Denomination"
    Coin.dd = "Value"
    Coin.ee = "Description/Condition"
    LOCATE 4, 3
    COLOR 11, Bgc%
    PRINT Coin.aa; CHR$(28); Coin.bb; CHR$(28); Coin.cc; CHR$(28); Coin.dd; CHR$(28); Coin.ee
    x% = Echo%
    i$ = ""
    i$ = i$ + " ש Scroll (" + CHR$(24) + CHR$(25) + ") Lines"
    i$ = i$ + " ש Scroll (" + CHR$(27) + CHR$(26) + ") Files"
    i$ = i$ + " ש"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = ""
    i$ = i$ + " ש TAB: Edit " + q + LCASE$(RTRIM$(Coin.ff)) + q
    i$ = i$ + " ש SHIFT/TAB: Edit " + q + INI + q
    i$ = i$ + " ש"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = ""
    i$ = i$ + " ש INSERT New File"
    i$ = i$ + " ש DELETE " + q + LCASE$(RTRIM$(Coin.ff)) + q
    i$ = i$ + " ש"
    SayIt i$, x%, Fgc%, Bgc%
    i$ = ""
    i$ = i$ + " ש F1:Edit Denominations"
    i$ = i$ + " ש F2:Edit Descriptions"
    i$ = i$ + " ש ESC to End"
    i$ = i$ + " ש"
    SayIt i$, x%, Fgc%, Bgc%
END SUB

SUB Purge
    DIM d$, f$, x%
    FOR x% = 0 TO 1
    SELECT CASE x%
        CASE 0: d$ = "text"
        CASE 1: d$ = "html"
    END SELECT
    SHELL "dir " + d$ + " > temp.txt /B"
    CLOSE : OPEN "temp.txt" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, f$
    SELECT CASE UCASE$(LEFT$(f$, 1))
        CASE "0" TO "9", "A" TO "Z": KILL d$ + "\" + f$
    END SELECT
    WEND
    NEXT
    CLOSE
    KILL "temp.txt"
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM h%, l$, r$, w%
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 2
    'h% = 1: w% = 40
    'h% = 2: w% = 39
    h% = 3: w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, h%
    PRINT l$; r$
    v% = v% + 1
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, h%, i$, m$, o$, v%, x%
    m$ = CHR$(28)
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Coin
        CASE e% + 1
            Coin.aa = STRING$(76, 205)
            Coin.bb = STRING$(76, 205)
            Coin.cc = STRING$(76, 205)
            Coin.dd = STRING$(76, 205)
            Coin.ee = STRING$(76, 205)
        CASE ELSE
            Coin.aa = STRING$(76, 32)
            Coin.bb = STRING$(76, 32)
            Coin.cc = STRING$(76, 32)
            Coin.dd = STRING$(76, 32)
            Coin.ee = STRING$(76, 32)
    END SELECT
    SELECT CASE UCASE$(LEFT$(Coin.aa, 1))
        CASE ".", "0" TO "9", CHR$(249)
            f% = VAL(LEFT$(Coin.op, 2))
            b% = VAL(RIGHT$(Coin.op, 2))
        CASE "A" TO "Z"
            f% = 11
            b% = VAL(RIGHT$(Coin.op, 2))
        CASE ELSE
            f% = Fgc%
            b% = Bgc%
    END SELECT
    COLOR f%, b%
    LOCATE x% + 5, 3
    PRINT Coin.aa; m$; Coin.bb; m$; Coin.cc; m$; Coin.dd; m$; Coin.ee
    NEXT
    COLOR Fgc%, Bgc%
END SUB

SUB xStore

END SUB

