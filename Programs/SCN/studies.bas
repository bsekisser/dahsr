DECLARE SUB DBR ()
DECLARE SUB Display (f$, k$)
DECLARE SUB DataChex ()
DECLARE SUB ReDate (i$)
DECLARE SUB Sort ()
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask (m%)
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
    CONST BOOT = "studies.bas", PRGM = "studies"
    CONST MXL = 79, TOP = 8, WSZ = 11
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Echo%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED MyFile$
    COMMON SHARED Mxd%
    COMMON SHARED Src$
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Book(1, 66) AS STRING
    ON ERROR GOTO Trap
    Initialize
    DataChex
    Sort
    Bufferize
    DBR
    Main
END
Trap: Flt% = ERR: RESUME NEXT

Books:
DATA "Genesis","Exodus","Leviticus","Numbers","Deuteronomy","Joshua"
DATA "Judges","Ruth","1 Samuel","2 Samuel","1 Kings","2 Kings"
DATA "1 Chronicles","2 Chronicles","Ezra","Nehemiah","Esther","Job"
DATA "Psalms","Proverbs","Ecclesiastes","Song of Solomon","Isaiah","Jeremiah"
DATA "Lamentations","Ezekiel","Daniel","Hosea","Joel","Amos"
DATA "Obadiah","Jonah","Micah","Nahum","Habakkuk","Zephaniah"
DATA "Haggai","Zechariah","Malachi","Matthew","Mark","Luke"
DATA "John","Acts","Romans","1 Corinthians","2 Corinthians","Galatians"
DATA "Ephesians","Philipians","Colossians","1 Thessalonians","2 Thessalonians","1 Timothy"
DATA "2 Timothy","Titus","Philemon","Hebrews","James","1 Peter"
DATA "2 Peter","1 John","2 John","3 John","Jude","Revelation"

SUB Bufferize
    DIM b$, d$, e$, i$, o$, p$, r$, s$, t$, x%
    Flt% = 0
    CLOSE
    OPEN PRGM + ".tmp" FOR INPUT AS #1
    OPEN PRGM + ".bf0" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        INPUT #1, d$, b$, p$
        IF s$ = "" THEN s$ = d$
        e$ = d$
        FOR x% = 1 TO 66
        IF UCASE$(b$) = UCASE$(Book(0, x%)) THEN Book(1, x%) = MID$(STR$(VAL(Book(1, x%)) + 1), 2)
        NEXT
        SELECT CASE UCASE$(p$)
            CASE "", "P", "PM", "MURRAY", "P MURRAY": p$ = "Pastor Murray": o$ = "[P]"
            CASE "A", "1", "AM", "A MURRAY", "A.MURRAY", "A. MURRAY", "ARNOLD MURRAY": p$ = "Arnold Murray": o$ = "[A]"
            CASE "D", "2", "DM", "D MURRAY", "D.MURRAY", "D. MURRAY", "DENNIS MURRAY": p$ = "Dennis Murray": o$ = "[D]"
            CASE ELSE: p$ = "?": o$ = "[?]"
        END SELECT
        ReDate d$
        PRINT #2, LEFT$(o$ + d$ + ":" + b$ + STRING$(MXL, 46), MXL - LEN(p$)); p$
        WEND
    END IF
    CLOSE #2: OPEN PRGM + ".bf2" FOR OUTPUT AS #2
    FOR x% = 1 TO 66
    'PRINT #2, LEFT$("[B]" + Book(0, x%) + STRING$(MXL, 46), MXL - 1 - LEN(Book(1, x%))); "x"; Book(1, x%)
    PRINT #2, LEFT$("[B]" + Book(0, x%) + STRING$(MXL, 46), MXL - 3); RIGHT$("000" + Book(1, x%), 3)
    t$ = MID$(STR$(VAL(t$) + VAL(Book(1, x%))), 2)
    NEXT
    CLOSE
    SHELL "sort /+4 " + PRGM + ".bf2 > " + PRGM + ".bf3"
    SHELL "sort /+" + MID$(STR$(MXL - 3), 2) + " " + PRGM + ".bf2 > " + PRGM + ".bf4"
    SHELL "sort /R /+" + MID$(STR$(MXL - 3), 2) + " " + PRGM + ".bf2 > " + PRGM + ".bf5"
    CLOSE #2: OPEN PRGM + ".bf1" FOR APPEND AS #2
    PRINT #2, STRING$(MXL, 196)
    PRINT #2, LEFT$("[T]Total Readings (from " + e$ + " to " + s$ + ")" + STRING$(MXL, 46), MXL - LEN(t$)); t$
    CLOSE
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB DataChex
    DIM d%, i$, x%
    CLOSE : OPEN PRGM + ".txt" FOR INPUT AS #1
    WHILE NOT EOF(1)
    LINE INPUT #1, i$
    d% = 0
    FOR x% = 1 TO LEN(i$)
    IF MID$(i$, x%, 1) = "," THEN d% = d% + 1
    NEXT
    IF d% <> 2 THEN
        CLS
        PRINT i$
        BEEP
        END
    END IF
    WEND
    CLOSE
END SUB

SUB DBR
    DIM d$, b$, o$, p$, x%
    CLOSE : OPEN PRGM + ".bf1" FOR OUTPUT AS #2
    FOR x% = 1 TO 66
    PRINT #2, LEFT$("[*]" + Book(0, x%) + STRING$(MXL, 46), MXL - LEN(Book(1, x%))); Book(1, x%)
    CLOSE #1: OPEN PRGM + ".tmp" FOR INPUT AS #1
    WHILE NOT EOF(1)
    INPUT #1, d$, b$, p$
    IF UCASE$(b$) = UCASE$(Book(0, x%)) THEN
        SELECT CASE UCASE$(p$)
            CASE "", "P", "PM", "MURRAY", "P MURRAY": p$ = "Pastor Murray": o$ = "[P]"
            CASE "A", "1", "AM", "A MURRAY", "A.MURRAY", "A. MURRAY", "ARNOLD MURRAY": p$ = "Arnold Murray": o$ = "[A]"
            CASE "D", "2", "DM", "D MURRAY", "D.MURRAY", "D. MURRAY", "DENNIS MURRAY": p$ = "Dennis Murray": o$ = "[D]"
            CASE ELSE: p$ = "?": o$ = "[?]"
        END SELECT
        PRINT #2, LEFT$(o$ + p$ + STRING$(MXL, 46), MXL - LEN(d$)); d$
    END IF
    WEND
    IF x% < 66 THEN PRINT #2, STRING$(MXL, 196)
    NEXT
    CLOSE
END SUB

SUB Display (f$, k$)
    DIM a%, e%, s%
    Mask VAL(RIGHT$(f$, 1))
    CLOSE : OPEN f$ FOR RANDOM AS #1 LEN = MXL + 2
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
        CASE CHR$(9): SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT: EXIT DO
        CASE CHR$(13): EXIT SUB
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "I": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "K": EXIT SUB
        CASE CHR$(0) + "M": EXIT SUB
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = s% + WSZ - 1: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
    IF k$ = "*" THEN RUN BOOT
END SUB

SUB Initialize
    DIM i$, x%
    Bgc% = 1
    Dbse$ = "dbse\"
    Echo% = TOP + WSZ + 1
    Fgc% = 15
    Flt% = 0
    MyFile$ = "myfiles\myfiles" + LCASE$(RIGHT$(BOOT, 4))
    Mxd% = 5
    Title$ = "Shepherd's Chapel Bible Studies"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 2)
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                
            END SELECT
        END IF
        NEXT
        WEND
    END IF
    CLOSE
    SHELL "dir > src.shl"
    CLOSE : OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "src.shl"
    y% = 0
    Src$ = LCASE$(MID$(i$, 15)) + "\"
    FOR x% = LEN(Src$) TO 1 STEP -1
    SELECT CASE MID$(Src$, x%, 1)
        CASE "\", "/"
            y% = y% + 1
            IF y% = 2 THEN
                MyFile$ = LEFT$(Src$, x%) + MyFile$
            END IF
    END SELECT
    NEXT
    FOR x% = 1 TO 66
    READ Book(0, x%): Book(1, x%) = "0"
    NEXT
END SUB

SUB Main
    DIM k$, n%
    Mask 0
    DO
    IF n% < 0 THEN
        n% = Mxd%
    ELSEIF n% > Mxd% THEN
        n% = 0
    END IF
    Display PRGM + ".bf" + MID$(STR$(n%), 2), k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(13): n% = n% + 1
        CASE CHR$(0) + "K": n% = n% - 1
        CASE CHR$(0) + "M": n% = n% + 1
    END SELECT
    LOOP
END SUB

SUB Mask (m%)
    DIM i$, j$, x%
    STATIC o$
    IF o$ = "" THEN
        o$ = "+"
        CLS
        COLOR Fgc%, Bgc%
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE TOP - 5, TOP - 3, TOP - 1, Echo% - 1: PRINT "Ç"; STRING$(78, 196); "¶"
            CASE 5: PRINT "º "; STRING$(76, 45); " º"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
        NEXT
        SayIt Title$ + " ù " + Today$, 2, 11, Bgc%
        SayIt "ù Scroll(" + CHR$(24) + CHR$(25) + ") Lines ù PAGE Lines ù", Echo%, Fgc%, Bgc%
        SayIt "ù Scroll(" + CHR$(27) + CHR$(26) + ") Displays ù ENTER Next Display ù TAB to Edit Data ù", Echo% + 1, Fgc%, Bgc%
        SayIt "ù BACKSPACE to Exit ù ESC to End ù", Echo% + 2, Fgc%, Bgc%
    END IF
    SELECT CASE m%
        CASE 0
            i$ = "Group " + CHR$(34) + CHR$(m% + 65) + CHR$(34) + ": Book Readings"
            j$ = LEFT$("Date" + STRING$(7, 32) + "Book Started" + STRING$(MXL, 32), MXL - 9) + "Pastor"
        CASE 1
            i$ = "Group " + CHR$(34) + CHR$(m% + 65) + CHR$(34) + ": Book Occurences and Dates of Readings in Biblical Order"
            j$ = LEFT$("Book/Pastor" + STRING$(MXL, 32), MXL - 18) + "Occurences/Date"
        CASE 2'C
            i$ = "Group " + CHR$(34) + CHR$(m% + 65) + CHR$(34) + ": Book Readings in Biblical Order"
            j$ = LEFT$("Book" + STRING$(MXL, 32), MXL - 11) + "Readings"
        CASE 3'D
            i$ = "Group " + CHR$(34) + CHR$(m% + 65) + CHR$(34) + ": Books in Alphabetical Order"
            j$ = LEFT$("Book" + STRING$(MXL, 32), MXL - 11) + "Readings"
        CASE 4'E
            i$ = "Group " + CHR$(34) + CHR$(m% + 65) + CHR$(34) + ": Books Sorted by Readings in Ascending Order"
            j$ = LEFT$("Book" + STRING$(MXL, 32), MXL - 11) + "Readings"
        CASE 5'F
            i$ = "Group " + CHR$(34) + CHR$(m% + 65) + CHR$(34) + ": Books Sorted by Readings in Descending Order"
            j$ = LEFT$("Book" + STRING$(MXL, 32), MXL - 11) + "Readings"
    END SELECT
    SayIt i$, TOP - 4, 11, Bgc%
    SayIt j$, TOP - 2, 11, Bgc%
END SUB

SUB ReDate (i$)
    DIM d$, j$, m$, x%, y$
    FOR x% = 1 TO LEN(i$)
    SELECT CASE MID$(i$, x%, 1)
        CASE "0" TO "9": j$ = j$ + "x"
        CASE "/": j$ = j$ + "/"
    END SELECT
    NEXT
    SELECT CASE j$
        CASE "x/x/x"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 1)
            y$ = RIGHT$(i$, 1)
        CASE "xx/x/x"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 4, 1)
            y$ = RIGHT$(i$, 1)
        CASE "x/xx/x"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 2)
            y$ = RIGHT$(i$, 1)
        CASE "xx/xx/x"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 3, 2)
            y$ = RIGHT$(i$, 1)
        CASE "x/x/xx"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 1)
            y$ = RIGHT$(i$, 2)
        CASE "xx/x/xx"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 4, 1)
            y$ = RIGHT$(i$, 2)
        CASE "x/xx/xx"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 2)
            y$ = RIGHT$(i$, 2)
        CASE "xx/xx/xx"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 4, 2)
            y$ = RIGHT$(i$, 2)
        CASE "x/x/xxx"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 1)
            y$ = RIGHT$(i$, 3)
        CASE "xx/x/xxx"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 4, 1)
            y$ = RIGHT$(i$, 3)
        CASE "x/xx/xxx"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 2)
            y$ = RIGHT$(i$, 3)
        CASE "xx/xx/xxx"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 4, 2)
            y$ = RIGHT$(i$, 3)
        CASE "x/x/xxxx"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 1)
            y$ = RIGHT$(i$, 4)
        CASE "x/x/xxxx"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 1)
            y$ = RIGHT$(i$, 4)
        CASE "xx/x/xxxx"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 4, 1)
            y$ = RIGHT$(i$, 4)
        CASE "x/xx/xxxx"
            m$ = LEFT$(i$, 1)
            d$ = MID$(i$, 3, 2)
            y$ = RIGHT$(i$, 4)
        CASE "xx/xx/xxxx"
            m$ = LEFT$(i$, 2)
            d$ = MID$(i$, 4, 2)
            y$ = RIGHT$(i$, 4)
    END SELECT
    i$ = RIGHT$("00" + m$, 2) + "/" + RIGHT$("00" + d$, 2) + "/" + RIGHT$(LEFT$(RIGHT$(DATE$, 4), 4 - LEN(y$)) + y$, 4)
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + l$, 38)
    r$ = LEFT$(r$ + STRING$(38, 32), 38)
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, o$, x%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE e% + 1: Dln = STRING$(MXL, 205)
        CASE ELSE: Dln = STRING$(MXL, 32)
    END SELECT
    o$ = LEFT$(Dln, 3)
    i$ = MID$(Dln, 4)
    SELECT CASE UCASE$(o$)
        CASE "[A]": f% = 10: b% = Bgc%
        CASE "[B]": f% = 11: b% = Bgc%
        CASE "[D]": f% = 11: b% = Bgc%
        CASE "[M]": f% = 13: b% = Bgc%
        CASE "[T]": f% = 11: b% = Bgc%
        CASE "[?]": f% = 14: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    SayIt i$, TOP + x% - 1, f%, b%
    NEXT
END SUB

SUB Sort
    DIM b$, d$, i$, p$
    CLOSE
    OPEN PRGM + ".txt" FOR INPUT AS #1
    OPEN "temp.txt" FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    INPUT #1, d$, b$, p$
    i$ = RIGHT$(d$, 4) + LEFT$(d$, 2) + MID$(d$, 4, 2)
    PRINT #2, i$; ","; b$; ","; p$
    WEND
    CLOSE
    SHELL "sort temp.txt > temp.sor /R"
    OPEN "temp.sor" FOR INPUT AS #1
    OPEN PRGM + ".tmp" FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    INPUT #1, d$, b$, p$
    i$ = MID$(d$, 5, 2) + "/" + RIGHT$(d$, 2) + "/" + LEFT$(d$, 4)
    PRINT #2, i$; ","; b$; ","; p$
    WEND
    CLOSE
    KILL "temp.sor"
    KILL "temp.txt"
END SUB

