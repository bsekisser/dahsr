DECLARE SUB SortMyStoreList ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Mask ()
DECLARE SUB Initialize ()
DECLARE SUB Scroll (s%, e%, p%)
DECLARE SUB Main ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB FormatMyStoreList (c%)
DECLARE SUB SwitchItem (s%, e%, k$)
    CONST MXL = 72, WSZ = 17
    CONST MYLIST = "MYLIST", MYSTORE = "MYSTORE"
    COMMON SHARED Bgc%
    COMMON SHARED Blk$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    'ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1200, 4
    SOUND 0, 2
END SUB

SUB FormatMyStoreList (c%)
    DIM i$, j$, k$
    c% = 0
    CLOSE
    OPEN MYSTORE + ".TXT" FOR INPUT AS #1
    IF Flt% = 0 THEN
        OPEN MYSTORE + ".BUF" FOR OUTPUT AS #2
        OPEN MYLIST + ".TXT" FOR OUTPUT AS #3
        PRINT #3, "Your Shopping List: " + DATE$
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        j$ = CHR$(45)
        k$ = j$
        SELECT CASE RIGHT$(i$, 1)
            CASE ">"
                DO
                i$ = LEFT$(i$, LEN(i$) - 1)
                LOOP WHILE RIGHT$(i$, 1) = ">"
                k$ = i$
                c% = c% + 1
                PRINT #3, j$
            CASE ELSE
                j$ = i$
        END SELECT
        PRINT #2, LEFT$(j$ + STRING$(MXL / 2 - LEN(j$), 32) + k$ + STRING$(MXL, 32), MXL)
        WEND
        CLOSE
    END IF
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Initialize
    Bgc% = 1
    Blk$ = STRING$(MXL, 32)
    Fgc% = 15
    Flt% = 0
    SortMyStoreList
END SUB

SUB Main
    DIM a%, e%, f$, i$, k$, m%, x%
    STATIC p%, s%
    m% = 0
    p% = 1
    s% = 1
    Mask
    DO
    FormatMyStoreList Si%
    CLOSE
    OPEN MYSTORE + ".BUF" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    LOCATE 2, 3
    COLOR 11
    PRINT "MyStore ù " + RIGHT$(DATE$, 4) + " ù Inventory:" + STR$(e%)
    LOCATE 2, 43
    COLOR 10
    PRINT "MyStore ù Selected Items:" + STR$(Si%)
    DO
    IF s% + p% - 1 > e% THEN
        s% = s% + 1
        IF s% > e% THEN s% = e%
        p% = e% - s% + 1
    END IF
    IF p% > WSZ THEN p% = WSZ: s% = s% + 1
    IF p% < 1 THEN p% = 1: s% = s% - 1
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%, p%
    DO
    Getkey a%, k$
    SELECT CASE k$
        CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
        CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
        CASE CHR$(0) + "I": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "Q": s% = s% + WSZ - 1: EXIT DO
        CASE CHR$(0) + "G": p% = 1: s% = 1: EXIT DO
        CASE CHR$(0) + "O": p% = WSZ: s% = e%: EXIT DO
        CASE CHR$(13), CHR$(0) + "R", CHR$(0) + "S"
            SwitchItem s% + p% - 1, e%, k$
            a% = 13
            EXIT DO
    END SELECT
    LOOP
    LOOP UNTIL a% = 13
    LOOP
END SUB

SUB Mask
    DIM x%
    CLS
    COLOR Fgc%, Bgc%
    LOCATE 1, 1
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(38, 205); "»É"; STRING$(38, 205); "»"
        CASE 3, 21: PRINT "Ç"; STRING$(38, 196); "¶Ç"; STRING$(38, 196); "¶"
        CASE 23: PRINT "È"; STRING$(38, 205); "¼È"; STRING$(38, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(38, 32); "ºº"; STRING$(38, 32); "º"
    END SELECT
    NEXT
    COLOR 11, Bgc%
    LOCATE 2, 3: PRINT "MyStore ù " + RIGHT$(DATE$, 4)
    LOCATE 22, 11: PRINT "INSERT Selected Item"
    COLOR 10, Bgc%
    LOCATE 22, 51: PRINT "DELETE Selected Item"
    COLOR Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$, w%
    w% = MXL / 2
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$
    LOCATE v%, 43
    PRINT r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%, p%)
    DIM b%, d AS STRING * MXL, f%, i$, x%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, d
        CASE e% + 1: d = STRING$(MXL, 205)
        CASE ELSE: d = STRING$(MXL, 32)
    END SELECT
    i$ = LEFT$(d, MXL)
    SELECT CASE x%
        CASE IS = p%: f% = 0: b% = Fgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    SayIt i$, x% + 3, f%, b%
    NEXT
END SUB

SUB SortMyStoreList
    DIM e%, i$, x%, y%
    Flt% = 0
    CLOSE
    OPEN MYSTORE + ".TXT" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        e% = e% + 1
        WEND
        CLOSE
        REDIM item(e%) AS STRING
        OPEN MYSTORE + ".TXT" FOR INPUT AS #1
        FOR x% = 1 TO e%
        LINE INPUT #1, i$
        item(x%) = i$
        NEXT
        FOR x% = 1 TO e%
        FOR y% = x% TO e%
        IF item(x%) > item(y%) THEN SWAP item(x%), item(y%)
        NEXT
        NEXT
        CLOSE
        OPEN MYSTORE + ".TXT" FOR OUTPUT AS #1
        FOR x% = 1 TO e%
        PRINT #1, item(x%)
        NEXT
    END IF
END SUB

SUB SwitchItem (s%, e%, k$)
    DIM x%
    CLOSE : OPEN MYSTORE + ".TXT" FOR INPUT AS #1
    OPEN MYSTORE + ".TMP" FOR OUTPUT AS #2
    FOR x% = 1 TO e%
    LINE INPUT #1, i$
    IF x% = s% THEN
        SELECT CASE RIGHT$(i$, 1)
            CASE ">"
                SELECT CASE k$
                    CASE CHR$(0) + "S", CHR$(13)
                        DO
                        IF RIGHT$(i$, 1) = ">" THEN i$ = LEFT$(i$, LEN(i$) - 1)
                        LOOP WHILE RIGHT$(i$, 1) = ">"
                END SELECT
            CASE ELSE
                SELECT CASE k$
                    CASE CHR$(0) + "R", CHR$(13)
                        DO
                        IF RIGHT$(i$, 1) = ">" THEN i$ = LEFT$(i$, LEN(i$) - 1)
                        LOOP WHILE RIGHT$(i$, 1) = ">"
                        i$ = i$ + ">"
                END SELECT
        END SELECT
    END IF
    PRINT #2, i$
    NEXT
    CLOSE
    KILL MYSTORE + ".TXT"
    NAME MYSTORE + ".TMP" AS MYSTORE + ".TXT"
END SUB

