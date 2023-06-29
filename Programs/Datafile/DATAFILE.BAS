DECLARE SUB Insert ()
DECLARE SUB Delete ()
DECLARE SUB SaveFld ()
DECLARE SUB LoadFld ()
DECLARE SUB Display ()
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "datafile.bas", MXL = 76, PRGM = "datafile", WSZ = 17
    COMMON SHARED Backup$
    COMMON SHARED Bgc%
    COMMON SHARED Dln AS STRING * MXL
    COMMON SHARED Ext$
    COMMON SHARED Fgc%
    COMMON SHARED Filename$
    COMMON SHARED Flt%
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Fld(1, WSZ) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA "Rx Name"
DATA "Rx Number"
DATA "Patient"
DATA "Patient's Telephone"
DATA "Doctor"
DATA "Doctor's Telephone"
DATA "Pharmacy"
DATA "Pharmacy's Telephone"
DATA "Pharmacy's Fax"
DATA "Stock Date"
DATA "Quantity"
DATA "Dosage"
DATA "Refills"
DATA "Refill Date"
DATA "Expiration Date"
DATA "Order Date"
DATA "Price"

SUB Bufferize
    DIM f$, i$, n$, x%, y%
    CLOSE
    OPEN "dbse.shl" FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    WHILE NOT EOF(1)
    LINE INPUT #1, f$
    CLOSE #3: OPEN "dbse\" + f$ FOR INPUT AS #3
    INPUT #3, n$
    PRINT #2, LEFT$(n$ + STRING$(MXL, 32), MXL - LEN(f$)); f$
    WEND
    CLOSE
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Delete
    IF Filename$ = "" THEN
        EXIT SUB
    ELSE
        SayIt "Delete " + Filename$ + " (Y) ?", 2, 14, 4
        DO
        Getkey k$
        IF UCASE$(k$) = "Y" THEN
            KILL "dbse\" + Filename$
            EXIT DO
        END IF
        LOOP UNTIL k$ = CHR$(13)
    END IF
END SUB

SUB Display
    DIM k$, o$, p%, x%
    p% = 1
    DO
    IF p% < 1 THEN p% = WSZ
    IF p% > WSZ THEN p% = 1
    FOR x% = 1 TO WSZ
    IF x% = p% THEN f% = 15: b% = 14 ELSE f% = 10: b% = Bgc%
    IF x% = 17 THEN o$ = "$" ELSE o$ = ""
    SayIt LEFT$(Fld(0, x%) + STRING$(MXL, 46), MXL - LEN(Fld(1, x%)) - LEN(o$)) + o$ + Fld(1, x%), x% + 3, f%, b%
    NEXT
    DO
    Getkey k$
    SELECT CASE k$
        CASE CHR$(8)
            IF LEN(Fld(1, p%)) > 0 THEN
                Fld(1, p%) = LEFT$(Fld(1, p%), LEN(Fld(1, p%)) - 1)
            END IF
            EXIT DO
        CASE CHR$(9): SHELL "notepad.exe dbse\" + Filename$: RUN BOOT
        CASE CHR$(13): EXIT SUB
        CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
        CASE CHR$(0) + "K": p% = p% - 1: EXIT DO
        CASE CHR$(0) + "M": p% = p% + 1: EXIT DO
        CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
        CASE CHR$(0) + "R"
            IF Fld(1, p%) = "" AND LEN(Backup$) > 0 THEN
                Fld(1, p%) = Backup$
                EXIT DO
            END IF
        CASE CHR$(0) + "S"
            IF LEN(Fld(1, p%)) > 0 THEN
                Backup$ = Fld(1, p%)
                Fld(1, p%) = ""
                EXIT DO
            END IF
        CASE CHR$(32) TO CHR$(122)
            Fld(1, p%) = Fld(1, p%) + k$
            IF LEN(Fld(0, p%)) + LEN(Fld(1, p%)) > MXL - 3 THEN
                Fld(1, p%) = MID$(Fld(1, p%), 2)
            END IF
            EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Getkey (k$)
    DIM a%
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
    SHELL "dir dbse > dbse.shl /B"
    SELECT CASE UCASE$(RIGHT$(BOOT, 4))
        CASE ".EXE": Ext$ = ".mod"
        CASE ELSE: Ext$ = ".sub"
    END SELECT
    Fgc% = 15
    Flt% = 0
    Title$ = "General Data Files"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 2)
    CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(LEFT$(i$, x%))
        END SELECT
        NEXT
        WEND
    END IF
    CLOSE
    FOR x% = 1 TO WSZ: READ Fld(0, x%): NEXT
END SUB

SUB Insert
    CLOSE #2: OPEN "dbse\newfile.txt" FOR OUTPUT AS #2
    PRINT #2, "NewFile"
    CLOSE #2
END SUB

SUB LoadFld
    DIM x%
    CLOSE #3: OPEN "dbse\" + Filename$ FOR INPUT AS #3
    FOR x% = 1 TO WSZ
    LINE INPUT #3, Fld(1, x%)
    NEXT
    CLOSE #3
END SUB

SUB Main
    DIM e%, s%
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe dbse\" + Filename$: RUN BOOT
        CASE CHR$(13): LoadFld: Display: SaveFld: EXIT DO
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "R": Insert: RUN BOOT
        CASE CHR$(0) + "S": Delete: RUN BOOT
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
        CASE 3, WSZ + 4: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " ù " + Today$, 2, 11, Bgc%
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù ENTER Selection ù TAB to Edit Selection ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB SaveFld
    DIM x%
    CLOSE #2: OPEN "dbse\" + Filename$ FOR OUTPUT AS #2
    FOR x% = 1 TO WSZ
    PRINT #2, Fld(1, x%)
    NEXT
    CLOSE #2
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM h%, l$, r$, w%
    w% = 38
    SELECT CASE w%
        CASE 38: h% = 3
        CASE 39: h% = 2
        CASE ELSE: h% = 1
    END SELECT
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    COLOR f%, b%
    LOCATE v%, h%
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, f%, i$, j$, q$, x%
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Dln
        CASE e% + 1: Dln = LEFT$(STRING$(MXL / 2 - 4, 205) + "[ EOR ]" + STRING$(MXL / 2, 205), MXL)
        CASE ELSE: Dln = STRING$(MXL, 32)
    END SELECT
    i$ = RTRIM$(LEFT$(Dln, MXL - 13))
    j$ = LTRIM$(RIGHT$(Dln, 13))
    IF x% = 1 THEN
        Filename$ = j$
        f% = 15
        b% = 4
    ELSE
        SELECT CASE UCASE$(LEFT$(i$, 1))
            CASE "0" TO "9", "A" TO "Z": f% = 10: b% = Bgc%
            CASE ELSE: f% = Fgc%: b% = Bgc%
        END SELECT
    END IF
    SELECT CASE UCASE$(LEFT$(j$, 1))
        CASE "0" TO "9", "A" TO "Z": q$ = CHR$(34)
        CASE ELSE: q$ = ""
    END SELECT
    SayIt LEFT$(i$ + STRING$(MXL, 46), MXL - LEN(j$) - 2) + q$ + j$ + q$, x% + 3, f%, b%
    NEXT
END SUB

