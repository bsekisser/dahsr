REM Originally Created on ? by Douglas A. Hughes Sr.
REM Edited on 2 July, 2015.
DECLARE SUB Bufferize ()
DECLARE SUB Calculate (s$)
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
TYPE dahsr
    brn AS STRING * 10
    typ AS STRING * 56
    mns AS STRING * 2
    wks AS STRING * 2
    dys AS STRING * 2
END TYPE
    CONST BOOT = "winery.bas", MXL = 72
    CONST PRGM = "winery", WSZ = 15
    COMMON SHARED Bgc%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED MyFile$
    COMMON SHARED Src$
    COMMON SHARED Today$
    DIM SHARED Mth(12) AS INTEGER, Wine AS dahsr
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA 31,28,31,30,31,30,31,31,30,31,30,31

SUB Bufferize
    DIM a$, d$, i$, x%
    Flt% = 0
    CLOSE
    OPEN PRGM + ".txt" FOR INPUT AS #1
    IF Flt% = 0 THEN
        OPEN PRGM + ".bfr" FOR OUTPUT AS #2
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        SELECT CASE LEFT$(i$, 1)
            CASE "0" TO "9"
                Wine.brn = i$
                Wine.typ = ""
                FOR x% = 1 TO LEN(i$)
                IF MID$(i$, x%, 1) = "," THEN
                    IF MID$(i$, x% + 1) = " " THEN
                        x% = x%
                    ELSE
                        i$ = LEFT$(i$, x%) + " " + MID$(i$, x% + 1)
                    END IF
                END IF
                NEXT
                FOR x% = 1 TO LEN(i$)
                IF MID$(i$, x%, 1) = "," THEN
                    Wine.brn = LEFT$(i$, x% - 1)
                    Wine.typ = MID$(i$, x% + 1)
                    EXIT FOR
                END IF
                NEXT
                Calculate Wine.brn
                PRINT #2, Wine.brn; Wine.typ; Wine.mns; Wine.wks; Wine.dys
        END SELECT
        WEND
    END IF
    CLOSE
END SUB

SUB Calculate (s$)
    DIM d%, i$, l%, m%, n, w%, x%, y%
    a$ = "0"
    d% = 0
    i$ = ""
    m% = 0
    n = 0
    y% = 0
    FOR x% = 1 TO LEN(s$)
    SELECT CASE MID$(s$, x%, 1)
        CASE "0" TO "9": i$ = i$ + MID$(s$, x%, 1)
    END SELECT
    NEXT
    i$ = RIGHT$(STRING$(6, "0") + LEFT$(i$, 4) + RIGHT$(i$, 2), 6)
    i$ = LEFT$(i$, 2) + "/" + MID$(i$, 3, 2) + "/" + RIGHT$(i$, 2)
    m% = VAL(LEFT$(i$, 2))
    d% = VAL(MID$(i$, 4, 2))
    y% = VAL(RIGHT$(i$, 2))
    DO
    IF i$ = Today$ THEN EXIT DO
    SELECT CASE y%
        CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44: Mth(2) = 29
        CASE ELSE: Mth(2) = 28
    END SELECT
    n = n + 1
    d% = d% + 1
    IF d% > Mth(m%) THEN d% = 1: m% = m% + 1
    IF m% > 12 THEN m% = 1: y% = y% + 1: IF y% > 99 THEN EXIT DO
    a$ = RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/"
    a$ = a$ + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/"
    a$ = a$ + RIGHT$("00" + MID$(STR$(y%), 2), 2)
    IF a$ = Today$ THEN EXIT DO
    LOOP WHILE n < 365 * 2
    mo = INT(n / 30)
    n = n - INT(mo * 30)
    we = INT(n / 7)
    n = n - INT(we * 7)
    da = n
    Wine.mns = RIGHT$("00" + MID$(STR$(mo), 2), 2)
    Wine.wks = RIGHT$("00" + MID$(STR$(we), 2), 2)
    Wine.dys = RIGHT$("00" + MID$(STR$(da), 2), 2)
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
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
    DIM i$, x%
    Bgc% = 1
    Fgc% = 15
    Flt% = 0
    MyFile$ = "myfiles\myfiles" + RIGHT$(BOOT, 4)
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
    FOR x% = 1 TO 12: READ Mth(x%): NEXT
    SHELL "dir > src.shl"
    CLOSE : OPEN "src.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
    CLOSE
    KILL "src.shl"
    Src$ = MID$(i$, 15) + "\"
    MyFile$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM) - 1) + MyFile$
END SUB

SUB Main
    DIM a%, e%, k$, s%
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
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
            'PRINT LEFT$(MyFile$, LEN(MyFile$) - 12): END
            CHDIR LEFT$(MyFile$, LEN(MyFile$) - 12)
            RUN MyFile$
            CHDIR LEFT$(Src$, LEN(Src$) - 1)
            RUN BOOT
        CASE CHR$(9): SHELL "notepad.exe " + PRGM + ".txt": RUN BOOT
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - WSZ + 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + WSZ - 1: EXIT DO
        CASE "*": RUN BOOT
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
    Wine.brn = "Born"
    Wine.typ = " Type"
    Wine.mns = "Mn"
    Wine.wks = "Wk"
    Wine.dys = "Dy"
    CLS
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3: PRINT "ÇÄ"; STRING$(LEN(Wine.brn), 196); "Â"; STRING$(LEN(Wine.typ), 196); "Â"; STRING$(LEN(Wine.mns), 196); "Â"; STRING$(LEN(Wine.wks), 196); "Â"; STRING$(LEN(Wine.dys), 196); "Ä¶"
        CASE 4: PRINT "º "; Wine.brn; "³"; Wine.typ; "³"; Wine.mns; "³"; Wine.wks; "³"; Wine.dys; " º"
        CASE 5: PRINT "ÇÄ"; STRING$(LEN(Wine.brn), 196); "Å"; STRING$(LEN(Wine.typ), 196); "Å"; STRING$(LEN(Wine.mns), 196); "Å"; STRING$(LEN(Wine.wks), 196); "Å"; STRING$(LEN(Wine.dys), 196); "Ä¶"
        CASE 6 TO WSZ + 5: PRINT "º "; STRING$(LEN(Wine.brn), 32); "³"; STRING$(LEN(Wine.typ), 32); "³"; STRING$(LEN(Wine.mns), 32); "³"; STRING$(LEN(Wine.wks), 32); "³"; STRING$(LEN(Wine.dys), 32); " º"
        CASE WSZ + 6: PRINT "ÇÄ"; STRING$(LEN(Wine.brn), 196); "Á"; STRING$(LEN(Wine.typ), 196); "Á"; STRING$(LEN(Wine.mns), 196); "Á"; STRING$(LEN(Wine.wks), 196); "Á"; STRING$(LEN(Wine.dys), 196); "Ä¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt "My Winery Summary ù Today is " + Today$, 2, 11, Bgc%
    SayIt "ù Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù TAB to Edit ù BACKSPACE to Exit ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$, x%
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
    DIM b%, f%, i$, m$, x%, y%
    m$ = CHR$(28)
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Wine
        CASE e% + 1
            Wine.brn = STRING$(LEN(Wine.brn), 205)
            Wine.typ = STRING$(LEN(Wine.typ), 205)
            Wine.mns = STRING$(LEN(Wine.mns), 205)
            Wine.wks = STRING$(LEN(Wine.wks), 205)
            Wine.dys = STRING$(LEN(Wine.dys), 205)
        CASE ELSE
            Wine.brn = STRING$(LEN(Wine.brn), 32)
            Wine.typ = STRING$(LEN(Wine.typ), 32)
            Wine.mns = STRING$(LEN(Wine.mns), 32)
            Wine.wks = STRING$(LEN(Wine.wks), 32)
            Wine.dys = STRING$(LEN(Wine.dys), 32)
    END SELECT
    SELECT CASE UCASE$(LEFT$(Wine.mns, 1))
        CASE "0" TO "9", "A" TO "Z"
            IF VAL(Wine.mns) = 0 AND VAL(Wine.wks) = 0 AND VAL(Wine.dys) = 0 THEN
                f% = 13
                b% = Bgc%
            ELSE
                IF VAL(Wine.dys) > 0 THEN f% = 14: b% = Bgc%
                IF VAL(Wine.wks) > 0 THEN f% = 11: b% = Bgc%
                IF VAL(Wine.mns) > 0 THEN f% = 10: b% = Bgc%
            END IF
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    COLOR f%, b%
    LOCATE x% + 5, 3
    PRINT Wine.brn; m$;
    PRINT Wine.typ; m$;
    PRINT Wine.mns; m$;
    PRINT Wine.wks; m$;
    PRINT Wine.dys
    NEXT
END SUB

