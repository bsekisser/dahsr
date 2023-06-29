DECLARE SUB Decant (i$, l$, f$, b$, d$)
DECLARE SUB Calculate (b$, d$, a$)
DECLARE SUB Scroll (s%, e%)
DECLARE SUB Bufferize ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
    CONST BOOT = "ourages.bas", INI = "ourages.ini", MXL = 77
    CONST PRGM = "ourages", WSZ = 15
    COMMON SHARED Age AS STRING * MXL
    COMMON SHARED Bfr$
    COMMON SHARED Bgc%
    COMMON SHARED Code%
    COMMON SHARED Dbse$
    COMMON SHARED Ext$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Mth(12)
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA 31,28,31,30,31,30,31,31,30,31,30,31

SUB Bufferize
    DIM a$, b$, d$, f$, l$, n$, o$
    Mask
    SayIt "Standby, Loading Names. . .", 2, Fgc%, Bgc%
    SayIt "Standby, Loading Names. . .", 22, Fgc%, Bgc%
    SHELL "sort " + Dbse$ + " > temp.txt"
    KILL Dbse$
    NAME "temp.txt" AS Dbse$
    Flt% = 0
    CLOSE
    OPEN Dbse$ FOR INPUT AS #1
    OPEN Bfr$ FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        Decant i$, l$, f$, b$, d$
        SELECT CASE UCASE$(LEFT$(l$, 1))
                CASE "0" TO "9", "A" TO "Z": l$ = l$
                CASE ELSE: f$ = "": l$ = "Anon"
        END SELECT
        SELECT CASE UCASE$(LEFT$(b$, 1))
                CASE "0" TO "9": b$ = b$
                CASE ELSE: b$ = Today$
        END SELECT
        SELECT CASE UCASE$(LEFT$(d$, 1))
                CASE "0" TO "9": d$ = d$: o$ = "-"
                CASE ELSE: d$ = Today$: o$ = "+"
        END SELECT
        Calculate b$, d$, a$
        IF b$ = Today$ THEN b$ = STRING$(LEN(Today$), 46)
        IF d$ = Today$ THEN d$ = STRING$(LEN(Today$), 46)
        b$ = RIGHT$(STRING$(MXL, 46) + "³" + b$, 11)
        d$ = RIGHT$(STRING$(MXL, 46) + "³" + d$, 11)
        a$ = "³" + RIGHT$(STRING$(3, 32) + a$, 3)
        IF Code% = 0 THEN
            PRINT #2, LEFT$(o$ + l$ + ", " + f$ + STRING$(MXL, 46), MXL - LEN(b$) - LEN(d$) - LEN(a$)); b$; d$; a$
        ELSEIF Code% = 1 THEN
            PRINT #2, LEFT$(o$ + f$ + " " + l$ + STRING$(MXL, 46), MXL - LEN(b$) - LEN(d$) - LEN(a$)); b$; d$; a$
        ELSEIF Code% = 2 THEN
            PRINT #2, LEFT$(o$ + l$ + " (" + f$ + ")" + STRING$(MXL, 46), MXL - LEN(b$) - LEN(d$) - LEN(a$)); b$; d$; a$
        ELSEIF Code% = 3 THEN
            PRINT #2, LEFT$(o$ + f$ + " (" + l$ + ")" + STRING$(MXL, 46), MXL - LEN(b$) - LEN(d$) - LEN(a$)); b$; d$; a$
        ELSE
            PRINT #2, LEFT$(o$ + f$ + " " + l$ + STRING$(MXL, 46), MXL - LEN(b$) - LEN(d$) - LEN(a$)); b$; d$; a$
        END IF
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE
END SUB

SUB Calculate (b$, t$, a$)
    DIM d%, m%, y%
    a$ = "0"
    IF b$ = Today$ OR b$ = t$ THEN EXIT SUB
    m% = VAL(LEFT$(b$, 2))
    d% = VAL(MID$(b$, 4, 2))
    y% = VAL(RIGHT$(b$, 4))
    Flt% = 0
    DO
    SELECT CASE VAL(RIGHT$(STR$(y%), 2))
        CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48: Mth(2) = 29
        CASE 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: Mth(2) = 29
        CASE ELSE: Mth(2) = 28
    END SELECT
    d% = d% + 1
    IF d% > Mth(m%) THEN d% = 1: m% = m% + 1
    IF m% > 12 THEN m% = 1: y% = y% + 1
    IF m% = VAL(LEFT$(t$, 2)) AND d% = VAL(MID$(t$, 4, 2)) THEN
        IF y% = VAL(RIGHT$(t$, 4)) THEN EXIT DO
        a$ = MID$(STR$(VAL(a$) + 1), 2)
    END IF
    LOOP WHILE Flt% = 0
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Decant (i$, l$, f$, b$, d$)
    f$ = "": l$ = "": b$ = "": d$ = ""
    CLOSE #9: OPEN "decant.txt" FOR OUTPUT AS #9
    PRINT #9, i$
    CLOSE #9: OPEN "decant.txt" FOR INPUT AS #9
    INPUT #9, l$, f$, b$, d$
    CLOSE #9
    KILL "decant.txt"
END SUB

SUB Getkey (k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    IF k$ = "*" THEN
        RUN BOOT
    ELSEIF k$ = CHR$(27) THEN
        END
    END IF
END SUB

SUB Initialize
    DIM i$, o$, x%
    CLS
    Bfr$ = PRGM + ".bfr"
    Bgc% = 1
    Code% = 0
    Dbse$ = PRGM + ".txt"
    IF UCASE$(RIGHT$(BOOT, 4)) = ".EXE" THEN Ext$ = ".mod" ELSE : Ext$ = ".sub"
    Fgc% = 15
    Title$ = "My QBasic Age Calculator"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    DO
    Flt% = 0: CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "CODE=": Code% = VAL(MID$(i$, x% + 1)): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        IF o$ = "" THEN
            o$ = "+"
        ELSE
            SayIt "INI Error", 2, 14, 4
            END
        END IF
        CLOSE : OPEN INI FOR OUTPUT AS #1
        PRINT #1, "CODE="; MID$(STR$(Code%), 2)
    END IF
    LOOP
    CLOSE
    FOR x% = 1 TO 12
    READ Mth(x%)
    NEXT
END SUB

SUB Main
    DIM e%, k$, s%
    Mask
    CLOSE : OPEN Bfr$ FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + Dbse$: Bufferize: RUN BOOT
        CASE CHR$(0) + CHR$(15): SHELL "notepad.exe " + PRGM + ".ini": Bufferize: RUN BOOT
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "O": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM x%
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
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3: PRINT "Ç"; STRING$(51, 196); "Â"; STRING$(10, 196); "Â"; STRING$(10, 196); "Â"; STRING$(4, 196); "¶"
        CASE 4: PRINT "º"; STRING$(51, 32); "³"; STRING$(10, 32); "³"; STRING$(10, 32); "³"; STRING$(4, 32); "º"
        CASE 5: PRINT "Ç"; STRING$(51, 196); "Å"; STRING$(10, 196); "Å"; STRING$(10, 196); "Å"; STRING$(4, 196); "¶"
        CASE 6 TO WSZ + 5: PRINT "º"; STRING$(51, 32); "³"; STRING$(10, 32); "³"; STRING$(10, 32); "³"; STRING$(4, 32); "º"
        CASE WSZ + 6: PRINT "Ç"; STRING$(51, 196); "Á"; STRING$(10, 196); "Á"; STRING$(10, 196); "Á"; STRING$(4, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$ + " ù Today is " + Today$, 2, 11, Bgc%
    COLOR 11, Bgc%
    LOCATE 4, 3: PRINT "Name"
    LOCATE 4, 54: PRINT "Born"
    LOCATE 4, 65: PRINT "Died"
    LOCATE 4, 76: PRINT "Age"
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù TAB: Edit Data ù SHIFT/TAB: Edit Ini ù ESC to End ù", 22, Fgc%, Bgc%
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
    DIM i$, m$, o$, x%
    m$ = CHR$(28)
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, Age
        CASE e% + 1: Age = STRING$(MXL, 205)
        CASE ELSE: Age = STRING$(MXL, 32)
    END SELECT
    i$ = LEFT$(Age, MXL)
    o$ = LEFT$(i$, 1)
    i$ = MID$(i$, 2)
    SELECT CASE o$
        CASE "+": f% = 10: b% = Bgc%
        CASE "-": f% = 14: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    'SayIt i$, x% + 5, f%, b%
    COLOR f%, b%
    LOCATE x% + 5, 3
    PRINT LEFT$(i$, 50); m$; MID$(i$, 52, 10); m$; MID$(i$, 63, 10); m$; MID$(i$, 74)
    NEXT
END SUB

