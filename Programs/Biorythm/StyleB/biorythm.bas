DECLARE SUB Calculate (B$, t$, v%)
DECLARE SUB CreateGraph ()
DECLARE SUB Bufferize ()
DECLARE SUB Mask ()
DECLARE SUB Scroll (s%, e%)
DECLARE SUB SayIt (i$, v%, c%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
TYPE dahsr
    na AS STRING * 54
    bi AS STRING * 10
    de AS STRING * 10
END TYPE
    CONST BOOT = "biorythm.bas", MXL = 74, PRGM = "biorythm", MXF = 7
    CONST TOP = 55, BTM = 345, LSD = 20, RSD = 620, ECHO = 25
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Ext$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Mid%
    COMMON SHARED Title$
    COMMON SHARED Today$
    COMMON SHARED Work$
    DIM SHARED Bio AS dahsr, Mth(12), Fld(1, MXF) AS STRING * 9
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
Fields:   
DATA "23","Physical"
DATA "28","Emotional"
DATA "33","Intellectual"
DATA "53","Spiritual"
DATA "48","Awareness"
DATA "38","Intuitiveness"
DATA "43","Asthetics"
Months:
DATA 31,28,31,30,31,30,31,31,30,31,30,31

SUB Bufferize
    SHELL "sort " + Dbse$ + " > " + Work$
    Flt% = 0
    CLOSE : OPEN Work$ FOR INPUT AS #1
    OPEN PRGM + ".bfr" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        INPUT #1, Bio.na, Bio.bi, Bio.de
        SELECT CASE UCASE$(LEFT$(Bio.na, 1))
            CASE "0" TO "9", "A" TO "Z"
                IF LEFT$(Bio.na, 1) = " " THEN Bio.na = "Anon"
                PRINT #2, Bio.na; Bio.bi; Bio.de
        END SELECT
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE
END SUB

SUB Calculate (B$, t$, v%)
    IF B$ = t$ THEN v% = 30: EXIT SUB
    LINE (LSD + 10, 184)-(RSD - 10, 230), 15, B
    LINE (LSD + 11, 185)-(RSD - 11, 229), 0, BF
    DIM d%, i$, j$, m%, n$, x%, y%
    i$ = " Creating a Graph for " + RTRIM$(Bio.na) + " "
    LOCATE 13, 40 - (LEN(i$) / 2): COLOR 14: PRINT i$
    m% = VAL(LEFT$(B$, 2))
    d% = VAL(MID$(B$, 4, 2))
    y% = VAL(RIGHT$(B$, 4))
    DO
    SELECT CASE y%
        CASE 1800, 1900, 2100, 2200, 2300, 2500
            Mth(2) = 28
        CASE ELSE
            SELECT CASE VAL(RIGHT$(MID$(STR$(y%), 2), 2))
                CASE 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96: Mth(2) = 29
                CASE ELSE: Mth(2) = 28
            END SELECT
    END SELECT
    v% = v% + 1
    d% = d% + 1
    IF d% > Mth(m%) THEN
        d% = 1
        m% = m% + 1
        IF m% > 12 THEN
            m% = 1
            y% = y% + 1
        END IF
    END IF
    n$ = RIGHT$("00" + MID$(STR$(m%), 2), 2) + "/"
    n$ = n$ + RIGHT$("00" + MID$(STR$(d%), 2), 2) + "/"
    n$ = n$ + RIGHT$("0000" + MID$(STR$(y%), 2), 4)
    LOCATE 14, 40 - ((LEN(n$) + 2) / 2): PRINT " "; n$; " "
    IF n$ = t$ THEN EXIT DO
    LOOP
    COLOR Fgc%
END SUB

SUB CreateGraph
    DIM d%, e%, f%, h%, l%, r%, v, w%, x%
    LINE (LSD, TOP)-(RSD, BTM), 1, BF
    Calculate RTRIM$(Bio.bi), RTRIM$(Bio.de), d%
    LINE (LSD, TOP)-(RSD, BTM), 1, BF
    h% = BTM - TOP
    w% = RSD - LSD
    l% = LSD
    r% = LSD
    e% = 2
    FOR x% = 1 TO MXF
    v = d% / VAL(Fld(0, x%)) - INT(d% / VAL(Fld(0, x%)))
    l% = LSD + (w% / MXF) * x% - (w% / MXF) + (e% / 2)
    r% = l% + (w% / MXF) - e%
    LINE (l%, BTM - (h% * v))-(r%, BTM), 16 - x%, BF
    NEXT
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
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
    Dbse$ = PRGM + ".txt"
    IF UCASE$(RIGHT$(BOOT, 4)) = ".EXE" THEN Ext$ = ".mod" ELSE Ext$ = ".sub"
    Fgc% = 15
    Mid% = (BTM - TOP) / 2 + TOP
    Title$ = "Biorythm Readings"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    Work$ = PRGM + ".wrk"
    Flt% = 0
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
    RESTORE Fields
    FOR x% = 1 TO MXF
    READ Fld(0, x%), Fld(1, x%)
    NEXT
    RESTORE Months: FOR x% = 1 TO 12: READ Mth(x%): NEXT
END SUB

SUB Main
    DIM e%, k$, s%
    Mask
    CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
    e% = LOF(1) / (MXL + 2)
    s% = 1
    DO
    IF s% < 1 THEN s% = e%
    IF s% > e% THEN s% = 1
    Scroll s%, e%
    DO
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(9): SHELL "notepad.exe " + Dbse$: RUN BOOT
        CASE CHR$(13): CreateGraph: EXIT DO
        CASE CHR$(0) + ";": SHELL "notepad.exe " + PRGM + ".fld": RUN BOOT
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
    END SELECT
    LOOP
    LOOP
END SUB

SUB Mask
    DIM i$, x%
    SCREEN 12
    Bio.na = "Name"
    Bio.bi = "Born"
    Bio.de = RIGHT$(STRING$(LEN(Bio.de), 32) + "Died", LEN(Bio.de))
    LINE (0, 0)-(640, 480), 0, BF
    LINE (LSD - 1, TOP - 1)-(RSD + 1, BTM + 1), 15, B
    LINE (LSD, TOP)-(RSD, BTM), 1, BF
    SayIt Title$ + " for " + Today$, 1, 11
    SayIt STRING$(80, 196), 2, Fgc%
    FOR x% = 1 TO MXF
    COLOR 16 - x%: LOCATE 3, 76 / MXF * x% - ((76 / MXF) / 1.65): PRINT Fld(1, x%)
    NEXT
    SayIt Bio.na + " " + Bio.bi + " " + Bio.de, ECHO - 2, Fgc%
    SayIt STRING$(80, 45), ECHO - 1, Fgc%
    SayIt STRING$(80, 196), ECHO + 1, Fgc%
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") to Select a Name and Then Press ENTER to Create the Graph ù", ECHO + 2, Fgc%
    SayIt "ù F1: Edit Fields ù ESC to End", ECHO + 3, Fgc%
END SUB

SUB SayIt (i$, v%, c%)
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
    COLOR c%
    LOCATE v%, h%
    PRINT l$; r$
    COLOR Fgc%
END SUB

SUB Scroll (s%, e%)
    DIM B%, f%, i$, m$
    m$ = CHR$(32)
    SELECT CASE s%
        CASE 1 TO e%: GET #1, s%, Bio
        CASE ELSE
            Bio.na = ""
            Bio.bi = ""
            Bio.de = ""
    END SELECT
    SELECT CASE UCASE$(LEFT$(Bio.na, 1))
        CASE "0" TO "9", "A" TO "Z": f% = 10: B% = Bgc%
        CASE ELSE: f% = Fgc%: B% = Bgc%
    END SELECT
    SELECT CASE LEFT$(Bio.bi, 1)
        CASE "0" TO "9": Bio.bi = Bio.bi
        CASE ELSE: Bio.bi = ""
    END SELECT
    SELECT CASE LEFT$(Bio.de, 1)
        CASE "0" TO "9": Bio.de = Bio.de
        CASE ELSE: Bio.de = ""
    END SELECT
    SayIt Bio.na + m$ + Bio.bi + m$ + Bio.de, ECHO, f%
    i$ = RTRIM$(Bio.bi)
    IF i$ = "" THEN Bio.bi = Today$
    i$ = RTRIM$(Bio.de)
    IF i$ = "" THEN Bio.de = Today$
END SUB

