DECLARE SUB PD2 ()
DECLARE SUB AddLine (i$)
DECLARE SUB LoadSourceTxt ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Scroll (s%, e%)
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Bufferize ()
DECLARE SUB Mask ()
DECLARE SUB Main ()
DECLARE SUB PN1 ()
DECLARE SUB PD1 ()
DECLARE SUB Display (b%, k$)
DECLARE SUB SetUpPbse ()
    CONST MXD = 1, MXL = 76, WSZ = MXL * 14
    COMMON SHARED Bfr$
    COMMON SHARED Bgc%
    COMMON SHARED Dbse$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Nof%
    COMMON SHARED Pbse$
    COMMON SHARED Sbse$
    COMMON SHARED Src$
    COMMON SHARED Valid$
    DIM SHARED Hdr$(1, 9)
    ON ERROR GOTO Trap
    Initialize
    Bufferize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB AddLine (i$)
    DIM x%
    FOR x% = 1 TO LEN(i$)
    PRINT #2, MID$(i$, x%, 1)
    NEXT
END SUB

SUB Bufferize
    SetUpPbse
    LoadSourceTxt
    Nof% = 0
    PD1
    PD2
    PN1
END SUB

SUB Display (b%, k$)
    DIM a%, e%, s%, x%
    CLOSE : OPEN Pbse$ + MID$(STR$(b%), 2) + ".txt" FOR RANDOM AS #1 LEN = MXD + 2
    s% = 1
    e% = LOF(1) / (MXD + 2)
    SayIt Hdr$(0, b%), 19, 10, Bgc%
    SayIt Hdr$(1, b%), 20, 10, Bgc%
    SayIt "Document Source File: " + CHR$(34) + Src$ + CHR$(34), 21, 11, Bgc%
    DO
    IF s% < 1 THEN s% = 1
    IF s% > e% THEN s% = e%
    Scroll s%, e%
    DO
    Getkey a%, k$
    SELECT CASE UCASE$(k$)
        CASE "-", "+": EXIT SUB
        CASE CHR$(9): SHELL "notepad.exe beale.ini": RUN "beale.bas"
        CASE CHR$(8), CHR$(13): EXIT SUB
        CASE CHR$(0) + "H": s% = s% - MXL: EXIT DO
        CASE CHR$(0) + "P": s% = s% + MXL: EXIT DO
        CASE CHR$(0) + "K": s% = s% - 1: EXIT DO
        CASE CHR$(0) + "M": s% = s% + 1: EXIT DO
        CASE CHR$(0) + "G": s% = 1: EXIT DO
        CASE CHR$(0) + "O": s% = e%: EXIT DO
        CASE CHR$(0) + "I", CHR$(0) + "Q": EXIT SUB
        CASE "1" TO "9": EXIT SUB
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
END SUB

SUB Initialize
    DIM i$, x%
    Bfr$ = "beale.bfr"
    Bgc% = 1
    Dbse$ = "dbse\"
    Fgc% = 15
    Pbse$ = "pbse\"
    Sbse$ = "sbse\"
    Src$ = "src.txt"
    Valid$ = "+"
    Flt% = 0
    CLOSE : OPEN "beale.ini" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(LEFT$(i$, x%))
            CASE "SRC=", "SOURCE=": Src$ = MID$(i$, x% + 1)
        END SELECT
        NEXT
        WEND
    END IF
    CLOSE
END SUB

SUB LoadSourceTxt
    IF Src$ = "" THEN Valid$ = "-": EXIT SUB
    DIM i$, x%
    Mask
    SayIt "Standby, Loading Source Document!", 2, 27, Bgc%
    Flt% = 0
    CLOSE #1: OPEN Sbse$ + Src$ FOR INPUT AS #1
    IF Flt% = 0 THEN
        CLOSE #2: OPEN "source.txt" FOR OUTPUT AS #2
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        SELECT CASE UCASE$(MID$(i$, x%, 1))
            CASE "0" TO "9", "A" TO "Z": PRINT #2, UCASE$(MID$(i$, x%, 1))
        END SELECT
        NEXT
        WEND
    ELSE
        Valid$ = "-"
    END IF
END SUB

SUB Main
    DIM b%, k$
    Mask
    IF Valid$ = "+" THEN
        b% = 1
        DO
        IF b% < 1 THEN b% = Nof%
        IF b% > Nof% THEN b% = 1
        Display b%, k$
        SELECT CASE k$
            CASE CHR$(0) + "I", CHR$(8), "-": b% = b% - 1
            CASE CHR$(0) + "Q", CHR$(13), "+": b% = b% + 1
            CASE "1" TO "9": b% = VAL(k$)
        END SELECT
        LOOP
    ELSE
        SayIt "Error: Source File " + CHR$(34) + Src$ + CHR$(34) + " Not Valid", 2, 14, 4
    END IF
END SUB

SUB Mask
    DIM x%
    CLS
    COLOR Fgc%, Bgc%
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, 18: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt "The Code of Thomas J. Beale", 2, 11, Bgc%
    SayIt "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") ù ENTER Page(+ -) ù ESC to End ù", 22, Fgc%, Bgc%
END SUB

SUB PD1
    DIM i$, v$, x%, y%
    Flt% = 0
    FOR x% = 1 TO 3
    Nof% = Nof% + 1
    CLOSE
    OPEN Dbse$ + MID$(STR$(x%), 2) + ".txt" FOR INPUT AS #1
    OPEN Pbse$ + MID$(STR$(Nof%), 2) + ".txt" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        Hdr$(0, Nof%) = "Each Code is Counted from the First Character."
        LINE INPUT #1, Hdr$(1, Nof%)
        WHILE NOT EOF(1)
        INPUT #1, v$
        CLOSE #3: OPEN "source.txt" FOR INPUT AS #3
        FOR y% = 1 TO VAL(v$)
        IF Flt% = 0 THEN
            INPUT #3, i$
        ELSE
            Flt% = 0
            i$ = ""
            EXIT FOR
        END IF
        NEXT
        AddLine i$
        WEND
    ELSE
        Flt% = 0
    END IF
    NEXT
END SUB

SUB PD2
    DIM i$, v$, x%, y%
    Flt% = 0
    FOR x% = 1 TO 3
    Nof% = Nof% + 1
    CLOSE
    OPEN Dbse$ + MID$(STR$(x%), 2) + ".txt" FOR INPUT AS #1
    OPEN Pbse$ + MID$(STR$(Nof%), 2) + ".txt" FOR OUTPUT AS #2
    OPEN "source.txt" FOR INPUT AS #3
    IF Flt% = 0 THEN
        Hdr$(0, Nof%) = "Each Code is Counted Starting Where Last Code Ended."
        LINE INPUT #1, Hdr$(1, Nof%)
        WHILE NOT EOF(1)
        INPUT #1, v$
        FOR y% = 1 TO VAL(v$)
        IF Flt% = 0 THEN
            INPUT #3, i$
        ELSE
            Flt% = 0
            i$ = ""
            EXIT FOR
        END IF
        NEXT
        AddLine i$
        WEND
    ELSE
        Flt% = 0
    END IF
    NEXT
END SUB

SUB PN1
    DIM v$, x%
    Flt% = 0
    FOR x% = 1 TO 3
    Nof% = Nof% + 1
    CLOSE
    OPEN Dbse$ + MID$(STR$(x%), 2) + ".txt" FOR INPUT AS #1
    OPEN Pbse$ + MID$(STR$(Nof%), 2) + ".txt" FOR OUTPUT AS #2
    IF Flt% = 0 THEN
        Hdr$(0, Nof%) = "Code Numbers as Documented."
        LINE INPUT #1, Hdr$(1, Nof%)
        WHILE NOT EOF(1)
        INPUT #1, v$
        AddLine "(" + RIGHT$("000" + v$, 3) + ")"
        WEND
    ELSE
        Flt% = 0
    END IF
    NEXT
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM h%, l$, r$, w%
    h% = 3
    w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
    r$ = LEFT$(r$ + STRING$(w%, 32), w%)
    LOCATE v%, h%
    COLOR f%, b%
    PRINT l$; r$
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%)
    DIM b%, d AS STRING * MXD, f%, h%, i$, v%, x%
    h% = 1
    v% = 1
    FOR x% = 1 TO WSZ
    SELECT CASE s% + x% - 1
        CASE 1 TO e%: GET #1, s% + x% - 1, d
        CASE ELSE: d = STRING$(MXD, 32)
    END SELECT
    i$ = LEFT$(d, MXD)
    SELECT CASE UCASE$(LEFT$(i$, 1))
        CASE "0" TO "9", "A" TO "Z": f% = 10: b% = Bgc%
        CASE ELSE: f% = Fgc%: b% = Bgc%
    END SELECT
    LOCATE v% + 3, h% + 2
    COLOR f%, b%
    PRINT i$
    h% = h% + 1
    IF h% > MXL THEN h% = 1: v% = v% + 1
    NEXT
END SUB

SUB Search

END SUB

SUB SetUpPbse
    MKDIR LEFT$(Pbse$, LEN(Pbse$) - 1)
END SUB

