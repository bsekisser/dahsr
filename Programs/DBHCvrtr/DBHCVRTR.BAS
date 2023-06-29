DECLARE SUB Cuckoo ()
DECLARE SUB Initialize ()
DECLARE SUB Getkey (a%, k$)
DECLARE SUB Main ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Convert (i$)
DECLARE SUB BtoD (b$, d$)
DECLARE SUB DtoH (d$, h$)
DECLARE SUB DtoB (d$, b$)
DECLARE SUB HtoD (h$, d$)
    COMMON SHARED Bgc%
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Stp%
    COMMON SHARED Prompt$
    COMMON SHARED Title$
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB BtoD (b$, d$)
    DIM v AS SINGLE, x%
    FOR x% = 1 TO LEN(b$)
    IF MID$(b$, x%, 1) = "1" THEN v = v + (2 ^ (LEN(b$) - x%))
    NEXT
    d$ = MID$(STR$(v), 2)
END SUB

SUB Convert (i$)
    DIM b$, c$, d$, h$, o$
    b$ = "?"
    c$ = CHR$(27) + " " + CHR$(26)
    d$ = "?"
    h$ = "?"
    IF LEN(i$) > 1 THEN
        o$ = LEFT$(i$, 1)
        SELECT CASE o$
            CASE "B"
                b$ = MID$(i$, 2)
                BtoD b$, d$
                DtoH d$, h$
            CASE "D"
                d$ = MID$(i$, 2)
                DtoH d$, h$
                DtoB d$, b$
            CASE "H"
                h$ = MID$(i$, 2)
                HtoD h$, d$
                DtoB d$, b$
        END SELECT
    END IF
    SayIt "Binary " + b$ + " " + c$ + " Decimal " + d$ + " " + c$ + " Hex " + h$, 20, 14, Bgc%
    i$ = ""
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Default (i$)
    CLS
    SayIt "Error: " + i$ + " Fault !", 12, 14, 4
    Cuckoo
    END
END SUB

SUB Display (d$, b$, h$)
    SayIt "Decimal " + d$ + " = Binary " + b$ + " = Hex " + h$, 20, 10, Bgc%
END SUB

SUB DtoB (d$, b$)
    DIM n$, s$, v, x%, y$
    b$ = ""
    n$ = d$
    s$ = "0"
    v = 0
    DO
    IF VAL(n$) > 2 ^ v THEN v = v + 1 ELSE EXIT DO
    LOOP
    FOR x% = v TO 0 STEP -1
    y$ = "0"
    IF VAL(n$) >= 2 ^ x% THEN
        y$ = "1"
        n$ = MID$(STR$(VAL(n$) - (2 ^ x%)), 2)
        IF s$ = "0" THEN s$ = "1"
    END IF
    IF b$ = "0" THEN
        b$ = y$
    ELSE
        b$ = b$ + y$
    END IF
    NEXT
END SUB

SUB DtoH (d$, h$)
    h$ = HEX$(VAL(d$))
END SUB

SUB Getkey (a%, k$)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = UCASE$(INKEY$)
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB HtoD (h$, d$)
    DIM b%, s$, v, x%, y%
    v = 0
    s$ = "0123456789ABCDEF"
    b% = LEN(s$)
    FOR x% = 1 TO LEN(h$)
    FOR y% = 1 TO LEN(s$)
    IF MID$(h$, x%, 1) = MID$(s$, y%, 1) THEN
        v = v + (b% ^ (LEN(h$) - x%)) * (y% - 1)
    END IF
    NEXT
    NEXT
    d$ = MID$(STR$(v), 2)
END SUB

SUB Initialize
    DIM c$
    c$ = CHR$(27) + " " + CHR$(26)
    Bgc% = 1
    Fgc% = 15
    Flt% = 0
    Prompt$ = "ù ESC to End ù"
    Title$ = "Binary " + c$ + " Decimal " + c$ + " Hex Number Inter-Converter ù " + RIGHT$(DATE$, 4)
    Stp% = 9
END SUB

SUB Main
    DIM a%, k$, n$
    Mask
    Convert n$
    DO
    SayIt "Enter Number:" + n$ + "_", 18, 10, Bgc%
    DO
    Getkey a%, k$
    SELECT CASE k$
        CASE CHR$(8)
            IF LEN(n$) = 1 THEN
                n$ = ""
                EXIT DO
            ELSE
                n$ = LEFT$(n$, LEN(n$) - 1)
                EXIT DO
            END IF
        CASE CHR$(13)
            IF LEN(n$) > 0 THEN
                Convert n$
                EXIT DO
            END IF
        CASE "A" TO "F", "H", "0" TO "9"
            IF n$ = "" THEN
                SELECT CASE k$
                    CASE "B", "D", "H": n$ = k$: EXIT DO
                END SELECT
            ELSE
                SELECT CASE LEFT$(n$, 1)
                    CASE "B"
                        SELECT CASE k$
                            CASE "0", "1": n$ = n$ + k$: EXIT DO
                        END SELECT
                    CASE "D"
                        SELECT CASE k$
                            CASE "0" TO "9": n$ = n$ + k$: EXIT DO
                        END SELECT
                    CASE "H"
                        SELECT CASE k$
                            CASE "A" TO "F", "0" TO "9": n$ = n$ + k$: EXIT DO
                        END SELECT
                END SELECT
            END IF
        CASE CHR$(0) + "S": n$ = "": EXIT DO
    END SELECT
    Convert n$
    Cuckoo
    LOOP
    LOOP
END SUB

SUB Mask
    DIM v%, x%
    COLOR Fgc%, Bgc%
    CLS
    FOR x% = 1 TO 23
    SELECT CASE x%
        CASE 1: PRINT "É"; STRING$(78, 205); "»"
        CASE 3, 17, 19, 21: PRINT "Ç"; STRING$(78, 196); "¶"
        CASE 23: PRINT "È"; STRING$(78, 205); "¼"
        CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
    END SELECT
    NEXT
    SayIt Title$, 2, 11, Bgc%
    v% = 5
    SayIt "Prefix all Entries with: (B)inary, (D)ecimal or (H)ex.", v%, 14, Bgc%
    SayIt "(B)inary Entries Only Accept Numbers 0 and 1.", v%, 11, Bgc%
    SayIt "(D)ecimal Entries Only Accept Numbers 0 to 9.", v%, 11, Bgc%
    SayIt "(H)exa-Decimal Entries Only Accept Numbers 0 to 9 and Letters A to F.", v%, 11, Bgc%
    SayIt Prompt$, 22, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM l$, r$
    IF v% = 0 THEN v% = 2
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(39, 32) + l$, 39)
    r$ = LEFT$(r$ + STRING$(39, 32), 39)
    COLOR f%, b%
    LOCATE v%, 2
    PRINT l$; r$
    COLOR Fgc%, Bgc%
    v% = v% + 3
END SUB

