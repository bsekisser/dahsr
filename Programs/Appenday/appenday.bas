DECLARE SUB Main ()
    Main
END

SUB Main
    DIM b$, d%, i$, m%, n%
    b$ = "appenday"
    COLOR 15, 1
    CLS
    CLOSE
    OPEN b$ + ".txt" FOR OUTPUT AS #2
    FOR m% = 1 TO 12
    SELECT CASE m%
        CASE 2
            SELECT CASE y%
                CASE 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032, 2036, 2040, 2044: n% = 29
                CASE ELSE: n% = 28
            END SELECT
        CASE 4, 6, 9, 11: n% = 30
        CASE ELSE: n% = 31
    END SELECT
    FOR d% = 1 TO n%
    PRINT #2, RIGHT$("00" + MID$(STR$(m%), 2), 2); RIGHT$("00" + MID$(STR$(d%), 2), 2); ","
    NEXT
    NEXT
    CLOSE
    i$ = "Your Year Was Created!"
    LOCATE 12, 40 - (LEN(i$) / 2)
    PRINT i$
END SUB

