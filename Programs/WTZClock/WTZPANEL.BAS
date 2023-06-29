DECLARE SUB Initialize ()
DECLARE SUB Main ()
DECLARE SUB Mask ()
    CONST HOME = -6
    COMMON SHARED Flt%
    DIM SHARED Zone(2, 24) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Main
END
Trap: Flt% = 0: RESUME NEXT
DATA "IDL","",""
DATA "","",""
DATA "","",""
DATA "PST","San","Francisco"
DATA "MST","Denver",""
DATA "CST","Chicago",""
DATA "EST","New York","Philadelphia"
DATA "Halifax","St John's",""
DATA "","",""
DATA "","",""
DATA "London","",""
DATA "GMT","",""
DATA "","",""
DATA "Paris","",""
DATA "Cairo","Jerusalem",""
DATA "Moscow","",""
DATA "Iran","",""
DATA "India","",""
DATA "","",""
DATA "Singapore","Hong Kong",""
DATA "","",""
DATA "","",""
DATA "","",""
DATA "","",""
DATA "","",""

SUB Initialize
    ON ERROR GOTO 0
    DIM x%
    FOR y% = 1 TO 24
    FOR x% = 0 TO 2
    READ Zone(x%, y%)
    NEXT
    NEXT
END SUB

SUB Main
    Mask
    DIM a%, d$, h%, m$, o$, t%, v%, x%, z%
    DO
    t% = HOME + VAL(LEFT$(TIME$, 2))
    FOR v% = 1 TO 4
    FOR h% = 1 TO 6
    IF t% > 23 THEN t% = 0
    SELECT CASE t%
        CASE 0: d$ = "0": o$ = " mid"
        CASE 1 TO 11: d$ = MID$(STR$(t%), 2): o$ = "am"
        CASE 12: d$ = MID$(STR$(t%), 2): o$ = " noon"
        CASE ELSE: d$ = MID$(STR$(t% - 12), 2): o$ = "pm"
    END SELECT
    LOCATE v% * 5 - 5 + 2, h% * 13 - 13 + 3
    PRINT LEFT$(d$ + ":" + MID$(TIME$, 4, 2) + o$ + STRING$(12, " "), 12)
    t% = t% + 1
    NEXT
    NEXT
    m$ = MID$(TIME$, 4, 2)
    DO
    IF INKEY$ > "" THEN END
    LOOP WHILE m$ = MID$(TIME$, 4, 2)
    LOOP
END SUB

SUB Mask
    DIM h%, s%, v%, x%, y%, z%
    s% = 12
    CLS
    COLOR 15, 1
    FOR x% = 1 TO 21
    SELECT CASE x%
        CASE 1
            PRINT "É"; STRING$(s% + 1, 205);
            FOR y% = 1 TO 4
            PRINT "Ñ"; STRING$(s%, 205);
            NEXT
            PRINT "Ñ"; STRING$(s%, 205); "»"
        CASE 6, 11, 16
            PRINT "Ç"; STRING$(s% + 1, 196);
            FOR y% = 1 TO 4
            PRINT "Å"; STRING$(s%, 196);
            NEXT
            PRINT "Å"; STRING$(s%, 196); "¶"
        CASE 21
            PRINT "È"; STRING$(s% + 1, 205);
            FOR y% = 1 TO 4
            PRINT "Ï"; STRING$(s%, 205);
            NEXT
            PRINT "Ï"; STRING$(s%, 205); "¼"
        CASE ELSE
            PRINT "º"; STRING$(s% + 1, 32);
            FOR y% = 1 TO 4
            PRINT "³"; STRING$(s%, 32);
            NEXT
            PRINT "³"; STRING$(s%, 32); "º"
    END SELECT
    NEXT
    FOR v% = 1 TO 4
    FOR h% = 1 TO 6
    z% = z% + 1
    FOR x% = 0 TO 2
    LOCATE v% * 5 - 5 + 3 + x%, h% * 13 - 13 + 3
    PRINT Zone(x%, z%)
    NEXT
    NEXT
    NEXT
END SUB

