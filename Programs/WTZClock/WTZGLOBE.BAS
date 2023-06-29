DECLARE SUB SayZone (sz$)
DECLARE SUB AmToPm (Tm%)
DECLARE SUB PhaseAmPm (Tm%)
DECLARE SUB SetTime (Sh%, Sp%)
DECLARE SUB Scroll (Dy%, Hr%, Pc%)
DECLARE SUB Getkey (Ak%, Ck$, Pl%)
DECLARE SUB Main ()
DECLARE SUB Mask ()

    COMMON SHARED CPhase AS INTEGER
   
    DIM SHARED Day(7) AS STRING * 3
    DIM SHARED TZ(24, 2) AS STRING
   
    CONST BGC = 1       'Background color.
    CONST FGC = 15      'Text color
    CONST EST = 7       'Set for Eastern Standard Time Zone - (Slot #7).
    CONST LDG = 16      'Column number of the left edge of the Graph.
                        '(Adjustable for centerization)
   
    FOR x% = 1 TO 7
    READ Day(x%)
    DATA Sun,Mon,Tue,Wed,Thu,Fri,Sat
    NEXT

    FOR x% = 1 TO 24
    READ TZ(x%, 1), TZ(x%, 2)
    TZ(x%, 1) = LEFT$(TZ(x%, 1) + "---", 3)
    DATA MPO,Eastern Siberia - Mid Pacific Ocean
    DATA MID,Midway Island
    DATA AST,Alaska - (Hawaii 8 1/2 Hrs)
    DATA PST,Pacific Time (DST from Last Sunday in March to Last Sunday in October)
    DATA MST,Mountain Time (DST from Last Sunday in March to Last Sunday in October)
    DATA CST,Central Time (DST from Last Sunday in March to Last Sunday in October)
    DATA EST,Eastern Time (DST from Last Sunday in March to Last Sunday in October)
    DATA NEF,Newfoundland
    DATA GRL,Greenland - Rio De Janeiro & Sao Paulo S.A.
    DATA MAO,Mid Atlantic Ocean
    DATA EAO,Eastern Atlantic Ocean
    DATA GMT,British Isles (England - Ireland - Scotland) - Iceland
    DATA PAR,Paris - Berlin
    DATA CEI,Cairo Egypt - Israel
    DATA MIA,Moscow - Iraq - Arabia - Tehran Iran (3 1/2 Hrs)
    DATA MIO,Mid Indian Ocean
    DATA IND,(Bombay & New Delhi India 5 1/2 Hrs)
    DATA EIO,Eastern Indian Ocean
    DATA JAK,Jakarta - Bankok
    DATA MAN,Manila - Shanghai - Taipei - Beijing - Western Australia
    DATA TOK,Tokyo - Osaka - (Mid Australia 9 1/2 Hr)
    DATA SYD,Sydney - Eastern Australia
    DATA EUS,Eastern USSR
    DATA IDL,New Zealand - Fiji Islands - Korea
    NEXT

    'IN REFERENCE TO THE TZ variable #1 - Zone Indications.
    'The Zone Letters were assigned by the programmer at application's
    'creation and are re-assignable by the user. So, although the letters
    'of each zone are capable of being changed by the user, we must make
    'sure that the (IDL) International Date Line at each end of the graph
    'and the (GMT) Greenwich Mean Time in the center of the graph are still
    'assigned as such indicators.
    TZ(0, 1) = "IDL"
    TZ(12, 1) = "GMT"
    TZ(24, 1) = "IDL"

    'IN REFERENCE TO THE TZ variable #2 - Description of zone.
    TZ(0, 2) = TZ(24, 2)
    'This is NOT re-assigning by the application. "TZ(0,2)", First Time Zone
    'of the graph, is also the IDL, the same as the last time zone of the
    'graph "TZ(24,2)".
   
    Main
END

SUB AmToPm (Tm%)
    SELECT CASE Tm%
        CASE 0: COLOR 14, 0             'Midnite
        CASE 12: COLOR 14, 11           'Noon
        CASE 1 TO 11: COLOR 0, 15       '1am to 11am
        CASE 13 TO 23: COLOR 15, 6      '1pm to 11pm
    END SELECT
END SUB

SUB Getkey (Ak%, Ck$, Pl%)
    DO: LOOP UNTIL INKEY$ = ""
    DO
    LOCATE 2, 39: PRINT MID$(TIME$, 4, 5)
    Ck$ = INKEY$
    IF Auto = 1 THEN
        IF Lst% <> VAL(TIME$) THEN EXIT SUB
    END IF
    LOOP WHILE Ck$ = ""
    Ak% = ASC(Ck$)
    IF Ak% = 27 THEN END
END SUB

SUB Main
    Mask

    CPhase = 1

    Da% = 1
    
    SetTime Ho%, Pl%

    DO

    IF Ho% < 0 THEN Ho% = 23: Da% = Da% - 1
    IF Da% < 1 THEN Da% = 7
    IF Ho% > 23 THEN Ho% = 0: Da% = Da% + 1
    IF Da% > 7 THEN Da% = 1
    
    IF Pl% < 1 THEN Pl% = 24
    IF Pl% > 24 THEN Pl% = 1
   
    Scroll Da%, Ho%, Pl%

    COLOR 10, BGC
    SayZone TZ(Pl%, 2)
    COLOR FGC, BGC
   
    LOCATE 3, Pl% * 2 + LDG + 1: PRINT CHR$(25)
    LOCATE 11, Pl% * 2 + LDG + 1: PRINT CHR$(18)
    LOCATE 15, Pl% * 2 + LDG + 1: PRINT CHR$(24)
   
    IF Pl% = 24 THEN
        LOCATE 3, LDG + 1: PRINT CHR$(25)
        LOCATE 11, LDG + 1: PRINT CHR$(18)
        LOCATE 15, LDG + 1: PRINT CHR$(24)
    END IF

    Getkey a%, k$, Pl%

    LOCATE 3, Pl% * 2 + LDG + 1: PRINT "Ä"
    LOCATE 11, Pl% * 2 + LDG + 1: PRINT "Ä"
    LOCATE 15, Pl% * 2 + LDG + 1: PRINT "Ä"

    IF Pl% = 24 THEN
        LOCATE 3, LDG + 1: PRINT "Ä"
        LOCATE 11, LDG + 1: PRINT "Ä"
        LOCATE 15, LDG + 1: PRINT "Ä"
    END IF
   
    SELECT CASE k$
        CASE CHR$(0) + "K": Ho% = Ho% - 1
        CASE CHR$(0) + "M": Ho% = Ho% + 1
        CASE CHR$(0) + "P": Pl% = Pl% - 1
        CASE CHR$(0) + "H": Pl% = Pl% + 1
        CASE CHR$(0) + "I": Da% = Da% + 1
        CASE CHR$(0) + "Q": Da% = Da% - 1
        CASE CHR$(0) + "G": SetTime Ho%, Pl%
        CASE "P", "p": CPhase = CPhase + 1: IF CPhase > 1 THEN CPhase = 0
    END SELECT

    LOOP

END SUB

SUB Mask
    CLS
    COLOR FGC, BGC

    'Draw basic mask
    PRINT "É"; STRING$(78, 205); "»"
    PRINT "º"; STRING$(78, 32); "º"
    PRINT "Ç"; STRING$(78, 196); "¶"
    FOR x% = 4 TO 10
    PRINT "º"; STRING$(78, 32); "º"
    NEXT
    PRINT "Ç"; STRING$(78, 196); "¶"
    FOR x% = 12 TO 14
    PRINT "º"; STRING$(78, 32); "º"
    NEXT
    PRINT "Ç"; STRING$(78, 196); "¶"
    PRINT "º"; STRING$(78, 32); "º"
    PRINT "Ç"; STRING$(78, 196); "¶"
    FOR x% = 18 TO 22
    PRINT "º"; STRING$(78, 32); "º"
    NEXT
    PRINT "È"; STRING$(78, 205); "¼"
    
    LOCATE 2, 3: PRINT "World Time"
    LOCATE 2, 66: PRINT "Zone Displays"
   
    'This section draws the Time and Zone display grid.
    FOR h% = 0 TO 25
    LOCATE 3, h% * 2 - 2 + LDG + 2
    PRINT "Â"
    FOR v% = 4 TO 14
    LOCATE v%, h% * 2 - 2 + LDG + 2
    IF v% = 11 THEN PRINT CHR$(197) ELSE PRINT CHR$(179)
    NEXT
    LOCATE 15, h% * 2 - 2 + LDG + 2
    PRINT "Á"
    NEXT

    LOCATE 5, 3: PRINT "Western"
    LOCATE 6, 3: PRINT "Hemisphere"
    LOCATE 7, 3: PRINT "GMT"
    LOCATE 8, 3: PRINT "-12 hours"


    LOCATE 5, 72: PRINT "Eastern"
    LOCATE 6, 69: PRINT "Hemisphere"
    LOCATE 7, 76: PRINT "GMT"
    LOCATE 8, 70: PRINT "+12 hours"

    LOCATE 12, 3: PRINT "Time"
    LOCATE 13, 3: PRINT "Zone"
    LOCATE 14, 3: PRINT "Symbols"
   
    LOCATE 12, 75: PRINT "Time"
    LOCATE 13, 75: PRINT "Zone"
    LOCATE 14, 72: PRINT "Symbols"

    'Place zone symbols into display.
    FOR y% = 0 TO 24
    COLOR 10, BGC
    IF TZ(y%, 1) = "IDL" OR TZ(y%, 1) = "GMT" THEN COLOR 14, BGC
    FOR x% = 1 TO 3
    LOCATE 11 + x%, y% * 2 + LDG + 1
    PRINT MID$(TZ(y%, 1), x%, 1)
    NEXT
    NEXT

    COLOR FGC, BGC

    'Place instructions into instruction area.
    i$ = "< - Scroll Hours + >"
    LOCATE 18, 40 - (LEN(i$) / 2)
    PRINT i$

    i$ = "PAGE UP to Increase Day,  PAGE DOWN to Decrease Day"
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$
   
    i$ = "UP ARROW (" + CHR$(24) + ") Moves Selected-Zone-Indicator Right"
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$

    i$ = "DOWN ARROW (" + CHR$(25) + ") Moves Selected-Zone-Indicator Left"
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$

    i$ = "Press HOME to Set " + TZ(EST, 1) + " Time, ESC to End"
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$

END SUB

SUB PhaseAmPm (Tm%)
    SELECT CASE Tm%
        CASE 0: COLOR 14, 0                 'Midnite
        CASE 12: COLOR 14, 11               'Noon
        CASE 18 TO 23, 1 TO 5: COLOR 15, 6  '6pm to 11pm, 1am to 5am
        CASE 6 TO 11, 13 TO 17: COLOR 0, 15 '6am to 11am, 1pm to 5pm
    END SELECT
END SUB

SUB SayZone (sz$)
    l$ = LEFT$(sz$, LEN(sz$) / 2)
    r$ = MID$(sz$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(38, 32) + l$, 38)
    r$ = LEFT$(r$ + STRING$(38, 32), 38)
    COLOR 10, BGC
    LOCATE 16, 3
    PRINT l$; r$
END SUB

SUB Scroll (Sd%, Sh%, Sp%)
    Dy% = Sd%
    Hr% = Sh%

    FOR t% = 1 TO 24

    i$ = Day(Dy%)

    SELECT CASE Hr%
        CASE 0
            Hour$ = RIGHT$("00" + MID$(STR$(Hr%), 2), 2)
            Md$ = "mn"
        CASE 1 TO 11
            Hour$ = RIGHT$("00" + MID$(STR$(Hr%), 2), 2)
            Md$ = "am"
        CASE IS = 12
            Hour$ = RIGHT$("00" + MID$(STR$(Hr%), 2), 2)
            Md$ = "no"
        CASE 13 TO 23
            Hour$ = RIGHT$("00" + MID$(STR$(Hr% - 12), 2), 2)
            Md$ = "pm"
        CASE 24
            Hour$ = RIGHT$("00" + MID$(STR$(Hr% - 12), 2), 2)
            Md$ = "mn"
    END SELECT

    i$ = i$ + Hour$ + Md$

    COLOR FGC, BGC

    IF t% = Sp% THEN
        LOCATE 2, 32: PRINT LEFT$(i$, 3)
        LOCATE 2, 36: PRINT MID$(i$, 4, 2); ":"
        LOCATE 2, 44: PRINT Md$; " "; TZ(Sp%, 1)
    END IF

    SELECT CASE CPhase
        CASE 0: AmToPm Hr%          'Sets color phases to Am and Pm.
        CASE 1: PhaseAmPm Hr%       'Sets color phases to sun.
    END SELECT

    FOR y% = 1 TO 11
    LOCATE y% + 3, t% * 2 + LDG + 1
    PRINT MID$(i$, y%, 1)

    IF t% = 24 THEN
        LOCATE y% + 3, LDG + 1
        PRINT MID$(i$, y%, 1)
    END IF
    NEXT

    Hr% = Hr% + 1
        IF Hr% > 23 THEN
        Hr% = 0
        Dy% = Dy% + 1
        IF Dy% > 7 THEN Dy% = 1
    END IF
   
    NEXT
END SUB

SUB SetTime (Sh%, Sp%)
    Sp% = EST
    Sh% = VAL(TIME$) - EST + 1
    IF Sh% < 0 THEN Sh% = Sh% + 23
END SUB

