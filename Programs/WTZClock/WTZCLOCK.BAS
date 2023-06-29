DECLARE SUB DetermineDay (w%)
DECLARE SUB Cuckoo ()
DECLARE SUB SayIt (Si$)
DECLARE SUB ShowZone (Zp%, Mo%)
DECLARE SUB BigBen (QtrHr%, Hr%)
DECLARE SUB SunPhase (Tm%)
DECLARE SUB AmPm (Tm%)
DECLARE SUB SetTime (Sh%)
DECLARE SUB Scroll (Sd%, Sh%, Pc%)
DECLARE SUB Getkey (Ak%, Ck$, Kd%, Kh%, Kp%)
DECLARE SUB Main ()
DECLARE SUB Mask ()
    COMMON SHARED CShift AS INTEGER
    COMMON SHARED LDay AS INTEGER
    COMMON SHARED LHour AS INTEGER
   
    DIM SHARED Day(7) AS STRING * 3
    DIM SHARED Month(12) AS INTEGER
    DIM SHARED TZ(24, 2) AS STRING
   
    CONST Bgc = 1       'Background color.
    CONST Fgc = 15      'Text color.
    CONST HZN = 7       'Home Zone - User set for Eastern Standard Time.
    CONST LDG = 16      'Column number of the left edge of the Graph.
                        '(Adjustable for centerization).
   
    FOR x% = 1 TO 7
    READ Day(x%)
    DATA Sun,Mon,Tue,Wed,Thu,Fri,Sat
    NEXT

    FOR x% = 1 TO 12
    READ Month(x%)
    DATA 31,28,31,30,31,30,31,31,30,31,30,31
    NEXT

    'The user can reassign the tags and labels of the next routine.
    FOR x% = 0 TO 24
    READ TZ(x%, 1), TZ(x%, 2)
    TZ(x%, 1) = LEFT$(TZ(x%, 1) + "---", 3)
    DATA IDL,Wellington New ZealandùFiji IslandsùWake Island
    DATA MPO,Eastern SiberiaùMid Pacific Ocean
    DATA MID,Midway Island
    DATA AST,Alaskaù(Hawaii -1/2 Hr)
    DATA PST,Los AngelesùSan Francisco
    DATA MST,Las VegasùDenver
    DATA CST,ChicagoùSt Louis
    DATA EST,BostonùPhiladelphiaùWashingtonùMiamiùTampaùCubaùPanamaùChile
    DATA NEF,NewfoundlandùVenezuela
    DATA GRL,GreenlandùRio De JaneiroùSao Paulo
    DATA MAO,Mid Atlantic Ocean
    DATA EAO,Eastern Atlantic Ocean
    DATA GMT,IcelandùBritish Isles (England - Ireland - Scotland)ùWest Africa
    DATA PAR,ParisùBerlin
    DATA CEI,Cairo EgyptùIsrael
    DATA MIA,MoscowùIraqùArabiaùTehran Iran (+1/2 Hr)
    DATA MIO,Mid Indian Ocean
    DATA IND,(Bombay & New Delhi India +1/2 Hr)
    DATA EIO,Eastern Indian Ocean
    DATA JAK,JakartaùBankok
    DATA HKG,Hong KongùWestern Australia
    DATA TOK,TokyoùOsakaù(Mid Australia +1/2 Hr)
    DATA SYD,SydneyùEastern Australia
    DATA EUS,Eastern USSR
    DATA IDL,Wellington New ZealandùFiji IslandsùWake Island
    NEXT

    'All of the Time Zone Tags are previously set via the above "READ"
    'routine. However all of this data can be changed by any user.
    'So, to insure the proper graph display of the Greenwich Mean Time
    'and International Date Line Zones (0, 12 and 24), the following
    'statement lines place the programmer's intended data
    'into their respective variables.
    TZ(0, 1) = "IDL"
    TZ(12, 1) = "GMT"
    TZ(24, 1) = "IDL"
    TZ(0, 2) = "International Date Line"
    TZ(12, 2) = "Greenwich Mean Time"
    TZ(24, 2) = "International Date Line"

    Main
END

SUB AmPm (Tm%)
    SELECT CASE Tm%
        CASE 0: COLOR 15, 0                     'Midnite
        CASE 12: COLOR 14, 11                   'Noon
        CASE 1 TO 11: COLOR 0, 15               '1am to 11am
        CASE 13 TO 23: COLOR 15, 6              '1pm to 11pm
    END SELECT
END SUB

SUB BigBen (QtrHr%, Hr%)
    IF Hr% > 24 THEN Hr% = Hr% - 24
    IF Hr% > 12 THEN Hr% = Hr% - 12
    SELECT CASE QtrHr%
        CASE IS = 0: Tune$ = "ECD<G P3 G>DEC P3 CED<G P3 G>DEC"
        CASE IS = 1500: Tune$ = "ECD<G"
        CASE IS = 3000: Tune$ = "ECD<G P3 G>DEC"
        CASE IS = 4500: Tune$ = "ECD<G P3 G>DEC P3 CED<G"
    END SELECT
    PLAY "L2O3 X" + VARPTR$(Tune$)
    IF QtrHr% = 0 THEN
        FOR x% = 1 TO Hr%
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        PLAY "O2CP8"
        NEXT
    END IF
END SUB

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1200, 4
    SOUND 0, 2
END SUB

SUB DetermineDay (w%)
    DIM da%, ma%, ya%, db%, mb%, yb%
    w% = 7
    ma% = VAL(DATE$)
    da% = VAL(MID$(DATE$, 4))
    ya% = VAL(MID$(DATE$, 7))
    mb% = 1
    db% = 1
    yb% = 2011
    DO
    db% = db% + 1
    IF db% > Month%(mb%) THEN db% = 1: mb% = mb% + 1
    IF mb% > 12 THEN mb% = 1: yb% = yb% + 1
    w% = w% + 1
    IF w% > 7 THEN w% = 1
    IF ma% = mb% AND da% = db% AND ya% = yb% THEN EXIT DO
    LOOP
END SUB

SUB Getkey (Ak%, Ck$, Kd%, Kh%, Kp%)
    DO: LOOP UNTIL INKEY$ = ""
    DO

    COLOR 11, Bgc
    LOCATE 2, 39: PRINT ; MID$(TIME$, 4, 5)
    COLOR Fgc, Bgc

    IF LHour <> VAL(TIME$) THEN
        SetTime Kh%
        Scroll Kd%, Kh%, Kp%
    END IF

    ms$ = MID$(TIME$, 4, 2) + RIGHT$(TIME$, 2)
    Hr% = LHour + Kp% - HZN

    SELECT CASE ms$
        CASE "0000", "1500", "3000", "4500"
            Scroll Kd%, Kh%, Kp%
            SayIt TZ(Kp%, 2)
            BigBen VAL(ms$), Hr%
    END SELECT

    Ck$ = INKEY$
    LOOP WHILE Ck$ = ""
    Ak% = ASC(Ck$)
    IF Ak% = 27 THEN END
END SUB

SUB Main
    Mask

    CShift = 1
   
    DetermineDay da%

    Pl% = HZN

    SetTime Ho%
   
    DO

    IF da% < 1 THEN da% = 7
    IF da% > 7 THEN da% = 1
    
    IF Pl% < 1 THEN Pl% = 24
    IF Pl% > 24 THEN Pl% = 1
    
    Scroll da%, Ho%, Pl%

    SayIt TZ(Pl%, 2)

    COLOR Fgc, Bgc

    ShowZone Pl%, 1

    Getkey a%, k$, da%, Ho%, Pl%

    ShowZone Pl%, 2
   
    SELECT CASE UCASE$(k$)
        CASE CHR$(0) + "K", CHR$(0) + "P": Pl% = Pl% - 1
        CASE CHR$(0) + "M", CHR$(0) + "H": Pl% = Pl% + 1
        CASE CHR$(0) + "Q": da% = da% - 1
        CASE CHR$(0) + "I", CHR$(32): da% = da% + 1
        CASE CHR$(0) + "G": Pl% = HZN
        CASE IS = "P": CShift = CShift + 1: IF CShift > 1 THEN CShift = 0
        CASE IS = "B", "C": BigBen 0, 1
    END SELECT

    LOOP

END SUB

SUB Mask
    CLS
    COLOR Fgc, Bgc

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
    LOCATE 2, 69: PRINT "Zone Clock"
   
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

    LOCATE 6, 3: PRINT "Western"
    LOCATE 7, 3: PRINT "Hemisphere"
    LOCATE 8, 3: PRINT "GMT-12hrs"
   
    LOCATE 6, 72: PRINT "Eastern"
    LOCATE 7, 69: PRINT "Hemisphere"
    LOCATE 8, 70: PRINT "GMT+12hrs"
   
    LOCATE 12, 3: PRINT "World"
    LOCATE 13, 3: PRINT "Time"
    LOCATE 14, 3: PRINT "Zones"
   
    LOCATE 12, 74: PRINT "World"
    LOCATE 13, 75: PRINT "Time"
    LOCATE 14, 74: PRINT "Zones"

    'Place zone symbols into display.
    FOR y% = 0 TO 24
    COLOR 10, Bgc
    IF TZ(y%, 1) = "IDL" OR TZ(y%, 1) = "GMT" THEN COLOR 14, Bgc
    FOR x% = 1 TO 3
    LOCATE 11 + x%, y% * 2 + LDG + 1
    PRINT MID$(TZ(y%, 1), x%, 1)
    NEXT
    NEXT

    COLOR Fgc, Bgc

    'Place instructions into instruction area.
    i$ = "PAGE UP a Day or PAGE DOWN a Day to"
    LOCATE 18, 40 - (LEN(i$) / 2)
    PRINT i$

    i$ = "set the World Clock day to correspond to your actual day."
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$
   
    i$ = "<- Move West - Zone Indicator Arrow - Move East ->"
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$
   
    i$ = "Press HOME to place Zone Indicator Arrow on " + TZ(HZN, 1) + " Time Zone."
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$

    i$ = "Press ESC to End."
    LOCATE , 40 - (LEN(i$) / 2)
    PRINT i$

    'Normally, the next routine would not be needed because the Zone Codes
    'are preset by the programmer. But there are times when one might utilize
    'other symbols. Therefore this possibility makes this routine necessary
    'within the procedure.

    'If user reassigns Zone Codes then this routine checks
    'for duplicate names for user correction.

    FOR x% = 1 TO 24
    FOR y% = x% TO 24
    SELECT CASE y%
        CASE IS > x%
            IF TZ(x%, 1) = TZ(y%, 1) THEN
                BEEP
                i$ = "Duplicate Zone Symbols at #" + STR$(x%) + " and #" + STR$(y%) + ", application terminated!"
                LOCATE 16, 40 - (LEN(i$) / 2)
                COLOR 14, Bgc
                PRINT i$
                END
            END IF
    END SELECT
    NEXT
    NEXT

END SUB

SUB SayIt (Si$)
    l$ = LEFT$(Si$, LEN(Si$) / 2)
    r$ = MID$(Si$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(39, 32) + l$, 39)
    r$ = LEFT$(r$ + STRING$(39, 32), 39)
    COLOR 10, Bgc
    LOCATE 16, 2
    PRINT l$; r$
    COLOR Fgc, Bgc
END SUB

SUB Scroll (Sd%, Sh%, Pc%)
    Dy% = Sd%
    Hr% = Sh%

    FOR t% = 1 TO 24

    i$ = Day(Dy%)

    SELECT CASE Hr%
        CASE 0, 24
            Hour$ = "12"
            Md$ = "PM"
        CASE 1 TO 11
            Hour$ = RIGHT$("00" + MID$(STR$(Hr%), 2), 2)
            Md$ = "am"
        CASE IS = 12
            Hour$ = "12"
            Md$ = "AM"
        CASE 13 TO 23
            Hour$ = RIGHT$("00" + MID$(STR$(Hr% - 12), 2), 2)
            Md$ = "pm"
    END SELECT

    COLOR Fgc, Bgc

    i$ = i$ + Hour$ + Md$

    IF t% = Pc% THEN
        COLOR 11, Bgc
        LOCATE 2, 32
        PRINT LEFT$(i$, 3)
        LOCATE 2, 36
        PRINT MID$(i$, 4, 2); ":"
        LOCATE 2, 44
        PRINT RIGHT$(i$, 2); " "; TZ(Pc%, 1)
        COLOR Fgc, Bgc
    END IF

    SELECT CASE CShift
        CASE IS = 0: AmPm Hr%'Sets display colors-Am/Pm.
        CASE IS = 1: SunPhase Hr%'Sets display colors-6a/6p.
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

    COLOR Fgc, Bgc

END SUB

SUB SetTime (Sh%)
    Sh% = VAL(TIME$) - HZN + 1
    IF Sh% < 0 THEN Sh% = Sh% + 12
    LHour = VAL(TIME$)
END SUB

SUB ShowZone (Zp%, Mo%)
    h% = Zp% * 2 + LDG + 1
SELECT CASE Mo%
    CASE IS = 1
        c1% = 25
        c2% = 18
        c3% = 24
    CASE IS = 2
        c1% = 196
        c2% = 196
        c3% = 196
END SELECT
    LOCATE 3, h%: PRINT CHR$(c1%)
    LOCATE 11, h%: PRINT CHR$(c2%)
    LOCATE 15, h%: PRINT CHR$(c3%)
    IF Zp% = 24 THEN
        h% = LDG + 1
        LOCATE 3, h%: PRINT CHR$(c1%)
        LOCATE 11, h%: PRINT CHR$(c2%)
        LOCATE 15, h%: PRINT CHR$(c3%)
    END IF
END SUB

SUB SunPhase (Tm%)
    SELECT CASE Tm%
        CASE 0: COLOR 15, 0                     'Midnight
        CASE 12: COLOR 14, 11                   'Noon
        CASE 18 TO 23, 1 TO 5: COLOR 15, 6      '6pm to 11pm, 1am to 5am
        CASE 6 TO 11, 13 TO 17: COLOR 0, 15     '6am to 11am, 1pm to 5pm
    END SELECT
END SUB

