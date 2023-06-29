DECLARE SUB Display (m%)
DECLARE SUB Process ()
DECLARE SUB Initialize ()
DECLARE SUB Main ()
DECLARE SUB Getkey (a%, k$)
    CONST MXG = 8, MXH = 640, MXL = 70, MXV = 480
    CONST C1 = 1, C2 = 2
    COMMON SHARED Flt%
    COMMON SHARED PHigh%
    COMMON SHARED PSide%
    COMMON SHARED PTop%
    COMMON SHARED PWide%
    DIM SHARED Tag(MXG) AS STRING, Opt(MXG) AS SINGLE, Lim(MXG) AS SINGLE
    Initialize
    Main
END
Trap: Flt% = ERR: RESUME NEXT
DATA "FIC1gpm",0,15
DATA "FIC2gpm",0,15
DATA "LIC1gal",0,100
DATA "LIC2gal",0,100
DATA "PIC1lbs",0,30
DATA "PIC2lbs",0,30
DATA "TIC1ø",0,212
DATA "TIC2ø",0,212

SUB Display (m%)
    DIM c%, x%, y%
    DIM bl$, cx%
    DIM gl%, gr%, gt%, gb%
    DIM cl%, cr%, ct%, cb%
    FOR x% = 1 TO MXG
    bl$ = STRING$(6, 32)
    gl% = PSide% + (PWide% / MXG) * x% - (PWide% / MXG) + (PWide% / MXG) * .1
    gr% = gl% + (PWide% / MXG) - (PWide% / MXG) * .2
    gt% = PTop% + (PHigh% * .1)
    gb% = gt% + PHigh% * .725
    cl% = gl% + ((gr% - gl%) * .25)
    cr% = cl% + ((gr% - gl%) * .5)
    ct% = gt% + ((gb% - gt%) * .05)
    cb% = ct% + ((gb% - gt%) * .9)
    SELECT CASE m%
        CASE IS < 0
            LINE (gl%, gt%)-(gr%, gb%), 7, BF
            FOR y% = 0 TO 100 STEP 5
            SELECT CASE y%
                CASE 0, 100: c% = 12
                CASE 25, 75: c% = 1
                CASE 50: c% = 15
                CASE ELSE: c% = 10
            END SELECT
            cx% = ct% + (cb% - ct%) / 100 * y%
            LINE (gl%, cx%)-(gr%, cx%), c%
            NEXT
            LINE (cl%, ct%)-(cr%, cb%), 14, BF
        CASE ELSE
            SELECT CASE Opt(x%)
                CASE 0 TO 4, 96 TO 100: c% = 12
                CASE ELSE: c% = 10
            END SELECT
            cx% = cb% - (cb% - ct%) / 100 * Opt(x%)
            LOCATE 5, MXL / MXG * x%: COLOR c%: PRINT LEFT$(LEFT$(Tag$(x%), 4) + bl$, LEN(bl$))
            LINE (cl%, ct%)-(cr%, cx%), 14, BF
            LINE (cl%, cx%)-(cr%, cb%), c%, BF
            COLOR c%
            LOCATE 24, MXL / MXG * x% - 1: PRINT USING ("###.#"); Opt(x%); : PRINT "%"
            LOCATE 25, MXL / MXG * x% - 1: PRINT USING ("###.##"); Opt(x%) * Lim(x%) / 100
            LOCATE 26, MXL / MXG * x% - 1: PRINT RIGHT$(bl$ + MID$(Tag$(x%), 5), LEN(bl$))
    END SELECT
    NEXT
END SUB

SUB Getkey (a%, k$)
    DIM x%
    DO: LOOP UNTIL INKEY$ = ""
    DO
    Process
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN END
END SUB

SUB Initialize
    DIM x%
    PHigh% = 370
    PTop% = (MXV - PHigh%) / 2
    PWide% = 560
    PSide% = (MXH - PWide%) / 2
    FOR x% = 1 TO MXG
    READ Tag$(x%), Opt(x%), Lim(x%)
    NEXT
END SUB

SUB Main
    SCREEN 12
    DIM a%, k$
    LINE (0, 0)-(MXH, MXV), 9, BF
    LINE (PSide%, PTop%)-(PSide% + PWide%, PTop% + PHigh%), 0, BF
    Display -1
    DO
    Getkey a%, k$
    SELECT CASE k$
        CASE CHR$(0) + "P": Opt(C1) = Opt(C1) - .1
        CASE CHR$(0) + "H": Opt(C1) = Opt(C1) + .1
        CASE CHR$(0) + "K": Opt(C2) = Opt(C2) - .1
        CASE CHR$(0) + "M": Opt(C2) = Opt(C2) + .1
    END SELECT
    LOOP
END SUB

SUB Process
    DIM a%, b%, c%, d%, e%, f%, g%, h%, x%
    FOR x% = 1 TO MXG
    IF Opt(x%) < 0 THEN Opt(x%) = 0
    IF Opt(x%) > 100 THEN Opt(x%) = 100
    NEXT
    Display 1
END SUB

