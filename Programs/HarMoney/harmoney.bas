DECLARE SUB Sequent (s$, d$, t$, a$)
DECLARE SUB FormatDate (i$)
DECLARE SUB Purge ()
DECLARE SUB LoadBank ()
DECLARE SUB FormatMBXTxt (i$)
DECLARE SUB MsgBox ()
DECLARE SUB Swipe (f%, b%)
DECLARE SUB Delete (a%)
DECLARE SUB Decant (i$, d$, t$, a$)
DECLARE SUB Display (r%, k$)
DECLARE SUB Scroll (s%, e%, p%, r%)
DECLARE SUB Mask (r%)
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB Switch (t%)
TYPE dahsr
    a AS STRING * 10
    b AS STRING * 54
    c AS STRING * 10
END TYPE
    CONST BOOT = "harmoney.bas"
    CONST DLR = "$**####.##"
    CONST ECHO = 20
    CONST INI = "harmoney.ini"
    CONST MXL = 36
    CONST PRGM = "harmoney"
    CONST ROWS = 13
    COMMON SHARED Bgc%
    COMMON SHARED Bkc$
    COMMON SHARED Bkd$
    COMMON SHARED Bkl$
    COMMON SHARED Cred$
    COMMON SHARED Debt$
    COMMON SHARED Edit$
    COMMON SHARED Ext$
    COMMON SHARED Fgc%
    COMMON SHARED Flt%
    COMMON SHARED Fxt$
    COMMON SHARED MyFile$
    COMMON SHARED q AS STRING * 1
    COMMON SHARED Src$
    COMMON SHARED Title$
    COMMON SHARED Today$
    DIM SHARED Account AS dahsr, Menu(2, 26) AS STRING
    ON ERROR GOTO Trap
    Initialize
    Purge
    LoadBank
    Main
END
Trap: Flt% = ERR: RESUME NEXT
MsgMask0:
DATA 20,-1,-1
DATA "Scroll [LUDR] Files ù ENTER Display"
DATA "TAB: Edit File ù INSERT: Swap Two Files ù DELETE Account"
DATA "BACKSPACE Exits [B] ù ESC to End"
DATA "*"
MsgMask1:
DATA 20,-1,-1
DATA "Scroll [UD] the Lines ù Scroll [LR] the Files"
DATA "Press TAB to Edit the File ù DELETE the Account"
DATA "ENTER Main Menu ù ESC to End"
DATA "*"
MsgSwitch:
DATA 20,14,-1
DATA "Scroll [LUDR] or Press a Character (''A'' to ''Z'') to Move Cursor."
DATA "Then Press INSERT to Switch the Indicated ''[R]'' Selected Files"
DATA "To Cancel, Press ENTER to Exit or ESC to End"
DATA "*"
MsgDelete:
DATA 21,14,4
DATA "To Cancel the Function"
DATA "Press ENTER to Exit or ESC to End"
DATA "*"

SUB Cuckoo
    SOUND 1400, 3
    SOUND 0, 2
    SOUND 1155, 4
    SOUND 0, 2
END SUB

SUB Decant (i$, d$, t$, a$)
    DIM s$
    d$ = "": t$ = "": a$ = ""
    CLOSE #9: OPEN "decant.tmp" FOR OUTPUT AS #9
    PRINT #9, i$
    CLOSE #9: OPEN "decant.tmp" FOR INPUT AS #9
    INPUT #9, d$, t$, a$
    CLOSE #9
    KILL "decant.tmp"
    d$ = LTRIM$(RTRIM$(d$))
    FormatDate d$
    t$ = LTRIM$(RTRIM$(t$))
    a$ = LTRIM$(RTRIM$(a$))
    Sequent s$, d$, t$, a$
END SUB

SUB Delete (a%)
    DIM k$
    RESTORE MsgDelete
    MsgBox
    SayIt "DELETE File " + q + CHR$(a%) + Fxt$ + q + " ?", ECHO, 14, 4
    DO
    Getkey k$
    SELECT CASE k$
        CASE CHR$(0) + "S"
            KILL "dbse\" + CHR$(a%) + Fxt$
            RUN BOOT
    END SELECT
    LOOP UNTIL k$ = CHR$(13)
END SUB

SUB Display (r%, k$)
    DIM e%, s%
    STATIC p%
    IF p% = 0 THEN p% = 1
    s% = 1
    Mask r%
    IF r% = 0 THEN
        e% = 26
    ELSE
        CLOSE : OPEN "abse\" + CHR$(r% + 64) + Fxt$ FOR RANDOM AS #1 LEN = LEN(Account) + 2
        e% = LOF(1) / (LEN(Account) + 2)
    END IF
    DO
    IF r% = 0 THEN
        IF p% < s% THEN p% = e%
        IF p% > e% THEN p% = s%
    ELSE
        IF s% < 1 THEN s% = 1
        IF s% > e% THEN s% = e%
    END IF
    Scroll s%, e%, p%, r%
    DO
    Getkey k$
    IF r% = 0 THEN
        SELECT CASE UCASE$(k$)
            CASE CHR$(8)
                CHDIR LEFT$(MyFile$, LEN(MyFile$) - 12)
                RUN MyFile$
                CHDIR LEFT$(Src$, LEN(Src$) - 1)
                RUN BOOT
            CASE CHR$(9): SHELL Edit$ + "dbse\" + CHR$(p% + 64): RUN BOOT
            CASE CHR$(13): r% = p%: EXIT SUB
            CASE CHR$(46): SHELL Edit$ + INI: RUN BOOT
            CASE CHR$(0) + "G": p% = 1: EXIT DO
            CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
            CASE CHR$(0) + "I": r% = r% + 1: EXIT SUB
            CASE CHR$(0) + "K": p% = p% - 13: EXIT DO
            CASE CHR$(0) + "M": p% = p% + 13: EXIT DO
            CASE CHR$(0) + "O": p% = e%: EXIT DO
            CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
            CASE CHR$(0) + "Q": r% = r% - 1: EXIT SUB
            CASE CHR$(0) + "R": Switch p%: EXIT SUB
            CASE CHR$(0) + "S": Delete p% + 64: EXIT SUB
            CASE "A" TO "Z": p% = ASC(UCASE$(k$)) - 64: EXIT DO
        END SELECT
    ELSE
        SELECT CASE UCASE$(k$)
            CASE CHR$(8): r% = r% - 1: EXIT SUB
            CASE CHR$(9): SHELL Edit$ + "dbse\" + CHR$(r% + 64): RUN BOOT
            CASE CHR$(13): r% = 0: EXIT SUB
            CASE CHR$(0) + "G": s% = 1: EXIT DO
            CASE CHR$(0) + "H": s% = s% - 1: EXIT DO
            CASE CHR$(0) + "I": r% = r% + 1: EXIT SUB
            CASE CHR$(0) + "K": r% = r% - 1: EXIT SUB
            CASE CHR$(0) + "M": r% = r% + 1: EXIT SUB
            CASE CHR$(0) + "O": s% = e% - 1: EXIT DO
            CASE CHR$(0) + "P": s% = s% + 1: EXIT DO
            CASE CHR$(0) + "Q": r% = r% - 1: EXIT SUB
            CASE CHR$(0) + "S": Delete r% + 64: EXIT SUB
            CASE "A" TO "Z": r% = ASC(UCASE$(k$)) - 64: EXIT SUB
        END SELECT
    END IF
    LOOP
    LOOP
END SUB

'This routine guarentees that all dates are formatted as a
'two digit month, two digit day and a four digit year (MM/DD/YYYY).
SUB FormatDate (i$)
    DIM d$, m$, y$, x%
    SELECT CASE LEFT$(i$, 1)
        CASE "0" TO "9"
            m$ = MID$(STR$(VAL(LEFT$(i$, 2))), 2)
            FOR x% = 1 TO LEN(i$)
            IF MID$(i$, x%, 1) = "/" THEN
                IF d$ = "" THEN
                    d$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
                ELSEIF y$ = "" THEN
                    y$ = MID$(STR$(VAL(MID$(i$, x% + 1))), 2)
                    EXIT FOR
                END IF
            END IF
            NEXT
            IF m$ = "" OR VAL(m$) < 1 OR VAL(m$) > 12 THEN m$ = LEFT$(DATE$, 2)
            IF d$ = "" OR VAL(d$) < 1 OR VAL(d$) > 31 THEN d$ = MID$(DATE$, 4, 2)
            IF y$ = "" OR VAL(y$) < 0 THEN y$ = RIGHT$(DATE$, 4)
            i$ = RIGHT$("00" + m$, 2) + "/"
            i$ = i$ + RIGHT$("00" + d$, 2) + "/"
            i$ = i$ + LEFT$(RIGHT$(DATE$, 4), 4 - LEN(y$)) + y$
        CASE "*": i$ = Today$
        CASE ELSE: IF i$ = "" THEN i$ = Bkd$
    END SELECT
END SUB

SUB FormatMBXTxt (i$)
    DIM o$, x%
    IF i$ = "-" THEN
        i$ = STRING$(78, 196)
    ELSE
        DO
        o$ = i$
        FOR x% = 1 TO LEN(i$)
        IF MID$(i$, x%, 3) = "[B]" THEN
            i$ = LEFT$(i$, x% - 1) + q + BOOT + q + MID$(i$, x% + 3)
            EXIT FOR
        END IF
        IF MID$(i$, x%, 2) = CHR$(39) + CHR$(39) THEN
            i$ = LEFT$(i$, x% - 1) + q + MID$(i$, x% + 2)
            EXIT FOR
        END IF
        IF MID$(i$, x%, 3) = "[R]" THEN
            i$ = LEFT$(i$, x% - 1) + "(" + CHR$(26) + ")" + MID$(i$, x% + 3)
            EXIT FOR
        END IF
        IF MID$(i$, x%, 6) = "[LUDR]" THEN
            i$ = LEFT$(i$, x% - 1) + "(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ")" + MID$(i$, x% + 6)
            EXIT FOR
        END IF
        IF MID$(i$, x%, 4) = "[UD]" THEN
            i$ = LEFT$(i$, x% - 1) + "(" + CHR$(24) + CHR$(25) + ")" + MID$(i$, x% + 4)
            EXIT FOR
        END IF
        IF MID$(i$, x%, 4) = "[LR]" THEN
            i$ = LEFT$(i$, x% - 1) + "(" + CHR$(27) + CHR$(26) + ")" + MID$(i$, x% + 4)
            EXIT FOR
        END IF
        NEXT
    'If there was no change in the variable "i$" then the variable "o$" will
    'still equal the variable "i$" causing an exit from the DO/LOOP.
        LOOP UNTIL o$ = i$
    END IF
END SUB

SUB Getkey (k$)
    DIM a%
    DO: LOOP UNTIL INKEY$ = ""
    DO
    k$ = INKEY$
    LOOP WHILE k$ = ""
    a% = ASC(k$)
    IF a% = 27 THEN
        END
    ELSEIF k$ = "*" THEN
        RUN BOOT
    END IF
END SUB

SUB Initialize
    DIM i$, x%
    MKDIR "abse"
    MKDIR "dbse"
    Bgc% = 1
    Bkc$ = CHR$(32)
    Bkd$ = "../../...."
    Bkl$ = STRING$(MXL, Bkc$)
    Edit$ = "notepad.exe "
    Fgc% = 15
    Fxt$ = ".txt"
    MyFile$ = "myfiles\myfiles" + LCASE$(RIGHT$(BOOT, 4))
    q = CHR$(34)
    Title$ = "Bank"
    Today$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    DO
    Flt% = 0: CLOSE : OPEN INI FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        FOR x% = 1 TO LEN(i$)
        IF x% < LEN(i$) THEN
            SELECT CASE UCASE$(LEFT$(i$, x%))
                CASE "BACKGROUNDCOLOR=": Bgc% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "EDITOR=": Edit$ = MID$(i$, x% + 1): EXIT FOR
                CASE "FILEEXTENTION=": Fxt$ = MID$(i$, x% + 1): EXIT FOR
                CASE "FOREGROUNDCOLOR=": Fgc% = VAL(MID$(i$, x% + 1)): EXIT FOR
                CASE "TITLE=": Title$ = MID$(i$, x% + 1): EXIT FOR
            END SELECT
        END IF
        NEXT
        WEND
        EXIT DO
    ELSE
        CLOSE : OPEN INI FOR OUTPUT AS #2
        PRINT #2, "BACKGROUNDCOLOR="; MID$(STR$(Bgc%), 2)
        PRINT #2, "EDITOR="; Edit$
        PRINT #2, "FILEEXTENTION="; Fxt$
        PRINT #2, "FOREGROUNDCOLOR="; MID$(STR$(Fgc%), 2)
        PRINT #2, "TITLE="; Title$
    END IF
    LOOP
    CLOSE
    SHELL "dir > dir.shl"
    OPEN "dir.shl" FOR INPUT AS #1
    FOR x% = 1 TO 4: INPUT #1, i$: NEXT
    CLOSE
    KILL "dir.shl"
    FOR x% = 1 TO LEN(i$)
    SELECT CASE LCASE$(MID$(i$, x%, 4))
        CASE "/fix", "\fix", "fix/", "fix\": Bgc% = 0: EXIT FOR
    END SELECT
    NEXT
    Src$ = LCASE$(MID$(i$, 14)) + "\"
    MyFile$ = LCASE$(LEFT$(Src$, LEN(Src$) - LEN(PRGM) - 1) + MyFile$)
    'Prove
END SUB

SUB LoadBank
    DIM a$, d$, i$, s$, t$, x%
    FOR x% = 1 TO 26
    Menu(0, x%) = Bkc$
    Flt% = 0: CLOSE : OPEN "dbse\" + CHR$(x% + 64) + Fxt$ FOR INPUT AS #1
    OPEN "abse\" + CHR$(x% + 64) + Fxt$ FOR OUTPUT AS #2
    IF NOT EOF(1) THEN
        s$ = ""
        INPUT #1, Menu(1, x%)
        IF NOT EOF(1) THEN
            s$ = ""
            WHILE NOT EOF(1)
            LINE INPUT #1, i$
            IF i$ = "" THEN
                i$ = ""
            ELSE
                Decant i$, d$, t$, a$
                Account.a = LEFT$(d$ + STRING$(LEN(Account.a), 46), LEN(Account.a))
                Account.b = LEFT$(t$ + STRING$(LEN(Account.b), 46), LEN(Account.b))
                Account.c = a$
                s$ = STR$(VAL(s$) + VAL(a$))
                PRINT #2, Account.a; Account.b; Account.c
            END IF
            WEND
            Menu(2, x%) = LTRIM$(RTRIM$(s$))
            IF VAL(s$) < 0 THEN
                Debt$ = STR$(VAL(Debt$) + VAL(MID$(s$, 2)))
            ELSEIF VAL(a$) > 0 THEN
                Cred$ = STR$(VAL(Cred$) + VAL(s$))
            END IF
            Account.a = Today$
            Account.b = LEFT$("Total" + STRING$(LEN(Account.b), 46), LEN(Account.b))
            Account.c = LTRIM$(RTRIM$(s$))
            PRINT #2, STRING$(LEN(Account), 196)
            PRINT #2, Account.a; Account.b; Account.c
            PRINT #2, STRING$(LEN(Account), 205)
        ELSE
            Account.a = Today$
            Account.b = LEFT$("No Activity" + STRING$(LEN(Account.b), 46), LEN(Account.b))
            Account.c = "0"
            PRINT #2, Account.a; Account.b; Account.c
            PRINT #2, STRING$(LEN(Account), 205)
        END IF
    ELSE
        Account.a = Today$
        Account.b = LEFT$("No Data" + STRING$(LEN(Account.b), 46), LEN(Account.b))
        Account.c = "0"
        PRINT #2, Account.a; Account.b; Account.c
        PRINT #2, STRING$(LEN(Account), 205)
        Menu(1, x%) = ""
        Menu(2, x%) = ""
    END IF
    NEXT
    CLOSE
END SUB

SUB Main
    DIM r%
    r% = 0
    DO
    IF r% < 0 THEN r% = 26
    IF r% > 26 THEN r% = 0
    Display r%, k$
    SELECT CASE k$
        CASE CHR$(8)
    END SELECT
    LOOP
END SUB

SUB Mask (r%)
    DIM i$, x%
    STATIC o$
    IF o$ = "" THEN
        o$ = "+"
        CLS
    ELSE
        LOCATE 1, 1, 0
    END IF
    COLOR Fgc%, Bgc%
    IF r% = 0 THEN
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3: PRINT "Ç"; STRING$(38, 196); "ÂÂ"; STRING$(38, 196); "¶"
            CASE 4 TO 16, 18: PRINT "º"; STRING$(38, 32); "³³"; STRING$(38, 32); "º"
            CASE 17: PRINT "Ç"; STRING$(38, 196); "ÅÅ"; STRING$(38, 196); "¶"
            CASE 19: PRINT "Ç"; STRING$(38, 196); "ÁÁ"; STRING$(38, 196); "¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
        NEXT
        SayIt Title$ + " Files ù " + Today$, 2, Fgc%, Bgc%
        FOR x% = 1 TO 13
        LOCATE x% + 3, 3: PRINT LEFT$(CHR$(x% + 64) + " " + Menu(1, x%) + STRING$(36, 46), 36)
        LOCATE x% + 3, 43: PRINT LEFT$(CHR$(x% + 77) + " " + Menu(1, x% + 13) + STRING$(36, 46), 36)
        NEXT
        LOCATE 18, 3: COLOR 14, Bgc%
        PRINT LEFT$("Debits" + STRING$(MXL, 46), MXL - LEN(DLR));
        PRINT USING (DLR); VAL(Debt$)
        LOCATE 18, 43: COLOR 10, Bgc%
        PRINT LEFT$("Credits" + STRING$(MXL, 46), MXL - LEN(DLR));
        PRINT USING (DLR); VAL(Cred$)
        RESTORE MsgMask0
        MsgBox
    ELSE
        FOR x% = 1 TO 23
        SELECT CASE x%
            CASE 1: PRINT "É"; STRING$(78, 205); "»"
            CASE 3: PRINT "Ç"; STRING$(LEN(Account.a) + 1, 196); "Â"; STRING$(LEN(Account.b), 196); "Â"; STRING$(LEN(Account.c) + 1, 196); "¶"
            CASE 4: PRINT "º"; STRING$(LEN(Account.a) + 1, 32); "³"; STRING$(LEN(Account.b), 32); "³"; STRING$(LEN(Account.c) + 1, 32); "º"
            CASE 5: PRINT "Ç"; STRING$(LEN(Account.a) + 1, 196); "Å"; STRING$(LEN(Account.b), 196); "Å"; STRING$(LEN(Account.c) + 1, 196); "¶"
            CASE 6 TO ECHO - 2
                PRINT "º"; STRING$(LEN(Account.a) + 1, 32); "³"; STRING$(LEN(Account.b), 32); "³"; STRING$(LEN(Account.c) + 1, 32); "º"
            CASE ECHO - 1: PRINT "Ç"; STRING$(LEN(Account.a) + 1, 196); "Á"; STRING$(LEN(Account.b), 196); "Á"; STRING$(LEN(Account.c) + 1, 196); "¶"
            CASE 23: PRINT "È"; STRING$(78, 205); "¼"
            CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
        END SELECT
        NEXT
        IF Menu(1, r%) = "" THEN
            i$ = q + CHR$(r% + 64) + Fxt$ + q
        ELSE
            i$ = q + Menu(1, r%) + q
        END IF
        SayIt Title$ + " ù " + i$ + " ù " + Today$, 2, Fgc%, Bgc%
        Account.a = "Date"
        Account.b = "Transaction"
        Account.c = RIGHT$(Bkl$ + "Amount", LEN(Account.c))
        LOCATE 4, 3: PRINT Account.a; CHR$(28); Account.b; CHR$(28); Account.c
        RESTORE MsgMask1
        MsgBox
    END IF
END SUB

SUB MsgBox
    DIM b%, f%, i$, o$, v%
    READ v%, f%, b%
    IF v% < ECHO THEN v% = ECHO
    IF v% > 22 THEN v% = 22
    IF f% < 0 THEN f% = Fgc%
    IF b% < 0 THEN b% = Bgc%
    Swipe f%, b%
    DO
    READ i$
    IF i$ = "*" THEN
        EXIT DO
    ELSE
        FormatMBXTxt i$
    END IF
    SayIt i$, v%, f%, b%
    LOOP
END SUB

SUB Prove
    CLS
    PRINT Src$
    PRINT MyFile$
    END
END SUB

SUB Purge
    DIM i$, x%
    SHELL "dir dbse > dbse.shl /B"
    CLOSE : OPEN "dbse.shl" FOR INPUT AS #1
    IF Flt% = 0 THEN
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        DO
        FOR x% = 1 TO 26
        IF UCASE$(i$) = UCASE$(CHR$(x% + 64) + Fxt$) THEN EXIT DO
        NEXT
        KILL "dbse\" + i$
        EXIT DO
        LOOP
        WEND
    ELSE
        Flt% = 0
    END IF
    CLOSE
    KILL "dbse.shl"
    KILL "abse\*.*"
END SUB

SUB SayIt (i$, v%, f%, b%)
    DIM e$, h%, l$, r$, w%
    IF i$ = "" OR i$ = " " THEN e$ = "" ELSE e$ = " ù "
    w% = 38
    l$ = LEFT$(i$, LEN(i$) / 2)
    r$ = MID$(i$, LEN(l$) + 1)
    l$ = RIGHT$(STRING$(w%, 32) + RIGHT$(e$, 2) + l$, w%)
    r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(w%, 32), w%)
    IF v% < 2 THEN v% = 2
    IF v% > 22 THEN v% = 22
    COLOR f%, b%
    LOCATE v%, 3
    PRINT l$; r$
    v% = v% + 1
    COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%, p%, r%)
    DIM b%, f%, h%, m$, o$, v%
    IF r% = 0 THEN
        FOR x% = s% TO e%
        SELECT CASE UCASE$(LEFT$(Menu(1, x%), 1))
            CASE "-", "0" TO "9", "A" TO "Z"
                o$ = "-"
                IF Menu(2, x%) = "" THEN
                    f% = 11
                ELSEIF VAL(Menu(2, x%)) <= 0 THEN
                    f% = 14
                ELSE
                    f% = 10
                END IF
            CASE ELSE: f% = 7: o$ = ""
        END SELECT
        SELECT CASE x%
            CASE 1 TO 13: LOCATE x% + 3, 2
            CASE ELSE: LOCATE x% - 10, 42
        END SELECT
        IF x% = p% THEN
            f% = 15
            b% = 13
        ELSE
            b% = Bgc%
        END IF
        IF Menu(0, x%) = CHR$(26) THEN COLOR 31, Bgc% ELSE COLOR f%, b%
        PRINT Menu(0, x%);
        COLOR f%, b%
        IF Menu(2, x%) = "" THEN
            PRINT LEFT$(CHR$(x% + 64) + o$ + Menu(1, x%) + STRING$(MXL, 46), MXL)
        ELSE
            PRINT LEFT$(CHR$(x% + 64) + o$ + Menu(1, x%) + STRING$(MXL, 46), MXL - LEN(DLR));
            PRINT USING (DLR); VAL(MID$(STR$(VAL(Menu(2, x%))), 2))
        END IF
        NEXT
    ELSE
        m$ = CHR$(28)
        FOR x% = 1 TO ROWS - 2
        SELECT CASE s% + x% - 1
            CASE 1 TO e%: GET #1, s% + x% - 1, Account
            CASE ELSE
                o$ = CHR$(32)
                Account.a = STRING$(LEN(Account.a), o$)
                Account.b = STRING$(LEN(Account.b), o$)
                Account.c = STRING$(LEN(Account.c), o$)
        END SELECT
        SELECT CASE UCASE$(LEFT$(Account.a, 1))
            CASE LEFT$(Bkd$, 1), "0" TO "9"
                f% = 10
                b% = Bgc%
            CASE "A" TO "Z"
                f% = 11
                b% = Bgc%
            CASE ELSE
                f% = Fgc%
                b% = Bgc%
        END SELECT
        SELECT CASE UCASE$(LEFT$(Account.a, 1))
            CASE "0" TO "9", "A" TO "Z": IF VAL(Account.c) <= 0 THEN f% = 14
        END SELECT
        LOCATE x% + 5, 3
        COLOR f%, b%
        PRINT Account.a; m$; Account.b; m$;
        SELECT CASE LEFT$(Account.c, 1)
            CASE "-", "0" TO "9": PRINT USING (DLR); VAL(Account.c)
            CASE ELSE: PRINT Account.c
        END SELECT
        NEXT
    END IF
    COLOR Fgc%, Bgc%
END SUB

SUB Sequent (s$, d$, t$, a$)
    IF d$ = "" THEN
        s$ = s$ + "x"
    ELSE
        SELECT CASE LCASE$(LEFT$(d$, 1))
            CASE "0" TO "9": s$ = s$ + "n"
            CASE "a" TO "z": s$ = s$ + "l"
            CASE ELSE: s$ = s$ + LCASE$(LEFT$(d$, 1))
        END SELECT
    END IF
    IF t$ = "" THEN
        s$ = s$ + "x"
    ELSE
        SELECT CASE LCASE$(LEFT$(t$, 1))
            CASE "0" TO "9": s$ = s$ + "n"
            CASE "a" TO "z": s$ = s$ + "l"
            CASE ELSE: s$ = s$ + LCASE$(LEFT$(t$, 1))
        END SELECT
    END IF
    IF a$ = "" THEN
        s$ = s$ + "x"
    ELSE
        SELECT CASE LCASE$(LEFT$(a$, 1))
            CASE "0" TO "9": s$ = s$ + "n"
            CASE "a" TO "z": s$ = s$ + "l"
            CASE ELSE: s$ = s$ + LCASE$(LEFT$(a$, 1))
        END SELECT
    END IF
    SELECT CASE LCASE$(s$)
        CASE "*ln": d$ = Today$
        CASE "*nl": d$ = Today$: SWAP t$, a$
        CASE "-ln": d$ = Bkd$
        CASE "-nl": d$ = Bkd$: SWAP t$, a$
        CASE "-lx": d$ = Bkd$
        CASE "-nx": d$ = Bkd$: a$ = t$: t$ = ""
        CASE "lnx": a$ = t$: t$ = d$: d$ = Bkd$
        CASE "nnx": a$ = t$: t$ = ""
    END SELECT
END SUB

SUB Swipe (f%, b%)
    DIM x%
    x% = ECHO
    DO
    SayIt "", x%, f%, b%
    LOOP UNTIL x% = 23
END SUB

SUB Switch (a%)
    DIM b%, c$, n$, o$, s$, x%
    b% = a%
    c$ = CHR$(26)
    RESTORE MsgSwitch
    MsgBox
    FOR v% = ECHO TO 22
    FOR h% = 2 TO 79
    s$ = CHR$(SCREEN(v%, h% - 1)) + CHR$(SCREEN(v%, h%)) + CHR$(SCREEN(v%, h% + 1))
    SELECT CASE UCASE$(s$)
        CASE "(" + c$ + ")"
            LOCATE v%, h%
            COLOR 31, Bgc%
            PRINT MID$(s$, 2, 1)
    END SELECT
    NEXT
    NEXT
    Menu(0, a%) = c$
    o$ = CHR$(a% + 64)
    DO
    IF b% < 1 THEN b% = 26
    IF b% > 26 THEN b% = 1
    Menu(0, b%) = c$
    n$ = CHR$(b% + 64)
    Scroll 1, 26, 0, 0
    Getkey k$
    SELECT CASE UCASE$(k$)
        CASE CHR$(13): EXIT DO
        CASE CHR$(0) + "R"
            NAME "dbse\" + o$ + Fxt$ AS "old.txt"
            NAME "dbse\" + n$ + Fxt$ AS "new.txt"
            NAME "new.txt" AS "dbse\" + o$ + Fxt$
            NAME "old.txt" AS "dbse\" + n$ + Fxt$
            RUN BOOT
        CASE CHR$(0) + "H": b% = b% - 1
        CASE CHR$(0) + "K": b% = b% - 13
        CASE CHR$(0) + "M": b% = b% + 13
        CASE CHR$(0) + "P": b% = b% + 1
        CASE "A" TO "Z": b% = ASC(UCASE$(k$)) - 64
    END SELECT
    FOR x% = 1 TO 26
    IF a% = x% THEN a% = x% ELSE Menu(0, x%) = Bkc$
    NEXT
    LOOP
    FOR x% = 1 TO 26: Menu(0, x%) = Bkc$: NEXT
END SUB

