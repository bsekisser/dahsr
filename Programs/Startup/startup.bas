DECLARE SUB Main ()
    ON ERROR GOTO Trap
    Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB Main
    DIM d$, i$, t$
    d$ = LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2) + "/" + RIGHT$(DATE$, 4)
    t$ = LEFT$(TIME$, 2) + MID$(TIME$, 4, 2)
    CLOSE
    OPEN "startup.txt" FOR INPUT AS #1
    OPEN "temp.txt" FOR OUTPUT AS #2
    IF EOF(1) THEN
        PRINT #2, d$; ":" + t$
    ELSE
        WHILE NOT EOF(1)
        LINE INPUT #1, i$
        IF LEN(i$) > 0 THEN
            IF EOF(1) THEN
                IF d$ = LEFT$(i$, LEN(d$)) THEN
                    PRINT #2, i$ + "/" + t$
                ELSE
                    PRINT #2, i$
                    PRINT #2, d$; ":" + t$
                END IF
            ELSE
                PRINT #2, i$
            END IF
        END IF
        WEND
    END IF
    CLOSE
    KILL "startup.txt"
    NAME "temp.txt" AS "startup.txt"
END SUB

