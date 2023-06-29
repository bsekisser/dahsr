Decimler:
	V$ = STR$(V): L = LEN(V$)
	FOR X = 1 TO L
		IF MID$(V$, X, 1) = "." THEN GOTO Decimler1
	NEXT X
	V$ = V$ + ".00"
Decimler1:
	IF L - X = 1 THEN V$ = V$ + "0"
	IF L - X > 2 THEN
		EV$ = MID$(V$, X + 3)
		V$ = LEFT$(V$, X + 2)
		IF VAL(LEFT$(EV$, 1)) > 5 THEN
			V = VAL(V$)
			V = V + .01
			V$ = STR$(V)
		END IF
	END IF
	IF V > 0 THEN V$ = MID$(V$, 2)
	RETURN
