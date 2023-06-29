20:
	READ D$
	IF LEN(D$) > 18 THEN D$ = LEFT$(D, 18)
	IF D$ <> "*" THEN D = D + 1: D$(D) = D$: GOTO 20
