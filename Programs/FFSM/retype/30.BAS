30:
	FOR X = 1 TO D
		FOR Y = X TO D
			IF X = Y THEN GOTO 31
			IF D$(X) > D$(Y) THEN T$ = D$(X): D$(X) = D$(Y): D$(Y) = T$
31:
		NEXT Y
	NEXT X
