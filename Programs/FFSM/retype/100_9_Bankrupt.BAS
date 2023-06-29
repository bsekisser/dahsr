Bankrupt:
	BK = INT(RND * D) + 1
	IF BK$ = "" THEN RETURN
	MV = MV - (Shares(BK) * Price(BK))
	Bankaccount = Bankaccount - (Yourstock(BK) * Price(BK))
	Shares(BK) = 0
	Yourstock(BK) = 0
	I$ = D$(BK) + "-bankrupt!  Press a key to continue."
	B$(BK) = ""
	LOCATE 9, 2
	PRINT Blank$
	LOCATE 9, 40 - (LEN(I$) / 2)
	COLOR 31, 4
	PRINT I$
	COLOR 0, 15
BKK:
	K$ = INKEY$: IF K$ = "" THEN GOTO BKK
	K$ = "": K = 0
	LOCATE 9, 2
	PRINT Blank$
	
	RETURN
