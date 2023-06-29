40:
	Bankaccount = 5000
	MV = 0
	BU$ = ""
	RANDOMIZE TIMER
	FOR X = 1 TO D
		V = INT(RND * 200) + 1
		V$ = MID$(STR$(V), 2)
		FR = INT(RND * 7) + 1
		V1 = VAL(LEFT$(FR$(FR), 1))
		V2 = VAL(RIGHT$(FR$(FR), 1))
		Price(X) = V + (V1 / V2)
		V = INT(RND * 999) + 1
		Shares(X) = V
		MV = MV + Price(X) * Shares(X)
		B$(X) = D$(X) + "=" + V$ + " " + FR$(FR)
	NEXT X
