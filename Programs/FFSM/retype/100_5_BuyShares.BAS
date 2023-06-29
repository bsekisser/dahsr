BuyShares:
	IF Stock < 1 THEN GOTO BSjump
	IF Shares(Stock) = 0 OR Bankaccount < 1 THEN GOTO BSjump
	LOCATE 7, 2
	PRINT Blank$
	I$ = "Buy how many shares"
	LOCATE 7, 40 - (LEN(I$) / 2)
	PRINT I$: INPUT I$
	IF LEN(I$) = 0 THEN GOTO BSjump
	V$ = VAL(I$)
	IF V > Shares(Stock) THEN BEEP: GOTO BuyShares
	Bankaccount = Bankaccount - (V * Price(Stock))
	Yourstock(Stock) = Yourstock(Stock) + V
	Shares(Stock) = Shares(Stock) - V
	IF Shares(Stock) < 0 THEN Shares(Stock) = 0
	MV = MV - (V * Price(Stock))
	GOTO BSjump
