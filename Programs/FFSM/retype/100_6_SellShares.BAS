SellShares:
	IF Stock < 1 THEN GOTO BSjump
	IF Yourstock(Stock) = 0 THEN GOTO BSjump
	LOCATE 7, 2
	PRINT Blank$
	I$ = "Sell how many shares"
	LOCATE 7, 40 - (LEN(I$) / 2)
	PRINT I$: INPUT I$
	IF LEN(I$) = 0 THEN GOTO BSjump
	IF I$ = "ALL" OR I$ = "DUMP" THEN I$ = STR(Yourstock(Stock))
	V$ = VAL(I$)
	IF V > Yourstock(Stock) THEN BEEP: GOTO SellShares
	Bankaccount = Bankaccount + (V * Price(Stock))
	Yourstock(Stock) = Yourstock(Stock) - V
	Shares(Stock) = Shares(Stock) + V
	MV = MV + (V * Price(Stock))
	GOTO BSjump
