BuySell:
	IF Stock < 1 THEN Stock = D
	IF Stock > D THEN Stock = 1
	LOCATE 9, 12
	FOR V = 1 TO 12
		LOCATE ,2: PRINT Blank$
	NEXT V
	LOCATE 9, 12
	COLOR 10, 15
	I$ = B$(Stock)
	IF I$ = "" THEN I$ = D$(Stock) + " bankrupt!"
	LOCATE , 40 - (LEN(I$) / 2): PRINT I$
	I$ = "Volume of shares on market = " + STR$(Shares(Stock))
	LOCATE , 40 - (LEN(I$) / 2): PRINT I$
	I$ = "Volume of shares you own = " + STR$(Yourstock(Stock))
	LOCATE , 40 - (LEN(I$) / 2): PRINT I$
	I$ = "Press (B)uy, (S)ell, scroll (U)p, scroll (D)own"
	LOCATE , 40 - (LEN(I$) / 2): PRINT I$
	I$ = "Press ESC, RETURN, or SPACEBAR to continue!"
	LOCATE , 40 - (LEN(I$) / 2): PRINT I$
Sckey:
	K$ = INKEY$: IF K$ = "" THEN GOTO Sckey
	K = ASC(K$)
	IF K = 27 OR K = 13 THEN K = 32
	IF K = 32 THEN K$ = "": K = 0: GOTO BSjump
	IF K$ = "B" THEN GOTO BuyShares
	IF K$ = "S" THEN GOTO SellShares
	IF K$ = "D" THEN Stock = Stock - 1: GOTO BuySell
	IF K$ = "U" THEN Stock = Stock + 1: GOTO BuySell
	GOTO Sckey
