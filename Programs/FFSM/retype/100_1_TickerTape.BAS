TickerTape:
	FOR Ticker = 1 TO D

		Stock$ = B$(Ticker) + Trailer$
		IF Yourstock(Ticker) > 0 THEN Stock$ = "**" + Stock$
		IF B$(Ticker) = "" THEN Stock$ = D$(Stock) + " bankrupt!" + Trailer$
		
		FOR Tape = 1 to LEN(Stock$)
			BU$ = BU$ + MID$(Stock$, Tape, 1)
			IF LEN(BU$) > 70 THEN BU$ = RIGHT(BU$, 78)
			LOCATE 3, 80 - (LEN(BU$))
			COLOR 10, 0
			PRINT BU$
			COLOR 0, 15
			
			RANDOMIZE TIMER
			
			Chance = INT(RND * 1000) + 1
			IF Chance = 1 THEN GOSUB Backinservice
			IF Chance = 2 THEN GOSUB Bankrupt
			IF Chance > 900 THEN GOSUB Stockvalues
			
			K$ = INKEY$: IF K$ <> "" THEN K = ASC(K$)
			
			IF K = 27 THEN END
			
			IF K = 32 THEN GOSUB BuySell
			
			V = MV: GOSUB Decimler: I$ = "Market value = $" + V$
			IF V > 1000000 THEN I$ = I$ + " m"
			V = Bankaccount: GOSUB Decimler: I$ = I$ + "   Bank acct = $" + V$
			IF V > 1000000 THEN I$ = I$ + " m"
			IF CKP = 1 THEN LOCATE 5, 2: PRINT Blank$: CKP = 0
			LOCATE 5, 40 - (LEN(I$) / 2)
			PRINT I$
			
			IF Bankaccount > 100000000000# THEN I$ = "You broke the bank": GOTO Eog
			IF Bankaccount < 1 THEN I$ = "You messed up turkey, you're busted, go home" : GOTO Eog
			IF MV < 1 THEN I$ = "Stock market crash": GOTO Eog
			
			FOR PZ = 1 TO 500: NEXT PZ
			
		NEXT Tape
TTjump:
	NEXT Ticker
	GOTO TickerTape
