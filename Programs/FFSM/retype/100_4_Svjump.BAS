Svjump:
	BOARD = 0
	Oldprice = Price(Stock)
	V = INT(RND * 200) + 1
	V$ = MID$(STR$(V), 2)
	FR = INT(RND * 7) + 1
	V1 = VAL(LEFT$(FR$(FR), 1))
	V2 = VAL(RIGHT$(FR$(FR), 1))
	Newprice = V + (V1 / V2)
	Price(Stock) = Newprice
	IF Yourstock(Stock) > 0 THEN
		Bankaccount = Bankaccount - (Oldprice * Yourstock(Stock))
		Bankaccount = Bankaccount + (Newprice * Yourstock(Stock))
	END IF
	MV = MV - (Oldprice * Shares(Stock)) + (Newprice * Shares(Stock))
	B$(Stock) = D$(Stock) + " = " + V$ + " "  FR$(FR)
	IF Yourstock(Stock) > 0 THEN Board = 1
	IF Shares(Stock) < 1 THEN Board = 2
	Board$ = D$(Stock) + Blank$
	Board$ = LEFT$(Board$, 21)
	I$ = MID$(STR$(Shares(Stock)), 2) + " shs"
	Board$ = Board$ + I$ + Blank$
	Board$ = LEFT$(Board$, 32)
	V = Oldprice: GOSUB Converter
	I$ = "Last " + V$ + " " + FR$(FR)
	Board$ = Board$ + I$ + Blank$
	Board$ = LEFT$(Board$, 47)
	V = Newprice: GOSUB Converter
	I$ = "Current " + V$ + " " + FR$(FR)
	Board$ = Board$ + I$ + Blank$
	Board$ = LEFT$(Board$, 78)
	IF Oldprice > Newprice THEN V = Oldprice - Newprice
	IF Oldprice < Newprice THEN V = Newprice - Oldprice
	IF Oldprice = Newprice THEN V = 0
	GOSUB Converter
	IF Oldprice = Newprice THEN I$ = "No change"
	IF Oldprice > Newprice THEN I$ = "Down " + V$ + " " + FR$(FR)
	IF Oldprice < Newprice THEN I$ = "Up " + V$ + " " + FR$(FR)
	Board$ = LEFT$(Board$, 78 - LEN(I$)) + I$
	FOR BP = 12 TO 2 STEP -1
		Board$(BP) = Board$(BP - 1)
		Board(BP) = Board(BP - 1)
	NEXT BP
	LOCATE 9, 2
	Board$(1) = Board$
	Board(1) = Board
	FOR BP = 1 TO 14
		COLOR 0, 15
		IF BP = 1 THEN COLOR 15, 15
		IF Board(BP) = 1 THEN COLOR 14
		IF Board(BP) = 2 THEN COLOR 4
		LOCATE ,2: PRINT Board$(BP)
		COLOR 0, 15
	NEXT BP
	
	CKP = 1
	
	RETURN
