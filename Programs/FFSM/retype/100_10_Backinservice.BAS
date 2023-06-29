Backinservice:
	Stock = INT(RND * D) + 1
	IF B$(Stock) <> "" THEN RETURN
	Shares(Stock) = INT(RND * 200) + 1
	V = INT(RND * 200) + 1
	V$ = MID$(STR$(V), 2)
	FR = INT(RND * 7) + 1
	V1 = VAL(LEFT$(FR$(FR), 1))
	V2 = VAL(RIGHT$(FR$(FR), 1))
	Newprice = V + (V1 / V2)
	Price(Stock) = Newprice
	MV = MV + (Newprice * Shares(Stock))
	B$(Stock) = D$(Stock) + " = " + V$ + " "  FR$(FR)
	LOCATE 9, 12
	FOR V = 1 TO 12
		LOCATE ,2: PRINT Blank$
	NEXT V
	LOCATE 9, 12
	COLOR 10, 15
	I$ = D$(Stock) + " is back in business!"
	LOCATE ,40 - (LEN(I$) / 2): PRINT I$
	I$ = "Volume of shares on market = " + STR$(Shares(Stock))
	LOCATE ,40 - (LEN(I$) / 2): PRINT I$
	I$ = "Price per share is " + V$ + " " + FR$(FR)
	LOCATE ,40 - (LEN(I$) / 2): PRINT I$
	I$ = "Press any key!"
	LOCATE ,40 - (LEN(I$) / 2): PRINT I$
Bisk:
	K$ = INKEY$: IF K$ = "" THEN GOTO Bisk
	K$ = "": K = 0
	LOCATE 9, 2
	FOR V = 1 TO 12
		LOCATE ,2: PRINT Blank$
	NEXT V
	
	RETURN
