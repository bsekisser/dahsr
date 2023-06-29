DATA "IBM", "AT&T", "SONOCO", "MACDONALDS", "KMART", "APPLE", "ARCO", "ATARI", "WHITE PHARMACEUTICALS",
"LASER", "PENN CENTRAL", "MOBILE", "APPLIED RESEARCH", "ARMOUR", "ACME", "BELL ATLANTIC",
"Ach, Top & SaFe RR", "BURGER KING",

DATA "GREEN GIANT", "LIBBYS", "EMC", "FISCHER & PORTER", "FORD", "CHEVROLET", "CHRYSLER", "AMERICAN MOTORS",
"PONTIAC", "DELA PWR & LIGHT", "PECO", "CHESTER WATER AUTHORITY", "TWA", "NORTHWEST", "BOAC", "BOEING",
"L.I. RR", "BOSTON INT", "NEW YORK HERALD",

DATA "FOOD FAIR", "SUPER SAVER", "PATHMARK", "ZEROX", "TOYOTA", "MERC", "MERCEDES", "BMW", "HONDA", "HYUNDAI",
"RADIO SHACK", "WALMART", "SUBARU", "L.A. TIMES", "TIME LIFE", "WENDYS", "SEVEN ELEVEN", "WAWA",
"M.H. HARDWARE", "SAVEN VARIETY",

DATA "CCMC", "CAMPBELLS", "AMERICAN AIR LINES", "SHORTLINE BUS CO", "YELLOW CAB", "TVA", "DELCORA",
"HECKINGERS", "84 LUMBER", "BARGAIN CITY SALES", "AMTRAC", "ABBOTS DAIRIES", "WONDER BAKING CO",
"TASTY CAKE", "MELLON BANK", "PHILA. NAT. BANK",

DATA "SUN EAST FED.C.U.", "RED ARROW BUS", "DEL MONTE", "SUNKIST", "ANHEISER-BUSH", "MILLER BREWING",
"MOGAN DAVID", "OTTOWA SAND", "BAYER", "PILLSBURY", "BETTY CROCKER", "NABISCO", "KEYSTONE BRICK",
"PIZZA HUT", "DOMINOES PIZZA", "NIFTY NORM'S HDWRE",

DATA "HUGHES/DOUGLAS AIR", "NORTHRUP", "LEEDS", "BP OIL", "SINCLAIR", "TIMEX", "SMITH-KLINE-BCKMN",
"BEECHNUT", "DAYS INN", "WALDORF-ASTORIA", "HOWARD-JOHNSONS", "JOHNSONS WAX", "AJAX", "COMET CLEANSERS",
"PALMOLIVE", "IPANA", "AMANA", "WESTINGHOUSE",

DATA "GENERAL MOTORS", "CAMAY SOAPS", "FELS NAPTHA", "IVORY", "WARNER BROS", "MGM", "UNITED AIR",
"REPUBLIC PICS", "RCA", "AMER TOBACCO", "ALCOA", "KEISER ALUMINUM", "KELLOGS", "PENNREC GYMS", "AMF", "NIKE"
"JORDACHE", "US STEEL", "LOCKHEED", "SIKORSKI AIR",

DATA "FISCHER/PRICE", "PRICE/PFISTER", "TOYS'R'US", "KAYBE TOYS", "TIME/LIFE", "PEPSICO", "COCA-COLA-CO",
"SUNBEAM", "OSTER", "WHIRLPOOL", "GENERAL ELEC", "COMCAST INT", "BOSCO BEAN", "I&I SLING",
"BLH LOCO WORKS", "HARTZELL MACH WKS", "LAZY BOY", "LANE MANU",

DATA "WESTBEND", "DELRAY BOOKS", "COOLRAY", "BREMER SKI", "CANNON", "CHANNEL", "M&M CANDY", "HERSHEY",
"FLEER", "MATCHBOX", "TONKA TOYS", "PIONEER", "JVC", "MITSUBISHI", "UPI", "AP WIRE SERVICE",
"REUTERS", "CON-EDISON", "MATLACK", "RANGER JOES",

DATA "DISNEY", "TURNER BRDCSTNG", "OCCIDENTAL PETRO", "OKIDATA", "TORRO", "LAWN BOY", "JOHN DEER",
"MASSEY FERGUSON", "ROLEX",

DATA "*"

DIM B$(1000), D$(1000), FR$(7), Price(1000), Board$(20), Yourstock(1000), Shares(1000),
	Board(20), Banner$(10)

Banner$(1) = "$ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $"
Banner$(2) = "$      WELCOME TO WALL STREET,    $"
Banner$(3) = "$  A STOCK MARKET GAME OF CHANCE. $"
Banner$(4) = "$    PRESS ANY KEY AND START AN   $"
Banner$(5) = "$   EXCITING TRIP INTO THE WORLD  $"
Banner$(6) = "$           OF ECONOMY.           $"
Banner$(7) = "$ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $"

CLS: LOCATE 12, 29: PRINT "STANDBY, LOADING DATA!"

10:
	PN$ = "FUNNY FARM STOCK MARKET"

	InfoLine$ = "Press SPACEBAR to buy and sell"

	Trailer$ = "          "

	FR$(1) = "1/8"
	FR$(2) = "1/4"
	FR$(3) = "3/8"
	FR$(4) = "1/2"
	FR$(5) = "5/8"
	FR$(6) = "3/4"
	FR$(7) = "7/8"

	LT$ = "I" + STRING$(78, 205) + ";"
	LM$ = ":" + STRING$(78, 32) + ":"
	LMA$ = "L" + STRING$(78, 205) + "9"
	LB$ = "H" + STRING$(78, 205) + "<"
	Blackline$ = STIRNG$(78, 219)
	Blank$ = STRING$(78, 32)
	D = 0

20:
	READ D$
	IF LEN(D$) > 18 THEN D$ = LEFT$(D, 18)
	IF D$ <> "*" THEN D = D + 1: D$(D) = D$: GOTO 20

30:
	FOR X = 1 TO D
		FOR Y = X TO D
			IF X = Y THEN GOTO 31
			IF D$(X) > D$(Y) THEN T$ = D$(X): D$(X) = D$(Y): D$(Y) = T$
31:
		NEXT Y
	NEXT X

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

50:
	FOR X = 1 TO 7
		LOCATE X + 8, 40 - (LEN(Banner$(X)) / 2)
		CR = INT(RND * 15) + 1
		COLOR CR, 0
		FOR Y = 1 TO LEN(Banner$(X))
			PRINT MID$(Banner$(X), Y, 1);
			IF INKEY$ <> "" THEN GOTO 60
		NEXT Y
	NEXT X
GOTO 50

60:
	COLOR 0, 15

100:
	CLS: PRINT SPC(40 - (LEN(PN$) / 2)); PN$
	PRINT LT$
	PRINT LM$
	PRINT LMA$
	PRINT LM$
	PRINT LMA$
	PRINT LM$
	PRINT LMA$
	FOR X = 1 TO 12
		PRINT LM$
	NEXT X
	PRINT LB$
	LOCATE 3, 2: PRINT Blackline$
	LOCATE 5, 28: PRINT ":"
	LOCATE 5, 54: PRINT ":"
	LOCATE 7, 40 - (LEN(Infoline$) / 2)
	COLOR 9, 15: PRINT Infoline$: COLOR 0, 15
	
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

Decimler:
	V$ = STR$(V): L = LEN(V$)
	FOR X = 1 TO L
		IF MID$(V$, X, 1) = "." THEN GOTO Decimler1
	NEXT X
	V$ = V$ + ".00"
Decimler1:
	IF L - X = 1 THEN V$ = V$ + "0"
	IF L - X > 2 THEN
		EV$ = MID$(V$, X + 3)
		V$ = LEFT$(V$, X + 2)
		IF VAL(LEFT$(EV$, 1)) > 5 THEN
			V = VAL(V$)
			V = V + .01
			V$ = STR$(V)
		END IF
	END IF
	IF V > 0 THEN V$ = MID$(V$, 2)
RETURN

Stockvalues:
	Stock = INT(RND * D) + 1
	IF B$(Stock) = "" THEN RETURN

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

BSjump:
	LOCATE 7, 2
	PRINT Blank$
	LOCATE 7, 40 - (LEN(Infoline$) / 2)
	COLOR 9, 15: PRINT Infoline$: COLOR 0, 15
	LOCATE 9, 2
	FOR V = 1 TO 12
		LOCATE ,2: PRINT Blank$
	NEXT V
RETURN
	
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

Converter:
	VA = INT(V): FR = 8 * ((V - VA) * 100) / 100
	V$ = MID$(STR$(VA), 2)
RETURN
	
Eog:
	I$ = I$ + ", game over, press a key! "
	LOCATE 22, 40 - (LEN(I$) / 2)
	COLOR 31, 4
	PRINT I$
	COLOR 0, 15
Eoga:
	K$ = INKEY$: IF K$ = "" THEN GOTO Eoga
RUN
