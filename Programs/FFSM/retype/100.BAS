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
	
