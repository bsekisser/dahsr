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
	
