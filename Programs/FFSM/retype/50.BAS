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
