Eog:
	I$ = I$ + ", game over, press a key! "
	LOCATE 22, 40 - (LEN(I$) / 2)
	COLOR 31, 4
	PRINT I$
	COLOR 0, 15
Eoga:
	K$ = INKEY$: IF K$ = "" THEN GOTO Eoga
	RUN
