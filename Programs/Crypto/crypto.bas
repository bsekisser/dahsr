DECLARE SUB ScanSpeed (k$)
DECLARE SUB Find (s%, e%, k$)
DECLARE SUB Solve ()
DECLARE SUB Encode (w$)
DECLARE SUB AutoSearch ()
DECLARE SUB Logo ()
DECLARE SUB Processkey (k$)
DECLARE SUB Justify (v%, h%, y%)
DECLARE SUB LoadGram ()
DECLARE SUB Delete (m%)
DECLARE SUB Insert ()
DECLARE SUB Launch (k$)
DECLARE SUB Display (k$)
DECLARE SUB Scroll (s%, e%, p%, m%)
DECLARE SUB Bufferize ()
DECLARE SUB Mask (m%)
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
	CONST BOOT = "crypto.bas"
	CONST COLS = 5
	CONST MXL = 12
	CONST MXW = 76
	CONST PRGM = "crypto"
	CONST ROWS = 14
	CONST TOP = 3
	COMMON SHARED Bgc%
	COMMON SHARED Cgram$
	COMMON SHARED Dln AS STRING * MXL
	COMMON SHARED Echo%
	COMMON SHARED Fgc%
	COMMON SHARED Filename$
	COMMON SHARED Flt%
	COMMON SHARED Lsp%
	COMMON SHARED MyFile$
	COMMON SHARED q$
	COMMON SHARED Sgram$
	COMMON SHARED Shdo%
	COMMON SHARED Span%
	COMMON SHARED Src$
	COMMON SHARED Tc$
	COMMON SHARED Tf$
	COMMON SHARED Tl$
	COMMON SHARED Tn$
	COMMON SHARED Wgram$
	DIM SHARED ABC(26) AS INTEGER
	ON ERROR GOTO Trap
	Initialize
	Bufferize
	Main
END
Trap: Flt% = ERR: RESUME NEXT

SUB AutoSearch
	DIM c$, ce$, d$, de$, k$, x%, y%
	Mask 2
	DO
	CLOSE #3: OPEN PRGM + ".dic" FOR INPUT AS #3
	WHILE NOT EOF(3)
	LINE INPUT #3, d$
	de$ = UCASE$(d$)
	Encode de$
	IF LEN(Cgram$) >= LEN(de$) THEN
		FOR x% = 1 TO LEN(Cgram$) - LEN(de$) + 1
		c$ = UCASE$(MID$(Cgram$, x%, LEN(de$)))
		ce$ = c$
		Encode ce$
		IF UCASE$(ce$) = UCASE$(de$) THEN
			FOR y% = 1 TO LEN(c$)
			Processkey MID$(c$, y%, 1)
			Processkey MID$(d$, y%, 1)
			NEXT
			Scroll 0, 0, 0, 1
			k$ = INKEY$
			SELECT CASE ASC(k$)
				CASE 13: EXIT DO
				CASE 27: END
			END SELECT
			ScanSpeed k$
		END IF
		NEXT
	END IF
	WEND
	LOOP
	CLOSE #3
END SUB

SUB Bufferize
	DIM i$
	SHELL "dir dbse > dbse.shl /B"
	CLOSE : OPEN "dbse.shl" FOR INPUT AS #1
	OPEN PRGM + ".bfr" FOR OUTPUT AS #2
	WHILE NOT EOF(1)
	LINE INPUT #1, i$
	SELECT CASE UCASE$(LEFT$(i$, 1))
		CASE "0" TO "9", "A" TO "Z"
			PRINT #2, LEFT$(i$ + STRING$(MXL, 32), MXL)
	END SELECT
	WEND
	CLOSE
END SUB

SUB Cuckoo
	SOUND 1400, 3
	SOUND 0, 2
	SOUND 1155, 4
	SOUND 0, 2
END SUB

SUB Delete (m%)
	SELECT CASE m%
		CASE 0
			DIM k$
			IF Filename$ = "" THEN
				EXIT SUB
			ELSE
				SayIt "DELETE " + q$ + Filename$ + q$ + " (Y)es or (N)o ?", 2, 14, Bgc%
				DO
				Getkey k$
				SELECT CASE UCASE$(k$)
					CASE CHR$(0) + "S", "Y"
						KILL "dbse\" + Filename$
						EXIT DO
					CASE "N": EXIT DO
				END SELECT
				LOOP
			END IF
			RUN BOOT
		CASE 1
			DIM c$, x%
			FOR x% = 1 TO LEN(Cgram$)
			c$ = UCASE$(MID$(Cgram$, x%, 1))
			SELECT CASE c$
				CASE "A" TO "Z": c$ = "_"
			END SELECT
			Wgram$ = LEFT$(Wgram$, x% - 1) + c$ + MID$(Wgram$, x% + 1)
			NEXT
	END SELECT
END SUB

SUB Display (k$)
	DIM b%, c$, f%, h%, v%, w$, x%
	Mask 1
	DO
	Scroll 0, 0, 0, 1
	Solve
	DO
	Getkey k$
	IF Flt% = 255 THEN Mask 1
	SELECT CASE UCASE$(k$)
		CASE CHR$(13): EXIT SUB
		CASE CHR$(27): END
		CASE CHR$(32): AutoSearch: Mask 1: EXIT DO
		CASE CHR$(0) + "S": Processkey k$: EXIT DO
		CASE "A" TO "Z": Processkey k$: EXIT DO
	END SELECT
	LOOP
	LOOP
END SUB

SUB Encode (w$)
	DIM a$, b$, c$, x%, y%, z%
	a$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	b$ = STRING$(LEN(w$), 32)
	z% = 0
	FOR x% = 1 TO LEN(w$)
	c$ = MID$(w$, z%, 1)
	SELECT CASE UCASE$(MID$(w$, x%, 1))
		CASE "A" TO "Z"
			z% = z% + 1
			c$ = MID$(a$, z%, 1)
			FOR y% = x% TO LEN(w$)
			IF c$ <> MID$(w$, x%, 1) AND MID$(w$, x%, 1) = MID$(w$, y%, 1) THEN
				b$ = LEFT$(b$, y% - 1) + c$ + MID$(b$, y% + 1)
			END IF
			NEXT
		CASE ELSE: b$ = LEFT$(b$, y% - 1) + c$ + MID$(b$, y% + 1)
	END SELECT
	NEXT
	w$ = b$
END SUB

SUB Find (s%, e%, k$)
	DIM x%
	x% = s%
	DO
	x% = x% + 1
	IF x% > e% THEN x% = 1
	IF x% = s% THEN EXIT DO
	GET #1, x%, Dln
	IF UCASE$(k$) = UCASE$(LEFT$(Dln, 1)) THEN EXIT DO
	LOOP
	s% = x%
END SUB

SUB Getkey (k$)
	DIM a%
	DO: LOOP UNTIL INKEY$ = ""
	DO
	k$ = INKEY$
	LOOP WHILE k$ = ""
	a% = ASC(k$)
	IF a% = 27 THEN END
	IF k$ = "*" THEN RUN BOOT
END SUB

SUB Initialize
	DIM i$, x%, y%
	Bgc% = 1
	MKDIR "dbse"
	Echo% = TOP + ROWS + 2
	Shdo% = 20
	Fgc% = 15
	Flt% = 0
	Lsp% = 2
	MyFile$ = "myfiles\myfiles" + LCASE$(RIGHT$(BOOT, 4))
	q$ = CHR$(34)
	Span% = COLS * ROWS
	Src$ = ""
	CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		FOR x% = 1 TO LEN(i$)
		IF x% < LEN(i$) THEN
			SELECT CASE UCASE$(LEFT$(i$, x%))
				CASE "LSP=", "LINES=", "SPACE=": Lsp% = VAL(MID$(i$, x% + 1)): EXIT FOR
			END SELECT
		END IF
		NEXT
		WEND
	END IF
	CLOSE
	SHELL "dir > source.shl"
	OPEN "source.shl" FOR INPUT AS #1
	FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
	CLOSE
	KILL "source.shl"
	Src$ = MID$(i$, 15) + "\"
	FOR x% = LEN(Src$) TO 1 STEP -1
	SELECT CASE MID$(Src$, x%, 1)
		CASE "/", "\"
			y% = y% + 1
			IF y% = 2 THEN
				MyFile$ = LEFT$(Src$, x%) + MyFile$
			END IF
	END SELECT
	NEXT
END SUB

SUB Insert
	DIM c$, g%
	FOR g% = 1 TO 32767
	c$ = "dbse\cg" + MID$(STR$(g%), 2) + ".txt"
	DO
	Flt% = 0
	CLOSE #3: OPEN c$ FOR INPUT AS #3
	IF Flt% = 0 THEN EXIT DO
	CLOSE #2: OPEN c$ FOR OUTPUT AS #2
	EXIT FOR
	LOOP
	NEXT
	CLOSE
	RUN BOOT
END SUB

SUB Justify (v%, h%, y%)
	DIM x%, z%
	z% = h%
	FOR x% = y% TO LEN(Cgram$)
	z% = z% + 1
	IF x% > y% AND MID$(Cgram$, x%, 1) = CHR$(32) THEN EXIT FOR
	NEXT
	IF z% > MXW THEN v% = v% + Lsp%: h% = 0
END SUB

SUB Launch (k$)
	DO
	IF Filename$ = "" THEN
		EXIT DO
	ELSE
		SELECT CASE k$
			CASE CHR$(9)
				SHELL "notepad.exe dbse\" + Filename$
				RUN BOOT
			CASE CHR$(13)
				LoadGram
				Display k$
				Mask 0
				EXIT DO
		END SELECT
	END IF
	LOOP
END SUB

SUB LoadGram
	DIM c$, i$, l%, x%
	l% = MXW * (ROWS - 3) / Lsp% - MXW
	Cgram$ = ""
	Wgram$ = ""
	Tc$ = "0"
	Tf$ = "0"
	Tl$ = "0"
	Tn$ = "0"
	CLOSE #3: OPEN "dbse\" + Filename$ FOR INPUT AS #3
	WHILE NOT EOF(3)
	LINE INPUT #3, i$
	IF NOT EOF(1) THEN i$ = i$ + " "
	Cgram$ = Cgram$ + UCASE$(i$)
	WEND
	CLOSE #3
	IF LEN(Cgram$) > l% THEN
		Cgram$ = LEFT$(Cgram$, l%)
	END IF
	FOR x% = 1 TO LEN(Cgram$)
	c$ = MID$(Cgram$, x%, 1)
	SELECT CASE UCASE$(c$)
		CASE "0" TO "9": Tn$ = MID$(STR$(VAL(Tn$) + 1), 2)
		CASE "A" TO "Z"
			ABC(ASC(c$) - 64) = ABC(ASC(c$) - 64) + 1
			c$ = "_"
			Tl$ = MID$(STR$(VAL(Tl$) + 1), 2)
		CASE ELSE: IF c$ <> CHR$(32) THEN Tf$ = MID$(STR$(VAL(Tf$) + 1), 2)
	END SELECT
	Wgram$ = Wgram$ + c$
	NEXT
	Tc$ = MID$(STR$(VAL(Tf$) + VAL(Tl$) + VAL(Tn$)), 2)
END SUB

SUB Logo
	COLOR 14, Bgc%
	LOCATE 2, 3: PRINT "Use Me"
	LOCATE 2, 67: PRINT "Save Erasers"
	COLOR Fgc%, Bgc%
END SUB

SUB Main
	DIM k$
	CLS
	Mask 0
	CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
	e% = LOF(1) / (MXL + 2)
	s% = 1
	p% = 1
	DO
	IF p% < 1 THEN p% = 1: s% = s% - 1
	IF p% > Span% THEN p% = Span%: s% = s% + 1
	IF s% < 1 THEN s% = 1
	IF s% > e% THEN s% = e%
	Scroll s%, e%, p%, 0
	DO
	Getkey k$
	SELECT CASE UCASE$(k$)
		CASE CHR$(8)
			CHDIR LEFT$(MyFile$, LEN(MyFile$) - 12)
			RUN MyFile$
			CHDIR LEFT$(Src$, LEN(Src$) - 1)
			RUN BOOT
		CASE CHR$(9), CHR$(13): Launch k$: EXIT DO
		CASE CHR$(27): END
		CASE CHR$(0) + ";": SHELL "notepad.exe " + PRGM + ".ini": RUN BOOT
		CASE CHR$(0) + "<": SHELL "notepad.exe " + PRGM + ".dic": RUN BOOT
		CASE CHR$(0) + "G": p% = 1: s% = 1: EXIT DO
		CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
		CASE CHR$(0) + "K": p% = p% - ROWS: EXIT DO
		CASE CHR$(0) + "M": p% = p% + ROWS: EXIT DO
		CASE CHR$(0) + "O": p% = 1: s% = e%: EXIT DO
		CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
		CASE CHR$(0) + "R": Insert: RUN BOOT
		CASE CHR$(0) + "S": Delete 0: RUN BOOT
		CASE "A" TO "Z": Find s%, e%, k$: EXIT DO
	END SELECT
	LOOP
	LOOP
END SUB

SUB Mask (m%)
	DIM i$, x%
	SELECT CASE m%
		CASE 0
			LOCATE 1, 1
			COLOR Fgc%, Bgc%
			FOR x% = 1 TO 23
			SELECT CASE x%
				CASE 1: PRINT "É"; STRING$(78, 205); "»"
				CASE TOP, Echo% - 1: PRINT "Ç"; STRING$(78, 196); "¶"
				CASE 23: PRINT "È"; STRING$(78, 205); "¼"
				CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
			END SELECT
			NEXT
			SayIt "ù DAHSR's " + q$ + "CRYPTO" + q$ + " ù", 2, 11, Bgc%
			Logo
			i$ = ""
			i$ = i$ + "ù Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") to Select a Cryptogram "
			i$ = i$ + "ù"
			SayIt i$, Echo%, Fgc%, Bgc%
			i$ = ""
			i$ = i$ + "ù Press ENTER to Work the Cryptogram "
			i$ = i$ + "or TAB to Edit the Cryptogram "
			i$ = i$ + "ù"
			SayIt i$, Echo% + 1, Fgc%, Bgc%
			i$ = ""
			i$ = i$ + "ù INSERT a New Cryptogram "
			i$ = i$ + "ù DELETE a Selected Cryptogram "
			i$ = i$ + "ù"
			SayIt i$, Echo% + 2, Fgc%, Bgc%
			i$ = ""
			i$ = i$ + "ù F1:Edit INI "
			i$ = i$ + "ù F2:Edit Dictionary "
			i$ = i$ + "ù BACKSPACE to Exit "
			i$ = i$ + "ù ESC to End "
			i$ = i$ + "ù"
			SayIt i$, Echo% + 3, Fgc%, Bgc%
		CASE 1
			LOCATE 1, 1
			COLOR Fgc%, Bgc%
			FOR x% = 1 TO 23
			SELECT CASE x%
				CASE 1: PRINT "É"; STRING$(78, 205); "»"
				CASE TOP, Shdo% - 5, Shdo% - 1: PRINT "Ç"; STRING$(78, 196); "¶"
				CASE 23: PRINT "È"; STRING$(78, 205); "¼"
				CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
			END SELECT
			NEXT
			SayIt "ù Filename:" + q$ + Filename$ + q$ + " ù", 2, 11, Bgc%
			Logo
			LOCATE Shdo% - 5, 33: PRINT "[ Letter Count ]"
			SayIt "ù Letters(" + Tl$ + ") + Numbers(" + Tn$ + ") + Figures(" + Tf$ + ") = Chrs(" + Tc$ + ") ù", Shdo% - 4, Fgc%, Bgc%
			h% = 3
			FOR x% = 1 TO 13
			LOCATE Shdo% - 3, h%: PRINT CHR$(x% + 64); ":"; RIGHT$("00" + MID$(STR$(ABC(x%)), 2), 2)
			LOCATE Shdo% - 2, h%: PRINT CHR$(x% + 77); ":"; RIGHT$("00" + MID$(STR$(ABC(x% + 13)), 2), 2)
			h% = h% + 6
			NEXT
			SayIt "ù Type Letters from " + q$ + "A" + q$ + " to " + q$ + "Z" + q$ + " to Select and Swap ù", Shdo%, Fgc%, Bgc%
			i$ = ""
			i$ = i$ + "ù Press SPACEBAR for AutoSearch "
			i$ = i$ + "ù DELETE Work "
			i$ = i$ + "ù"
			SayIt i$, Shdo% + 1, Fgc%, Bgc%
			i$ = ""
			i$ = i$ + "ù ENTER to Exit "
			i$ = i$ + "ù ESC to End "
			i$ = i$ + "ù"
			SayIt i$, Shdo% + 2, Fgc%, Bgc%
		CASE 2
			SayIt "ù Searching Dictionary ù", Shdo%, 30, Bgc%
			SayIt "ù (Decrease) " + CHR$(27) + "- Scan Speed -" + CHR$(26) + " (Increase) ù", Shdo% + 1, 10, Bgc%
			SayIt "ù ENTER to Exit ù ESC to End ù", Shdo% + 2, Fgc%, Bgc%
		CASE 3
			SayIt "ù Possible Solution ù", Shdo%, 30, Bgc%
	END SELECT
END SUB

SUB Processkey (k$)
	DIM b$, c$, m$, w$, x%
	STATIC l$, o%
	IF k$ = CHR$(0) + "S" THEN
		Delete 1
		o% = 1
	ELSE
		b$ = "_"
		k$ = UCASE$(k$)
		m$ = CHR$(24)
		FOR x% = 1 TO LEN(Cgram$)
		c$ = MID$(Cgram$, x%, 1)
		w$ = MID$(Wgram$, x%, 1)
		IF o% = 0 THEN
			IF c$ = k$ THEN
				w$ = m$
				l$ = k$
			END IF
		ELSEIF o% = 1 THEN
			IF k$ = l$ THEN
				IF c$ = k$ THEN w$ = b$
			ELSE
				IF w$ = k$ THEN w$ = b$
				IF w$ = m$ THEN w$ = k$
			END IF
		END IF
		Wgram$ = LEFT$(Wgram$, x% - 1) + w$ + MID$(Wgram$, x% + 1)
		NEXT
	END IF
	IF o% = 0 THEN
		o% = 1
	ELSE
		o% = 0
	END IF
END SUB

SUB SayIt (i$, v%, f%, b%)
	DIM h%, l$, r$, w%
	w% = 38
	SELECT CASE w%
		CASE 38: h% = 3
		CASE 39: h% = 2
		CASE ELSE: h% = 1
	END SELECT
	l$ = LEFT$(i$, LEN(i$) / 2)
	r$ = MID$(i$, LEN(l$) + 1)
	l$ = RIGHT$(STRING$(w%, 32) + l$, w%)
	r$ = LEFT$(r$ + STRING$(w%, 32), w%)
	COLOR f%, b%
	LOCATE v%, h%
	PRINT l$; r$
	COLOR Fgc%, Bgc%
END SUB

SUB ScanSpeed (k$)
	DIM m%, x%, y%
	STATIC s%
	m% = 5000
	IF s% = 0 THEN s% = m% / 2
	IF s% < 1 THEN s% = 1
	IF s% > m% THEN s% = m%
	SELECT CASE k$
		CASE CHR$(0) + "K": s% = s% - (s% + .5)
		CASE CHR$(0) + "M": s% = s% + (s% + .5)
	END SELECT
	FOR x% = 0 TO s%
	FOR y% = 0 TO s%
	NEXT
	NEXT
END SUB

SUB Scroll (s%, e%, p%, m%)
	DIM b%, f%, h%, i$, v%, x%
	SELECT CASE m%
		CASE 0
			Filename$ = ""
			h% = 5
			v% = 1
			FOR x% = 1 TO Span%
			SELECT CASE s% + x% - 1
				CASE 1 TO e%: GET #1, s% + x% - 1, Dln
				CASE ELSE: Dln = STRING$(MXL, 32)
			END SELECT
			i$ = RTRIM$(Dln)
			SELECT CASE UCASE$(LEFT$(i$, 1))
				CASE "0" TO "9", "A" TO "Z"
					f% = 10
					b% = Bgc%
					IF x% = p% THEN
						Filename$ = i$
					END IF
				CASE ELSE
					f% = Fgc%
					b% = Bgc%
			END SELECT
			IF x% = p% THEN f% = 15: b% = 13
			LOCATE v% + 3, h%
			COLOR f%, b%
			PRINT LEFT$(i$ + STRING$(MXL, 46), MXL)
			v% = v% + 1
			IF v% > ROWS THEN v% = 1: h% = h% + MXL + 3
			NEXT
		CASE 1
			h% = 1
			v% = 1
			FOR x% = 1 TO LEN(Cgram$)
			c$ = MID$(Cgram$, x%, 1)
			SELECT CASE UCASE$(c$)
				CASE "0" TO "9": f% = 14: b% = Bgc%
				CASE "A" TO "Z": f% = 10: b% = Bgc%
				CASE ELSE: f% = Fgc%: b% = Bgc%
			END SELECT
			IF c$ = CHR$(32) THEN Justify v%, h%, x%
			LOCATE v% + 3, h% + 2
			COLOR f%, b%
			PRINT c$
			w$ = MID$(Wgram$, x%, 1)
			SELECT CASE UCASE$(w$)
				CASE "A" TO "Z": f% = 11: b% = Bgc%
				CASE "0" TO "9": f% = 14: b% = Bgc%
				CASE "_": f% = Fgc%: b% = Bgc%
				CASE CHR$(24): f% = 31: b% = 13
				CASE ELSE: f% = Fgc%: b% = Bgc%
			END SELECT
			LOCATE v% + 4, h% + 2
			COLOR f%, b%
			PRINT w$
			h% = h% + 1
			IF h% > MXW THEN v% = v% + Lsp%: h% = 1
			NEXT
	END SELECT
END SUB

SUB Solve
	DIM x%
	Flt% = 0
	DO
	FOR x% = 1 TO LEN(Wgram$)
	SELECT CASE MID$(Wgram$, x%, 1)
		CASE "_", CHR$(24): EXIT DO
	END SELECT
	NEXT
	Flt% = 255
	Mask 3
	EXIT DO
	LOOP
END SUB

