DECLARE SUB Footer (n%)
DECLARE SUB Header (n%)
DECLARE SUB FormatTxtLine (i$)
DECLARE SUB SortDbse ()
DECLARE SUB Decant (i$, j$)
DECLARE SUB Htmlize ()
DECLARE SUB Search (s%, e%, k$)
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%, p%)
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB FormatHtmLine (i$)
	CONST BOOT = "kwiklist.bas"
	CONST MXL = 76
	CONST PRGM = "kwiklist"
	CONST WSZ = 13
	COMMON SHARED Bgc%
	COMMON SHARED BoxBorder$
	COMMON SHARED Dln AS STRING * MXL
	COMMON SHARED Echo%
	COMMON SHARED Fgc%
	COMMON SHARED Filename$
	COMMON SHARED Flt%
	COMMON SHARED Hrc$
	COMMON SHARED MarLeft$
	COMMON SHARED MyFile$
	COMMON SHARED Padding$
	COMMON SHARED q AS STRING * 1
	COMMON SHARED Sbse$
	COMMON SHARED ShellBorder$
	COMMON SHARED Source$
	COMMON SHARED Src$
	COMMON SHARED Target$
	COMMON SHARED Tb%
	COMMON SHARED Title$
	COMMON SHARED Today$
	COMMON SHARED Width$
	DIM SHARED Mth(12) AS STRING
	ON ERROR GOTO Trap
	Initialize
	SortDbse
	Bufferize
	Htmlize
	Main
END
Trap: Flt% = ERR: RESUME NEXT
Months:
DATA "January","February","March","April","May","June"
DATA "July","August","September","October","November","December"

SUB Bufferize
	DIM b$, f$, h$, i$
	b$ = "  "
	Flt% = 0
	CLOSE : OPEN Sbse$ FOR INPUT AS #1
	OPEN PRGM + ".bfr" FOR OUTPUT AS #2
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, f$
		CLOSE #3: OPEN Source$ + f$ FOR INPUT AS #3
		LINE INPUT #3, h$
		FormatTxtLine h$
		PRINT #2, LEFT$(h$ + " " + STRING$(MXL, 32), MXL)
		WHILE NOT EOF(3)
		LINE INPUT #3, i$
		FormatTxtLine i$
		PRINT #2, LEFT$(b$ + i$ + STRING$(MXL, 46), MXL)
		WEND
		IF NOT EOF(1) THEN PRINT #2, STRING$(MXL, 196)
		WEND
	ELSE
		Flt% = 0
		PRINT #2, LEFT$("Error: Cannot Find " + q + Sbse$ + q + STRING$(MXL, 46), MXL)
	END IF
	CLOSE
	KILL "temp.txt"
END SUB

SUB Cuckoo
	SOUND 1400, 3
	SOUND 0, 2
	SOUND 1155, 4
	SOUND 0, 2
END SUB

SUB Decant (i$, j$)
	CLOSE #9: OPEN "decant.txt" FOR OUTPUT AS #9
	PRINT #9, i$
	i$ = "": j$ = ""
	CLOSE #9: OPEN "decant.txt" FOR INPUT AS #9
	INPUT #9, i$
	LINE INPUT #9, j$
	CLOSE #9
	KILL "decant.txt"
END SUB

SUB Footer (n%)
	PRINT #n%, "</div>"
	PRINT #n%, "</body>"
	PRINT #n%, "</html>"
END SUB

SUB FormatHtmLine (i$)
	DIM x%
	DO UNTIL x% > LEN(i$)
	FOR x% = 1 TO LEN(i$)
	IF MID$(i$, x%, 1) = CHR$(34) THEN
		i$ = LEFT$(i$, x% - 1) + "&#34;" + MID$(i$, x% + 1)
		EXIT FOR
	ELSEIF MID$(i$, x%, 1) = CHR$(39) THEN
		i$ = LEFT$(i$, x% - 1) + "&#39;" + MID$(i$, x% + 1)
		EXIT FOR
	ELSEIF MID$(i$, x%, 1) = CHR$(40) THEN
		i$ = LEFT$(i$, x% - 1) + "&#40;" + MID$(i$, x% + 1)
		EXIT FOR
	ELSEIF MID$(i$, x%, 1) = CHR$(41) THEN
		i$ = LEFT$(i$, x% - 1) + "&#41;" + MID$(i$, x% + 1)
		EXIT FOR
	ELSEIF MID$(i$, x%, 1) = CHR$(44) THEN
		i$ = LEFT$(i$, x% - 1) + ": " + MID$(i$, x% + 1)
		EXIT FOR
	ELSEIF MID$(i$, x%, 1) = CHR$(60) THEN
		i$ = LEFT$(i$, x% - 1) + "&#60;" + MID$(i$, x% + 1)
		EXIT FOR
	ELSEIF MID$(i$, x%, 1) = CHR$(62) THEN
		i$ = LEFT$(i$, x% - 1) + "&#62;" + MID$(i$, x% + 1)
		EXIT FOR
	ELSEIF MID$(i$, x%, 3) = " & " THEN
		i$ = LEFT$(i$, x%) + "&#38;" + MID$(i$, x% + 2)
		EXIT FOR
	END IF
	NEXT
	LOOP
END SUB

SUB FormatTxtLine (i$)
	DIM x%
	FOR x% = 1 TO LEN(i$)
	IF MID$(i$, x%, 1) = "," THEN
		i$ = LEFT$(i$, x% - 1) + ": " + MID$(i$, x% + 1)
		EXIT FOR
	END IF
	NEXT
END SUB

SUB Getkey (k$)
	DO
	k$ = INKEY$
	LOOP UNTIL k$ = ""
	DO
	k$ = INKEY$
	LOOP WHILE k$ = ""
	IF ASC(k$) = 27 THEN
		END
	ELSEIF k$ = "*" THEN
		RUN BOOT
	END IF
END SUB

SUB Header (n%)
	PRINT #n%, "<!doctype html>"
	PRINT #n%, "<html lang="; q; "en"; q; ">"
	PRINT #n%, "<head>"
	PRINT #n%, "<meta charset="; q; "utf-8"; q; ">"
	PRINT #n%, "<title>KwikList</title>"
	PRINT #n%, "<style type="; q; "text/css"; q; ">"
	PRINT #n%, "BODY"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "font-family:georgia,"; q; "times new roman"; q; ",trebuchet,serif;"
	PRINT #n%, TAB(Tb%); "font-style:normal;"
	PRINT #n%, TAB(Tb%); "font-weight:bold;"
	PRINT #n%, TAB(Tb%); "text-align:left;"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, "HR"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "color:"; Hrc$; ";"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, "LI"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "text-indent:4%;"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, "#shell"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "border:"; ShellBorder$; ";"
	PRINT #n%, TAB(Tb%); "margin:auto;"
	PRINT #n%, TAB(Tb%); "width:"; Width$; ";"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".boxa"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "border:"; BoxBorder$; ";"
	PRINT #n%, TAB(Tb%); "padding:"; Padding$; ";"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".boxb"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "border:0px solid #fff;"
	PRINT #n%, TAB(Tb%); "margin-left:"; MarLeft$; ";"
	PRINT #n%, TAB(Tb%); "padding:"; Padding$; ";"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".mhdr"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "font-size:30px;"
	PRINT #n%, TAB(Tb%); "padding:"; Padding$; ";"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".shdr"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "font-size:16px;"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".mhdr, .shdr"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "background:#00f;"
	PRINT #n%, TAB(Tb%); "color:#fff;"
	PRINT #n%, TAB(Tb%); "text-align:center;"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".mtxt"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "color:#00f;"
	PRINT #n%, TAB(Tb%); "font-size:22px;"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".stxt, .itxt"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "color:#00f;"
	PRINT #n%, TAB(Tb%); "font-size:20px;"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, ".text"
	PRINT #n%, TAB(Tb%); "{"
	PRINT #n%, TAB(Tb%); "color:#000;"
	PRINT #n%, TAB(Tb%); "font-size:18px;"
	PRINT #n%, TAB(Tb%); "font-style:italic;"
	PRINT #n%, TAB(Tb%); "}"
	PRINT #n%, "</style>"
	PRINT #n%, "</head>"
	PRINT #n%, "<body>"
END SUB

SUB Htmlize
	DIM f$, i$, j$, n$, x%
	CLOSE : OPEN Target$ FOR OUTPUT AS #2
	Header 2
	PRINT #2, "<div id="; q; "shell"; q; ">"
	PRINT #2, TAB(Tb% * 2); "<div class="; q; "mhdr"; q; ">"
	PRINT #2, TAB(Tb% * 2); "DAHSR&#39;s &#34;KwikList&#34;<br>"
	PRINT #2, TAB(Tb% * 3); "<span class="; q; "shdr"; q; ">A Quick Html Tagname Reference</span>"
	PRINT #2, TAB(Tb% * 2); "</div>"
	Flt% = 0: OPEN Sbse$ FOR INPUT AS #1
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, f$
		CLOSE #3: OPEN Source$ + f$ FOR INPUT AS #3
		IF Flt% = 0 THEN
			PRINT #2, TAB(Tb% * 2); "<div class="; q; "boxa"; q; ">"
			LINE INPUT #3, i$
			Decant i$, j$
			FormatHtmLine i$
			PRINT #2, TAB(Tb% * 3); "<span class="; q; "mtxt"; q; ">"; LCASE$(i$); "</span>";
			IF j$ = "" THEN
				j$ = ""
				PRINT #2, "<br>"
				PRINT #2, ""
			ELSE
				FormatHtmLine j$
				PRINT #2, ": <span class="; q; "stxt"; q; ">"; j$; "</span><br>"
				PRINT #2, ""
			END IF
			IF NOT EOF(3) THEN
				PRINT #2, TAB(Tb% * 4); "<div class="; q; "boxb"; q; ">"
				PRINT #2, "<hr>"
				n$ = "0"
				WHILE NOT EOF(3)
				LINE INPUT #3, i$
				Decant i$, j$
				IF i$ = "" THEN
					i$ = ""
				ELSE
					n$ = MID$(STR$(VAL(n$) + 1), 2)
					FormatHtmLine i$
					PRINT #2, TAB(Tb% * 3); "<span class="; q; "itxt"; q; ">"; n$; ". "; LCASE$(i$); "</span>";
					IF j$ = "" THEN
						j$ = ""
						IF NOT EOF(3) THEN PRINT #2, "<br>"
						PRINT #2, ""
					ELSE
						FormatHtmLine j$
						IF RIGHT$(j$, 1) = "." THEN j$ = j$ ELSE j$ = j$ + "."
						PRINT #2, "<span class="; q; "text"; q; ">: "; j$; "</span>";
						IF NOT EOF(3) THEN PRINT #2, "<br>"
						PRINT #2, ""
					END IF
				END IF
				WEND
				PRINT #2, TAB(Tb% * 4); "</div>"
			ELSE
				PRINT #2, ""
			END IF
			PRINT #2, TAB(Tb% * 2); "</div>"
		ELSE
			Flt% = 0
		END IF
		WEND
	ELSE
		Flt% = 0
	END IF
	Footer 2
	CLOSE
END SUB

SUB Initialize
	DIM i$, x%
	RESTORE Months
	FOR x% = 1 TO 12: READ Mth(x%): NEXT
	Bgc% = 1
	BoxBorder$ = "1px solid #000"
	Echo% = WSZ + 5
	Fgc% = 15
	Flt% = 0
	Hrc$ = "#f0f"
	MarLeft$ = "50px"
	MyFile$ = "myfiles\myfiles" + LCASE$(RIGHT$(BOOT, 4))
	Padding$ = "5px"
	q = CHR$(34)
	Sbse$ = "dbse.txt": SHELL "dir dbse > " + Sbse$ + " /B"
	ShellBorder$ = "1px solid #000"
	Source$ = "dbse\": MKDIR "dbse"
	Target$ = PRGM + ".htm"
	Tb% = 4
	Title$ = "DAHSR's " + q + "KwikList" + q + " ù " + "A Quick Html Tagename Reference"
	Today$ = MID$(DATE$, 4, 2) + " " + Mth(VAL(LEFT$(DATE$, 2))) + " " + RIGHT$(DATE$, 4)
	Width$ = "100%"
	DO
	Flt% = 0: CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		FOR x% = 1 TO LEN(i$)
		IF x% < LEN(i$) THEN
			SELECT CASE UCASE$(LEFT$(i$, x%))
				CASE "BOXBORDER=": BoxBorder$ = MID$(i$, x% + 1): EXIT FOR
				CASE "HRCOLOR=": Hrc$ = MID$(i$, x% + 1): EXIT FOR
				CASE "SHELLBORDER=": ShellBorder$ = MID$(i$, x% + 1): EXIT FOR
				CASE "MARGINLEFT=": MarLeft$ = MID$(i$, x% + 1): EXIT FOR
				CASE "MYFILES=": MyFile$ = MID$(i$, x% + 1): EXIT FOR
				CASE "PADDING=": Padding$ = MID$(i$, x% + 1): EXIT FOR
				CASE "SOURCE=": Source$ = MID$(i$, x% + 1): EXIT FOR
				CASE "TARGET=": Target$ = MID$(i$, x% + 1): EXIT FOR
				CASE "WIDTH=": Width$ = MID$(i$, x% + 1): EXIT FOR
			END SELECT
		END IF
		NEXT
		WEND
		EXIT DO
	ELSE
		CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #2
		PRINT #2, "BOXBORDER="; BoxBorder$
		PRINT #2, "HRCOLOR="; Hrc$
		PRINT #2, "SHELLBORDER="; ShellBorder$
		PRINT #2, "MARGINLEFT="; MarLeft$
		PRINT #2, "MYFILES="; MyFile$
		PRINT #2, "PADDING="; Padding$
		PRINT #2, "SOURCE="; Source$
		PRINT #2, "TARGET="; Target$
		PRINT #2, "WIDTH="; Width$
	END IF
	LOOP
	CLOSE
	SHELL "dir > src.shl"
	OPEN "src.shl" FOR INPUT AS #1
	FOR x% = 1 TO 4: LINE INPUT #1, i$: NEXT
	CLOSE
	KILL "src.shl"
	Src$ = LCASE$(MID$(i$, 15)) + "\"
	MyFile$ = LEFT$(Src$, LEN(Src$) - LEN(PRGM)) + MyFile$
	FOR x% = 1 TO LEN(Src$)
	SELECT CASE UCASE$(MID$(Src$, x%, 4))
		CASE "/FIX", "FIX/", "\FIX", "FIX\"
			Bgc% = 0
			EXIT FOR
	END SELECT
	NEXT
	FOR x% = 1 TO LEN(Source$)
	SELECT CASE MID$(Source$, x%, 1)
		CASE "/", "\": MKDIR LEFT$(Source$, x% - 1)
	END SELECT
	NEXT
	'CLS
	'PRINT Src$
	'PRINT MyFile$
	'END
END SUB

SUB Main
	DIM e%, k$, p%, s%
	Mask
	CLOSE : OPEN PRGM + ".bfr" FOR RANDOM AS #1 LEN = MXL + 2
	e% = LOF(1) / (MXL + 2)
	p% = 1
	s% = 1
	DO
	IF p% < 1 THEN p% = 1: s% = s% - 1
	IF p% > WSZ THEN p% = WSZ: s% = s% + 1
	IF s% < 1 THEN s% = 1
	IF s% > e% THEN s% = e%
	Scroll s%, e%, p%
	DO
	Getkey k$
	SELECT CASE UCASE$(k$)
		CASE CHR$(9)
			IF Filename$ = "" THEN
				SHELL "notepad.exe " + PRGM + ".ini"
			ELSE
				SHELL "notepad.exe " + Source$ + Filename$
			END IF
			RUN BOOT
		CASE CHR$(0) + CHR$(15)
			SHELL "notepad.exe " + PRGM + ".ini"
			RUN BOOT
		CASE CHR$(0) + "G": p% = 1: s% = 1: EXIT DO
		CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
		CASE CHR$(0) + "I": Search s%, e%, k$: EXIT DO
		CASE CHR$(0) + "K": p% = p% - WSZ + 1: EXIT DO
		CASE CHR$(0) + "M": p% = p% + WSZ - 1: EXIT DO
		CASE CHR$(0) + "O": p% = 1: s% = e%: EXIT DO
		CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
		CASE CHR$(0) + "Q": Search s%, e%, k$: p% = 1: EXIT DO
		CASE "A" TO "Z": Search s%, e%, k$: EXIT DO
	END SELECT
	LOOP
	LOOP
END SUB

SUB Mask
	DIM i$, x%
	STATIC o$
	IF o$ = "" THEN
		o$ = "+"
		CLS
	ELSE
		o$ = "-"
		LOCATE 1, 1, 0
	END IF
	COLOR Fgc%, Bgc%
	FOR x% = 1 TO 23
	SELECT CASE x%
		CASE 1: PRINT "É"; STRING$(78, 205); "»"
		CASE 3, WSZ + 4: PRINT "Ç"; STRING$(78, 196); "¶"
		CASE 23: PRINT "È"; STRING$(78, 205); "¼"
		CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
	END SELECT
	NEXT
	SayIt Title$, 2, 11, Bgc%
	IF o$ = "+" THEN
		FOR x% = 4 TO WSZ + 3
		SayIt "Standby, Sorting Database", x%, 14, Bgc%
		NEXT
	ELSE
		SayIt "Source: " + q + Source$ + q, Echo%, 10, Bgc%
		SayIt "Target: " + q + Target$ + q, Echo% + 1, 10, Bgc%
		i$ = ""
		i$ = i$ + "Scroll (" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Lines"
		i$ = i$ + " ù PAGE Tagnames"
		i$ = i$ + " ù Goto (A to Z) Tagname"
		SayIt i$, Echo% + 2, 11, Bgc%
		SayIt "ESC to End", Echo% + 4, Fgc%, Bgc%
	END IF
END SUB

SUB SayIt (i$, v%, f%, b%)
	DIM e$, h%, l$, r$, w%
	IF i$ = "" THEN e$ = "" ELSE e$ = " ù "
	IF v% < 2 THEN v% = 2
	IF v% > 22 THEN v% = 22
	h% = 3: w% = 38
	l$ = LEFT$(i$, LEN(i$) / 2)
	r$ = MID$(i$, LEN(l$) + 1)
	l$ = RIGHT$(STRING$(w%, 32) + RIGHT$(e$, 2) + l$, w%)
	r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(w%, 32), w%)
	COLOR f%, b%
	LOCATE v%, h%
	PRINT l$; r$
	COLOR Fgc%, Bgc%
END SUB

SUB Scroll (s%, e%, p%)
	DIM b%, f%, i$, m$, x%, y%
	Filename$ = ""
	FOR x% = 1 TO WSZ
	SELECT CASE s% + x% - 1
		CASE 1 TO e%: GET #1, s% + x% - 1, Dln
		CASE e% + 1: Dln = STRING$(MXL, 205)
		CASE ELSE: Dln = STRING$(MXL, 32)
	END SELECT
	i$ = LEFT$(Dln, MXL)
	IF UCASE$(LEFT$(i$, 5)) = "ERROR" THEN
		f% = 14
		b% = 4
	ELSE
		SELECT CASE UCASE$(LEFT$(i$, 1))
			CASE "<"
				f% = 10
				b% = Bgc%
				IF x% = p% THEN
					Filename$ = LEFT$(i$, 1)
					FOR y% = 1 TO LEN(i$)
					SELECT CASE UCASE$(MID$(i$, y%, 1))
						CASE "0" TO "9", "A" TO "Z"
							Filename$ = Filename$ + MID$(i$, y%, 1)
						CASE ">"
							Filename$ = Filename$ + MID$(i$, y%, 1)
							EXIT FOR
					END SELECT
					NEXT
				END IF
			CASE " ": f% = 14: b% = Bgc%
			CASE ELSE: f% = Fgc%: b% = Bgc%
		END SELECT
		IF x% = p% THEN f% = 15: b% = 13
	END IF
	SayIt LEFT$(i$ + STRING$(MXL, 46), MXL), x% + 3, f%, b%
	IF LEFT$(i$, 1) = "<" THEN
		i$ = i$
	ELSE
		FOR y% = 1 TO LEN(i$)
		IF MID$(i$, y%, 1) = ":" THEN
			LOCATE x% + 3, y% + 3
			COLOR 11, b%: PRINT MID$(i$, y% + 1)
			EXIT FOR
		END IF
		NEXT
	END IF
	NEXT
	IF Filename$ = "" THEN
		SayIt "Press TAB or SHIFT/TAB to Edit " + q + LCASE$(PRGM) + ".ini" + q, Echo% + 3, Fgc%, Bgc%
	ELSE
		i$ = "Press TAB to Edit " + q + LCASE$(Filename$) + q
		i$ = i$ + " or SHIFT/TAB to Edit " + q + LCASE$(PRGM) + ".ini" + q
		SayIt i$, Echo% + 3, 11, Bgc%
	END IF
END SUB

SUB Search (s%, e%, k$)
	DIM x%
	x% = s%
	DO
	SELECT CASE UCASE$(k$)
		CASE CHR$(0) + "I": x% = x% - 1
		CASE CHR$(0) + "Q": x% = x% + 1
		CASE "A" TO "Z": x% = x% + 1
	END SELECT
	IF x% < 1 THEN x% = e%
	IF x% > e% THEN x% = 1
	IF x% = s% THEN EXIT DO
	GET #1, x%, Dln
	SELECT CASE UCASE$(k$)
		CASE "A" TO "Z": IF UCASE$(MID$(Dln, 2, 1)) = UCASE$(k$) THEN EXIT DO
		CASE ELSE: IF LEFT$(Dln, 1) = "<" THEN EXIT DO
	END SELECT
	LOOP
	s% = x%
END SUB

SUB SortDbse
	DIM f$
	Mask
	CLOSE : OPEN Sbse$ FOR INPUT AS #1
	WHILE NOT EOF(1)
	LINE INPUT #1, f$
	SHELL "sort " + Source$ + f$ + " > temp.txt"
	KILL Source$ + f$
	NAME "temp.txt" AS Source$ + f$
	WEND
	CLOSE
END SUB

