DECLARE SUB Decant (i$, a$, b$, c$, d$, e$)
DECLARE SUB ValidateDocument ()
DECLARE SUB Swipe (f%, b%)
DECLARE SUB CreateGCSS ()
DECLARE SUB CreateFCSS ()
DECLARE SUB Td (n%, a$, b$, c$, s$, t$, i$)
DECLARE SUB Table (n%, b$, c$, p$, s$, w$)
DECLARE SUB CreateGenealogy ()
DECLARE SUB ValidateFamily ()
DECLARE SUB Header (n%)
DECLARE SUB Bufferize ()
DECLARE SUB Scroll (s%, e%, p%)
DECLARE SUB Initialize ()
DECLARE SUB Getkey (k$)
DECLARE SUB Main ()
DECLARE SUB Mask ()
DECLARE SUB SayIt (i$, v%, f%, b%)
DECLARE SUB AccessLinks (i$, j$)
DECLARE SUB Footer (n%)
DECLARE SUB CreateLinks ()
DECLARE SUB GetFamilyRecord (i$, re$, ge$)
DECLARE SUB GetChildren (i$)
DECLARE SUB GetNotes (i$)
DECLARE SUB Launch (k$)
DECLARE SUB CreateDocuments ()
DECLARE SUB Generate ()
	CONST BOOT = "genelogy.bas", CSS = "genelogy.css", MXL = 76
	CONST PRGM = "genelogy", WSZ = 13
	COMMON SHARED Bfr$
	COMMON SHARED Bgc%
	COMMON SHARED Dbse$
	COMMON SHARED Directory$
	COMMON SHARED Dln AS STRING * MXL
	COMMON SHARED Echo%
	COMMON SHARED Family$
	COMMON SHARED Fgc%
	COMMON SHARED Flt%
	COMMON SHARED Gen$
	COMMON SHARED Html$
	COMMON SHARED Mxr$
	COMMON SHARED Nog$
	COMMON SHARED q$
	COMMON SHARED Shade$
	COMMON SHARED Tb%
	COMMON SHARED Tbc$  'Table border color.
	COMMON SHARED Tbp$  'Table cellpadding.
	COMMON SHARED Tbr$  'Table border size.
	COMMON SHARED Tbw$  'Table width.
	COMMON SHARED Title$
	DIM SHARED Month$(12)
	ON ERROR GOTO Trap
	Initialize
	Bufferize
	CreateGCSS
	Main
END
Trap: Flt% = ERR: RESUME NEXT

Months:
DATA "January", "February", "March", "April", "May", "June"
DATA "July","August","September","October","November","December"

Styles:
	DATA "A","{","color:#00f;","text-decoration:none;","}",
	DATA "A:hover","{","color:#fff;","background:#00f;","}",
	DATA "BODY","{","background:#bce;","font-family:georgia,(times new roman),trebuchet,serif;","font-style:normal;","font-weight:normal;","}",
	DATA "TABLE","{","background:#fff;","}",
	DATA "p","{","color:#000;","text-align:left;","text-indent:30px;","}",
	DATA ".btxt,",".gtxt,",".ital,",".note,",".rgst,",".rtxt,",".text,",".term,",".ytxt","{","font-size:16px;","}",
	DATA ".btxt","{","color:#00f;","}",
	DATA "#chld","{","color:#000;","font-weight:bold;","font-size:18px;","}",
	DATA ".chld","{","color:#000;","font-weight:bold;","font-size:16px;","}",
	DATA "#desc","{","color:#000;","font-weight:bold;","font-size:18px;","}",
	DATA ".desc","{","color:#000;","font-weight:bold;","font-size:16px;","}",
	DATA ".divs","{","color:#fff;","background:#484;","font-weight:bold;","font-size:18px;","}",
	DATA ".divs A","{","color:#fff;","background:#484;","font-weight:bold;","font-size:18px;","}",
	DATA ".fttr","{","color:#000;","background:#fff;","font-weight:bold;","font-size:12px;","}",
	DATA ".gtxt","{","color:#363;","}",
	DATA ".ital","{","color:#fff;","background:#000;","font-style:italic;","}",
	DATA ".menu","{","color:#fff;","background:#00f;","font-weight:bold;","font-size:30px;","}",
	DATA ".mhdr","{","color:#00f;","font-weight:bold;","font-size:30px;","}",
	DATA ".mtxt","{","color:#00f;","font-weight:bold;","font-size:20px;","}",
	DATA "#note","{","color:#000;","font-weight:bold;","font-size:18px;","}",
	DATA ".note","{","color:#000;","font-weight:bold;","font-size:16px;","}",
	DATA ".path","{","color:#000;","font-weight:bold;","font-size:22px;","}",
	DATA ".rgst","{","color:#000;","font-weight:bold;","}",
	DATA ".rtxt","{","color:#f00;","}",
	DATA ".shdr","{","color:#00f;","font-size:14px;","}",
	DATA "#spse","{","color:#000;","font-weight:bold;","font-size:18px;","}",
	DATA ".spse","{","color:#000;","font-weight:bold;","font-size:16px;","}",
	DATA ".stxt","{","color:#00f;","font-weight:bold;","font-size:18px;","}",
	DATA ".term","{","color:#000;","font-style:terminal;","font-weight:bold;","}",
	DATA ".text","{","color:#000;","}",
	DATA ".ytxt","{","color:#ff0;","}","*"

'Channel Input and Output Assignments:
'#1: Main Menu Buffer and DataFile Input
'#2: Document.htm Output
'#3: D#.htm Output
'#4: Register.htm Output
'#5: R#.htm Output
'#6: Create Link Output

SUB AccessLinks (i$, j$)
	DIM c$, p$, r$
	q$ = CHR$(34)
	CLOSE #6: OPEN Html$ + Directory$ + "\" + Directory$ + ".lnx" FOR INPUT AS #6
	DO WHILE NOT EOF(6)
	INPUT #6, c$, p$, r$
	IF UCASE$(i$) = c$ AND UCASE$(j$) = p$ THEN
		j$ = "<a href=" + q$ + "r" + r$ + ".htm" + q$ + ">" + p$ + "</a>"
		EXIT DO
	END IF
	LOOP
	CLOSE #6
END SUB

SUB Bufferize
	DIM b$, d$, f$, r$
	SHELL "dir " + LEFT$(Dbse$, 4) + " > shell.txt /B"
	r$ = "0"
	Flt% = 0
	CLOSE
	OPEN "shell.txt" FOR INPUT AS #1
	OPEN "genelogy.htm" FOR OUTPUT AS #8
	Header 8
	Td 8, "", "", "menu", "", "", "Genealogy Family Menu"
	IF Flt% = 0 THEN
		OPEN Bfr$ FOR OUTPUT AS #2
		WHILE NOT EOF(1)
		LINE INPUT #1, d$
		SELECT CASE UCASE$(LEFT$(d$, 1))
			CASE "0" TO "9", "A" TO "Z"
				CLOSE #3: OPEN Dbse$ + d$ + "\family.txt" FOR INPUT AS #3
				IF Flt% = 0 THEN
					DO WHILE NOT EOF(3)
					LINE INPUT #3, f$
					IF UCASE$(LEFT$(f$, 4)) = "[F]," THEN
						f$ = MID$(f$, 5)
						r$ = MID$(STR$(VAL(r$) + 1), 2)
						PRINT #2, LEFT$(d$ + STRING$(MXL, 32), MXL - LEN(f$)); f$
						IF b$ = "" THEN
							b$ = "#ffffff"
						ELSEIF b$ = "#ffffff" THEN
							b$ = Shade$
						ELSE
							b$ = "#ffffff"
						END IF
						Table 8, Tbr$, Tbc$, Tbp$, "", ""
						Td 8, "<", b$, "stxt", "4", "<", r$
						Td 8, "<", b$, "mtxt", "32", "", f$
						Td 8, "", b$, "stxt", "32", "", "<a href=" + q$ + Html$ + d$ + "\register.htm" + q$ + ">Register</a>"
						Td 8, ">", b$, "stxt", "32", ">", "<a href=" + q$ + Html$ + d$ + "\document.htm" + q$ + ">Documents</a>"
						PRINT #8, "</table>"
						EXIT DO
					END IF
					LOOP
				ELSE
					Flt% = 0
				END IF
		END SELECT
		WEND
	END IF
	Footer 8
	CLOSE
	KILL "shell.txt"
END SUB

SUB CreateDocuments
	DIM d$, i$, t$
	Flt% = 0
	CLOSE
	OPEN Dbse$ + Directory$ + "\document.txt" FOR INPUT AS #1
	OPEN Html$ + Directory$ + "\document.htm" FOR OUTPUT AS #2
	Header 2
	Td 2, "", "", "shdr", "", "", "<a href=" + q$ + "register.htm" + q$ + ">Register</a>"
	PRINT #2, ""
	Td 2, "", "", "menu", "", "<>", Family$ + " Genealogy Documents"
	Td 2, "<", "", "", "", "<", ""
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		SELECT CASE UCASE$(LEFT$(i$, 5))
			CASE "[DOC]"
				IF LEN(i$) > 3 THEN
					t$ = MID$(i$, 6)
				ELSE
					LINE INPUT #1, t$
				END IF
				d$ = MID$(STR$(VAL(d$) + 1), 2)
				PRINT #2, "<a href="; q$; "D"; d$; ".htm"; q$; ">"; t$; "</a><br>"
				CLOSE #3: OPEN Html$ + Directory$ + "\D" + d$ + ".htm" FOR OUTPUT AS #3
				Header 3
				Td 3, "", "", "shdr", "", "", "<a href=" + q$ + "register.htm" + q$ + ">Register</a> - <a href=" + q$ + "document.htm" + q$ + ">Documents</a>"
				PRINT #3, ""
				Td 3, "", "", "menu", "", "<>", Family$ + " Genealogy Document"
				Td 3, "", "", "text", "", "<", ""
				DO WHILE NOT EOF(1)
				LINE INPUT #1, i$
				SELECT CASE UCASE$(LEFT$(i$, 5))
					CASE "[EOD]"
						PRINT #3, "</td></tr>"
						Footer 3
						EXIT DO
					CASE ELSE
						PRINT #3, i$
				END SELECT
				LOOP
		END SELECT
		WEND
	END IF
	PRINT #2, "</td></tr>"
	Footer 2
	CLOSE
END SUB

SUB CreateFCSS
	DIM i$
	DO
	Flt% = 0
	CLOSE : OPEN CSS FOR INPUT AS #1
	IF Flt% = 0 THEN
		OPEN Html$ + Directory$ + "\" + LCASE$(Directory$) + ".css" FOR OUTPUT AS #2
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		PRINT #2, i$
		WEND
		EXIT DO
	ELSE
		CreateGCSS
	END IF
	LOOP
	CLOSE
END SUB

SUB CreateGCSS
	DIM i$, x%
	RESTORE Styles
	CLOSE : OPEN CSS FOR OUTPUT AS #2
	DO
	READ i$
	IF i$ = "*" THEN
		EXIT DO
	ELSEIF i$ = "" OR i$ = "-" THEN
		PRINT #2, ""
	ELSE
		FOR x% = 1 TO LEN(i$)
		IF MID$(i$, x%, 1) = "(" OR MID$(i$, x%, 1) = ")" THEN
			i$ = LEFT$(i$, x% - 1) + q$ + MID$(i$, x% + 1)
		END IF
		NEXT
		IF i$ = "{" OR i$ = "}" OR RIGHT$(i$, 1) = ";" THEN
			PRINT #2, TAB(Tb%); i$
		ELSE
			PRINT #2, i$
		END IF
	END IF
	LOOP
	CLOSE #2
END SUB

SUB CreateGenealogy
	DIM g$, i$, r$, x%
	r$ = "0"
	Flt% = 0
	CLOSE
	OPEN Dbse$ + Directory$ + "\family.txt" FOR INPUT AS #1
	IF Flt% = 0 THEN
		OPEN Html$ + Directory$ + "\register.htm" FOR OUTPUT AS #4
		Header 4
		Td 4, "", "", "shdr", "", "", "<a href=" + q$ + "document.htm" + q$ + ">Documents</a>"
		PRINT #4, ""
		Td 4, "", "", "menu", "", "<>", Family$ + " Family Register"
		Td 4, "", "", "stxt", "", "<", ""
		PRINT #4, "<u>Generations:</u><br>"
		FOR x% = 1 TO VAL(Nog$)
		PRINT #4, "<a href="; q$; "#gen" + MID$(STR$(x%), 2); q$; ">"; MID$(STR$(x%), 2); "</a>";
		IF x% < VAL(Nog$) THEN
			PRINT #4, " - "
		END IF
		NEXT
		PRINT #4, "</td></tr>"
		PRINT #4, ""
		Td 4, "<", "", "rgst", "", "<", ""
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		SELECT CASE UCASE$(LEFT$(i$, 3))
			CASE "[D]"
				r$ = MID$(STR$(VAL(r$) + 1), 2)
				GetFamilyRecord i$, r$, g$
			CASE "[G]"
				g$ = MID$(STR$(VAL(g$) + 1), 2)
				PRINT #4, "<div align="; q$; "center"; q$;
				PRINT #4, " class="; q$; "divs"; q$; ">";
				PRINT #4, "<a name="; q$; "gen"; g$; q$; ">";
				PRINT #4, "Generation: "; g$; "</a></div>"
		END SELECT
		WEND
		PRINT #4, "</td></tr>"
		Footer 4
	END IF
	CLOSE
END SUB

SUB CreateLinks
	DIM i$, j$, r$
	Mxr$ = "0"
	Nog$ = "0"
	Flt% = 0
	CLOSE
	OPEN Dbse$ + Directory$ + "\family.txt" FOR INPUT AS #1
	IF Flt% = 0 THEN
		OPEN Html$ + Directory$ + "\" + Directory$ + ".lnx" FOR OUTPUT AS #2
		WHILE NOT EOF(1)
		INPUT #1, i$
		SELECT CASE UCASE$(LEFT$(i$, 3))
			CASE "[C]"
				INPUT #1, j$
				PRINT #2, "[C],"; UCASE$(j$); ","; Mxr$
			CASE "[D]"
				Mxr$ = MID$(STR$(VAL(Mxr$) + 1), 2)
				INPUT #1, j$
				PRINT #2, "[D],"; UCASE$(j$); ","; Mxr$
			CASE "[G]": Nog$ = MID$(STR$(VAL(Nog$) + 1), 2)
		END SELECT
		WEND
	END IF
	CLOSE
END SUB

SUB Cuckoo
	SOUND 1400, 3
	SOUND 0, 2
	SOUND 1200, 4
	SOUND 0, 2
END SUB

SUB Decant (i$, a$, b$, c$, d$, e$)
	a$ = "": b$ = "": c$ = "": d$ = "": e$ = ""
	CLOSE #9: OPEN "temp.txt" FOR OUTPUT AS #9
	PRINT #9, i$
	i$ = ""
	CLOSE #9: OPEN "temp.txt" FOR INPUT AS #9
	INPUT #9, i$, a$, b$, c$, d$, e$
	CLOSE #9
	KILL "temp.txt"
END SUB

SUB Default
	DIM x%
	FOR x% = Echo% TO 22
	IF Flt% = 0 THEN
		READ i$
	ELSEIF i$ = "*" THEN
		EXIT FOR
	ELSE
		i$ = ""
	END IF
	SayIt i$, x%, 15, 4
	NEXT
	Flt% = 0
	DO
	Getkey k$
	LOOP UNTIL k$ = CHR$(13)
END SUB

SUB Footer (n%)
	PRINT #n%, "</table>"
	PRINT #n%, "</td>"
	PRINT #n%, "</table>"   'Frame table.
	PRINT #n%, "</body>"
	PRINT #n%, "</html>"
END SUB

SUB Generate
	DIM k$
	Swipe 14, Bgc%
	SayIt "Generating " + q$ + "html" + q$ + " Files for", Echo%, 14, Bgc%
	SayIt "the " + q$ + Family$ + q$ + " Genealogy", Echo% + 1, 14, Bgc%
	SayIt "Family Register, Records", Echo% + 2, 14, Bgc%
	SayIt "Documents and", Echo% + 3, 14, Bgc%
	SayIt "the " + q$ + LCASE$(Directory$) + ".css" + q$ + " File", Echo% + 4, 14, Bgc%
	ValidateFamily
	ValidateDocument
	KILL Directory$ + "\*.*"
	RMDIR Html$ + Directory$
	MKDIR Html$ + Directory$
	CreateFCSS
	CreateLinks
	CreateGenealogy
	CreateDocuments
	Swipe 10, Bgc%
	SayIt "The " + Family$ + " Genealogy", Echo%, 10, Bgc%
	SayIt q$ + "html" + q$ + " Register, Records,", Echo% + 1, 10, Bgc%
	SayIt "Documents and the", Echo% + 2, 10, Bgc%
	SayIt q$ + LCASE$(Directory$) + ".css" + q$ + " File is Finished", Echo% + 3, 10, Bgc%
	SayIt "ENTER to Continue ù ESC to End", Echo% + 4, Fgc%, Bgc%
	DO
	Getkey k$
	LOOP UNTIL k$ = CHR$(13)
END SUB

SUB GetChildren (i$)
	DIM p$, c$, b$, d$, v$
	Td 5, "<", "", "chld", "100", "<", ""
	PRINT #5, "<div align="; q$; "center"; q$; " id="; q$; "chld"; q$; ">";
	PRINT #5, "<u>Children</u></div>"
	Decant i$, p$, c$, b$, d$, v$
	AccessLinks "[D]", p$
	PRINT #5, p$; ": "; c$;
	IF LEN(b$) > 2 THEN PRINT #5, ", "; b$;
	IF LEN(d$) > 2 THEN PRINT #5, ", "; d$;
	PRINT #5, "<br>"
	DO
	LINE INPUT #1, i$
	SELECT CASE UCASE$(LEFT$(i$, 3))
		CASE "[C]"
			Decant i$, p$, c$, b$, d$, v$
			AccessLinks "[D]", p$
			PRINT #5, p$; ": "; c$;
			IF LEN(b$) > 2 THEN PRINT #5, ", "; b$;
			IF LEN(d$) > 2 THEN PRINT #5, ", "; d$;
			PRINT #5, "<br>"
		CASE "[N]": GetNotes i$: EXIT DO
		CASE "[E]": EXIT DO
	END SELECT
	LOOP
	PRINT #5, "</td></tr>"
	PRINT #5, ""
END SUB

SUB GetFamilyRecord (i$, re$, ge$)
	DIM ph$, de$, db$, dd$, ma$
	DIM sp$, sb$, sd$, pa$, vd$
	CLOSE #5: OPEN Html$ + Directory$ + "\R" + re$ + ".htm" FOR OUTPUT AS #5
	Header 5
	Td 5, "", "", "menu", "", "", Family$ + " Family Record"
	PRINT #5, ""
	Decant i$, ph$, de$, db$, dd$, ma$
	LINE INPUT #1, i$
	Decant i$, sp$, sb$, sd$, pa$, vd$
	PRINT #4, "<a href=" + q$ + "R" + re$ + ".htm" + q$ + ">" + ph$ + "</a>: ";
	PRINT #4, de$;
	IF LEN(sp$) > 0 THEN PRINT #4, " &amp; " + sp$;
	PRINT #4, ".<br>"
	AccessLinks "[C]", ph$
	Td 5, "", "", "mtxt", "30", "<", "<a href=" + q$ + "register.htm" + q$ + ">Register</a>"
	Td 5, "", "", "path", "40", "", ph$
	Td 5, "", "", "mtxt", "30", ">", "<a href=" + q$ + "document.htm" + q$ + ">Documents</a>"
	PRINT #5, ""
	Td 5, "<", "", "desc", "50", "<", ""
	PRINT #5, "<div align="; q$; "center"; q$; " id="; q$; "desc"; q$; "><u>Descendant</u></div>"
	PRINT #5, de$; "<br>";
	IF LEN(db$) > 2 THEN PRINT #5, db$; "<br>"
	IF LEN(dd$) > 2 THEN PRINT #5, dd$; "<br>"
	IF LEN(ma$) > 2 THEN PRINT #5, ma$
	PRINT #5, "</td>";
	Td 5, "<", "", "spse", "50", "", ""
	PRINT #5, "<div align="; q$; "center"; q$; " id="; q$; "spse"; q$; "><u>Spouse</u></div>"
	PRINT #5, sp$; "<br>";
	IF LEN(sb$) > 2 THEN PRINT #5, sb$; "<br>"
	IF LEN(sd$) > 2 THEN PRINT #5, sd$; "<br>"
	IF LEN(pa$) > 2 THEN PRINT #5, pa$
	PRINT #5, "</td></tr>"
	PRINT #5, ""
	DO WHILE NOT EOF(1)
	LINE INPUT #1, i$
	SELECT CASE UCASE$(LEFT$(i$, 3))
		CASE "[C]": GetChildren i$: EXIT DO
		CASE "[N]": GetNotes i$: EXIT DO
		CASE "[U]"
			Td 5, "", "", "chld", "", "<>", "Children Unknown"
			PRINT #5, ""
		CASE "[X]"
			Td 5, "", "", "chld", "", "<>", "No Children"
			PRINT #5, ""
		CASE "[E]": EXIT DO
	END SELECT
	LOOP
	Td 5, "", "", "btxt", "100", "<>", "<hr>Generation: " + ge$
	Footer 5
	CLOSE #5
END SUB

SUB Getkey (k$)
	DO: k$ = INKEY$: LOOP UNTIL k$ = ""
	DO: k$ = INKEY$: LOOP WHILE k$ = ""
	IF ASC(k$) = 27 THEN
		END
	ELSEIF k$ = "*" THEN
		RUN BOOT
	ELSEIF k$ = CHR$(0) + ";" THEN
		SHELL "notepad.exe " + PRGM + ".ini"
		RUN BOOT
	END IF
END SUB

SUB GetNotes (i$)
	Td 5, "<", "", "note", "", "<", ""
	PRINT #5, "<div align="; q$; "center"; q$; " id="; q$; "note"; q$; ">";
	PRINT #5, "<u>Notes</u></div>"
	PRINT #5, MID$(i$, 5); "<br>"
	DO WHILE NOT EOF(1)
	LINE INPUT #1, i$
	SELECT CASE UCASE$(LEFT$(i$, 3))
		CASE "[E]": EXIT DO
		CASE "[N]"
			PRINT #5, MID$(i$, 5); "<br>"
	END SELECT
	LOOP
	PRINT #5, "</td></tr>"
	PRINT #5, ""
END SUB

SUB Header (n%)
	PRINT #n%, "<!DOCTYPE html>"
	PRINT #n%, "<html lang="; q$; "en"; q$; ">"
	PRINT #n%, "<head><title>";
	IF n% = 8 THEN
		PRINT #n%, "Genealogy Menu";
	ELSE
		PRINT #n%, Family$; " Genealogy";
	END IF
	PRINT #n%, "</title>"
	PRINT #n%, "<meta charset="; q$; "utf-8"; q$;
	PRINT #n%, " name="; q$; "Genealogy"; q$;
	PRINT #n%, " description="; q$; Family$ + " Family Genealogy"; q$; ">"
	PRINT #n%, "<link href="; q$;
	IF n% = 8 THEN
		PRINT #n%, LCASE$(CSS);
	ELSE
		PRINT #n%, LCASE$(Directory$ + ".css");
	END IF
	PRINT #n%, q$; " rel="; q$; "stylesheet"; q$;
	PRINT #n%, " type="; q$; "text/css"; q$; ">"
	PRINT #n%, "</head>"
	PRINT #n%, "<body>"
	PRINT #n%, ""
	Table n%, Tbr$, Tbc$, "", "", Tbw$  'Frame table.
	PRINT #n%, "<td>"
	PRINT #n%, ""
	Table n%, "", "", "5", "", ""
	PRINT #n%, ""
END SUB

SUB Initialize
	DIM i$, x%
	RESTORE Months
	FOR x% = 1 TO 12
	READ Month$(x%)
	NEXT
	Bfr$ = "genelogy.bfr"
	Bgc% = 1
	Dbse$ = "dbse\": MKDIR "dbse"
	Echo% = WSZ + 5
	Family$ = ""
	Fgc% = 15
	Directory$ = ""
	Html$ = "html\": MKDIR "html"
	q$ = CHR$(34)
	Shade$ = "#dddddd"
	Tb% = 4
	Tbc$ = "#000000"
	Tbp$ = "5"
	Tbr$ = "1"
	Tbw$ = "90"
	Title$ = q$ + "html" + q$ + " Genealogy Generator"
	Flt% = 0
	CLOSE : OPEN PRGM + ".ini" FOR INPUT AS #1
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		FOR x% = 1 TO LEN(i$)
		'The function "IF x% < LEN(i$)" prevents a null variable.
		IF x% < LEN(i$) THEN
			SELECT CASE UCASE$(LEFT$(i$, x%))
				CASE "PADDING=": Tbp$ = MID$(i$, x% + 1): EXIT FOR
				CASE "SHADE=": Shade$ = MID$(i$, x% + 1): EXIT FOR
				CASE "TAB=": Tb% = VAL(MID$(i$, x% + 1)): EXIT FOR
				CASE "TABLEBORDER=": Tbr$ = MID$(i$, x% + 1): EXIT FOR
				CASE "TABLEBORDERCOLOR=": Tbc$ = MID$(i$, x% + 1): EXIT FOR
				CASE "TABLEWIDTH=": Tbw$ = MID$(i$, x% + 1): EXIT FOR
			END SELECT
		END IF
		NEXT
		WEND
	ELSE
		CLOSE : OPEN PRGM + ".ini" FOR OUTPUT AS #1
		PRINT #1, "PADDING="; Tbp$
		PRINT #1, "SHADE="; Shade$
		PRINT #1, "TAB="; MID$(STR$(Tb%), 2)
		PRINT #1, "TABLEBORDER="; Tbr$
		PRINT #1, "TABLEBORDERCOLOR="; Tbc$
		PRINT #1, "TABLEWIDTH="; Tbw$
	END IF
	CLOSE
	IF Tb% > 12 THEN Tb% = 12
	IF LEFT$(Tbc$, 1) <> "#" THEN Tbc$ = "#" + Tbc$
END SUB

SUB Launch (k$)
	DO
	IF Family$ = "" OR Directory$ = "" THEN
		EXIT DO
	ELSE
		SELECT CASE UCASE$(k$)
			CASE CHR$(9): SHELL "notepad.exe " + Dbse$ + Directory$ + "\family.txt"
			CASE CHR$(13): Generate
			CASE CHR$(0) + CHR$(15): SHELL "notepad.exe " + Dbse$ + Directory$ + "\document.txt": RUN BOOT
			CASE ELSE: EXIT DO
		END SELECT
		RUN BOOT
	END IF
	LOOP
END SUB

SUB Main
	DIM a%, e%, k$, p%, s%
	Mask
	CLOSE : OPEN Bfr$ FOR RANDOM AS #1 LEN = MXL + 2
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
		CASE CHR$(0) + "G": p% = 1: s% = 1: EXIT DO
		CASE CHR$(0) + "H": p% = p% - 1: EXIT DO
		CASE CHR$(0) + "K": p% = p% - 1: EXIT DO
		CASE CHR$(0) + "M": p% = p% + 1: EXIT DO
		CASE CHR$(0) + "O": p% = 1: s% = e%: EXIT DO
		CASE CHR$(0) + "P": p% = p% + 1: EXIT DO
		CASE ELSE: Launch k$: EXIT DO
	END SELECT
	LOOP
	LOOP
END SUB

SUB Mask
	DIM x%
	CLS
	COLOR Fgc%, Bgc%
	LOCATE 1, 1, 0
	FOR x% = 1 TO 23
	SELECT CASE x%
		CASE 1: PRINT "É"; STRING$(11, 205); "Ñ"; STRING$(54, 205); "Ñ"; STRING$(11, 205); "»"
		CASE 2: PRINT "º"; STRING$(11, 32); "³"; STRING$(54, 32); "³"; STRING$(11, 32); "º"
		CASE 3: PRINT "Ç"; STRING$(11, 196); "Á"; STRING$(54, 196); "Á"; STRING$(11, 196); "¶"
		CASE Echo% - 1: PRINT "Ç"; STRING$(78, 196); "¶"
		CASE 23: PRINT "È"; STRING$(78, 205); "¼"
		CASE ELSE: PRINT "º"; STRING$(78, 32); "º"
	END SELECT
	NEXT
	COLOR 11, Bgc%
	LOCATE 2, 3: PRINT "Directory"
	LOCATE 2, 40 - (LEN(Title$) / 2): PRINT Title$
	LOCATE 2, 73: PRINT "Family"
	SayIt "F1: Edit " + q$ + LCASE$(PRGM) + ".ini" + q$ + " ù ESC to End", Echo% + 4, Fgc%, Bgc%
END SUB

SUB SayIt (i$, v%, f%, b%)
	DIM e$, l$, r$, w%
	w% = 38
	IF i$ = "" THEN
		e$ = ""
	ELSE
		e$ = " ù "
	END IF
	l$ = LEFT$(i$, LEN(i$) / 2)
	r$ = MID$(i$, LEN(l$) + 1)
	l$ = RIGHT$(STRING$(w%, 32) + RIGHT$(e$, 2) + l$, w%)
	r$ = LEFT$(r$ + LEFT$(e$, 2) + STRING$(w%, 32), w%)
	SELECT CASE v%
		CASE 2 TO 22
			COLOR f%, b%
			LOCATE v%, 3
			PRINT l$; r$
			COLOR Fgc%, Bgc%
	END SELECT
END SUB

SUB Scroll (s%, e%, p%)
	DIM b%, f%, i$, j$, o$, x%
	Directory$ = ""
	FOR x% = 1 TO WSZ
	SELECT CASE s% + x% - 1
		CASE 1 TO e%: GET #1, s% + x% - 1, Dln: o$ = CHR$(46)
		CASE e% + 1: Dln = STRING$(MXL, 32): o$ = CHR$(205)
		CASE ELSE: Dln = STRING$(MXL, 32): o$ = CHR$(32)
	END SELECT
	i$ = RTRIM$(LEFT$(Dln, 8))
	j$ = LTRIM$(MID$(Dln, 9))
	SELECT CASE UCASE$(LEFT$(i$, 1))
		CASE "0" TO "9", "A" TO "Z"
			f% = 10
			b% = Bgc%
			IF x% = p% THEN
				f% = 15
				b% = 13
				Directory$ = i$
				Family$ = j$
			END IF
		CASE ELSE: f% = Fgc%: b% = Bgc%
	END SELECT
	IF x% = p% THEN f% = 15: b% = 13
	SayIt LEFT$(i$ + STRING$(MXL, o$), MXL - LEN(j$)) + j$, x% + 3, f%, b%
	NEXT
	IF Directory$ = "" THEN
		SayIt "Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Lines to a Selection", Echo%, 14, Bgc%
		SayIt "No Family Selected", Echo% + 1, 14, Bgc%
		SayIt "No Family Records to Edit", Echo% + 2, 14, Bgc%
		SayIt "No Family Documents to Edit", Echo% + 3, 14, Bgc%
	ELSE
		SayIt "Scroll(" + CHR$(27) + CHR$(24) + CHR$(25) + CHR$(26) + ") Lines to a Selection", Echo%, 10, Bgc%
		SayIt "Press ENTER to Generate the " + q$ + Family$ + q$ + " Genealogy", Echo% + 1, 10, Bgc%
		SayIt "Press TAB to Edit the Family Records", Echo% + 2, 10, Bgc%
		SayIt "Press SHIFT/TAB to Edit the Documents", Echo% + 3, 10, Bgc%
	END IF
END SUB

SUB Swipe (f%, b%)
	DIM x%, y%
	FOR x% = Echo% TO 22
	SayIt "", x%, f%, b%
	NEXT
END SUB

SUB Table (n%, b$, c$, p$, s$, w$)
	IF b$ = "" THEN b$ = "0"
	IF p$ = "" THEN p$ = "0"
	IF s$ = "" THEN s$ = "0"
	IF w$ = "" THEN w$ = "100"
	PRINT #n%, "<table";
	PRINT #n%, " align="; q$; "center"; q$;
	PRINT #n%, " border="; q$; b$; q$;
	IF c$ = "" THEN
		c$ = ""
	ELSE
		PRINT #n%, " bordercolor="; q$; c$; q$;
	END IF
	PRINT #n%, " cellpadding="; q$; p$; q$;
	PRINT #n%, " cellspacing="; q$; s$; q$;
	PRINT #n%, " width="; q$; w$; "%"; q$; ">"
END SUB

SUB Td (n%, a$, b$, c$, s$, t$, i$)
	SELECT CASE a$
		CASE "<": a$ = "left"
		CASE ">": a$ = "right"
		CASE ELSE: a$ = "center"
	END SELECT
	IF s$ = "" OR VAL(s$) < 0 OR VAL(s$) > 100 THEN s$ = "100"
	IF LEFT$(t$, 1) = "<" THEN PRINT #n%, "<tr>";
	PRINT #n%, "<td";
	PRINT #n%, " align="; q$; a$; q$;
	IF b$ = "" THEN
		b$ = ""
	ELSE
		PRINT #n%, " bgcolor="; q$; b$; q$;
	END IF
	IF LEN(c$) > 1 THEN
		IF LEFT$(c$, 1) = "." THEN
			PRINT #n%, " class="; q$; MID$(c$, 2); q$;
		ELSEIF LEFT$(c$, 1) = "#" THEN
			PRINT #n%, " id="; q$; MID$(c$, 2); q$;
		ELSE
			PRINT #n%, " class="; q$; c$; q$;
		END IF
	END IF
	PRINT #n%, " colspan="; q$; s$; q$;
	PRINT #n%, " valign="; q$; "top"; q$;
	PRINT #n%, " width="; q$; s$ + "%"; q$; ">";
	IF LEN(i$) > 0 THEN
		PRINT #n%, i$; "</td>";
		IF RIGHT$(t$, 1) = ">" THEN
			PRINT #n%, "</tr>"
		ELSE
			PRINT #n%, ""
		END IF
	ELSE
		PRINT #n%, ""
	END IF
END SUB

SUB ValidateDocument
	DIM c%, f%, i$, l%, p%, x%
	f% = 0
	c% = 0
	Flt% = 0
	CLOSE : OPEN Dbse$ + Directory$ + "\document.txt" FOR INPUT AS #1
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		l% = l% + 1
		IF UCASE$(LEFT$(i$, 8)) = "[FAMILY]" THEN
			f% = 1
		ELSEIF UCASE$(LEFT$(i$, 5)) = "[DOC]" THEN
			c% = c% + 1
		ELSEIF UCASE$(LEFT$(i$, 5)) = "[EOD]" THEN
			c% = c% - 1
		ELSEIF LCASE$(LEFT$(i$, 2)) = "<p" THEN
			p% = p% + 1
		ELSEIF LCASE$(LEFT$(i$, 3)) = "</p" THEN
			p% = p% - 1
		END IF
		WEND
		CLOSE
		DO
		IF f% = 1 AND c% = 0 AND p% = 0 THEN
			EXIT DO
		ELSE
			Swipe 15, 4
			SayIt "Document Tag Error", Echo%, 15, 4
			IF f% = 0 THEN
				SayIt "Cannot Find Family Tag [FAMILY]", Echo% + 1, 15, 4
			ELSEIF c% < 0 THEN
				SayIt "Imbalance: [DOC] < [EOD]", Echo% + 1, 15, 4
			ELSEIF c% > 0 THEN
				SayIt "Imbalance: [DOC] > [EOD]", Echo% + 1, 15, 4
			ELSEIF p% < 0 THEN
				SayIt "Imbalance: " + q$ + "<p>" + q$ + " < " + q$ + "</p>" + q$, Echo% + 1, 15, 4
			ELSEIF p% > 0 THEN
				SayIt "Imbalance: " + q$ + "<p>" + q$ + " > " + q$ + "</p>" + q$, Echo% + 1, 15, 4
			END IF
			SayIt "ENTER to Exit", Echo% + 2, 15, 4
			SayIt "TAB to Edit Document", Echo% + 3, 15, 4
			SayIt "ESC to End", Echo% + 4, 15, 4
			DO
			Getkey k$
			IF k$ = CHR$(9) OR k$ = CHR$(0) + CHR$(15) THEN
				SHELL "notepad.exe " + Dbse$ + Directory$ + "\document.txt"
				RUN BOOT
			END IF
			LOOP UNTIL k$ = CHR$(13)
			RUN BOOT
		END IF
		LOOP
	ELSE
		Flt% = 0
		CLOSE
	END IF
END SUB

SUB ValidateFamily
	DIM c%, i$, l%, x%
	l% = 0
	Flt% = 0
	CLOSE : OPEN Dbse$ + Directory$ + "\family.txt" FOR INPUT AS #1
	IF Flt% = 0 THEN
		WHILE NOT EOF(1)
		LINE INPUT #1, i$
		l% = l% + 1
		DO
		SELECT CASE UCASE$(LEFT$(i$, 3))
			CASE "[D]": c% = c% + 1: EXIT DO
			CASE "[E]": c% = c% - 1: EXIT DO
			CASE "[C]", "[F]", "[G]", "[N]", "[S]", "[U]", "[X]": EXIT DO
		END SELECT
		Swipe 15, 4
		SayIt q$ + Family$ + q$, Echo%, 15, 4
		SayIt "Tag Error: " + q$ + LCASE$(LEFT$(i$, 3)) + q$ + " on Line #" + MID$(STR$(l%), 2), Echo% + 1, 15, 4
		SayIt "ENTER to Exit ù TAB to Edit ù ESC to End", 22, 15, 4
		DO
		Getkey k$
		IF k$ = CHR$(9) THEN
			SHELL "notepad.exe " + Dbse$ + Directory$ + "\family.txt"
			RUN BOOT
		END IF
		LOOP UNTIL k$ = CHR$(13)
		RUN BOOT
		LOOP
		WEND
		IF c% <> 0 THEN
			Swipe 15, 4
			IF c% < 0 THEN
				SayIt "There Are More [E] Tags Than [D] Tags", Echo%, 15, 4
			ELSEIF c% > 0 THEN
				SayIt "There Are More [D] Tags Than [E] Tags", Echo%, 15, 4
			END IF
			SayIt "ENTER to Exit ù TAB to Edit ù ESC to End", 22, 15, 4
			DO
			Getkey k$
			IF k$ = CHR$(9) THEN
				SHELL "notepad.exe " + Dbse$ + Directory$ + "\family.txt"
				RUN BOOT
			END IF
			LOOP UNTIL k$ = CHR$(13)
			RUN BOOT
		END IF
	ELSE
		Flt% = 0
	END IF
	CLOSE
END SUB

