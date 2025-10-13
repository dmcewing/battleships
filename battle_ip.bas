DECLARE SUB center (row!, text$, maxcol!)
DECLARE SUB sendstr (strng$)
DECLARE SUB recvstr ()
DECLARE SUB getturn ()
DECLARE SUB updatehis ()
DECLARE SUB send (byte AS INTEGER)
DECLARE SUB taketurn ()
DECLARE SUB update ()
DECLARE FUNCTION dirpos$ (length!, x%, y%)
DECLARE SUB init ()
DECLARE SUB grid ()
DECLARE FUNCTION recieve% ()
DECLARE SUB position ()

CONST TRUE = -1, false = 0
CONST DEFAULT_HOST = "dmcewinghome.australiaeast.cloudapp.azure.com"
CONST HOST_PORT = 25123

DIM SHARED client&
DIM SHARED mygrid$(17, 9)
DIM SHARED air, bat, cru1, cru2, des1, des2, des3, subs
DIM SHARED hisgrid$(17, 9)
DIM SHARED hair, hbat, hcru1, hcru2, hdes1, hdes2, hdes3, hsub
DIM SHARED endofgame
DIM SHARED opponentName$, myname$

_TITLE "Battle Ships via IP"
init
SCREEN 9
update
position

SendInit myname$
LOCATE 24, 1
PRINT "Waiting for Partner ...";
c$ = GetCommand$
turn = GetX(c$) <> 0
opponentName$ = GetMessage$(c$)

endofgame = false

DO
    IF turn = TRUE THEN
        PALETTE 8, 6
        updatehis
        taketurn
    END IF
    IF turn = false THEN
        PALETTE 8, 7
        update
        getturn
    END IF
    IF endofgame = false THEN
        IF turn = false THEN
            turn = TRUE
        ELSE
            turn = false
        END IF
    END IF

LOOP UNTIL endofgame = TRUE

IF turn = TRUE THEN
    CLS
    LOCATE 10, 29
    COLOR 14
    PRINT "Congratulations!!"
    LOCATE 11, 29
    PRINT "You have Won"
ELSE
    CLS
    LOCATE 10, 25
    COLOR 14
    PRINT "This just isn't your day!!"
    LOCATE 11, 25
    PRINT "You have Lost"
END IF
DO
    PRINT "Play again ";
    INPUT ; "", yn$
LOOP UNTIL INSTR("YN", UCASE$(yn$))

IF UCASE$(yn$) = "Y" THEN RUN
IF client& <> 0 THEN CLOSE #client&
END

SUB center (row, text$, maxcol)
    col = maxcol \ 2
    colas = col - (LEN(text$) / 2 + .5)
    IF colas < 1 THEN colas = 1
    LOCATE row, colas
    PRINT text$;
END SUB

FUNCTION dirpos$ (length, x%, y%)
    length = length - 1
    IF y% + length < 10 THEN north = TRUE
    IF y% - length > -1 THEN south = TRUE
    IF x% + length < 17 THEN east = TRUE
    IF x% - length > -1 THEN west = TRUE
    FOR n = 1 TO length
        IF north = TRUE THEN IF mygrid$(x%, y% + n) <> "" THEN north = false
        IF south = TRUE THEN IF mygrid$(x%, y% - n) <> "" THEN south = false
        IF east = TRUE THEN IF mygrid$(x% + n, y%) <> "" THEN east = false
        IF west = TRUE THEN IF mygrid$(x% - n, y%) <> "" THEN west = false
    NEXT
    IF north = TRUE THEN e$ = "N|"
    IF south = TRUE THEN e$ = e$ + "S|"
    IF east = TRUE THEN e$ = e$ + "E|"
    IF west = TRUE THEN e$ = e$ + "W|"
    FOR n = 2 TO 8 STEP 2
        IF LEN(e$) = n THEN e$ = LEFT$(e$, n - 1)
    NEXT n
    dirpos$ = e$
END FUNCTION

SUB getturn
    DIM cmd$

    LOCATE 24, 1: PRINT "Waiting for missile to be fired ...";
    cmd$ = GetCommand$

    x% = GetX%(cmd$)
    y% = GetY%(cmd$)
    IF mygrid$(x%, y%) = "" THEN SendResult (0): mygrid$(x%, y%) = "x"
    IF mygrid$(x%, y%) = "B" THEN SendResult (1): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): bat = bat + 1
    IF mygrid$(x%, y%) = "C1" THEN SendResult (2): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): cru1 = cru1 + 1
    IF mygrid$(x%, y%) = "C2" THEN SendResult (3): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): cru2 = cru2 + 1
    IF mygrid$(x%, y%) = "D1" THEN SendResult (4): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): des1 = des1 + 1
    IF mygrid$(x%, y%) = "D2" THEN SendResult (5): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): des2 = des2 + 1
    IF mygrid$(x%, y%) = "D3" THEN SendResult (6): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): des3 = des3 + 1
    IF LEFT$(mygrid$(x%, y%), 1) = "S" THEN SendResult (7): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): subs = subs + 1
    IF mygrid$(x%, y%) = "A" THEN SendResult (8): mygrid$(x%, y%) = "X" + mygrid$(x%, y%): air = air + 1
    CLS: update
    'recvstr
    message$ = GetMessage$(cmd$)
    center 23, message$, 80
    COLOR 15
    LOCATE 24, 1: PRINT "Press any key to continue...";
    DO: LOOP UNTIL INKEY$ = "" 'clear buffer
    DO: LOOP UNTIL INKEY$ <> "" 'Wait for key
    IF air = 6 AND bat = 5 AND cru1 = 4 AND cru2 = 4 AND des1 = 3 AND des2 = 3 AND des3 = 3 AND subs = 4 THEN endofgame = TRUE
END SUB

SUB grid
    CLS
    FOR x = 0 TO 18
        LINE (x * 32 + 30, 10)-(x * 32 + 30, 280), 8
    NEXT
    FOR y = 0 TO 10
        LINE (30, y * 27 + 10)-(606, y * 27 + 10), 8
    NEXT
    COLOR 14
    FOR n = 9 TO 0 STEP -1
        LOCATE n * 2 + 2, 1
        PRINT 9 - n
    NEXT
    LOCATE 22, 5
    FOR n = 0 TO 17
        IF n < 10 THEN PRINT n; " ";
        IF n >= 10 THEN PRINT n;
    NEXT n
END SUB

SUB init
    DEFINT A-Z
    CLS
    'Input name
    'input host
    PRINT "Enter host server/ip: (Empty will use default)";
    INPUT ; ""; host$
    IF host$ = "" THEN host$ = DEFAULT_HOST

    client& = _OPENCLIENT("TCP/IP:" + STR$(HOST_PORT) + ":" + host$)
    IF client& = 0 THEN
        PRINT "Client can't connect"
        END
    END IF

    PRINT "Please give me a name (so they know who to send condolences too)";
    INPUT ; myname$

END SUB

DEFSNG A-Z
SUB position
    DO: DO
            LOCATE 24, 1
            PRINT "Enter Position for Aircraft Carrier (x <ENTER> y) :";
            INPUT ; "", x%
            INPUT ; ",", y%
            LOCATE 24, 1
            PRINT STRING$(60, " ");
        LOOP WHILE x% < 0 OR x% > 17 OR y% < 0 OR y% > 9
    LOOP UNTIL mygrid$(x%, y%) = ""
    DO
        LOCATE 24, 1
        d$ = dirpos(6, x%, y%)
        DO
            PRINT "Enter direction of ship ("; d$; ") :";
            INPUT ; "", e$
            LOCATE 24, 1: PRINT STRING$(60, " ");
        LOOP UNTIL e$ <> ""
    LOOP WHILE INSTR(d$, UCASE$(e$)) = 0
    FOR n = 0 TO 5
        IF UCASE$(e$) = "N" THEN mygrid$(x%, y% + n) = "A"
        IF UCASE$(e$) = "S" THEN mygrid$(x%, y% - n) = "A"
        IF UCASE$(e$) = "E" THEN mygrid$(x% + n, y%) = "A"
        IF UCASE$(e$) = "W" THEN mygrid$(x% - n, y%) = "A"
    NEXT
    update
    DO: DO
            LOCATE 24, 1
            PRINT "Enter Position for Battleship (x <ENTER> y) :";
            INPUT ; "", x%
            INPUT ; ",", y%
            LOCATE 24, 1
            PRINT STRING$(60, " ");
        LOOP WHILE x% < 0 OR x% > 17 OR y% < 0 OR y% > 9
    LOOP UNTIL mygrid$(x%, y%) = ""
    DO
        LOCATE 24, 1
        d$ = dirpos(5, x%, y%)
        IF LEN(d$) = 1 THEN e$ = d$: EXIT DO
        DO
            PRINT "Enter direction of ship ("; d$; ") :";
            INPUT ; "", e$
            LOCATE 24, 1: PRINT STRING$(60, " ");
        LOOP UNTIL e$ <> ""
    LOOP WHILE INSTR(d$, UCASE$(e$)) = 0
    FOR n = 0 TO 4
        IF UCASE$(e$) = "N" THEN mygrid$(x%, y% + n) = "B"
        IF UCASE$(e$) = "S" THEN mygrid$(x%, y% - n) = "B"
        IF UCASE$(e$) = "E" THEN mygrid$(x% + n, y%) = "B"
        IF UCASE$(e$) = "W" THEN mygrid$(x% - n, y%) = "B"
    NEXT
    update
    FOR ships = 1 TO 2
        DO: DO
                LOCATE 24, 1
                PRINT "Enter Position for Crusier Ship #"; ships; " (x <ENTER> y) :";
                INPUT ; "", x%
                INPUT ; ",", y%
                LOCATE 24, 1
                PRINT STRING$(60, " ");
            LOOP WHILE x% < 0 OR x% > 17 OR y% < 0 OR y% > 9
        LOOP UNTIL mygrid$(x%, y%) = ""
        DO
            LOCATE 24, 1
            d$ = dirpos(4, x%, y%)
            IF LEN(d$) = 1 THEN e$ = d$: EXIT DO
            DO
                PRINT "Enter direction of ship ("; d$; ") :";
                INPUT ; "", e$
                LOCATE 24, 1: PRINT STRING$(60, " ");
            LOOP UNTIL e$ <> ""
        LOOP WHILE INSTR(d$, UCASE$(e$)) = 0
        FOR n = 0 TO 3
            IF UCASE$(e$) = "N" THEN mygrid$(x%, y% + n) = "C" + LTRIM$(STR$(ships))
            IF UCASE$(e$) = "S" THEN mygrid$(x%, y% - n) = "C" + LTRIM$(STR$(ships))
            IF UCASE$(e$) = "E" THEN mygrid$(x% + n, y%) = "C" + LTRIM$(STR$(ships))
            IF UCASE$(e$) = "W" THEN mygrid$(x% - n, y%) = "C" + LTRIM$(STR$(ships))
        NEXT
        update
    NEXT ships
    FOR ships = 1 TO 3
        DO: DO
                LOCATE 24, 1
                PRINT "Enter Position for Destroyer Ship #"; ships; " (x <ENTER> y) :";
                INPUT ; "", x%
                INPUT ; ",", y%
                LOCATE 24, 1
                PRINT STRING$(60, " ");
            LOOP WHILE x% < 0 OR x% > 17 OR y% < 0 OR y% > 9
        LOOP UNTIL mygrid$(x%, y%) = ""
        DO
            LOCATE 24, 1
            d$ = dirpos(3, x%, y%)
            IF LEN(d$) = 1 THEN e$ = d$: EXIT DO
            DO
                PRINT "Enter direction of ship ("; d$; ") :";
                INPUT ; "", e$
                LOCATE 24, 1: PRINT STRING$(60, " ");
            LOOP UNTIL e$ <> ""
        LOOP WHILE INSTR(d$, UCASE$(e$)) = 0
        FOR n = 0 TO 2
            IF UCASE$(e$) = "N" THEN mygrid$(x%, y% + n) = "D" + LTRIM$(STR$(ships))
            IF UCASE$(e$) = "S" THEN mygrid$(x%, y% - n) = "D" + LTRIM$(STR$(ships))
            IF UCASE$(e$) = "E" THEN mygrid$(x% + n, y%) = "D" + LTRIM$(STR$(ships))
            IF UCASE$(e$) = "W" THEN mygrid$(x% - n, y%) = "D" + LTRIM$(STR$(ships))
        NEXT
        update
    NEXT ships
    FOR ships = 1 TO 4
        DO: DO
                LOCATE 24, 1
                PRINT "Enter Position for Submarine Ship #"; ships; " (x <ENTER> y) :";
                INPUT ; "", x%
                INPUT ; ",", y%
                LOCATE 24, 1
                PRINT STRING$(60, " ");
            LOOP UNTIL x% > -1 AND x% < 18 AND y% > -1 AND y% < 10
        LOOP WHILE mygrid$(x%, y%) <> ""
        mygrid$(x%, y%) = "S" + LTRIM$(STR$(ships))
        update
    NEXT ships
END SUB

SUB taketurn
    DIM cmd$

    DO: DO
            LOCATE 24, 1
            PRINT "Enter Coordinates to Fire At (x,y) :";
            INPUT ; "", x%
            INPUT ; ",", y%
            LOCATE 24, 1: PRINT SPACE$(60);
        LOOP UNTIL x% < 18 AND x% > -1 AND y% < 10 AND y% > -1
    LOOP WHILE hisgrid$(x%, y%) <> ""
    LOCATE 24, 1
    LINE INPUT ; "Enter Message :"; mess$
    'send (x%): send (y%)
    'strike1 = recieve
    'sendstr mess$
    SendCommand x%, y%, mess$
    cmd$ = GetCommand$
    strike1 = GetY%(cmd$)
    LOCATE 24, 1: PRINT SPACE$(60);
    IF strike1 = 0 THEN hisgrid$(x%, y%) = "x"
    IF strike1 = 1 THEN hbat = hbat + 1: hisgrid$(x%, y%) = "XB"
    IF strike1 = 2 THEN hcru1 = hcru1 + 1: hisgrid$(x%, y%) = "XC1"
    IF strike1 = 3 THEN hcru2 = hcru2 + 1: hisgrid$(x%, y%) = "XC2"
    IF strike1 = 4 THEN hdes1 = hdes1 + 1: hisgrid$(x%, y%) = "XD1"
    IF strike1 = 5 THEN hdes2 = hdes2 + 1: hisgrid$(x%, y%) = "XD2"
    IF strike1 = 6 THEN hdes3 = hdes3 + 1: hisgrid$(x%, y%) = "XD3"
    IF strike1 = 7 THEN hsub = hsub + 1: hisgrid$(x%, y%) = "XS"
    IF strike1 = 8 THEN hair = hair + 1: hisgrid$(x%, y%) = "XA"

    updatehis
    COLOR 15
    LOCATE 24, 1: PRINT "Press any key to continue...";
    DO: LOOP UNTIL INKEY$ = "" 'clear buffer
    DO: LOOP UNTIL INKEY$ <> "" 'Wait for key
    IF hair = 6 AND hbat = 5 AND hcru1 = 4 AND hcru2 = 4 AND hdes1 = 3 AND hdes2 = 3 AND hdes3 = 3 AND hsub = 4 THEN endofgame = TRUE

END SUB

SUB update
    CLS
    grid
    FOR y% = 9 TO 0 STEP -1
        FOR x% = 0 TO 17
            LOCATE ((9 - y%) * 2 + 2), (x% * 4 + 6)
            LET value$ = mygrid$(x%, y%)
            SELECT CASE LEFT$(value$, 1)
                CASE "A"
                    COLOR 15
                CASE "B"
                    COLOR 10
                CASE "D"
                    COLOR 11
                CASE "C"
                    COLOR 14
                CASE "S"
                    COLOR 13
                CASE "X"
                    COLOR 12
                CASE "x"
                    COLOR 15
            END SELECT
            PRINT LEFT$(mygrid$(x%, y%), 2);
        NEXT x%
        PRINT
    NEXT y%
    COLOR 15

END SUB

SUB updatehis
    CLS
    grid
    FOR y% = 9 TO 0 STEP -1
        FOR x% = 0 TO 17
            LOCATE ((9 - y%) * 2 + 2), (x% * 4 + 6)
            LET value$ = hisgrid$(x%, y%)
            SELECT CASE LEFT$(value$, 1)
                CASE "x"
                    COLOR 15
                CASE "X"
                    COLOR 12
            END SELECT
            LET reveal = false
            SELECT CASE MID$(value$, 2)
                CASE "A"
                    IF hair = 6 THEN reveal = TRUE
                CASE "B"
                    IF hbat = 5 THEN reveal = TRUE
                CASE "C1"
                    IF hcru1 = 4 THEN reveal = TRUE
                CASE "C2"
                    IF hcru2 = 4 THEN reveal = TRUE
                CASE "D1"
                    IF hdes1 = 3 THEN reveal = TRUE
                CASE "D2"
                    IF hdes2 = 3 THEN reveal = TRUE
                CASE "D3"
                    IF hdes3 = 3 THEN reveal = TRUE
                CASE "S"
                    reveal = TRUE
            END SELECT
            IF reveal = TRUE THEN
                PRINT MID$(value$, 2, 1);
            ELSE
                PRINT LEFT$(value$, 1)
            END IF
        NEXT x%
        PRINT
    NEXT y%
    COLOR 15

END SUB



FUNCTION GetCommand$
    DIM temp$
    GetCommand$ = ""

    temp$ = ""
    DO UNTIL EOF(client&) OR LEN(temp$) <> 0
        GET #client&, , temp$
        IF NOT (EOF(client&)) AND LEN(temp$) > 0 THEN
            GetCommand$ = temp$
            IF temp$ = "ENDGAME" THEN
                LOCATE 24, 1
                PRINT "Opponent has disconnected, I suppose therefore you won!"
                END
            END IF
        END IF
        _DELAY 0.02
    LOOP
END FUNCTION

SUB SendCommand (x%, y%, message$)
    DIM s$

    s$ = "C"
    s$ = s$ + RIGHT$("00" + RTRIM$(LTRIM$(STR$(x%))), 2)
    s$ = s$ + RIGHT$("00" + RTRIM$(LTRIM$(STR$(y%))), 2)
    s$ = s$ + message$

    PUT #client&, , s$

END SUB

SUB SendResult (tye%)
    DIM s$
    s$ = "R00" + RIGHT$("00" + LTRIM$(RTRIM$(STR$(tye%))), 2)
    PUT #client&, , s$

END SUB

SUB SendInit (me$)
    DIM s$
    s$ = "G0055" + me$
    PUT #client&, , s$
END SUB

FUNCTION GetY% (cmd$)
    GetY% = CINT(VAL(MID$(cmd$, 4, 2)))
END FUNCTION

FUNCTION GetX% (cmd$)
    GetX% = CINT(VAL(MID$(cmd$, 2, 2)))
END FUNCTION

FUNCTION GetMessage$ (cmd$)
    GetMessage$ = MID$(cmd$, 6)
END FUNCTION
