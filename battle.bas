'$DEBUG
OPTION _EXPLICIT
'$include:'sharedvariables.bi'

DIM yn$, c$, turn AS INTEGER

_TITLE "Battleships"
SCREEN 9
Init

SCREEN _NEWIMAGE(640, 680, 9), , ,
_SCREENMOVE _MIDDLE
Update
PlaceShips

SendInit myname$
'LOCATE 24, 1
PRINT "Waiting for Partner ...";
c$ = GetCommand$
PRINT ""
turn = GetX(c$) <> 0
opponentName$ = GetMessage$(c$)

DO WHILE NOT endOfGame%
    Update
    IF turn THEN
        TakeTurn
    ELSE
        GetTurn
    END IF
    turn = NOT turn
LOOP

'Reverse the last turn (switch)
turn = NOT turn

VIEW PRINT
CLS
IF turn = TRUE THEN
    LOCATE 10, 29
    COLOR 14
    PRINT "Congratulations!!"
    LOCATE 11, 29
    PRINT "You have Won"
ELSE
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

SUB GetTurn
    DIM cmd$, x%, y%, strike AS INTEGER
    DIM message$

    PRINT "Waiting for missile to be fired ...";
    cmd$ = GetCommand$
    PRINT ""

    x% = GetX%(cmd$)
    y% = GetY%(cmd$)
    IF mygrid$(x%, y%) = "" THEN
        SendResult (0)
        mygrid$(x%, y%) = "x"
    ELSE
        strike = shipIndex%(mygrid$(x%, y%))
        SendResult (strike)
        mygrid$(x%, y%) = "X" + mygrid$(x%, y%)
        shiphits(strike) = shiphits(strike) - 1
    END IF
    '    message$ = GetMessage$(cmd$)
    '   COLOR 14
    '  PRINT message$
    ' COLOR 15
END SUB

SUB Grid (isTop%)
    DIM x AS INTEGER, y AS INTEGER, n AS INTEGER

    DIM LeftMargin%, TopMargin%, cellHeight%, cellWidth%, bottom%, right%
    DIM colour%

    LeftMargin% = 30

    IF isTop% THEN
        TopMargin% = 10
        colour% = 9
    ELSE
        TopMargin% = 319
        colour% = 5
    END IF

    cellHeight% = 27
    cellWidth% = 32

    bottom% = (10 * cellHeight% + TopMargin%)
    right% = (18 * cellWidth% + LeftMargin%)

    FOR x = 0 TO 18
        LINE (x * cellWidth% + LeftMargin%, TopMargin%)-(x * cellWidth% + LeftMargin%, bottom%), colour%
    NEXT
    FOR y = 0 TO 10
        LINE (LeftMargin%, y * cellHeight% + TopMargin%)-(right%, y * cellHeight% + TopMargin%), colour%
    NEXT

    'Draw Axes labels
    COLOR 14
    FOR n = 9 TO 0 STEP -1
        LOCATE n * 2 + 2 + (22 * isTop% * -1), 1
        PRINT 9 - n
    NEXT
    LOCATE 22, 5
    FOR n = 0 TO 17
        IF n < 10 THEN PRINT n; " ";
        IF n >= 10 THEN PRINT n;
    NEXT n
END SUB

SUB Init
    DIM host$, yn$

    AutoPlaceShips = FALSE

    CLS
    'Input name
    'input host

    'COLOR 14
    'PRINT "Play against computer? (Y/N)";
    'COLOR 15
    'yn$ = INKEY$
    'IF UCASE$(yn$) = "N" THEN
    '    NetworkGame = FALSE
    'ELSE
    '    NetworkGame = TRUE
    COLOR 14
    PRINT "Enter host server/ip: (Empty will use default)";
    COLOR 15
    INPUT ; ""; host$
    IF host$ = "" THEN host$ = DEFAULT_HOST

    client& = _OPENCLIENT("TCP/IP:" + STR$(HOST_PORT) + ":" + host$)
    IF client& = 0 THEN
        PRINT "Client can't connect"
        END
    END IF

    COLOR 14
    PRINT "Please give me a name (so they know who to send condolences too)";
    COLOR 15
    INPUT ; myname$
    '  END IF

    COLOR 14
    PRINT "Auto place ships (Y/N)?";
    COLOR 15
    INPUT ; yn$
    IF UCASE$(yn$) = "Y" THEN AutoPlaceShips = TRUE

END SUB

SUB TakeTurn
    DIM cmd$, x%, y%, mess$, strike1 AS INTEGER
    DIM shipCode$, shipNum%

    DO: DO
            CLS 2
            PRINT "Enter Coordinates to Fire At (x,y) :";
            INPUT ; "", x%
            INPUT ; ",", y%
            PRINT
        LOOP UNTIL x% < 18 AND x% > -1 AND y% < 10 AND y% > -1
    LOOP WHILE hisgrid$(x%, y%) <> ""

    '    LINE INPUT ; "Enter Message :"; mess$
    '   IF LEN(mess$) > 20 THEN mess$ = LEFT$(mess$, 20)
    '  PRINT ""
    mess$ = ""
    'send (x%): send (y%)
    'strike1 = recieve
    'sendstr mess$
    SendCommand x%, y%, mess$
    cmd$ = GetCommand$
    strike1 = GetY%(cmd$)

    IF strike1 = 0 THEN
        hisgrid$(x%, y%) = "x"
    ELSE
        hisshiphits(strike1) = hisshiphits(strike1) - 1
        shipCode$ = MID$(SHIPLIST$, strike1, 1)
        shipNum% = strike1 - INSTR(SHIPLIST$, shipCode$) + 1
        hisgrid$(x%, y%) = "X" + shipCode$ + _TRIM$(STR$(shipNum%))
    END IF
END SUB

SUB Update
    DrawGrid mygrid$(), shiphits(), TRUE
    DrawGrid hisgrid$(), hisshiphits(), FALSE
END SUB

SUB DrawGrid (agData$(), agHits%(), topBottom%)
    DIM x%, y%, value$, topOffset%, idx%
    VIEW PRINT
    Grid topBottom%
    IF topBottom% THEN
        topOffset% = 2
    ELSE
        topOffset% = 24
    END IF
    FOR y% = 9 TO 0 STEP -1
        FOR x% = 0 TO 17
            LOCATE ((9 - y%) * 2 + topOffset%), (x% * 4 + 6)
            LET value$ = agData$(x%, y%)
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
            IF topBottom% AND LEN(agData$(x%, y%)) > 0 THEN
                PRINT LEFT$(agData$(x%, y%), 2);
            ELSEIF LEN(agData$(x%, y%)) > 0 THEN
                idx% = shipIndex%(agData$(x%, y%))
                IF idx% = 0 THEN
                    PRINT LEFT$(value$, 1);
                ELSEIF agHits%(idx%) = 0 THEN
                    PRINT LEFT$(value$, 2);
                ELSE
                    PRINT LEFT$(value$, 1);
                END IF
            END IF
        NEXT x%
        PRINT
    NEXT y%
    COLOR 15
    VIEW PRINT 44 TO 48
    PRINT "":
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
                VIEW PRINT
                LOCATE 24, 1
                PRINT "Opponent has disconnected, I suppose therefore you won!"
                END
            END IF
        END IF
        _DELAY 0.02
    LOOP
END FUNCTION

'$include:'library.bi'
