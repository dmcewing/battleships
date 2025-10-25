'$DEBUG
$CONSOLE:ONLY
$COLOR:0
'$include:'sharedvariables.bi'

DIM turn AS INTEGER, forcedEnd AS INTEGER

DIM SHARED hosth&

forcedEnd = FALSE
InitMyBoard

COLOR White
PRINT "Opening host: ";
hosth& = _OPENHOST("TCP/IP:" + STR$(HOST_PORT))
IF hosth& = 0 THEN
    COLOR Red
    PRINT "Error opening host: ", ERR
ELSE
    COLOR Green
    PRINT "Started"
    COLOR White
    GameLoop
    ResetNetGame
END IF

SUB InitMyBoard
    AutoPlaceShips = TRUE
    COLOR White
    PRINT "Placing Ships on virtual board...";
    PlaceShips
    COLOR Green
    PRINT "OK"

END SUB

SUB ResetNetGame
    IF client& <> 0 THEN
        CLOSE client&
        PRINT "Client disconnected"
        client& = 0
    END IF

    IF hosth& <> 0 THEN
        CLOSE hosth&
        hosth& = 0
        PRINT "Game hosting ended"
    END IF

END SUB

SUB GameLoop
    DO
        IF client& = 0 THEN
            client& = _OPENCONNECTION(hosth&)
        ELSE
            IF _CONNECTED(client&) = 0 THEN
                ResetClient
            ELSE
                PlayGame
            END IF
        END IF
        _DELAY 0.5
    LOOP
END SUB

SUB PlayGame
    DIM cmd$
    PRINT "Waiting player...";
    DO
        cmd$ = GetCommand$
    LOOP UNTIL UCASE$(LEFT$(cmd$, 1)) = "G"
    m$ = "G0101Computer"
    PUT #client&, , m$
    COLOR Green
    PRINT cmd$
    turn = FALSE

    DO
        IF turn THEN
            TakeTurn
        ELSE
            GetTurn
        END IF
        turn = NOT turn

    LOOP UNTIL endOfGame% OR forcedEnd

END SUB

SUB TakeTurn
    DIM cmd$, x%, y%, mess$, strike1 AS INTEGER
    DIM shipCode$, shipNum%

    _DELAY CSNG(RND * 4)

    DO: DO
            x% = INT(RND * 18)
            y% = INT(RND * 10)
        LOOP UNTIL x% < 18 AND x% > -1 AND y% < 10 AND y% > -1
    LOOP WHILE hisgrid$(x%, y%) <> ""

    mess$ = ""
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

SUB GetTurn
    DIM cmd$, x%, y%, strike AS INTEGER
    DIM message$
    COLOR White
    PRINT "Waiting for missile to be fired ...";
    cmd$ = GetCommand$
    PRINT cmd$, " ";

    x% = GetX%(cmd$)
    y% = GetY%(cmd$)
    IF mygrid$(x%, y%) = "" THEN
        COLOR Green
        PRINT "Miss"
        SendResult (0)
        mygrid$(x%, y%) = "x"
    ELSE
        strike = shipIndex%(mygrid$(x%, y%))
        SendResult (strike)
        mygrid$(x%, y%) = "X" + mygrid$(x%, y%)
        shiphits(strike) = shiphits(strike) - 1
        COLOR Red
        PRINT "Hit ";
        IF shiphits(strike) <= 0 THEN
            COLOR Cyan
            PRINT "Sunk ", strike
        ELSE
            PRINT ""
        END IF
    END IF
END SUB

SUB Update
    'Do nothing for robot
END SUB

SUB ResetClient
    PRINT "Client has disconnected."
    pair = client.pair
    CLOSE #client&
    client& = 0
    RUN
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
                forcedEnd% = TRUE
            END IF
        END IF
        _DELAY 0.02
    LOOP
END FUNCTION

'$include:'library.bi'
