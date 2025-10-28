'$DEBUG
$CONSOLE
$SCREENHIDE
$COLOR:0
'$include:'sharedvariables.bi'

DIM turn AS INTEGER, forcedEnd AS INTEGER, shipcount AS INTEGER, shipId$

DIM SHARED hosth&
DIM SHARED ShipSizes(1 TO LEN(SHIPLIST$))
DIM SHARED heatmap&
DIM SHARED level%

DIM SHARED probabilityMap(GRIDCOL%, GRIDROW%) AS INTEGER

CONST LeftMargin% = 30
CONST cellHeight% = 27
CONST cellWidth% = 32

RANDOMIZE TIMER

'Process command arguments
level% = 3
count = _COMMANDCOUNT
FOR c = 1 TO count
    param$ = UCASE$(LEFT$(COMMAND$(c), 1))
    SELECT CASE param$
        CASE "H": level% = 3
        CASE "E": level% = 0
    END SELECT
NEXT

FOR shipcount = LBOUND(ShipSizes) TO UBOUND(ShipSizes)
    shipId$ = MID$(SHIPLIST$, shipcount, 1)
    SELECT CASE shipId$
        CASE "A": ShipSizes(shipcount) = AIRCRAFT_SIZE
        CASE "B": ShipSizes(shipcount) = BATTLESHIP_SIZE
        CASE "C": ShipSizes(shipcount) = CRUISER_SIZE
        CASE "D": ShipSizes(shipcount) = DESTROYER_SIZE
        CASE "S": ShipSizes(shipcount) = SUBMARINE_SIZE
    END SELECT
NEXT

'Produce HeatMap display
heatmap& = _NEWIMAGE(640, 320, 32)
IF level% >= 2 THEN
    _SCREENSHOW
    SCREEN heatmap&
    _TITLE "Battlebot: Heatmap"

    DrawHeatMap

END IF
_DEST _CONSOLE

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
    LOOP UNTIL endOfGame% OR forcedEnd
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

    _DELAY CSNG(RND * 2)

    IF level% = 3 THEN
        GuessProb x%, y%
    END IF

    'This is due to early bugs, it just prevents a repeat on a same point, which triggers a client error.
    ' but is the "EASY" implementation.
    IF LEN(hisgrid$(x%, y%)) > 0 THEN
        DO: DO
                x% = INT(RND * 18)
                y% = INT(RND * 10)
            LOOP UNTIL x% < 18 AND x% > -1 AND y% < 10 AND y% > -1
        LOOP WHILE hisgrid$(x%, y%) <> ""
    END IF
    COLOR Yellow
    PRINT "Firing at", x%, y%

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

    DrawHeatMap

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
    END
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

SUB Grid (isTop%)
    DIM x AS INTEGER, y AS INTEGER, n AS INTEGER

    DIM TopMargin%, bottom%, right%
    DIM colour%
    TopMargin% = 10
    colour% = 15

    bottom% = (10 * cellHeight% + TopMargin%)
    right% = (18 * cellWidth% + LeftMargin%)

    FOR x = 0 TO 18
        LINE (x * cellWidth% + LeftMargin%, TopMargin%)-(x * cellWidth% + LeftMargin%, bottom%), colour%
    NEXT
    FOR y = 0 TO 10
        LINE (LeftMargin%, y * cellHeight% + TopMargin%)-(right%, y * cellHeight% + TopMargin%), colour%
    NEXT

    'Draw Axes labels
    'COLOR 14
    'FOR n = 9 TO 0 STEP -1
    '    LOCATE n * 2 + 2 + (22 * isTop% * -1), 1
    '    PRINT 9 - n
    'NEXT
    'LOCATE 22, 5
    'FOR n = 0 TO 17
    '    IF n < 10 THEN PRINT n; " ";
    '    IF n >= 10 THEN PRINT n;
    'NEXT n
END SUB

SUB DrawHeatMap
    'Reset the Map Array
    _DEST _CONSOLE
    PRINT "Recalculating Probability..."
    FOR row% = 0 TO GRIDROW%
        FOR col% = 0 TO GRIDCOL%
            probabilityMap(col%, row%) = 0
    NEXT col%, row%
    CalculateProbabilityMap

    _DEST heatmap&
    FOR row% = 0 TO GRIDROW%
        FOR col% = 0 TO GRIDCOL%
            PaintSquare probabilityMap(col%, row%), col%, row%
    NEXT col%, row%
    Grid TRUE
    _DEST _CONSOLE
END SUB

SUB CheckRange (prob%(), startrow, startcol, endrow, endcol)
    allClear% = TRUE
    FOR row = startrow TO endrow
        FOR col = startcol TO endcol
            IF LEN(hisgrid$(col, row)) > 0 THEN
                allClear% = FALSE
                EXIT FOR
            END IF
        NEXT col
        IF NOT allClear% THEN EXIT FOR
    NEXT row

    IF allClear% THEN
        FOR row = startrow TO endrow
            FOR col = startcol TO endcol
                prob%(col, row) = prob%(col, row) + 1
        NEXT col, row
    END IF

END SUB

SUB GuessProb (x AS INTEGER, y AS INTEGER)
    'get the row, col numbers of the largest element in PROB_MAP
    value = 0
    COLOR Yellow
    PRINT "Finding hottest spot..."
    COLOR White

    FOR row% = 0 TO GRIDROW%
        FOR col% = 0 TO GRIDCOL%
            IF probabilityMap(col%, row%) > value THEN
                x = col%
                y = row%
                value = probabilityMap(col%, row%)
            ELSEIF probabilityMap(col%, row%) = value THEN
                IF RND > 0.5 THEN
                    x = col%
                    y = row%
                END IF
            END IF
    NEXT col%, row%

END SUB

SUB PaintSquare (heat%, col%, row%)

    startx% = col% * cellWidth% + LeftMargin%
    starty% = (9 - row%) * cellHeight% + 10
    endx% = (col% + 1) * cellWidth% + LeftMargin%
    endy% = ((9 - row%) + 1) * cellHeight% + 10

    c = _RGB32(heat%)
    IF LEN(hisgrid$(col%, row%)) > 2 THEN
        c = _RGB32(200, 0, 0)
    END IF
    LINE (startx%, starty%)-(endx%, endy%), c, BF

END SUB

SUB CalculateProbabilityMap
    DIM col AS INTEGER, row AS INTEGER
    FOR shipidx% = 1 TO LEN(SHIPLIST$)
        use_size = ShipSizes(shipidx%) - 1
        'check where a ship will fit on the board
        FOR row = 0 TO GRIDROW%
            FOR col = 0 TO GRIDCOL%
                IF LEN(hisgrid$(col, row)) = 0 THEN
                    'get potential ship endpoints
                    IF row - use_size >= 0 THEN
                        CheckRange probabilityMap(), (row - use_size), col, row, col
                    END IF
                    IF row + use_size <= GRIDROW% THEN
                        CheckRange probabilityMap(), row, col, row + use_size, col
                    END IF
                    IF col - use_size >= 0 THEN
                        CheckRange probabilityMap(), row, col - use_size, row, col
                    END IF
                    IF col + use_size <= GRIDCOL% THEN
                        CheckRange probabilityMap(), row, col, row, col + use_size
                    END IF
                END IF
                'increase probability of attacking squares near successful hits
                IF LEN(hisgrid$(col, row)) > 0 THEN 'Shot map = 1
                    IF hisgrid$(col, row) = "x" THEN
                        'decrease probability for misses to 0
                        probabilityMap(col, row) = 0
                    ELSE
                        shipid% = shipIndex%(hisgrid$(col, row))
                        IF hisshiphits(shipid%) > 0 THEN 'Unweight hits on sunk ships
                            IF row + 1 <= GRIDROW% THEN
                                IF LEN(hisgrid$(col, row + 1)) = 0 THEN
                                    probabilityMap(col, row + 1) = probabilityMap(col, row + 1) + 10
                                    IF row - 1 >= 0 THEN
                                        shipid% = shipIndex%(hisgrid$(col, row - 1))
                                        IF shipid% > 0 THEN
                                            IF hisshiphits(shipid%) > 0 THEN
                                                probabilityMap(col, row + 1) = probabilityMap(col, row + 1) + 5
                                            END IF
                                        END IF
                                    END IF
                                END IF
                            END IF

                            IF row - 1 >= 0 THEN
                                IF LEN(hisgrid$(col, row - 1)) = 0 THEN
                                    probabilityMap(col, row - 1) = probabilityMap(col, row - 1) + 10
                                    IF row + 1 <= GRIDROW% THEN
                                        shipid% = shipIndex%(hisgrid$(col, row + 1))
                                        IF (shipid% > 0) THEN
                                            IF hisshiphits(shipid%) > 0 THEN
                                                probabilityMap(col, row - 1) = probabilityMap(col, row - 1) + 5
                                            END IF
                                        END IF
                                    END IF
                                END IF
                            END IF

                            IF col + 1 <= GRIDCOL% THEN
                                IF LEN(hisgrid$(col + 1, row)) = 0 THEN
                                    probabilityMap(col + 1, row) = probabilityMap(col + 1, row) + 10
                                    IF col - 1 >= 0 THEN
                                        shipid% = shipIndex%(hisgrid$(col - 1, row))
                                        IF (shipid% > 0) THEN
                                            IF hisshiphits(shipid%) > 0 THEN
                                                probabilityMap(col + 1, row) = probabilityMap(col + 1, row) + 5
                                            END IF
                                        END IF
                                    END IF
                                END IF
                            END IF

                            IF col - 1 >= 0 THEN
                                IF LEN(hisgrid$(col - 1, row)) = 0 THEN
                                    probabilityMap(col - 1, row) = probabilityMap(col - 1, row) + 10
                                    IF col + 1 <= GRIDCOL% THEN
                                        shipid% = shipIndex%(hisgrid$(col + 1, row))
                                        IF (shipid% > 0) THEN
                                            IF hisshiphits(shipid%) > 0 THEN
                                                probabilityMap(col - 1, row) = probabilityMap(col - 1, row) + 5
                                            END IF
                                        END IF
                                    END IF
                                END IF
                            END IF
                        END IF
                    END IF
                END IF
    NEXT col%, row%, shipidx%

END SUB

'$include:'library.bi'


