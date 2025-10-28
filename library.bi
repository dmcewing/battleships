FUNCTION DirPos$ (length, x%, y%)
    DIM north, south, east, west, n AS INTEGER, e$

    length = length - 1
    ' Check if the end of the boat is still on the grid.
    IF y% + length < 10 THEN north = TRUE
    IF y% - length > -1 THEN south = TRUE
    IF x% + length < 17 THEN east = TRUE
    IF x% - length > -1 THEN west = TRUE

    ' Check if there is another boat in the way
    FOR n = 1 TO length
        IF north = TRUE THEN IF mygrid$(x%, y% + n) <> "" THEN north = FALSE
        IF south = TRUE THEN IF mygrid$(x%, y% - n) <> "" THEN south = FALSE
        IF east = TRUE THEN IF mygrid$(x% + n, y%) <> "" THEN east = FALSE
        IF west = TRUE THEN IF mygrid$(x% - n, y%) <> "" THEN west = FALSE
    NEXT
    IF north = TRUE THEN e$ = "|N"
    IF south = TRUE THEN e$ = e$ + "|S"
    IF east = TRUE THEN e$ = e$ + "|E"
    IF west = TRUE THEN e$ = e$ + "|W"

    e$ = MID$(e$, 2)
    DirPos$ = e$
END FUNCTION

FUNCTION shipIndex% (cellValue$)
    DIM shipClass$

    IF UCASE$(LEFT$(cellValue$, 1)) = "X" THEN
        shipClass$ = MID$(cellValue$, 2, 1)
    ELSE
        shipClass$ = MID$(cellValue$, 1, 1)
    END IF
    shipIndex% = INSTR(SHIPLIST$, shipClass$) + VAL(RIGHT$(cellValue$, 1)) - 1

END FUNCTION

SUB PlaceShips
    DIM n AS INTEGER, lastShipPlaced$, ship$, typeCount%, size%, name$

    lastShipPlaced$ = ""
    FOR n = 1 TO LEN(SHIPLIST$)
        ship$ = MID$(SHIPLIST$, n, 1)
        IF lastShipPlaced$ = ship$ THEN typeCount% = typeCount% + 1 ELSE typeCount% = 1
        size% = 0
        SELECT CASE UCASE$(ship$)
            CASE "A": size% = AIRCRAFT_SIZE: name$ = "Aircraft"
            CASE "B": size% = BATTLESHIP_SIZE: name$ = "Battleship"
            CASE "C": size% = CRUISER_SIZE: name$ = "Cruiser"
            CASE "D": size% = DESTROYER_SIZE: name$ = "Destroyer"
            CASE "S": size% = SUBMARINE_SIZE: name$ = "Submarine"
        END SELECT
        PlaceShip name$, size%, typeCount%
        shiphits(n) = size%
        hisshiphits(n) = size%
        lastShipPlaced$ = ship$
        Update
    NEXT
END SUB

SUB PlaceShip (shipName$, shipSize%, i)
    DIM x%, y%, d$, e$, n AS INTEGER
    DO
        IF AutoPlaceShips THEN
            x% = INT(RND * 18)
            y% = INT(RND * 10)
        ELSE
            CLS 2
            PRINT "Enter Position for "; shipName$; i; " (x <ENTER> y):";
            INPUT ; "", x%
            INPUT ; ",", y%
            PRINT
        END IF
    LOOP WHILE x% < 0 OR x% > 17 OR y% < 0 OR y% > 9 OR mygrid$(x%, y%) <> ""

    DO
        d$ = DirPos(shipSize%, x%, y%)
        IF AutoPlaceShips THEN
            e$ = MID$("NEWS", INT(RND * 4) + 1, 1)
        ELSE
            'LOCATE 24, 1:
            PRINT "Enter direction of ship ("; d$; ") :";
            INPUT ; "", e$
            PRINT
        END IF
    LOOP WHILE INSTR(d$, UCASE$(e$)) = 0

    FOR n = 0 TO shipSize% - 1
        SELECT CASE UCASE$(e$)
            CASE "N": mygrid$(x%, y% + n) = UCASE$(LEFT$(shipName$, 1)) + LTRIM$(STR$(i))
            CASE "S": mygrid$(x%, y% - n) = UCASE$(LEFT$(shipName$, 1)) + LTRIM$(STR$(i))
            CASE "E": mygrid$(x% + n, y%) = UCASE$(LEFT$(shipName$, 1)) + LTRIM$(STR$(i))
            CASE "W": mygrid$(x% - n, y%) = UCASE$(LEFT$(shipName$, 1)) + LTRIM$(STR$(i))
        END SELECT
    NEXT
END SUB

FUNCTION endOfGame%
    DIM mineToGo AS INTEGER, hisToGo AS INTEGER
    DIM i%

    LET mineToGo = 0
    LET hisToGo = 0
    FOR i% = LBOUND(shiphits) TO UBOUND(shiphits)
        mineToGo = mineToGo + shiphits(i%)
    NEXT
    FOR i% = LBOUND(hisshiphits) TO UBOUND(hisshiphits)
        hisToGo = hisToGo + hisshiphits(i%)
    NEXT

    endOfGame% = (mineToGo = 0 OR hisToGo = 0)

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
