'$DEBUG
$CONSOLE:ONLY

DECLARE SUB GameLoop ()
DECLARE SUB ResetNetGame ()
DECLARE SUB CopyMessage (clientIdx%)
DECLARE SUB ResetClient (idx%)
DECLARE SUB FindPair (idx%)

CONST HOST_PORT = 25123
CONST PLAYER_MIN = 1
CONST PLAYER_MAX = 8

TYPE tclient 'a client
    conn AS INTEGER
    ip AS STRING * 100
    pair AS INTEGER
    ready AS INTEGER
    name AS STRING * 100
END TYPE

DIM SHARED client(PLAYER_MIN TO PLAYER_MAX) AS tclient
DIM SHARED hosth&


hosth& = _OPENHOST("TCP/IP:" + STR$(HOST_PORT))
IF hosth& = 0 THEN
    PRINT "Error opening host: ", ERR
ELSE
    PRINT "Game hosting started"
    GameLoop
    ResetNetGame
END IF

SUB ResetNetGame
    FOR n = PLAYER_MIN TO PLAYER_MAX
        IF client(n).conn <> 0 THEN
            CLOSE client(n).conn
            PRINT "Client ", n, " disconnected"
	    ResetClientState n
        END IF
    NEXT n

    IF hosth& <> 0 THEN
        CLOSE hosth&
        hosth& = 0
        PRINT "Game hosting ended"
    END IF

END SUB

SUB GameLoop
    DO
        FOR idx% = PLAYER_MIN TO PLAYER_MAX
            IF client(idx%).conn = 0 THEN
                client(idx%).conn = _OPENCONNECTION(hosth&)
                'IF client(idx%).conn <> 0 THEN FindPair idx%
            ELSE
                IF _CONNECTED(client(idx%).conn) = 0 THEN
                    ResetClient idx%
                ELSE
                    CopyMessage idx%
                END IF
            END IF
        NEXT
        _DELAY 0.5
    LOOP
END SUB



SUB CopyMessage (clientIdx%)
    DIM pairIdx%, outConn, inConn, inti%, newPair$, m$

    pairIdx% = client(clientIdx%).pair
    IF pairIdx% <> 0 THEN outConn = client(pairIdx%).conn

    inConn = client(clientIdx%).conn

    inti% = 0
    v$ = " "
    DO UNTIL EOF(inConn) OR LEN(v$) = 0
        _DELAY 0.02
        GET #inConn, , v$
        inti% = inti% + 1
        IF NOT (EOF(inConn)) AND LEN(v$) > 0 THEN
            IF LEFT$(v$, 1) = "G" THEN
                PRINT "(" + LTRIM$(RTRIM$(STR$(clientIdx%))) + ") <<< ", v$
                client(clientIdx%).ready = -1
                client(clientIdx%).name = MID$(v$, 6)
                FindPair clientIdx%
                IF client(clientIdx%).pair <> 0 THEN
                    pairIdx% = client(clientIdx%).pair
                    newPair$ = client(pairIdx%).name
                    m$ = "G0000" + newPair$
                    PRINT "   (" + LTRIM$(RTRIM$(STR$(pairIdx%))) + ") >>> ", m$
                    PUT #inConn, , m$
                END IF
                EXIT DO
            ELSEIF outConn <> 0 THEN
                PRINT "(" + STR$(clientIdx%) + ") --> (" + STR$(pairIdx%) + ") >>> ", v$
                PUT #outConn, , v$
            END IF
        END IF
    LOOP

END SUB

SUB ResetClient (idx%)
    PRINT "Client #", idx%, " has disconnected."
    pair = client(idx%).pair
    CLOSE #client(idx%).conn
    client(idx%).conn = 0
    IF pair > 0 THEN
        PRINT "  - Closing client pair #", pair
        m$ = "ENDGAME"
        PUT #client(pair).conn, , m$
        CLOSE #client(pair).conn
	ResetClientState(pair)
	ResetClientState(idx%)
    END IF
END SUB

SUB ResetClientState (idx%)
	client(idx%).pair = 0
	client(idx%).conn = 0
	client(idx%).ready = 0
	client(idx%).name = ""
END SUB

SUB FindPair (idx%)
    DIM m$
    PRINT "Finding pair for #", idx%, ": ";
    client(idx%).pair = 0
    FOR i = PLAYER_MIN TO PLAYER_MAX
        IF (i <> idx%) THEN
            IF client(i).conn <> 0 AND client(i).pair = 0 AND client(i).ready = -1 THEN
                PRINT " Paired with ", i
                'Set Pair
                client(i).pair = idx%
                client(idx%).pair = i
                m$ = "G0101" + client(idx%).name
                PRINT "   (" + LTRIM$(RTRIM$(STR$(i))) + ") >>> ", m$
                PUT #client(i).conn, , m$
                EXIT FOR
            END IF
        END IF
    NEXT
    IF client(idx%).pair = 0 THEN
        PRINT " - No pairing found"
    END IF
END SUB
