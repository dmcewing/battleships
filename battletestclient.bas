$DEBUG

CONST HOST_PORT = 20257
CONST PLAYER_MIN = 1
CONST PLAYER_MAX = 8

DIM SHARED client&

client& = _OPENCLIENT("TCP/IP:" + STR$(HOST_PORT) + ":localhost")
IF client& = 0 THEN
    PRINT "Client can't connect"
    END
END IF

DO
    _DELAY 0.2
    p$ = UCASE$(INKEY$)
    SELECT CASE p$
        CASE "X"
            PRINT "Exiting"
            EXIT DO

        CASE "M"
            LINE INPUT ; "Message?", a$
            PRINT ""
            sendMsg a$

    END SELECT
    readMsg
LOOP

CLOSE #client&


'===============================================
' Send Message as a client.
'===============================================
SUB sendMsg (a$)
    PUT #client&, , a$
END SUB

'===============================================
' Read Message as a client.
'===============================================
SUB readMsg
    v$ = " "
    DO UNTIL EOF(client&) OR LEN(v$) = 0
        GET #client&, , v$
        IF NOT (EOF(client&)) AND LEN(v$) > 0 THEN
            PRINT v$
        END IF
        _DELAY 0.02
    LOOP

END SUB
