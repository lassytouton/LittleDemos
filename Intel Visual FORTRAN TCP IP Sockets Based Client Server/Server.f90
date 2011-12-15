PROGRAM SERVER
    USE IFWIN

    IMPLICIT NONE 

    INTEGER WINSOCK_V2_2

    INTEGER CONNECTION_DROPPED_BY_REMOTE_PARTY

    INTEGER SUCCESS

    PARAMETER(WINSOCK_V2_2 = X'202', &
              CONNECTION_DROPPED_BY_REMOTE_PARTY = X'05050505', &
              SUCCESS = 0)

    TYPE T_SOCKADDR_UNION
        SEQUENCE
            UNION
                MAP
                    TYPE(T_SOCKADDR_IN) sockAddrIn
                END MAP
                MAP
                    TYPE(T_SOCKADDR) sockAddr
                END MAP
            END UNION
    END TYPE

    TYPE(T_SOCKADDR_UNION) listenerInfo

    TYPE(T_SOCKADDR_UNION) connectionInfo

    TYPE(T_WSADATA) wsaInfo

    TYPE T_CLIENT_SERVER_MESSAGE
        SEQUENCE
            ! The fields in this TYPE must share memory space with a dummy CHARACTER variable
            ! In this instance the X field from the first UNION MAP and the buffer CHARACTER variable from the
            ! second UNION map both start at the same memory address (i.e. they overlay one another in memory)
            ! The buffer CHARACTER variable is required because the Win32 send and recv routines accept a
            ! CHARACTER variable whose address is the starting memory address of the message to be sent or received,
            ! respectively
            UNION
                MAP
                    INTEGER ClientSendCount
                    INTEGER ServerSendCount
                END MAP
                MAP
                    CHARACTER buffer
                END MAP
            END UNION
    END TYPE

    TYPE(T_CLIENT_SERVER_MESSAGE) clientServerMessage

    INTEGER connection

    INTEGER listener

    CHARACTER*15 host

    INTEGER port

    INTEGER SendMsg

    INTEGER ReceiveMsg

    INTEGER status


    ! Initialize Winsock v2.
    status = WSAStartup(WINSOCK_V2_2, wsaInfo)
    IF (status .NE. SUCCESS) THEN
        WRITE(*, *) 'WSAStartup - ', status

        STOP
    ENDIF

    listener = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    IF (listener .EQ. INVALID_SOCKET) THEN
        WRITE(*, *) 'socket - ', INVALID_SOCKET

        status = WSACleanup()

        STOP
    ENDIF
    
    host = '0.0.0.0'
    port = 5000

    listenerInfo%sockAddrIn%sin_family = AF_INET
    listenerInfo%sockAddrIn%sin_port = htons(port)
    listenerInfo%sockAddrIn%sin_addr%s_addr = inet_addr(host(1:LEN_TRIM(host)))

    status = bind(listener, listenerInfo%sockAddr, SIZEOF(listenerInfo%sockAddr))
    IF (status .EQ. SOCKET_ERROR) THEN
        WRITE(*, *) 'bind - ', WSAGetLastError()

        status = WSACleanup()

        STOP
    ENDIF

    DO WHILE(.TRUE.)
        status = listen(listener, 1)
        IF (status .EQ. SOCKET_ERROR) THEN
            WRITE(*, *) 'listen - ', WSAGetLastError()

            status = WSACleanup()

            STOP
        ENDIF

        IF (status .EQ. SUCCESS) THEN
            connection = SOCKET_ERROR

            DO WHILE (connection .EQ. SOCKET_ERROR)
                connection = accept(listener, connectionInfo%sockAddr, 0)

                Call Sleep(1000)
            ENDDO

            DO WHILE(.TRUE.)
                status = ReceiveMsg(connection, clientServerMessage%buffer, SIZEOF(clientServerMessage))
                IF (status .EQ. SUCCESS) THEN
                    WRITE(*, *) clientServerMessage%ClientSendCount, clientServerMessage%ServerSendCount
                ELSE
                    IF (status .EQ. CONNECTION_DROPPED_BY_REMOTE_PARTY) THEN
                        WRITE(*, *) 'ReceiveMsg - ', 'Connection dropped by remote party'
                    ELSE
                        WRITE(*, *) 'ReceiveMsg - ', status
                    ENDIF

                    status = closesocket(connection)

                    EXIT
                ENDIF

                clientServerMessage%ServerSendCount = clientServerMessage%ServerSendCount + 1

                WRITE(*, *) clientServerMessage%ClientSendCount, clientServerMessage%ServerSendCount

                status = SendMsg(connection, clientServerMessage%buffer, SIZEOF(clientServerMessage))
                IF (status .NE. SUCCESS) THEN
                    WRITE(*, *) 'SendMsg - ', status
                ENDIF

            ENDDO
        ENDIF
    ENDDO

    status = closesocket(connection)

    status = closesocket(listener)

    status = WSACleanup()

    PAUSE
STOP
END

INTEGER FUNCTION SendMsg(connection, buffer, size)
    USE IFWIN

    IMPLICIT NONE 

    INTEGER SUCCESS

    PARAMETER(SUCCESS = 0)

    INTEGER connection

    INTEGER size

    CHARACTER*(size) buffer

    INTEGER status


    SendMsg = SUCCESS

    status = send(connection, buffer, size, 0)
    IF (status .EQ. SOCKET_ERROR) THEN
        SendMsg = WSAGetLastError()

        RETURN
    ENDIF

    RETURN
END

INTEGER FUNCTION ReceiveMsg(connection, buffer, size)
    USE IFWIN

    IMPLICIT NONE 

    INTEGER CONNECTION_DROPPED_BY_REMOTE_PARTY

    INTEGER SUCCESS

    PARAMETER(CONNECTION_DROPPED_BY_REMOTE_PARTY = X'05050505', &
              SUCCESS = 0)

    INTEGER connection

    INTEGER size

    CHARACTER*(size) buffer

    INTEGER bytesReceived
    INTEGER bytesReceivedTotal

    INTEGER status


    ReceiveMsg = SUCCESS

    bytesReceived = 0
    bytesReceivedTotal = 0

    DO WHILE (bytesReceivedTotal < size)
        bytesReceived = recv(connection, buffer(bytesReceivedTotal + 1:bytesReceivedTotal + 1), (size - bytesReceivedTotal), 0)
        IF (bytesReceived .EQ. SOCKET_ERROR) THEN
            ReceiveMsg = WSAGetLastError()

            RETURN
        ELSEIF (bytesReceived .EQ. 0) THEN
            ReceiveMsg = CONNECTION_DROPPED_BY_REMOTE_PARTY

            RETURN
        ENDIF
        
        bytesReceivedTotal = bytesReceivedTotal + bytesReceived
    END DO

    RETURN
END
