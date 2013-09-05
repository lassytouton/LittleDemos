        PROGRAM SERVER
            USE KERNEL32

            USE WS2_32

            IMPLICIT NONE

            INTEGER WINSOCK_V2_2

            INTEGER CONNECTION_DROPPED_BY_REMOTE_PARTY

            INTEGER SUCCESS

            PARAMETER(WINSOCK_V2_2 = X'202',
     +                CONNECTION_DROPPED_BY_REMOTE_PARTY = X'05050505',
     +                SUCCESS = 0)

            TYPE(T_SOCKADDR_IN) listenerInfo

            TYPE(T_SOCKADDR_IN) connectionInfo

            TYPE(T_WSADATA) wsaInfo

            TYPE T_CLIENT_SERVER_MESSAGE
                SEQUENCE
                    ! The fields in this TYPE must share memory space with a dummy CHARACTER variable
                    ! In this instance the ClientSendCount field from the first UNION MAP and the buffer CHARACTER variable from the
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

            INTEGER*2 port

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
            port = 55000

            listenerInfo%sin_family = AF_INET
            listenerInfo%sin_port = htons(port)
            listenerInfo%sin_addr%s_addr =
     +                   inet_addr(host(1:LEN_TRIM(host)))

            status = bind(listener,
     +                    %REF(listenerInfo),
     +                    SIZEOF(listenerInfo))
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
                        connection = accept(listener,
     +                                      %REF(connectionInfo),
     +                                      0)

                        Call Sleep(1000)
                    ENDDO

                    DO WHILE(.TRUE.)
                        status = ReceiveMsg(connection,
     +                                      clientServerMessage%buffer,
     +                                      SIZEOF(clientServerMessage))
                        IF (status .EQ. SUCCESS) THEN
                            WRITE(*, *)
     +                            clientServerMessage%ClientSendCount,
     +                            clientServerMessage%ServerSendCount
                        ELSE
                            IF (status .EQ.
     +                          CONNECTION_DROPPED_BY_REMOTE_PARTY) THEN
                                WRITE(*, *)
     +                              'ReceiveMsg - ',
     +                              'Connection dropped by remote party'
                            ELSE
                                WRITE(*, *) 'ReceiveMsg - ', status
                            ENDIF

                            status = closesocket(connection)

                            EXIT
                        ENDIF

                        clientServerMessage%ServerSendCount =
     +                        clientServerMessage%ServerSendCount + 1

                        WRITE(*, *) clientServerMessage%ClientSendCount,
     +                              clientServerMessage%ServerSendCount

                        status = SendMsg(connection,
     +                                   clientServerMessage%buffer,
     +                                   SIZEOF(clientServerMessage))
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
            USE WS2_32

            IMPLICIT NONE 

            INTEGER SUCCESS

            PARAMETER(SUCCESS = 0)

            INTEGER connection

            INTEGER size

            CHARACTER*(size) buffer

            INTEGER bytesSent
            INTEGER bytesSentTotal

            INTEGER status


            SendMsg = SUCCESS

            bytesSent = 0
            bytesSentTotal = 0

            DO WHILE (bytesSentTotal < size)
                bytesSent = send(connection,
     +                           buffer(bytesSentTotal + 1:
     +                                  bytesSentTotal + 1),
     +                           (size - bytesSentTotal),
     +                           0)
                IF (bytesSent .EQ. SOCKET_ERROR) THEN
                    SendMsg = WSAGetLastError()

                    RETURN
                ENDIF
        
                bytesSentTotal = bytesSentTotal + bytesSent
            END DO

            RETURN
        END

        INTEGER FUNCTION ReceiveMsg(connection, buffer, size)
            USE WS2_32

            IMPLICIT NONE 

            INTEGER CONNECTION_DROPPED_BY_REMOTE_PARTY

            INTEGER SUCCESS

            PARAMETER(CONNECTION_DROPPED_BY_REMOTE_PARTY = X'05050505',
     +                SUCCESS = 0)

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
                bytesReceived = recv(connection,
     +                               buffer(bytesReceivedTotal + 1:
     +                                      bytesReceivedTotal + 1),
     +                               (size - bytesReceivedTotal),
     +                               0)
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
