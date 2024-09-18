MODULE Automata
    USE ObjetoCaracter   ! Importamos el módulo que define CaracterInfo
    USE Extraccion       ! Importamos el módulo que genera listaCaracteres
    IMPLICIT NONE

    TYPE :: Token
        CHARACTER(LEN=100) :: valor  ! El valor del token
        CHARACTER(LEN=20) :: tipo    ! El tipo de token (reservada, cadena, signo, desconocido, entero, porcentual)
        INTEGER :: fila              ! Posición de fila del token
        INTEGER :: columna           ! Posición de columna del token
    END TYPE Token

    TYPE(Token), ALLOCATABLE :: reconocidos(:)  ! Lista dinámica de tokens reconocidos
    TYPE(Token), ALLOCATABLE :: noReconocidos(:)  ! Lista dinámica de tokens no reconocidos

CONTAINS

    SUBROUTINE procesarTokens()
        INTEGER :: i, numReconocidos, numNoReconocidos

        ! Inicializar contadores
        numReconocidos = 0
        numNoReconocidos = 0

        ! Inicializar listas dinámicas
        IF (.NOT. ALLOCATED(reconocidos)) THEN
            ALLOCATE(reconocidos(0))
        END IF

        IF (.NOT. ALLOCATED(noReconocidos)) THEN
            ALLOCATE(noReconocidos(0))
        END IF

        ! Llenar listaCaracteres con la subrutina del módulo Extraccion
        CALL ExtraccionDeCaracteres()

        ! Asegúrate de que listaCaracteres se ha llenado en ExtraccionDeCaracteres
        IF (.NOT. ALLOCATED(listaCaracteres)) THEN
            PRINT *, "Error: listaCaracteres no ha sido inicializada."
            RETURN
        END IF

        ! Procesar cada token en la lista de caracteres
        DO i = 1, SIZE(listaCaracteres)
            CALL reconocerToken(TRIM(listaCaracteres(i)%caracter), listaCaracteres(i)%fila, listaCaracteres(i)%columna, numReconocidos, numNoReconocidos)
        END DO

        ! Imprimir resultados de depuración
        PRINT *, "Total de tokens reconocidos:", numReconocidos
        PRINT *, "Total de tokens no reconocidos:", numNoReconocidos
        CALL mostrarTokens(reconocidos, numReconocidos, "RECONOCIDOS")
        CALL mostrarTokens(noReconocidos, numNoReconocidos, "NO RECONOCIDOS")
    END SUBROUTINE procesarTokens

    SUBROUTINE reconocerToken(token, fila, columna, numReconocidos, numNoReconocidos)
        CHARACTER(LEN=*), INTENT(IN) :: token
        INTEGER, INTENT(IN) :: fila, columna
        INTEGER, INTENT(INOUT) :: numReconocidos, numNoReconocidos
        CHARACTER(LEN=20) :: tipoToken
        LOGICAL :: esEntero
        INTEGER :: i  ! Declaración de la variable i

        ! Determinar el tipo de token
        IF (token == "grafica" .OR. token == "nombre" .OR. token == "continente" .OR. &
            token == "pais" .OR. token == "poblacion" .OR. token == "saturacion" .OR. token == "bandera") THEN
            tipoToken = "reservada"
        ELSE IF (token(1:1) == '"' .AND. token(LEN_TRIM(token):LEN_TRIM(token)) == '"') THEN
            tipoToken = "cadena"
        ELSE IF (token == "{" .OR. token == "}" .OR. token == ":" .OR. token == ";") THEN
            tipoToken = "signo"
        ELSE IF (token == "%") THEN
            tipoToken = "porcentual"
        ELSE
            ! Verificar si es un número entero
            esEntero = .TRUE.
            DO i = 1, LEN_TRIM(token)
                IF (.NOT. (token(i:i) >= '0' .AND. token(i:i) <= '9')) THEN
                    esEntero = .FALSE.
                    EXIT
                END IF
            END DO

            IF (esEntero) THEN
                tipoToken = "entero"
            ELSE
                tipoToken = "desconocido"
            END IF
        END IF

        ! Almacenar el token en la lista correspondiente
        IF (tipoToken /= "desconocido") THEN
            CALL agregarToken(reconocidos, token, tipoToken, numReconocidos, fila, columna)
        ELSE
            CALL agregarToken(noReconocidos, token, tipoToken, numNoReconocidos, fila, columna)
        END IF
    END SUBROUTINE reconocerToken

    SUBROUTINE agregarToken(lista, valor, tipo, numElementos, fila, columna)
        TYPE(Token), ALLOCATABLE :: lista(:)
        CHARACTER(LEN=*), INTENT(IN) :: valor, tipo
        INTEGER, INTENT(IN) :: fila, columna
        INTEGER, INTENT(INOUT) :: numElementos
        TYPE(Token), ALLOCATABLE :: temp(:)

        ! Inicializar o redimensionar la lista
        numElementos = numElementos + 1  ! Incrementar el contador de elementos

        IF (.NOT. ALLOCATED(lista)) THEN
            ALLOCATE(lista(1))
        ELSE
            ALLOCATE(temp(SIZE(lista) + 1))
            temp(1:SIZE(lista)) = lista
            DEALLOCATE(lista)
            lista = temp
        END IF

        lista(numElementos)%valor = valor
        lista(numElementos)%tipo = tipo
        lista(numElementos)%fila = fila
        lista(numElementos)%columna = columna
    END SUBROUTINE agregarToken

    SUBROUTINE mostrarTokens(lista, numElementos, titulo)
        TYPE(Token), ALLOCATABLE, INTENT(IN) :: lista(:)
        INTEGER, INTENT(IN) :: numElementos
        CHARACTER(LEN=*), INTENT(IN) :: titulo
        INTEGER :: i

        PRINT *, titulo, ":"
        IF (ALLOCATED(lista) .AND. SIZE(lista) > 0) THEN
            DO i = 1, numElementos
                PRINT *, "Token:", TRIM(lista(i)%valor), "- Tipo:", TRIM(lista(i)%tipo), "- Fila:", lista(i)%fila, "- Columna:", lista(i)%columna
            END DO
        ELSE
            PRINT *, "Lista no asignada o vacía."
        END IF
    END SUBROUTINE mostrarTokens

END MODULE Automata
