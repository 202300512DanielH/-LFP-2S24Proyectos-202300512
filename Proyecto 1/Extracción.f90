MODULE Extraccion
    USE ObjetoCaracter  ! Importamos el módulo que define el tipo de datos CaracterInfo
    IMPLICIT NONE

    TYPE(CaracterInfo), ALLOCATABLE :: listaCaracteres(:)  ! Arreglo dinámico de CaracterInfo

CONTAINS

    SUBROUTINE ExtraccionDeCaracteres()
        CHARACTER(LEN=100) :: palabra                          ! Para leer palabras
        INTEGER :: i, j, numCaracteres, fila, columna, iostat
        CHARACTER(LEN=1000) :: linea                           ! Almacenar cada línea de entrada
        LOGICAL :: dentro_comillas                             ! Bandera para el manejo de comillas
        CHARACTER(LEN=1) :: c

        fila = 1  ! Inicializamos la fila
        numCaracteres = 0  ! Contador de caracteres leídos
        dentro_comillas = .FALSE.  ! Inicializamos la bandera de comillas

        ! Leer de la entrada estándar hasta el final
        DO
            READ(*, '(A)', IOSTAT=iostat) linea
            IF (iostat /= 0) EXIT  ! Salir del bucle si hay un error de lectura o EOF

            columna = 1  ! Inicializamos la columna para cada nueva línea

            ! Procesar la línea carácter por carácter
            i = 1
            DO WHILE (i <= LEN(linea))  ! Procesamos la línea completa, incluyendo los espacios
                c = linea(i:i)

                ! Si el carácter es un espacio, solo se incrementa la columna y se ignora el token
                IF (c == ' ' .OR. c == CHAR(9)) THEN
                    columna = columna + 1
                    i = i + 1
                    CYCLE
                END IF

                ! Si encontramos comillas, procesar el contenido entre comillas como un token
                IF (c == '"') THEN
                    j = i + 1
                    DO WHILE (j <= LEN(linea) .AND. linea(j:j) /= '"')
                        j = j + 1
                    END DO
                    IF (j <= LEN(linea)) THEN
                        j = j + 1  ! Incluir la comilla de cierre
                    END IF
                    palabra = linea(i:j-1)
                    CALL almacenarToken(listaCaracteres, palabra, fila, columna, numCaracteres)
                    columna = columna + LEN_TRIM(palabra)
                    i = j
                    CYCLE
                END IF

                ! Procesar símbolos como tokens individuales
                IF (c == '!' .OR. c == '#' .OR. c == '$' .OR. c == '%' .OR. c == '&' .OR. c == '@' .OR. &
                    c == ':' .OR. c == ';' .OR. c == ',' .OR. c == '.') THEN
                    CALL almacenarToken(listaCaracteres, c, fila, columna, numCaracteres)
                    columna = columna + 1
                    i = i + 1
                    CYCLE
                END IF

                ! Procesar palabras (secuencias de letras y números)
                j = i
                DO WHILE (j <= LEN(linea) .AND. linea(j:j) /= ' ' .AND. linea(j:j) /= CHAR(9) .AND. &
                         linea(j:j) /= '!' .AND. linea(j:j) /= '#' .AND. linea(j:j) /= '$' .AND. &
                         linea(j:j) /= '%' .AND. linea(j:j) /= '&' .AND. linea(j:j) /= '@' .AND. &
                         linea(j:j) /= ':' .AND. linea(j:j) /= ';' .AND. linea(j:j) /= ',' .AND. linea(j:j) /= '.')
                    j = j + 1
                END DO
                palabra = linea(i:j-1)
                CALL almacenarToken(listaCaracteres, TRIM(palabra), fila, columna, numCaracteres)  ! Eliminar espacios
                columna = columna + LEN_TRIM(palabra)
                i = j
            END DO

            fila = fila + 1  ! Siguiente fila
        END DO

        ! Mostrar el resultado
        CALL mostrarLista(listaCaracteres, numCaracteres)

    END SUBROUTINE ExtraccionDeCaracteres

    SUBROUTINE almacenarToken(lista, token, fila, columna, numCaracteres)
        TYPE(CaracterInfo), ALLOCATABLE :: lista(:)
        CHARACTER(LEN=*), INTENT(IN) :: token
        INTEGER, INTENT(IN) :: fila, columna
        INTEGER, INTENT(INOUT) :: numCaracteres

        ! Almacenar el token eliminando posibles espacios al principio y al final
        numCaracteres = numCaracteres + 1
        IF (.NOT. ALLOCATED(lista)) THEN
            ALLOCATE(lista(1))
        ELSE
            CALL aumentarLista(lista, numCaracteres)
        END IF
        lista(numCaracteres) = CaracterInfo(TRIM(token), fila, columna)
    END SUBROUTINE almacenarToken

    SUBROUTINE aumentarLista(lista, newSize)
        TYPE(CaracterInfo), ALLOCATABLE :: lista(:)
        INTEGER, INTENT(IN) :: newSize
        TYPE(CaracterInfo), ALLOCATABLE :: temp(:)

        ALLOCATE(temp(newSize))
        IF (ALLOCATED(lista)) THEN
            temp(1:SIZE(lista)) = lista
            DEALLOCATE(lista)
        END IF
        lista = temp
    END SUBROUTINE aumentarLista

    SUBROUTINE mostrarLista(lista, size)
        TYPE(CaracterInfo), ALLOCATABLE :: lista(:)
        INTEGER, INTENT(IN) :: size
        INTEGER :: i

        PRINT *, "LISTA de Caracteres y su posición:"
        DO i = 1, size
            PRINT *, TRIM(lista(i)%caracter), " - Fila:", lista(i)%fila, "Columna:", lista(i)%columna
        END DO
    END SUBROUTINE mostrarLista

END MODULE Extraccion
