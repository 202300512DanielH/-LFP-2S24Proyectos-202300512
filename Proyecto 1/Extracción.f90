! archivo: Extracción.f90
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

        fila = 1  ! Inicializamos la fila
        numCaracteres = 0  ! Contador de caracteres leídos
        dentro_comillas = .FALSE.  ! Inicializamos la bandera de comillas

        ! Leer de la entrada estándar hasta el final
        DO
            READ(*, '(A)', IOSTAT=iostat) linea
            IF (iostat /= 0) EXIT  ! Salir del bucle si hay un error de lectura o EOF

            columna = 1  ! Inicializamos la columna para cada nueva línea

            ! Dividir la línea en palabras y signos y almacenarlos
            i = 1
            DO WHILE (i <= LEN_TRIM(linea))
                IF (linea(i:i) /= ' ') THEN
                    ! Comienza o termina texto entre comillas dobles
                    IF (linea(i:i) == '"') THEN
                        IF (.NOT. dentro_comillas) THEN
                            ! Inicio de texto entre comillas
                            dentro_comillas = .TRUE.
                            j = i + 1
                            DO WHILE (j <= LEN_TRIM(linea) .AND. linea(j:j) /= '"')
                                j = j + 1
                            END DO
                            IF (j <= LEN_TRIM(linea)) THEN
                                j = j + 1  ! Incluir la comilla de cierre
                            END IF
                            ! Almacenar solo el contenido dentro de las comillas
                            palabra = linea(i:j-1)
                            CALL almacenarToken(listaCaracteres, palabra, fila, columna, numCaracteres)
                            columna = columna + LEN_TRIM(palabra)
                            i = j

                            ! Se cierra el texto entre comillas
                            dentro_comillas = .FALSE.  

                            ! Verificar si el siguiente carácter es un signo de puntuación y separarlo
                            IF (i <= LEN_TRIM(linea) .AND. (linea(i:i) == ';' .OR. linea(i:i) == ',' .OR. linea(i:i) == '.' .OR. linea(i:i) == '%')) THEN
                                palabra = linea(i:i)
                                CALL almacenarToken(listaCaracteres, palabra, fila, columna, numCaracteres)
                                i = i + 1
                            END IF
                            CYCLE
                        END IF
                    ELSE IF (linea(i:i) == '%') THEN
                        ! Manejar el símbolo '%' como un token independiente
                        palabra = linea(i:i)
                        j = i + 1
                    ELSE
                        ! No estamos dentro de comillas, procesamos como palabra o signo
                        IF (linea(i:i) == '.' .OR. linea(i:i) == ',' .OR. linea(i:i) == ';' .OR. linea(i:i) == ':') THEN
                            ! Es un signo de puntuación
                            palabra = linea(i:i)
                            j = i + 1
                        ELSE
                            ! Es parte de una palabra o número
                            j = i
                            DO WHILE (j <= LEN_TRIM(linea) .AND. linea(j:j) /= ' ' .AND. &
                                     linea(j:j) /= '.' .AND. linea(j:j) /= ',' .AND. linea(j:j) /= ';' .AND. linea(j:j) /= ':' .AND. linea(j:j) /= '%')
                                j = j + 1
                            END DO
                            palabra = linea(i:j-1)
                        END IF
                    END IF

                    ! Almacenar el token procesado
                    CALL almacenarToken(listaCaracteres, palabra, fila, columna, numCaracteres)
                    columna = columna + LEN_TRIM(palabra)  ! Actualizar la columna
                    i = j  ! Actualizar 'i' para continuar desde el final de la palabra o signo
                ELSE
                    i = i + 1  ! Avanzar al siguiente carácter
                END IF
            END DO

            fila = fila + 1  ! Siguiente fila
        END DO

        ! Mostrar el resultado
        CALL mostrarLista(listaCaracteres, numCaracteres)

    END SUBROUTINE ExtraccionDeCaracteres

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

    SUBROUTINE almacenarToken(lista, token, fila, columna, numCaracteres)
        TYPE(CaracterInfo), ALLOCATABLE :: lista(:)
        CHARACTER(LEN=*), INTENT(IN) :: token
        INTEGER, INTENT(IN) :: fila, columna
        INTEGER, INTENT(INOUT) :: numCaracteres

        numCaracteres = numCaracteres + 1
        IF (.NOT. ALLOCATED(lista)) THEN
            ALLOCATE(lista(1))
        ELSE
            CALL aumentarLista(lista, numCaracteres)
        END IF
        lista(numCaracteres) = CaracterInfo(TRIM(token), fila, columna)
    END SUBROUTINE almacenarToken

    SUBROUTINE mostrarLista(lista, size)
        TYPE(CaracterInfo), ALLOCATABLE :: lista(:)
        INTEGER, INTENT(IN) :: size
        INTEGER :: i

        PRINT *, "LISTA de Caracteres y su posición:"
        !DO i = 1, size
        !    PRINT *, TRIM(lista(i)%caracter), " - Fila:", lista(i)%fila, "Columna:", lista(i)%columna
        !END DO
    END SUBROUTINE mostrarLista

END MODULE Extraccion
