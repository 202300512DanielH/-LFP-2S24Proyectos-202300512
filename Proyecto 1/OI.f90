MODULE OI
    USE Automata        ! Para obtener la lista de tokens reconocidos y no reconocidos
    IMPLICIT NONE

    TYPE :: Informacion
        CHARACTER(LEN=100) :: dato        ! Nombre del dato (e.g., "nombre de gráfica", "nombre de continente", etc.)
        CHARACTER(LEN=100) :: valordato   ! Valor del dato (e.g., "Gráfico de Población", "América", etc.)
    END TYPE Informacion

    ! Lista dinámica para almacenar objetos del tipo Informacion
    TYPE(Informacion), ALLOCATABLE :: listaInformacion(:)

CONTAINS

    SUBROUTINE procesarTokensOI()
        INTEGER :: i, valorEntero
        CHARACTER(LEN=100) :: valorCadena
        LOGICAL :: graficaEncontrada, continenteEncontrado, paisEncontrado
        LOGICAL :: poblacionEncontrada, saturacionEncontrada, banderaEncontrada
        TYPE(Informacion), ALLOCATABLE :: temp(:)  ! Variable temporal para redimensionar

        ! Verificar si hay tokens no reconocidos
        IF (ALLOCATED(noReconocidos) .AND. SIZE(noReconocidos) > 0) THEN
            PRINT *, "No se harán datos de lecturas de archivos parcial o totalmente erróneos"
            RETURN
        END IF

        ! Verificamos si la lista de tokens reconocidos está asignada y tiene al menos un elemento
        IF (.NOT. ALLOCATED(reconocidos)) THEN
            PRINT *, "Error: No hay tokens reconocidos disponibles."
            RETURN
        END IF

        IF (SIZE(reconocidos) == 0) THEN
            PRINT *, "La lista de tokens reconocidos está vacía."
            RETURN
        END IF

        ! Inicializar variables
        graficaEncontrada = .FALSE.
        continenteEncontrado = .FALSE.
        paisEncontrado = .FALSE.
        poblacionEncontrada = .FALSE.
        saturacionEncontrada = .FALSE.
        banderaEncontrada = .FALSE.

        ! Recorrer la lista de tokens
        DO i = 1, SIZE(reconocidos)
            ! Si encontramos un token de tipo "reservada" y valor "grafica"
            IF (TRIM(reconocidos(i)%tipo) == "reservada" .AND. TRIM(reconocidos(i)%valor) == "grafica") THEN
                PRINT *, "Token 'grafica' encontrado. Buscando el valor de la gráfica..."
                graficaEncontrada = .TRUE.
                CYCLE
            END IF

            ! Si encontramos un token de tipo "reservada" y valor "continente"
            IF (TRIM(reconocidos(i)%tipo) == "reservada" .AND. TRIM(reconocidos(i)%valor) == "continente") THEN
                PRINT *, "Token 'continente' encontrado. Buscando el valor del continente..."
                continenteEncontrado = .TRUE.
                CYCLE
            END IF

            ! Si encontramos un token de tipo "reservada" y valor "pais"
            IF (TRIM(reconocidos(i)%tipo) == "reservada" .AND. TRIM(reconocidos(i)%valor) == "pais") THEN
                PRINT *, "Token 'pais' encontrado. Buscando el valor del país..."
                paisEncontrado = .TRUE.
                CYCLE
            END IF

            ! Si encontramos un token de tipo "reservada" y valor "poblacion"
            IF (TRIM(reconocidos(i)%tipo) == "reservada" .AND. TRIM(reconocidos(i)%valor) == "poblacion") THEN
                PRINT *, "Token 'poblacion' encontrado. Buscando el valor de la población..."
                poblacionEncontrada = .TRUE.
                CYCLE
            END IF

            ! Si encontramos un token de tipo "reservada" y valor "saturacion"
            IF (TRIM(reconocidos(i)%tipo) == "reservada" .AND. TRIM(reconocidos(i)%valor) == "saturacion") THEN
                PRINT *, "Token 'saturacion' encontrado. Buscando el valor de la saturación..."
                saturacionEncontrada = .TRUE.
                CYCLE
            END IF

            ! Si encontramos un token de tipo "reservada" y valor "bandera"
            IF (TRIM(reconocidos(i)%tipo) == "reservada" .AND. TRIM(reconocidos(i)%valor) == "bandera") THEN
                PRINT *, "Token 'bandera' encontrado. Buscando el valor de la bandera..."
                banderaEncontrada = .TRUE.
                CYCLE
            END IF

            ! Procesar los valores después de encontrar los tokens reservados

            ! Valor de gráfica
            IF (graficaEncontrada .AND. TRIM(reconocidos(i)%tipo) == "cadena") THEN
                CALL almacenarInformacion("nombre de gráfica", TRIM(reconocidos(i)%valor))
                PRINT *, "Nombre de gráfica almacenado:", TRIM(reconocidos(i)%valor)
                graficaEncontrada = .FALSE.
                CYCLE
            END IF

            ! Valor de continente
            IF (continenteEncontrado .AND. TRIM(reconocidos(i)%tipo) == "cadena") THEN
                CALL almacenarInformacion("nombre de continente", TRIM(reconocidos(i)%valor))
                PRINT *, "Nombre de continente almacenado:", TRIM(reconocidos(i)%valor)
                continenteEncontrado = .FALSE.
                CYCLE
            END IF

            ! Valor de país
            IF (paisEncontrado .AND. TRIM(reconocidos(i)%tipo) == "cadena") THEN
                CALL almacenarInformacion("nombre de país", TRIM(reconocidos(i)%valor))
                PRINT *, "Nombre de país almacenado:", TRIM(reconocidos(i)%valor)
                paisEncontrado = .FALSE.
                CYCLE
            END IF

            ! Valor de población
            IF (poblacionEncontrada .AND. TRIM(reconocidos(i)%tipo) == "entero") THEN
                READ(reconocidos(i)%valor, *) valorEntero
                WRITE(valorCadena, "(I10)") valorEntero  ! Convertir el entero en cadena
                CALL almacenarInformacion("población", TRIM(valorCadena))
                PRINT *, "Población almacenada:", valorEntero
                poblacionEncontrada = .FALSE.
                CYCLE
            END IF

            ! Valor de saturación
            IF (saturacionEncontrada .AND. TRIM(reconocidos(i)%tipo) == "entero") THEN
                READ(reconocidos(i)%valor, *) valorEntero
                WRITE(valorCadena, "(I10)") valorEntero  ! Convertir el entero en cadena
                CALL almacenarInformacion("saturación", TRIM(valorCadena))
                PRINT *, "Saturación almacenada:", valorEntero
                saturacionEncontrada = .FALSE.
                CYCLE
            END IF

            ! Valor de bandera
            IF (banderaEncontrada .AND. TRIM(reconocidos(i)%tipo) == "cadena") THEN
                CALL almacenarInformacion("bandera", TRIM(reconocidos(i)%valor))
                PRINT *, "Bandera almacenada:", TRIM(reconocidos(i)%valor)
                banderaEncontrada = .FALSE.
                CYCLE
            END IF
        END DO

        ! Mostrar el contenido de la lista de información
        CALL mostrarInformacion()
    END SUBROUTINE procesarTokensOI

    SUBROUTINE almacenarInformacion(dato, valor)
        CHARACTER(LEN=*), INTENT(IN) :: dato, valor
        TYPE(Informacion), ALLOCATABLE :: temp(:)

        ! Redimensionar la lista de información de forma segura
        IF (.NOT. ALLOCATED(listaInformacion)) THEN
            ALLOCATE(listaInformacion(1))
        ELSE
            ALLOCATE(temp(SIZE(listaInformacion) + 1))  ! Redimensionar usando una variable temporal
            temp(1:SIZE(listaInformacion)) = listaInformacion
            DEALLOCATE(listaInformacion)
            CALL MOVE_ALLOC(temp, listaInformacion)  ! Mover el arreglo redimensionado a listaInformacion
        END IF

        ! Almacenar la información
        listaInformacion(SIZE(listaInformacion))%dato = dato
        listaInformacion(SIZE(listaInformacion))%valordato = valor
    END SUBROUTINE almacenarInformacion

    SUBROUTINE mostrarInformacion()
        INTEGER :: i

        ! Verificar si la lista de información está asignada y tiene al menos un elemento
        IF (.NOT. ALLOCATED(listaInformacion)) THEN
            PRINT *, "Error: No hay información disponible."
            RETURN
        END IF

        IF (SIZE(listaInformacion) == 0) THEN
            PRINT *, "La lista de información está vacía."
            RETURN
        END IF

        ! Recorrer y mostrar todos los datos en la lista
        DO i = 1, SIZE(listaInformacion)
            PRINT *, "Información: Dato:", TRIM(listaInformacion(i)%dato), &
                     "- Valor:", TRIM(listaInformacion(i)%valordato)
        END DO
    END SUBROUTINE mostrarInformacion

END MODULE OI
