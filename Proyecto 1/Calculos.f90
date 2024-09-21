MODULE Calculos
    USE OI           ! Importar el módulo OI para acceder a la lista de información
    IMPLICIT NONE

    TYPE Pais
        CHARACTER(LEN=100) :: nombre
        INTEGER :: poblacion
        REAL :: saturacion
        CHARACTER(LEN=100) :: bandera
    END TYPE Pais

    TYPE Continente
        CHARACTER(LEN=100) :: nombre
        REAL :: saturacion   ! Saturación promedio del continente
        TYPE(Pais), ALLOCATABLE :: paises(:)
    END TYPE Continente

CONTAINS

    SUBROUTINE iniciarCalculos()
        INTEGER :: i, j, numPaises, idxContinente
        TYPE(Continente), ALLOCATABLE :: continentes(:)
        LOGICAL :: continenteEncontrado
        CHARACTER(LEN=100) :: nombreGrafica
        INTEGER :: tempInt, ioStat
        REAL :: tempReal, sumaSaturacion, menorSaturacionPais, menorSaturacionContinente
        CHARACTER(LEN=100) :: valordatoTrimmed
        CHARACTER(LEN=100) :: paisSeleccionado, banderaSeleccionada, continenteSeleccionado
        INTEGER :: poblacionSeleccionada

        ! Verificamos si la lista de información está asignada y tiene al menos un elemento
        IF (.NOT. ALLOCATED(listaInformacion)) THEN
            PRINT *, "Error: La lista de información no está asignada."
            RETURN
        END IF

        IF (SIZE(listaInformacion) == 0) THEN
            PRINT *, "Error: La lista de información está vacía."
            RETURN
        END IF

        ! Buscar el nombre de la gráfica
        DO i = 1, SIZE(listaInformacion)
            IF (TRIM(listaInformacion(i)%dato) == 'nombre de gráfica') THEN
                nombreGrafica = TRIM(listaInformacion(i)%valordato)
                !PRINT *, "Iniciando Cálculos. Nombre de la gráfica:", nombreGrafica
                EXIT
            END IF
        END DO

        ! Si no se encuentra el nombre de gráfica
        IF (TRIM(nombreGrafica) == '') THEN
            PRINT *, "Error: No se encontró el nombre de la gráfica."
            RETURN
        END IF

        ! Inicializamos la agrupación de continentes y países
        idxContinente = 0
        continenteEncontrado = .FALSE.
        IF (.NOT. ALLOCATED(continentes)) THEN
            ALLOCATE(continentes(0))
        END IF

        DO i = 1, SIZE(listaInformacion)
            valordatoTrimmed = TRIM(listaInformacion(i)%valordato)

            SELECT CASE (TRIM(listaInformacion(i)%dato))

            CASE ('nombre de continente')
                ! Encontramos un nuevo continente, incrementar el contador y almacenar el nombre
                idxContinente = idxContinente + 1
                IF (ALLOCATED(continentes)) THEN
                    ! Redimensionamos el arreglo de continentes para añadir uno nuevo
                    CALL REALLOCATE_CONTINENTES(continentes, idxContinente)
                ELSE
                    ALLOCATE(continentes(idxContinente))
                END IF
                continentes(idxContinente)%nombre = valordatoTrimmed
                ALLOCATE(continentes(idxContinente)%paises(0))  ! Inicializamos la lista de países
                continentes(idxContinente)%saturacion = 0.0  ! Inicializamos la saturación del continente
                continenteEncontrado = .TRUE.

            CASE ('nombre de país')
                IF (continenteEncontrado) THEN
                    ! Agregar un nuevo país al último continente encontrado
                    numPaises = SIZE(continentes(idxContinente)%paises)
                    numPaises = numPaises + 1
                    CALL REALLOCATE_PAISES(continentes(idxContinente)%paises, numPaises)
                    continentes(idxContinente)%paises(numPaises)%nombre = valordatoTrimmed
                END IF

            CASE ('población')
                IF (continenteEncontrado .AND. numPaises > 0) THEN
                    ! Intentar convertir el valor a entero
                    READ(valordatoTrimmed, *, IOSTAT=ioStat) tempInt
                    IF (ioStat == 0) THEN
                        continentes(idxContinente)%paises(numPaises)%poblacion = tempInt
                    ELSE
                        PRINT *, "Error: No se pudo convertir la población a entero para el país:", &
                                 continentes(idxContinente)%paises(numPaises)%nombre
                    END IF
                END IF

            CASE ('saturación')
                IF (continenteEncontrado .AND. numPaises > 0) THEN
                    ! Intentar convertir el valor a real
                    READ(valordatoTrimmed, *, IOSTAT=ioStat) tempReal
                    IF (ioStat == 0) THEN
                        continentes(idxContinente)%paises(numPaises)%saturacion = tempReal
                    ELSE
                        PRINT *, "Error: No se pudo convertir la saturación a real para el país:", &
                                 continentes(idxContinente)%paises(numPaises)%nombre
                    END IF
                END IF

            CASE ('bandera')
                IF (continenteEncontrado .AND. numPaises > 0) THEN
                    continentes(idxContinente)%paises(numPaises)%bandera = valordatoTrimmed
                END IF

            END SELECT
        END DO

        ! Calcular la saturación promedio de cada continente
        DO idxContinente = 1, SIZE(continentes)
            sumaSaturacion = 0.0
            numPaises = SIZE(continentes(idxContinente)%paises)
            IF (numPaises > 0) THEN
                DO i = 1, numPaises
                    sumaSaturacion = sumaSaturacion + continentes(idxContinente)%paises(i)%saturacion
                END DO
                continentes(idxContinente)%saturacion = sumaSaturacion / numPaises
            ELSE
                continentes(idxContinente)%saturacion = 0.0
            END IF
        END DO

        ! Inicializar las variables para el país con menor saturación
        menorSaturacionPais = HUGE(0.0)
        menorSaturacionContinente = HUGE(0.0)
        paisSeleccionado = ""
        continenteSeleccionado = ""
        banderaSeleccionada = ""
        poblacionSeleccionada = 0

        ! Encontrar el país con menor saturación
        DO idxContinente = 1, SIZE(continentes)
            DO i = 1, SIZE(continentes(idxContinente)%paises)
                IF (continentes(idxContinente)%paises(i)%saturacion < menorSaturacionPais) THEN
                    ! Actualizamos con el país de menor saturación
                    menorSaturacionPais = continentes(idxContinente)%paises(i)%saturacion
                    menorSaturacionContinente = continentes(idxContinente)%saturacion
                    paisSeleccionado = continentes(idxContinente)%paises(i)%nombre
                    continenteSeleccionado = continentes(idxContinente)%nombre
                    banderaSeleccionada = continentes(idxContinente)%paises(i)%bandera
                    poblacionSeleccionada = continentes(idxContinente)%paises(i)%poblacion
                ELSE IF (continentes(idxContinente)%paises(i)%saturacion == menorSaturacionPais) THEN
                    ! Si hay empate en saturación, seleccionamos el país del continente con menor saturación
                    IF (continentes(idxContinente)%saturacion < menorSaturacionContinente) THEN
                        menorSaturacionContinente = continentes(idxContinente)%saturacion
                        paisSeleccionado = continentes(idxContinente)%paises(i)%nombre
                        continenteSeleccionado = continentes(idxContinente)%nombre
                        banderaSeleccionada = continentes(idxContinente)%paises(i)%bandera
                        poblacionSeleccionada = continentes(idxContinente)%paises(i)%poblacion
                    END IF
                END IF
            END DO
        END DO

        ! Mostrar el país seleccionado
        PRINT *, "El país con menor saturación es:", paisSeleccionado
        PRINT *, "Continente:", continenteSeleccionado
        PRINT *, "Población:", poblacionSeleccionada
        PRINT *, "Saturación:", menorSaturacionPais
        PRINT *, "Bandera:", banderaSeleccionada

    END SUBROUTINE iniciarCalculos

    SUBROUTINE REALLOCATE_CONTINENTES(continentes, newSize)
        TYPE(Continente), ALLOCATABLE, INTENT(INOUT) :: continentes(:)
        INTEGER, INTENT(IN) :: newSize
        TYPE(Continente), ALLOCATABLE :: temp(:)

        ! Redimensionar el arreglo de continentes manteniendo los valores existentes
        ALLOCATE(temp(SIZE(continentes)))
        temp = continentes
        DEALLOCATE(continentes)
        ALLOCATE(continentes(newSize))
        continentes(:SIZE(temp)) = temp
    END SUBROUTINE REALLOCATE_CONTINENTES

    SUBROUTINE REALLOCATE_PAISES(paises, newSize)
        TYPE(Pais), ALLOCATABLE, INTENT(INOUT) :: paises(:)
        INTEGER, INTENT(IN) :: newSize
        TYPE(Pais), ALLOCATABLE :: temp(:)

        ! Redimensionar el arreglo de países manteniendo los valores existentes
        ALLOCATE(temp(SIZE(paises)))
        temp = paises
        DEALLOCATE(paises)
        ALLOCATE(paises(newSize))
        paises(:SIZE(temp)) = temp
    END SUBROUTINE REALLOCATE_PAISES

END MODULE Calculos
