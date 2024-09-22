! archivo: main.f90
PROGRAM Main
    USE ObjetoCaracter   ! Módulo que define el tipo de datos CaracterInfo
    USE Extraccion       ! Módulo que contiene la lógica de extracción de caracteres
    USE Automata         ! Módulo que contiene el autómata para procesar los caracteres
    USE OI               ! Módulo que contiene la lógica para obtener tokens válidos
    USE Reporte          ! Módulo que genera el reporte HTML
    USE Calculos         ! Módulo que inicia los cálculos con la lista de información
    USE Grafica         ! Módulo que genera la gráfica
    IMPLICIT NONE

    ! Llamar a la subrutina para generar la lista de caracteres
    CALL ExtraccionDeCaracteres()

    ! Verificar que la lista de caracteres ha sido generada
    IF (.NOT. ALLOCATED(listaCaracteres)) THEN
        PRINT *, "Error: No se ha inicializado la lista de caracteres."
        STOP
    END IF

    ! Llamar al autómata para procesar la lista de caracteres generada
    CALL procesarTokens()

    ! Generar el reporte HTML basado en los tokens reconocidos y no reconocidos
    CALL generarReporte()

    ! OI obtiene la lista de tokens válidos
    CALL procesarTokensOI()

    ! Calculos inicia los cálculos con la lista de información
    CALL iniciarCalculos()

    ! Grafica genera la gráfica con la información de los cálculos
    CALL generarGrafica()



END PROGRAM Main
