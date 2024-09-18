! archivo: main.f90
PROGRAM Main
    USE ObjetoCaracter   ! Módulo que define el tipo de datos CaracterInfo
    USE Extraccion       ! Módulo que contiene la lógica de extracción de caracteres
    USE Automata         ! Módulo que contiene el autómata para procesar los caracteres
    USE Reporte          ! Módulo que genera el reporte HTML
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

END PROGRAM Main
