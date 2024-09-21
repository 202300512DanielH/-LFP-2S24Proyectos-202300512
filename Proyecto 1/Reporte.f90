! archivo: Reporte.f90
MODULE Reporte
    USE Automata       ! Importamos el módulo Automata para acceder a los tokens
    IMPLICIT NONE

CONTAINS

    SUBROUTINE generarReporte()
        INTEGER :: i, numElementos
        CHARACTER(LEN=500) :: lineaHTML
        CHARACTER(LEN=10) :: strNumero, strFila, strColumna
        LOGICAL :: hayNoReconocidos

        ! Verificar si hay tokens no reconocidos
        hayNoReconocidos = SIZE(noReconocidos) > 0

        ! Abrir archivo HTML para escribir
        OPEN(UNIT=10, FILE="reporte.html", STATUS="REPLACE", ACTION="WRITE", IOSTAT=numElementos)
        IF (numElementos /= 0) THEN
            PRINT *, "Error al abrir el archivo de reporte. Código de error:", numElementos
            RETURN
        END IF

        ! Escribir encabezado HTML con enlace a la hoja de estilo CSS
        WRITE(10, '(A)') "<html>"
        WRITE(10, '(A)') "<head><title>Reporte de Analisis Lexico</title>"
        WRITE(10, '(A)') "<link rel='stylesheet' type='text/css' href='Reportes.css'>"
        WRITE(10, '(A)') "</head>"
        WRITE(10, '(A)') "<body>"

        IF (.NOT. hayNoReconocidos) THEN
            ! No hay tokens no reconocidos: reporte de tokens reconocidos
            WRITE(10, '(A)') "<h1> Reporte de Tokens Reconocidos </h1>"
            WRITE(10, '(A)') "<table class='tabla-tokens' border='1'><tr><th>Numero</th><th>Lexema</th><th>Tipo</th><th>Fila</th><th>Columna</th></tr>"

            IF (SIZE(reconocidos) > 0) THEN
                DO i = 1, SIZE(reconocidos)
                    WRITE(strNumero, '(I10)') i  ! Convertir número a cadena
                    WRITE(strFila, '(I10)') reconocidos(i)%fila  ! Convertir fila a cadena
                    WRITE(strColumna, '(I10)') reconocidos(i)%columna  ! Convertir columna a cadena

                    lineaHTML = "<tr><td>" // TRIM(ADJUSTL(strNumero)) // "</td><td>" // TRIM(reconocidos(i)%valor) // "</td><td>" // TRIM(reconocidos(i)%tipo) // "</td><td>" // TRIM(ADJUSTL(strFila)) // "</td><td>" // TRIM(ADJUSTL(strColumna)) // "</td></tr>"
                    WRITE(10, '(A)') TRIM(lineaHTML)
                END DO
            END IF

            WRITE(10, '(A)') "</table>"
        ELSE
            ! Hay tokens no reconocidos: reporte de errores léxicos
            WRITE(10, '(A)') "<h1> Reporte de Errores Lexicos </h1>"
            WRITE(10, '(A)') "<table class='tabla-tokens' border='1'><tr><th>Numero</th><th>Lexema de Error</th><th>Elemento Lexico Desconocido</th><th>Fila</th><th>Columna</th></tr>"

            IF (SIZE(noReconocidos) > 0) THEN
                DO i = 1, SIZE(noReconocidos)
                    WRITE(strNumero, '(I10)') i  ! Convertir número a cadena
                    WRITE(strFila, '(I10)') noReconocidos(i)%fila  ! Convertir fila a cadena
                    WRITE(strColumna, '(I10)') noReconocidos(i)%columna  ! Convertir columna a cadena

                    lineaHTML = "<tr><td>" // TRIM(ADJUSTL(strNumero)) // "</td><td>" // TRIM(noReconocidos(i)%valor) // "</td><td>Elemento léxico desconocido</td><td>" // TRIM(ADJUSTL(strFila)) // "</td><td>" // TRIM(ADJUSTL(strColumna)) // "</td></tr>"
                    WRITE(10, '(A)') TRIM(lineaHTML)
                END DO
            END IF

            WRITE(10, '(A)') "</table>"
        END IF

        ! Escribir pie de página HTML y cerrar archivo
        WRITE(10, '(A)') "</body></html>"
        CLOSE(10)

        !PRINT *, "Reporte generado correctamente en 'reporte.html'."
    END SUBROUTINE generarReporte

END MODULE Reporte
