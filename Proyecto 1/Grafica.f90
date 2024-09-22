MODULE Grafica
    USE Calculos  ! Importar el módulo Calculos para acceder a los datos
    USE Automata ! Importar el módulo Automata para acceder a las funciones de validación
    IMPLICIT NONE

CONTAINS

SUBROUTINE generarGrafica()
    INTEGER :: i, j
    CHARACTER(LEN=100) :: nombreContinente, nombrePais, tempNombreGrafica, color
    INTEGER :: saturacionContinente, saturacionPais
    CHARACTER(LEN=256) :: nombreArchivoDot

    ! Verificar si hay tokens no reconocidos
    IF (ALLOCATED(noReconocidos) .AND. SIZE(noReconocidos) > 0) THEN
        PRINT *, "N/A"
        ! Crear un archivo .dot vacío
        nombreArchivoDot = 'grafico_vacio.dot'
        OPEN(UNIT=10, FILE=nombreArchivoDot, STATUS='REPLACE')
        WRITE(10,*) 'digraph G {}'
        CLOSE(10)

        ! Generar la imagen PNG en blanco
        CALL SYSTEM("dot -Tpng grafico_vacio.dot -o grafico.png")
        PRINT *, "Archivo .png en blanco generado con éxito: grafico_vacio.png"
        RETURN
    END IF

        ! Verificar si los datos están asignados
        IF (TRIM(nombreGrafica) == "") THEN
            PRINT *, "Error: El nombre de la gráfica no está asignado."
            RETURN
        END IF

        IF (.NOT. ALLOCATED(continentes)) THEN
            PRINT *, "Error: La lista de continentes no está asignada."
            RETURN
        END IF

        ! Definir nombre del archivo .dot
        nombreArchivoDot = 'grafico.dot'

        ! Abrir el archivo .dot para escribir
        OPEN(UNIT=10, FILE=nombreArchivoDot, STATUS='REPLACE')

        ! Escribir encabezado del archivo .dot
        WRITE(10,*) 'digraph G {'
        WRITE(10,*) '    node [shape=box, style=filled];'

        ! Procesar y eliminar comillas dobles en el nombre de la gráfica
        tempNombreGrafica = TRIM(nombreGrafica)
        CALL eliminarComillasDobles(tempNombreGrafica)

        ! Escribir el nodo raíz (nombre de la gráfica) sin color
        WRITE(10,*) '    "', TRIM(tempNombreGrafica), '" [label="', TRIM(tempNombreGrafica), '"];'

        ! Recorrer los continentes y escribir nodos y conexiones
        DO i = 1, SIZE(continentes)
            ! Procesar nombre del continente y su saturación
            nombreContinente = TRIM(continentes(i)%nombre)
            CALL eliminarComillasDobles(nombreContinente)
            saturacionContinente = INT(continentes(i)%saturacion + 0.5)  ! Redondear la saturación

            ! Obtener color para el continente basado en su saturación
            color = getColor(continentes(i)%saturacion)

            ! Escribir el nodo del continente con el color y la conexión con el nodo raíz
            WRITE(10,*) '    "', TRIM(tempNombreGrafica), '" -> "', TRIM(nombreContinente), '";'
            WRITE(10,*) '    "', TRIM(nombreContinente), '" [label="', TRIM(nombreContinente), ' Saturación: ', saturacionContinente, '", fillcolor=', color, '];'

            ! Recorrer los países del continente y conectarlos al continente
            DO j = 1, SIZE(continentes(i)%paises)
                nombrePais = TRIM(continentes(i)%paises(j)%nombre)
                CALL eliminarComillasDobles(nombrePais)
                saturacionPais = INT(continentes(i)%paises(j)%saturacion + 0.5)  ! Redondear la saturación

                ! Obtener color para el país basado en su saturación
                color = getColor(continentes(i)%paises(j)%saturacion)

                ! Escribir el nodo del país con el color y la conexión con el continente
                WRITE(10,*) '    "', TRIM(nombreContinente), '" -> "', TRIM(nombrePais), '";'
                WRITE(10,*) '    "', TRIM(nombrePais), '" [label="', TRIM(nombrePais), ' Saturación: ', saturacionPais, '", fillcolor=', color, '];'
            END DO
        END DO

        ! Escribir el cierre del archivo .dot
        WRITE(10,*) '}'

        ! Cerrar el archivo
        CLOSE(10)

        PRINT *, "Archivo .dot generado con éxito:", TRIM(nombreArchivoDot)

        ! Generar el archivo PNG usando Graphviz (comando system)
        CALL SYSTEM("dot -Tpng grafico.dot -o grafico.png")
        PRINT *, "Archivo .png generado con éxito: grafico.png"

    END SUBROUTINE generarGrafica

    ! Función para obtener el color basado en la saturación
    FUNCTION getColor(saturation) RESULT(color)
        IMPLICIT NONE
        REAL, INTENT(IN) :: saturation
        CHARACTER(LEN=10) :: color
    
        IF (saturation <= 15.0) THEN
            color = '"white"'
        ELSE IF (saturation <= 30.0) THEN
            color = '"blue"'
        ELSE IF (saturation <= 45.0) THEN
            color = '"green"'
        ELSE IF (saturation <= 60.0) THEN
            color = '"yellow"'
        ELSE IF (saturation <= 75.0) THEN
            color = '"orange"'
        ELSE
            color = '"red"'
        END IF
    END FUNCTION getColor

    ! Subrutina para eliminar las comillas dobles de una cadena
    SUBROUTINE eliminarComillasDobles(cadena)
        CHARACTER(LEN=*), INTENT(INOUT) :: cadena
        INTEGER :: startPos, endPos

        startPos = INDEX(cadena, '"')  ! Buscar la primera comilla
        IF (startPos /= 0) THEN
            cadena = cadena(startPos + 1:)  ! Eliminar la comilla inicial
        END IF

        endPos = INDEX(cadena, '"')  ! Buscar la segunda comilla
        IF (endPos /= 0) THEN
            cadena = cadena(:endPos - 1)  ! Eliminar la comilla final
        END IF
    END SUBROUTINE eliminarComillasDobles

END MODULE Grafica
