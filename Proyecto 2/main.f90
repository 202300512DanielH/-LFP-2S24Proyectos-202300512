PROGRAM Main
    USE ExtractorModule  ! Usamos el módulo que contiene 'ListaCaracteres'
    IMPLICIT NONE

    INTEGER :: i, numLineas
    CHARACTER(LEN=200), ALLOCATABLE :: texto(:)  ! Declaración correcta del arreglo dinámico de texto
    CHARACTER(LEN=200) :: linea  ! Para leer cada línea de entrada
    TYPE(ListaCaracteres) :: lista  ! Cambio el nombre de la variable para evitar conflicto con el tipo

    ! Leer número de líneas a procesar
    READ(*,*) numLineas

    ! Asignamos espacio para las líneas
    ALLOCATE(texto(numLineas))

    ! Leer las líneas de texto
    DO i = 1, numLineas
        READ(*,'(A)') linea
        texto(i) = linea
    END DO

    ! Procesar el texto y extraer los caracteres
    CALL extraer_caracteres(texto, lista)

    ! Imprimir los caracteres extraídos (puedes cambiar esto para realizar otro tipo de procesamiento)
    CALL imprimir_caracteres(lista)

    ! Liberamos la memoria
    DEALLOCATE(texto)

END PROGRAM Main
