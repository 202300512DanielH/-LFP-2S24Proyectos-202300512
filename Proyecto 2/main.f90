PROGRAM Main
    USE ExtractorModule  ! Usamos el módulo que contiene 'ListaCaracteres'
    USE LexicoOne  ! Usamos el módulo LexicoOne para el análisis léxico
    IMPLICIT NONE

    INTEGER :: i, numLineas
    CHARACTER(LEN=200), ALLOCATABLE :: texto(:)  ! Arreglo dinámico de texto
    CHARACTER(LEN=200) :: linea  ! Para leer cada línea de entrada
    TYPE(ListaCaracteres) :: lista  ! Variable para almacenar los caracteres
    TYPE(ListaErrores) :: errores  ! Lista para caracteres no reconocidos

    ! Leer número de líneas a procesar
    READ(*,*) numLineas

    ! Asignamos espacio para las líneas
    ALLOCATE(texto(numLineas))

    ! Leer las líneas de texto
    DO i = 1, numLineas
        READ(*, '(A)') linea
        texto(i) = linea
    END DO

    ! Procesar el texto y extraer los caracteres
    CALL extraer_caracteres(texto, lista)

    ! Analizar los caracteres y asignar tokens, generando lista de errores
    CALL analizar_lexico(lista, errores)

    ! Imprimir los caracteres reconocidos y los errores
    CALL imprimir_analisis(lista, errores)

    ! Liberamos la memoria
    DEALLOCATE(texto)

END PROGRAM Main
