PROGRAM Main
    USE ExtractorModule  ! Usamos el módulo que contiene 'ListaCaracteres'
    USE LexicoOne  ! Usamos el módulo LexicoOne para el análisis léxico
    USE LexicoTwo  ! Usamos el módulo LexicoTwo para limpiar la lista y eliminar "Sin definir"
    IMPLICIT NONE

    INTEGER :: i, numLineas
    CHARACTER(LEN=200), ALLOCATABLE :: texto(:)  ! Arreglo dinámico de texto
    CHARACTER(LEN=200) :: linea  ! Para leer cada línea de entrada
    TYPE(ListaCaracteres) :: lista  ! Variable para almacenar los caracteres
    TYPE(ListaNoReconocidos) :: noReconocidos  ! Lista para caracteres no reconocidos

    ! Leer número de líneas a procesar
    PRINT *, 'Ingrese el número de líneas de texto:'
    READ(*,*) numLineas

    ! Asignamos espacio para las líneas
    ALLOCATE(texto(numLineas))

    ! Leer las líneas de texto
    PRINT *, 'Ingrese el texto línea por línea:'
    DO i = 1, numLineas
        READ(*,'(A)') linea
        texto(i) = linea
    END DO

    ! Procesar el texto y extraer los caracteres
    CALL extraer_caracteres(texto, lista)

    ! Analizar los caracteres y asignar tokens
    CALL analizar_lexico(lista, noReconocidos)

    ! Imprimir los resultados del análisis léxico
    CALL imprimir_analisis(lista, noReconocidos)

    ! Limpiar la lista eliminando los caracteres con token "Sin definir"
    !CALL limpiar_lista(lista)

    ! Imprimir la lista filtrada sin los "Sin definir"
    CALL imprimir_lista_limpia(lista)

    ! Liberamos la memoria
    DEALLOCATE(texto)

END PROGRAM Main
