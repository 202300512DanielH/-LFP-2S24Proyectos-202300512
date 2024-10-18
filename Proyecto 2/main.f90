PROGRAM Main
    USE ExtractorModule   ! Contiene 'ListaCaracteres'
    USE LexicoOne         ! Para el análisis léxico
    USE LexicoTwo         ! Filtrado de tokens reconocidos
    IMPLICIT NONE

    INTEGER :: i, numLineas
    CHARACTER(LEN=200), ALLOCATABLE :: texto(:)  ! Arreglo dinámico de texto
    CHARACTER(LEN=200) :: linea                 ! Para leer cada línea de entrada
    TYPE(ListaCaracteres) :: lista              ! Almacena los caracteres extraídos
    TYPE(ListaErrores) :: errores               ! Lista de errores no reconocidos
    TYPE(ListaCaracteres) :: listaLimpia        ! Lista de tokens reconocidos


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

    ! Filtrar tokens reconocidos y generar lista limpia
    CALL filtrar_tokens_reconocidos(lista, listaLimpia)

    CALL imprimir_lista_limpia(listaLimpia)

    ! Imprimir los errores léxicos desde LexicoTwo
    CALL imprimir_errores_lexico_two(errores)

    ! Liberamos la memoria
    DEALLOCATE(texto)

END PROGRAM Main
