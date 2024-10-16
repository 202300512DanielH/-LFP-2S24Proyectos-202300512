PROGRAM Main
    USE ExtractorModule  ! Usamos el módulo que contiene 'ListaCaracteres'
    USE LexicoOne  ! Usamos el módulo LexicoOne para el análisis léxico
    USE LexicoTwo  ! Usamos el módulo LexicoTwo para limpiar la lista y eliminar "Sin definir"
    USE LexicoThree  ! Usamos el módulo LexicoThree para agregar 'Léxico' a la lista y encontrar palabras
    IMPLICIT NONE

    INTEGER :: i, numLineas
    CHARACTER(LEN=200), ALLOCATABLE :: texto(:)  ! Arreglo dinámico de texto
    CHARACTER(LEN=200) :: linea  ! Para leer cada línea de entrada
    TYPE(ListaCaracteres) :: lista  ! Variable para almacenar los caracteres
    TYPE(ListaNoReconocidos) :: noReconocidos  ! Lista para caracteres no reconocidos
    TYPE(ListaLexico) :: lista_lexico  ! Lista para caracteres con 'Léxico' agregado
    TYPE(ListaLexico) :: lista_no_reconocidos_lexico  ! Lista para caracteres no reconocidos con 'Léxico' agregado
    TYPE(ListaLexico) :: lista_palabras  ! Lista para almacenar las palabras encontradas

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

    ! Limpiar la lista eliminando los caracteres con token "Sin definir"
    CALL limpiar_lista(lista)

    ! Agregar 'Léxico' a cada carácter de la lista limpia
    CALL agregar_lexico(lista, lista_lexico)

    ! Encontrar las palabras en la lista de caracteres
    CALL encontrar_palabras(lista_lexico, lista_palabras)

    ! Imprimir los caracteres reconocidos, no reconocidos y las palabras encontradas
    CALL imprimir_lista_con_lexico(lista_lexico, noReconocidos, lista_palabras)

    ! Liberamos la memoria
    DEALLOCATE(texto)

END PROGRAM Main
