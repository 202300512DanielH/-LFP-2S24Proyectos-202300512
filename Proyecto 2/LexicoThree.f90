! Archivo: LexicoThree.f90
MODULE LexicoThree
    USE CaracterModule
    USE ExtractorModule
    USE LexicoOne
    IMPLICIT NONE
    PUBLIC :: agregar_lexico, agregar_lexico_no_reconocidos, imprimir_lista_con_lexico, encontrar_palabras

    TYPE :: ListaLexico
        TYPE(Caracter), ALLOCATABLE :: caracteres(:)  ! Lista dinámica de objetos 'Caracter'
    END TYPE ListaLexico

CONTAINS

    ! Subrutina para agregar 'Léxico' a cada carácter de la lista limpia
    SUBROUTINE agregar_lexico(lista_limpia, lista_lexico)
        TYPE(ListaCaracteres), INTENT(IN) :: lista_limpia
        TYPE(ListaLexico), INTENT(OUT) :: lista_lexico
        INTEGER :: i, total_caracteres

        ! Contar el número de caracteres en la lista limpia
        total_caracteres = SIZE(lista_limpia%caracteres)

        ! Asignar espacio para la lista dinámica 'lista_lexico'
        ALLOCATE(lista_lexico%caracteres(total_caracteres))

        ! Recorrer la lista limpia y copiar los caracteres, agregando 'Léxico' a su valor
        DO i = 1, total_caracteres
            lista_lexico%caracteres(i)%valor = lista_limpia%caracteres(i)%valor
            lista_lexico%caracteres(i)%tipo = 'Léxico'
            lista_lexico%caracteres(i)%posicionFila = lista_limpia%caracteres(i)%posicionFila
            lista_lexico%caracteres(i)%posicionColumna = lista_limpia%caracteres(i)%posicionColumna
            IF (lista_limpia%caracteres(i)%valor == '.') THEN
                lista_lexico%caracteres(i)%token = 'punto'
            ELSE IF (lista_limpia%caracteres(i)%valor == ';') THEN
                lista_lexico%caracteres(i)%token = 'punto y coma'
            ELSE IF (lista_limpia%caracteres(i)%valor == ',') THEN
                lista_lexico%caracteres(i)%token = 'coma'
            ELSE IF (lista_limpia%caracteres(i)%valor == '(' .OR. lista_limpia%caracteres(i)%valor == ')') THEN
                lista_lexico%caracteres(i)%token = 'paréntesis'
            ELSE IF (lista_limpia%caracteres(i)%valor >= '0' .AND. lista_limpia%caracteres(i)%valor <= '9') THEN
                lista_lexico%caracteres(i)%token = 'digito'
            ELSE
                lista_lexico%caracteres(i)%token = lista_limpia%caracteres(i)%token
            END IF
        END DO
    END SUBROUTINE agregar_lexico

    ! Subrutina para agregar 'Léxico' a cada carácter de la lista de no reconocidos
    SUBROUTINE agregar_lexico_no_reconocidos(lista_no_reconocidos, lista_lexico)
        TYPE(ListaNoReconocidos), INTENT(IN) :: lista_no_reconocidos
        TYPE(ListaLexico), INTENT(OUT) :: lista_lexico
        INTEGER :: i, total_caracteres

        ! Contar el número de caracteres en la lista de no reconocidos
        total_caracteres = SIZE(lista_no_reconocidos%caracteres)

        ! Asignar espacio para la lista dinámica 'lista_lexico'
        ALLOCATE(lista_lexico%caracteres(total_caracteres))

        ! Recorrer la lista de no reconocidos y copiar los caracteres, agregando 'Léxico' a su valor
        DO i = 1, total_caracteres
            lista_lexico%caracteres(i)%valor = lista_no_reconocidos%caracteres(i)%valor
            lista_lexico%caracteres(i)%tipo = 'Léxico'
            lista_lexico%caracteres(i)%posicionFila = lista_no_reconocidos%caracteres(i)%posicionFila
            lista_lexico%caracteres(i)%posicionColumna = lista_no_reconocidos%caracteres(i)%posicionColumna
            lista_lexico%caracteres(i)%token = 'Léxico'
        END DO
    END SUBROUTINE agregar_lexico_no_reconocidos

    ! Subrutina para imprimir los caracteres de la lista con 'Léxico' agregado
    SUBROUTINE imprimir_lista_con_lexico(lista_lexico, lista_no_reconocidos, lista_palabras)
        TYPE(ListaLexico), INTENT(IN) :: lista_lexico
        TYPE(ListaNoReconocidos), INTENT(IN) :: lista_no_reconocidos
        TYPE(ListaLexico), INTENT(IN) :: lista_palabras
        INTEGER :: i

        PRINT *, '--- Errores ---'
        IF (SIZE(lista_no_reconocidos%caracteres) == 0) THEN
            PRINT *, 'No hay caracteres no reconocidos.'
        ELSE
            DO i = 1, SIZE(lista_no_reconocidos%caracteres)
                PRINT *, 'Valor: ', lista_no_reconocidos%caracteres(i)%valor, &
                         ' Tipo: ', lista_no_reconocidos%caracteres(i)%tipo, &
                         ' Fila: ', lista_no_reconocidos%caracteres(i)%posicionFila, &
                         ' Columna: ', lista_no_reconocidos%caracteres(i)%posicionColumna, &
                         ' Token: ', lista_no_reconocidos%caracteres(i)%token
            END DO
        END IF

        PRINT *, '--- Tokens ---'
        IF (SIZE(lista_lexico%caracteres) == 0) THEN
            PRINT *, 'La lista está vacía.'
            RETURN
        END IF

        DO i = 1, SIZE(lista_lexico%caracteres)
            PRINT *, 'Valor: ', lista_lexico%caracteres(i)%valor, &
                     ' Tipo: ', lista_lexico%caracteres(i)%tipo, &
                     ' Fila: ', lista_lexico%caracteres(i)%posicionFila, &
                     ' Columna: ', lista_lexico%caracteres(i)%posicionColumna, &
                     ' Token: ', lista_lexico%caracteres(i)%token
        END DO

        PRINT *, '--- Palabras ---'
        IF (SIZE(lista_palabras%caracteres) == 0) THEN
            PRINT *, 'No se encontraron palabras.'
        ELSE
            DO i = 1, SIZE(lista_palabras%caracteres)
                PRINT *, 'Palabra: ', lista_palabras%caracteres(i)%valor, &
                         ' Fila: ', lista_palabras%caracteres(i)%posicionFila, &
                         ' Columna: ', lista_palabras%caracteres(i)%posicionColumna
            END DO
        END IF
    END SUBROUTINE imprimir_lista_con_lexico

    ! Subrutina para encontrar palabras entre símbolos o espacios
    SUBROUTINE encontrar_palabras(lista_lexico, lista_palabras)
        TYPE(ListaLexico), INTENT(IN) :: lista_lexico
        TYPE(ListaLexico), INTENT(OUT) :: lista_palabras
        INTEGER :: i, j, total_caracteres, palabra_inicio, palabra_longitud
        CHARACTER(LEN=200) :: palabra_actual
        LOGICAL :: en_palabra

        total_caracteres = SIZE(lista_lexico%caracteres)
        en_palabra = .FALSE.
        palabra_longitud = 0

        ! Inicializar lista de palabras
        IF (.NOT. ALLOCATED(lista_palabras%caracteres)) THEN
            ALLOCATE(lista_palabras%caracteres(0))
        ELSE
            DEALLOCATE(lista_palabras%caracteres)
            ALLOCATE(lista_palabras%caracteres(0))
        END IF

        DO i = 1, total_caracteres
            SELECT CASE (lista_lexico%caracteres(i)%token)
                CASE ('letra', 'digito')
                    IF (.NOT. en_palabra) THEN
                        en_palabra = .TRUE.
                        palabra_inicio = i
                        palabra_longitud = 1
                    ELSE
                        palabra_longitud = palabra_longitud + 1
                    END IF
                CASE ('espacio', 'simbolo', 'punto', 'punto y coma', 'paréntesis', 'coma')
                    IF (en_palabra) THEN
                        en_palabra = .FALSE.
                        ! Crear una nueva palabra
                        lista_palabras%caracteres = [lista_palabras%caracteres, Caracter('', 'Léxico', 0, 0, '')]
                        palabra_actual = ''
                        DO j = palabra_inicio, palabra_inicio + palabra_longitud - 1
                            palabra_actual = TRIM(palabra_actual) // lista_lexico%caracteres(j)%valor
                        END DO
                        lista_palabras%caracteres(SIZE(lista_palabras%caracteres))%valor = TRIM(palabra_actual)
                        lista_palabras%caracteres(SIZE(lista_palabras%caracteres))%tipo = 'Léxico'
                        lista_palabras%caracteres(SIZE(lista_palabras%caracteres))%posicionFila = lista_lexico%caracteres(palabra_inicio)%posicionFila
                        lista_palabras%caracteres(SIZE(lista_palabras%caracteres))%posicionColumna = lista_lexico%caracteres(palabra_inicio)%posicionColumna
                        lista_palabras%caracteres(SIZE(lista_palabras%caracteres))%token = ''
                    END IF
            END SELECT
        END DO

    END SUBROUTINE encontrar_palabras

END MODULE LexicoThree