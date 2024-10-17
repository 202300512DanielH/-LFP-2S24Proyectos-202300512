MODULE LexicoOne
    USE CaracterModule
    USE ExtractorModule
    IMPLICIT NONE
    PUBLIC :: analizar_lexico, imprimir_analisis

    TYPE :: ListaErrores
        TYPE(Caracter), ALLOCATABLE :: caracteres(:)  ! Lista dinámica para caracteres no reconocidos
    END TYPE ListaErrores

CONTAINS

SUBROUTINE analizar_lexico(lista, errores)
    TYPE(ListaCaracteres), INTENT(INOUT) :: lista
    TYPE(ListaErrores), INTENT(OUT) :: errores
    INTEGER :: i, j, total_errores, total_reconocidos
    CHARACTER(LEN=1) :: caracter_actual
    LOGICAL :: en_comentario_bloque

    total_errores = 0
    total_reconocidos = 0
    en_comentario_bloque = .FALSE.

    i = 1
    DO WHILE (i <= SIZE(lista%caracteres))
        caracter_actual = lista%caracteres(i)%valor

        ! Detectar inicio de comentario de bloque "/*"
        IF (caracter_actual == '/' .AND. i < SIZE(lista%caracteres)) THEN
            IF (lista%caracteres(i + 1)%valor == '*') THEN
                en_comentario_bloque = .TRUE.
                lista%caracteres(i)%token = 'comentario'
                lista%caracteres(i)%tipo = 'Léxico'
                lista%caracteres(i)%descripcion = 'Comentario de bloque'
                total_reconocidos = total_reconocidos + 1
                lista%caracteres(i)%correlativo = total_reconocidos
                i = i + 1  ! Avanzar al '*'
            END IF
        END IF

        ! Procesar caracteres dentro del comentario de bloque
        IF (en_comentario_bloque) THEN
            lista%caracteres(i)%token = 'comentario'
            lista%caracteres(i)%tipo = 'Léxico'
            lista%caracteres(i)%descripcion = 'Comentario de bloque'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

            ! Detectar el final del comentario de bloque "*/"
            IF (caracter_actual == '*' .AND. i < SIZE(lista%caracteres)) THEN
                IF (lista%caracteres(i + 1)%valor == '/') THEN
                    lista%caracteres(i + 1)%token = 'comentario'
                    lista%caracteres(i + 1)%tipo = 'Léxico'
                    lista%caracteres(i + 1)%descripcion = 'Comentario de bloque'
                    total_reconocidos = total_reconocidos + 1
                    lista%caracteres(i + 1)%correlativo = total_reconocidos
                    en_comentario_bloque = .FALSE.
                    i = i + 2  ! Saltar "*/"
                    CYCLE
                END IF
            END IF

            i = i + 1
            CYCLE
        END IF

        ! Detectar comentario de línea "//"
        IF (caracter_actual == '/' .AND. i < SIZE(lista%caracteres)) THEN
            IF (lista%caracteres(i + 1)%valor == '/') THEN
                DO
                    lista%caracteres(i)%token = 'comentario'
                    lista%caracteres(i)%tipo = 'Léxico'
                    lista%caracteres(i)%descripcion = 'Comentario de línea'
                    total_reconocidos = total_reconocidos + 1
                    lista%caracteres(i)%correlativo = total_reconocidos
                    i = i + 1
                    IF (i > SIZE(lista%caracteres) .OR. &
                        lista%caracteres(i)%posicionFila /= lista%caracteres(i - 1)%posicionFila) EXIT
                END DO
                CYCLE
            END IF
        END IF

        ! Asignación de tokens para otros caracteres
        SELECT CASE (caracter_actual)
        CASE (' ', CHAR(9), CHAR(10))
            lista%caracteres(i)%token = 'espacio'
            lista%caracteres(i)%descripcion = 'Espacio en blanco'
        
        CASE ('A':'Z', 'a':'z')
            lista%caracteres(i)%token = 'letra'
            lista%caracteres(i)%descripcion = 'Letra'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE ('<')
            lista%caracteres(i)%token = 'simboloAperturaBloque'
            lista%caracteres(i)%descripcion = 'Apertura de bloque'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE ('>')
            lista%caracteres(i)%token = 'simboloCierreBloque'
            lista%caracteres(i)%descripcion = 'Cierre de bloque'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE ('-')
            lista%caracteres(i)%token = 'simboloBloque-'
            lista%caracteres(i)%descripcion = 'Símbolo de bloque -'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos
        
        CASE ('!')
            lista%caracteres(i)%token = 'simboloBloque!'
            lista%caracteres(i)%descripcion = 'Símbolo de bloque !'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE ('0':'9')
            lista%caracteres(i)%token = 'digito'
            lista%caracteres(i)%descripcion = 'Dígito'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE (';')
            lista%caracteres(i)%token = 'separadorLinea'
            lista%caracteres(i)%descripcion = 'Separador de línea'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE (',')
            lista%caracteres(i)%token = 'SeparadorComa'
            lista%caracteres(i)%descripcion = 'Separador de coma'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos
        
        CASE ('.')
            lista%caracteres(i)%token = 'SeparadorPunto'
            lista%caracteres(i)%descripcion = 'Separador de punto'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE ('(')
            lista%caracteres(i)%token = 'parentesisAbierto'
            lista%caracteres(i)%descripcion = 'Paréntesis de apertura'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos
        
        CASE (')')
            lista%caracteres(i)%token = 'parentesisCerrado'
            lista%caracteres(i)%descripcion = 'Paréntesis de cierre'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE ('"')
            lista%caracteres(i)%token = 'comillas'
            lista%caracteres(i)%descripcion = 'Comillas'
            total_reconocidos = total_reconocidos + 1
            lista%caracteres(i)%correlativo = total_reconocidos

        CASE DEFAULT
            lista%caracteres(i)%token = 'desconocido'
            lista%caracteres(i)%descripcion = 'Token no reconocido'
            total_errores = total_errores + 1
        END SELECT

        lista%caracteres(i)%tipo = 'Léxico'
        i = i + 1
    END DO

    ALLOCATE(errores%caracteres(total_errores))

    total_errores = 0  

    DO j = 1, SIZE(lista%caracteres)
        IF (lista%caracteres(j)%token == 'desconocido') THEN
            total_errores = total_errores + 1
            errores%caracteres(total_errores) = lista%caracteres(j)
            errores%caracteres(total_errores)%correlativo = total_errores
        END IF
    END DO
END SUBROUTINE analizar_lexico




    SUBROUTINE imprimir_analisis(lista, errores)
        TYPE(ListaCaracteres), INTENT(IN) :: lista
        TYPE(ListaErrores), INTENT(IN) :: errores
        INTEGER :: i

        PRINT *, '--- Carácteres Reconocidos ---'
        DO i = 1, SIZE(lista%caracteres)
            IF (lista%caracteres(i)%token /= 'desconocido') THEN
                PRINT *, 'Carácter: ', lista%caracteres(i)%valor, &
                         ' Token: ', lista%caracteres(i)%token, &
                         ' Tipo: ', lista%caracteres(i)%tipo, &
                         ' Descripción: ', lista%caracteres(i)%descripcion, &
                         ' Fila: ', lista%caracteres(i)%posicionFila, &
                         ' Columna: ', lista%caracteres(i)%posicionColumna, &
                         ' Correlativo: ', lista%caracteres(i)%correlativo
            END IF
        END DO

        PRINT *, '--- Carácteres No Reconocidos ---'
        IF (SIZE(errores%caracteres) == 0) THEN
            PRINT *, 'No hay caracteres no reconocidos.'
        ELSE
            DO i = 1, SIZE(errores%caracteres)
                PRINT *, 'Carácter: ', errores%caracteres(i)%valor, &
                         ' Tipo: ', errores%caracteres(i)%tipo, &
                         ' Descripción: ', errores%caracteres(i)%descripcion, &
                         ' Fila: ', errores%caracteres(i)%posicionFila, &
                         ' Columna: ', errores%caracteres(i)%posicionColumna, &
                         ' Correlativo: ', errores%caracteres(i)%correlativo
            END DO
        END IF
    END SUBROUTINE imprimir_analisis

END MODULE LexicoOne
