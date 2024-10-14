! Archivo: LexicoOne.f90
MODULE LexicoOne
    USE CaracterModule
    USE ExtractorModule
    IMPLICIT NONE
    PUBLIC :: analizar_lexico, imprimir_analisis

    TYPE :: ListaNoReconocidos
        TYPE(Caracter), ALLOCATABLE :: caracteres(:)  ! Lista dinámica para caracteres no reconocidos
    END TYPE ListaNoReconocidos

CONTAINS

    ! Subrutina para analizar los caracteres y capturar comentarios de línea
    SUBROUTINE analizar_lexico(lista, noReconocidos)
        TYPE(ListaCaracteres), INTENT(INOUT) :: lista
        TYPE(ListaNoReconocidos), INTENT(OUT) :: noReconocidos
        INTEGER :: i, j, total_no_reconocidos, fila_actual
        CHARACTER(LEN=1) :: caracter_actual
        LOGICAL :: en_comentario_bloque
        CHARACTER(LEN=500) :: comentario_linea

        total_no_reconocidos = 0
        en_comentario_bloque = .FALSE.
        comentario_linea = ""

        ! Realizamos el análisis léxico
        i = 1
        DO WHILE (i <= SIZE(lista%caracteres))
            caracter_actual = lista%caracteres(i)%valor
            fila_actual = lista%caracteres(i)%posicionFila  ! Obtenemos el número de fila actual

            ! Ignorar caracteres dentro de un comentario de bloque
            IF (en_comentario_bloque) THEN
                ! Detectar el final del comentario de bloque "*/"
                IF (caracter_actual == '*' .AND. i < SIZE(lista%caracteres)) THEN
                    IF (lista%caracteres(i + 1)%valor == '/') THEN
                        en_comentario_bloque = .FALSE.  ! Fin del comentario de bloque
                        i = i + 2  ! Saltar "*/"
                        CYCLE
                    END IF
                END IF
                i = i + 1
                CYCLE  ! Seguir ignorando mientras estamos en un comentario de bloque
            END IF

            ! Detectar el inicio de un comentario de bloque "/*"
            IF (caracter_actual == '/' .AND. i < SIZE(lista%caracteres)) THEN
                IF (lista%caracteres(i + 1)%valor == '*') THEN
                    en_comentario_bloque = .TRUE.
                    i = i + 2  ! Saltar "/*"
                    CYCLE
                END IF
            END IF

            ! Detectar el inicio de un comentario de línea "//"
            IF (caracter_actual == '/' .AND. i < SIZE(lista%caracteres)) THEN
                IF (lista%caracteres(i + 1)%valor == '/') THEN
                    ! Capturamos el comentario completo hasta el final de la línea
                    comentario_linea = comentario_linea // 'Comentario detectado: '
                    i = i + 2  ! Saltar el "//"
                    DO WHILE (i <= SIZE(lista%caracteres))
                        IF (lista%caracteres(i)%posicionFila /= fila_actual) THEN
                            EXIT  ! Terminamos el comentario al cambiar de fila
                        END IF
                        comentario_linea = comentario_linea // '"' // TRIM(lista%caracteres(i)%valor) // '" '
                        i = i + 1
                    END DO
                    PRINT *, TRIM(comentario_linea)  ! Imprimir el comentario detectado
                    comentario_linea = ""  ! Limpiar la variable para el siguiente comentario
                    CYCLE  ! Continuar con la siguiente iteración después del comentario
                END IF
            END IF

            ! Asignación de tokens normales si no estamos en un comentario
            SELECT CASE (caracter_actual)
                CASE (' ', CHAR(9), CHAR(10))  ! Espacios, tabulaciones o retornos
                    lista%caracteres(i)%token = 'espacio'

                CASE ('A':'Z', 'a':'z')  ! Todas las letras
                    lista%caracteres(i)%token = 'letra'

                CASE ('<')  ! Símbolo de apertura de bloque
                    lista%caracteres(i)%token = 'simbolo apertura bloque'
                
                CASE ('>')  ! Símbolo de cierre de bloque
                    lista%caracteres(i)%token = 'simbolo cierre bloque'

                CASE ('-')  ! Símbolo de bloque
                    lista%caracteres(i)%token = 'simbolo bloque'

                CASE ('!')  ! Símbolo de bloque
                    lista%caracteres(i)%token = 'simbolo bloque'

                CASE ('/')  ! Símbolo de comentario
                    lista%caracteres(i)%token = 'simbolo comentario'

                CASE ('*')  ! Símbolo de comentario
                    lista%caracteres(i)%token = 'simbolo comentario'

                CASE ('0':'9')  ! Todos los números
                    lista%caracteres(i)%token = 'digito'

                CASE (';')  ! Símbolo de cierre de línea
                    lista%caracteres(i)%token = 'simbolo cierre linea'

                CASE (',')  ! Símbolo de separación
                    lista%caracteres(i)%token = 'simbolo separacion'

                CASE ('.')  ! Símbolo de separación
                    lista%caracteres(i)%token = 'simbolo separacion'

                CASE ('"')  ! Comillas
                    lista%caracteres(i)%token = 'comillas'
                
                CASE ('(')  ! Símbolo apertura paréntesis
                    lista%caracteres(i)%token = 'simbolo apertura'

                CASE (')')  ! Símbolo cierre paréntesis
                    lista%caracteres(i)%token = 'simbolo cierre'

                CASE DEFAULT
                    lista%caracteres(i)%token = 'desconocido'
                    total_no_reconocidos = total_no_reconocidos + 1
            END SELECT

            i = i + 1  ! Avanzamos al siguiente carácter
        END DO

        ! Reservamos espacio en la lista de no reconocidos
        ALLOCATE(noReconocidos%caracteres(total_no_reconocidos))

        ! Recorremos la lista para agregar los no reconocidos
        total_no_reconocidos = 0
        DO j = 1, SIZE(lista%caracteres)
            IF (lista%caracteres(j)%token == 'desconocido') THEN
                total_no_reconocidos = total_no_reconocidos + 1
                noReconocidos%caracteres(total_no_reconocidos) = lista%caracteres(j)
            END IF
        END DO

    END SUBROUTINE analizar_lexico

    ! Subrutina para imprimir los caracteres y sus tokens reconocidos
    SUBROUTINE imprimir_analisis(lista, noReconocidos)
        TYPE(ListaCaracteres), INTENT(IN) :: lista
        TYPE(ListaNoReconocidos), INTENT(IN) :: noReconocidos
        INTEGER :: i

        PRINT *, '--- Carácteres Reconocidos ---'
        DO i = 1, SIZE(lista%caracteres)
            IF (lista%caracteres(i)%token /= 'desconocido') THEN
                PRINT *, 'Carácter: ', lista%caracteres(i)%valor, &
                         ' Token: ', lista%caracteres(i)%token, &
                         ' Fila: ', lista%caracteres(i)%posicionFila, &
                         ' Columna: ', lista%caracteres(i)%posicionColumna
            END IF
        END DO

        PRINT *, '--- Carácteres No Reconocidos ---'
        IF (SIZE(noReconocidos%caracteres) == 0) THEN
            PRINT *, 'No hay caracteres no reconocidos.'
        ELSE
            DO i = 1, SIZE(noReconocidos%caracteres)
                PRINT *, 'Carácter: ', noReconocidos%caracteres(i)%valor, &
                         ' Fila: ', noReconocidos%caracteres(i)%posicionFila, &
                         ' Columna: ', noReconocidos%caracteres(i)%posicionColumna
            END DO
        END IF
    END SUBROUTINE imprimir_analisis

END MODULE LexicoOne
