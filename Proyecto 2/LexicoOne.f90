! Archivo: LexicoOne.f90
MODULE LexicoOne
    USE CaracterModule
    USE ExtractorModule
    IMPLICIT NONE
    PUBLIC :: analizar_lexico

    TYPE :: ListaNoReconocidos
        TYPE(Caracter), ALLOCATABLE :: caracteres(:)  ! Lista dinámica para caracteres no reconocidos
    END TYPE ListaNoReconocidos

CONTAINS

    ! Subrutina para analizar los caracteres y asignar tokens
    SUBROUTINE analizar_lexico(lista, noReconocidos)
        TYPE(ListaCaracteres), INTENT(INOUT) :: lista
        TYPE(ListaNoReconocidos), INTENT(OUT) :: noReconocidos
        INTEGER :: i, total_no_reconocidos
        CHARACTER(LEN=1) :: caracter_actual

        total_no_reconocidos = 0

        DO i = 1, SIZE(lista%caracteres)
            caracter_actual = lista%caracteres(i)%valor

            SELECT CASE (caracter_actual)
                CASE (' ', CHAR(9), CHAR(10))  ! Espacios, tabulaciones o retornos
                    lista%caracteres(i)%token = 'espacio'

                CASE ('A':'Z', 'a':'z')  ! Todas las letras
                    lista%caracteres(i)%token = 'letra'

                CASE ('<')  ! Simbolo de apertura de bloque
                    lista%caracteres(i)%token = 'simbolo apertura bloque'
                
                CASE ('>')  ! Simbolo de apertura de bloque
                    lista%caracteres(i)%token = 'simbolo cierre bloque'

                CASE ('-')  ! Simbolo de bloque
                    lista%caracteres(i)%token = 'simbolo bloque'

                CASE ('!')  ! Simbolo de bloque
                    lista%caracteres(i)%token = 'simbolo bloque'

                CASE ('/')  ! Simbolo de comentario
                    lista%caracteres(i)%token = 'simbolo comentario'

                CASE ('*')  ! Simbolo de comentario
                    lista%caracteres(i)%token = 'simbolo comentario'

                CASE ('0':'9')  ! Todos los números
                    lista%caracteres(i)%token = 'digito'

                CASE (';')  ! Simbolo de cierre de línea
                    lista%caracteres(i)%token = 'simbolo cierre linea'

                CASE (',')  ! Simbolo de separación
                    lista%caracteres(i)%token = 'simbolo separacion'

                CASE ('.')  ! Simbolo de separación
                    lista%caracteres(i)%token = 'simbolo separacion'

                CASE ('"')  ! Comillas
                    lista%caracteres(i)%token = 'comillas'
                
                CASE ('(')  ! abrir partentsis
                    lista%caracteres(i)%token = 'símbolo apertura'
                
                CASE (')')  ! cerrar partentsis
                    lista%caracteres(i)%token = 'símbolo cierre'

                CASE DEFAULT
                    lista%caracteres(i)%token = 'desconocido'
                    ! Aumentamos el contador de no reconocidos y los añadimos a la lista de no reconocidos
                    total_no_reconocidos = total_no_reconocidos + 1
            END SELECT
        END DO

        ! Ahora reservamos espacio en la lista de no reconocidos
        ALLOCATE(noReconocidos%caracteres(total_no_reconocidos))

        ! Recorremos la lista para agregar los no reconocidos a la lista correspondiente
        total_no_reconocidos = 0
        DO i = 1, SIZE(lista%caracteres)
            IF (lista%caracteres(i)%token == 'desconocido') THEN
                total_no_reconocidos = total_no_reconocidos + 1
                noReconocidos%caracteres(total_no_reconocidos) = lista%caracteres(i)
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