MODULE LexicoTwo
    USE CaracterModule    ! Reutilizamos el módulo de Caracter
    USE ExtractorModule   ! Para acceder a 'ListaCaracteres'
    USE LexicoOne, ONLY: ListaErrores   ! Importamos ListaErrores del módulo LexicoOne
    IMPLICIT NONE
    PUBLIC :: filtrar_tokens_reconocidos, imprimir_lista_limpia, imprimir_errores_lexico_two, ListaCaracteres

CONTAINS

    SUBROUTINE filtrar_tokens_reconocidos(lista_original, lista_limpia)
        TYPE(ListaCaracteres), INTENT(IN) :: lista_original
        TYPE(ListaCaracteres), INTENT(OUT) :: lista_limpia
        INTEGER :: i, total_reconocidos

        ! Contar cuántos caracteres tienen tokens reconocidos (excluyendo comentarios y desconocidos)
        total_reconocidos = 0
        DO i = 1, SIZE(lista_original%caracteres)
            IF (lista_original%caracteres(i)%token /= 'comentario' .AND. &
                lista_original%caracteres(i)%token /= 'desconocido') THEN
                total_reconocidos = total_reconocidos + 1
            END IF
        END DO

        ! Asignar espacio para la lista limpia
        ALLOCATE(lista_limpia%caracteres(total_reconocidos))

        ! Copiar solo los caracteres con tokens reconocidos
        total_reconocidos = 0
        DO i = 1, SIZE(lista_original%caracteres)
            IF (lista_original%caracteres(i)%token /= 'comentario' .AND. &
                lista_original%caracteres(i)%token /= 'desconocido') THEN
                total_reconocidos = total_reconocidos + 1
                lista_limpia%caracteres(total_reconocidos) = lista_original%caracteres(i)
            END IF
        END DO
    END SUBROUTINE filtrar_tokens_reconocidos

    SUBROUTINE imprimir_lista_limpia(lista_limpia)
        TYPE(ListaCaracteres), INTENT(IN) :: lista_limpia
        INTEGER :: i

        PRINT *, '--- Lista de Tokens Reconocidos ---'
        IF (SIZE(lista_limpia%caracteres) == 0) THEN
            PRINT *, 'No hay tokens reconocidos en la lista limpia.'
        ELSE
            DO i = 1, SIZE(lista_limpia%caracteres)
                PRINT *, 'Correlativo: ', lista_limpia%caracteres(i)%correlativo, &
                         'Carácter: ', lista_limpia%caracteres(i)%valor, &
                         ' Token: ', lista_limpia%caracteres(i)%token, &
                         ' Tipo: ', lista_limpia%caracteres(i)%tipo, &
                         ' Fila: ', lista_limpia%caracteres(i)%posicionFila, &
                         ' Columna: ', lista_limpia%caracteres(i)%posicionColumna
            END DO
        END IF
    END SUBROUTINE imprimir_lista_limpia

    SUBROUTINE imprimir_errores_lexico_two(errores)
        TYPE(ListaErrores), INTENT(IN) :: errores
        INTEGER :: i

        PRINT *, '--- Soy LexicoTwo ---'
        PRINT *, '--- Lista de Errores LexicoOne ---'
        IF (SIZE(errores%caracteres) == 0) THEN
            PRINT *, 'No hay errores léxicos registrados.'
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
    END SUBROUTINE imprimir_errores_lexico_two

END MODULE LexicoTwo