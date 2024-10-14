! Archivo: LexicoTwo.f90
MODULE LexicoTwo
    USE CaracterModule
    USE ExtractorModule
    IMPLICIT NONE
    PUBLIC :: limpiar_lista, imprimir_lista_limpia

CONTAINS

    ! Subrutina para eliminar caracteres con token "Sin definir" de la lista
    SUBROUTINE limpiar_lista(lista)
        TYPE(ListaCaracteres), INTENT(INOUT) :: lista
        TYPE(Caracter), ALLOCATABLE :: lista_filtrada(:)
        INTEGER :: i, contador_validos

        ! Inicializamos el contador para los caracteres válidos
        contador_validos = 0

        ! Primera pasada: contamos cuántos elementos no tienen el token "Sin definir"
        DO i = 1, SIZE(lista%caracteres)
            IF (TRIM(lista%caracteres(i)%token) /= 'Sin definir') THEN
                contador_validos = contador_validos + 1
            END IF
        END DO

        ! Si no hay caracteres válidos, vaciamos la lista
        IF (contador_validos == 0) THEN
            DEALLOCATE(lista%caracteres)
            RETURN
        END IF

        ! Segunda pasada: guardamos solo los caracteres válidos
        ALLOCATE(lista_filtrada(contador_validos))
        contador_validos = 0
        DO i = 1, SIZE(lista%caracteres)
            IF (TRIM(lista%caracteres(i)%token) /= 'Sin definir') THEN
                contador_validos = contador_validos + 1
                lista_filtrada(contador_validos) = lista%caracteres(i)
            END IF
        END DO

        ! Actualizamos la lista con los caracteres válidos
        DEALLOCATE(lista%caracteres)
        lista%caracteres = lista_filtrada
    END SUBROUTINE limpiar_lista

    ! Subrutina para imprimir la lista de caracteres filtrada
    SUBROUTINE imprimir_lista_limpia(lista)
        TYPE(ListaCaracteres), INTENT(IN) :: lista
        INTEGER :: i

        PRINT *, '--- Lista de Carácteres Filtrados (Sin "Sin definir") ---'
        IF (SIZE(lista%caracteres) == 0) THEN
            PRINT *, 'La lista está vacía.'
            RETURN
        END IF

        DO i = 1, SIZE(lista%caracteres)
            PRINT *, 'Carácter: ', lista%caracteres(i)%valor, &
                     ' Token: ', lista%caracteres(i)%token, &
                     ' Fila: ', lista%caracteres(i)%posicionFila, &
                     ' Columna: ', lista%caracteres(i)%posicionColumna
        END DO
    END SUBROUTINE imprimir_lista_limpia

END MODULE LexicoTwo
