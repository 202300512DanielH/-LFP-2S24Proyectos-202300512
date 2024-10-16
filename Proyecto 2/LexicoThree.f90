! Archivo: LexicoThree.f90
MODULE LexicoThree
    USE CaracterModule
    USE ExtractorModule
    IMPLICIT NONE
    PUBLIC :: agregar_lexico, imprimir_lista_con_lexico

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
            lista_lexico%caracteres(i)%token = lista_limpia%caracteres(i)%token
        END DO
    END SUBROUTINE agregar_lexico

    ! Subrutina para imprimir los caracteres de la lista con 'Léxico' agregado
    SUBROUTINE imprimir_lista_con_lexico(lista_lexico)
        TYPE(ListaLexico), INTENT(IN) :: lista_lexico
        INTEGER :: i

        PRINT *, '--- Lista con "Léxico" agregado ---'
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
    END SUBROUTINE imprimir_lista_con_lexico

END MODULE LexicoThree