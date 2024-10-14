! Archivo: Extractor.f90
MODULE ExtractorModule
    USE CaracterModule  ! Usamos el módulo de Caracter
    IMPLICIT NONE
    PUBLIC :: extraer_caracteres, imprimir_caracteres, ListaCaracteres  ! Hacemos público el tipo 'ListaCaracteres'

    TYPE :: ListaCaracteres
        TYPE(Caracter), ALLOCATABLE :: caracteres(:)  ! Lista dinámica de objetos 'Caracter'
    END TYPE ListaCaracteres

CONTAINS

    SUBROUTINE extraer_caracteres(texto, lista)
        CHARACTER(LEN=*), INTENT(IN) :: texto(:)
        TYPE(ListaCaracteres), INTENT(OUT) :: lista
        INTEGER :: i, j, fila, columna, longitud
        INTEGER :: total_caracteres

        total_caracteres = 0
        fila = 0

        DO i = 1, SIZE(texto)
            fila = fila + 1
            longitud = LEN_TRIM(texto(i))
            total_caracteres = total_caracteres + longitud
        END DO

        ALLOCATE(lista%caracteres(total_caracteres))

        total_caracteres = 0
        fila = 0
        DO i = 1, SIZE(texto)
            fila = fila + 1
            longitud = LEN_TRIM(texto(i))
            DO j = 1, longitud
                total_caracteres = total_caracteres + 1
                lista%caracteres(total_caracteres)%valor = texto(i)(j:j)
                lista%caracteres(total_caracteres)%posicionFila = fila
                lista%caracteres(total_caracteres)%posicionColumna = j
                lista%caracteres(total_caracteres)%tipo = 'Desconocido'
                lista%caracteres(total_caracteres)%token = 'Sin definir'
            END DO
        END DO
    END SUBROUTINE extraer_caracteres

    SUBROUTINE imprimir_caracteres(lista)
        TYPE(ListaCaracteres), INTENT(IN) :: lista
        INTEGER :: i

        DO i = 1, SIZE(lista%caracteres)
            PRINT *, 'Carácter: ', lista%caracteres(i)%valor, &
                     ' Fila: ', lista%caracteres(i)%posicionFila, &
                     ' Columna: ', lista%caracteres(i)%posicionColumna
        END DO
    END SUBROUTINE imprimir_caracteres

END MODULE ExtractorModule
