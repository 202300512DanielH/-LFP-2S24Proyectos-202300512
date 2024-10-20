MODULE error
    implicit none

    type :: Err
        CHARACTER(LEN = 100) :: token_no_reconocido
        CHARACTER(LEN = 100) :: mensaje_error
        integer :: fila
        integer :: columna
    End type Err

    ! Declaración de un arreglo dinámico para almacenar los errores
    type(Err), ALLOCATABLE :: error_array(:)

contains

    ! Subrutina para agregar errores al arreglo
    subroutine agregar_error(token_no_reconocido, mensaje_error, fila, columna)
        CHARACTER(LEN=*), INTENT(IN) :: token_no_reconocido
        CHARACTER(LEN=*), INTENT(IN) :: mensaje_error
        integer, INTENT(IN) :: fila, columna
        type(Err) :: nuevo_error
        integer :: n
        type(Err), ALLOCATABLE :: temp_array(:)

        ! Inicializar el error con los valores dados
        nuevo_error%token_no_reconocido = token_no_reconocido
        nuevo_error%mensaje_error = mensaje_error
        nuevo_error%fila = fila
        nuevo_error%columna = columna

        ! Agregar el nuevo error al arreglo de errores
        if (.NOT. ALLOCATED(error_array)) then
            ALLOCATE(error_array(1))
            error_array(1) = nuevo_error
        else
            n = size(error_array)
            ALLOCATE(temp_array(n + 1))
            temp_array(:n) = error_array
            temp_array(n + 1) = nuevo_error
            DEALLOCATE(error_array)
            ALLOCATE(error_array(n + 1))
            error_array = temp_array
        end if
    end subroutine agregar_error

    ! Subrutina para imprimir los errores encontrados
    subroutine imprimir_errores()
        integer :: i
        character(len=20) :: str_fila, str_columna

        if (.NOT. ALLOCATED(error_array)) then
            print *, "No hay errores léxicos."
        else
            print *, "Errores léxicos encontrados: ", size(error_array)
            DO i = 1, size(error_array)
                write(str_fila, '(I0)') error_array(i)%fila
                write(str_columna, '(I0)') error_array(i)%columna

                print *, "Token no reconocido: ", trim(error_array(i)%token_no_reconocido)
                print *, "Mensaje de error: ", trim(error_array(i)%mensaje_error)
                print *, "Fila: ", trim(str_fila)
                print *, "Columna: ", trim(str_columna)
                print *, "---------------------------------"
            END DO
        end if
    end subroutine imprimir_errores

END MODULE error
