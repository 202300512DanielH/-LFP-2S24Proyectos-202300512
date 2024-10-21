MODULE errores
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    type :: Fail
        CHARACTER(LEN = 100) :: token_no_reconocido
        CHARACTER(LEN = 100) :: mensaje_error
        integer :: fila
        integer :: columna
    end type Fail

    type :: Err
        CHARACTER(LEN = 100) :: ultimo_token
        CHARACTER(LEN = 100) :: token_esperado
        integer :: fila
        integer :: columna
    end type Err

    type(Fail), ALLOCATABLE :: error_lexico_array(:)
    type(Err), ALLOCATABLE :: error_sintactico_array(:)

contains

    subroutine agregar_error_lexico(token_no_reconocido, mensaje_error, fila, columna)
        CHARACTER(LEN=*), INTENT(IN) :: token_no_reconocido, mensaje_error
        integer, INTENT(IN) :: fila, columna
        type(Fail) :: nuevo_error
        integer :: n
        type(Fail), ALLOCATABLE :: temp_array(:)

        nuevo_error%token_no_reconocido = token_no_reconocido
        nuevo_error%mensaje_error = mensaje_error
        nuevo_error%fila = fila
        nuevo_error%columna = columna

        if (.NOT. ALLOCATED(error_lexico_array)) then
            ALLOCATE(error_lexico_array(1))
            error_lexico_array(1) = nuevo_error
        else
            n = size(error_lexico_array)
            ALLOCATE(temp_array(n + 1))
            temp_array(:n) = error_lexico_array
            temp_array(n + 1) = nuevo_error
            DEALLOCATE(error_lexico_array)
            ALLOCATE(error_lexico_array(n + 1))
            error_lexico_array = temp_array
        end if
    end subroutine agregar_error_lexico

    subroutine agregar_error_sintactico(ultimo_token, token_esperado, fila, columna)
        CHARACTER(LEN=*), INTENT(IN) :: ultimo_token, token_esperado
        integer, INTENT(IN) :: fila, columna
        type(Err) :: nuevo_error
        integer :: n
        type(Err), ALLOCATABLE :: temp_array(:)

        nuevo_error%ultimo_token = ultimo_token
        nuevo_error%token_esperado = token_esperado
        nuevo_error%fila = fila
        nuevo_error%columna = columna

        if (.NOT. ALLOCATED(error_sintactico_array)) then
            ALLOCATE(error_sintactico_array(1))
            error_sintactico_array(1) = nuevo_error
        else
            n = size(error_sintactico_array)
            ALLOCATE(temp_array(n + 1))
            temp_array(:n) = error_sintactico_array
            temp_array(n + 1) = nuevo_error
            DEALLOCATE(error_sintactico_array)
            ALLOCATE(error_sintactico_array(n + 1))
            error_sintactico_array = temp_array
        end if
    end subroutine agregar_error_sintactico

    subroutine guardar_errores_json()
        integer :: i, unit_number, ios
        character(len=20) :: str_fila, str_columna

        open(newunit=unit_number, file="errores.json", status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            print *, "Error al abrir el archivo errores.json"
            return
        end if

        write(unit_number, '(A)') '['  ! Inicio del array JSON

        ! Escribir errores léxicos
        if (ALLOCATED(error_lexico_array)) then
            DO i = 1, size(error_lexico_array)
                write(str_fila, '(I0)') error_lexico_array(i)%fila
                write(str_columna, '(I0)') error_lexico_array(i)%columna
                write(unit_number, '(A)') &
                    '  { "tipo": "léxico", "linea": "' // trim(str_fila) // '", ' // &
                    '"columna": "' // trim(str_columna) // '", ' // &
                    '"token_no_reconocido": "' // trim(error_lexico_array(i)%token_no_reconocido) // '", ' // &
                    '"mensaje_error": "' // trim(error_lexico_array(i)%mensaje_error) // '" }'
                if (i /= size(error_lexico_array) .OR. ALLOCATED(error_sintactico_array)) &
                    write(unit_number, '(A)') ','
            END DO
        end if

        ! Escribir errores sintácticos
        if (ALLOCATED(error_sintactico_array)) then
            DO i = 1, size(error_sintactico_array)
                write(str_fila, '(I0)') error_sintactico_array(i)%fila
                write(str_columna, '(I0)') error_sintactico_array(i)%columna
                write(unit_number, '(A)') &
                    '  { "tipo": "sintáctico", "linea": "' // trim(str_fila) // '", ' // &
                    '"columna": "' // trim(str_columna) // '", ' // &
                    '"ultimo_token": "' // trim(error_sintactico_array(i)%ultimo_token) // '", ' // &
                    '"token_esperado": "' // trim(error_sintactico_array(i)%token_esperado) // '" }'
                if (i /= size(error_sintactico_array)) write(unit_number, '(A)') ','
            END DO
        end if

        write(unit_number, '(A)') ']'  ! Fin del array JSON
        close(unit_number)
    end subroutine guardar_errores_json
END MODULE errores
