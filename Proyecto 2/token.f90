MODULE token
    use error
    implicit none

    type :: Tkn
        CHARACTER(LEN = 100) :: lexema
        CHARACTER(LEN = 200) :: tipo 
        integer :: fila
        integer :: columna
    End type Tkn

    type(Tkn), ALLOCATABLE :: token_array(:)

contains

    subroutine agregar_token(lexema, tipo, fila, columna)
        CHARACTER(LEN=*), INTENT(IN) :: lexema
        CHARACTER(LEN=*), INTENT(IN) :: tipo
        integer :: fila
        integer :: columna
        type(Tkn) :: nuevo_token
        integer :: n
        type(Tkn), ALLOCATABLE :: temp_array(:)

        nuevo_token%lexema = lexema
        nuevo_token%tipo = tipo
        nuevo_token%fila = fila
        nuevo_token%columna = columna

        if (.NOT. ALLOCATED(token_array)) then
            ALLOCATE(token_array(1))
            token_array(1) = nuevo_token
        else
            n = size(token_array)
            ALLOCATE(temp_array(n + 1))
            temp_array(:n) = token_array
            temp_array(n + 1) = nuevo_token
            DEALLOCATE(token_array)
            ALLOCATE(token_array(n + 1))
            token_array = temp_array
        end if
    end subroutine agregar_token

    subroutine imprimir_tokens()
        integer :: i
        character(len=20) :: str_fila, str_columna

        if (.NOT. ALLOCATED(token_array)) then
            print *, "No hay tokens"
        else
            print *, "tokens encontrados: ", size(token_array)
            DO i = 1, size(token_array)
                write(str_fila, '(I0)') token_array(i)%fila
                write(str_columna, '(I0)') token_array(i)%columna
            
                print *, 'lexema: ', trim(token_array(i)%lexema)
                print *, 'tipo: ', trim(token_array(i)%tipo)
                print *, 'fila: ', trim(str_fila)
                print *, 'columna: ', trim(str_columna)
                print *, '---------------------------------'
            END DO
        end if
    end subroutine imprimir_tokens

    subroutine generar_json_tokens()
        integer :: i, unit_number, ios
        character(len=500) :: token_json, lexema_limpio
        character(len=20) :: str_fila, str_columna, correlativo
        character(len=100) :: filename
    
        ! Nombre del archivo de salida
        filename = 'tokens.json'
    
        ! Abrir el archivo para escritura
        open(newunit=unit_number, file=trim(filename), status='replace', action='write', iostat=ios)
    
        if (ios /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if
    
        ! Escribir el encabezado del JSON
        write(unit_number, '(A)') '{'
        write(unit_number, '(A)') '  "tokens": ['
    
        ! Generar cada token como una línea en JSON
        DO i = 1, size(token_array)
            write(correlativo, '(I0)') i
            write(str_fila, '(I0)') token_array(i)%fila
            write(str_columna, '(I0)') token_array(i)%columna
    
            ! Eliminar saltos de línea y comillas innecesarias del lexema
            lexema_limpio = trim(adjustl(token_array(i)%lexema))
            call reemplazar_saltos_linea(lexema_limpio)
            call eliminar_comillas(lexema_limpio)
    
            ! Generar el JSON para el token actual
            token_json = '    {' // &
                '"correlativo": ' // trim(correlativo) // ', ' // &
                '"lexema": "' // trim(lexema_limpio) // '", ' // &
                '"tipo": "' // trim(token_array(i)%tipo) // '", ' // &
                '"fila": ' // trim(str_fila) // ', ' // &
                '"columna": ' // trim(str_columna) // '}'
    
            ! Añadir coma si no es el último token
            if (i < size(token_array)) then
                token_json = trim(token_json) // ','
            end if
    
            ! Escribir el token al archivo
            write(unit_number, '(A)') trim(token_json)
        END DO
    
        ! Cerrar el JSON
        write(unit_number, '(A)') '  ]'
        write(unit_number, '(A)') '}'
    
        ! Cerrar el archivo
        close(unit_number)
        print *, "Archivo tokens.json generado exitosamente."
    end subroutine generar_json_tokens
    
    ! Subrutina para reemplazar saltos de línea en el lexema
    subroutine reemplazar_saltos_linea(lexema)
        character(len=*), intent(inout) :: lexema
        integer :: i
    
        ! Reemplaza cada salto de línea por un espacio en blanco
        do i = 1, len(lexema)
            if (lexema(i:i) == char(10) .or. lexema(i:i) == char(13)) then
                lexema(i:i) = ' '
            end if
        end do
    
        ! Asegura que el lexema no contenga espacios dobles
        lexema = trim(adjustl(lexema))
    end subroutine reemplazar_saltos_linea
    
    ! Subrutina para eliminar comillas dobles al inicio y final del lexema
    subroutine eliminar_comillas(lexema)
        character(len=*), intent(inout) :: lexema
        integer :: len_lexema
    
        len_lexema = len_trim(lexema)
    
        ! Verifica si el lexema empieza y termina con comillas dobles
        if (len_lexema >= 2 .and. lexema(1:1) == '"' .and. lexema(len_lexema:len_lexema) == '"') then
            ! Elimina las comillas dobles al inicio y al final
            lexema = lexema(2:len_lexema-1)
        end if
    end subroutine eliminar_comillas
    
    
END MODULE token
