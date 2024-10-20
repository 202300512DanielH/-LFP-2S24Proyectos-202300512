MODULE token
    use error
    use error_sintactico

    use etiqueta
    use boton
    use contenedor
    use clave
    use texto
    use chequeo
    use radio_boton
    use area_Texto
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

    subroutine filtrar_comentarios()
        type(Tkn), ALLOCATABLE :: temp_array(:)
        integer :: i, j, n

        if (.NOT. ALLOCATED(token_array)) return

        n = size(token_array)
        ALLOCATE(temp_array(n))
        j = 0

        ! Copiar solo los tokens que no sean comentarios
        DO i = 1, n
            if (token_array(i)%tipo /= 'TKN_comentario') then
                j = j + 1
                temp_array(j) = token_array(i)
            end if
        END DO

        ! Redimensionar el arreglo temporal al tamaño adecuado
        DEALLOCATE(token_array)
        ALLOCATE(token_array(j))
        token_array = temp_array(:j)
    end subroutine filtrar_comentarios


    subroutine parser()
        integer :: i
    
        ! Filtrar los comentarios antes del parseo
        call filtrar_comentarios()
    
        if (.NOT. ALLOCATED(token_array)) then
            print *, "No hay tokens"
        else
            DO i = 1, size(token_array)
                ! Verificación de tokens para controles 
                if (token_array(i)%tipo == 'TKN_etiqueta') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_etiqueta(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if
                if (token_array(i)%tipo == 'TKN_boton') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_boton(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if

                if (token_array(i)%tipo == 'TKN_contenedor') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_contenedor(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if

                if (token_array(i)%tipo == 'TKN_clave') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_clave(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if

                if (token_array(i)%tipo == 'TKN_texto') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_texto(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if

                if (token_array(i)%tipo == 'TKN_check') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_chequeo(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if

                if (token_array(i)%tipo == 'TKN_ra_boton') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_radio_boton(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if

                if (token_array(i)%tipo == 'TKN_area_texto') then
                    if (token_array(i+1)%tipo == 'TKN_id' .and. token_array(i+2)%tipo == 'TKN_pyc') then
                        call agregar_radio_boton(token_array(i+1)%lexema)
                    else
                        call agregar_error_sintactico(token_array(i+1)%lexema, 'TKN_id', token_array(i+1)%fila, token_array(i+1)%columna)
                    end if
                end if
        
                if (token_array(i)%tipo == 'TKN_id' .and. token_array(i+1)%tipo == 'TKN_punto') then
                    if (token_array(i+2)%tipo == 'TKN_setAncho') then
                        if (token_array(i+3)%tipo .ne. 'TKN_par_izq') then
                            call agregar_error_sintactico(token_array(i+3)%lexema, 'TKN_par_izq', token_array(i+3)%fila, token_array(i+3)%columna)
                        elseif (token_array(i+4)%tipo .ne. 'TKN_num') then
                            call agregar_error_sintactico(token_array(i+4)%lexema, 'TKN_num', token_array(i+4)%fila, token_array(i+4)%columna)
                        elseif (token_array(i+5)%tipo .ne. 'TKN_par_der') then
                            call agregar_error_sintactico(token_array(i+5)%lexema, 'TKN_par_der', token_array(i+5)%fila, token_array(i+5)%columna)
                        elseif (token_array(i+6)%tipo .ne. 'TKN_pyc') then
                            call agregar_error_sintactico(token_array(i+6)%lexema, 'TKN_pyc', token_array(i+6)%fila, token_array(i+6)%columna)
                        else
                            call etiqueta_set_ancho(token_array(i)%lexema, token_array(i+4)%lexema)
                        end if
                    end if
        
                    if (token_array(i+2)%tipo == 'TKN_setAlto') then
                        if (token_array(i+3)%tipo .ne. 'TKN_par_izq') then
                            call agregar_error_sintactico(token_array(i+3)%lexema, 'TKN_par_izq', token_array(i+3)%fila, token_array(i+3)%columna)
                        elseif (token_array(i+4)%tipo .ne. 'TKN_num') then
                            call agregar_error_sintactico(token_array(i+4)%lexema, 'TKN_num', token_array(i+4)%fila, token_array(i+4)%columna)
                        elseif (token_array(i+5)%tipo .ne. 'TKN_par_der') then
                            call agregar_error_sintactico(token_array(i+5)%lexema, 'TKN_par_der', token_array(i+5)%fila, token_array(i+5)%columna)
                        elseif (token_array(i+6)%tipo .ne. 'TKN_pyc') then
                            call agregar_error_sintactico(token_array(i+6)%lexema, 'TKN_pyc', token_array(i+6)%fila, token_array(i+6)%columna)
                        else
                            call etiqueta_set_alto(token_array(i)%lexema, token_array(i+4)%lexema)
                        end if
                    end if
        
                    if (token_array(i+2)%tipo == 'TKN_setTexto') then
                        if (token_array(i+3)%tipo .ne. 'TKN_par_izq') then
                            call agregar_error_sintactico(token_array(i+3)%lexema, 'TKN_par_izq', token_array(i+3)%fila, token_array(i+3)%columna)
                        elseif (token_array(i+4)%tipo .ne. 'TKN_literal') then
                            call agregar_error_sintactico(token_array(i+4)%lexema, 'TKN_literal', token_array(i+4)%fila, token_array(i+4)%columna)
                        elseif (token_array(i+5)%tipo .ne. 'TKN_par_der') then
                            call agregar_error_sintactico(token_array(i+5)%lexema, 'TKN_par_der', token_array(i+5)%fila, token_array(i+5)%columna)
                        elseif (token_array(i+6)%tipo .ne. 'TKN_pyc') then
                            call agregar_error_sintactico(token_array(i+6)%lexema, 'TKN_pyc', token_array(i+6)%fila, token_array(i+6)%columna)
                        else
                            call etiqueta_set_texto(token_array(i)%lexema, token_array(i+4)%lexema)
                        end if
                    end if
        
                    if (token_array(i+2)%tipo == 'TKN_setColorLetra') then
                        if (token_array(i+3)%tipo .ne. 'TKN_par_izq') then
                            call agregar_error_sintactico(token_array(i+3)%lexema, 'TKN_par_izq', token_array(i+3)%fila, token_array(i+3)%columna)
                        elseif (token_array(i+4)%tipo .ne. 'TKN_num') then
                            call agregar_error_sintactico(token_array(i+4)%lexema, 'TKN_num', token_array(i+4)%fila, token_array(i+4)%columna)
                        elseif (token_array(i+5)%tipo .ne. 'TKN_coma') then
                            call agregar_error_sintactico(token_array(i+5)%lexema, 'TKN_coma', token_array(i+5)%fila, token_array(i+5)%columna)
                        elseif (token_array(i+6)%tipo .ne. 'TKN_num') then
                            call agregar_error_sintactico(token_array(i+6)%lexema, 'TKN_num', token_array(i+6)%fila, token_array(i+6)%columna)
                        elseif (token_array(i+7)%tipo .ne. 'TKN_coma') then
                            call agregar_error_sintactico(token_array(i+7)%lexema, 'TKN_coma', token_array(i+7)%fila, token_array(i+7)%columna)
                        elseif (token_array(i+8)%tipo .ne. 'TKN_num') then
                            call agregar_error_sintactico(token_array(i+8)%lexema, 'TKN_num', token_array(i+8)%fila, token_array(i+8)%columna)
                        elseif (token_array(i+9)%tipo .ne. 'TKN_par_der') then
                            call agregar_error_sintactico(token_array(i+9)%lexema, 'TKN_par_der', token_array(i+9)%fila, token_array(i+9)%columna)
                        elseif (token_array(i+10)%tipo .ne. 'TKN_pyc') then
                            call agregar_error_sintactico(token_array(i+10)%lexema, 'TKN_pyc', token_array(i+10)%fila, token_array(i+10)%columna)
                        else
                            call etiqueta_set_color_texto(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema, token_array(i+8)%lexema)
                        end if
                    end if
        
                    if (token_array(i+2)%tipo == 'TKN_setPosicion') then
                        if (token_array(i+3)%tipo .ne. 'TKN_par_izq') then
                            call agregar_error_sintactico(token_array(i+3)%lexema, 'TKN_par_izq', token_array(i+3)%fila, token_array(i+3)%columna)
                        elseif (token_array(i+4)%tipo .ne. 'TKN_num') then
                            call agregar_error_sintactico(token_array(i+4)%lexema, 'TKN_num', token_array(i+4)%fila, token_array(i+4)%columna)
                        elseif (token_array(i+5)%tipo .ne. 'TKN_coma') then
                            call agregar_error_sintactico(token_array(i+5)%lexema, 'TKN_coma', token_array(i+5)%fila, token_array(i+5)%columna)
                        elseif (token_array(i+6)%tipo .ne. 'TKN_num') then
                            call agregar_error_sintactico(token_array(i+6)%lexema, 'TKN_num', token_array(i+6)%fila, token_array(i+6)%columna)
                        elseif (token_array(i+7)%tipo .ne. 'TKN_par_der') then
                            call agregar_error_sintactico(token_array(i+7)%lexema, 'TKN_par_der', token_array(i+7)%fila, token_array(i+7)%columna)
                        elseif (token_array(i+8)%tipo .ne. 'TKN_pyc') then
                            call agregar_error_sintactico(token_array(i+8)%lexema, 'TKN_pyc', token_array(i+8)%fila, token_array(i+8)%columna)
                        else
                            call etiqueta_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
                        end if
                    end if
                end if
            END DO
        end if
        
    end subroutine parser


END MODULE token
