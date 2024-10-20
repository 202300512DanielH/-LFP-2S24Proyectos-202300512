program analizador_lexico
    use error
    use token

    implicit none
    integer :: len, fila, columna, estado, puntero, i, num_lineas
    character(len=100000) :: contenido, linea
    character(len=1) :: char, next_char
    character(len=100) :: aux_tkn

    estado = 0
    puntero = 1
    columna = 0
    fila = 1
    aux_tkn = ""
    contenido = ""

    ! Leer el número de líneas desde stdin
    read(*, *) num_lineas
    if (num_lineas < 1) then
        print *, "Error: El número de líneas debe ser positivo."
        stop
    end if

    ! Leer todas las líneas desde stdin
    do i = 1, num_lineas
        read(*, '(A)') linea
        contenido = trim(contenido) // trim(linea) // new_line('a')
    end do

    len = len_trim(contenido)

    ! Análisis léxico
    do while (puntero <= len)
        char = contenido(puntero:puntero)

        ! Verificar si hay más caracteres para predecir "//" o "/*"
        if (puntero < len) then
            next_char = contenido(puntero + 1:puntero + 1)
        else
            next_char = " "
        end if

        select case (estado)
            case (0)
                if (char == '/' .and. next_char == '/') then
                    estado = 5  ! Estado de comentario de línea
                    puntero = puntero + 2
                    columna = columna + 2
                    aux_tkn = "//"
                elseif (char == '/' .and. next_char == '*') then
                    estado = 6  ! Estado de comentario multi-línea
                    puntero = puntero + 2
                    columna = columna + 2
                    aux_tkn = "/*"
                elseif (char == ';' .or. char == '-' .or. char == '.' .or. char == '(' .or. &
                    char == ')' .or. char == ',' .or. char == '<' .or. char == '>' .or. char == '!' .or. &
                    char == '/' .or. char == '*') then
                    estado = 1
                    columna = columna + 1
                elseif (char >= 'A' .and. char <= 'Z' .or. (char >= 'a' .and. char <= 'z')) then
                    estado = 2
                elseif (char >= '0' .and. char <= '9') then
                    estado = 3
                elseif (char == '"') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                    estado = 4
                elseif (ichar(char) == 10) then
                    columna = 0
                    fila = fila + 1
                    puntero = puntero + 1
                elseif (ichar(char) == 9) then
                    columna = columna + 4
                    puntero = puntero + 1
                elseif (ichar(char) == 32) then
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    call agregar_error(char, "Token no reconocido", fila, columna)
                    columna = columna + 1
                    puntero = puntero + 1
                end if

            case (1)
                if (char == ';') then
                    call agregar_token(char, 'TKN_pyc', fila, columna)
                elseif (char == '.') then
                    call agregar_token(char, 'TKN_punto', fila, columna)
                elseif (char == ',') then
                    call agregar_token(char, 'TKN_coma', fila, columna)
                elseif (char == '>') then
                    call agregar_token(char, 'TKN_mayor', fila, columna)
                elseif (char == '<') then
                    call agregar_token(char, 'TKN_menor', fila, columna)
                elseif (char == '(') then
                    call agregar_token(char, 'TKN_par_izq', fila, columna)
                elseif (char == ')') then
                    call agregar_token(char, 'TKN_par_der', fila, columna)
                elseif (char == '-') then
                    call agregar_token(char, 'TKN_guion', fila, columna)
                elseif (char == '!') then
                    call agregar_token(char, 'TKN_exp', fila, columna)
                elseif (char == '/') then
                    call agregar_token(char, 'TKN_slash', fila, columna)
                elseif (char == '*') then
                    call agregar_token(char, 'TKN_ast', fila, columna)
                end if
                puntero = puntero + 1
                estado = 0

            case (2)
                if ((char >= 'A' .and. char <= 'Z') .or. (char >= 'a' .and. char <= 'z') .or. &
                    (char >= '0' .and. char <= '9')) then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    !Palabras reservadas de contenedor
                    if ((aux_tkn == 'Etiqueta')) then
                        call agregar_token(aux_tkn, 'tk_etiqueta', fila, columna)
                    
                    elseif ((aux_tkn == 'Boton')) then
                        call agregar_token(aux_tkn, 'tk_boton', fila, columna)
                    
                    elseif ((aux_tkn == 'Check')) then
                        call agregar_token(aux_tkn, 'tk_boton', fila, columna)

                    elseif ((aux_tkn == 'RadioBoton')) then
                        call agregar_token(aux_tkn, 'tk_ra_boton', fila, columna)
                    
                    elseif ((aux_tkn == 'Texto')) then
                        call agregar_token(aux_tkn, 'tk_texto', fila, columna)
                    
                    elseif ((aux_tkn == 'AreaTexto')) then
                        call agregar_token(aux_tkn, 'tk_area_texto', fila, columna)

                    elseif ((aux_tkn == 'Clave')) then
                        call agregar_token(aux_tkn, 'tk_clave', fila, columna)    

                    elseif ((aux_tkn == 'Contenedor')) then
                        call agregar_token(aux_tkn, 'tk_contenedor', fila, columna)
                    
                        
                    !Palabras reservadas de propiedades
                    
                    elseif ((aux_tkn == 'setAncho')) then
                        call agregar_token(aux_tkn, 'tk_setAncho', fila, columna)
                    
                    elseif ((aux_tkn == 'setAlto')) then
                        call agregar_token(aux_tkn, 'tk_setAlto', fila, columna)
                    
                    elseif ((aux_tkn == 'setAlineacion')) then
                        call agregar_token(aux_tkn, 'tk_setAlineacion', fila, columna)
                    
                    elseif ((aux_tkn == 'setColorFondo')) then
                        call agregar_token(aux_tkn, 'tk_setColorFondo', fila, columna)

                    elseif ((aux_tkn == 'setColorLetra')) then
                        call agregar_token(aux_tkn, 'tk_setColorLetra', fila, columna)
                    
                    elseif ((aux_tkn == 'setTexto')) then
                        call agregar_token(aux_tkn, 'tk_setTexto', fila, columna)
                    
                    elseif ((aux_tkn == 'setMarcada')) then
                        call agregar_token(aux_tkn, 'tk_setMarc', fila, columna)
            
                    elseif ((aux_tkn == 'setGrupo')) then
                        call agregar_token(aux_tkn, 'tk_setGrupo', fila, columna)
                    
                    !Palabras reservadas de posicion

                    elseif ((aux_tkn == 'setPosicion')) then
                        call agregar_token(aux_tkn, 'tk_setPosicion', fila, columna)
                    
                    elseif (aux_tkn == 'this') then
                        call agregar_token(aux_tkn, 'tk_this', fila, columna)
                    
                    elseif (aux_tkn == 'add') then
                        call agregar_token(aux_tkn, 'tk_add', fila, columna)
                        
                    !Palabras reservadas de bloque
                    elseif (aux_tkn == 'Controles') then
                        call agregar_token(aux_tkn, 'tk_nm_bloque_ctrls', fila, columna)
                    
                    elseif (aux_tkn == 'Propiedades') then
                        call agregar_token(aux_tkn, 'tk_nm_bloque_props', fila, columna)
                    
                    elseif (aux_tkn == 'Colocacion') then
                        call agregar_token(aux_tkn, 'tk_nm_bloque_coloc', fila, columna)

                    else 
                        call agregar_token(aux_tkn, 'tk_id', fila, columna)

                    end if

                    aux_tkn = ""
                    estado = 0      
                        
                end if

            case (3)
                if (char >= '0' .and. char <= '9') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    call agregar_token(aux_tkn, 'TKN_num', fila, columna)
                    aux_tkn = ""
                    estado = 0
                end if

            case (4)
                ! Acumulamos todo hasta encontrar el cierre del literal con comillas dobles
                if (char /= '"') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    ! Añadimos también la comilla de cierre al token
                    aux_tkn = trim(aux_tkn) // char
                    call agregar_token(aux_tkn, 'TKN_literal', fila, columna)
                    aux_tkn = ""
                    puntero = puntero + 1
                    estado = 0
                end if
            

            case (5)  ! Estado de comentario de línea
                if (ichar(char) == 10) then  ! Salto de línea
                    call agregar_token(aux_tkn, 'TKN_comentario', fila, columna)
                    aux_tkn = ""
                    columna = 0
                    fila = fila + 1
                    puntero = puntero + 1
                    estado = 0
                else
                    aux_tkn = trim(aux_tkn) // char
                    puntero = puntero + 1
                    columna = columna + 1
                end if

            case (6)  ! Estado de comentario multi-línea
                if (char == '*' .and. next_char == '/') then
                    aux_tkn = trim(aux_tkn) // "*/"
                    call agregar_token(aux_tkn, 'TKN_comentario', fila, columna)
                    aux_tkn = ""
                    puntero = puntero + 2
                    columna = columna + 2
                    estado = 0
                else
                    aux_tkn = trim(aux_tkn) // char
                    puntero = puntero + 1
                    columna = columna + 1
                end if
        end select
    end do

    call imprimir_tokens
    call imprimir_errores
    call generar_json_tokens
end program analizador_lexico
