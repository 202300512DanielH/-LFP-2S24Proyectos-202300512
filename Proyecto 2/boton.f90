MODULE boton
    implicit none
        type :: button
            CHARACTER(LEN = 50) :: id
            CHARACTER(LEN = 20) :: tipo
            CHARACTER(LEN = 20) :: alto
            CHARACTER(LEN = 20) :: ancho
            CHARACTER(LEN = 200) :: texto
            CHARACTER(LEN = 200) :: alineacion_texto
            CHARACTER(LEN = 50) :: color_texto_r
            CHARACTER(LEN = 50) :: color_texto_g
            CHARACTER(LEN = 50) :: color_texto_b
            CHARACTER(LEN = 50) :: color_fondo_r
            CHARACTER(LEN = 50) :: color_fondo_g
            CHARACTER(LEN = 50) :: color_fondo_b
            CHARACTER(LEN = 50) :: posicion_x
            CHARACTER(LEN = 50) :: posicion_y
            CHARACTER(LEN = 200) :: add
        End type button
    
        ! Declaración de un arreglo de Tag para almacenar los etiquetas
        type(button), ALLOCATABLE ::  button_array(:)
    
    contains
    subroutine agregar_boton(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(button) :: nuevo_boton
        integer :: n
        type(button), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_boton%id = id
        nuevo_boton%tipo = 'Boton'
        nuevo_boton%alto = "25"
        nuevo_boton%ancho = "100"
        nuevo_boton%texto = ""
        nuevo_boton%alineacion_texto = "left"
        nuevo_boton%color_texto_r = ""
        nuevo_boton%color_texto_g = ""
        nuevo_boton%color_texto_b = ""
        nuevo_boton%color_fondo_r = ""
        nuevo_boton%color_fondo_g = ""
        nuevo_boton%color_fondo_b = ""
        nuevo_boton%posicion_x = ""
        nuevo_boton%posicion_y = ""
        nuevo_boton%add = ""



        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(button_array)) then !Si esta vacia
            ALLOCATE(button_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            button_array(1) =  nuevo_boton !Se convierte en el etiqueta nuevo
        else
            n = size(button_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = button_array !Reservo memoria
            temp_array(n+1) = nuevo_boton
            DEALLOCATE(button_array) !Libero memoria
            ALLOCATE(button_array(n+1)) !Reservo memoria de nuevo
            button_array = temp_array
        end if
    end subroutine agregar_boton

    subroutine imprimir_botones()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            print *, "botones encontrados: ", size(button_array)
            DO i = 1, size(button_array)
                print *, 'id: ', trim(button_array(i)%id)
                print *, 'alto: ', trim(button_array(i)%alto)
                print *, 'ancho: ', trim(button_array(i)%ancho)
                print *, 'texto: ', trim(button_array(i)%texto)
                print *, 'color_texto_r: ', trim(button_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(button_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(button_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(button_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(button_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(button_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(button_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(button_array(i)%posicion_y)
                print *, 'add: ', trim(button_array(i)%add)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_botones

    subroutine boton_set_alto(id, alto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%alto = alto
                end if
            END DO
        end if
    end subroutine boton_set_alto

    subroutine boton_set_ancho(id, ancho)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: ancho
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%ancho = ancho
                end if
            END DO
        end if
    end subroutine boton_set_ancho

    subroutine boton_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%texto = texto
                end if
            END DO
        end if
    end subroutine boton_set_texto

    subroutine boton_set_alineacion_texto(id, alineacion_texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion_texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%alineacion_texto = alineacion_texto
                end if
            END DO
        end if
    end subroutine boton_set_alineacion_texto

    subroutine boton_set_color_texto(id, color_texto_r, color_texto_g, color_texto_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_r
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_g
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%color_texto_r = color_texto_r
                    button_array(i)%color_texto_g = color_texto_g
                    button_array(i)%color_texto_b = color_texto_b
                end if
            END DO
        end if
    end subroutine boton_set_color_texto

    subroutine boton_set_color_fondo(id, color_fondo_r, color_fondo_g, color_fondo_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_r
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_g
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%color_fondo_r = color_fondo_r
                    button_array(i)%color_fondo_g = color_fondo_g
                    button_array(i)%color_fondo_b = color_fondo_b
                end if
            END DO
        end if
    end subroutine boton_set_color_fondo

    subroutine boton_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%posicion_x = posicion_x
                    button_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if
    end subroutine boton_set_posicion

    subroutine boton_set_add(id, add)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: add
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(button_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(button_array)
                if (trim(button_array(i)%id) == id) then
                    button_array(i)%add = add
                end if
            END DO
        end if
    end subroutine boton_set_add

    
END MODULE boton