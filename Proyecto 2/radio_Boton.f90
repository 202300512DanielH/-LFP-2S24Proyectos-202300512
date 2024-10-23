MODULE radio_boton
    implicit none
        type :: radioButton
            CHARACTER(LEN = 50) :: id
            CHARACTER(LEN = 20) :: tipo
            CHARACTER(LEN = 20) :: alto
            CHARACTER(LEN = 20) :: ancho
            CHARACTER(LEN = 200) :: texto
            CHARACTER(LEN = 200) :: marca
            CHARACTER(LEN = 50) :: color_texto_r
            CHARACTER(LEN = 50) :: color_texto_g
            CHARACTER(LEN = 50) :: color_texto_b
            CHARACTER(LEN = 50) :: color_fondo_r
            CHARACTER(LEN = 50) :: color_fondo_g
            CHARACTER(LEN = 50) :: color_fondo_b
            CHARACTER(LEN = 50) :: posicion_x
            CHARACTER(LEN = 50) :: posicion_y
        End type radioButton
    
        ! Declaración de un arreglo de Tag para almacenar los etiquetas
        type(radioButton), ALLOCATABLE ::  radioButton_array(:)
    
    contains
    subroutine agregar_radio_boton(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(radioButton) :: nuevo_radio_boton
        integer :: n
        type(radioButton), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_radio_boton%id = id
        nuevo_radio_boton%tipo = 'radio_boton'
        nuevo_radio_boton%alto = "25"
        nuevo_radio_boton%ancho = "100"
        nuevo_radio_boton%texto = ""
        nuevo_radio_boton%marca = ""
        nuevo_radio_boton%color_texto_r = ""
        nuevo_radio_boton%color_texto_g = ""
        nuevo_radio_boton%color_texto_b = ""
        nuevo_radio_boton%color_fondo_r = ""
        nuevo_radio_boton%color_fondo_g = ""
        nuevo_radio_boton%color_fondo_b = ""
        nuevo_radio_boton%posicion_x = ""
        nuevo_radio_boton%posicion_y = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(radioButton_array)) then !Si esta vacia
            ALLOCATE(radioButton_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            radioButton_array(1) =  nuevo_radio_boton !Se convierte en el etiqueta nuevo
        else
            n = size(radioButton_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = radioButton_array !Reservo memoria
            temp_array(n+1) = nuevo_radio_boton
            DEALLOCATE(radioButton_array) !Libero memoria
            ALLOCATE(radioButton_array(n+1)) !Reservo memoria de nuevo
            radioButton_array = temp_array
        end if
    end subroutine agregar_radio_boton

    subroutine imprimir_radio_botones()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioButton_array)) then
            print *, "No hay radio_botones"
        else
            print *, "radio_botones encontrados: ", size(radioButton_array)
            DO i = 1, size(radioButton_array)
                print *, 'id: ', trim(radioButton_array(i)%id)
                print *, 'alto: ', trim(radioButton_array(i)%alto)
                print *, 'ancho: ', trim(radioButton_array(i)%ancho)
                print *, 'texto: ', trim(radioButton_array(i)%texto)
                print *, 'color_texto_r: ', trim(radioButton_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(radioButton_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(radioButton_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(radioButton_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(radioButton_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(radioButton_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(radioButton_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(radioButton_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_radio_botones

    subroutine radio_boton_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto

        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioButton_array)) then
            print *, "No hay radio_botones"
        else
            DO i = 1, size(radioButton_array)
                if (trim(radioButton_array(i)%id) == trim(id)) then
                    radioButton_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine radio_boton_set_texto

    subroutine radio_boton_set_marca(id, marca)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: marca

        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioButton_array)) then
            print *, "No hay radio_botones"
        else
            DO i = 1, size(radioButton_array)
                if (trim(radioButton_array(i)%id) == trim(id)) then
                    radioButton_array(i)%marca = marca
                end if
            END DO
        end if

    end subroutine radio_boton_set_marca

    subroutine radio_boton_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y

        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioButton_array)) then
            print *, "No hay radio_botones"
        else
            DO i = 1, size(radioButton_array)
                if (trim(radioButton_array(i)%id) == trim(id)) then
                    radioButton_array(i)%posicion_x = posicion_x
                    radioButton_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine radio_boton_set_posicion


END MODULE radio_boton