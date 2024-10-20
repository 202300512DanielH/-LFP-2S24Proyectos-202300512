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
        nuevo_boton%color_texto_r = ""
        nuevo_boton%color_texto_g = ""
        nuevo_boton%color_texto_b = ""
        nuevo_boton%color_fondo_r = ""
        nuevo_boton%color_fondo_g = ""
        nuevo_boton%color_fondo_b = ""
        nuevo_boton%posicion_x = ""
        nuevo_boton%posicion_y = ""


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
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_botones
END MODULE boton