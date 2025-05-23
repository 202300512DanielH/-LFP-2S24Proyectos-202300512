MODULE texto
    implicit none
        type :: text
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
        End type text
    
        ! Declaración de un arreglo de Tag para almacenar los etiquetas
        type(text), ALLOCATABLE ::  text_array(:)
    
    contains
    subroutine agregar_texto(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(text) :: nuevo_texto
        integer :: n
        type(text), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_texto%id = id
        nuevo_texto%tipo = 'texto'
        nuevo_texto%alto = "25"
        nuevo_texto%ancho = "100"
        nuevo_texto%texto = ""
        nuevo_texto%color_texto_r = ""
        nuevo_texto%color_texto_g = ""
        nuevo_texto%color_texto_b = ""
        nuevo_texto%color_fondo_r = ""
        nuevo_texto%color_fondo_g = ""
        nuevo_texto%color_fondo_b = ""
        nuevo_texto%posicion_x = ""
        nuevo_texto%posicion_y = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(text_array)) then !Si esta vacia
            ALLOCATE(text_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            text_array(1) =  nuevo_texto !Se convierte en el etiqueta nuevo
        else
            n = size(text_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = text_array !Reservo memoria
            temp_array(n+1) = nuevo_texto
            DEALLOCATE(text_array) !Libero memoria
            ALLOCATE(text_array(n+1)) !Reservo memoria de nuevo
            text_array = temp_array
        end if
    end subroutine agregar_texto

    subroutine imprimir_texto()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(text_array)) then
            print *, "No hay textos"
        else
            print *, "textoes encontrados: ", size(text_array)
            DO i = 1, size(text_array)
                print *, 'id: ', trim(text_array(i)%id)
                print *, 'alto: ', trim(text_array(i)%alto)
                print *, 'ancho: ', trim(text_array(i)%ancho)
                print *, 'texto: ', trim(text_array(i)%texto)
                print *, 'color_texto_r: ', trim(text_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(text_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(text_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(text_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(text_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(text_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(text_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(text_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_texto

    subroutine texto_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto

        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(text_array)) then
            print *, "No hay textoes"
        else
            DO i = 1, size(text_array)
                if (text_array(i)%id == id) then
                    text_array(i)%texto = texto
                    return
                end if
            END DO
        end if

    end subroutine texto_set_texto

    subroutine texto_set_alineacion_texto(id, alineacion_texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion_texto

        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(text_array)) then
            print *, "No hay textoes"
        else
            DO i = 1, size(text_array)
                if (text_array(i)%id == id) then
                    text_array(i)%alineacion_texto = alineacion_texto
                    return
                end if
            END DO
        end if

    end subroutine texto_set_alineacion_texto 

    subroutine texto_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y

        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(text_array)) then
            print *, "No hay textoes"
        else
            DO i = 1, size(text_array)
                if (text_array(i)%id == id) then
                    text_array(i)%posicion_x = posicion_x
                    text_array(i)%posicion_y = posicion_y
                    return
                end if
            END DO
        end if

    end subroutine texto_set_posicion
END MODULE texto