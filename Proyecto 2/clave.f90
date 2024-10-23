MODULE clave
    implicit none
        type :: key
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
        End type key
    
        ! Declaración de un arreglo de Tag para almacenar los etiquetas
        type(key), ALLOCATABLE ::  key_array(:)
    
    contains
    subroutine agregar_clave(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(key) :: nuevo_clave
        integer :: n
        type(key), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_clave%id = id
        nuevo_clave%tipo = 'clave'
        nuevo_clave%alto = "25"
        nuevo_clave%ancho = "100"
        nuevo_clave%texto = ""
        nuevo_clave%alineacion_texto = "izquierda"
        nuevo_clave%color_texto_r = ""
        nuevo_clave%color_texto_g = ""
        nuevo_clave%color_texto_b = ""
        nuevo_clave%color_fondo_r = ""
        nuevo_clave%color_fondo_g = ""
        nuevo_clave%color_fondo_b = ""
        nuevo_clave%posicion_x = ""
        nuevo_clave%posicion_y = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(key_array)) then !Si esta vacia
            ALLOCATE(key_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            key_array(1) =  nuevo_clave !Se convierte en el etiqueta nuevo
        else
            n = size(key_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = key_array !Reservo memoria
            temp_array(n+1) = nuevo_clave
            DEALLOCATE(key_array) !Libero memoria
            ALLOCATE(key_array(n+1)) !Reservo memoria de nuevo
            key_array = temp_array
        end if
    end subroutine agregar_clave

    subroutine imprimir_claves()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(key_array)) then
            print *, "No hay claves"
        else
            print *, "claves encontrados: ", size(key_array)
            DO i = 1, size(key_array)
                print *, 'id: ', trim(key_array(i)%id)
                print *, 'alto: ', trim(key_array(i)%alto)
                print *, 'ancho: ', trim(key_array(i)%ancho)
                print *, 'texto: ', trim(key_array(i)%texto)
                print *, 'color_texto_r: ', trim(key_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(key_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(key_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(key_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(key_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(key_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(key_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(key_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_claves

    subroutine clave_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(key_array)) then
            print *, "No hay claves"
        else
            DO i = 1, size(key_array)
                if (key_array(i)%id == id) then
                    key_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine clave_set_texto

    subroutine clave_set_alineacion_texto(id, alineacion_texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion_texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(key_array)) then
            print *, "No hay claves"
        else
            DO i = 1, size(key_array)
                if (key_array(i)%id == id) then
                    key_array(i)%alineacion_texto = alineacion_texto
                end if
            END DO
        end if

    end subroutine clave_set_alineacion_texto

    subroutine clave_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(key_array)) then
            print *, "No hay claves"
        else
            DO i = 1, size(key_array)
                if (key_array(i)%id == id) then
                    key_array(i)%posicion_x = posicion_x
                    key_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine clave_set_posicion

END MODULE clave