MODULE contenedor
    implicit none
        type :: conteiner
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
            CHARACTER(LEN = 50) :: this
            CHARACTER(LEN = 200) :: add
        End type conteiner
    
        ! Declaración de un arreglo de Tag para almacenar los etiquetas
        type(conteiner), ALLOCATABLE ::  conteiner_array(:)
    
    contains
    subroutine agregar_contenedor(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(conteiner) :: nuevo_contenedor
        integer :: n
        type(conteiner), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_contenedor%id = id
        nuevo_contenedor%tipo = 'contenedor'
        nuevo_contenedor%alto = ""
        nuevo_contenedor%ancho = ""
        nuevo_contenedor%texto = ""
        nuevo_contenedor%alineacion_texto = ""
        nuevo_contenedor%color_texto_r = ""
        nuevo_contenedor%color_texto_g = ""
        nuevo_contenedor%color_texto_b = ""
        nuevo_contenedor%color_fondo_r = ""
        nuevo_contenedor%color_fondo_g = ""
        nuevo_contenedor%color_fondo_b = ""
        nuevo_contenedor%posicion_x = ""
        nuevo_contenedor%posicion_y = ""
        nuevo_contenedor%this = ""
        nuevo_contenedor%add = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(conteiner_array)) then !Si esta vacia
            ALLOCATE(conteiner_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            conteiner_array(1) =  nuevo_contenedor !Se convierte en el etiqueta nuevo
        else
            n = size(conteiner_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = conteiner_array !Reservo memoria
            temp_array(n+1) = nuevo_contenedor
            DEALLOCATE(conteiner_array) !Libero memoria
            ALLOCATE(conteiner_array(n+1)) !Reservo memoria de nuevo
            conteiner_array = temp_array
        end if
    end subroutine agregar_contenedor

    subroutine imprimir_contenedores()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay contenedores"
        else
            print *, "contenedores encontrados: ", size(conteiner_array)
            DO i = 1, size(conteiner_array)
                print *, 'id: ', trim(conteiner_array(i)%id)
                print *, 'alto: ', trim(conteiner_array(i)%alto)
                print *, 'ancho: ', trim(conteiner_array(i)%ancho)
                print *, 'texto: ', trim(conteiner_array(i)%texto)
                print *, 'alineacion_texto: ', trim(conteiner_array(i)%alineacion_texto)
                print *, 'color_texto_r: ', trim(conteiner_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(conteiner_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(conteiner_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(conteiner_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(conteiner_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(conteiner_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(conteiner_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(conteiner_array(i)%posicion_y)
                print *, 'this: ', trim(conteiner_array(i)%this)
                print *, 'add: ', trim(conteiner_array(i)%add)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_contenedores

    subroutine contenedor_set_ancho(id, ancho)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: ancho
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%ancho = ancho
                end if
            END DO
        end if

    end subroutine contenedor_set_ancho

    subroutine contenedor_set_alto(id, alto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%alto = alto
                end if
            END DO
        end if

    end subroutine contenedor_set_alto

    subroutine contenedor_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine contenedor_set_texto

    subroutine contenedor_set_alineacion_texto(id, alineacion_texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion_texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%alineacion_texto = alineacion_texto
                end if
            END DO
        end if

    end subroutine contenedor_set_alineacion_texto

    subroutine contenedor_set_color_texto(id, r, g, b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: r
        CHARACTER(LEN=*), INTENT(IN) :: g
        CHARACTER(LEN=*), INTENT(IN) :: b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%color_texto_r = r
                    conteiner_array(i)%color_texto_g = g
                    conteiner_array(i)%color_texto_b = b
                end if
            END DO
        end if

    end subroutine contenedor_set_color_texto

    subroutine contenedor_set_color_fondo(id, r, g, b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: r
        CHARACTER(LEN=*), INTENT(IN) :: g
        CHARACTER(LEN=*), INTENT(IN) :: b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%color_fondo_r = r
                    conteiner_array(i)%color_fondo_g = g
                    conteiner_array(i)%color_fondo_b = b
                end if
            END DO
        end if

    end subroutine contenedor_set_color_fondo

    subroutine contenedor_set_posicion(id, x, y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: x
        CHARACTER(LEN=*), INTENT(IN) :: y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%posicion_x = x
                    conteiner_array(i)%posicion_y = y
                end if
            END DO
        end if

    end subroutine contenedor_set_posicion

    subroutine contenedor_set_this(id, this)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: this
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == id) then
                    conteiner_array(i)%this = this
                end if
            END DO
        end if

    end subroutine contenedor_set_this

    subroutine contenedor_set_add(id, add)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: add
        integer :: i
    
        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == trim(id)) then
                    ! Verifica si ya hay valores en el campo 'add'
                    if (len_trim(conteiner_array(i)%add) > 0) then
                        conteiner_array(i)%add = trim(conteiner_array(i)%add) // ',' // trim(add)
                    else
                        conteiner_array(i)%add = trim(add)
                    end if
                end if
            END DO
        end if
    end subroutine contenedor_set_add
    

    FUNCTION encontrar_contenedor_por_id(id)RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(conteiner_array)) then
            print *, "No hay contenedores"
        else
            DO i = 1, size(conteiner_array)
                if (trim(conteiner_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    exit
                end if
            END DO
        end if
    End FUNCTION encontrar_contenedor_por_id
END MODULE contenedor