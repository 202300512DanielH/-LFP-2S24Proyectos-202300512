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
        nuevo_contenedor%alto = "25"
        nuevo_contenedor%ancho = "100"
        nuevo_contenedor%texto = ""
        nuevo_contenedor%color_texto_r = ""
        nuevo_contenedor%color_texto_g = ""
        nuevo_contenedor%color_texto_b = ""
        nuevo_contenedor%color_fondo_r = ""
        nuevo_contenedor%color_fondo_g = ""
        nuevo_contenedor%color_fondo_b = ""
        nuevo_contenedor%posicion_x = ""
        nuevo_contenedor%posicion_y = ""


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
                print *, 'color_texto_r: ', trim(conteiner_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(conteiner_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(conteiner_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(conteiner_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(conteiner_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(conteiner_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(conteiner_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(conteiner_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_contenedores
END MODULE contenedor