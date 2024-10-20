MODULE chequeo
    implicit none
        type :: check
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
        End type check
    
        ! Declaración de un arreglo de Tag para almacenar los etiquetas
        type(check), ALLOCATABLE ::  check_array(:)
    
    contains
    subroutine agregar_chequeo(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(check) :: nuevo_chequeo
        integer :: n
        type(check), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_chequeo%id = id
        nuevo_chequeo%tipo = 'chequeo'
        nuevo_chequeo%alto = "25"
        nuevo_chequeo%ancho = "100"
        nuevo_chequeo%texto = ""
        nuevo_chequeo%color_texto_r = ""
        nuevo_chequeo%color_texto_g = ""
        nuevo_chequeo%color_texto_b = ""
        nuevo_chequeo%color_fondo_r = ""
        nuevo_chequeo%color_fondo_g = ""
        nuevo_chequeo%color_fondo_b = ""
        nuevo_chequeo%posicion_x = ""
        nuevo_chequeo%posicion_y = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(check_array)) then !Si esta vacia
            ALLOCATE(check_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            check_array(1) =  nuevo_chequeo !Se convierte en el etiqueta nuevo
        else
            n = size(check_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = check_array !Reservo memoria
            temp_array(n+1) = nuevo_chequeo
            DEALLOCATE(check_array) !Libero memoria
            ALLOCATE(check_array(n+1)) !Reservo memoria de nuevo
            check_array = temp_array
        end if
    end subroutine agregar_chequeo

    subroutine imprimir_chequeo()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(check_array)) then
            print *, "No hay chequeo"
        else
            print *, "chequeos encontrados: ", size(check_array)
            DO i = 1, size(check_array)
                print *, 'id: ', trim(check_array(i)%id)
                print *, 'alto: ', trim(check_array(i)%alto)
                print *, 'ancho: ', trim(check_array(i)%ancho)
                print *, 'texto: ', trim(check_array(i)%texto)
                print *, 'color_texto_r: ', trim(check_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(check_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(check_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(check_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(check_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(check_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(check_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(check_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_chequeo
END MODULE chequeo