MODULE area_Texto
    implicit none
        type :: text_Area
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
        End type text_Area
    
        ! Declaración de un arreglo de Tag para almacenar los etiquetas
        type(text_Area), ALLOCATABLE ::  text_Area_array(:)
    
    contains
    subroutine agregar_area_Texto(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(text_Area) :: nuevo_area_Texto
        integer :: n
        type(text_Area), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_area_Texto%id = id
        nuevo_area_Texto%tipo = 'area_Texto'
        nuevo_area_Texto%alto = "25"
        nuevo_area_Texto%ancho = "100"
        nuevo_area_Texto%texto = ""
        nuevo_area_Texto%color_texto_r = ""
        nuevo_area_Texto%color_texto_g = ""
        nuevo_area_Texto%color_texto_b = ""
        nuevo_area_Texto%color_fondo_r = ""
        nuevo_area_Texto%color_fondo_g = ""
        nuevo_area_Texto%color_fondo_b = ""
        nuevo_area_Texto%posicion_x = ""
        nuevo_area_Texto%posicion_y = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(text_Area_array)) then !Si esta vacia
            ALLOCATE(text_Area_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            text_Area_array(1) =  nuevo_area_Texto !Se convierte en el etiqueta nuevo
        else
            n = size(text_Area_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = text_Area_array !Reservo memoria
            temp_array(n+1) = nuevo_area_Texto
            DEALLOCATE(text_Area_array) !Libero memoria
            ALLOCATE(text_Area_array(n+1)) !Reservo memoria de nuevo
            text_Area_array = temp_array
        end if
    end subroutine agregar_area_Texto

    subroutine imprimir_area_Texto()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(text_Area_array)) then
            print *, "No hay area_Textoes"
        else
            print *, "area_Textoes encontrados: ", size(text_Area_array)
            DO i = 1, size(text_Area_array)
                print *, 'id: ', trim(text_Area_array(i)%id)
                print *, 'alto: ', trim(text_Area_array(i)%alto)
                print *, 'ancho: ', trim(text_Area_array(i)%ancho)
                print *, 'texto: ', trim(text_Area_array(i)%texto)
                print *, 'color_texto_r: ', trim(text_Area_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(text_Area_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(text_Area_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(text_Area_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(text_Area_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(text_Area_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(text_Area_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(text_Area_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_area_Texto
END MODULE area_Texto