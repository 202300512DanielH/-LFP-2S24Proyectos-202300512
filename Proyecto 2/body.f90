MODULE cuerpo
    implicit none
    type :: body
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: tipo

    End type body
    
    type(body), ALLOCATABLE ::  body_array(:)

contains
subroutine agregar_body(id)
        CHARACTER(LEN=*), INTENT(IN) :: id
    
        type(body) :: nuevo_body
        integer :: n
        type(body), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_body%id = id
        nuevo_body%tipo = 'body'
    
        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(body_array)) then !Si esta vacia
            ALLOCATE(body_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            body_array(1) =  nuevo_body !Se convierte en el etiqueta nuevo
        else
            n = size(body_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = body_array !Reservo memoria
            temp_array(n+1) = nuevo_body
            DEALLOCATE(body_array) !Libero memoria
            ALLOCATE(body_array(n+1)) !Reservo memoria de nuevo
            body_array = temp_array
        end if
    end subroutine agregar_body

    subroutine imprimir_body()
        integer :: i 
        if (.NOT. ALLOCATED(body_array)) then
            print *, "No hay etiquetas body"
        else
            print* , "Etiquetas body encontradas: ", size(body_array)
            do i = 1, size(body_array)
                print *, "id: ", body_array(i)%id
                print *, "tipo: ", body_array(i)%tipo
                print *, "----------------------"
            end do
        end if
    end subroutine imprimir_body
end MODULE cuerpo
