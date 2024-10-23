#  Manual Técnico
El Proyecto está desarrollado en 2 lenguajes: 
- **Python:** Encargada de la interfaz gráfica.
- **Fortran:** Encargada del flujo interno de la aplicación.

## Apartado Gráfico
La aplicación usa **TKinter** para la estructura gráfica, lo que nos permite tener una interfaz agradable para el usuario y funcional.

### Funciones en Pyhton
La siguiente función es muy importante ya que es la que se comunica con el main en fortran que ejecuta el flujo del programa
```
def ejecutar_analisis():
    texto = text_area.get(1.0, "end-1c")  # Obtener el contenido del área de texto
    if not texto.strip():
        messagebox.showwarning("Advertencia", "El área de texto está vacía.")
        return

    # Dividir el texto en líneas
    lineas = texto.split("\n")

    try:
        # Ejecutar el archivo main.exe y pasarle el texto
        process = subprocess.Popen(
            ["./main.exe"],  # Ejecuta el archivo compilado de Fortran
            stdin=subprocess.PIPE,  # Permite pasarle datos de entrada
            stdout=subprocess.PIPE,  # Captura la salida
            stderr=subprocess.PIPE,  # Captura errores
            text=True  # Para manejar texto en lugar de bytes
        )

        # Enviar el número de líneas y el texto al proceso Fortran
        entrada = f"{len(lineas)}\n" + "\n".join(lineas) + "\n"
        stdout, stderr = process.communicate(input=entrada)

        # Mostrar el resultado de la ejecución
        if stderr:
            messagebox.showerror("Error", f"Ocurrió un error al ejecutar Fortran:\n{stderr}")
        else:
            messagebox.showinfo("Resultado", f"Analisis completado con éxito.")
            print(f"{stdout}")
            actualizar_tabla_errores()
             

    except FileNotFoundError:
        messagebox.showerror("Error", "No se encontró el archivo main.exe. Asegúrate de haberlo compilado.")
    except Exception as e:
        messagebox.showerror("Error", f"Ocurrió un error inesperado: {e}")
```

# Fortran
Para el proyecto se aplicó el paradigma de POO trabajando cada parte del código en módulos con funciones propias, las cuales son ejecutadas por el main

A continuación se presenta el analizador léxico
```
program analizador_lexico
    use errores
    use token
    use traductor

    implicit none
    integer :: len, fila, columna, estado, puntero, i, num_lineas
    character(len=100000) :: contenido, linea
    character(len=1) :: char, next_char
    character(len=100) :: aux_tkn

    estado = 0
    puntero = 1
    columna = 0
    fila = 1
    aux_tkn = ""
    contenido = ""

    ! Leer el número de líneas desde stdin
    read(*, *) num_lineas
    if (num_lineas < 1) then
        print *, "Error: El número de líneas debe ser positivo."
        stop
    end if

    ! Leer todas las líneas desde stdin
    do i = 1, num_lineas
        read(*, '(A)') linea
        contenido = trim(contenido) // trim(linea) // new_line('a')
    end do

    len = len_trim(contenido)

    ! Análisis léxico
    do while (puntero <= len)
        char = contenido(puntero:puntero)

        ! Verificar si hay más caracteres para predecir "//" o "/*"
        if (puntero < len) then
            next_char = contenido(puntero + 1:puntero + 1)
        else
            next_char = " "
        end if

        select case (estado)
            case (0)
                if (char == '/' .and. next_char == '/') then
                    estado = 5  ! Estado de comentario de línea
                    puntero = puntero + 2
                    columna = columna + 2
                    aux_tkn = "//"
                elseif (char == '/' .and. next_char == '*') then
                    estado = 6  ! Estado de comentario multi-línea
                    puntero = puntero + 2
                    columna = columna + 2
                    aux_tkn = "/*"
                elseif (char == ';' .or. char == '-' .or. char == '.' .or. char == '(' .or. &
                    char == ')' .or. char == ',' .or. char == '<' .or. char == '>' .or. char == '!' .or. &
                    char == '/' .or. char == '*') then
                    estado = 1
                    columna = columna + 1
                elseif (char >= 'A' .and. char <= 'Z' .or. (char >= 'a' .and. char <= 'z')) then
                    estado = 2
                elseif (char >= '0' .and. char <= '9') then
                    estado = 3
                elseif (char == '"') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                    estado = 4
                elseif (ichar(char) == 10) then
                    columna = 0
                    fila = fila + 1
                    puntero = puntero + 1
                elseif (ichar(char) == 9) then
                    columna = columna + 4
                    puntero = puntero + 1
                elseif (ichar(char) == 32) then
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    call agregar_error_lexico(char, "Token no reconocido", fila, columna)
                    columna = columna + 1
                    puntero = puntero + 1
                end if

            case (1)
                if (char == ';') then
                    call agregar_token(char, 'TKN_pyc', fila, columna)
                elseif (char == '.') then
                    call agregar_token(char, 'TKN_punto', fila, columna)
                elseif (char == ',') then
                    call agregar_token(char, 'TKN_coma', fila, columna)
                elseif (char == '>') then
                    call agregar_token(char, 'TKN_mayor', fila, columna)
                elseif (char == '<') then
                    call agregar_token(char, 'TKN_menor', fila, columna)
                elseif (char == '(') then
                    call agregar_token(char, 'TKN_par_izq', fila, columna)
                elseif (char == ')') then
                    call agregar_token(char, 'TKN_par_der', fila, columna)
                elseif (char == '-') then
                    call agregar_token(char, 'TKN_guion', fila, columna)
                elseif (char == '!') then
                    call agregar_token(char, 'TKN_exp', fila, columna)
                elseif (char == '/') then
                    call agregar_token(char, 'TKN_slash', fila, columna)
                elseif (char == '*') then
                    call agregar_token(char, 'TKN_ast', fila, columna)
                end if
                puntero = puntero + 1
                estado = 0

            case (2)
                if ((char >= 'A' .and. char <= 'Z') .or. (char >= 'a' .and. char <= 'z') .or. &
                    (char >= '0' .and. char <= '9')) then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    !Palabras reservadas de contenedor
                    if ((aux_tkn == 'Etiqueta')) then
                        call agregar_token(aux_tkn, 'TKN_etiqueta', fila, columna)
                    
                    elseif ((aux_tkn == 'Boton')) then
                        call agregar_token(aux_tkn, 'TKN_boton', fila, columna)
                    
                    elseif ((aux_tkn == 'Check')) then
                        call agregar_token(aux_tkn, 'TKN_check', fila, columna)

                    elseif ((aux_tkn == 'RadioBoton')) then
                        call agregar_token(aux_tkn, 'TKN_ra_boton', fila, columna)
                    
                    elseif ((aux_tkn == 'Texto')) then
                        call agregar_token(aux_tkn, 'TKN_texto', fila, columna)
                    
                    elseif ((aux_tkn == 'AreaTexto')) then
                        call agregar_token(aux_tkn, 'TKN_area_texto', fila, columna)

                    elseif ((aux_tkn == 'Clave')) then
                        call agregar_token(aux_tkn, 'TKN_clave', fila, columna)    

                    elseif ((aux_tkn == 'Contenedor')) then
                        call agregar_token(aux_tkn, 'TKN_contenedor', fila, columna)
                    
                        
                    !Palabras reservadas de propiedades
                    
                    elseif ((aux_tkn == 'setAncho')) then
                        call agregar_token(aux_tkn, 'TKN_setAncho', fila, columna)
                    
                    elseif ((aux_tkn == 'setAlto')) then
                        call agregar_token(aux_tkn, 'TKN_setAlto', fila, columna)
                    
                    elseif ((aux_tkn == 'setAlineacion')) then
                        call agregar_token(aux_tkn, 'TKN_setAlineacion', fila, columna)

                    elseif ((aux_tkn == 'centro')) then
                        call agregar_token(aux_tkn, 'TKN_setAlineacion_Pos', fila, columna)    
                    elseif ((aux_tkn == 'izquierdo')) then
                        call agregar_token(aux_tkn, 'TKN_setAlineacion_Pos', fila, columna)
                    elseif ((aux_tkn == 'derecho')) then
                        call agregar_token(aux_tkn, 'TKN_setAlineacion_Pos', fila, columna)
                    
                    elseif ((aux_tkn == 'setColorFondo')) then
                        call agregar_token(aux_tkn, 'TKN_setColorFondo', fila, columna)

                    elseif ((aux_tkn == 'setColorLetra')) then
                        call agregar_token(aux_tkn, 'TKN_setColorLetra', fila, columna)
                    
                    elseif ((aux_tkn == 'setTexto')) then
                        call agregar_token(aux_tkn, 'TKN_setTexto', fila, columna)
                    
                    elseif ((aux_tkn == 'setMarcada')) then
                        call agregar_token(aux_tkn, 'TKN_setMarc', fila, columna)
                    
                    elseif ((aux_tkn == 'false')) then
                        call agregar_token(aux_tkn, 'TKN_False', fila, columna)

                    elseif ((aux_tkn == 'true')) then
                        call agregar_token(aux_tkn, 'TKN_True', fila, columna)
            
                    elseif ((aux_tkn == 'setGrupo')) then
                        call agregar_token(aux_tkn, 'TKN_setGrupo', fila, columna)
                    
                    !Palabras reservadas de posicion

                    elseif ((aux_tkn == 'setPosicion')) then
                        call agregar_token(aux_tkn, 'TKN_setPosicion', fila, columna)
                    
                    elseif (aux_tkn == 'this') then
                        call agregar_token(aux_tkn, 'TKN_this', fila, columna)
                    
                    elseif (aux_tkn == 'add') then
                        call agregar_token(aux_tkn, 'TKN_add', fila, columna)
                        
                    !Palabras reservadas de bloque
                    elseif (aux_tkn == 'Controles') then
                        call agregar_token(aux_tkn, 'TKN_nm_bloque_ctrls', fila, columna)
                    
                    elseif (aux_tkn == 'Propiedades') then
                        call agregar_token(aux_tkn, 'TKN_nm_bloque_props', fila, columna)
                    
                    elseif (aux_tkn == 'Colocacion') then
                        call agregar_token(aux_tkn, 'TKN_nm_bloque_coloc', fila, columna)

                    else 
                        call agregar_token(aux_tkn, 'TKN_id', fila, columna)

                    end if

                    aux_tkn = ""
                    estado = 0      
                        
                end if

            case (3)
                if (char >= '0' .and. char <= '9') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    call agregar_token(aux_tkn, 'TKN_num', fila, columna)
                    aux_tkn = ""
                    estado = 0
                end if

            case (4)
                ! Acumulamos todo hasta encontrar el cierre del literal con comillas dobles
                if (char /= '"') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                else
                    ! Añadimos también la comilla de cierre al token
                    aux_tkn = trim(aux_tkn) // char
                    call agregar_token(aux_tkn, 'TKN_literal', fila, columna)
                    aux_tkn = ""
                    puntero = puntero + 1
                    estado = 0
                end if
            

            case (5)  ! Estado de comentario de línea
                if (ichar(char) == 10) then  ! Salto de línea
                    call agregar_token(aux_tkn, 'TKN_comentario', fila, columna)
                    aux_tkn = ""
                    columna = 0
                    fila = fila + 1
                    puntero = puntero + 1
                    estado = 0
                else
                    aux_tkn = trim(aux_tkn) // char
                    puntero = puntero + 1
                    columna = columna + 1
                end if

            case (6)  ! Estado de comentario multi-línea
                if (char == '*' .and. next_char == '/') then
                    aux_tkn = trim(aux_tkn) // "*/"
                    call agregar_token(aux_tkn, 'TKN_comentario', fila, columna)
                    aux_tkn = ""
                    puntero = puntero + 2
                    columna = columna + 2
                    estado = 0
                else
                    aux_tkn = trim(aux_tkn) // char
                    puntero = puntero + 1
                    columna = columna + 1
                end if
        end select
    end do

    !Fase léxica

    call generar_json_tokens

    !Fase sintáctica
    call parser
    !Errores
    call guardar_errores_json

    !Resultado Final de las 2 fases de análisis
    !call imprimir_body
    !call imprimir_etiquetas
    !call imprimir_botones
    !call imprimir_contenedores
    !call imprimir_claves
    !call imprimir_texto
    !call imprimir_chequeo
    !call imprimir_radio_botones
    !call imprimir_area_texto
    call generar_html
    


end program analizador_lexico


```
## `Modulos` 
Definimos un objeto con una estructura genérica para almacenar correctamente la información de cada elemento, a continuacion se presenta una de las más completas estructuras
```
MODULE etiqueta
    implicit none

    type :: Tag
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

    End type Tag

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(Tag), ALLOCATABLE ::  etiqueta_array(:)
    

contains

    ! Subrutina para agregar etiquetas a la lista de etiqueta
    subroutine agregar_etiqueta(id )
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(Tag) :: nuevo_etiqueta
        integer :: n
        type(Tag), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_etiqueta%id = id
        nuevo_etiqueta%tipo = 'Etiqueta'
        nuevo_etiqueta%alto = ""
        nuevo_etiqueta%ancho = ""
        nuevo_etiqueta%texto = ""
        nuevo_etiqueta%alineacion_texto = ""
        nuevo_etiqueta%color_texto_r = ""
        nuevo_etiqueta%color_texto_g = ""
        nuevo_etiqueta%color_texto_b = ""
        nuevo_etiqueta%color_fondo_r = ""
        nuevo_etiqueta%color_fondo_g = ""
        nuevo_etiqueta%color_fondo_b = ""
        nuevo_etiqueta%posicion_x = ""
        nuevo_etiqueta%posicion_y = ""
        nuevo_etiqueta%add = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(etiqueta_array)) then !Si esta vacia
            ALLOCATE(etiqueta_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            etiqueta_array(1) =  nuevo_etiqueta !Se convierte en el etiqueta nuevo
        else
            n = size(etiqueta_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = etiqueta_array !Reservo memoria
            temp_array(n+1) = nuevo_etiqueta
            DEALLOCATE(etiqueta_array) !Libero memoria
            ALLOCATE(etiqueta_array(n+1)) !Reservo memoria de nuevo
            etiqueta_array = temp_array
        end if
    end subroutine agregar_etiqueta

    subroutine imprimir_etiquetas()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            print *, "etiquetas encontrados: ", size(etiqueta_array)
            DO i = 1, size(etiqueta_array)
                print *, 'id: ', trim(etiqueta_array(i)%id)
                print *, 'alto: ', trim(etiqueta_array(i)%alto)
                print *, 'ancho: ', trim(etiqueta_array(i)%ancho)
                print *, 'texto: ', trim(etiqueta_array(i)%texto)
                print *, 'alineacion_texto: ', trim(etiqueta_array(i)%alineacion_texto)
                print *, 'color_texto_r: ', trim(etiqueta_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(etiqueta_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(etiqueta_array(i)%color_texto_b)
                print *, 'color_fondo_r: ', trim(etiqueta_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(etiqueta_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(etiqueta_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(etiqueta_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(etiqueta_array(i)%posicion_y)
                print *, 'add: ', trim(etiqueta_array(i)%add)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_etiquetas

    ! Subrutina para buscar una etiqueta por su id
    subroutine etiqueta_set_alto(id, alto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%alto = alto
                end if
            END DO
        end if

    end subroutine etiqueta_set_alto

    subroutine etiqueta_set_ancho(id, ancho)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: ancho
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%ancho = ancho
                end if
            END DO
        end if

    end subroutine etiqueta_set_ancho

    subroutine etiqueta_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine etiqueta_set_texto

    subroutine etiqueta_set_alineacion_texto(id, alineacion_texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion_texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%alineacion_texto = alineacion_texto
                end if
            END DO
        end if

    end subroutine etiqueta_set_alineacion_texto

    subroutine etiqueta_set_color_texto(id, color_texto_r, color_texto_g, color_texto_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_r
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_g
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%color_texto_r = color_texto_r
                    etiqueta_array(i)%color_texto_g = color_texto_g
                    etiqueta_array(i)%color_texto_b = color_texto_b
                end if
            END DO
        end if

    end subroutine etiqueta_set_color_texto

    subroutine etiqueta_set_color_fondo(id, color_fondo_r, color_fondo_g, color_fondo_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_r
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_g
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%color_fondo_r = color_fondo_r
                    etiqueta_array(i)%color_fondo_g = color_fondo_g
                    etiqueta_array(i)%color_fondo_b = color_fondo_b
                end if
            END DO
        end if

    end subroutine etiqueta_set_color_fondo

    subroutine etiqueta_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%posicion_x = posicion_x
                    etiqueta_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine etiqueta_set_posicion


    subroutine etiqueta_set_add(id, add)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: add
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then

                    if (trim(etiqueta_array(i)%add) /= "") then
                        etiqueta_array(i)%add = trim(etiqueta_array(i)%add) // ", " // add
                    else
                        etiqueta_array(i)%add = trim(add)
                    end if
                end if
            END DO
        end if

    end subroutine etiqueta_set_add

    ! Función para buscar una etiqueta por su ID
    FUNCTION buscar_etiqueta_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_etiqueta_por_id

END MODULE etiqueta

```

## **Tabla de Tokens**

| **Token**                  | **Descripción**                      | **Expresión Regular (RegEx)**        |
|----------------------------|--------------------------------------|-------------------------------------|
| `TKN_pyc`                  | Punto y coma                         | `;`                                 |
| `TKN_punto`                | Punto                                | `\.`                                |
| `TKN_coma`                 | Coma                                 | `,`                                 |
| `TKN_mayor`                | Mayor que                            | `>`                                 |
| `TKN_menor`                | Menor que                            | `<`                                 |
| `TKN_par_izq`              | Paréntesis izquierdo                 | `\(`                                |
| `TKN_par_der`              | Paréntesis derecho                   | `\)`                                |
| `TKN_guion`                | Guion                                | `-`                                 |
| `TKN_exp`                  | Exclamación                          | `!`                                 |
| `TKN_slash`                | Barra inclinada                      | `/`                                 |
| `TKN_ast`                  | Asterisco                            | `\*`                                |
| `TKN_id`                   | Identificador                        | `[a-zA-Z][a-zA-Z0-9]*`              |
| `TKN_num`                  | Número entero                        | `[0-9]+`                            |
| `TKN_literal`              | Literal entre comillas dobles        | `".*?"`                             |
| `TKN_comentario`           | Comentario                           | `//.*|/\*[\s\S]*?\*/`               |
| `TKN_etiqueta`             | Palabra reservada `Etiqueta`         | `Etiqueta`                          |
| `TKN_boton`                | Palabra reservada `Boton`            | `Boton`                             |
| `TKN_check`                | Palabra reservada `Check`            | `Check`                             |
| `TKN_ra_boton`             | Palabra reservada `RadioBoton`       | `RadioBoton`                        |
| `TKN_texto`                | Palabra reservada `Texto`            | `Texto`                             |
| `TKN_area_texto`           | Palabra reservada `AreaTexto`        | `AreaTexto`                         |
| `TKN_clave`                | Palabra reservada `Clave`            | `Clave`                             |
| `TKN_contenedor`           | Palabra reservada `Contenedor`       | `Contenedor`                        |
| `TKN_setAncho`             | Propiedad `setAncho`                 | `setAncho`                          |
| `TKN_setAlto`              | Propiedad `setAlto`                  | `setAlto`                           |
| `TKN_setAlineacion`        | Propiedad `setAlineacion`            | `setAlineacion`                     |
| `TKN_setAlineacion_centro` | Alineación `centro`                  | `centro`                            |
| `TKN_setAlineacion_Izq`    | Alineación `izquierdo`               | `izquierdo`                         |
| `TKN_setAlineacion_Der`    | Alineación `derecho`                 | `derecho`                           |
| `TKN_setColorFondo`        | Propiedad `setColorFondo`            | `setColorFondo`                     |
| `TKN_setColorLetra`        | Propiedad `setColorLetra`            | `setColorLetra`                     |
| `TKN_setTexto`             | Propiedad `setTexto`                 | `setTexto`                          |
| `TKN_setMarc`              | Propiedad `setMarcada`               | `setMarcada`                        |
| `TKN_False`                | Valor booleano `false`               | `false`                             |
| `TKN_True`                 | Valor booleano `true`                | `true`                              |
| `TKN_setGrupo`             | Propiedad `setGrupo`                 | `setGrupo`                          |
| `TKN_setPosicion`          | Propiedad `setPosicion`              | `setPosicion`                       |
| `TKN_this`                 | Palabra clave `this`                 | `this`                              |
| `TKN_add`                  | Palabra clave `add`                  | `add`                               |
| `TKN_nm_bloque_ctrls`      | Bloque `Controles`                   | `Controles`                         |
| `TKN_nm_bloque_props`      | Bloque `Propiedades`                 | `Propiedades`                       |
| `TKN_nm_bloque_coloc`      | Bloque `Colocacion`                  | `Colocacion`                        |

# **Automata Finito**
![alt text](<Autómata P14.png>) 
![alt text](<Autómata P15.png>) 
![alt text](<Autómata P16.png>)

# **Gramática Libre de Contexto (GLC)**

## **Reglas Gramaticales**

```bnf
<programa> ::= <bloque_controles> | <bloque_propiedades> | <bloque_colocacion> | <control> | <propiedad> | <body>

<bloque_controles> ::= "<--" "Controles" | "Controles" "-->" 
<bloque_propiedades> ::= "<--" "Propiedades" | "Propiedades" "-->" 
<bloque_colocacion> ::= "<--" "Colocacion" | "Colocacion" "-->"

<control> ::= <etiqueta> | <boton> | <contenedor> | <clave> | <texto> | <check> | <radio_boton> | <area_texto>

<etiqueta> ::= "Etiqueta" <identificador> ";"
<boton> ::= "Boton" <identificador> ";"
<contenedor> ::= "Contenedor" <identificador> ";"
<clave> ::= "Clave" <identificador> ";"
<texto> ::= "Texto" <identificador> ";"
<check> ::= "Check" <identificador> ";"
<radio_boton> ::= "RadioBoton" <identificador> ";"
<area_texto> ::= "AreaTexto" <identificador> ";"

<propiedad> ::= <identificador> "." <propiedad_nombre> "(" <valor> ")" ";"

<propiedad_nombre> ::= "setAncho" | "setAlto" | "setTexto" | "setColorFondo" | "setColorLetra" | "setAlineacion" | "setPosicion" | "setMarcada"

<valor> ::= <numero> | <literal> | <alineacion> | <color> | <booleano>
<alineacion> ::= "centro" | "izquierdo" | "derecho"
<color> ::= <numero> "," <numero> "," <numero>
<booleano> ::= "true" | "false"

<body> ::= "this" "." "add" "(" <identificador> ")" ";"

<numero> ::= [0-9]+
<literal> ::= "\"" .* "\""
<identificador> ::= [a-zA-Z][a-zA-Z0-9]*
