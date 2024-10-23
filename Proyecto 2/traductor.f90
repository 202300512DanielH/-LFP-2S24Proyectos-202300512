MODULE traductor
    USE cuerpo
    USE contenedor
    USE boton
    USE clave
    USE etiqueta
    USE texto
    USE chequeo
    USE radio_boton
    USE area_Texto
    IMPLICIT NONE

    LOGICAL, ALLOCATABLE :: added_flags(:)  ! Bandera para evitar duplicados

CONTAINS

SUBROUTINE generar_html()
    CHARACTER(LEN=1000) :: html_header

    PRINT *, "Iniciando generación del HTML."

    ! Inicializar HTML y CSS
    html_header = "<!DOCTYPE html>" // NEW_LINE('A') // &
                  "<html lang='es'>" // NEW_LINE('A') // &
                  "<head><meta charset='UTF-8'>" // NEW_LINE('A') // &
                  "<title>Página Generada</title>" // NEW_LINE('A') // &
                  "<link rel='stylesheet' type='text/css' href='A_styles.css'>" // NEW_LINE('A') // &
                  "</head><body>" // NEW_LINE('A')

    CALL guardar_archivo("A_output.html", html_header, .FALSE.)
    CALL guardar_archivo("A_styles.css", "", .FALSE.)

    CALL inicializar_flags()

    ! Procesar todos los elementos que vienen en los bodies
    CALL procesar_body_array()

    CALL guardar_archivo("A_output.html", "</body></html>" // NEW_LINE('A'), .TRUE.)

    PRINT *, "Generación de HTML finalizada."
END SUBROUTINE generar_html

SUBROUTINE inicializar_flags()
    ALLOCATE(added_flags(SIZE(body_array)))
    added_flags = .FALSE.
    PRINT *, "Banderas inicializadas."
END SUBROUTINE inicializar_flags

SUBROUTINE procesar_body_array()
    INTEGER :: i

    PRINT *, "Procesando cuerpos principales (body)."

    DO i = 1, SIZE(body_array)
        PRINT *, "Procesando body:", TRIM(body_array(i)%id)

        ! Intentamos procesar cada body sin marcarlo como procesado hasta confirmarlo
        CALL procesar_elemento_individual(body_array(i)%id, "A_output.html")
    END DO
END SUBROUTINE procesar_body_array


SUBROUTINE convertir_contenedor_html(index, nombre_archivo)
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo

    PRINT *, "Generando HTML para contenedor:", conteiner_array(index)%id

    CALL guardar_archivo(nombre_archivo, "<div id='" // TRIM(conteiner_array(index)%id) // "'>" // NEW_LINE('A'), .TRUE.)

    IF (LEN_TRIM(conteiner_array(index)%add) > 0) THEN
        CALL procesar_elementos_hijos(conteiner_array(index)%add, nombre_archivo)
    END IF

    CALL guardar_archivo(nombre_archivo, "</div>" // NEW_LINE('A'), .TRUE.)
END SUBROUTINE convertir_contenedor_html

   
SUBROUTINE procesar_elementos_hijos(add, nombre_archivo)
    CHARACTER(LEN=*), INTENT(IN) :: add, nombre_archivo
    CHARACTER(LEN=50), ALLOCATABLE :: ids(:)
    INTEGER :: j

    CALL dividir_cadena(add, ",", ids)

    PRINT *, "IDs de hijos encontrados:", SIZE(ids)

    DO j = 1, SIZE(ids)
        PRINT *, "Procesando hijo con ID:", TRIM(ids(j))
        CALL procesar_elemento_individual(TRIM(ids(j)), nombre_archivo)
    END DO
END SUBROUTINE procesar_elementos_hijos

SUBROUTINE procesar_elemento_individual(id, nombre_archivo)
    CHARACTER(LEN=*), INTENT(IN) :: id, nombre_archivo
    INTEGER :: k

    PRINT *, "Buscando elemento con ID:", TRIM(ADJUSTL(id))

    ! Procesar si es un contenedor
    DO k = 1, SIZE(conteiner_array)
        IF (TRIM(ADJUSTL(conteiner_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Contenedor encontrado:", conteiner_array(k)%id
            CALL convertir_contenedor_html(k, nombre_archivo)
            CALL escribir_css_contenedor(conteiner_array(k))
            RETURN  ! Salimos porque ya encontramos y procesamos el elemento
        END IF
    END DO

    ! Verificar si es un botón
    DO k = 1, SIZE(button_array)
        IF (TRIM(ADJUSTL(button_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Botón encontrado:", button_array(k)%id
            CALL convertir_boton_html(k, nombre_archivo)
            CALL escribir_css_button(button_array(k))
            RETURN
        END IF
    END DO

    ! Verificar si es una clave
    DO k = 1, SIZE(key_array)
        IF (TRIM(ADJUSTL(key_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Clave encontrada:", key_array(k)%id
            CALL convertir_clave_html(k, nombre_archivo)
            CALL escribir_css_key(key_array(k))
            RETURN
        END IF
    END DO

    ! Verificar si es una etiqueta
    DO k = 1, SIZE(etiqueta_array)
        IF (TRIM(ADJUSTL(etiqueta_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Etiqueta encontrada:", etiqueta_array(k)%id
            CALL convertir_etiqueta_html(k, nombre_archivo)
            CALL escribir_css_etiqueta(etiqueta_array(k))
            RETURN
        END IF
    END DO

    ! Verificar si es un texto
    DO k = 1, SIZE(text_array)
        IF (TRIM(ADJUSTL(text_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Texto encontrado:", text_array(k)%id
            CALL convertir_texto_html(k, nombre_archivo)
            CALL escribir_css_texto(text_array(k))
            RETURN
        END IF
    END DO

    DO k = 1, SIZE(check_array)
        IF (TRIM(ADJUSTL(check_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Chequeo encontrado:", check_array(k)%id
            CALL convertir_chequeo_html(k, nombre_archivo)
            CALL escribir_css_chequeo(check_array(k))
            RETURN
        END IF
    END DO

    DO k = 1, SIZE(radioButton_array)
        IF (TRIM(ADJUSTL(radioButton_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Radio Boton encontrado:", radioButton_array(k)%id
            CALL convertir_radio_boton_html(k, nombre_archivo)
            CALL escribir_css_radio_boton(radioButton_array(k))
            RETURN
        END IF
    END DO

    DO k = 1, SIZE(text_Area_array)
        IF (TRIM(ADJUSTL(text_Area_array(k)%id)) == TRIM(ADJUSTL(id))) THEN
            PRINT *, "Area de Texto encontrada:", text_Area_array(k)%id
            CALL convertir_area_Texto_html(k, nombre_archivo)
            CALL escribir_css_text_Area(text_Area_array(k))
            RETURN
        END IF
    END DO

    PRINT *, "Elemento no encontrado para ID:", id
END SUBROUTINE procesar_elemento_individual

SUBROUTINE escribir_css_contenedor(c)
    TYPE(conteiner), INTENT(IN) :: c
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(c%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(c%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(c%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(c%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(c%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(c%color_fondo_r) // "," // &
               TRIM(c%color_fondo_g) // "," // TRIM(c%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(c%color_texto_r) // "," // &
               TRIM(c%color_texto_g) // "," // TRIM(c%color_texto_b) // ");" // NEW_LINE('A') // &
               "}" 

    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_contenedor

SUBROUTINE escribir_css_button(b)
    TYPE(button), INTENT(IN) :: b
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(b%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(b%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(b%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(b%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(b%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(b%color_fondo_r) // "," // &
               TRIM(b%color_fondo_g) // "," // TRIM(b%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(b%color_texto_r) // "," // &
               TRIM(b%color_texto_g) // "," // TRIM(b%color_texto_b) // ");" // NEW_LINE('A') // &
               "  text-align: " // TRIM(b%alineacion_texto) // ";" // NEW_LINE('A') // &
               "}"
    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_button

SUBROUTINE escribir_css_key(k) 
    TYPE(key), INTENT(IN) :: k
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(k%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(k%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(k%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(k%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(k%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(k%color_fondo_r) // "," // &
               TRIM(k%color_fondo_g) // "," // TRIM(k%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(k%color_texto_r) // "," // &
               TRIM(k%color_texto_g) // "," // TRIM(k%color_texto_b) // ");" // NEW_LINE('A') // &
               "}"
    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_key

SUBROUTINE escribir_css_etiqueta(e) 
    TYPE(taG), INTENT(IN) :: e
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(e%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(e%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(e%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(e%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(e%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(e%color_fondo_r) // "," // &
               TRIM(e%color_fondo_g) // "," // TRIM(e%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(e%color_texto_r) // "," // &
               TRIM(e%color_texto_g) // "," // TRIM(e%color_texto_b) // ");" // NEW_LINE('A') // &
               "}"
    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_etiqueta

SUBROUTINE escribir_css_texto(t) 
    TYPE(text), INTENT(IN) :: t
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(t%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(t%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(t%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(t%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(t%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(t%color_fondo_r) // "," // &
               TRIM(t%color_fondo_g) // "," // TRIM(t%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(t%color_texto_r) // "," // &
               TRIM(t%color_texto_g) // "," // TRIM(t%color_texto_b) // ");" // NEW_LINE('A') // &
               "}"
    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_texto

SUBROUTINE escribir_css_chequeo(h)
    TYPE(check), INTENT(IN) :: h
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(h%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(h%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(h%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(h%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(h%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(h%color_fondo_r) // "," // &
               TRIM(h%color_fondo_g) // "," // TRIM(h%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(h%color_texto_r) // "," // &
               TRIM(h%color_texto_g) // "," // TRIM(h%color_texto_b) // ");" // NEW_LINE('A') // &
               "}"
    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_chequeo

SUBROUTINE escribir_css_radio_boton(r)
    TYPE(radioButton), INTENT(IN) :: r
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(r%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(r%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(r%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(r%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(r%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(r%color_fondo_r) // "," // &
               TRIM(r%color_fondo_g) // "," // TRIM(r%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(r%color_texto_r) // "," // &
               TRIM(r%color_texto_g) // "," // TRIM(r%color_texto_b) // ");" // NEW_LINE('A') // &
               "}"
    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_radio_boton

SUBROUTINE escribir_css_text_Area(ta)
    TYPE(text_Area), INTENT(IN) :: ta
    CHARACTER(LEN=500) :: css_rule

    css_rule = "#" // TRIM(ta%id) // " {" // NEW_LINE('A') // &
               "  position: absolute;" // NEW_LINE('A') // &
               "  left: " // TRIM(ta%posicion_x) // "px;" // NEW_LINE('A') // &
               "  top: " // TRIM(ta%posicion_y) // "px;" // NEW_LINE('A') // &
               "  width: " // TRIM(ta%ancho) // "px;" // NEW_LINE('A') // &
               "  height: " // TRIM(ta%alto) // "px;" // NEW_LINE('A') // &
               "  background-color: rgb(" // TRIM(ta%color_fondo_r) // "," // &
               TRIM(ta%color_fondo_g) // "," // TRIM(ta%color_fondo_b) // ");" // NEW_LINE('A') // &
               "  color: rgb(" // TRIM(ta%color_texto_r) // "," // &
               TRIM(ta%color_texto_g) // "," // TRIM(ta%color_texto_b) // ");" // NEW_LINE('A') // &
               "}"
    CALL guardar_archivo("A_styles.css", css_rule, .TRUE.)
END SUBROUTINE escribir_css_text_Area

SUBROUTINE dividir_cadena(cadena, separador, partes)
    CHARACTER(LEN=*), INTENT(IN) :: cadena, separador
    CHARACTER(LEN=50), ALLOCATABLE, INTENT(OUT) :: partes(:)
    INTEGER :: n, pos, i
    CHARACTER(LEN=LEN(cadena)) :: copia

    copia = TRIM(cadena)
    n = 0

    DO WHILE (INDEX(copia, separador) > 0)
        n = n + 1
        copia = copia(INDEX(copia, separador) + 1:)
    END DO
    n = n + 1

    ALLOCATE(partes(n))

    copia = TRIM(cadena)
    i = 1

    DO WHILE (INDEX(copia, separador) > 0)
        pos = INDEX(copia, separador)
        partes(i) = ADJUSTL(copia(1:pos - 1))
        copia = copia(pos + 1:)
        i = i + 1
    END DO
    partes(i) = ADJUSTL(copia)
END SUBROUTINE dividir_cadena
    

    SUBROUTINE agregar_elemento_por_id(id, nombre_archivo)
        CHARACTER(LEN=*), INTENT(IN) :: id, nombre_archivo
        INTEGER :: j

        ! Verificar y agregar botones
        DO j = 1, SIZE(button_array)
            IF (TRIM(button_array(j)%id) == TRIM(id) .AND. .NOT. added_flags(j)) THEN
                CALL convertir_boton_html(j, nombre_archivo)
                added_flags(j) = .TRUE.
            END IF
        END DO

        ! Verificar y agregar claves
        DO j = 1, SIZE(key_array)
            IF (TRIM(key_array(j)%id) == TRIM(id) .AND. .NOT. added_flags(j)) THEN
                CALL convertir_clave_html(j, nombre_archivo)
                added_flags(j) = .TRUE.
            END IF
        END DO

        ! Verificar y agregar etiquetas
        DO j = 1, SIZE(etiqueta_array)
            IF (TRIM(etiqueta_array(j)%id) == TRIM(id) .AND. .NOT. added_flags(j)) THEN
                CALL convertir_etiqueta_html(j, nombre_archivo)
                added_flags(j) = .TRUE.
            END IF
        END DO

        ! Verificar y agregar textos
        DO j = 1, SIZE(text_array)
            IF (TRIM(text_array(j)%id) == TRIM(id) .AND. .NOT. added_flags(j)) THEN
                CALL convertir_texto_html(j, nombre_archivo)
                added_flags(j) = .TRUE.
            END IF
        END DO

        ! Verificar y agregar chequeos
        DO j = 1, SIZE(check_array)
            IF (TRIM(check_array(j)%id) == TRIM(id) .AND. .NOT. added_flags(j)) THEN
                CALL convertir_chequeo_html(j, nombre_archivo)
                added_flags(j) = .TRUE.
            END IF
        END DO

        ! Verificar y agregar radio botones
        DO j = 1, SIZE(radioButton_array)
            IF (TRIM(radioButton_array(j)%id) == TRIM(id) .AND. .NOT. added_flags(j)) THEN
                CALL convertir_radio_boton_html(j, nombre_archivo)
                added_flags(j) = .TRUE.
            END IF
        END DO

        ! Verificar y agregar áreas de texto
        DO j = 1, SIZE(text_Area_array)
            IF (TRIM(text_Area_array(j)%id) == TRIM(id) .AND. .NOT. added_flags(j)) THEN
                CALL convertir_area_Texto_html(j, nombre_archivo)
                added_flags(j) = .TRUE.
            END IF
        END DO
    END SUBROUTINE agregar_elemento_por_id

    SUBROUTINE convertir_boton_html(j, nombre_archivo)
        INTEGER, INTENT(IN) :: j
        CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo
        CALL guardar_archivo(nombre_archivo, "<input type='submit' id='" // &
                             TRIM(button_array(j)%id) // "' value='" // TRIM(button_array(j)%texto) // "' />" // NEW_LINE('A'), .TRUE.)
    END SUBROUTINE convertir_boton_html

    SUBROUTINE convertir_clave_html(j, nombre_archivo)
        INTEGER, INTENT(IN) :: j
        CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo
        CALL guardar_archivo(nombre_archivo, "<input type='password' id='" // &
                             TRIM(key_array(j)%id) // "' />" // NEW_LINE('A'), .TRUE.)
    END SUBROUTINE convertir_clave_html

    SUBROUTINE convertir_etiqueta_html(j, nombre_archivo)
        INTEGER, INTENT(IN) :: j
        CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo
        CALL guardar_archivo(nombre_archivo, "<label id='" // TRIM(etiqueta_array(j)%id) // "'>" // &
                             TRIM(etiqueta_array(j)%texto) // "</label>" // NEW_LINE('A'), .TRUE.)
    END SUBROUTINE convertir_etiqueta_html

    SUBROUTINE convertir_texto_html(j, nombre_archivo)
        INTEGER, INTENT(IN) :: j
        CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo
        CALL guardar_archivo(nombre_archivo, "<input type='text' id='" // &
                             TRIM(text_array(j)%id) // "' />" // NEW_LINE('A'), .TRUE.)
    END SUBROUTINE convertir_texto_html

    SUBROUTINE convertir_chequeo_html(j, nombre_archivo)
        INTEGER, INTENT(IN) :: j
        CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo
        CALL guardar_archivo(nombre_archivo, "<input type='checkbox' id='" // &
                             TRIM(check_array(j)%id) // "' " // TRIM(check_array(j)%Marca)// " />" // NEW_LINE('A'), .TRUE.)
    END SUBROUTINE convertir_chequeo_html

    SUBROUTINE convertir_radio_boton_html(j, nombre_archivo)
        INTEGER, INTENT(IN) :: j
        CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo
        CALL guardar_archivo(nombre_archivo, "<input type='radio' id='" // &
                             TRIM(radioButton_array(j)%id) // "' " // TRIM(radioButton_array(j)%marca)// " />" // NEW_LINE('A'), .TRUE.)
    END SUBROUTINE convertir_radio_boton_html

    SUBROUTINE convertir_area_Texto_html(j, nombre_archivo)
        INTEGER, INTENT(IN) :: j
        CHARACTER(LEN=*), INTENT(IN) :: nombre_archivo
        CALL guardar_archivo(nombre_archivo, "<textarea id='" // &
                             TRIM(text_Area_array(j)%id) // "'>" // TRIM(text_Area_array(j)%texto) // "</textarea>" // NEW_LINE('A'), .TRUE.)
    END SUBROUTINE convertir_area_Texto_html

    SUBROUTINE guardar_archivo(nombre, contenido, append)
        CHARACTER(LEN=*), INTENT(IN) :: nombre, contenido
        LOGICAL, INTENT(IN) :: append

        IF (append) THEN
            OPEN(UNIT=10, FILE=nombre, STATUS='OLD', POSITION='APPEND')
        ELSE
            OPEN(UNIT=10, FILE=nombre, STATUS='REPLACE')
        END IF

        WRITE(10, '(A)') contenido
        CLOSE(10)
    END SUBROUTINE guardar_archivo

END MODULE traductor
