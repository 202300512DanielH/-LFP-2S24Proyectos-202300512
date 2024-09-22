#  Manual Técnico
El Proyecto está desarrollado en 2 lenguajes: 
- **Python:** Encargada de la interfaz gráfica.
- **Fortran:** Encargada del flujo interno de la aplicación.

## Apartado Gráfico
La aplicación usa **TKinter** para la estructura gráfica, lo que nos permite tener una interfaz agradable para el usuario y funcional.

### Funciones en Pyhton
```
def analizar_archivo():
    global imagen_grafico  # Aseguramos que la referencia a la imagen sea global
    
    if text_area.get("1.0", "end-1c").strip():
        try:
            input_content = text_area.get("1.0", "end-1c")
            resultado = subprocess.run(
                ["./main"],  # Asumiendo que este es tu programa que genera la salida
                input=input_content,
                capture_output=True,
                text=True
            )
            if resultado.stderr:
                print("Error en el programa Fortran:", resultado.stderr)
            else:
                salida = resultado.stdout
                print(salida)  # Para ver la salida completa

                # Extraer los valores de país, población, etc.
                lines = salida.splitlines()
                pais = lines[0].split(":")[-1].strip().replace('"', '')  # Extrae "Mexico"
                poblacion = lines[2].split(":")[-1].strip()  # Extrae "119713"

                # Actualizar los textos en el canvas
                canvas.itemconfig(text_pais, text=pais)  # Actualiza con el nombre del país
                canvas.itemconfig(text_poblacion, text=poblacion)  # Actualiza con la población

                # Verificar si el archivo grafico.png ha sido generado
                ruta_grafico = "grafico.png"
                if Path(ruta_grafico).is_file():
                    try:
                        # Usar PIL para cargar y redimensionar la imagen del gráfico
                        grafico_image = Image.open(ruta_grafico)  # Abrir la imagen del gráfico
                        grafico_resized = grafico_image.resize((300, 250))  # Redimensionar la imagen del gráfico
                        imagen_grafico = ImageTk.PhotoImage(grafico_resized)  # Convertir a formato PhotoImage

                        # Actualizar el canvas con la nueva imagen del gráfico
                        canvas.itemconfig(image_1, image=imagen_grafico)
                    except Exception as e:
                        print(f"Error al cargar la imagen grafico.png: {e}")
                        messagebox.showerror("Error", f"No se pudo cargar la imagen del gráfico: {e}")
                else:
                    print("El archivo grafico.png no fue encontrado.")
                    messagebox.showwarning("Advertencia", "El archivo grafico.png no fue generado.")

                # Condición para mostrar grafico.png si pais o poblacion son "N/A"
                if pais == "N/A" or poblacion == "N/A":
                    print("País o población es 'N/A', mostrando grafico.png en lugar de la bandera.")

                    # Usar grafico.png en lugar de la bandera si pais o poblacion es "N/A"
                    if Path(ruta_grafico).is_file():
                        try:
                            bandera_fallback_image = Image.open(ruta_grafico)  # Abrir grafico.png como reemplazo
                            bandera_fallback_resized = bandera_fallback_image.resize((150, 100))  # Redimensionar la imagen
                            imagen_grafico_bandera = ImageTk.PhotoImage(bandera_fallback_resized)  # Convertir a PhotoImage

                            # Actualizar el canvas con grafico.png en lugar de la bandera
                            canvas.itemconfig(image_2, image=imagen_grafico_bandera)
                            canvas.image = imagen_grafico_bandera  # Mantener referencia a la imagen
                        except Exception as e:
                            print(f"Error al cargar grafico.png como imagen de reemplazo: {e}")
                            messagebox.showerror("Error", f"No se pudo cargar grafico.png como imagen de reemplazo: {e}")
                    else:
                        print("El archivo grafico.png no fue encontrado como reemplazo.")
                        messagebox.showwarning("Advertencia", "No se encontró grafico.png como imagen de respaldo.")
                else:
                    # Verificar si la segunda imagen (bandera) fue actualizada y cargarla normalmente
                    ruta_bandera = lines[4].split(":")[-1].strip().replace('"', '')  # Extraer la ruta de la bandera
                    if Path(ruta_bandera).is_file():
                        try:
                            bandera_image = Image.open(ruta_bandera)  # Abrir la imagen de la bandera
                            bandera_resized = bandera_image.resize((150, 100))  # Redimensionar la imagen de la bandera
                            nueva_imagen_bandera = ImageTk.PhotoImage(bandera_resized)  # Convertir a formato PhotoImage

                            # Actualizar el canvas con la nueva imagen de la bandera
                            canvas.itemconfig(image_2, image=nueva_imagen_bandera)
                            canvas.image = nueva_imagen_bandera  # Mantener una referencia a la imagen
                        except Exception as e:
                            print(f"Error al cargar la imagen de la bandera: {e}")
                            messagebox.showerror("Error", f"No se pudo cargar la imagen de la bandera: {e}")
                    else:
                        print("El archivo de la bandera no fue encontrado.")
                        messagebox.showwarning("Advertencia", "El archivo de la bandera no fue generado.")

                # Mostrar alerta de que el análisis fue realizado correctamente
                messagebox.showinfo("Éxito", "Análisis realizado correctamente.")
        except Exception as e:
            print(f"Error al ejecutar el analizador: {e}")
            messagebox.showerror("Error", f"Ocurrió un error: {e}")
    else:
        print("Primero debes cargar un archivo .ORG para analizar.")
        messagebox.showwarning("Advertencia", "Primero debes cargar un archivo .ORG para analizar.")
```

Esta función es la más importante integrada en python, ya que su función es comunicar el INPUT de con Fortran Y recibir el OUTPUT de Fortran

# Fortran
Para el proyecto se aplicó el paradigma de POO trabajando cada parte del código en módulos con funciones propias, las cuales son ejecutadas por el main

```
! archivo: main.f90
PROGRAM Main
    USE ObjetoCaracter   ! Módulo que define el tipo de datos CaracterInfo
    USE Extraccion       ! Módulo que contiene la lógica de extracción de caracteres
    USE Automata         ! Módulo que contiene el autómata para procesar los caracteres
    USE OI               ! Módulo que contiene la lógica para obtener tokens válidos
    USE Reporte          ! Módulo que genera el reporte HTML
    USE Calculos         ! Módulo que inicia los cálculos con la lista de información
    USE Grafica         ! Módulo que genera la gráfica
    IMPLICIT NONE

    ! Llamar a la subrutina para generar la lista de caracteres
    CALL ExtraccionDeCaracteres()

    ! Llamar al autómata para procesar la lista de caracteres generada
    CALL procesarTokens()

    ! Generar el reporte HTML basado en los tokens reconocidos y no reconocidos
    CALL generarReporte()

    ! OI obtiene la lista de tokens válidos
    CALL procesarTokensOI()

    ! Calculos inicia los cálculos con la lista de información
    CALL iniciarCalculos()

    ! Grafica genera la gráfica con la información de los cálculos
    CALL generarGrafica()



END PROGRAM Main

```
## Modulo `ObjetoCaracter`
Definimos un objeto para almacenar correctamente la información
```
! archivo: ObjetoCaracter.f90
MODULE ObjetoCaracter
    IMPLICIT NONE
    PRIVATE

    TYPE :: CaracterInfo
        CHARACTER(LEN=100) :: caracter  ! Atributo para almacenar la palabra o signo
        INTEGER :: fila                 ! Fila donde se encuentra el caracter
        INTEGER :: columna              ! Columna donde se encuentra el caracter
    END TYPE CaracterInfo

    PUBLIC :: CaracterInfo
END MODULE ObjetoCaracter

```

## Módulo `Extraccion`
Este módulo maneja la extracción de caracteres desde una entrada de texto y los almacena en un arreglo dinámico del tipo `CaracterInfo`. A continuación se describen sus principales componentes:

1. Declaración Global
Se utiliza el módulo `ObjetoCaracter` para definir el tipo `CaracterInfo` y se declara un arreglo dinámico llamado `listaCaracteres` que almacenará la información de los caracteres extraídos.

2. Subrutina `ExtraccionDeCaracteres`
Esta subrutina lee líneas de texto desde la entrada estándar y las procesa carácter por carácter. Identifica y almacena palabras, símbolos, y cadenas entre comillas en el arreglo `listaCaracteres`, junto con su posición en la línea (fila y columna). Utiliza ciclos para recorrer las líneas y maneja comillas y espacios de forma especial.

3. Subrutina `almacenarToken`
Guarda los tokens (caracteres o palabras) en el arreglo dinámico, actualizando el número total de caracteres procesados. Si el arreglo aún no ha sido asignado, lo hace dinámicamente.

4. Subrutina `aumentarLista`
Gestiona la memoria dinámica de `listaCaracteres`, redimensionando el arreglo si es necesario para almacenar más tokens.

5. Subrutina `mostrarLista`
Imprime el contenido del arreglo `listaCaracteres`, mostrando cada token almacenado junto con su posición en el texto.

## Módulo `Automata`
Este módulo procesa una lista de caracteres y los clasifica en tokens reconocidos y no reconocidos. A continuación se detallan los componentes principales:

1. Estructura de datos Token
La estructura Token almacena información sobre los tokens que se reconocen durante el análisis. Cada Token tiene:
- valor: el valor del token (por ejemplo, una palabra o símbolo).
- tipo: el tipo de token (como reservada, cadena, signo, entero, porcentual o desconocido).
fila y columna: la posición donde se encuentra el token en el texto.
2. Listas Dinámicas
`reconocidos`: almacena tokens que han sido identificados como válidos.
`noReconocidos`: almacena tokens que no fueron clasificados como válidos (desconocidos).
3. Subrutina `procesarTokens`
Esta subrutina controla el proceso de análisis. Primero, inicializa las listas dinámicas y llama a la subrutina `ExtraccionDeCaracteres` para obtener la lista de caracteres. Luego, recorre cada carácter y lo pasa a la subrutina `reconocerToken` para clasificarlo.

4. Subrutina `reconocerToken`
Clasifica cada token de acuerdo con reglas predefinidas:

- Si el token coincide con ciertas palabras clave (como "grafica" o "pais"), se clasifica como palabra reservada.
- Si el token está entre comillas, se clasifica como una cadena.
- Símbolos específicos se clasifican como signos.
- Si es un número entero, se clasifica como entero.
- Si no coincide con ninguna de las reglas anteriores, se clasifica como desconocido.
- Dependiendo de su clasificación, el token se almacena en la lista correspondiente (`reconocidos` o `noReconocidos`).

5. Subrutina `agregarToken`
Esta subrutina agrega el token a la lista correcta, redimensionando dinámicamente la lista si es necesario.

A continuación se presenta el análisis hecho por el estudiante
![Parte 1](<Autómata P14.png>)
![Parte 2](<Autómata P15.png>)
![Parte 3](<Autómata P16.png>)

## Módulo `Reporte`
Importación del módulo `Automata`:
Se importa el módulo `Automata`, lo que permite acceder a las listas de tokens reconocidos y no reconocidos procesados anteriormente.

Subrutina `generarReporte`:
Esta subrutina genera un informe en formato HTML, dependiendo de si existen tokens no reconocidos. El reporte se almacena en un archivo y está diseñado para ser presentado visualmente en un navegador web con estilos CSS.


## Módulo `OI` 
Está diseñado para procesar una lista de tokens reconocidos (provenientes del módulo `Automata`), extraer información específica, y almacenarla en una lista dinámica de datos clave-valor del tipo Informacion. Aquí te detallo el funcionamiento de este módulo:

1. Estructura `Informacion`:
Define un tipo de datos `Informacion`, que tiene dos campos:

- dato: el nombre de la información (por ejemplo, "nombre de gráfica", "población", etc.).
- valordato: el valor asociado a ese dato (por ejemplo, "Gráfico de Población", "América", etc.).
Se utiliza una lista dinámica para almacenar múltiples entradas de este tipo.

2. Subrutina `procesarTokensOI`:
Esta subrutina recorre la lista de tokens reconocidos, buscando palabras clave específicas para extraer información relevante. Algunos ejemplos de datos que busca son:

grafica para extraer el nombre de una gráfica.
continente para obtener el nombre del continente.
pais para identificar el nombre de un país.
poblacion, saturacion, y bandera para obtener valores relacionados.
La subrutina se encarga de:

- Buscar tokens reservados como grafica, continente, o poblacion.
- Una vez encontrado un token de interés, extraer el siguiente token asociado como valor.
- Llamar a la subrutina almacenarInformacion para guardar este valor.
3. Subrutina `almacenarInformacion`:
Esta subrutina redimensiona de manera dinámica la lista `listaInformacion` para almacenar nuevos datos. Si la lista no está inicializada, se asigna. Si ya está asignada, se redimensiona usando una variable temporal para no perder los datos existentes.

Cada vez que se encuentra un nuevo dato relevante en la subrutina `procesarTokensOI`, se almacena el nombre del dato y su valor asociado en la lista.

## Módulo `Calculos`
Tiene como objetivo procesar la lista de información extraída de los tokens reconocidos (proveniente del módulo `OI`) para calcular estadísticas sobre continentes y países. A continuación te detallo el funcionamiento de este módulo:

1. Tipos `Pais` y `Continente`
`Pais`: Define un país con sus atributos principales: nombre, población, saturación y bandera.
`Continente`: Define un continente con su nombre, la saturación promedio de sus países y una lista dinámica de países.
2. Subrutina `iniciarCalculos`
Esta subrutina es el núcleo del módulo, ya que:

- Verificación inicial: Si existen tokens no reconocidos o la lista de información no está asignada, muestra un error y termina la ejecución.
Extracción de datos:
- Busca el nombre de la gráfica en la lista de información.
- Agrupa la información de continentes y países: nombre del continente, países, población, saturación y bandera.
- Redimensiona dinámicamente la lista de continentes y países utilizando las subrutinas REALLOCATE_CONTINENTES y REALLOCATE_PAISES.
- Cálculo de saturación promedio: Para cada continente, calcula la saturación promedio de todos sus países.
- Identificación del país con menor saturación: Recorre todos los países en los continentes y selecciona el país con la saturación más baja, mostrando su nombre, continente, población y bandera.
3. Subrutinas `REALLOCATE_CONTINENTES` y `REALLOCATE_PAISES`
Estas subrutinas se encargan de redimensionar las listas dinámicas de continentes y países:

- `REALLOCATE_CONTINENTES`: Redimensiona la lista de continentes cuando se agrega un nuevo continente.
- `REALLOCATE_PAISES`: Redimensiona la lista de países cuando se agrega un nuevo país dentro de un continente.

## Módulo `Grafica`
Tiene como propósito generar un gráfico visual que representa la información de continentes y países con base en su saturación. Este gráfico es creado en formato dot (usado por Graphviz) y luego exportado como una imagen PNG. A continuación se explica su funcionamiento:

1. Verificación de Tokens no Reconocidos
Antes de generar el gráfico, el programa verifica si existen tokens no reconocidos. Si los hay, se genera un archivo .dot vacío y un archivo PNG en blanco, lo que indica que no se puede crear el gráfico por falta de datos válidos.

2. Generación de Archivo DOT
Si no hay errores, el archivo .dot es generado, donde se especifica la estructura del gráfico.
La subrutina comienza verificando que el nombre de la gráfica y la lista de continentes estén correctamente asignados.
3. Estructura del Gráfico
El gráfico se estructura jerárquicamente de la siguiente manera:

El nodo raíz es el nombre de la gráfica (por ejemplo, "Gráfico de Saturación").
Cada continente se conecta con el nodo raíz. A su vez, los países dentro de ese continente se conectan con el nodo del continente.
4. Coloración Basada en Saturación
Para hacer el gráfico más informativo, cada nodo (continente o país) tiene un color que refleja su saturación:

Blanco para saturaciones ≤ 15.0.
Azul para saturaciones ≤ 30.0.
Verde para saturaciones ≤ 45.0.
Amarillo para saturaciones ≤ 60.0.
Naranja para saturaciones ≤ 75.0.
Rojo para saturaciones mayores a 75.0.
El color de cada nodo se determina utilizando la función getColor.

5. Escritura en Archivo DOT
El archivo .dot comienza con un encabezado de gráfico. Luego:

Se escribe un nodo raíz que representa el nombre de la gráfica.
Se recorren los continentes y países, generando nodos para cada uno y creando las conexiones entre ellos.
Cada nodo tiene una etiqueta con el nombre del continente/país y su saturación.
6. Generación de la Imagen PNG
Una vez que se ha escrito el archivo .dot, se utiliza el comando dot de Graphviz para generar una imagen PNG a partir de dicho archivo.

7. Subrutinas Auxiliares
`getColor`(saturation): Esta función determina el color que se asignará a un nodo en función de la saturación.
`eliminarComillasDobles`(cadena): Elimina comillas dobles de las cadenas, asegurando que el formato del archivo .dot sea correcto.
