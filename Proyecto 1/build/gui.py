import subprocess
from pathlib import Path
from tkinter import Toplevel, Label, Scrollbar, Text, messagebox, PhotoImage
from PIL import Image, ImageTk

# Explicit imports to satisfy Flake8
from tkinter import Tk, Canvas, Button, PhotoImage, Scrollbar, Text, filedialog

OUTPUT_PATH = Path(__file__).parent
ASSETS_PATH = OUTPUT_PATH / Path(r"C:\Users\danie.000\Documents\LFP\Proyectos\Proyecto 1\build\assets\frame0")


def relative_to_assets(path: str) -> Path:
    return ASSETS_PATH / Path(path)

window = Tk()
window.geometry("942x561")
window.configure(bg="#1A936F")

canvas = Canvas(
    window,
    bg="#1A936F",
    height=561,
    width=942,
    bd=0,
    highlightthickness=0,
    relief="ridge"
)

canvas.place(x=0, y=0)
canvas.create_rectangle(
    572.0,
    364.0,
    726.0,
    512.0,
    fill="#76C7C0",
    outline="")

# Eliminar el uso de Entry y Entry BG, y utilizar el widget Text en su lugar
text_area = Text(
    window,  # Cambiamos canvas por window para anclar el Text a la ventana principal
    bd=0,
    bg="#FBFFFE",
    fg="#000716",
    wrap="none",  # Desactiva el ajuste automático de texto para permitir barras de desplazamiento
    font=("Inter", 12),  # Ajusta la fuente a una que se vea bien con el tema de la interfaz
    highlightthickness=0
)
text_area.place(
    x=19.0,  # Mantener la misma posición en X
    y=23.0,  # Mantener la misma posición en Y
    width=375.0,  # Ajuste de ancho
    height=500.0  # Ajuste de alto
)

# Crear las barras de desplazamiento (scrollbars)
scrollbar_vertical = Scrollbar(window, command=text_area.yview)
scrollbar_vertical.place(x=395, y=23, height=500)  # Ajusta la posición a la derecha del área de texto

scrollbar_horizontal = Scrollbar(window, command=text_area.xview, orient='horizontal')
scrollbar_horizontal.place(x=19, y=525, width=375)  # Ajusta la posición debajo del área de texto

# Configurar el área de texto para que funcione con las barras de desplazamiento
text_area.configure(yscrollcommand=scrollbar_vertical.set, xscrollcommand=scrollbar_horizontal.set)

# Variable para almacenar la ruta del archivo cargado o guardado
ruta_archivo_guardado = None

def cargar_archivo_org():
    global ruta_archivo_guardado

    # Abrir el explorador de archivos para seleccionar un archivo .org
    archivo_ruta = filedialog.askopenfilename(
        filetypes=[("Archivos ORG", "*.org")],  # Filtro para archivos .org
        title="Seleccionar Archivo .ORG"
    )
    
    if archivo_ruta:  # Si se selecciona un archivo
        ruta_archivo_guardado = archivo_ruta  # Actualizar la variable global con la ruta del archivo seleccionado
        with open(archivo_ruta, 'r', encoding='utf-8') as archivo:  # Abrir el archivo en modo lectura
            contenido = archivo.read()  # Leer el contenido del archivo
            
        # Limpiar el área de texto antes de mostrar el contenido nuevo
        text_area.delete(1.0, "end")  
        text_area.insert("end", contenido)  # Mostrar el contenido en el área de texto
        #print(f"Archivo cargado: {ruta_archivo_guardado}")  # Información para depuración

# Actualizar el botón "Cargar Archivo .ORG" para que llame a la función cargar_archivo_org
button_image_1 = PhotoImage(
    file=relative_to_assets("button_1.png"))
button_1 = Button(
    image=button_image_1,
    text="Cargar Archivo .ORG",
    compound="center",
    bg="#1A936F",
    borderwidth=0,
    highlightthickness=0,
    command=cargar_archivo_org,  # Llama a la nueva función cargar_archivo_org
    relief="flat"
)
button_1.place(
    x=418.0,
    y=124.0,
    width=160.0,
    height=32.0
)


# Variable para almacenar la ruta del archivo guardado
ruta_archivo_guardado = None

def guardar_archivo():
    global ruta_archivo_guardado

    # Si no se ha guardado antes (primera vez)
    if not ruta_archivo_guardado:
        ruta_archivo_guardado = filedialog.asksaveasfilename(
            defaultextension=".org",  # Extensión predeterminada del archivo
            filetypes=[("Archivos ORG", "*.org")],  # Filtro para archivos .org
            title="Guardar Archivo"
        )

    # Si el usuario seleccionó una ruta de guardado
    if ruta_archivo_guardado:
        # Obtener el contenido del área de texto
        contenido = text_area.get("1.0", "end-1c")  # Obtiene todo el texto desde el inicio hasta el final sin el carácter de nueva línea final
        # Guardar el contenido en el archivo
        with open(ruta_archivo_guardado, 'w', encoding='utf-8') as archivo:
            archivo.write(contenido)
        print(f"Archivo guardado en {ruta_archivo_guardado}")



button_image_2 = PhotoImage(
    file=relative_to_assets("button_2.png"))
button_2 = Button(
    image=button_image_2,
    text="Guardar",
    compound="center",
    bg="#1A936F",
    borderwidth=0,
    highlightthickness=0,
    command=guardar_archivo,  # Llama a la nueva función guardar_archivo
    relief="flat"
)
button_2.place(
    x=418.0,
    y=175.0,
    width=160.0,
    height=32.0
)


def guardar_como_archivo():
    # Abrir el explorador de archivos para seleccionar el nombre y ubicación del archivo
    ruta_archivo = filedialog.asksaveasfilename(
        defaultextension=".org",  # Extensión predeterminada del archivo
        filetypes=[("Archivos ORG", "*.org")],  # Filtro para archivos .org
        title="Guardar Como"
    )

    # Si el usuario seleccionó una ruta de guardado
    if ruta_archivo:
        # Obtener el contenido del área de texto
        contenido = text_area.get("1.0", "end-1c")  # Obtiene todo el texto desde el inicio hasta el final sin el carácter de nueva línea final
        # Guardar el contenido en el archivo
        with open(ruta_archivo, 'w', encoding='utf-8') as archivo:
            archivo.write(contenido)
        print(f"Archivo guardado como {ruta_archivo}")

# Actualizar el botón "Guardar Como" para que llame a la función guardar_como_archivo
button_image_3 = PhotoImage(
    file=relative_to_assets("button_3.png"))
button_3 = Button(
    image=button_image_3,
    text="Guardar Como",
    compound="center",
    bg="#1A936F",
    borderwidth=0,
    highlightthickness=0,
    command=guardar_como_archivo,  # Llama a la nueva función guardar_como_archivo
    relief="flat"
)
button_3.place(
    x=418.0,
    y=226.0,
    width=160.0,
    height=32.0
)



from tkinter import PhotoImage
from PIL import Image, ImageTk  # Importar PIL para redimensionar la imagen

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





# Actualizar el botón "Analizar" para que llame a la función analizar_archivo
button_image_4 = PhotoImage(
    file=relative_to_assets("button_4.png"))
button_4 = Button(
    image=button_image_4,
    text="Analizar",
    compound="center",
    bg="#1A936F",
    borderwidth=0,
    highlightthickness=0,
    command=analizar_archivo,  # Llama a la nueva función analizar_archivo
    relief="flat"
)
button_4.place(
    x=418.0,
    y=277.0,
    width=160.0,
    height=32.0
)



def mostrar_acerca_de():
    # Crear una nueva ventana
    ventana_acerca_de = Toplevel(window)
    ventana_acerca_de.title("Acerca de")
    ventana_acerca_de.geometry("400x300")  # Ajustar el tamaño de la ventana para incluir la imagen
    ventana_acerca_de.configure(bg="#1A936F")  # Mismo color de fondo que la ventana principal

    # Cargar la imagen
    imagen = Image.open("AcercaDe.png")
    imagen = imagen.resize((100, 100), Image.LANCZOS)  # Redimensionar la imagen si es necesario
    imagen_tk = ImageTk.PhotoImage(imagen)

    # Crear el contenido de la ventana "Acerca de"
    Label(ventana_acerca_de, image=imagen_tk, bg="#1A936F").pack(pady=10)
    Label(ventana_acerca_de, text="Estudiante: Daniel Andreé Hernandez Flores", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)
    Label(ventana_acerca_de, text="Carné: 202300512", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)
    Label(ventana_acerca_de, text="Curso: Lenguajes Formales y de Programación", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)
    Label(ventana_acerca_de, text="Sección: B", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)

    # Desactivar la capacidad de cambiar el tamaño de la ventana "Acerca de"
    ventana_acerca_de.resizable(False, False)

    # Mantener una referencia a la imagen para evitar que sea recolectada por el garbage collector
    ventana_acerca_de.imagen_tk = imagen_tk

button_image_5 = PhotoImage(
    file=relative_to_assets("button_5.png"))
button_5 = Button(
    image=button_image_5,
    text="Acerca de",
    compound="center",
    bg="#1A936F",
    borderwidth=0,
    highlightthickness=0,
    command=mostrar_acerca_de,  # Llama a la nueva función mostrar_acerca_de
    relief="flat"
)
button_5.place(
    x=598.0,
    y=14.0,
    width=160.0,
    height=32.0
)

canvas.create_text(
    598.0,
    388.0,
    anchor="nw",
    text="País Seleccionado:",
    fill="#000000",
    font=("Inter", 12 * -1)
)

canvas.create_text(
    622.0,
    445.0,
    anchor="nw",
    text="Población:",
    fill="#000000",
    font=("Inter", 12 * -1)
)

# Guardar los elementos de texto para poder modificarlos después
text_pais = canvas.create_text(
    638.0,
    417.0,
    anchor="nw",
    text="",  # Texto temporal que luego será sustituido
    fill="#000000",
    font=("Inter", 12 * -1)
)

text_poblacion = canvas.create_text(
    639.0,
    474.0,
    anchor="nw",
    text="",  # Texto temporal que luego será sustituido
    fill="#000000",
    font=("Inter", 12 * -1)
)


button_image_6 = PhotoImage(
    file=relative_to_assets("button_6.png"))
button_6 = Button(
    image=button_image_6,
    text="Salir",  # Text added here
    compound="center",
    bg="#1A936F",  # Set the same background color as the window
    borderwidth=0,
    highlightthickness=0,
    command=window.destroy,  # Updated command to close the window
    relief="flat"
)
button_6.place(
    x=766.0,
    y=14.0,
    width=160.0,
    height=32.0
)

# Cargar y redimensionar la primera imagen (image_1.png)
image_1_path = relative_to_assets("image_2.png")  # Ruta de la imagen
image_1_original = Image.open(image_1_path)  # Abrir la imagen con PIL
image_1_resized = image_1_original.resize((325, 250))  # Redimensionar (ajusta el tamaño a lo que necesites)
image_image_1 = ImageTk.PhotoImage(image_1_resized)  # Convertir a PhotoImage para Tkinter

image_1 = canvas.create_image(
    767.0,
    206.0,
    image=image_image_1
)

# Cargar y redimensionar la segunda imagen (image_2.png)
image_2_path = relative_to_assets("image_1.png")  # Ruta de la imagen
image_2_original = Image.open(image_2_path)  # Abrir la imagen con PIL
image_2_resized = image_2_original.resize((150, 100))  # Redimensionar (ajusta el tamaño a lo que necesites)
image_image_2 = ImageTk.PhotoImage(image_2_resized)  # Convertir a PhotoImage para Tkinter

image_2 = canvas.create_image(
    827.0,
    447.0,
    image=image_image_2
)


window.resizable(False, False)
window.mainloop()

