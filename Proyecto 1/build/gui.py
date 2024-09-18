import subprocess

from pathlib import Path

from tkinter import Toplevel, Label, Scrollbar, Text

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
        print(f"Archivo cargado: {ruta_archivo_guardado}")  # Información para depuración

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



def analizar_archivo():
    print("Funcion para extraer llamada")  # Para depuración
    if text_area.get("1.0", "end-1c").strip():
        try:
            input_content = text_area.get("1.0", "end-1c")
            resultado = subprocess.run(
                ["./main"],
                input=input_content,
                capture_output=True,
                text=True
            )
            if resultado.stderr:
                print("Error en el programa Fortran:", resultado.stderr)
            else:
                print("Salida del programa Fortran:", resultado.stdout)  # Muestra la salida
        except Exception as e:
            print(f"Error al ejecutar el analizador: {e}")
    else:
        print("Primero debes cargar un archivo .ORG para analizar.")


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
    ventana_acerca_de.geometry("400x200")
    ventana_acerca_de.configure(bg="#1A936F")  # Mismo color de fondo que la ventana principal

    # Crear el contenido de la ventana "Acerca de"
    Label(ventana_acerca_de, text="Estudiante: Daniel Andreé Hernandez Flores", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)
    Label(ventana_acerca_de, text="Carné: 202300512", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)
    Label(ventana_acerca_de, text="Curso: Lenguajes Formales y de Programación", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)
    Label(ventana_acerca_de, text="Sección: B", bg="#1A936F", fg="#FBFFFE", font=("Inter", 12)).pack(pady=10)

    # Desactivar la capacidad de cambiar el tamaño de la ventana "Acerca de"
    ventana_acerca_de.resizable(False, False)

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

canvas.create_text(
    638.0,
    417.0,
    anchor="nw",
    text="Fill 1",
    fill="#000000",
    font=("Inter", 12 * -1)
)

canvas.create_text(
    639.0,
    474.0,
    anchor="nw",
    text="Fill 2",
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

image_image_1 = PhotoImage(
    file=relative_to_assets("image_1.png"))
image_1 = canvas.create_image(
    767.0,
    206.0,
    image=image_image_1
)

image_image_2 = PhotoImage(
    file=relative_to_assets("image_2.png"))
image_2 = canvas.create_image(
    827.0,
    447.0,
    image=image_image_2
)

window.resizable(False, False)
window.mainloop()

