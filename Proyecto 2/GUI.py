from tkinter import Tk, Canvas, Button, Text, Scrollbar, RIGHT, Y, X, HORIZONTAL, VERTICAL, Frame, ttk, filedialog, messagebox
import os
import subprocess

window = Tk()

# Configurar ventana principal
window.geometry("1500x700")
window.configure(bg="#212121")

archivo_actual = None

# Crear el Canvas
canvas = Canvas(
    window,
    bg="#212121",
    height=700,
    width=1500,
    bd=0,
    highlightthickness=0,
    relief="ridge"
)

canvas.place(x=0, y=0)

# Función para abrir un archivo .LFP y cargarlo en el área de texto
def abrir_archivo():
    global archivo_actual
    archivo = filedialog.askopenfilename(defaultextension=".LFP", filetypes=[("Archivos LFP", "*.LFP")])
    
    if archivo:
        archivo_actual = archivo
        with open(archivo_actual, "r", encoding="utf-8") as file:
            contenido = file.read()
        text_area.delete(1.0, "end")  # Limpiar área de texto
        text_area.insert("end", contenido)  # Insertar el contenido del archivo en el área de texto
        window.title(f"Editor - {os.path.basename(archivo_actual)}")

# Función para crear un nuevo archivo
def nuevo_archivo():
    global archivo_actual
    
    if text_area.get(1.0, "end-1c"):  # Verifica si hay texto en el editor
        respuesta = messagebox.askyesnocancel("Guardar", "¿Deseas guardar los cambios antes de crear un nuevo archivo?")
        if respuesta:  # Si el usuario desea guardar
            guardar_archivo()
        elif respuesta is None:  # Si el usuario cancela
            return

    archivo_actual = None
    text_area.delete(1.0, "end")  # Limpiar el área de texto
    window.title("Editor - Nuevo Archivo")

# Función para guardar el archivo actual
def guardar_archivo():
    global archivo_actual

    if archivo_actual:  # Si hay un archivo abierto
        with open(archivo_actual, "w", encoding="utf-8") as file:
            file.write(text_area.get(1.0, "end-1c"))  # Guardar el contenido del área de texto
        messagebox.showinfo("Guardar", f"Archivo guardado: {archivo_actual}")
    else:
        guardar_como()

# Función para guardar el archivo con un nuevo nombre
def guardar_como():
    global archivo_actual
    archivo = filedialog.asksaveasfilename(defaultextension=".LFP", filetypes=[("Archivos LFP", "*.LFP")])

    if archivo:
        archivo_actual = archivo
        with open(archivo_actual, "w", encoding="utf-8") as file:
            file.write(text_area.get(1.0, "end-1c"))  # Guardar el contenido del área de texto
        window.title(f"Editor - {os.path.basename(archivo_actual)}")
        messagebox.showinfo("Guardar Como", f"Archivo guardado como: {archivo_actual}")

# Variable global para el menú desplegable
menu_frame = None

# Variables para las posiciones
pos_y_label = None
pos_x_label = None

# Función para mostrar/ocultar el menú de opciones al presionar "Archivo"
def toggle_menu_archivo():
    global menu_frame

    # Si el menú ya está abierto, lo destruimos (cerramos)
    if menu_frame is not None:
        menu_frame.destroy()
        menu_frame = None
    else:
        # Crear un frame justo debajo del botón "Archivo"
        menu_frame = Frame(window, bg="#FF5370")
        menu_frame.place(x=0, y=49)

        # Ancho del botón "Archivo" para que los otros coincidan
        button_width = 20

        # Crear los botones del menú
        btn_nuevo = Button(menu_frame, text="Nuevo", width=button_width, bg="#FF5370", fg="white",
                           activebackground="#E04C65", activeforeground="white", relief="flat", command=nuevo_archivo)
        btn_nuevo.pack(fill="x")

        btn_abrir = Button(menu_frame, text="Abrir", width=button_width, bg="#FF5370", fg="white",
                           activebackground="#E04C65", activeforeground="white", relief="flat", command=abrir_archivo)
        btn_abrir.pack(fill="x")

        btn_guardar = Button(menu_frame, text="Guardar", width=button_width, bg="#FF5370", fg="white",
                             activebackground="#E04C65", activeforeground="white", relief="flat", command=guardar_archivo)
        btn_guardar.pack(fill="x")

        btn_guardar_como = Button(menu_frame, text="Guardar Como", width=button_width, bg="#FF5370", fg="white",
                                  activebackground="#E04C65", activeforeground="white", relief="flat", command=guardar_como)
        btn_guardar_como.pack(fill="x")

        btn_salir = Button(menu_frame, text="Salir", width=button_width, bg="#FF5370", fg="white",
                           activebackground="#E04C65", activeforeground="white", relief="flat", command=window.quit)
        btn_salir.pack(fill="x")

# Función para actualizar la posición del cursor
def actualizar_posicion(event=None):
    # Obtener la posición del cursor en términos de índice de tkinter (ejemplo: "1.5")
    pos = text_area.index('insert')

    # Separar en fila y columna
    fila, columna = pos.split(".")
    
    # Actualizar las etiquetas con la nueva posición
    canvas.itemconfig(pos_y_label, text=f"Fila {fila}")
    canvas.itemconfig(pos_x_label, text=f"Columna {columna}")

# Barra superior (ahora de borde a borde)
canvas.create_rectangle(
    0.0,
    0.0,
    1500.0,
    49.0,
    fill="#555252",
    outline=""
)

canvas.create_rectangle(
    1214.0,
    49.0,
    1500.0,
    500.0,  # Ajustamos el valor faltante
    fill="#323232",
    outline=""
)

# Frame para contener el text_area y los scrollbars
text_frame = Canvas(window)
text_frame.place(x=0.0, y=49.0, width=1214.0, height=450.0)

# Scrollbars
scrollbar_y = Scrollbar(text_frame, orient=VERTICAL)
scrollbar_y.pack(side=RIGHT, fill=Y)

scrollbar_x = Scrollbar(text_frame, orient=HORIZONTAL)
scrollbar_x.pack(side="bottom", fill=X)

# Text área con scrollbars
text_area = Text(
    text_frame,
    bg="#FFFFFF",
    fg="#000000",
    font=("Inter", 11),
    wrap="none",
    yscrollcommand=scrollbar_y.set,
    xscrollcommand=scrollbar_x.set,
    relief="flat"
)
text_area.pack(expand=True, fill='both')

scrollbar_y.config(command=text_area.yview)
scrollbar_x.config(command=text_area.xview)

# Vincular el evento de movimiento del cursor a la función de actualización de posición
text_area.bind("<KeyRelease>", actualizar_posicion)
text_area.bind("<ButtonRelease>", actualizar_posicion)

# Pie de página o parte inferior (ahora visible en la parte inferior)
canvas.create_rectangle(
    0.0,
    500.0,
    1500.0,
    700.0,
    fill="#00A9FF",
    outline=""
)

# Crear frame para la tabla con scrollbar vertical
table_frame = Canvas(window, bg="#00A9FF")
table_frame.place(x=0.0, y=510.0, width=1500.0, height=180.0)

# Scrollbar vertical para la tabla
scrollbar_table_y = Scrollbar(table_frame, orient=VERTICAL)
scrollbar_table_y.pack(side=RIGHT, fill=Y)

# Estilo personalizado para que la tabla tenga el color del rectángulo azul
style = ttk.Style()
style.configure("Custom.Treeview",
                background="#00A9FF",
                foreground="white",
                fieldbackground="#00A9FF",
                rowheight=25,
                font=("Inter", 11))

style.configure("Custom.Treeview.Heading",
                background="#008BD1",
                foreground="black",
                font=("Inter", 12, "bold"))

# Crear tabla en el área azul con el estilo personalizado
columns = ("#1", "#2", "#3", "#4", "#5")

table = ttk.Treeview(table_frame, columns=columns, show="headings",
                     height=5, style="Custom.Treeview", yscrollcommand=scrollbar_table_y.set)

# Definir los encabezados de las columnas
for col in columns:
    table.heading(col, text=f"Columna {col[-1]}")

# Ajustar el tamaño de cada columna
table.column("#1", anchor="center", width=100)
table.column("#2", anchor="center", width=150)
table.column("#3", anchor="center", width=150)
table.column("#4", anchor="center", width=150)
table.column("#5", anchor="center", width=150)

# Insertar filas de prueba (puedes eliminarlas o añadir tus datos)
for i in range(10):
    table.insert('', 'end', values=(f"Fila {i+1} C1", f"Fila {i+1} C2", f"Fila {i+1} C3", f"Fila {i+1} C4", f"Fila {i+1} C5"))

# Posicionar la tabla dentro del frame
table.pack(expand=True, fill="both")

# Configurar el scrollbar vertical para que funcione con la tabla
scrollbar_table_y.config(command=table.yview)

# Crear botones sin imágenes, solo texto (más pequeños y ajustados)
button_1 = Button(
    text="Archivo",
    borderwidth=0,
    highlightthickness=0,
    relief="flat",
    bg="#FF5370",  # Color de fondo del botón
    fg="white",  # Color del texto
    activebackground="#E04C65",  # Tono más oscuro al hacer clic
    activeforeground="white",  # Texto blanco al hacer clic
    font=("Inter Black", 11),
    command=toggle_menu_archivo  # Llama a la función que muestra/oculta el menú al hacer clic
)
button_1.place(
    x=0.0,
    y=0.0,
    width=153.0,
    height=49.0
)

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
            print(f"Salida de Fortran:\n{stdout}")

    except FileNotFoundError:
        messagebox.showerror("Error", "No se encontró el archivo main.exe. Asegúrate de haberlo compilado.")
    except Exception as e:
        messagebox.showerror("Error", f"Ocurrió un error inesperado: {e}")

button_2 = Button(
    text="Análisis",
    borderwidth=0,
    highlightthickness=0,
    relief="flat",
    bg="#00A9FF",
    fg="white",
    activebackground="#008BD1",  # Tono más oscuro al hacer clic
    activeforeground="white",
    font=("Inter Black", 11),
    command=ejecutar_analisis  # Llama a la función que ejecuta el análisis
)
button_2.place(
    x=153.0,
    y=0.0,
    width=153.0,
    height=49.0
)

button_3 = Button(
    text="Tokens",
    borderwidth=0,
    highlightthickness=0,
    relief="flat",
    bg="#00A9FF",
    fg="white",
    activebackground="#008BD1",  # Tono más oscuro al hacer clic
    activeforeground="white",
    font=("Inter Black", 11)
)
button_3.place(
    x=306.0,
    y=0.0,
    width=153.0,
    height=49.0
)

# Texto para "Posición Cursor"
canvas.create_text(
    1241.0,
    51.0,
    anchor="nw",
    text="Posición Cursor",
    fill="#FFFFFF",
    font=("Inter", 11)
)

# Texto para Posiciones X y Y (tamaño de letra reducido)
canvas.create_text(
    1241.0,
    81.0,
    anchor="nw",
    text="Posición Y: ",
    fill="#FFFFFF",
    font=("Inter", 11)
)

pos_y_label = canvas.create_text(
    1371.0,
    81.0,
    anchor="nw",
    text="Fila",
    fill="#FFFFFF",
    font=("Inter", 11)
)

canvas.create_text(
    1241.0,
    148.0,
    anchor="nw",
    text="Posición X: ",
    fill="#FFFFFF",
    font=("Inter", 11)
)

pos_x_label = canvas.create_text(
    1371.0,
    148.0,
    anchor="nw",
    text="Columna",
    fill="#FFFFFF",
    font=("Inter", 11)
)

window.resizable(False, False)
window.mainloop()
