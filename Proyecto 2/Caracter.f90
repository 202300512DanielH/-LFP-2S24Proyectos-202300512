! Archivo: Caracter.f90
MODULE CaracterModule
    IMPLICIT NONE
    PRIVATE  ! Los datos y procedimientos serán privados por defecto, salvo los que se hagan públicos
    TYPE, PUBLIC :: Caracter
        CHARACTER(LEN=100) :: valor    ! Representa el carácter
        CHARACTER(LEN=50) :: tipo    ! Tipo del carácter
        INTEGER :: posicionFila      ! Fila donde se encuentra el carácter
        INTEGER :: posicionColumna   ! Columna donde se encuentra el carácter
        CHARACTER(LEN=50) :: token   ! Token asociado al carácter
    END TYPE Caracter
END MODULE CaracterModule
