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
