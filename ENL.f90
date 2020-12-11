PROGRAM P
    !Modulo
    USE ENL_MET
    USE ENL_FUNC
    IMPLICIT NONE
    
    REAL(8) :: A, B, XINI, XRES, XRES2
    REAL(8) :: ERROR, TOL
    
    A = -2
    B = 2
    XINI = 0.85
    ERROR = 1D-5    
    PRINT *, 'Empezando resolución por Bisección'
    CALL ENL_BISECCION(A, B, ERROR, XRES)
    PRINT *, 'Método de Bisección terminado.'
    PRINT *, 'El valor de la raíz es: ', XRES
    
    
    PRINT *, 'Empezando resolución por Punto Fijo Sistemático'
    CALL ENL_PUNTOFIJO_SIST(A, B, TOL, XRES2, XINI)
    PRINT *, 'El valor de la raíz es: ', XRES2
    
    CALL GUARDAR_FUNCION(A-5, B+5)
    CALL GUARDAR_RAICES(XRES)
    CALL SYSTEM("gnuplot script_func.p")
CONTAINS
    
    !Guarda los valores de X y F(X) (funcion en ENL_FUNC), para graficarlos.
    !Supongo A < B
    SUBROUTINE GUARDAR_FUNCION(A, B)
        REAL(8), INTENT(IN) :: A, B
        !
        REAL(8) :: H, XACT
        H = 1D-3; XACT = A
        
        OPEN(1, FILE = 'Funcion.txt', ACTION = 'WRITE')
    
        DO WHILE(XACT < B)
            WRITE(1, '(2F15.8)') XACT, F(XACT)
            XACT = XACT + H
        END DO
        WRITE(1, '(2F15.8)') XACT, F(XACT)
        CLOSE(1)
    END SUBROUTINE
    
    !Se supone que guarde un vector de raíces, pero las estoy usando por separado.
    !Así que X es una simple raíz
    SUBROUTINE GUARDAR_RAICES(X)
        REAL(8), INTENT(IN) :: X
        !
        OPEN(1, FILE = 'Raices.txt', ACTION = 'WRITE')
        WRITE(1, '(2F15.8)') X, F(X)
        CLOSE(1)
    END SUBROUTINE
END PROGRAM
