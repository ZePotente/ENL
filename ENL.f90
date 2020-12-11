PROGRAM P
    !Modulo
    USE ENL_MET
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
CONTAINS
    
    
END PROGRAM
