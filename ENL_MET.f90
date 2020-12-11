MODULE ENL_MET
    USE ENL_FUNC
    
    IMPLICIT NONE
CONTAINS

    !No detecta raíces de grado par. (si no entiendo mal el código, devuelve b en ese caso)
    SUBROUTINE ENL_BISECCION(XIZQ, XDER, ERROR)
        REAL(8), INTENT(IN) :: XIZQ, XDER, ERROR
        !
        REAL(8) :: M, A, B
        INTEGER :: I, N
        A = XIZQ; B = XDER;
        !Se puede saber de antemano la cantidad de iteraciones:
        N = FLOOR((LOG(ABS(B-A)/ERROR))/LOG(2.0) + 0.5)
        
        I = 0
        DO I = 1, N
            M = (A + B)/2. !punto medio
            IF (F(A)*F(M) < 0) THEN !Si tienen signo diferente está en ese intervalo.
                B = M
            ELSE
                A = M
            END IF
        END DO
    END SUBROUTINE
    
    
    
END MODULE
