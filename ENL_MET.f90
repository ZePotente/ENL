MODULE ENL_MET
    USE ENL_FUNC
    IMPLICIT NONE
CONTAINS

    !No detecta raíces de grado par. (si no entiendo mal el código, devuelve b en ese caso)
    !Supongo XIZQ < XDER
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
    
    !Supongo XIZQ < XDER
    SUBROUTINE ENL_PUNTOFIJO_SIST(XIZQ, XDER, TOL, XINI, CP)
        REAL(8), INTENT(IN) :: XIZQ, XDER, TOL
        REAL(8), INTENT(IN), OPTIONAL :: XINI
        INTEGER, INTENT(IN), OPTIONAL :: CP
        !
        REAL(8) :: X, A, B, ERROR, LAMBDA
        INTEGER :: ITER, MAXITER, CPUNTOS
        IF (PRESENT(XINI)) THEN
            X = XINI
        ELSE
            X = XIZQ !por poner algún valor por defecto
        END IF
        
        IF (PRESENT(CP)) THEN
            CPUNTOS = CP
        ELSE
            CPUNTOS = 20
        END IF
        A = XIZQ; B = XDER
        ERROR = 2*TOL
        LAMBDA = 1/MAXDF(A, B, CPUNTOS)
        
        ITER = 0; MAXITER = 100
        DO WHILE ((ERROR >= TOL) .AND. (ITER <= MAXITER))
            X = X - LAMBDA*F(X)
            ERROR = ABS(F(X)) !Se toma el error como la diferencia que hay en y hasta el 0.
            ITER = ITER + 1
        END DO
    END SUBROUTINE
    
    !N es la cantidad de puntos que quiero en el intervalo [a, b]
    !Supongo A < B
    FUNCTION MAXDF(A, B, N)
        REAL(8) :: MAXDF
        REAL(8), INTENT(IN) :: A, B
        INTEGER, INTENT(IN) :: N
        !
        REAL(8) :: H, X, DERIV
        INTEGER :: I
        H = ABS(B-A)/ (N-1)
        PRINT *, 'H = ', H
        
        X = A
        DO I = 1, N
            DERIV = DF(X)
            PRINT *, 'DF(', X, ') = ', DERIV
            PRINT *, 'MAXDF = ', MAXDF
            IF (ABS(DERIV) > ABS(MAXDF)) MAXDF = DERIV
            X = X + H
        END DO
    END FUNCTION
END MODULE
