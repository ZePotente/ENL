MODULE ENL_FUNC
    IMPLICIT NONE
CONTAINS
    FUNCTION F(X)
        REAL(8) :: F
        REAL(8), INTENT(IN) :: X
        
        F = X*X*X + 1
    END FUNCTION
    
    FUNCTION DF(X)
        REAL(8) :: DF
        REAL(8), INTENT(IN) :: X
        
        DF = 3*X*X
    END FUNCTION
END MODULE
