MODULE ENL_FUNC
    IMPLICIT NONE
CONTAINS
    FUNCTION F(X)
        REAL(8) :: F
        REAL(8), INTENT(IN) :: X
        
        F = X
    END FUNCTION
    
    FUNCTION DF(X)
        REAL(8) :: DF
        REAL(8), INTENT(IN) :: X
        
        DF = X
    END FUNCTION
END MODULE
