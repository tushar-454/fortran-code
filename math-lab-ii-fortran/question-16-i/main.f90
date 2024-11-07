! A class of 100 students takes an examination on which scores range from 0 to 100.Write a Fortran program which finds
! i) The average score.


PROGRAM AVERAGE_SCORE

    IMPLICIT NONE
    INTEGER :: STUDENT, I
    REAL :: SCORE, AVERAGE, SUM

    PRINT *, "Enter the Quentity of STUDENT:"
    READ *, STUDENT

    SUM = 0.0
    AVERAGE = 0.0

    DO I = 1, STUDENT
        PRINT*, "Enter the score for STUDENT", I, ":"
        READ*, SCORE
        IF(SCORE.LT.0.0 .OR. SCORE.GT.100.0) THEN
            PRINT*, "Score must be greater then 0 and Less then 100. Try Again please."
            STOP
        END IF
        SUM = SUM + SCORE
    END DO

    AVERAGE = SUM / STUDENT

    PRINT*, "The average score is:", AVERAGE

END PROGRAM AVERAGE_SCORE
