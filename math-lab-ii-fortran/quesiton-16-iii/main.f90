! A class of 100 students takes an examination on which scores range from 0 to 100.Write a Fortran program which finds
! iii)The number of students who score below 40

PROGRAM GET_40_BELOW_STUDENT_NUMBER

    IMPLICIT NONE
    INTEGER :: STUDENTS, I, SCORE, STUDENT

    PRINT *, "Enter the Quentity of STUDENTS:"
    READ *, STUDENTS

    STUDENT = 0

    DO I = 1, STUDENTS
        PRINT*, "Enter the score for STUDENT", I, ":"
        READ*, SCORE
        IF(SCORE.LT.0.0 .OR. SCORE.GT.100.0) THEN
            PRINT*, "Score must be greater then 0 and Less then 100. Try Again please."
            STOP
        END IF
        IF(SCORE.LT.40) THEN
            STUDENT = STUDENT + 1
        END IF
    END DO

    PRINT*, "The number of students who scored below 40 is:", STUDENT

END PROGRAM GET_40_BELOW_STUDENT_NUMBER
