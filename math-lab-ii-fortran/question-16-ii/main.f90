! A class of 100 students takes an examination on which scores range from 0 to 100.Write a Fortran program which finds
! ii)The number of students who scored above 80

PROGRAM GET_80_PLUS_STUDENT_NUMBER

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
        IF(SCORE.GT.80) THEN
            STUDENT = STUDENT + 1
        END IF
    END DO

    PRINT*, "The number of students who scored above 80 is:", STUDENT

END PROGRAM GET_80_PLUS_STUDENT_NUMBER
