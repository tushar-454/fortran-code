PROGRAM STUDENT_EXAM_ANALYSIS
  IMPLICIT NONE  
  ! Variable declarations with descriptive names
  INTEGER, PARAMETER :: num_students = 20
  INTEGER, PARAMETER :: num_tests = 4
  REAL(8), DIMENSION(num_students, num_tests) :: scores
  REAL(8), DIMENSION(num_students) :: student_average
  REAL(8) :: total_sum, overall_average, student_sum
  INTEGER :: count_first_class, count_second_class, count_third_class, count_failed
  INTEGER :: i, j
  
  ! Initialize counters
  count_first_class = 0
  count_second_class = 0
  count_third_class = 0
  count_failed = 0
  total_sum = 0.0D0
  
  PRINT*, REPEAT('=', 80)
  PRINT*, '     STUDENT EXAM SCORE ANALYSIS SYSTEM'
  PRINT*, '     Computer Course - 20 Students, 4 Tests'
  PRINT*, REPEAT('=', 80)
  PRINT*
  
  ! Input scores for all students
  PRINT*, 'ENTER SCORES FOR 20 STUDENTS (4 TESTS EACH):'
  PRINT*, REPEAT('-', 80)
  
  DO i = 1, num_students
    PRINT '(A, I2, A)', 'Student ', i, ' - Enter 4 test scores:'
    DO j = 1, num_tests
      PRINT '(A, I1, A)', '  Test ', j, ': '
      READ*, scores(i, j)
    END DO
    PRINT*
  END DO
  
  ! Display all student scores
  PRINT*, REPEAT('=', 80)
  PRINT*, '                    STUDENT SCORES TABLE'
  PRINT*, REPEAT('=', 80)
  PRINT '(A10, 4A12, A15)', 'Student', 'Test 1', 'Test 2', 'Test 3', 'Test 4', 'Average'
  PRINT*, REPEAT('-', 80)
  
  ! Calculate average for each student
  DO i = 1, num_students
    student_sum = 0.0D0
    DO j = 1, num_tests
      student_sum = student_sum + scores(i, j)
    END DO
    student_average(i) = student_sum / REAL(num_tests, 8)
    total_sum = total_sum + student_average(i)
    
    ! Display student scores
    PRINT '(I7, 4F12.2, F15.2)', i, (scores(i, j), j = 1, num_tests), student_average(i)
  END DO
  
  PRINT*, REPEAT('=', 80)
  PRINT*
  
  ! Calculate overall average
  overall_average = total_sum / REAL(num_students, 8)
  
  ! Classify students based on their average scores
  DO i = 1, num_students
    IF (student_average(i) >= 60.0D0) THEN
      count_first_class = count_first_class + 1
    ELSE IF (student_average(i) >= 45.0D0 .AND. student_average(i) < 60.0D0) THEN
      count_second_class = count_second_class + 1
    ELSE IF (student_average(i) >= 40.0D0 .AND. student_average(i) < 45.0D0) THEN
      count_third_class = count_third_class + 1
    ELSE
      count_failed = count_failed + 1
    END IF
  END DO
  
  ! Display results
  PRINT*, REPEAT('=', 80)
  PRINT*, '                    ANALYSIS RESULTS'
  PRINT*, REPEAT('=', 80)
  PRINT*
  PRINT '(A, F8.2, A)', '(i)   OVERALL AVERAGE SCORE: ', overall_average, '%'
  PRINT*
  PRINT '(A, I3, A)', '(ii)  NUMBER OF FIRST CLASS STUDENTS (Score >= 60%): ', &
                       count_first_class, ' students'
  PRINT*
  PRINT '(A, I3, A)', '(iii) NUMBER OF SECOND CLASS STUDENTS (45% <= Score < 60%): ', &
                       count_second_class, ' students'
  PRINT*
  PRINT '(A, I3, A)', '(iv)  NUMBER OF THIRD CLASS STUDENTS (40% <= Score < 45%): ', &
                       count_third_class, ' students'
  PRINT*
  PRINT '(A, I3, A)', '(v)   NUMBER OF FAILED STUDENTS (Score < 40%): ', &
                       count_failed, ' students'
  PRINT*
  PRINT*, REPEAT('=', 80)
  
  ! Display detailed classification
  PRINT*
  PRINT*, REPEAT('=', 80)
  PRINT*, '                    DETAILED CLASSIFICATION'
  PRINT*, REPEAT('=', 80)
  
  IF (count_first_class > 0) THEN
    PRINT*
    PRINT*, 'FIRST CLASS STUDENTS (Score >= 60%):'
    PRINT*, REPEAT('-', 40)
    DO i = 1, num_students
      IF (student_average(i) >= 60.0D0) THEN
        PRINT '(A, I2, A, F8.2, A)', '  Student ', i, ' - Average: ', student_average(i), '%'
      END IF
    END DO
  END IF
  
  IF (count_second_class > 0) THEN
    PRINT*
    PRINT*, 'SECOND CLASS STUDENTS (45% <= Score < 60%):'
    PRINT*, REPEAT('-', 40)
    DO i = 1, num_students
      IF (student_average(i) >= 45.0D0 .AND. student_average(i) < 60.0D0) THEN
        PRINT '(A, I2, A, F8.2, A)', '  Student ', i, ' - Average: ', student_average(i), '%'
      END IF
    END DO
  END IF
  
  IF (count_third_class > 0) THEN
    PRINT*
    PRINT*, 'THIRD CLASS STUDENTS (40% <= Score < 45%):'
    PRINT*, REPEAT('-', 40)
    DO i = 1, num_students
      IF (student_average(i) >= 40.0D0 .AND. student_average(i) < 45.0D0) THEN
        PRINT '(A, I2, A, F8.2, A)', '  Student ', i, ' - Average: ', student_average(i), '%'
      END IF
    END DO
  END IF
  
  IF (count_failed > 0) THEN
    PRINT*
    PRINT*, 'FAILED STUDENTS (Score < 40%):'
    PRINT*, REPEAT('-', 40)
    DO i = 1, num_students
      IF (student_average(i) < 40.0D0) THEN
        PRINT '(A, I2, A, F8.2, A)', '  Student ', i, ' - Average: ', student_average(i), '%'
      END IF
    END DO
  END IF
  
  PRINT*
  PRINT*, REPEAT('=', 80)
  PRINT*, '                    END OF ANALYSIS'
  PRINT*, REPEAT('=', 80)
  
END PROGRAM STUDENT_EXAM_ANALYSIS
