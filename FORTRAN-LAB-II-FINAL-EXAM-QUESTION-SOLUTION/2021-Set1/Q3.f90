PROGRAM matrix_test
  INTEGER :: i, j, k
  REAL :: A(3,3), AT(3,3), A2(3,3), C(3,3), D(3,3), Iden(3,3)
  LOGICAL :: isSym, isAntiSym, isOrtho

  ! Input matrix A (column-wise initialization)
  DATA A / 0.0, -5.0, 4.0, &
           5.0,  0.0, -3.0, &
          -4.0,  3.0, 0.0 /

  ! Transpose A -> AT
  DO i = 1, 3
    DO j = 1, 3
      AT(i,j) = A(j,i)
    END DO
  END DO

  ! Check symmetric and anti-symmetric
  isSym = .TRUE.
  isAntiSym = .TRUE.
  DO i = 1, 3
    DO j = 1, 3
      IF (A(i,j) .NE. AT(i,j)) isSym = .FALSE.
      IF (A(i,j) .NE. -AT(i,j)) isAntiSym = .FALSE.
    END DO
  END DO

  ! Calculate A^2 = A2
  DO i = 1, 3
    DO j = 1, 3
      A2(i,j) = 0.0
      DO k = 1, 3
        A2(i,j) = A2(i,j) + A(i,k) * A(k,j)
      END DO
    END DO
  END DO

  ! Calculate C = A2 + A
  DO i = 1, 3
    DO j = 1, 3
      C(i,j) = A2(i,j) + A(i,j)
    END DO
  END DO

  ! Calculate D = A2 * A
  DO i = 1, 3
    DO j = 1, 3
      D(i,j) = 0.0
      DO k = 1, 3
        D(i,j) = D(i,j) + A2(i,k) * A(k,j)
      END DO
    END DO
  END DO

  ! Check orthogonality: AT * A = Identity?
  isOrtho = .TRUE.
  DO i = 1, 3
    DO j = 1, 3
      Iden(i,j) = 0.0
      DO k = 1, 3
        Iden(i,j) = Iden(i,j) + AT(i,k) * A(k,j)
      END DO
      IF ( (i.EQ.j .AND. ABS(Iden(i,j) - 1.0) > 1E-4) .OR. &
           (i.NE.j .AND. ABS(Iden(i,j)) > 1E-4) ) THEN
        isOrtho = .FALSE.
      END IF
    END DO
  END DO

  ! Results
  PRINT *, 'Symmetric? ', isSym
  PRINT *, 'Anti-symmetric? ', isAntiSym
  PRINT *, 'Orthogonal? ', isOrtho

  PRINT *, 'Matrix C = A^2 + A:'
  DO i = 1, 3
    PRINT *, C(i,1), C(i,2), C(i,3)
  END DO

  PRINT *, 'Matrix D = A^2 * A:'
  DO i = 1, 3
    PRINT *, D(i,1), D(i,2), D(i,3)
  END DO

END PROGRAM matrix_test
