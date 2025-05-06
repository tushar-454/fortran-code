! Q1- (b) - START computes first nth term
program compute_terms
    integer :: i, n
    real :: term
    external :: general_term

    print*, "Enter the number of term:"
    read*, n

    do i = 1, n
        call general_term(i, term)
        print*, "Term,",i,"=", term
    end do
end program compute_terms
! Q1- (b) - STOP

! Q1- (a) - START subroutine of general term of the seies
subroutine general_term(n, term)
    integer :: n
    real :: term
    term = (n**2.0)/((n+1.0)*(n+2.0))*4.0**n
end subroutine
! Q1- (a) - STOP
