program triangle_problem
    integer :: A, B, C

    print*, "Enter the value of A, B, C"
10  read*, A, B, C

    if(A.LE.0 .OR. B.LE.0 .OR. C.LE.0) then
        print*, "Enter A, B, C value Greater then 0"
        goto 10
    end if

    !check possibility of form a triangle with data
    if((A+B).GT.C .AND. (B+C).GT.A .AND. (C+A).GT.B) then
        print*, "Possible to form a triangle"
    else
        print*, "Not Possible to form a triangle"
    end if

    !check the type of triangle
    if(A == B .AND. B == C) then
        print*, "Triangle is equilateral"
    else if (A == B .OR. B == C .OR. C == A) then
        print*, "Triangle is isosceles"
    else if ((A**2 + B**2 == C**2) .OR. (A**2 + C**2 == B**2) .OR. (B**2 + C**2 == A**2)) then
        print*, "Triangle is right angled"
    else
        print*, "Not a standard type considered here"
    end if

end program triangle_problem
