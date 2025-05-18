DIMENSION A(3,4)
A = reshape([1,1,1,3,4,3,8,3,4,4,-2,1],[3,4])
DO I=1,3
  DO J=1,3
    IF (I.gt.J)THEN
      
      print 10, ((A(ii,jj),jj=1,4),ii=1,3)
10	  format( 4f10.1)
	  P=A(I,J)/A(J,J)
	  print*,"Operation by", p
      DO M=1,4
    	A(I,M)=A(I,M)-P*A(J,M)
      END DO
      
    ENDIF
  END DO
END DO
z = A(3,4)/A(3,3)
y = (A(2,4)-A(2,3)*z)/A(2,2)
x = (a(1,4)-a(1,3)*z-a(1,2)*y)/a(1,1)
PRINT*,'X=',x
PRINT*,'Y=',y
PRINT*,'Z=',z
STOP
END