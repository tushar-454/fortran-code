            real x(0:15),y(0:15,0:15)
            print*,'Enter the number of intervals <15'
            read*,n
            print*,'Enter N+1 values of x'
            read*,(x(i),i=0,n)
            print*,'Enter N+1 values of y'
            read*,(y(0,i),i=0,n)
            print*,'Enter the values of x'
            read*,xx
            do 15 i=1,n
                do 25 j=0,n-1
                    y(i,j)=y(i-1,j+1)-y(i-1,j)
25              continue
15          continue
            u=(xx-x(0))/(x(1)-x(0))
            sum=y(0,0)
            P=1
            do 35 i=1,n
                p=p*(u-i+1)/real(i)
                sum=sum+p*y(i,0)
35          continue
            write(*,19)sum
19          format(/,40('='),//,1x,"Interpolated values of Y(X) = ",f12.5)
            stop
            end
