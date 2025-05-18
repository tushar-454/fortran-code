    f(x)=x**3+x**2-1
    df(x)=3*x**2+2*x
    tol=0.0001
    i=1
    x0=1.0
10  x1=x0-f(x0)/df(x0)
    write(*,11)i,x0,x1
11  format(3x,'On iteration no.',i2,', assumption changed from ',f8.4,' to ',f8.4)
    i=i+1
    if(abs(x0-x1).gt.tol)then
      x0=x1
      goto 10
    endif
    print*, 'root is ',x1
    end

    