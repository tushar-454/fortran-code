!== Bisection method
    f(x)=exp(-x)-sin(x)
    print*,'enter initial values'
10  read*,a,b
    if(f(a)*f(b).ge.0.0)then
      print*,'try another values'
      goto 10
    endif
    i=1
30  c=(a+b)/2.0
    if(f(a)*f(c).lt.0.0)then
      b=c
    else
      a=c
    endif
    write(*,20)i,a,b
20  format(3x,'after trial no. ',i4,', Approximation is ',f11.7,' and ',f11.7)
    i=i+1
    if(mod(i,1000).eq.0)then
50    print*,'1. continue, 2. end'
      read*,j
      if(j.eq.2)goto 40
      if(j.ne.1)goto 50
    endif
    if(abs(f(b)-f(a)).gt.0.0000001)goto 30
40  write(*,41)a
41  format(3x,"solution is ",f11.7)
    end
