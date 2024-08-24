sum = 0.0
do 7 k = 1,1000,1
7 sum = sum + 1.0/(k**2)
continue
print*, 'sum=',sum
stop
end
