integer function factorial(n)
    implicit none
    integer :: i, n
  
    factorial = 1
  
    do i= 2,n,1
      factorial = factorial*i
    end do
  
    return
end function
  
program exp_series
    implicit none
    integer :: n, factorial,i
    real::x,sum,e,error

    write(*, fmt='(a)',advance="no") "Enter n: "
    read*,n
    write(*, fmt='(a)',advance="no") "Enter x: "
    read*,x
    write(*,fmt="(a12,a12,a12,a18,a18)")'i','x','sum','e(x)','error'
    sum=1.0
    do i=1,n
        sum=sum + x**(i)/factorial(i)
        e = exp(x)
        error=abs(e-sum)        
        write(*,fmt="(i12,f12.4,f12.4,f18.4,f18.4)")i,x,sum,e,error     
    end do 
end program exp_series