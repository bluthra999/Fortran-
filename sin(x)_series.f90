integer function factorial(n)
    implicit none
    integer :: i, n
  
    factorial = 1
  
    do i= 2,n,1
      factorial = factorial*i
    end do
  
    return
end function
  
program sin_series
    implicit none
    integer :: n, factorial,i
    real::x,sum,e,error,radians

    write(*, fmt='(a)',advance="no") "Enter number of terms: "
    read*,n
    write(*, fmt='(a)',advance="no") "Enter x (in degrees): "
    read*,x
    radians=x*3.14159/180
    sum=0
    write(*,fmt="(a,a12,a12,a18,a18)")'i','x','sum','sin(x)','error'
    do i=0,n
        sum = sum + (-1)**i * radians**(2*i+1) / factorial(2*i+1)
        e = sin(radians)
        error=abs(e-sum)        
        write(*,fmt="(i12,f12.4,f12.4,f18.4,f18.4)")i,x,sum,e,error       
    end do 
end program sin_series