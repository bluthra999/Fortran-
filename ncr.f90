integer function factorial(n)
    implicit none
    integer :: i, n
  
    factorial = 1
  
    do i= 2,n,1
      factorial = factorial*i
    end do
  
    return
end function


program ncr
    implicit none
    integer::a,b,c,d,e,factorial,f
    write(*, fmt='(a)',advance="no")" Enter the total number of students: " 
    read(*,*)a
    write(*, fmt='(a)',advance="no")"Number of students you want to select : " 
    read*,b
    c=factorial(a)
    d=factorial(a-b)
    e=factorial(b)
    f=c/(d*e)
    write(*,*)"The number of ways to select",a,"students out of ",b,"is",f
end program ncr