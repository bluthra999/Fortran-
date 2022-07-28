program legendre_equation
    implicit none
    real :: k1, k2, k3, k4, l1, l2, l3, l4
    Common n
    real :: x0, y0, v0, x, h, n, f, g
    integer :: steps, i,x_correction
    print*,"Enter initial conditions x0,y0,v0 with spaces in between"
    read *, x0, y0, v0
    write(*,'("Enter the value of ending time : ")', advance = "no")
    read *, x
    write(*,'("Enter the value of n  : ")', advance = "no")
    read *, n
    write(*,'("Enter the value of step size : ")', advance = "no")
    read *, h
    steps = x / h
    open(19, file = "legendre.dat")
    if (n==1) then
        do x_correction = -1 , 1
            y0=x_correction
            write(19,*) x_correction, y0
        end do
    ELSE 
        do i = 1, steps
            
            k1=h*f(v0)
            l1=h*g(x0, y0, v0)

            k2=h*f(v0+l1/2)
            l2=h*g(x0 + h/2, y0 + k1/2, v0 + l1/2)

            k3=h*f(v0+l2/2)
            l3=h*g(x0 + h/2, y0 + k2/2, v0 + l2/2)

            k4=h*f(v0+l3)
            l4=h*g(x0 + h, y0 + k3, v0 + l3)


            y0 = y0 + (k1 + 2*k2 + 2*k3 + k4)/6
            v0 = v0 + (l1 + 2*l2 + 2*l3 + l4)/6

            write(19, '(F0.4, 2X, F0.4)') i*h, y0
    end do
end if 
    close(19)

end program legendre_equation

function f(u)
implicit none
real :: f, u
f= u
end function f

function g(x,y,u)
implicit none
real :: g, x, y, u, n
common n
g = (2*x*u - n*(n+1)*y)/(1-x*x)
end function g