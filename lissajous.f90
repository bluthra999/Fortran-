program lissajous
    implicit none
    real,parameter :: pi=22.0/7.0
    real :: A,B,w1,w2,t0,tf,dt,theta,t,x,y
    write(*,fmt="(a)",advance="no")"Enter amplitudes of the wave A*sin(w1*t+theta): "
    read*,A
    write(*,fmt="(a)",advance="no")"Enter amplitudes of the wave A*sin(w2*t): "
    read*,B
    write(*,fmt="(a)",advance="no")"Enter the frequency of A*sin(w1*t+theta): "
    read*,w1
    write(*,fmt="(a)",advance="no")"Enter the frequency of A*sin(w2*t): "
    read*,w2
    write(*,fmt="(a)",advance="no")"Enter the starting time: "
    read*, t0
    write(*,fmt="(a)",advance="no")"Enter the  final time:  "
    read*, tf
    write(*,fmt="(a)",advance="no")"Enter the step size: "
    read*, dt
    write(*,fmt="(a)",advance="no")"Enter the phase difference in degrees: "
    read*,theta
    theta=(pi/180)*theta
    t=t0
    open(unit=10,file="Lissa.dat")
    do while(t<tf)
        x= A*sin(w1*t+theta)
        y= B*sin(w2*t)
        write(10,*)t,x,y
        t= t+dt
    end do
    close(10)
    end program lissajous