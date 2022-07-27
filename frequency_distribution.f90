
program frequency_distribution
    implicit none
    integer::i ,array_size
    REAL, DIMENSION(100) :: x,y
    Real::sumY,sumXY,mean,standard_deviation,d,fd2
    write(*, fmt='(a)',advance="no") "Enter number of data points: "
    read*,array_size


    do i=1,array_size
        write(*, fmt='(a)',advance="no") "Enter x value: "
        read*,x(i)
        write(*, fmt='(a)',advance="no") "Enter frequency value: "
        read*,y(i)
    end do
    sumY=0
    sumXY=0
    do i=1,array_size
        sumY=sumY+y(i)
        sumXY=sumXY+x(i)*y(i)
    end do
    mean = sumXY/sumY
    print*,'mean=',mean
    d=0
    fd2=0

    do i=1,array_size
        d=(x(i)-mean)**2
        fd2=y(i)*d+fd2
    end do
    standard_deviation=sqrt(fd2/sumY)
    print*,'standard deviation=',standard_deviation
end program frequency_distribution

    

