program linear_fit
    implicit none
    integer::i ,array_size
    REAL, DIMENSION(100) :: x,y,yc
    Real::sumX,sumY,sumXY,sumX2,slope,intercept
    write(*, fmt='(a)',advance="no") "Enter number of data points: "
    read*,array_size


    do i=1,array_size
        write(*, fmt='(a)',advance="no") "Enter x value: "
        read*,x(i)
        write(*, fmt='(a)',advance="no") "Enter y value: "
        read*,y(i)
    end do
    sumX = 0
    sumY = 0
    sumXY = 0
    sumX2 = 0

    do i=1,array_size
        sumX=sumX+x(i)
        sumY=sumY+y(i)
        sumXY=sumXY+x(i)*y(i)
        sumX2=sumX2+x(i)*x(i)
    end do
    slope = (array_size * sumXY - sumX * sumY) / (array_size * sumX2 - sumX * sumX)
    intercept = (sumY - slope * sumX) /array_size
    print*,'Eqn of Sl is ' , 'y=',slope,'*x+',intercept
    

    do i=1,array_size

        yc(i)=x(i)*slope+intercept
        
    end do


    write(*, fmt='(2X,a,7X,a,7X,a,6x,a)') "x","yo","yc","|yo-yc|"
    do i=1,array_size

        write(*, fmt='(F4.1,5X,F4.1,5X,F4.1,5x,F4.1)')x(i),y(i),yc(i),abs(yc(i)-y(i))

    
    end do


end program linear_fit







