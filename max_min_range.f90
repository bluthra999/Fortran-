program max_min_range
    implicit none
    integer::i ,array_size
    REAL, DIMENSION(100) :: x
    real :: max,min,range
    write(*, fmt='(a)',advance="no") "Enter number of data points: "
    read*,array_size   
    do i=1,array_size
        write(*, fmt='(a)',advance="no") "Enter the data : "
        read*,x(i)
    end do
    max=x(1)
    min=x(1)
    do i=2,array_size
        if (x(i) < min) then
            min = x(i)
        endif
        if (x(i) > max) then
            max = x(i)
        endif
    end do
    range = max - min
    print*,'max=',max,'min=',min,'range=',range
end program max_min_range