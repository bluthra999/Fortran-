program OddEven
    implicit none
    integer :: a,b,i,odd_series,even_series
        write(*, fmt='(a)',advance="no")"Press 1 for odd numbers and 2 even numbers : " 
        read*,i
        write(*, fmt='(a)',advance="no")"Enter the lower limit : " 
        read(*,*)a
        write(*, fmt='(a)',advance="no")"Enter the upper limit : " 
        read(*,*)b
        select case (i)
        Case(1)
            print*,'Finding odd numbers in the btw ',a,'and',b 
            if (mod(a, 2) == 0) then
              a = a + 1
            else 
                a = a + 2
            end if
            if (mod(b, 2) == 0) then
                b = b - 1
              else 
                  b = b - 2
              end if
            do odd_series= a,b,2
                print*,  odd_series 
            end do
        Case(2)
            print*,'Finding even numbers in the btw  ',a,'and',b
            if (mod(a, 2) /= 0) then
                a = a + 1
            else 
                a = a + 2
            end if
            if(mod(b,2)/=0) then
                b = b - 1
            else 
                b = b - 2
            end if

            do even_series= a,b,2
                print*,  even_series    
            end do
        case default
            print*, "Invalid input"
        end select
      end program OddEven