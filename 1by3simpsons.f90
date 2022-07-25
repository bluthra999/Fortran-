! Function to integrate
real function f(x)
Real::x
f=1/(1+x)
return
end function f


program simpsons
implicit none
Real:: lower_limit,upper_limit,h,integration,f,k,ans
integer::i,num_intervals

write(*, fmt='(a)',advance="no")"Enter the lower limit : "
read(*,*)lower_limit
write(*, fmt='(a)',advance="no")"Enter the upper limit : "
read(*,*)upper_limit
write(*, fmt='(a)',advance="no")" Enter number of sub intervals: "
read*,num_intervals

! only works when number of intervals is even

if (mod(num_intervals,2) /= 0)then
  print*,"number of intervals  should be an even number."
  num_intervals = num_intervals+1
end if
print *, "Number of intervals is:",num_intervals

! Finding step size
h=(upper_limit-lower_limit)/num_intervals
print*,"step size:",h

! Finding Integration Value
integration=f(lower_limit)+f(upper_limit)
  do i=1,num_intervals-1
    k=lower_limit+(i*h)

    if(mod(i,2)==0)then
      integration = integration + 2*(f(k))
     
    else
      integration = integration + 4*(f(k))
  
    end if
  end do


  integration=integration*h
  ans=integration/3
  write(*,fmt="(a,f9.5)")"Value of integration is ",ans


end program simpsons