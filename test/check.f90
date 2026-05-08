program test
 use precision_mod
 use spline_interpolation

 implicit none

real(dp), allocatable, dimension(:) :: X, Y, P, splineX, splineY
real(dp), parameter :: pi = 4.0_dp*atan(1.0_dp)
real(dp) :: h 
integer :: n = 1000
integer :: q = 100
integer :: i

h = 2.0_dp * pi / real(n, dp)
allocate(X(n+1), Y(n+1), P(n+1))

do i = 0, n
x(i+1) = real(i, dp) * h
end do

y = cos(x)
P = 1.0_dp

call interpolate_spline(x, y, p, q, splineX, splineY)
call write_spline("result.dat", splineX, splineY)

end program test
