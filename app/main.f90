program main
  use precision_mod
  use spline_interpolation
  implicit none

  real(dp), allocatable, dimension(:) :: A, B, P, X, Y
  integer :: q

  q = 100

  call read_data("data.dat", A, B, P)
  call interpolate_spline(A, B, P, q, X, Y)
  call write_spline("result.dat", X, Y)

end program main
