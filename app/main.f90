program main
  use precision_mod
  use spline_interpolation
  implicit none

  character(len=256) :: line
  real(dp), allocatable, dimension(:) :: A, B, P, X, Y
  integer :: q = 100

  if (COMMAND_ARGUMENT_COUNT() == 1) then
    call GET_COMMAND_ARGUMENT(1, line)
    read(line, *) q
  end if


  call read_data("data.dat", A, B, P)
  call interpolate_spline(A, B, P, q, X, Y)
  call write_spline("result.dat", X, Y)

end program main
