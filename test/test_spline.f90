program test_spline
  use precision_mod
  use spline_interpolation, only: interpolate_spline, write_spline
  implicit none

  integer, parameter :: n = 10000           ! число отрезков (n+1 точка)
  integer, parameter :: q = 100          ! детализация выходной сетки
  real(dp), parameter :: PI = 4.0_dp * atan(1.0_dp)
  real(dp), allocatable :: x(:), y(:), w(:)
  real(dp), allocatable :: x_out(:), y_out(:)
  real(dp) :: left, right
  integer :: i, npts

  npts = n + 1
  allocate(x(npts), y(npts), w(npts))

  left  = 0.0_dp
  right = 20.0_dp * PI

  ! ---- Генерация данных ----
  do i = 1, npts
    x(i) = left + real(i-1, dp) * (right - left) / real(n, dp)
    y(i) = sin(x(i))              
    w(i) = 1.0_dp              
  end do
  ! Шумы 
  do i = 10, npts, 100
  call RANDOM_NUMBER(y(i))
  y(i) = y(i)*10.0_dp - 5.0_dp
  w(i) = 1.0e-2_dp
  end do

  ! ---- Построение сплайна ----
  call interpolate_spline(x, y, w, q, x_out, y_out)

  ! ---- Сохранение результатов ----
  call write_spline("original.dat", x, y)          ! исходные точки
  call write_spline("spline.dat",   x_out, y_out)  ! сплайн

  call system("gnuplot test_plot.plt")
end program test_spline
