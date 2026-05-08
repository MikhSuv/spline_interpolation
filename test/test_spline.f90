! test_spline.f90 – проверка гладкости сплайна на сложной функции
program test_spline
  use precision_mod
  use spline_interpolation, only: interpolate_spline, write_spline
  implicit none

  integer, parameter :: n = 1000           ! число отрезков (n+1 точка)
  integer, parameter :: q = 100          ! детализация выходной сетки
  real(dp), parameter :: PI = 4.0_dp * atan(1.0_dp)
  real(dp), allocatable :: x(:), y(:), w(:)
  real(dp), allocatable :: x_out(:), y_out(:)
  real(dp) :: left, right
  integer :: i, npts

  npts = n + 1
  allocate(x(npts), y(npts), w(npts))

  left  = 0.0_dp
  right = 2.0_dp * PI

  ! ---- Генерация данных ----
  do i = 1, npts
    x(i) = left + real(i-1, dp) * (right - left) / real(n, dp)
    y(i) = sin(x(i))              ! гладкая функция с ненулевой второй производной на концах
    w(i) = 1.0e10_dp              ! большой вес => практически интерполяция
  end do
  ! Можно также добавить слегка зашумлённую точку, чтобы показать устойчивость
  y(23) = 3
  w(23) = 1.0e-2_dp

  ! ---- Построение сплайна ----
  call interpolate_spline(x, y, w, q, x_out, y_out)

  ! ---- Сохранение результатов ----
  call write_spline("original.dat", x, y)          ! исходные точки
  call write_spline("spline.dat",   x_out, y_out)  ! сплайн

  ! Точная функция для сравнения
  block
    integer :: j
    open(unit=10, file="exact.dat", action="write")
    do j = 1, size(x_out)
      write(10, *) x_out(j), sin(x_out(j)) 
    end do
    close(10)
  end block

  print *, "Файлы original.dat, spline.dat, exact.dat созданы."
  call system("gnuplot plot.plt")
  call generate_data_file("data.dat", 1000, 0.12_dp, 12.0_dp)
! Файл: generate_data.f90
! Содержит подпрограмму для записи файла data.dat с зашумлёнными данными
end program test_spline
