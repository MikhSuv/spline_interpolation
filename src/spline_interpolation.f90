module spline_interpolation
  use precision_mod
  implicit none
  ! private

  ! public ::
contains

  subroutine read_data(filename, X, Y, P)
    ! Чтение данных функции, которую будем интерполировать
    ! X, Y - абсциссы и ординаты
    ! P - веса 
    character(len=*), intent(in) :: filename
    real(dp), allocatable, intent(out), dimension(:) :: X, Y, P
    
    integer ::n, iunit, iostatus, i
    character(len=256) :: line ! для чтения первой строки
    
    open(newunit=iunit, file=filename, status='old', &
    action = 'read', iostat=iostatus)
    if (iostatus /= 0) error stop 'Error occured while opening file'

    read(iunit, '(a)', iostat=iostatus) line
    line = adjustl(line) !Удаление пробелов слева и добавление их в конец
    if (line(1:1) == "#") line = line(2:) ! пропуск '#'
    read(line, *) n ! чтение порядка матрицы

    allocate(X(n+1), Y(n+1), P(n+1), source = 0.0_dp)
    
    do i = 1, n+1
      read(iunit, *) X(i), Y(i), P(i)
    end do
    
    close(iunit)
    
  end subroutine read_data
  
end module spline_interpolation
