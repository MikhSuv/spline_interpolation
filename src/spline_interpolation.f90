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

function solve_penta(a, d) result(x)
    ! Решает СЛАУ с пятидиагональной симметричной матрицей 
    ! методом пятиточечной прогонки.
    !
    ! Структура хранения матрицы a(5,n):
    !   a(1,:) — вторая поддиагональ  (элементы i,i-2)
    !   a(2,:) — первая поддиагональ   (элементы i,i-1)
    !   a(3,:) — главная диагональ
    !   a(4,:) — первая наддиагональ   (элементы i,i+1)
    !   a(5,:) — вторая наддиагональ   (элементы i,i+2)
    !
    real(dp), intent(in) :: a(:, :)
    real(dp), intent(in) :: d(:)
    real(dp), allocatable :: x(:)

    integer :: n, i
    real(dp), allocatable :: p(:), q(:), r(:)
    real(dp) :: beta, alpha

    n = size(d)
    if (size(a, 2) /= n) error stop 'solve_penta: dimension mismatch'
    if (size(a, 1) /= 5) error stop 'solve_penta: expected 5×n matrix'

    allocate(x(n), source=0.0_dp)
    allocate(p(n), source=0.0_dp)
    allocate(q(n), source=0.0_dp)
    allocate(r(n), source=0.0_dp)

    ! Прямой ход
    do i = 1, n
        beta = 0.0_dp
        if (i > 1) beta = a(4, i-1)                 
        if (i > 2) beta = beta - p(i-2) * a(5, i-2) 

        alpha = a(3, i)
        if (i > 1) alpha = alpha - p(i-1) * beta
        if (i > 2) alpha = alpha - q(i-2) * a(5, i-2)

        if (abs(alpha) < tiny(1.0_dp)) &
            error stop 'solve_penta: zero pivot encountered'

        if (i < n) then
            p(i) = a(4, i)
            if (i > 1) p(i) = p(i) - q(i-1) * beta
            p(i) = p(i) / alpha
        end if

        if (i < n - 1) then
            q(i) = a(5, i) / alpha
        end if

        r(i) = d(i)
        if (i > 1) r(i) = r(i) - r(i-1) * beta
        if (i > 2) r(i) = r(i) - r(i-2) * a(5, i-2)
        r(i) = r(i) / alpha
    end do

    ! Обратный ход
    x(n) = r(n)
    if (n > 1) x(n-1) = r(n-1) - p(n-1) * x(n)

    do i = n - 2, 1, -1
        x(i) = r(i) - p(i) * x(i+1) - q(i) * x(i+2)
    end do

end function solve_penta
  
end module spline_interpolation
