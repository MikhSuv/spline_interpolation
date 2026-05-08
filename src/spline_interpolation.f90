module spline_interpolation
  use precision_mod
  use tridiagonal_matrix, only: tdmatmul
  implicit none
   private

   public :: read_data, interpolate_spline, write_spline
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

subroutine build_AB(X, A, B)
  ! Вспомогательная подпрограмма. Вычисляет матрицы A и B
  real(dp), intent(in) :: X(:)
  real(dp), allocatable, intent(out) :: A(:,:), B(:,:)

  integer :: i, m
  real(dp) :: h_prev, h_curr

  m = size(X)

  allocate(A(3, m), source = 0.0_dp)
  allocate(B(3, m), source = 0.0_dp)

  A(2, 1) = 2.0_dp * (X(2) - X(1))

  do i = 2, m-1
    h_prev = X(i)-X(i-1)
    h_curr = X(i+1) - X(i)

    A(1, i) = h_prev
    A(2, i) = 2.0_dp * (h_prev+h_curr)
    A(3, i) = h_curr

    B(1, i) = 1.0_dp / h_prev
    B(2, i) = -(1.0_dp/h_prev + 1.0_dp / h_curr)
    B(3, i) = 1.0_dp / h_curr
  end do

  A(2, m) = 2.0_dp * (X(m) - X(m-1))
  A(3, m-1) = 0.0_dp

end subroutine build_AB

function td_matvec(A, v) result(w)
  ! Вспомогательная функция умножения 
  ! трехдиагональной матрицы на вектор
  real(dp), intent(in) :: A(:,:), v(:)
  real(dp), ALLOCATABLE :: w(:)
  integer :: n, i

  n = size(v)
  if (size(A, 1) /= 3 .or. size(A, 2) /= n) &
        error stop 'td_matvec: input must be 3×n matrix and n-vector'
  allocate(w(n), source = 0.0_dp)

  do i = 1, n
    w(i) = A(2, i) * v(i)
    if (i > 1) w(i) = w(i) + A(1, i) * v(i-1)
    if (i < n) w(i) = w(i) + A(3, i) * v(i+1)
  end do
end function td_matvec

subroutine prepare_spline(x, y, weights, s, r) 
  real(dp), intent(in) :: x(:), y(:), weights(:)
  real(dp), allocatable, intent(out) :: s(:), r(:)

  real(dp), allocatable, DIMENSION(:, :) :: A, B, BT, QBT, C
  real(dp), ALLOCATABLE :: Q(:), rhs(:), BT_S(:)
   
  integer ::  i, m

  m = size(x)
  allocate(q(m))
  q = 1.0_dp/weights

  call build_AB(X, A, B)
  allocate(BT(3, m), source = 0.0_dp)
  BT(1, 2:m) = B(3, 1:m-1)
  BT(2, :) = B(2, :)
  BT(3, 1:m-1) = B(1, 2:m)
  allocate(qbt(3, m), source = 0.0_dp)

  ! Левая часть системы
  do i = 1, m
    qbt(:, i) = q(i) * bt(:, i)
  end do

  C = tdmatmul(B, QBT )
  C = 6.0_dp * C
  C(3, : ) = C(3, :) + A(2, :)
  C(2, 2:m) = C(2, 2:m) + A(1, 2:m)
  C(4, 1:m-1) = C(4, 1:m-1) + A(3,  1:m-1)
  deallocate(A, QBT)

  ! Правая часть системы

  rhs = td_matvec(B, y)
  rhs = 6.0_dp* rhs

  s = solve_penta(C, rhs)

  deallocate(C)

  BT_S = td_matvec(BT, s)
  allocate(r(m))
  r = y - q*bt_s

end subroutine prepare_spline

subroutine interpolate_spline(x, y, p, q, x_out, y_out)
  real(dp), intent(in) :: x(:), y(:), p(:)
  integer, intent(in) :: q
  real(dp), allocatable, intent(out) :: x_out(:), y_out(:)

  real(dp), allocatable :: S(:), R(:)
  integer :: m, n, n_out, i, k
  real(dp) :: h, t, chi, dchi


  m = size(x)
  n = m - 1 ! число исходных отрезков

  call prepare_spline(x, y, p, S, R)

  n_out =  q*n + 1
  allocate(x_out(n_out), y_out(n_out))

  dchi = (x(m) - x(1)) / real(n_out - 1, dp)

  do i = 1, n_out
    chi = x(1) + real((i-1), dp)*dchi

    if (chi >= x(m)) then 
      k = n
    else 
      do k = 1, n
        if (chi >= x(k) .and. chi < x(k+1)) exit
      end do
    end if

    h = x(k+1) - x(k)
    t = (chi - x(k)) / h
    y_out(i) = R(k)* (1.0_dp - t) + R(k+1)*t &
      - h**2 * t * (1.0_dp -t) / 6.0_dp &
      * ((2.0_dp - t)*S(k) + (1.0_dp + t)*s(k+1))
    x_out(i) = chi
  end do

end subroutine interpolate_spline

subroutine write_spline(filename, X, Y)
    character(len=*), intent(in) :: filename
    real(dp), allocatable, intent(in), dimension(:) :: X, Y

    integer :: i, n, ounit
    n = size(X)

    open(newunit=ounit, file=filename, action='write')
    do i = 1, n
    write(ounit, *) X(i), Y(i)
    end do
    close(ounit)
end subroutine write_spline
  
end module spline_interpolation
