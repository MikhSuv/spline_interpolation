module tridiagonal_matrix

  use precision_mod
  implicit none
  private
  public :: read_tdmatrix, print_matrix, generate, tdmatmul, write_pdmatrix

  contains
    ! Чтение трёхдиагональной матрицы из файла в массив 3×n
    subroutine read_tdmatrix(A, filename)
      character(len=*), intent(in) :: filename
      real(dp), allocatable, intent(out) :: A(:,:)

      integer ::n, iunit, iostatus, i
      character(len=256) :: line ! для чтения первой строки

      open(newunit=iunit, file=filename, status='old', &
      action = 'read', iostat=iostatus)
      if (iostatus /= 0) error stop 'Error occured while opening file'

      read(iunit, '(a)', iostat=iostatus) line

      if (iostatus /= 0)  error stop 'Error occured while reading line'
      line = adjustl(line) !Удаление пробелов слева и добавление их в конец
      if (line(1:1) == "#") line = line(2:) ! пропуск '#'
      read(line, *) n ! чтение порядка матрицы

      allocate(A(3, n), source=0.0_dp)

      read(iunit, *) A(2,1), A(3, 1)

      do i = 2,n-1
        read(iunit, *) A(1, i), A(2, i), A(3, i)
      end do
      
      read(iunit, *) A(1, n), A(2,n)
      close(iunit)

    end subroutine read_tdmatrix

! Печать трёхдиагональной матрицы
    subroutine print_matrix(A)
      real(dp), intent(in) :: A(:,:)
      integer :: n

      n = size(A, 2)

      print *, 'Main diagonal: ', A(2, :)
      print *, 'Lower diagonal: ', A(1,2:)
      print *, 'Upper diagonal: ', A(3, :n-1)
      
    end subroutine print_matrix

 ! Генерация случайной трёхдиагональной матрицы размера n
    subroutine generate(A, n)
      real(dp), allocatable, intent(out) :: A(:,:)
      integer, intent(in) :: n
      allocate(A(3, n), source = 0.0_dp)
      call RANDOM_NUMBER(A(1,2:n))
      call RANDOM_NUMBER(A(2,:))
      call RANDOM_NUMBER(A(3,1:n-1))
    end subroutine generate

! Умножение двух трёхдиагональных матриц → пятидиагональная (5×n)
  function tdmatmul(A, B) result(C)
    real(dp), intent(in) :: A(:,:), B(:,:)
    real(dp), allocatable :: C(:,:)
    integer :: i, n

    if (size(A,1) /= 3 .or. size(B,1) /= 3) error stop 'tdmatmul: inputs must be 3×n'
    n = size(A,2)
    if (size(B,2) /= n) error stop 'tdmatmul: dimension mismatch'

    allocate(C(5, n), source=0.0_dp)

    ! Главная диагональ (C(3,:))
    do i = 1, n
      C(3,i) = A(2,i) * B(2,i)
      if (i > 1) C(3,i) = C(3,i) + A(1,i) * B(3,i-1)
      if (i < n) C(3,i) = C(3,i) + A(3,i) * B(1,i+1)
    end do

    ! Первая наддиагональ (C(4,:))
    do i = 1, n-1
      C(4,i) = A(2,i) * B(3,i) + A(3,i) * B(2,i+1)
    end do

    ! Вторая наддиагональ (C(5,:))
    do i = 1, n-2
      C(5,i) = A(3,i) * B(3,i+1)
    end do

    ! Первая поддиагональ (C(2,:))
    do i = 2, n
      C(2,i) = A(1,i) * B(2,i-1) + A(2,i) * B(1,i)
    end do

    ! Вторая поддиагональ (C(1,:))
    do i = 3, n
      C(1,i) = A(1,i) * B(1,i-1)
    end do
  end function tdmatmul
! Запись пятидиагональной матрицы в файл
  subroutine write_pdmatrix(C, filename)
    real(dp), intent(in) :: C(:,:)
    character(len=*), intent(in) :: filename
    integer :: i, n, ounit, iostatus

    if (size(C,1) /= 5) error stop 'write_pdmatrix: input must be 5×n'
    n = size(C,2)

    open(newunit=ounit, file=filename, action='write', iostat=iostatus)
    if (iostatus /= 0) error stop 'Error opening file'

    write(ounit, '("# ", i0)') n
    do i = 1, n
      if (i == 1) then
        write(ounit, '(3(f12.6,1x))') C(3:5, i)
      else if (i == 2) then
        write(ounit, '(4(f12.6,1x))') C(2:5, i)
      else if (i == n - 1) then
        write(ounit, '(4(f12.6,1x))') C(1:4, i)
      else if (i == n) then
        write(ounit, '(3(f12.6,1x))') C(1:3, i)
      else
        write(ounit, '(5(f12.6,1x))') C(1:5, i)
      end if
    end do
    close(ounit)
  end subroutine write_pdmatrix
!
end module tridiagonal_matrix 

