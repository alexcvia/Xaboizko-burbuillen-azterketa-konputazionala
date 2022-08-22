module funtzioak
use tipos
use parametroak

public:: h_kernel       !---> h altuera numerikoko kernel funtzioa
public:: o_kernel       !---> erabiliko dugun kernel funtzio orokorra
public:: fi_kernel      !---> sistema lokala lortzeko kernel funtzioa
public:: psi_kernel     !---> tentsore metrikoa lortzeko kernel funtzioa
public:: dh_kernel      !---> bere deribatua r-rekiko
public:: do_kernel      !---> bere deribatua r-rekiko
public:: bektorial      !---> bi 3d-ko bektoreen arteko biderketa bektoriala
public:: alderantzizko  !---> matrize baten alderantzizkoa lortzen du


!-------------------------------------------------
contains
function h_kernel(r_in,h)
real(kind=dp),intent(in):: r_in,h
real(kind=dp):: h_kernel
real(kind=dp):: kte,r

r=abs(r_in)
kte = h/3.0_dp

  if (r < kte) then

    h_kernel = (3.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel  -  6.0_dp*(2.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel  + 15.0_dp*(1.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel*kte_h_kernel

  elseif (r >= kte .and. r < 2.0_dp*kte) then

    h_kernel = (3.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel  -  6.0_dp*(2.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel*kte_h_kernel

  elseif (r >= 2.0_dp*kte .and. r < h) then

    h_kernel = (3.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel*kte_h_kernel

  else
    h_kernel = 0.0_dp
  endif

endfunction h_kernel

!---------------------
!===========================================================================
!---------------------

function o_kernel(r,h)
real(kind=dp),intent(in):: r,h
real(kind=dp):: o_kernel
real(kind=dp):: kte

  if (r<=h) then

    o_kernel = kte_o_kernel*(h-r)**3

  else
    o_kernel = 0.0_dp
  endif

endfunction o_kernel

!---------------------
!===========================================================================
!---------------------

function fi_kernel(r,h)
real(kind=dp),intent(in):: r,h
real(kind=dp):: fi_kernel

  if (r<h) then

    fi_kernel = 1-(r/h)**3

  else
    fi_kernel = 0.0_dp
  endif

endfunction fi_kernel

!---------------------
!===========================================================================
!---------------------

function psi_kernel(r,h,n)
real(kind=dp),intent(in):: r,h
integer,intent(in)      :: n
real(kind=dp)           :: psi_kernel

  if (r>h) then
   
    psi_kernel = 0.0_dp

  elseif (r<=h .and. r>0.0_dp) then

    psi_kernel = 1.0_dp/n

  elseif (r==0.0_dp)     ! auzokoa = i-partikula bada
    psi_kernel = 1.0_dp
  endif

endfunction psi_kernel

!---------------------
!===========================================================================
!---------------------

function dh_kernel(r_in,h)
real(kind=dp),intent(in):: r_in,h
real(kind=dp):: dh_kernel
real(kind=dp):: kte,r

r=abs(r_in)
kte = h/3.0_dp

  if (r < kte) then

    dh_kernel = -(15.0_dp/h)*(3.0_dp-(3.0_dp*r)/h)**4
    dh_kernel = dh_kernel  +  (90.0_dp/h)*(2.0_dp-(3.0_dp*r)/h)**4
    dh_kernel = dh_kernel  - (225.0_dp/h)*(1.0_dp-(3.0_dp*r)/h)**4
    dh_kernel = dh_kernel*kte_h_kernel

  elseif (r >= kte .and. r < 2.0_dp*kte) then

    dh_kernel = -(15.0_dp/h)*(3.0_dp-(3.0_dp*r)/h)**4
    dh_kernel = dh_kernel  +  (90.0_dp/h)*(2.0_dp-(3.0_dp*r)/h)**4
    dh_kernel = dh_kernel*kte_h_kernel

  elseif (r >= 2.0_dp*kte .and. r < h) then

    dh_kernel = -(15.0_dp/h)*(3.0_dp-(3.0_dp*r)/h)**4
    dh_kernel = dh_kernel*kte_h_kernel

  else
    dh_kernel = 0.0_dp
  endif

endfunction dh_kernel

!---------------------
!===========================================================================
!---------------------

function do_kernel(r,h)
real(kind=dp),intent(in):: r,h
real(kind=dp):: do_kernel
real(kind=dp):: kte

  if (r<=h) then

    do_kernel = kte_o_kernel*(-3.0_dp)*(h-r)**2

  else
    do_kernel = 0.0_dp
  endif

endfunction do_kernel

!---------------------
!===========================================================================
!---------------------

function bektorial(a,b)
real(kind=dp),dimension(3),intent(in) :: a,b
real(kind=dp),dimension(3)            :: bektorial

  bektorial(1) = a(2) * b(3) - a(3) * b(2)
  bektorial(2) = a(3) * b(1) - a(1) * b(3)
  bektorial(3) = a(1) * b(2) - a(2) * b(1)

endfunction bektorial

!---------------------
!===========================================================================
!---------------------

function alderantzizko(A) result(Ainv)
  ! Returns the inverse of a matrix calculated by finding the LU
  ! decomposition.  Depends on LAPACK.
  real(kind=dp), dimension(:,:), intent(in) :: A
  real(kind=dp), dimension(size(A,1),size(A,2)) :: Ainv

  real(kind=dp), dimension(size(A,1)) :: work  ! work array for LAPACK
  integer, dimension(size(A,1)) :: ipiv   ! pivot indices
  integer :: n, info

  ! External procedures defined in LAPACK
  external DGETRF
  external DGETRI

  ! Store A in Ainv to prevent it from being overwritten by LAPACK
  Ainv = A
  n = size(A,1)

  ! DGETRF computes an LU factorization of a general M-by-N matrix A
  ! using partial pivoting with row interchanges.
  call DGETRF(n, n, Ainv, n, ipiv, info)

  if (info /= 0) then
     stop 'Matrix is numerically singular!'
  end if

  ! DGETRI computes the inverse of a matrix using the LU factorization
  ! computed by DGETRF.
  call DGETRI(n, Ainv, n, ipiv, work, n, info)

  if (info /= 0) then
     stop 'Matrix inversion failed!'
  end if
end function alderantzizko

endmodule funtzioak
