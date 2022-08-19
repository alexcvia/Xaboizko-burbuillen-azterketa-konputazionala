module prestakuntza
!-------------------------------------
!-
!-  Modulo honek partukula bati buruz behar diren
!-  aldagaiak/funtzioak prestatuko ditu. Edo gutxienez
!-  beste programetan erabiltzeko prest egongo dira
!-
!-------------------------------------
use tipos
use parametroak
use funtzioak
use auzokoak
use sistemal !<----- i-ren erreferentzi sistema lokala

!public:: i_ren_auzokoak           
public:: i_ren_gainazal_tentsioa  !---> 
public:: i_ren_dibergentzia       !---> 
public:: i_ren_kurbatura_lokala   !---> 
public:: i_ren_presioa            !---> 



!-------------------------------------------------
contains
function i_ren_gainazal_tentsioa(kontzentrazioa)

  real(kind=dp),intent(in):: kontzentrazioa
  real(kind=dp)           :: i_ren_gainazal_tentsioa

    i_ren_gainazal_tentsioa = gamma_0 + gamma_a*kontzentrazioa

endfunction i_ren_gainazal_tentsioa
!-------------------------------------------------
!subroutine i_ren_auzokoak(partikula_guztiak,indizea,auzokoen_indizeak)
!
!  real(kind=dp),intent(in),dimension(:,:)     :: partikula_guztiak
!  integer,intent(in)                          :: indizea
!  integer,intent(out),dimension(:),allocatable:: auzokoen_indizeak
!  real(kind=dp)                               :: x
!  integer                                     :: i,j
!  
!  allocate(auzokoen_indizeak(n_j))
!
!endsubroutine i_ren_auzokoak

















endmodule prestakuntza
