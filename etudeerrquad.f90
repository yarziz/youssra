program etudeerrquad
  implicit none
  integer, parameter :: PR=8
  real(PR) :: Q, II, a, b, h, s
  integer :: n, meth, i, nmax, j, p
  real(PR), dimension(:), allocatable :: points
 

  II=0.75

  a=1._PR
  b=2._PR
  print*, 'donnez nmax'
  read*, nmax
  print*, 'donnez meth'
  read*, meth

  open(unit=3, file = 'erreur_rectg.dat')

  Do j=0,nmax
       p=2**j
     h=(b-a)/real(p)
   
     s= errquad(a,b,p,meth)
     write(3,*) h ,s

  end Do

  close(3)

  
contains

  function f(x) result(y)
    real(PR), intent(in) :: x
    real(PR) :: y

    y=1._PR/x
  end function f

  
  function errquad(a,b,n,meth) result(y)
    real(PR), intent(in) :: a,b
    integer, intent(in) :: n, meth
    real(PR) :: y

      allocate(points(0:n))
  Do i=0, n
     points(i)= a+i*h
  end Do
  Q=0._PR
  
  select case(meth)
  case(1)
     !rectangle à droite
     do i=1,n
        Q=Q+f(points(i))
     end do
     Q=Q*h

  case(2)
     ! Méthode du point milieu
     Do i=0,n-1
        Q=f(points(i)+(h/2._PR))
     end Do
     Q=Q*h

  case(3)
     ! méthode des trapèzes
     Q=(1._PR/2._PR)*(f(a)+f(b))
     do i=1,n-2
        Q=Q+f(points(i))+f(points(i+1))
     end do
     Q=Q*h

  case default 
     ! rectangle à gauche
     do i=0,n-1
        Q=Q+f(points(i))
     end do
     Q=Q*h


  end select

  deallocate(points)


    y=II-Q
  end function errquad

end program etudeerrquad
