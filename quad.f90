program quad
  implicit none
  integer, parameter :: PR=8
  integer :: n, i
  real(PR) :: a,b,h, Q
  integer :: p
  real(PR), dimension(:), allocatable :: points, poids

  print*, 'donnez a'
  read*, a
  print*, 'donnez b'
  read*, b
  print*, 'donnez n'
  read*, n
  print*, 'donnez p'
  read*, p

  h=(b-a)/real(n)
  allocate(points(0:n))
  Do i=0, n
     points(i)= a+i*h
  end Do
  Q=0._PR
  
  select case(p)
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

  open(unit=3, file='quad.dat')
  write (3,*) Q
  close(3)

contains

  function f(x)result(y)
    real(PR), intent(in) :: x
    real(PR) :: y

    y=1._PR/x
  end function f
  
end program quad

  
  
