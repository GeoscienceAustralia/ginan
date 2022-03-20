! SUBROUTINE: mat2quater
! ----------------------------------------------------------------------
! PURPOSE:
! This subroutine is an example of a possible algorithm to avoid
! numerical instabilities when computing a quaternion of rotation
! from the elements of the corresponding matrix of rotation.
! The matrix given in entry have to be a rotation matrix (i.e.
! having a determinant equal to 1). This is not checked here!
!
! INPUT : xmat (3,3) : matrix of rotation
! OUTPUT : quater(4) : corresponding quaternion of rotation
! (with a positive scalar part)
! quater=(q0,q1,q2,q3) q0: scalar part
! q1,q2,q3: vectorial part
!
! example:
!
! | 0.0000000000000000 -0.5000000000000001 -0.8660254037844386 |
! xmat= | 0.9238795325112867 -0.3314135740355917 0.1913417161825449 |
! | -0.3826834323650897 -0.8001031451912655 0.4619397662556435 |
! gives:
!
! quater = ( 0.5316310262343734 ! q0
! -0.4662278970042302 ! q1
! -0.2272920256568435 ! q2
! 0.6695807158758448 ) ! q3
!
! ----------------------------------------------------------------------

subroutine mat2quater(xmat,quater)
implicit none
double precision , dimension(3,3) :: xmat
double precision , dimension(4) :: quater , qsquare
double precision :: xtr, xs , max , denominator
integer :: imax , i
!
! computation of the squares of the quadriplet:
xtr = xmat(1,1)+xmat(2,2)+xmat(3,3)
qsquare(1) = (xtr + 1.d0)/4.d0
xs = 0.5d0 - qsquare(1)
qsquare(2) = xs + xmat(1,1)/2.d0
qsquare(3) = xs + xmat(2,2)/2.d0
qsquare(4) = xs + xmat(3,3)/2.d0
!
! we cannot take as it is the square root of these squares since this
! gives numerical errors for nearly vanishing values.
!
! selection of the max of the values (to be used like pivot):
!
max = -1.d0
do i=1,4
if (qsquare(i) > max ) then
max = qsquare(i)
imax = i
end if
end do
quater(imax) = sqrt(qsquare(imax))
denominator=4.d0*quater(imax)
!
! quaternion values now computed using extra diagonal terms
! of the matrix
!
if (imax == 1) then
quater(2)=(xmat(3,2)-xmat(2,3) )/denominator
quater(3)=(xmat(1,3)-xmat(3,1) )/denominator
quater(4)=(xmat(2,1)-xmat(1,2) )/denominator
else if (imax == 2) then
quater(1)= (xmat(3,2)-xmat(2,3))/denominator
quater(3)= (xmat(1,2)+xmat(2,1))/denominator
quater(4)= (xmat(1,3)+xmat(3,1))/denominator
else if (imax == 3) then
quater(1)= (xmat(1,3)-xmat(3,1))/denominator
quater(2)= (xmat(1,2)+xmat(2,1))/denominator
quater(4)= (xmat(2,3)+xmat(3,2))/denominator
else if (imax == 4) then
quater(1)= (xmat(2,1)-xmat(1,2))/denominator
quater(2)= (xmat(1,3)+xmat(3,1))/denominator
quater(3)= (xmat(2,3)+xmat(3,2))/denominator
end if
!
! as quater and -quater are equivalent this subroutine always return
! the one of the two having a positive scalar part.
! warning: this procedure does not imply that two successive call
! will give two quaternions that can be interpolated.
!
if (quater(1)<0.d0) quater(1:4)=-quater(1:4)
!
return
end
