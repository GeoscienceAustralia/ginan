       SUBROUTINE cross_product(X,Y,Z)

! PURPOSE    :  computation of the cross product of two vectors X and Y


! PARAMETERS :
!         IN :  X(I),I=1,2,3 COMPONENTS OF  X          
!               Y(I),I=1,2,3 COMPONENTS OF  Y          
!        OUT :  Z(I),I=1,2,3 COMPONENTS OF  Z      
!
! AUTHOR     :  Dr. Tzupang Tseng 
!
! CREATED    :  01-NOV-2017
!
!
! COPYRIGHT  :  GEOSCIENCE AUSTRALIA 
!     

       IMPLICIT NONE
       REAL*8       :: X(3),Y(3),Z(3)

! computation of the cross product of two vectros X and Y
! -------------------------------------------------------
       Z(1)=X(2)*Y(3)-X(3)*Y(2)
       Z(2)=X(3)*Y(1)-X(1)*Y(3)
       Z(3)=X(1)*Y(2)-X(2)*Y(1)
       RETURN
       END SUBROUTINE
    
