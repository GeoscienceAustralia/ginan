      SUBROUTINE LAGINT (X,Y,n,xint,yout)
C 
C     This subroutine performs lagrangian interpolation
C     within a set of (X,Y) pairs to give the y
C     value corresponding to xint. This program uses a
C     window of 4 data points to perform the interpolation.
C     if the window size needs to be changed, this can be
C     done by changing the indices in the do loops for
C     variables m and j.
C
C     PARAMETERS ARE :
C     X     - array of values of the independent variable
C     Y     - array of function values corresponding to x
C     n     - number of points
C     xint  - the x-value for which estimate of y is desired
C     yout  - the y value returned to caller

      implicit none
      REAL*8 X(n),Y(n),xint,yout,term
      INTEGER i,j,k,m,n

      yout = 0.d0
      do  i = 1,n-1
        if ( xint .ge. X(i) .and. xint .lt. X(i+1) ) k = i
      enddo
    
      if ( k .lt. 2 ) k = 2
      if ( k .gt. n-2 ) k = n-2
   
      do m = k-1,k+2
        term = y(m)
        do  j = k-1,k+2
          if ( m .ne. j ) then
            term = term * (xint - X(j))/(X(m) - X(j))
          end if
        enddo 
        yout = yout + term
      enddo
   
      return
      end