! ----------------------------------------------------------------------
! MODULE: m_get_lambda
! ----------------------------------------------------------------------
! Purpose:
!   calculate lambda values from radian values
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------

        MODULE m_get_lambda
      IMPLICIT NONE

        CONTAINS

        SUBROUTINE get_lambda(rs, rp, sep, lambda, idbprt, idebug )

!     Calculate lambda

!      rs  : apparent radius of sun as viewed from satellite (radians)
!      rp  : apparent radius of eclipsing body as viewed from satellite (radians)
!      sep : apparent separation of the center of the Sun and eclipsing body (radians)

!      lambda : fraction of Sun's disk visible (1.0 = no eclipse; 0, = total eclipse)

        IMPLICIT NONE 
        INTEGER (KIND = 4) :: idbprt,idebug  
        REAL (KIND = 8)    :: rs,rp,sep,lambda
        REAL (KIND = 8)    :: r1,r2,phi,hgt,thet
        REAL (KIND = 8)    :: area1,area2,area3,ari,pi


!     function
       
        pi =  3.14159265359d0

        if (rs+rp.le.sep) then
!       no eclipse 
        return
        elseif( rp-rs.ge.sep ) then 
!       full eclipse
        lambda = 0.d0
        return
        else
!       partial eclipse, do the calculations

        if(sep.le.rs-rp)go to 361
!       set r1 = smaller disc, r2 = larger
        if(rs.gt.rp)go to 371
        r1=rs
        r2=rp
        go to 370
  371   continue
        r1=rp
        r2=rs
  370   continue
!         phi = 1/2 angle subtended in disc 1 by arc of intersection
        phi = dacos((r1*r1+sep*sep-r2*r2)/(2.0d0*r1*sep))
        if (phi.lt.0.d0) phi = pi + phi
        if(r2/r1.gt.5.0d0)go to 365
!        thet = 1/2 angle subtended in disc 2 by arc of intersection
!        hgt  = 1/2 linear distance between ends of arc of intersection
        hgt=r1*dsin(phi)
        thet=dasin(hgt/r2)
        area2=sep*hgt
        area3=thet*r2**2   
        go to 366
  365   continue
!         one disc much bigger - treat boundary as a straight line
        hgt=dsqrt(r1**2-(sep-r2)**2)
        area2=hgt*(sep-r2)
        area3=0.0d0
  366 continue
        area1=(pi-phi)*r1**2
!         ari = area of non-overlapped portion of small disc
        ari=area1+area2-area3  
        area1=pi*rs**2
        if(rs.gt.rp)go to 362
!         sun is small disc
        lambda=ari/area1
        go to 999
  362   continue
!         eclipsing body is small disc
        area2=pi*rp**2
        lambda=(area1+ari-area2)/area1
        go to 999
  361  continue
!         eclipsing body lies within sun's disc - what fraction of sun's disk is blocked
        lambda=(rs**2-rp**2)/rs**2   

      endif
                 
  999 continue

      return
      END SUBROUTINE

      END MODULE

