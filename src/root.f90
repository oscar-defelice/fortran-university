PROGRAM root_function
USE napier
IMPLICIT NONE

REAL(kind= rk):: x,e,a,b,at,bt,tm
INTEGER:: i,j,n

PRINT*, "il programma risolve l'equazione f(x)=0"

!metodo di Newton-Raphson

DO
PRINT*, "immetti il punto da cui partire nell'esecuzione dell'algoritmo di Newton"
READ*,x
e=5.0_rk*epsilon(x)
IF (Df(x)/=0.0_rk) EXIT
PRINT*, 'la funzione ha un punto critico in ', x,"l'algoritmo non funziona"
END DO 

PRINT*, "inserisci il numero di iterazioni"
READ*,n

DO i=1,n

x=x-(f(x)/Df(x))
IF (Df(x)==0.0_rk) THEN
PRINT*, 'la funzione ha un punto critico in ', x,"l'algoritmo non funziona"
EXIT
END IF
IF (f(x)==0.0_rk) THEN
PRINT*, 'f(x)=0'
EXIT
ELSE IF (ABS(f(x))<e) THEN
PRINT*, '|f(x)|< precisione richiesta'
EXIT
END IF

END DO

PRINT*,"soluzione (NEWTON)=",x,"passi: ",i

!metodo della falsa posizione

DO 
PRINT*, "inserisci gli estremi dell'intervallo in cui vuoi ricercare la soluzione"
READ*,a,b

IF(f(a)*f(b)<0) EXIT

PRINT*, "f(a)*f(b)>0, oppure non sono rispettate condizioni di esistenza, riprova con altri a e b"
END DO 
at=a
bt=b

x=a

DO j=0,n
x=x-(f(x)*((x-b)/(f(x)-f(b))))
 IF (f(x)==0.0_rk) THEN
 EXIT
 ELSE IF(f(x)*f(b)>0.0_rk) THEN
  b=x
  ELSE
  b=b
 END IF
 IF (ABS(f(x))<e) THEN
 PRINT*, '|f(x)|< precisione richiesta'
 EXIT
 ELSE IF (ABS(x-b)<e) THEN
 PRINT*, "ampiezza dell'intervallo < precisione richiesta"
 EXIT
 END IF
END DO

PRINT*,"soluzione (FIBONACCI)=",x,"passi: ",j

!metodo delle secanti

a=at
b=bt

x=a

tm=x

DO j=0,n
x=x-(f(x)*((x-b)/(f(x)-f(b))))
 IF (f(x)==0.0_rk) THEN
 EXIT
 ELSE IF(f(x)*f(b)>0.0_rk) THEN
  b=tm
 ELSE
  b=b
  END IF
 IF (ABS(f(x))<e) THEN
 PRINT*, '|f(x)|< precisione richiesta'
 EXIT
 ELSE IF (ABS(x-b)<e) THEN
 PRINT*, "ampiezza dell'intervallo < precisione richiesta"
 EXIT
 END IF
tm=x
END DO

PRINT*,"soluzione (SECANTI)=",x,"passi: ",j

END PROGRAM root_function
