PROGRAM serie_numeriche

IMPLICIT NONE

!dichiaro le variabili
INTEGER :: i, n
REAL :: x, sm, y, er

PRINT*, "inserisci x, verrˆ calcolato ln(1+x)"
READ*, x
PRINT*, "inserisci n 'abbastanza' grande"

 DO 
  READ*, n !stabilisce il limite superiore delle ridotte parziali
  y= log(1+x)
  sm=0
  
   DO i=1,n
  
   sm= sm+(((-1)**(i-1))*(x**i)/i)
   er= abs(sm-y)/y
   
    IF (er<= 10.**(-4)) THEN
     PRINT*, "valore serie", sm
     PRINT*, "valore logaritmo", y
     PRINT*, "termine da cui l'errore  minore di 10**-4", i
    IF (er<= 10.**(-4)) EXIT
     END IF
   END DO

   IF (er> 10.**(-4)) THEN
    PRINT*, "inserisci n maggiore"
    ELSE 
    EXIT
   END IF

 END DO

END PROGRAM serie_numeriche
