MODULE napier 
IMPLICIT NONE

INTEGER,PARAMETER:: rk= selected_real_kind(15)

CONTAINS

FUNCTION f(x) RESULT(r)

REAL(kind=rk), INTENT(IN):: x
REAL(kind=rk):: r

r=log(x)-x**2.0_rk+4.0_rk

END FUNCTION

FUNCTION Df(x) RESULT(d)

REAL(kind=rk), INTENT(IN):: x
REAL(kind=rk):: d

d=1.0_rk/x -2.0_rk*x

END FUNCTION

END MODULE
