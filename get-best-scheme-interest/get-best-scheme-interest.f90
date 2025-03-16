PRINT*, "Enter the total investment"
READ*, TINVEST
PRINT*, "Enter the number of year"
READ*, YEAR
PRINT*, "Enter the simple interest percentage of a year"
READ*, SIP
PRINT*, "Enter the compound interest percentage of a year"
READ*, CIP

SINT=0.0
CINT=0.0
SINT=TINVEST*YEAR*(SIP/100.0)
CINT=TINVEST*(1.0+(CIP/100.0))**YEAR - TINVEST
IF(SINT.GT.CINT) THEN
    PRINT*, "Simple interest is the best scheme"
ELSE
    PRINT*, "Compound interest is the best scheme"
ENDIF
STOP
END
