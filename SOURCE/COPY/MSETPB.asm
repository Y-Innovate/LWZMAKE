         MACRO
&NAME    MSETPB &OBJ=,&PROP=
&NAME    DS    0H
*
         L     R15,0(,R1)
         USING &OBJ._obj,R15
*
         L     R1,4(,R1)
         STC   R1,&PROP
*
         DROP
*
         XR    R15,R15
         BR    R14
.*
         MEND
