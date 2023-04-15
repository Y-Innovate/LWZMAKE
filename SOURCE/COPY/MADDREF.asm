         MACRO
&NAME    MADDREF
&NAME    DS    0H
*
         L     R1,0(,R1)         * Param 1 point to this object
         USING COM_obj,R1
*
         ASI   count,1           * Increase reference count
         L     R15,count         * Return new count
*
         DROP
*
         BR    R14
         MEND
