         MACRO
         MNEWOBJ &OBJTYPE=,&WORK=,&OBJPTR=OBJPTR,&RETPTR=RETPTR
.*
         LA    R1,&WORK
         MVC   0(4,R1),=A(8+I&OBJTYPE._Vtbl_SIZ+&OBJTYPE._obj_SIZ)
         LA    R15,&OBJPTR
         ST    R15,4(,R1)
         L     R15,G_GTST
         BASR  R14,R15
*
         L     R15,&OBJPTR
         MVC   0(8,R15),=C'@@I&OBJTYPE.@@'
         LA    R15,8(,R15)
         MVC   0(I&OBJTYPE._Vtbl_SIZ,R15),&OBJTYPE.#01A
         LA    R14,I&OBJTYPE._Vtbl_SIZ(,R15)
*
         LR    R2,R14
         LA    R3,&OBJTYPE._obj_SIZ
         XR    R0,R0
         XR    R1,R1
         MVCL  R2,R0
*
         USING COM_obj,R14
         ST    R15,lpVtbl
         MVC   count,=A(1)
         DROP  R14
*
         L     R15,&RETPTR
         ST    R14,0(,R15)
*
         LA    R15,COM_obj_SIZ(,R14)
.*
         MEND
