*---------------------------------------------------------------------*         
* Program    : LWZMFMG                                                *         
* Description: COM object for File ManaGer                            *         
*---------------------------------------------------------------------*         
         TITLE 'LWZMFMG'                                                        
*                                                                               
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's             
*                                                                               
         COPY  IFACES            * Object interfaces                            
*                                                                               
         COPY  MINSTANT          * Macro to instantiate new object              
*                                                                               
* Main routine creates a new FMG object                                         
*                                                                               
LWZMFMG  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10                             
*                                                                               
         USING WORKDSA,R13       * Address DSA and extra stg for vars           
*                                                                               
         L     R6,0(,R1)         * Pointer to GUID                              
         L     R5,4(,R1)         * Pointer to pointer object                    
         ST    R5,RETPTR         * Save return object ptr ptr                   
         MVC   0(4,R5),=A(0)     * By default return NULL object                
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
*        Was a new FMG object requested?                                        
         IF (CLC,0(16,R6),EQ,G_IFMG_GUID) THEN                                  
            MNEWOBJ OBJTYPE=FMG,WORK=WORK * Alloc new object                    
*                                                                               
*           Init obj attributes                                                 
            LR    R7,R14                                                        
            USING FMG_obj,R7                                                    
            DROP  R7                                                            
*                                                                               
            ASI   G_OBJCOUNT,1   * Increate global obj count                    
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN                           
               ST    R7,G_DEC8                                                  
               UNPK  G_ZONED8(9),G_DEC8(5)                                      
               L     R15,G_HEXTAB                                               
               TR    G_ZONED8(8),0(R15)                                         
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK                            
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=MAK501D_FMG                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=G_ZONED8                                                    
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
            ENDIF                                                               
*                                                                               
*        Was a new FFO object requested?                                        
         ELSEIF (CLC,0(16,R6),EQ,G_IFFO_GUID) THEN                              
            MNEWOBJ OBJTYPE=FFO,WORK=WORK * Alloc new object                    
*                                                                               
*           Init obj attributes                                                 
            LR    R7,R14                                                        
            USING FFO_obj,R7                                                    
            MVI   FFO_exists,C'N'                                               
            DROP  R7                                                            
*                                                                               
            ASI   G_OBJCOUNT,1   * Increate global obj count                    
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN                           
               ST    R7,G_DEC8                                                  
               UNPK  G_ZONED8(9),G_DEC8(5)                                      
               L     R15,G_HEXTAB                                               
               TR    G_ZONED8(8),0(R15)                                         
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK                            
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=MAK501D_FFO                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=G_ZONED8                                                    
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
            ENDIF                                                               
*                                                                               
         ELSE                                                                   
            CALL  CEE3ABD,(=A(1003),=A(3)),MF=(E,WORK)                          
         ENDIF                                                                  
*                                                                               
LWZMFMG_RET EQU   *                                                             
         CEETERM                 * Return to caller                             
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
FMG#01A                      DC    A(FMG#01)   * QueryInterface                 
FMG#02A                      DC    A(FMG#02)   * AddRef                         
FMG#03A                      DC    A(FMG#03)   * Release                        
FMG#04A                      DC    A(FMG#04)   * Stat                           
FMG#05A                      DC    A(FMG#05)   * Memberlist                     
FMG#06A                      DC    A(FMG#06)   * DDNameToDSName                 
*                                                                               
                             DS    0F                                           
FFO#01A                      DC    A(FFO#01)   * QueryInterface                 
FFO#02A                      DC    A(FFO#02)   * AddRef                         
FFO#03A                      DC    A(FFO#03)   * Release                        
*                                                                               
                             DS    0F                                           
MAK501D_FMG                  DC    C'MAK501D Created IFMG object '              
                             DS    0F                                           
MAK501D_FFO                  DC    C'MAK501D Created IFFO object '              
*                                                                               
PPA      CEEPPA                                                                 
*                                                                               
         CEECAA                                                                 
*                                                                               
         CEEDSA                                                                 
*                                                                               
WORKDSA                      DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
FC                           DS    3A  * Feedback code                          
WORK                         DS    4A  * Work area                              
RETPTR                       DS    A   * Address of return ptr                  
OBJPTR                       DS    A   * Address of new obj stg                 
*                                                                               
WORKDSA_SIZ                  EQU   *-WORKDSA                                    
*                                                                               
         MGLOBAL                 * Global area DSECT                            
*                                                                               
         COPY  DSCOMOBJ          * Generic COM obj DSECT                        
*                                                                               
         COPY  DSFMG             * IFMG obj DSECT                               
         COPY  DSFFO             * IFFO obj DSECT                               
         COPY  DSSTB             * ISTB obj DSECT                               
*                                                                               
* DSECT for addressing a DCB                                                    
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
* DSECT for addresssing a DCBE                                                  
         IHADCBE                                                                
*                                                                               
         CVT DSECT=YES,LIST=YES                                                 
*                                                                               
         IHAPSA DSECT=YES,LIST=YES                                              
*                                                                               
         IKJTCB DSECT=YES,LIST=YES                                              
*                                                                               
TIOTDSECT DSECT                                                                 
         IEFTIOT1                                                               
*                                                                               
         IEFJESCT                                                               
*                                                                               
         IEFZB505 LOCEPAX=YES                                                   
*                                                                               
         DROP                                                                   
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
* IFMG QueryInterface                                                           
*                                                                               
FMG#01   MQRYIFCE SUF=F01,IFACE=IFMG                                            
*                                                                               
* IFMG AddRef                                                                   
*                                                                               
FMG#02   MADDREF                                                                
*                                                                               
* IFMG Release                                                                  
*                                                                               
FMG#03   MRELEASE SUF=F03,OBJ=FMG                                               
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IFMG Stat                                                                     
*                                                                               
FMG#04   CEEENTRY AUTO=WORKDSAF04_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF04,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   PARMFNAMEF04,4(R1) * Parm 2 is zero term file name               
*                                                                               
         L     R7,8(,R1)         * Parm 3 is IFFO object                        
         ST    R7,PARMIFFOF04    * Save in local var                            
         USING FFO_obj,R7                                                       
*                                                                               
*        Determine length of file name                                          
         XR    R0,R0             * Search for terminating zero                  
         L     R1,PARMFNAMEF04   * Point R1 to file name                        
         LA    R2,4095(,R1)      * Search for max 4K                            
         SRST  R2,R1             * Scan for zero terminator                     
         BRC   1,*-4             * Scan was incomplete, try again               
         BRC   2,*-12            * Not found, try another 4K                    
*                                                                               
         L     R1,PARMFNAMEF04   * Point R1 to file name                        
         SR    R2,R1             * Calculate length                             
         ST    R2,FNAMELENF04    * Save in local var                            
*                                                                               
*        If target is not phony and starts with /                               
         IF (CLI,FFO_ftype,NE,C'.'),AND,                               X        
               (CLI,0(R1),EQ,C'/') THEN                                         
            MVI   FFO_ftype,C'U'                                                
*                                                                               
            XR    R0,R0          * Search for                                   
            IC    R0,=C'/'       *    forward slash                             
            L     R1,PARMFNAMEF04 * Point R1 to file name                       
            LR    R2,R1                                                         
            A     R2,FNAMELENF04                                                
            LR    R3,R1                                                         
            LR    R4,R2                                                         
            SRST  R2,R1          * Scan for bracket                             
            BRC   1,*-4          * Scan was incomplete, try again               
*                                                                               
            DO WHILE=(4)         * If / found                                   
               LA    R1,1(,R2)                                                  
               LR    R3,R1                                                      
*                                                                               
               LR    R2,R4                                                      
*                                                                               
               SRST  R2,R1       * Scan for bracket                             
               BRC   1,*-4       * Scan was incomplete, try again               
            ENDDO                                                               
*                                                                               
            SR    R2,R1                                                         
            IF (2) THEN                                                         
               LA    R2,1(,R2)                                                  
               LA    R1,WORKF04                                                 
               ST    R2,0(,R1)                                                  
               LA    R15,FFO_member                                             
               ST    R15,4(,R1)                                                 
               L     R15,G_GTST                                                 
               BASR  R14,R15                                                    
*                                                                               
               L     R5,FFO_member                                              
               BCTR  R2,R0                                                      
               B     *+10                                                       
               MVC   0(1,R5),0(R3)                                              
               EX    R2,*-6                                                     
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (CLI,FFO_ftype,EQ,C'.'),OR,                                X        
               (CLI,FFO_ftype,EQ,C'U') THEN                                     
            LA    R1,WORKF04                                                    
            L     R2,FNAMELENF04                                                
            LA    R2,1(,R2)                                                     
            ST    R2,0(,R1)                                                     
            LA    R15,FFO_fname                                                 
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            LR    R3,R2                                                         
            L     R2,FFO_fname                                                  
            L     R14,PARMFNAMEF04                                              
            LR    R15,R3                                                        
            MVCL  R2,R14                                                        
*                                                                               
            MVC   FFO_fnameLen,FNAMELENF04                                      
*                                                                               
            B     FMG#04_FTYPE_OK                                               
         ENDIF                                                                  
*                                                                               
*        See if it's an MVS data set name                                       
         IF (CLC,FNAMELENF04,LE,=A(54)) THEN                                    
            XR    R0,R0          * Search for                                   
            IC    R0,=C'('       *    open bracket                              
            L     R1,PARMFNAMEF04 * Point R1 to file name                       
            LA    R2,46(,R1)     * Search for max 45 chars                      
            SRST  R2,R1          * Scan for bracket                             
            BRC   1,*-4          * Scan was incomplete, try again               
*                                                                               
            IF (4) THEN          * If ( found                                   
               MVI   FFO_ftype,C'M'                                             
*                                                                               
               LR    R4,R2                                                      
               L     R3,PARMFNAMEF04                                            
               SR    R4,R3                                                      
*                                                                               
               ST    R4,FFO_fnameLen                                            
*                                                                               
               LA    R1,WORKF04                                                 
               ST    R4,0(,R1)                                                  
               ASI   0(R1),1                                                    
               LA    R15,FFO_fname                                              
               ST    R15,4(,R1)                                                 
               L     R15,G_GTST                                                 
               BASR  R14,R15                                                    
*                                                                               
               L     R5,FFO_fname                                               
               XR    R14,R14                                                    
               STC   R14,0(R4,R5)                                               
               BCTR  R4,R0                                                      
               B     *+10                                                       
               MVC   0(1,R5),0(R3)                                              
               EX    R4,*-6                                                     
*                                                                               
               IF (C,R4,LE,=A(45)),AND,                                X        
               (C,R4,LT,FNAMELENF04) THEN                                       
                  IC    R0,=C')'                                                
                  LA    R4,1(,R2)                                               
                  L     R1,PARMFNAMEF04                                         
                  A     R1,FNAMELENF04                                          
                  SRST  R1,R4                                                   
                  BRC   1,*-4                                                   
*                                                                               
                  IF (4) THEN    * If ) found                                   
                     LR    R3,R1                                                
                     SR    R3,R4                                                
                     IF (C,R3,GT,=A(0)) THEN                                    
                        ST    R3,FFO_memberLen                                  
                        LA    R3,1(,R3)                                         
*                                                                               
                        LA    R1,WORKF04                                        
                        ST    R3,0(,R1)                                         
                        LA    R15,FFO_member                                    
                        ST    R15,4(,R1)                                        
                        L     R15,G_GTST                                        
                        BASR  R14,R15                                           
*                                                                               
                        L     R2,FFO_member                                     
                        L     R3,FFO_memberLen                                  
                        BCTR  R3,R0                                             
                        B     *+10                                              
                        MVC   0(1,R2),0(R4)                                     
                        EX    R3,*-6                                            
*                                                                               
                        B     FMG#04_FTYPE_OK                                   
                     ENDIF                                                      
                  ENDIF                                                         
               ENDIF                                                            
            ELSE                                                                
               IF (CLC,FNAMELENF04,LE,=A(44)) THEN                              
                  MVI   FFO_ftype,C'D'                                          
*                                                                               
                  LA    R1,WORKF04                                              
                  L     R2,FNAMELENF04                                          
                  LA    R2,1(,R2)                                               
                  ST    R2,0(,R1)                                               
                  LA    R15,FFO_fname                                           
                  ST    R15,4(,R1)                                              
                  L     R15,G_GTST                                              
                  BASR  R14,R15                                                 
*                                                                               
                  LR    R3,R2                                                   
                  L     R2,FFO_fname                                            
                  L     R14,PARMFNAMEF04                                        
                  LR    R15,R3                                                  
                  MVCL  R2,R14                                                  
*                                                                               
                  MVC   FFO_fnameLen,FNAMELENF04                                
*                                                                               
                  B     FMG#04_FTYPE_OK                                         
               ENDIF                                                            
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
FMG#04_FTYPE_OK EQU   *                                                         
*                                                                               
         IF (CLI,FFO_ftype,EQ,C'D'),OR,(CLI,FFO_ftype,EQ,C'M') THEN             
            LA    R1,WORKF04                                                    
            ST    R7,0(,R1)                                                     
            L     R15,FMG#99A_F04 * Catalog search                              
            BASR  R14,R15                                                       
*                                                                               
            LTR   R15,R15                                                       
            IF (NZ) THEN                                                        
               MVI   FFO_exists,C'Y'                                            
*                                                                               
*              L     R15,G_ILOG                                                 
*              IF (CLI,13(R15),GE,LOG_LEVEL_INFO),AND,                 X        
               (CLI,FFO_ftype,EQ,C'D') THEN                                     
*                 ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF04                      
*                 ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF04,   X        
               ZSTR=MAK310I_F04                                                 
*                 L     R2,FFO_fname                                            
*                 ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF04,   X        
               ZSTR=0(,R2)                                                      
*                                                                               
*                 L     R2,G_ISTB_tmp                                           
*                 L     R2,STB_lpBuf-STB_obj(,R2)                               
*                 ILOG_Write OBJECT=G_ILOG,WORK=WORKF04,LINE=0(,R2),   X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
*              ENDIF                                                            
*                                                                               
               LA    R1,WORKF04                                                 
               ST    R7,0(,R1)                                                  
               L     R15,FMG#98A_F04 * CAMLST OBTAIN                            
               BASR  R14,R15                                                    
*                                                                               
               IF (CLI,FFO_ftype,EQ,C'M'),AND,                         X        
               (TM,FFO_dsorg,DS1DSGPO,Z) THEN                                   
                  ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF04                      
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF04,   X        
               ZSTR=MAK112E_F04                                                 
                  L     R2,FFO_fname                                            
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF04,   X        
               ZSTR=0(,R2)                                                      
*                                                                               
                  L     R2,G_ISTB_tmp                                           
                  L     R2,STB_lpBuf-STB_obj(,R2)                               
                  ILOG_Write OBJECT=G_ILOG,WORK=WORKF04,LINE=0(,R2),   X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
                  MVC   G_RETCODE,=A(8)                                         
                  B     FMG#04_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLI,FFO_ftype,EQ,C'M') THEN                                  
                  MVI   FFO_exists,C'N'                                         
*                                                                               
                  IF (TM,FFO_recfm,DS1RECFU,1) THEN                             
                     LA    R1,WORKF04                                           
                     ST    R7,0(,R1)                                            
                     L     R15,FMG#96A_F04 * Read load mod date                 
                     BASR  R14,R15                                              
                  ELSE                                                          
                     LA    R1,WORKF04                                           
                     ST    R7,0(,R1)                                            
                     L     R15,FMG#97A_F04 * Read PDS directory                 
                     BASR  R14,R15                                              
                  ENDIF                                                         
               ENDIF                                                            
            ENDIF                                                               
         ELSEIF (CLI,FFO_ftype,EQ,C'U') THEN                                    
            IUSS_BPX1STA OBJECT=G_IUSS,WORK=WORKF04,IFFO=PARMIFFOF04            
         ENDIF                                                                  
*                                                                               
FMG#04_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK112E_F04                  DC    C'MAK112E Member specified on non-PDX        
               S: ',X'00'                                                       
*                                                                               
*                            DS    0F                                           
*AK310I_F04                  DC    C'MAK310I Data set is cataloged: ',XX        
               '00'                                                             
*                                                                               
                             DS    0F                                           
FMG#96A_F04                  DC    A(FMG#96) * Read load mod date               
FMG#97A_F04                  DC    A(FMG#97) * Read PDS directory               
FMG#98A_F04                  DC    A(FMG#98) * CAMLST OBTAIN                    
FMG#99A_F04                  DC    A(FMG#99) * IGGCSI00                         
*                                                                               
WORKDSAF04                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF04                      DS    4A                                           
*                                                                               
PARMFNAMEF04                 DS    A                                            
PARMIFFOF04                  DS    A                                            
*                                                                               
                             DS    0F                                           
FNAMELENF04                  DS    F                                            
*                                                                               
WORKDSAF04_SIZ               EQU   *-WORKDSAF04                                 
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IFMG Memberlist                                                               
*                                                                               
FMG#05   CEEENTRY AUTO=WORKDSAF05_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF05,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   PARMPDSNAMEF05,4(R1) * Parm 2 is PDS name                        
*                                                                               
         MVC   PARMFILTERF05,8(R1) * Parm 3 is member filter                    
*                                                                               
         L     R7,12(,R1)        * Parm 4 is ISTB return ptr                    
         ST    R7,PARMISTBRETF05 * Save as local var                            
*                                                                               
         IF (CLC,0(4,R7),EQ,=A(0)) THEN                                         
            MINSTANT GUID=G_ISTB_GUID,WORK=WORKF05,OBJPTR=0(,R7)                
         ELSE                                                                   
            ISTB_Init OBJECT=0(,R7),WORK=WORKF05                                
         ENDIF                                                                  
*                                                                               
         L     R3,PARMFILTERF05                                                 
         IF (C,R3,NE,=A(0)),AND,(CLI,0(R3),NE,X'00') THEN                       
            XR    R0,R0                                                         
            LA    R2,8(,R3)                                                     
            SRST  R2,R3                                                         
            BC    1,*-4                                                         
            IF (4) THEN                                                         
               SR    R2,R3                                                      
               ST    R2,FILTERLEN_F05                                           
            ELSE                                                                
               MVC   FILTERLEN_F05,=A(8)                                        
            ENDIF                                                               
         ELSE                                                                   
            MVC   FILTERLEN_F05,=A(0)                                           
         ENDIF                                                                  
*                                                                               
         MINSTANT GUID=G_IFFO_GUID,WORK=WORKF05,OBJPTR=IFFO_F05                 
*                                                                               
         L     R3,IFFO_F05                                                      
         MVC   FFO_fname-FFO_obj(4,R3),PARMPDSNAMEF05                           
         MVI   FFO_ftype-FFO_obj(R3),C'D'                                       
*                                                                               
         XR    R0,R0             * Search for terminating zero                  
         L     R1,PARMPDSNAMEF05 * Point R1 to zero term PDS name               
         LA    R2,44(,R1)        * Search for max 44 chars                      
         SRST  R2,R1             * Scan for zero terminator                     
         BRC   1,*-4             * Scan was incomplete, try again               
         BRC   2,*-12            * Not found, try another 4K                    
*                                                                               
         L     R1,PARMPDSNAMEF05 * Point R1 to zero term PDS name               
         SR    R2,R1             * Calculate length                             
         ST    R2,FFO_fnameLen-FFO_obj(,R3) * Save PDS name length              
*                                                                               
         LA    R1,WORKF05                                                       
         MVC   0(4,R1),IFFO_F05                                                 
         XR    R15,R15                                                          
         IC    R15,=C'A'                                                        
         ST    R15,4(,R1)        * Allocate data set, no member                 
         LA    R15,DDNAME_F05    * Return DD name here                          
         ST    R15,8(,R1)                                                       
         L     R15,FMG#80A_F05   * Dynamic allocation/unallocation              
         BASR  R14,R15                                                          
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   FMG#05_RET                                                       
*                                                                               
         GETMAIN RU,LV=DCB_PDSDIR_DSECT_SIZ,LOC=24                              
*                                                                               
         ST    R1,DCB_PDSDIR_PTR_F05                                            
*                                                                               
         LA    R2,DCB_PDSDIR-DCB_PDSDIR_DSECT(,R1)                              
         L     R15,CDCB_PDSDIRA_F05                                             
         MVC   0(LEN_DCB_PDSDIR,R2),0(R15)                                      
         LA    R3,DCBE_PDSDIR-DCB_PDSDIR_DSECT(,R1)                             
         L     R15,CDCBE_PDSDIRA_F05                                            
         MVC   0(LEN_DCBE_PDSDIR,R3),0(R15)                                     
*                                                                               
         ST    R3,DCBDCBE-IHADCB(,R2)                                           
*                                                                               
         LA    R4,FMG#05_EOF                                                    
         ST    R4,DCBEEODA-DCBE(,R3)                                            
*                                                                               
         MVC   DCBDDNAM-IHADCB(8,R2),DDNAME_F05                                 
*                                                                               
         MVC   G_OPEND,G_OPENL                                                  
*                                                                               
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                              
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            CVD   R15,G_DEC8                                                    
            UNPK  G_ZONED8,G_DEC8                                               
            OI    G_ZONED8+7,X'F0'                                              
            MVI   G_ZONED8+8,X'00'                                              
            L     R14,G_TRT_ONLY_ZEROS                                          
            TRT   G_ZONED8(7),0(R14)                                            
            BC    7,*+8                                                         
            LA    R1,G_ZONED8+7                                                 
            LR    R6,R1                                                         
*                                                                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF05                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF05,         X        
               ZSTR=MAK101E_F05                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF05,         X        
               ZSTR=0(,R6)                                                      
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF05,         X        
               ZSTR==X'4000'                                                    
            MVI   DDNAME_F05+8,X'00'                                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF05,         X        
               ZSTR=DDNAME_F05                                                  
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKF05,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
            MVC   G_RETCODE,=A(12)                                              
            B     FMG#05_FREE                                                   
         ENDIF                                                                  
*                                                                               
         MVI   PDSDIR_bEOF_F05,C'N'                                             
         MVI   FIRSTMEM_F05,C'Y'                                                
*                                                                               
FMG#05_GET_DIRREC EQU *                                                         
         L     R1,DCB_PDSDIR_PTR_F05                                            
         LA    R2,DCB_PDSDIR-DCB_PDSDIR_DSECT(,R1)                              
         LA    R6,FMG#05_DIRREC_NOMORE                                          
         GET   (R2),FMG_DIRREC                                                  
*                                                                               
         LA    R3,FMG_DIRREC                                                    
         XR    R4,R4                                                            
         LH    R4,0(,R3)                                                        
         C     R4,=F'14'                                                        
         BL    FMG#05_DIRREC_END_OF_BLOCK                                       
         LA    R3,2(,R3)                                                        
         S     R4,=F'2'                                                         
FMG#05_NEXT_DIRREC_ENTRY EQU *                                                  
         CLC   0(8,R3),=8X'FF'                                                  
         BE    FMG#05_DIRREC_NOMORE                                             
*                                                                               
         MVI   MEM_F05,C' '                                                     
         MVI   MEM_F05+1,X'00'                                                  
         MVC   MEM_F05+2(8),MEM_F05+1                                           
*                                                                               
         L     R2,G_TRT_ONLY_SPACE                                              
         LA    R14,7(,R3)                                                       
         TRTR  0(8,R14),0(R2)                                                   
         SR    R1,R3                                                            
         B     *+10                                                             
         MVC   MEM_F05+1(1),0(R3)                                               
         EX    R1,*-6                                                           
*                                                                               
         LT    R15,FILTERLEN_F05                                                
         IF (NZ) THEN                                                           
            BCTR  R15,R0                                                        
            CR    R15,R1                                                        
            BH    FMG#05_SKIP_MEM                                               
*                                                                               
            LA    R14,MEM_F05+1                                                 
            L     R1,PARMFILTERF05                                              
            B     *+10                                                          
            CLC   0(1,R14),0(R1)                                                
            EX    R15,*-6                                                       
            BNE   FMG#05_SKIP_MEM                                               
         ENDIF                                                                  
*                                                                               
         IF (CLI,FIRSTMEM_F05,EQ,C'Y') THEN                                     
            LA    R2,MEM_F05+1                                                  
            MVI   FIRSTMEM_F05,C'N'                                             
         ELSE                                                                   
            LA    R2,MEM_F05                                                    
         ENDIF                                                                  
*                                                                               
         ISTB_AppendZString OBJECT=0(,R7),WORK=WORKF05,ZSTR=0(,R2)              
*                                                                               
FMG#05_SKIP_MEM EQU   *                                                         
*                                                                               
         L     R5,8(,R3)                                                        
         N     R5,=X'0000001F'                                                  
         SLL   R5,1                                                             
         LA    R3,12(,R3)                                                       
         S     R4,=F'12'                                                        
         AR    R3,R5                                                            
         SR    R4,R5                                                            
         BC    B'0010',FMG#05_NEXT_DIRREC_ENTRY                                 
*                                                                               
FMG#05_DIRREC_END_OF_BLOCK EQU *                                                
         B     FMG#05_GET_DIRREC                                                
*                                                                               
FMG#05_DIRREC_NOMORE EQU *                                                      
         L     R1,DCB_PDSDIR_PTR_F05                                            
         LA    R2,DCB_PDSDIR-DCB_PDSDIR_DSECT(,R1)                              
         MVC   G_CLOSED,G_CLOSEL                                                
         CLOSE ((R2)),MODE=31,MF=(E,G_CLOSED)                                   
*                                                                               
FMG#05_FREE EQU   *                                                             
         L     R2,DCB_PDSDIR_PTR_F05 * Get DCB memory pointer                   
         FREEMAIN RU,LV=DCB_PDSDIR_DSECT_SIZ,A=(R2) * and free it               
*                                                                               
         LA    R1,WORKF05                                                       
         MVC   0(4,R1),IFFO_F05                                                 
         XR    R15,R15                                                          
         IC    R15,=C'U'                                                        
         ST    R15,4(,R1)        * Allocate data set, no member                 
         L     R15,FMG#80A_F05   * Dynamic allocation/unallocation              
         BASR  R14,R15                                                          
*                                                                               
         IFFO_Release OBJECT=IFFO_F05,WORK=WORKF05                              
         MVC   IFFO_F05,=A(0)                                                   
*                                                                               
FMG#05_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
* PDSDIR is EOF (EODAD routine)                                                 
*                                                                               
FMG#05_EOF DS    0H                                                             
         MVI   PDSDIR_bEOF_F05,C'Y'                                             
         BR    R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK101E_F05                  DC    C'MAK101E Error opening ',X'00'              
*                                                                               
                             DS    0F                                           
FMG#80A_F05                  DC    A(FMG#80)                                    
*                                                                               
                             DS    0F                                           
CDCB_PDSDIRA_F05             DC    A(CDCB_PDSDIR)                               
CDCBE_PDSDIRA_F05            DC    A(CDCBE_PDSDIR)                              
*                                                                               
WORKDSAF05                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF05                      DS    4A                                           
*                                                                               
PARMPDSNAMEF05               DS    A                                            
PARMFILTERF05                DS    A                                            
PARMISTBRETF05               DS    A                                            
*                                                                               
IFFO_F05                     DS    A                                            
*                                                                               
DDNAME_F05                   DS    CL8,C                                        
PDSDIR_bEOF_F05              DS    C                                            
FIRSTMEM_F05                 DS    C                                            
*                                                                               
DCB_PDSDIR_PTR_F05           DS    A                                            
*                                                                               
FILTERLEN_F05                DS    F                                            
MEM_F05                      DS    CL10                                         
*                                                                               
WORKDSAF05_SIZ               EQU   *-WORKDSAF05                                 
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IFMG DDNameToDSName                                                           
*                                                                               
FMG#06   CEEENTRY AUTO=WORKDSAF06_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF06,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   PARMDDNAMEF06,4(R1) * Parm 2 is DD name                          
*                                                                               
         L     R15,8(,R1)        * Parm 3 is DS name                            
         ST    R15,PARMDSNAMEF06 * Save in local var                            
         MVI   0(R15),C' '                                                      
         MVC   1(43,R15),0(R15)                                                 
*                                                                               
         XR    R7,R7                                                            
         L     R7,PSATOLD-PSA(,R7)                                              
         L     R7,TCBTIO-TCB(,R7)                                               
         LA    R7,TIOENTRY-TIOT1(,R7)                                           
         L     R6,PARMDDNAMEF06                                                 
         DO WHILE=(CLI,0(R7),NE,X'00')                                          
            IF (CLC,TIOEDDNM-TIOENTRY(8,R7),EQ,0(R6)) THEN                      
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            XR    R15,R15                                                       
            IC    R15,0(,R7)                                                    
            AR    R7,R15                                                        
         ENDDO                                                                  
*                                                                               
         IF (CLI,0(R7),EQ,X'00') THEN                                           
            B     FMG#06_RET                                                    
         ENDIF                                                                  
*                                                                               
         LA    R5,G_EPA                                                         
         ST    R5,G_SWEPAPTR                                                    
         USING ZB505,R5                                                         
         XC    SWAEPAX,SWAEPAX                                                  
         MVC   SWVA,TIOEJFCB-TIOENTRY(R7)                                       
*                                                                               
         SWAREQ UNAUTH=YES,FCODE=RL,EPA=G_SWEPAPTR,MF=(E,G_SWAREQD)             
*                                                                               
         L     R7,SWBLKPTR                                                      
         L     R6,PARMDSNAMEF06                                                 
         MVC   0(44,R6),0(R7)                                                   
*                                                                               
FMG#06_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAF06                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF06                      DS    4A                                           
*                                                                               
PARMDDNAMEF06                DS    A                                            
PARMDSNAMEF06                DS    A                                            
*                                                                               
WORKDSAF06_SIZ               EQU   *-WORKDSAF06                                 
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* Dynamic allocation/unallocation                                               
*                                                                               
FMG#80   CEEENTRY AUTO=WORKDSAF80_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF80,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,0(,R1)         * Parm 1 is IFFO object                        
         ST    R7,IFFO_F80       * Save in local var                            
         USING FFO_obj,R7        * Address FFO object                           
*                                                                               
         MVC   FLAGS_F80,4(R1)   * Parm 2 contains flags                        
*                                                                               
         IF (CLI,FLAGS_F80+3,EQ,C'A') THEN                                      
            MVC   RETDDNAMEPTR_F80,8(R1) * Parm 3 is pointer to ddname          
         ELSE                                                                   
            MVC   RETDDNAMEPTR_F80,=A(0)                                        
         ENDIF                                                                  
*                                                                               
         IF (CLC,FMG_DYNALLOC_WORKAREA_PTR,EQ,=A(0)) THEN                       
            LA    R1,WORKF80                                                    
            MVC   0(4,R1),=A(DYNALLOC_WORKAREA_SIZ)                             
            LA    R15,FMG_DYNALLOC_WORKAREA_PTR                                 
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
         ENDIF                                                                  
*                                                                               
         L     R6,FMG_DYNALLOC_WORKAREA_PTR                                     
         USING S99RBP,R6                                                        
         LA    R4,S99RBPTR+4                                                    
         USING S99RB,R4                                                         
         ST    R4,S99RBPTR                                                      
         OI    S99RBPTR,S99RBPND                                                
         MVI   S99RB,X'00'                                                      
         MVC   S99RB+1(S99RBEND-S99RB-1),S99RB                                  
         MVI   S99RBLN,S99RBEND-S99RB                                           
*                                                                               
         IF (CLI,FLAGS_F80+3,EQ,C'A') THEN                                      
            MVI   S99VERB,S99VRBAL                                              
            OI    S99FLG11,S99MSGL0                                             
*                                                                               
            LA    R5,S99RB+(S99RBEND-S99RB)+16                                  
            MVC   0(CDSNTU_L+CMEMBERTU_L+CSTATUSTU_L+CRETDDN_L,R5),CDSNX        
               TU                                                               
*                                                                               
            MVI   6(R5),C' '                                                    
            MVC   7(43,R5),6(R5)                                                
            L     R14,FFO_fname                                                 
            L     R15,FFO_fnameLen                                              
            BCTR  R15,R0                                                        
            B     *+10                                                          
            MVC   6(1,R5),0(R14)                                                
            EX    R15,*-6                                                       
*                                                                               
            LA    R3,S99RB+(S99RBEND-S99RB)                                     
            ST    R3,S99TXTPP                                                   
            ST    R5,0(,R3)                                                     
*                                                                               
            LA    R3,4(,R3)                                                     
            IF (CLC,FFO_member,NE,=A(0)),AND,                          X        
               (CLI,FLAGS_F80+2,EQ,C'M') THEN                                   
               LA    R5,CDSNTU_L(,R5)                                           
               MVI   6(R5),C' '                                                 
               MVC   7(7,R5),6(R5)                                              
               L     R6,FFO_member                                              
               L     R15,FFO_memberLen                                          
               BCTR  R15,R0                                                     
               B     *+10                                                       
               MVC   6(1,R5),0(R6)                                              
               EX    R15,*-6                                                    
*                                                                               
               ST    R5,0(,R3)                                                  
               LA    R3,4(,R3)                                                  
*                                                                               
               LA    R5,CMEMBERTU_L(,R5)                                        
            ELSE                                                                
               LA    R5,CDSNTU_L+CMEMBERTU_L(,R5)                               
            ENDIF                                                               
            ST    R5,0(,R3)                                                     
            LA    R3,4(,R3)                                                     
*                                                                               
            LA    R5,CSTATUSTU_L(,R5)                                           
            ST    R5,0(,R3)                                                     
            OI    0(R3),X'80'                                                   
*                                                                               
            L     R1,FMG_DYNALLOC_WORKAREA_PTR                                  
            DYNALLOC                                                            
*                                                                               
            LTR   R15,R15                                                       
            IF (Z) THEN                                                         
               LA    R5,S99RBEND+16+CDSNTU_L+CMEMBERTU_L+CSTATUSTU_L            
               L     R14,RETDDNAMEPTR_F80                                       
               MVC   0(8,R14),6(R5)                                             
            ELSE                                                                
               CVD   R15,G_DEC8                                                 
               UNPK  G_ZONED8,G_DEC8                                            
               OI    G_ZONED8+7,X'F0'                                           
               MVI   G_ZONED8+8,X'00'                                           
               L     R14,G_TRT_ONLY_ZEROS                                       
               TRT   G_ZONED8(7),0(R14)                                         
               BC    7,*+8                                                      
               LA    R1,G_ZONED8+7                                              
               LR    R6,R1                                                      
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF80                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR=MAK113E_F80                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR=0(,R6)                                                      
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR==X'4000'                                                    
*                                                                               
               UNPK  G_ZONED8(9),S99RSC(5)                                      
               MVI   G_ZONED8+8,X'00'                                           
               L     R15,G_HEXTAB                                               
               TR    G_ZONED8(8),0(R15)                                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR=G_ZONED8                                                    
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR==X'4000'                                                    
*                                                                               
               L     R6,FFO_fname                                               
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR=0(,R6)                                                      
               IF (CLC,FFO_memberLen,GT,=A(0)),AND,                    X        
               (CLI,FLAGS_F80+2,EQ,C'M') THEN                                   
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,   X        
               ZSTR==X'4000'                                                    
                  L     R6,FFO_member                                           
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,   X        
               ZSTR=0(,R6)                                                      
               ENDIF                                                            
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKF80,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
               MVC   G_RETCODE,=A(8)                                            
               B     FMG#80_RET                                                 
            ENDIF                                                               
         ELSEIF (CLI,FLAGS_F80+3,EQ,C'U') THEN                                  
            MVI   S99VERB,S99VRBUN                                              
            OI    S99FLG11,S99MSGL0                                             
*                                                                               
            LA    R5,S99RB+(S99RBEND-S99RB)+16                                  
            MVC   0(CDSNTU_L+CMEMBERTU_L,R5),CDSNTU                             
*                                                                               
            MVI   6(R5),C' '                                                    
            MVC   7(43,R5),6(R5)                                                
            L     R14,FFO_fname                                                 
            L     R15,FFO_fnameLen                                              
            BCTR  R15,R0                                                        
            B     *+10                                                          
            MVC   6(1,R5),0(R14)                                                
            EX    R15,*-6                                                       
*                                                                               
            LA    R3,S99RB+(S99RBEND-S99RB)                                     
            ST    R3,S99TXTPP                                                   
            ST    R5,0(,R3)                                                     
*                                                                               
            IF (CLC,FFO_member,NE,=A(0)),AND,                          X        
               (CLI,FLAGS_F80+2,EQ,C'M') THEN                                   
               LA    R5,CDSNTU_L(,R5)                                           
               MVI   6(R5),C' '                                                 
               MVC   7(7,R5),6(R5)                                              
               L     R6,FFO_member                                              
               L     R15,FFO_memberLen                                          
               BCTR  R15,R0                                                     
               B     *+10                                                       
               MVC   6(1,R5),0(R6)                                              
               EX    R15,*-6                                                    
*                                                                               
               ST    R5,0(,R3)                                                  
            ENDIF                                                               
            OI    0(R3),X'80'                                                   
*                                                                               
            L     R1,FMG_DYNALLOC_WORKAREA_PTR                                  
            DYNALLOC                                                            
*                                                                               
            LTR   R15,R15                                                       
            IF (NZ) THEN                                                        
               CVD   R15,G_DEC8                                                 
               UNPK  G_ZONED8,G_DEC8                                            
               OI    G_ZONED8+7,X'F0'                                           
               MVI   G_ZONED8+8,X'00'                                           
               L     R14,G_TRT_ONLY_ZEROS                                       
               TRT   G_ZONED8(7),0(R14)                                         
               BC    7,*+8                                                      
               LA    R1,G_ZONED8+7                                              
               LR    R6,R1                                                      
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF80                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR=MAK114E_F80                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR=0(,R6)                                                      
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR==X'4000'                                                    
               L     R6,FFO_fname                                               
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,      X        
               ZSTR=0(,R6)                                                      
               IF (CLC,FFO_memberLen,GT,=A(0)),AND,                    X        
               (CLI,FLAGS_F80+2,EQ,C'M') THEN                                   
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,   X        
               ZSTR==X'4000'                                                    
                  L     R6,FFO_member                                           
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF80,   X        
               ZSTR=0(,R6)                                                      
               ENDIF                                                            
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKF80,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
               MVC   G_RETCODE,=A(8)                                            
               B     FMG#80_RET                                                 
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
FMG#80_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK113E_F80                  DC    C'MAK113E Dynamic allocation failed X        
               ',X'00'                                                          
                             DS    0F                                           
MAK114E_F80                  DC    C'MAK114E Dynamic deallocation faileX        
               d ',X'00'                                                        
*                                                                               
                             DS    0F                                           
CDSNTU                       DC    AL2(DALDSNAM)                                
                             DC    X'0001'                                      
                             DC    X'002C'                                      
                             DC    CL44' '                                      
CDSNTU_L                     EQU   *-CDSNTU                                     
*                                                                               
CMEMBERTU                    DC    AL2(DALMEMBR)                                
                             DC    X'0001'                                      
                             DC    X'0008'                                      
                             DC    CL8' '                                       
CMEMBERTU_L                  EQU   *-CMEMBERTU                                  
*                                                                               
CSTATUSTU                    DC    AL2(DALSTATS)                                
                             DC    X'0001'                                      
                             DC    X'0001'                                      
                             DC    X'08'                                        
CSTATUSTU_L                  EQU   *-CSTATUSTU                                  
*                                                                               
CRETDDN                      DC    AL2(DALRTDDN)                                
                             DC    X'0001'                                      
                             DC    X'0008'                                      
                             DC    CL8' '                                       
CRETDDN_L                    EQU   *-CRETDDN                                    
*                                                                               
WORKDSAF80                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF80                      DS    4A                                           
*                                                                               
IFFO_F80                     DS    A                                            
FLAGS_F80                    DS    A                                            
RETDDNAMEPTR_F80             DS    A                                            
*                                                                               
WORKDSAF80_SIZ               EQU   *-WORKDSAF80                                 
*                                                                               
DYNALLOC_WORKAREA            DSECT                                              
                             ORG   *+4  * RBP                                   
                             ORG   *+(S99RBEND-S99RB)  * RB                     
                             ORG   *+16 * TU ptr list                           
                             ORG   *+CDSNTU_L+CMEMBERTU_L+CSTATUSTU_L+CX        
               RETDDN_L                                                         
DYNALLOC_WORKAREA_SIZ        EQU   *-DYNALLOC_WORKAREA                          
*                                                                               
         IEFZB4D0                                                               
         IEFZB4D2                                                               
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* Read load module date                                                         
*                                                                               
FMG#96   CEEENTRY AUTO=WORKDSAF96_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF96,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,0(,R1)         * Parm 1 is IFFO object                        
         ST    R7,IFFO_F96       * Save in local var                            
         USING FFO_obj,R7        * Address FFO object                           
*                                                                               
         LA    R1,WORKF96                                                       
         MVC   0(4,R1),IFFO_F96                                                 
         XR    R15,R15                                                          
         IC    R15,=C'A'                                                        
         ST    R15,4(,R1)        * Allocate data set, no member                 
         LA    R15,DDNAME_F96    * Return DD name here                          
         ST    R15,8(,R1)                                                       
         L     R15,FMG#80A_F96   * Dynamic allocation/unallocation              
         BASR  R14,R15                                                          
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   FMG#96_RET                                                       
*                                                                               
         GETMAIN RU,LV=DCB_PDSBDR_DSECT_SIZ,LOC=24                              
*                                                                               
         ST    R1,DCB_PDSBDR_PTR_F96                                            
*                                                                               
         LA    R2,DCB_PDSBDR-DCB_PDSBDR_DSECT(,R1)                              
         L     R15,CDCB_PDSBDRA_F96                                             
         MVC   0(LEN_DCB_PDSBDR,R2),0(R15)                                      
*                                                                               
         MVC   DCBDDNAM-IHADCB(8,R2),DDNAME_F96                                 
*                                                                               
         MVC   G_OPEND,G_OPENL                                                  
*                                                                               
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                              
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            CVD   R15,G_DEC8                                                    
            UNPK  G_ZONED8,G_DEC8                                               
            OI    G_ZONED8+7,X'F0'                                              
            MVI   G_ZONED8+8,X'00'                                              
            L     R14,G_TRT_ONLY_ZEROS                                          
            TRT   G_ZONED8(7),0(R14)                                            
            BC    7,*+8                                                         
            LA    R1,G_ZONED8+7                                                 
            LR    R6,R1                                                         
*                                                                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK101E_F96                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=0(,R6)                                                      
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR==X'4000'                                                    
            MVI   DDNAME_F96+8,X'00'                                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=DDNAME_F96                                                  
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKF96,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
            MVC   G_RETCODE,=A(12)                                              
            B     FMG#96_FREE                                                   
         ENDIF                                                                  
*                                                                               
         MVI   FOUND_F96,C'N'                                                   
*                                                                               
         MVC   FMG_BLDLIST(2),=H'1'                                             
         MVC   FMG_BLDLIST+2(2),=H'62'                                          
         MVI   FMG_BLDL_DE,C' '                                                 
         MVC   FMG_BLDL_DE+1(L'FMG_BLDL_DE-1),FMG_BLDL_DE                       
         L     R14,FFO_member                                                   
         L     R15,FFO_memberLen                                                
         BCTR  R15,R0                                                           
         B     *+10                                                             
         MVC   FMG_BLDL_DE(1),0(R14)                                            
         EX    R15,*-6                                                          
*                                                                               
         L     R14,DCB_PDSBDR_PTR_F96                                           
         LA    R2,DCB_PDSBDR-DCB_PDSBDR_DSECT(,R14)                             
         BLDL  (R2),FMG_BLDLIST                                                 
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
*           ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                            
*           ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK311I_F96                                                 
*           L     R2,FFO_fname                                                  
*           ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=0(,R2)                                                      
*           ISTB_APpendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR==X'4D00'                                                    
*           L     R2,FFO_member                                                 
*           ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=0(,R2)                                                      
*           ISTB_APpendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR==X'5D00'                                                    
*                                                                               
*           L     R2,G_ISTB_tmp                                                 
*           L     R2,STB_lpBuf-STB_obj(,R2)                                     
*           ILOG_Write OBJECT=G_ILOG,WORK=WORKF96,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
*                                                                               
            B     FMG#96_SKIP_IEWBIND                                           
         ENDIF                                                                  
*                                                                               
         IEWBIND FUNC=STARTD,DIALOG=FMG_IEWBIND_DIALOG                          
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            STM   R15,R0,SAVER0_F96                                             
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_F96                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_STARTD_F96                                          
            BAL   R6,FMG#96_MAK115E                                             
            B     FMG#96_RET                                                    
         ENDIF                                                                  
*                                                                               
         IEWBIND FUNC=CREATEW,WORKMOD=FMG_IEWBIND_WORKMOD,             X        
               DIALOG=FMG_IEWBIND_DIALOG,INTENT=A                               
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            STM   R15,R0,SAVER0_F96                                             
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_F96                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_CREATEW_F96                                         
            BAL   R6,FMG#96_MAK115E                                             
            B     FMG#96_RET                                                    
         ENDIF                                                                  
*                                                                               
         L     R14,DCB_PDSBDR_PTR_F96                                           
         LA    R1,DCB_PDSBDR-DCB_PDSBDR_DSECT(,R14)                             
         ST    R1,FMG_IEWBIND_DCBPTR                                            
         LA    R1,FMG_BLDL_DE                                                   
         ST    R1,FMG_IEWBIND_DEPTR                                             
*                                                                               
         IEWBIND FUNC=INCLUDE,WORKMOD=FMG_IEWBIND_WORKMOD,             X        
               INTYPE=POINTER,DCBPTR=FMG_IEWBIND_DCBPTR,               X        
               DEPTR=FMG_IEWBIND_DEPTR                                          
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            STM   R15,R0,SAVER0_F96                                             
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_F96                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_INCLUDE_F96                                         
            BAL   R6,FMG#96_MAK115E                                             
            B     FMG#96_RET                                                    
         ENDIF                                                                  
*                                                                               
IEWBIDB_BASE EQU R4                      Base register for IDRB buffer.         
IDB_BASE     EQU R5                      Base register for IDRB entry.          
         IEWBUFF FUNC=GETBUF,TYPE=IDRB   Get memory for IDRB buffer.            
         IEWBUFF FUNC=INITBUF,TYPE=IDRB  Init IDRB buffer.                      
*                                                                               
         MVC   FMG_IEWBIND_CURSOR,=F'0'                                         
         MVC   FMG_IEWBIND_COUNT,=F'0'                                          
         MVC   FMG_IEWBIND_CLASS(2),=H'6'                                       
         MVC   FMG_IEWBIND_CLASS+2(6),=C'B_IDRB'                                
*                                                                               
         IEWBIND FUNC=GETD,WORKMOD=FMG_IEWBIND_WORKMOD,                X        
               CLASS=FMG_IEWBIND_CLASS,AREA=(R4),                      X        
               CURSOR=FMG_IEWBIND_CURSOR,COUNT=FMG_IEWBIND_COUNT                
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            IF (C,R15,NE,=A(4)),OR,(C,R0,NE,=XL4'83000800') THEN                
               STM   R15,R0,SAVER0_F96                                          
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,      X        
               ZSTR=MAK115E_F96                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,      X        
               ZSTR=MAK115E_GETD_F96                                            
               BAL   R6,FMG#96_MAK115E                                          
               B     FMG#96_RET                                                 
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         MVI   FOUND_F96,C'Y'                                                   
*                                                                               
         IEWBIND FUNC=DELETEW,WORKMOD=FMG_IEWBIND_WORKMOD                       
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            STM   R15,R0,SAVER0_F96                                             
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_F96                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_DELETEW_F96                                         
            BAL   R6,FMG#96_MAK115E                                             
            B     FMG#96_RET                                                    
         ENDIF                                                                  
*                                                                               
         IEWBIND FUNC=ENDD,DIALOG=FMG_IEWBIND_DIALOG                            
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            STM   R15,R0,SAVER0_F96                                             
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_F96                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,         X        
               ZSTR=MAK115E_ENDD_F96                                            
            BAL   R6,FMG#96_MAK115E                                             
            B     FMG#96_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLI,FOUND_F96,EQ,C'Y') THEN                                        
            MVI   FFO_exists,C'Y'                                               
*                                                                               
            MVI   G_CONVTOD_INAREA,X'00'                                        
            MVC   G_CONVTOD_INAREA+1(15),G_CONVTOD_INAREA                       
            PACK  G_CONVTOD_INAREA(4),IDB_TIME_BOUND(7)                         
            MVI   G_CONVTOD_INAREA+3,X'00'                                      
            PACK  G_CONVTOD_INAREA+8(5),IDB_DATE_BOUND(8)                       
            MVI   G_CONVTOD_INAREA+12,X'00'                                     
            MVI   G_CONVTOD_OUTAREA,X'00'                                       
            MVC   G_CONVTOD_OUTAREA+1(7),G_CONVTOD_OUTAREA                      
            MVI   G_STCKCONV_OUTAREA,X'00'                                      
            MVC   G_STCKCONV_OUTAREA+1(15),G_STCKCONV_OUTAREA                   
            MVC   G_CONVTODD,G_CONVTODL                                         
            CONVTOD CONVVAL=G_CONVTOD_INAREA,TODVAL=G_CONVTOD_OUTAREA, X        
               TIMETYPE=DEC,DATETYPE=YYYYDDD,MF=(E,G_CONVTODD)                  
            MVC   G_STCKCONVD,G_STCKCONVL                                       
            STCKCONV STCKVAL=G_CONVTOD_OUTAREA,                        X        
               CONVVAL=G_STCKCONV_OUTAREA,TIMETYPE=DEC,                X        
               DATETYPE=YYYYMMDD,MF=(E,G_STCKCONVD)                             
            MVC   G_DATEWORK_DEC_1(4),G_STCKCONV_OUTAREA+8                      
            MVC   G_DATEWORK_DEC_1+4(3),G_STCKCONV_OUTAREA                      
            MVO   G_DATEWORK_DEC_2,G_DATEWORK_DEC_1(7)                          
            MVN   G_DATEWORK_DEC_2+7(1),=X'0F'                                  
            UNPK  G_DATEWORK_ZON,G_DATEWORK_DEC_2                               
            MVC   FFO_alterDate(14),G_DATEWORK_ZON+2                            
            MVI   FFO_alterDate+14,X'00'                                        
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG) THEN                            
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF96                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,      X        
               ZSTR=MAK413D_F96                                                 
               L     R2,FFO_fname                                               
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,      X        
               ZSTR=0(,R2)                                                      
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,      X        
               ZSTR==X'4000'                                                    
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,      X        
               ZSTR=FFO_alterDate                                               
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKF96,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
               ENDIF                                                            
         ENDIF                                                                  
*                                                                               
         IEWBUFF FUNC=FREEBUF,TYPE=IDRB  Free IDRB buffer.                      
*                                                                               
FMG#96_SKIP_IEWBIND EQU *                                                       
*                                                                               
         DROP  R4                                                               
         DROP  R5                                                               
*                                                                               
FMG#96_CLOSE EQU   *                                                            
         L     R1,DCB_PDSBDR_PTR_F96                                            
         LA    R2,DCB_PDSBDR-DCB_PDSBDR_DSECT(,R1)                              
         MVC   G_CLOSED,G_CLOSEL                                                
         CLOSE ((R2)),MODE=31,MF=(E,G_CLOSED)                                   
*                                                                               
FMG#96_FREE EQU   *                                                             
         L     R2,DCB_PDSBDR_PTR_F96 * Get DCB memory pointer                   
         FREEMAIN RU,LV=DCB_PDSBDR_DSECT_SIZ,A=(R2) * and free it               
*                                                                               
         LA    R1,WORKF96                                                       
         MVC   0(4,R1),IFFO_F96                                                 
         XR    R15,R15                                                          
         IC    R15,=C'U'                                                        
         ST    R15,4(,R1)        * Allocate data set, no member                 
         L     R15,FMG#80A_F96   * Dynamic allocation/unallocation              
         BASR  R14,R15                                                          
*                                                                               
FMG#96_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
* Construct rest of MAK115E message                                             
*                                                                               
FMG#96_MAK115E DS    0H                                                         
         L     R15,SAVER15_F96                                                  
         CVD   R15,G_DEC8                                                       
         UNPK  G_ZONED8,G_DEC8                                                  
         OI    G_ZONED8+7,X'F0'                                                 
         MVI   G_ZONED8+8,X'00'                                                 
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,            X        
               ZSTR=G_ZONED8                                                    
*                                                                               
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,            X        
               ZSTR==X'4000'                                                    
*                                                                               
         MVC   G_DEC8,SAVER0_F96                                                
         UNPK  G_ZONED8(9),G_DEC8(5)                                            
         L     R15,G_HEXTAB                                                     
         TR    G_ZONED8(8),0(R15)                                               
         MVI   G_ZONED8+8,X'00'                                                 
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,            X        
               ZSTR=G_ZONED8                                                    
*                                                                               
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,            X        
               ZSTR==X'4000'                                                    
*                                                                               
         L     R2,FFO_fname                                                     
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,ZSTR=0(,R2)          
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,            X        
               ZSTR==X'4D00'                                                    
         L     R2,FFO_member                                                    
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,ZSTR=0(,R2)          
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF96,            X        
               ZSTR==X'5D00'                                                    
*                                                                               
         L     R2,G_ISTB_tmp                                                    
         L     R2,STB_lpBuf-STB_obj(,R2)                                        
         ILOG_Write OBJECT=G_ILOG,WORK=WORKF96,LINE=0(,R2),            X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
         MVC   G_RETCODE,=A(12)                                                 
*                                                                               
         BR    R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK101E_F96                  DC    C'MAK101E Error opening ',X'00'              
                             DS    0F                                           
MAK115E_F96                  DC    C'MAK115E Binder error ',X'00'               
                             DS    0F                                           
MAK115E_STARTD_F96           DC    C'STARTD ',X'00'                             
                             DS    0F                                           
MAK115E_CREATEW_F96          DC    C'CREATEW ',X'00'                            
                             DS    0F                                           
MAK115E_INCLUDE_F96          DC    C'INCLUDE ',X'00'                            
                             DS    0F                                           
MAK115E_GETD_F96             DC    C'GETD ',X'00'                               
                             DS    0F                                           
MAK115E_DELETEW_F96          DC    C'DELETEW ',X'00'                            
                             DS    0F                                           
MAK115E_ENDD_F96             DC    C'ENDD ',X'00'                               
                             DS    0F                                           
MAK311I_F96                  DC    C'MAK311I Member not found ',X'00'           
                             DS    0F                                           
MAK413D_F96                  DC    C'MAK413D Load module date of ',X'00X        
               '                                                                
*                                                                               
                             DS    0F                                           
FMG#80A_F96                  DC    A(FMG#80)                                    
*                                                                               
                             DS    0F                                           
CDCB_PDSBDRA_F96             DC    A(CDCB_PDSBDR)                               
*                                                                               
IDBBUF                       IEWBUFF FUNC=MAPBUF,TYPE=IDRB,VERSION=6,BYX        
               TES=2048                                                         
*                                                                               
WORKDSAF96                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF96                      DS    4A                                           
*                                                                               
IFFO_F96                     DS    A                                            
*                                                                               
DDNAME_F96                   DS    CL8,C                                        
*                                                                               
DCB_PDSBDR_PTR_F96           DS    A                                            
*                                                                               
MEM_F96                      DS    CL8                                          
*                                                                               
SAVER0_F96                   DS    F                                            
SAVER15_F96                  DS    F                                            
*                                                                               
FOUND_F96                    DS    C                                            
*                                                                               
WORKDSAF96_SIZ               EQU   *-WORKDSAF96                                 
*                                                                               
DCB_PDSBDR_DSECT             DSECT                                              
*                                                                               
* DCB for any PDS dynamically allocated for reading load module                 
DCB_PDSBDR                   DS    0F                                           
                             ORG   *+LEN_DCB_PDSBDR                             
*                                                                               
DCB_PDSBDR_DSECT_SIZ         EQU   *-DCB_PDSBDR_DSECT                           
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* Find member stats in PDS directory                                            
*                                                                               
FMG#97   CEEENTRY AUTO=WORKDSAF97_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF97,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,0(,R1)         * Parm 1 is IFFO object                        
         ST    R7,IFFO_F97       * Save in local var                            
         USING FFO_obj,R7        * Address FFO object                           
*                                                                               
         LA    R1,WORKF97                                                       
         MVC   0(4,R1),IFFO_F97                                                 
         XR    R15,R15                                                          
         IC    R15,=C'A'                                                        
         ST    R15,4(,R1)        * Allocate data set, no member                 
         LA    R15,DDNAME_F97    * Return DD name here                          
         ST    R15,8(,R1)                                                       
         L     R15,FMG#80A_F97   * Dynamic allocation/unallocation              
         BASR  R14,R15                                                          
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   FMG#97_RET                                                       
*                                                                               
         GETMAIN RU,LV=DCB_PDSDIR_DSECT_SIZ,LOC=24                              
*                                                                               
         ST    R1,DCB_PDSDIR_PTR_F97                                            
*                                                                               
         LA    R2,DCB_PDSDIR-DCB_PDSDIR_DSECT(,R1)                              
         L     R15,CDCB_PDSDIRA_F97                                             
         MVC   0(LEN_DCB_PDSDIR,R2),0(R15)                                      
         LA    R3,DCBE_PDSDIR-DCB_PDSDIR_DSECT(,R1)                             
         L     R15,CDCBE_PDSDIRA_F97                                            
         MVC   0(LEN_DCBE_PDSDIR,R3),0(R15)                                     
*                                                                               
         ST    R3,DCBDCBE-IHADCB(,R2)                                           
*                                                                               
         LA    R4,FMG#97_EOF                                                    
         ST    R4,DCBEEODA-DCBE(,R3)                                            
*                                                                               
         MVC   DCBDDNAM-IHADCB(8,R2),DDNAME_F97                                 
*                                                                               
         MVC   G_OPEND,G_OPENL                                                  
*                                                                               
         OPEN  ((R2),INPUT),MODE=31,MF=(E,G_OPEND)                              
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            CVD   R15,G_DEC8                                                    
            UNPK  G_ZONED8,G_DEC8                                               
            OI    G_ZONED8+7,X'F0'                                              
            MVI   G_ZONED8+8,X'00'                                              
            L     R14,G_TRT_ONLY_ZEROS                                          
            TRT   G_ZONED8(7),0(R14)                                            
            BC    7,*+8                                                         
            LA    R1,G_ZONED8+7                                                 
            LR    R6,R1                                                         
*                                                                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF97                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,         X        
               ZSTR=MAK101E_F97                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,         X        
               ZSTR=0(,R6)                                                      
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,         X        
               ZSTR==X'4000'                                                    
            MVI   DDNAME_F97+8,X'00'                                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,         X        
               ZSTR=DDNAME_F97                                                  
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKF97,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
            MVC   G_RETCODE,=A(12)                                              
            B     FMG#97_FREE                                                   
         ENDIF                                                                  
*                                                                               
         MVI   PDSDIR_bEOF_F97,C'N'                                             
         MVI   FOUND_F97,C'N'                                                   
*                                                                               
         MVI   MEM_F97,C' '                                                     
         MVC   MEM_F97+1(7),MEM_F97                                             
         LA    R2,MEM_F97                                                       
         L     R3,FFO_memberLen                                                 
         L     R4,FFO_member                                                    
         BCTR  R3,R0                                                            
         B     *+10                                                             
         MVC   0(1,R2),0(R4)                                                    
         EX    R3,*-6                                                           
*                                                                               
FMG#97_GET_DIRREC EQU *                                                         
         L     R1,DCB_PDSDIR_PTR_F97                                            
         LA    R2,DCB_PDSDIR-DCB_PDSDIR_DSECT(,R1)                              
         LA    R6,FMG#97_DIRREC_NOMORE                                          
         GET   (R2),FMG_DIRREC                                                  
*                                                                               
         LA    R3,FMG_DIRREC                                                    
         XR    R4,R4                                                            
         LH    R4,0(,R3)                                                        
         C     R4,=F'14'                                                        
         BL    FMG#97_DIRREC_END_OF_BLOCK                                       
         LA    R3,2(,R3)                                                        
         S     R4,=F'2'                                                         
FMG#97_NEXT_DIRREC_ENTRY EQU *                                                  
         CLC   0(8,R3),=8X'FF'                                                  
         BE    FMG#97_DIRREC_NOMORE                                             
         CLC   0(8,R3),MEM_F97                                                  
         IF (EQ) THEN                                                           
            MVI   FOUND_F97,C'Y'                                                
            MVI   FFO_exists,C'Y'                                               
*                                                                               
            L     R5,8(,R3)                                                     
            N     R5,=X'0000001F'                                               
            SLL   R5,1                                                          
            C     R5,=F'30'                                                     
            IF (NL) THEN                                                        
               MVI   G_CONVTOD_INAREA,X'00'                                     
               MVC   G_CONVTOD_INAREA+1(15),G_CONVTOD_INAREA                    
               MVC   G_CONVTOD_INAREA(2),24(R3)                                 
               MVC   G_CONVTOD_INAREA+2(1),15(R3)                               
               MVC   G_CONVTOD_INAREA+8(4),20(R3)                               
               MVI   G_CONVTOD_OUTAREA,X'00'                                    
               MVC   G_CONVTOD_OUTAREA+1(7),G_CONVTOD_OUTAREA                   
               MVI   G_STCKCONV_OUTAREA,X'00'                                   
               MVC   G_STCKCONV_OUTAREA+1(15),G_STCKCONV_OUTAREA                
               MVC   G_CONVTODD,G_CONVTODL                                      
               CONVTOD CONVVAL=G_CONVTOD_INAREA,                       X        
               TODVAL=G_CONVTOD_OUTAREA,TIMETYPE=DEC,DATETYPE=YYDDD,   X        
               MF=(E,G_CONVTODD)                                                
               MVC   G_STCKCONVD,G_STCKCONVL                                    
               STCKCONV STCKVAL=G_CONVTOD_OUTAREA,                     X        
               CONVVAL=G_STCKCONV_OUTAREA,TIMETYPE=DEC,                X        
               DATETYPE=YYYYMMDD,MF=(E,G_STCKCONVD)                             
*                                                                               
               MVC   G_DATEWORK_DEC_1(4),G_STCKCONV_OUTAREA+8                   
               MVC   G_DATEWORK_DEC_1+4(3),G_STCKCONV_OUTAREA                   
               MVO   G_DATEWORK_DEC_2,G_DATEWORK_DEC_1(7)                       
               MVN   G_DATEWORK_DEC_2+7(1),=X'0F'                               
               UNPK  G_DATEWORK_ZON,G_DATEWORK_DEC_2                            
               MVC   FFO_alterDate(14),G_DATEWORK_ZON+2                         
               MVI   FFO_alterDate+14,X'00'                                     
*                                                                               
               L     R15,G_ILOG                                                 
               IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG) THEN                         
                  ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF97                      
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,   X        
               ZSTR=MAK412D_F97                                                 
                  L     R2,FFO_fname                                            
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,   X        
               ZSTR=0(,R2)                                                      
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,   X        
               ZSTR==X'4000'                                                    
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,   X        
               ZSTR=FFO_alterDate                                               
*                                                                               
                  L     R2,G_ISTB_tmp                                           
                  L     R2,STB_lpBuf-STB_obj(,R2)                               
                  ILOG_Write OBJECT=G_ILOG,WORK=WORKF97,LINE=0(,R2),   X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
               ENDIF                                                            
            ELSE                                                                
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF97                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,      X        
               ZSTR=MAK201W_F97                                                 
               L     R6,FFO_fname                                               
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,      X        
               ZSTR=0(,R6)                                                      
               IF (CLC,FFO_memberLen,GT,=A(0)) THEN                             
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,   X        
               ZSTR==X'4D00'                                                    
                  L     R6,FFO_member                                           
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,   X        
               ZSTR=0(,R6)                                                      
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF97,   X        
               ZSTR==X'5D00'                                                    
               ENDIF                                                            
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKF97,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_WARNING                                       
            ENDIF                                                               
            B     FMG#97_DIRREC_NOMORE                                          
         ELSE                                                                   
            L     R5,8(,R3)                                                     
            N     R5,=X'0000001F'                                               
            SLL   R5,1                                                          
            LA    R3,12(,R3)                                                    
            S     R4,=F'12'                                                     
            AR    R3,R5                                                         
            SR    R4,R5                                                         
            BC    B'0010',FMG#97_NEXT_DIRREC_ENTRY                              
         ENDIF                                                                  
*                                                                               
FMG#97_DIRREC_END_OF_BLOCK EQU *                                                
         B     FMG#97_GET_DIRREC                                                
*                                                                               
FMG#97_DIRREC_NOMORE EQU *                                                      
*                                                                               
         L     R1,DCB_PDSDIR_PTR_F97                                            
         LA    R2,DCB_PDSDIR-DCB_PDSDIR_DSECT(,R1)                              
         MVC   G_CLOSED,G_CLOSEL                                                
         CLOSE ((R2)),MODE=31,MF=(E,G_CLOSED)                                   
*                                                                               
FMG#97_FREE EQU   *                                                             
         L     R2,DCB_PDSDIR_PTR_F97 * Get DCB memory pointer                   
         FREEMAIN RU,LV=DCB_PDSDIR_DSECT_SIZ,A=(R2) * and free it               
*                                                                               
         LA    R1,WORKF97                                                       
         MVC   0(4,R1),IFFO_F97                                                 
         XR    R15,R15                                                          
         IC    R15,=C'U'                                                        
         ST    R15,4(,R1)        * Allocate data set, no member                 
         L     R15,FMG#80A_F97   * Dynamic allocation/unallocation              
         BASR  R14,R15                                                          
*                                                                               
FMG#97_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
* PDSDIR is EOF (EODAD routine)                                                 
*                                                                               
FMG#97_EOF DS    0H                                                             
         MVI   PDSDIR_bEOF_F97,C'Y'                                             
         BR    R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK101E_F97                  DC    C'MAK101E Error opening ',X'00'              
                             DS    0F                                           
MAK412D_F97                  DC    C'MAK412D Last altered date of ',X'0X        
               0'                                                               
                             DS    0F                                           
MAK201W_F97                  DC    C'MAK201W Member without statistics X        
               ',X'00'                                                          
*                                                                               
                             DS    0F                                           
FMG#80A_F97                  DC    A(FMG#80)                                    
*                                                                               
                             DS    0F                                           
CDCB_PDSDIRA_F97             DC    A(CDCB_PDSDIR)                               
CDCBE_PDSDIRA_F97            DC    A(CDCBE_PDSDIR)                              
*                                                                               
WORKDSAF97                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF97                      DS    4A                                           
*                                                                               
IFFO_F97                     DS    A                                            
*                                                                               
DDNAME_F97                   DS    CL8,C                                        
*                                                                               
DCB_PDSDIR_PTR_F97           DS    A                                            
*                                                                               
MEM_F97                      DS    CL8                                          
*                                                                               
PDSDIR_bEOF_F97              DS    C                                            
FOUND_F97                    DS    C                                            
*                                                                               
WORKDSAF97_SIZ               EQU   *-WORKDSAF97                                 
*                                                                               
DCB_PDSDIR_DSECT             DSECT                                              
*                                                                               
* DCB for any PDS dynamically allocated for reading directory                   
DCB_PDSDIR                   DS    0F                                           
                             ORG   *+LEN_DCB_PDSDIR                             
*                                                                               
* DCBE for any PDS dynamically allocated for reading directory                  
DCBE_PDSDIR                  EQU   *                                            
                             ORG   *+LEN_DCBE_PDSDIR                            
*                                                                               
DCB_PDSDIR_DSECT_SIZ         EQU   *-DCB_PDSDIR_DSECT                           
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IFMG CAMLST OBTAIN                                                            
*                                                                               
FMG#98   CEEENTRY AUTO=WORKDSAF98_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF98,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,0(,R1)         * Parm 1 is IFFO object                        
         ST    R7,IFFO_F98       * Save in local var                            
         USING FFO_obj,R7        * Address FFO object                           
*                                                                               
         LA    R2,FMG_CAMLST_DSNAME                                             
         MVI   0(R2),C' '                                                       
         MVC   1(43,R2),0(R2)                                                   
         L     R4,FFO_fname                                                     
         L     R3,FFO_fnameLen                                                  
         BCTR  R3,R0                                                            
         B     *+10                                                             
         MVC   0(1,R2),0(R4)                                                    
         EX    R3,*-6                                                           
*                                                                               
         MVC   FMG_CAMLST_VOLSER,FFO_volser                                     
*                                                                               
         LA    R1,FMG_CAMLST_PARM                                               
         XR    R15,R15                                                          
         ICM   R15,B'1000',=AL1(193)                                            
         ST    R15,0(,R1)                                                       
         LA    R15,FMG_CAMLST_DSNAME                                            
         ST    R15,4(,R1)                                                       
         LA    R15,FMG_CAMLST_VOLSER                                            
         ST    R15,8(,R1)                                                       
         LA    R15,FMG_CAMLST_WORKAREA                                          
         ST    R15,12(,R1)                                                      
*                                                                               
         OBTAIN FMG_CAMLST_PARM,NUMBERDSCB=1,EADSCB=OK                          
*                                                                               
         LTR   R15,R15                                                          
         IF (NZ) THEN                                                           
            CVD   R15,G_DEC8                                                    
            UNPK  G_ZONED8,G_DEC8                                               
            OI    G_ZONED8+7,X'F0'                                              
            MVI   G_ZONED8+8,X'00'                                              
            L     R14,G_TRT_ONLY_ZEROS                                          
            TRT   G_ZONED8(7),0(R14)                                            
            BC    7,*+8                                                         
            LA    R1,G_ZONED8+7                                                 
            LR    R7,R1                                                         
*                                                                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF98                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF98,         X        
               ZSTR=MAK111E_F98                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF98,         X        
               ZSTR=0(,R7)                                                      
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKF98,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
            MVC   G_RETCODE,=A(12)                                              
            B     FMG#98_RET                                                    
         ENDIF                                                                  
*                                                                               
         LA    R2,FMG_CAMLST_WORKAREA                                           
         USING DS1FMTID,R2                                                      
         MVC   FFO_recfm,DS1RECFM                                               
         MVC   FFO_dsorg,DS1DSORG                                               
         DROP  R2                                                               
*                                                                               
FMG#98_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK111E_F98                  DC    C'MAK111E CAMLST OBTAIN error ',X'00X        
               '                                                                
*                                                                               
*DSCBABC  CAMLST SEARCH,DSABC,VOLNUM,WORKAREA                                   
*DSABC    DC    CL44'LWZM010.SAMPLE07.ASM'                                      
*VOLNUM   DC    CL6'YINS01'                                                     
*WORKAREA DS    140C                                                            
*                                                                               
WORKDSAF98                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF98                      DS    4A                                           
*                                                                               
IFFO_F98                     DS    A                                            
*                                                                               
WORKDSAF98_SIZ               EQU   *-WORKDSAF98                                 
*                                                                               
OBTAIN_DSECT                 DSECT                                              
                             IECSDSL1 1                                         
OBTAIN_DSECT_SIZ             EQU   *-OBTAIN_DSECT                               
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IFMG IGGCSI00 catalog search                                                  
*                                                                               
FMG#99   CEEENTRY AUTO=WORKDSAF99_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAF99,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING FMG_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,0(,R1)         * Parm 1 is IFFO object                        
         ST    R7,IFFO_F99       * Save in local var                            
         USING FFO_obj,R7        * Address FFO object                           
*                                                                               
         IF (CLC,G_IGGCSI00A,EQ,=A(0)) THEN                                     
            MVC   G_LOADD,G_LOADL                                               
*                                                                               
*                                * Load catalog search interface                
            LOAD  EP=IGGCSI00,SF=(E,G_LOADD) * entry point IGGCSI00             
*           No error handling, missing module will cause S806 abend             
*                                                                               
            ST    R0,G_IGGCSI00A * and store in global var                      
*                                                                               
            LA    R1,WORKF99                                                    
            MVC   0(4,R1),=A(1028)                                              
            LA    R15,FMG_DAREA_PTR                                             
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            L     R15,FMG_DAREA_PTR                                             
            MVC   0(4,R15),=A(1024)                                             
         ENDIF                                                                  
*                                                                               
         LA    R2,FMG_CSIFIELD                                                  
         L     R3,=A(CSIFIELD_SIZ)                                              
         LA    R14,CONST_CSIFIELD                                               
         LR    R15,R3                                                           
         MVCL  R2,R14                                                           
*                                                                               
         LA    R2,FMG_CSIFIELD                                                  
         L     R4,FFO_fname                                                     
         L     R3,FFO_fnameLen                                                  
         BCTR  R3,R0                                                            
         B     *+10                                                             
         MVC   CSIFILTK-CSIFIELD_DSECT(1,R2),0(R4)                              
         EX    R3,*-6                                                           
*                                                                               
         LA    R1,WORKF99                                                       
         LA    R15,FMG_MODRSNRT                                                 
         ST    R15,0(,R1)                                                       
         LA    R15,FMG_CSIFIELD                                                 
         ST    R15,4(,R1)                                                       
         MVC   8(4,R1),FMG_DAREA_PTR                                            
         OI    8(R1),X'80'                                                      
         L     R15,G_IGGCSI00A                                                  
         BASR  R14,R15                                                          
*                                                                               
         IF (C,R15,LE,=A(4)) THEN                                               
            L     R1,FMG_DAREA_PTR                                              
            IF (CLC,8(4,R1),GT,=A(64)) THEN                                     
               LA    R2,1                                                       
*                                                                               
               MVC   FFO_volser,116(R1)                                         
            ELSE                                                                
               XR    R2,R2                                                      
            ENDIF                                                               
         ELSE                                                                   
            CVD   R15,G_DEC8                                                    
            UNPK  G_ZONED8,G_DEC8                                               
            OI    G_ZONED8+7,X'F0'                                              
            MVI   G_ZONED8+8,X'00'                                              
            L     R14,G_TRT_ONLY_ZEROS                                          
            TRT   G_ZONED8(7),0(R14)                                            
            BC    7,*+8                                                         
            LA    R1,G_ZONED8+7                                                 
            LR    R7,R1                                                         
*                                                                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKF99                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF99,         X        
               ZSTR=MAK108E_F99                                                 
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF99,         X        
               ZSTR=0(,R7)                                                      
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF99,         X        
               ZSTR==X'4000'                                                    
*                                                                               
            L     R15,FMG_MODRSNRT                                              
            CVD   R15,G_DEC8                                                    
            UNPK  G_ZONED8,G_DEC8                                               
            OI    G_ZONED8+7,X'F0'                                              
            MVI   G_ZONED8+8,X'00'                                              
            L     R14,G_TRT_ONLY_ZEROS                                          
            TRT   G_ZONED8(7),0(R14)                                            
            BC    7,*+8                                                         
            LA    R1,G_ZONED8+7                                                 
            LR    R7,R1                                                         
*                                                                               
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKF99,         X        
               ZSTR=0(,R7)                                                      
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKF99,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
            CALL  CEE3ABD,(=A(1007),=A(3)),MF=(E,WORKF99)                       
         ENDIF                                                                  
*                                                                               
FMG#99_RET EQU   *                                                              
         CEETERM RC=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
CONST_CSIFIELD               DS    0F                                           
         DC    CL44' '        CSIFILTK FILTER   KEY                             
         DC    CL44' '        CSICATNM CATALOG NAME OR BLANKS                   
         DC    CL44' '        CSIRESNM RESUME NAME OR BLANKS                    
         DS    0CL16          CSIDTYPD ENTRY TYPES                              
         DC    CL16'                ' CSIDTYPS                                  
         DS    0CL4           CSIOPTS  CSI OPTIONS                              
         DC    CL1'Y'         CSICLDI  RETURN D&I IF C A MATCH Y OR ' '         
         DC    CL1' '         CSIRESUM RESUME FLAG             Y OR ' '         
         DC    CL1'Y'         CSIS1CAT SEARCH CATALOG          Y OR ' '         
         DC    XL1'00'        CSIRESRV RESERVED                                 
         DC    H'1'           CSINUMEN NUMBER OF ENTRIES FOLLOWING              
         DS    0CL8           CSIENTS  VARIABLE NUMBER OF ENTRIES               
         DC    CL8'VOLSER  '  CSIFLDNM FIELD NAME                               
CSIFIELD_SIZ                 EQU   *-CONST_CSIFIELD                             
*                                                                               
                             DS    0F                                           
MAK108E_F99                  DC    C'MAK108E IGGCSI00 Catalog search erX        
               ror ',X'00'                                                      
*                                                                               
WORKDSAF99                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKF99                      DS    4A                                           
*                                                                               
IFFO_F99                     DS    A                                            
*                                                                               
WORKDSAF99_SIZ               EQU   *-WORKDSAF99                                 
*                                                                               
* DSECT for IGGCSI00 catalog search interface                                   
CSIFIELD_DSECT               DSECT                                              
CSIFILTK                     DS    CL44    FILTER   KEY                         
CSICATNM                     DS    CL44    CATALOG NAME OR BLANKS               
CSIRESNM                     DS    CL44    RESUME NAME OR BLANKS                
CSIDTYPD                     DS    0CL16   ENTRY TYPES                          
CSIDTYPS                     DS    CL16                                         
CSIOPTS                      DS    0CL4    CSI OPTIONS                          
CSICLDI                      DS    CL1     RETURN DATA OR INDEX                 
CSIRESUM                     DS    CL1     RESUME FLAG                          
CSIS1CAT                     DS    CL1     SEARCH CATALOG                       
CSIRESRV                     DS    XL1     RESERVED                             
CSINUMEN                     DS    H       NUMBER OF ENTRIES FOLLOWING          
CSIENTS                      DS    0CL8    VARIABLE NUMBER OF ENTRIES           
CSIFLDNM                     DS    CL8     FIELD NAME                           
CSIFIELD_DSECT_SIZ           EQU   *-CSIFIELD_DSECT                             
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IFFO QueryInterface                                                           
*                                                                               
FFO#01   MQRYIFCE SUF=I01,IFACE=IFFO                                            
*                                                                               
* IFFO AddRef                                                                   
*                                                                               
FFO#02   MADDREF                                                                
*                                                                               
* IFFO Release                                                                  
*                                                                               
FFO#03   MRELEASE SUF=I03,OBJ=FFO                                               
*                                                                               
LWZMFMG  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
CDCB_PDSDIR                  DCB   LRECL=256,BLKSIZE=256,MACRF=(GM),DEVX        
               D=DA,DSORG=PS,RECFM=F,DCBE=CDCBE_PDSDIR                          
LEN_DCB_PDSDIR               EQU   *-CDCB_PDSDIR                                
*                                                                               
CDCBE_PDSDIR                 DCBE  EODAD=0,RMODE31=BUFF                         
LEN_DCBE_PDSDIR              EQU   *-CDCBE_PDSDIR                               
*                                                                               
CDCB_PDSBDR                  DCB   MACRF=R,DSORG=PO,RECFM=U                     
LEN_DCB_PDSBDR               EQU   *-CDCB_PDSBDR                                
*                                                                               
         COPY  REGS              * Register equates                             
*                                                                               
         END   LWZMFMG                                                          
