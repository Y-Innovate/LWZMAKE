*---------------------------------------------------------------------*         
* Program    : LWZMSTR                                                *         
* Description: COM object for String and StringBuilder                *         
*---------------------------------------------------------------------*         
         TITLE 'LWZMSTR'                                                        
*                                                                               
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's             
*                                                                               
         COPY  IFACES            * Object interfaces                            
*                                                                               
         COPY  MINSTANT          * Macro to instantiate new object              
*                                                                               
* Main routine creates a new STR or STB object                                  
*                                                                               
LWZMSTR  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10                             
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
*        Was a new STB object requested?                                        
         IF (CLC,0(16,R6),EQ,G_ISTR_GUID) THEN                                  
            MNEWOBJ OBJTYPE=STR,WORK=WORK * Alloc new object                    
*                                                                               
*           Init obj attributes                                                 
            USING STR_obj,R14                                                   
            DROP  R14                                                           
*                                                                               
            ASI   G_OBJCOUNT,1   * Increate global obj count                    
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN                           
               ST    R14,G_DEC8                                                 
               UNPK  G_ZONED8(9),G_DEC8(5)                                      
               L     R15,G_HEXTAB                                               
               TR    G_ZONED8(8),0(R15)                                         
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK                            
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=MAK501D_STR                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=G_ZONED8                                                    
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
            ENDIF                                                               
*                                                                               
*        Was a new STB object requested?                                        
         ELSEIF (CLC,0(16,R6),EQ,G_ISTB_GUID) THEN                              
            MNEWOBJ OBJTYPE=STB,WORK=WORK * Alloc new object                    
*                                                                               
*           Init obj attributes                                                 
            LR    R4,R14                                                        
            USING STB_obj,R4                                                    
*                                                                               
            LA    R1,WORK                                                       
            ST    R4,0(,R1)                                                     
            L     R15,STB#04A    * Call Init during instantiate                 
            BASR  R14,R15                                                       
*                                                                               
            DROP  R4                                                            
*                                                                               
            ASI   G_OBJCOUNT,1   * Increate global obj count                    
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN                           
               ST    R4,G_DEC8                                                  
               UNPK  G_ZONED8(9),G_DEC8(5)                                      
               L     R15,G_HEXTAB                                               
               TR    G_ZONED8(8),0(R15)                                         
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORK                            
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=MAK501D_STB                                                 
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
LWZMSTR_RET EQU   *                                                             
         CEETERM                 * Return to caller                             
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
STR#01A                      DC    A(STR#01)   * QueryInterface                 
STR#02A                      DC    A(STR#02)   * AddRef                         
STR#03A                      DC    A(STR#03)   * Release                        
STR#04A                      DC    A(STR#04)   * Set                            
STR#05A                      DC    A(STR#05)   * Equals                         
STR#06A                      DC    A(STR#06)   * EqualsZStr                     
STR#07A                      DC    A(STR#07)   * CharAt                         
STR#08A                      DC    A(STR#08)   * ToUpperCase                    
STR#09A                      DC    A(STR#09)   * ToLowerCase                    
*                                                                               
                             DS    0F                                           
STB#01A                      DC    A(STB#01)   * QueryInterface                 
STB#02A                      DC    A(STB#02)   * AddRef                         
STB#03A                      DC    A(STB#03)   * Release                        
STB#04A                      DC    A(STB#04)   * Init                           
STB#05A                      DC    A(STB#05)   * Equals                         
STB#06A                      DC    A(STB#06)   * EqualsZStr                     
STB#07A                      DC    A(STB#07)   * CharAt                         
STB#08A                      DC    A(STB#08)   * ToUpperCase                    
STB#09A                      DC    A(STB#09)   * ToLowerCase                    
STB#10A                      DC    A(STB#10)   * AppendChar                     
STB#11A                      DC    A(STB#11)   * AppendString                   
STB#12A                      DC    A(STB#12)   * AppendZString                  
STB#13A                      DC    A(STB#13)   * ToString                       
*                                                                               
                             DS    0F                                           
MAK501D_STR                  DC    C'MAK501D Created ISTR object '              
                             DC    X'00'                                        
                             DS    0F                                           
MAK501D_STB                  DC    C'MAK501D Created ISTB object '              
                             DC    X'00'                                        
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
         COPY  DSSTR             * ISTR obj DSECT                               
         COPY  DSSTB             * ISTB obj DSECT                               
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTR QueryInterface                                                           
*                                                                               
STR#01   MQRYIFCE SUF=S01,IFACE=ISTR                                            
*                                                                               
* ISTR AddRef                                                                   
*                                                                               
STR#02   MADDREF                                                                
*                                                                               
* ISTR Release                                                                  
*                                                                               
STR#03   CEEENTRY AUTO=WORKDSAS03_SIZ,MAIN=NO,BASE=(R10)                        
*                                                                               
         USING WORKDSAS03,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global DSECT                         
*                                                                               
         L     R6,0(,R1)         * Param 1 points to this object                
         USING COM_obj,R6                                                       
*                                                                               
         LT    R5,count          * Load current ref count                       
         BZ    STR#03_RET        * Should never happen....                      
         S     R5,=A(1)          * Decrease ref count                           
         ST    R5,count          * Put new ref count back                       
*                                                                               
*        If reference count dropped to 0, object can be freed                   
         IF (Z) THEN                                                            
            DROP  R6                                                            
            USING STR_obj,R6                                                    
*                                                                               
            IF (CLC,STR_lpString,NE,=A(0)) THEN                                 
*              CALL  CEEFRST,(STR_lpString,FCS03),MF=(E,WORKS03)                
               MVC   STR_lpString,=A(0)                                         
            ENDIF                                                               
*                                                                               
            DROP  R6                                                            
            USING COM_obj,R6                                                    
*                                                                               
            MVC   lpVtbl,=A(0)                                                  
*                                                                               
*           L     R5,lpVtbl      * Get ptr to Vtbl                              
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher              
*           ST    R5,OBJPTRS03   * Put ptr in variable                          
*           CALL  CEEFRST,(OBJPTRS03,FCS03),MF=(E,WORKS03)                      
*                                                                               
            L     R15,G_OBJCOUNT                                                
            BCTR  R15,R0                                                        
            ST    R15,G_OBJCOUNT                                                
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2) THEN                           
               ST    R6,G_DEC8                                                  
               UNPK  G_ZONED8(9),G_DEC8(5)                                      
               L     R15,G_HEXTAB                                               
               TR    G_ZONED8(8),0(R5)                                          
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKS03                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS03,      X        
               ZSTR=MAK502D_STR                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS03,      X        
               ZSTR=G_ZONED8                                                    
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKS03,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
STR#03_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK502D_STR                  DC    C'MAK502D Deleted ISTR object '              
                             DC    X'00'                                        
*                                                                               
WORKDSAS03                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
OBJPTRS03                    DS    A                                            
WORKS03                      DS    3A                                           
*                                                                               
WORKDSAS03_SIZ               EQU   *-WORKDSAS03                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTR Set                                                                      
*                                                                               
STR#04   CEEENTRY AUTO=WORKDSAS04_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS04,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STR_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   NEWSTRS04,4(R1)   * Parm 2 is zero terminated str ptr            
*                                                                               
         XR    R0,R0             * Search for terminating zero                  
         L     R2,NEWSTRS04      * Point R2 to incoming zero str                
         LR    R1,R2             * Point R1 to next byte of str to srch         
         LA    R2,4095(,R1)      * Search for max 4K                            
         SRST  R2,R1             * Scan for zero terminator                     
         BRC   1,*-4             * Scan was incomplete, try again               
         BRC   2,*-14            * Not found, try another 4K                    
*                                                                               
         L     R1,NEWSTRS04      * Point R1 to incoming zero str                
         SR    R2,R1             * Calculate length                             
         ST    R2,STR_nStrLen    * Save new length                              
         LA    R2,1(,R2)         * Add 1 for zero term                          
         ST    R2,NEWSTRSIZS04   * And save it for GTST                         
*                                                                               
         LA    R1,WORKS04                                                       
         ST    R2,0(,R1)                                                        
         LA    R15,STR_lpString                                                 
         ST    R15,4(,R1)                                                       
         L     R15,G_GTST                                                       
         BASR  R14,R15                                                          
*                                                                               
         XR    R0,R0             * Move until zero terminator                   
         L     R1,STR_lpString   * Point R1 to new allocated buffer             
         L     R2,NEWSTRS04      * Point R2 to incoming zero str                
MOVECONT MVST  R1,R2             * Move string                                  
         BC    1,MOVECONT        * Move was incomplete, try again               
         MVI   0(R1),X'00'       * Add zero terminator                          
*                                                                               
STR#04_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAS04                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS04                      DS    4A                                           
NEWSTRS04                    DS    A                                            
NEWSTRSIZS04                 DS    F                                            
*                                                                               
WORKDSAS04_SIZ               EQU   *-WORKDSAS04                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTR Equals                                                                   
*                                                                               
STR#05   CEEENTRY AUTO=WORKDSAS05_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS05,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
THISSTR  USING STR_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is ISTR to compare with               
THATSTR  USING STR_obj,R7        * Address object DSECT                         
*                                                                               
*        Only compare if both strings are allocated                             
         IF (CLC,THISSTR.STR_lpString,NE,=A(0)),AND,                   X        
               (CLC,THATSTR.STR_lpString,NE,=A(0)) THEN                         
            XR    R0,R0          * Compare until zero terminator                
            L     R2,THISSTR.STR_lpString * Point R2 to this string             
            L     R3,THATSTR.STR_lpString * Point R3 to incoming string         
            CLST  R2,R3          * Compare string                               
            BC    1,*-4          * Compare was incomplete, try again            
*                                                                               
            IF (8) THEN          * If equal                                     
               LA    R2,1                                                       
            ELSE                                                                
               XR    R2,R2                                                      
            ENDIF                                                               
         ELSE                                                                   
            XR    R2,R2                                                         
         ENDIF                                                                  
*                                                                               
STR#05_RET EQU   *                                                              
         CEETERM RC=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAS05                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKDSAS05_SIZ               EQU   *-WORKDSAS05                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTR EqualsZStr                                                               
*                                                                               
STR#06   CEEENTRY AUTO=WORKDSAS06_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS06,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STR_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   ZSTRS06,4(R1)     * Parm 2 is ptr to zero term string            
*                                                                               
*        Only compare if both strings are allocated                             
         IF (CLC,STR_lpString,NE,=A(0)),AND,                           X        
               (CLC,ZSTRS06,NE,=A(0)) THEN                                      
            XR    R0,R0          * Compare until zero terminator                
            L     R2,STR_lpString * Point R2 to this string                     
            L     R3,ZSTRS06     * Point R3 to incoming string                  
            CLST  R2,R3          * Compare string                               
            BC    1,*-4          * Compare was incomplete, try again            
*                                                                               
            IF (8) THEN          * If equal                                     
               LA    R2,1                                                       
            ELSE                                                                
               XR    R2,R2                                                      
            ENDIF                                                               
         ELSE                                                                   
            XR    R2,R2                                                         
         ENDIF                                                                  
*                                                                               
STR#06_RET EQU   *                                                              
         CEETERM RC=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAS06                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
ZSTRS06                      DS    A                                            
RESULTS06                    DS    A                                            
*                                                                               
WORKDSAS06_SIZ               EQU   *-WORKDSAS06                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTR CharAt                                                                   
*                                                                               
STR#07   CEEENTRY AUTO=WORKDSAS07_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS07,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STR_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   POSS07,4(R1)      * Parm 2 is position of char                   
*                                                                               
         L     R15,8(,R1)        * Parm 3 is ptr to result char                 
         ST    R15,RESULTS07     * Save as local var                            
         MVI   0(R15),X'00'      * Preset char to zero                          
*                                                                               
         IF (CLC,POSS07,LE,STR_nStrLen) THEN                                    
            L     R2,STR_lpString                                               
            A     R2,POSS07                                                     
            MVC   0(1,R15),0(R2)                                                
         ENDIF                                                                  
*                                                                               
STR#07_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAS07                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
POSS07                       DS    A                                            
RESULTS07                    DS    A                                            
*                                                                               
WORKDSAS07_SIZ               EQU   *-WORKDSAS07                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTR ToUpperCase                                                              
*                                                                               
STR#08   CEEENTRY AUTO=WORKDSAS08_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS08,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
THISSTR  USING STR_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is return ISTR ptr                    
         ST    R7,PARMSTROUTS08  * Save in a local var                          
         IF (CLC,0(4,R7),NE,=A(0)) THEN                                         
            ISTR_Release OBJECT=0(,R7),WORK=WORKS08                             
            MVC   0(4,R7),=A(0)  * Initialize ISTR to NULL                      
         ENDIF                                                                  
*                                                                               
         MINSTANT GUID=G_ISTR_GUID,WORK=WORKS08,OBJPTR=0(,R7)                   
*                                                                               
         L     R7,0(,R7)                                                        
THATSTR  USING STR_obj,R7                                                       
*                                                                               
         L     R6,THISSTR.STR_nStrLen                                           
         ST    R6,THATSTR.STR_nStrLen                                           
         LA    R6,1(,R6)                                                        
         ST    R6,G_GTSTSIZ                                                     
*                                                                               
         LA    R1,WORKS08                                                       
         ST    R6,0(,R1)                                                        
         LA    R15,THATSTR.STR_lpString                                         
         ST    R15,4(,R1)                                                       
         L     R15,G_GTST                                                       
         BASR  R14,R15                                                          
*                                                                               
         L     R4,THATSTR.STR_lpString                                          
         L     R5,THATSTR.STR_nStrLen                                           
         L     R2,THISSTR.STR_lpString                                          
         L     R3,THISSTR.STR_nStrLen                                           
         MVCL  R4,R2                                                            
*                                                                               
         XR    R0,R0                                                            
         L     R2,THATSTR.STR_lpString                                          
         L     R3,THATSTR.STR_nStrLen                                           
         LARL  R4,TRT_UPPER_037                                                 
         TRE   R2,R4                                                            
*                                                                               
STR#08_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAS08                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS08                      DS    4A                                           
*                                                                               
PARMSTROUTS08                DS    A                                            
*                                                                               
WORKDSAS08_SIZ               EQU   *-WORKDSAS08                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTR ToLowerCase                                                              
*                                                                               
STR#09   CEEENTRY AUTO=WORKDSAS09_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS09,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
THISSTR  USING STR_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is return ISTR ptr                    
         ST    R7,PARMSTROUTS09  * Save in a local var                          
         IF (CLC,0(4,R7),NE,=A(0)) THEN                                         
            ISTR_Release OBJECT=0(,R7),WORK=WORKS09                             
            MVC   0(4,R7),=A(0)  * Initialize ISTR to NULL                      
         ENDIF                                                                  
*                                                                               
         MINSTANT GUID=G_ISTR_GUID,WORK=WORKS09,OBJPTR=0(,R7)                   
*                                                                               
         L     R7,0(,R7)                                                        
THATSTR  USING STR_obj,R7                                                       
*                                                                               
         L     R6,THISSTR.STR_nStrLen                                           
         ST    R6,THATSTR.STR_nStrLen                                           
         LA    R6,1(,R6)                                                        
         ST    R6,G_GTSTSIZ                                                     
*                                                                               
         LA    R1,WORKS09                                                       
         ST    R6,0(,R1)                                                        
         LA    R15,THATSTR.STR_lpString                                         
         ST    R15,4(,R1)                                                       
         L     R15,G_GTST                                                       
         BASR  R14,R15                                                          
*                                                                               
         L     R4,THATSTR.STR_lpString                                          
         L     R5,THATSTR.STR_nStrLen                                           
         L     R2,THISSTR.STR_lpString                                          
         L     R3,THISSTR.STR_nStrLen                                           
         MVCL  R4,R2                                                            
*                                                                               
         XR    R0,R0                                                            
         L     R2,THATSTR.STR_lpString                                          
         L     R3,THATSTR.STR_nStrLen                                           
         LARL  R4,TRT_LOWER_037                                                 
         TRE   R2,R4                                                            
*                                                                               
STR#09_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAS09                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS09                      DS    4A                                           
*                                                                               
PARMSTROUTS09                DS    A                                            
*                                                                               
WORKDSAS09_SIZ               EQU   *-WORKDSAS09                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB QueryInterface                                                           
*                                                                               
STB#01   MQRYIFCE SUF=B01,IFACE=ISTB                                            
*                                                                               
* ISTB AddRef                                                                   
*                                                                               
STB#02   MADDREF                                                                
*                                                                               
* ISTB Release                                                                  
*                                                                               
STB#03   CEEENTRY AUTO=WORKDSAB03_SIZ,MAIN=NO,BASE=(R10)                        
*                                                                               
         USING WORKDSAB03,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global DSECT                         
*                                                                               
         L     R6,0(,R1)         * Param 1 points to this object                
         USING COM_obj,R6                                                       
*                                                                               
         LT    R5,count          * Load current ref count                       
         BZ    STB#03_RET        * Should never happen....                      
         S     R5,=A(1)          * Decrease ref count                           
         ST    R5,count          * Put new ref count back                       
*                                                                               
*        If reference count dropped to 0, object can be freed                   
         IF (Z) THEN                                                            
            DROP  R6                                                            
            USING STB_obj,R6                                                    
*                                                                               
            IF (CLC,STB_lpBuf,NE,=A(0)) THEN                                    
*              CALL  CEEFRST,(STB_lpBuf,FCB03),MF=(E,WORKB03)                   
               MVC   STB_lpBuf,=A(0)                                            
            ENDIF                                                               
*                                                                               
            DROP  R6                                                            
            USING COM_obj,R6                                                    
*                                                                               
            MVC   lpVtbl,=A(0)                                                  
*                                                                               
*           L     R5,lpVtbl      * Get ptr to Vtbl                              
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher              
*           ST    R5,OBJPTRB03   * Put ptr in variable                          
*           CALL  CEEFRST,(OBJPTRB03,FCB03),MF=(E,WORKB03)                      
*                                                                               
            L     R15,G_OBJCOUNT                                                
            BCTR  R15,R0                                                        
            ST    R15,G_OBJCOUNT                                                
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG2),AND,                  X        
               (C,R6,NE,G_ISTB_tmp) THEN                                        
               ST    R6,G_DEC8                                                  
               UNPK  G_ZONED8(9),G_DEC8(5)                                      
               L     R15,G_HEXTAB                                               
               TR    G_ZONED8(8),0(R15)                                         
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKB03                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKB03,      X        
               ZSTR=MAK502D_STB                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKB03,      X        
               ZSTR=G_ZONED8                                                    
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKB03,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
STB#03_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK502D_STB                  DC    C'MAK502D Deleted ISTB object '              
                             DC    X'00'                                        
*                                                                               
WORKDSAB03                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
OBJPTRB03                    DS    A                                            
WORKB03                      DS    3A                                           
*                                                                               
WORKDSAB03_SIZ               EQU   *-WORKDSAB03                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB Init                                                                     
*                                                                               
STB#04   CEEENTRY AUTO=WORKDSAB04_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB04,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         IF (CLC,STB_nBufSize,EQ,=A(0)) THEN                                    
            LA    R1,WORKB04                                                    
            MVC   0(4,R1),=A(256)                                               
            LA    R15,STB_lpBuf                                                 
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            L     R2,STB_lpBuf                                                  
            MVC   0(2,R2),=X'0000'                                              
            LA    R2,2(,R2)                                                     
*                                                                               
            ST    R2,STB_lpBuf                                                  
            MVC   STB_nBufSize,=A(254)                                          
         ENDIF                                                                  
*                                                                               
         IF (CLC,STB_nStrLen,GT,=A(0)) THEN                                     
            L     R2,STB_lpBuf      * Initialize                                
            L     R3,STB_nStrLen    *   used bufffer                            
            XR    R0,R0             *     to all                                
            XR    R1,R1             *        zeros                              
            MVCL  R2,R0                                                         
         ENDIF                                                                  
*                                                                               
         MVC   STB_nStrLen,=A(0)                                                
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKB04,LINE=MAK503D,           X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
*                                                                               
         XR    R0,R0                                                            
         XR    R15,R15                                                          
*                                                                               
STB#04_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK503D                      DC    C'MAK503D Initialized ISTB object',XX        
               '00'                                                             
*                                                                               
WORKDSAB04                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKB04                      DS    4A                                           
*                                                                               
WORKDSAB04_SIZ               EQU   *-WORKDSAB04                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB Equals                                                                   
*                                                                               
STB#05   CEEENTRY AUTO=WORKDSAB05_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB05,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
THISSTB  USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is ISTR to compare with               
THATSTR  USING STR_obj,R7        * Address object DSECT                         
*                                                                               
*        Only compare if both strings are allocated                             
         IF (CLC,THISSTB.STB_lpBuf,NE,=A(0)),AND,                      X        
               (CLC,THATSTR.STR_lpString,NE,=A(0)) THEN                         
            XR    R0,R0          * Compare until zero terminator                
            L     R2,THISSTB.STB_lpBuf * Point R2 to this string                
            L     R3,THATSTR.STR_lpString * Point R3 to incoming string         
            CLST  R2,R3          * Compare string                               
            BC    1,*-4          * Compare was incomplete, try again            
*                                                                               
            IF (8) THEN          * If equal                                     
               LA    R2,1                                                       
            ELSE                                                                
               XR    R2,R2                                                      
            ENDIF                                                               
         ELSE                                                                   
            XR    R2,R2                                                         
         ENDIF                                                                  
*                                                                               
STB#05_RET EQU   *                                                              
         CEETERM RC=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB05                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKDSAB05_SIZ               EQU   *-WORKDSAB05                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB EqualsZStr                                                               
*                                                                               
STB#06   CEEENTRY AUTO=WORKDSAB06_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB06,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   ZSTRB06,4(R1)     * Parm 2 is ptr to zero term string            
*                                                                               
*        Only compare if both strings are allocated                             
         IF (CLC,STB_lpBuf,NE,=A(0)),AND,                              X        
               (CLC,ZSTRB06,NE,=A(0)) THEN                                      
            XR    R0,R0          * Compare until zero terminator                
            L     R2,STB_lpBuf   * Point R2 to this string                      
            L     R3,ZSTRB06     * Point R3 to incoming string                  
            CLST  R2,R3          * Compare string                               
            BC    1,*-4          * Compare was incomplete, try again            
*                                                                               
            IF (8) THEN          * If equal                                     
               LA    R2,1                                                       
            ELSE                                                                
               XR    R2,R2                                                      
            ENDIF                                                               
         ELSE                                                                   
            XR    R2,R2                                                         
         ENDIF                                                                  
*                                                                               
STB#06_RET EQU   *                                                              
         CEETERM RC=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB06                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
ZSTRB06                      DS    A                                            
RESULTB06                    DS    A                                            
*                                                                               
WORKDSAB06_SIZ               EQU   *-WORKDSAB06                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB CharAt                                                                   
*                                                                               
STB#07   CEEENTRY AUTO=WORKDSAB07_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB07,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   POSB07,4(R1)      * Parm 2 is position of char                   
*                                                                               
         L     R15,8(,R1)        * Parm 3 is ptr to result char                 
         ST    R15,RESULTB07     * Save as local var                            
         MVI   0(R15),X'00'      * Preset char to zero                          
*                                                                               
         IF (CLC,POSB07,LE,STB_nStrLen) THEN                                    
            L     R2,STB_lpBuf                                                  
            A     R2,POSB07                                                     
            MVC   0(1,R15),0(R2)                                                
         ENDIF                                                                  
*                                                                               
STB#07_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB07                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
POSB07                       DS    A                                            
RESULTB07                    DS    A                                            
*                                                                               
WORKDSAB07_SIZ               EQU   *-WORKDSAB07                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB ToUpperCase                                                              
*                                                                               
STB#08   CEEENTRY AUTO=WORKDSAB08_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB08,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
THISSTB  USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is return ISTR ptr                    
         ST    R7,PARMSTROUTB08  * Save in a local var                          
         IF (CLC,0(4,R7),NE,=A(0)) THEN                                         
            ISTR_Release OBJECT=0(,R7),WORK=WORKB08                             
            MVC   0(4,R7),=A(0)  * Initialize ISTR to NULL                      
         ENDIF                                                                  
*                                                                               
         MINSTANT GUID=G_ISTR_GUID,WORK=WORKB08,OBJPTR=0(,R7)                   
*                                                                               
         L     R7,0(,R7)                                                        
THATSTR  USING STR_obj,R7                                                       
*                                                                               
         L     R6,THISSTB.STB_nStrLen                                           
         ST    R6,THATSTR.STR_nStrLen                                           
         LA    R6,1(,R6)                                                        
         ST    R6,G_GTSTSIZ                                                     
*                                                                               
         LA    R1,WORKB08                                                       
         ST    R6,0(,R1)                                                        
         LA    R15,THATSTR.STR_lpString                                         
         ST    R15,4(,R1)                                                       
         L     R15,G_GTST                                                       
         BASR  R14,R15                                                          
*                                                                               
         L     R4,THATSTR.STR_lpString                                          
         L     R5,THATSTR.STR_nStrLen                                           
         L     R2,THISSTB.STB_lpBuf                                             
         L     R3,THISSTB.STB_nStrLen                                           
         MVCL  R4,R2                                                            
*                                                                               
         XR    R0,R0                                                            
         L     R2,THATSTR.STR_lpString                                          
         L     R3,THATSTR.STR_nStrLen                                           
         LARL  R4,TRT_UPPER_037                                                 
         TRE   R2,R4                                                            
*                                                                               
STB#08_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB08                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKB08                      DS    4A                                           
*                                                                               
PARMSTROUTB08                DS    A                                            
*                                                                               
WORKDSAB08_SIZ               EQU   *-WORKDSAB08                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB ToLowerCase                                                              
*                                                                               
STB#09   CEEENTRY AUTO=WORKDSAB09_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB09,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
THISSTB  USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is return ISTR ptr                    
         ST    R7,PARMSTROUTB09  * Save in a local var                          
         IF (CLC,0(4,R7),NE,=A(0)) THEN                                         
            ISTR_Release OBJECT=0(,R7),WORK=WORKB09                             
            MVC   0(4,R7),=A(0)  * Initialize ISTR to NULL                      
         ENDIF                                                                  
*                                                                               
         MINSTANT GUID=G_ISTR_GUID,WORK=WORKB09,OBJPTR=0(,R7)                   
*                                                                               
         L     R7,0(,R7)                                                        
THATSTR  USING STR_obj,R7                                                       
*                                                                               
         L     R6,THISSTB.STB_nStrLen                                           
         ST    R6,THATSTR.STR_nStrLen                                           
         LA    R6,1(,R6)                                                        
         ST    R6,G_GTSTSIZ                                                     
*                                                                               
         LA    R1,WORKB09                                                       
         ST    R6,0(,R1)                                                        
         LA    R15,THATSTR.STR_lpString                                         
         ST    R15,4(,R1)                                                       
         L     R15,G_GTST                                                       
         BASR  R14,R15                                                          
*                                                                               
         L     R4,THATSTR.STR_lpString                                          
         L     R5,THATSTR.STR_nStrLen                                           
         L     R2,THISSTB.STB_lpBuf                                             
         L     R3,THISSTB.STB_nStrLen                                           
         MVCL  R4,R2                                                            
*                                                                               
         XR    R0,R0                                                            
         L     R2,THATSTR.STR_lpString                                          
         L     R3,THATSTR.STR_nStrLen                                           
         LARL  R4,TRT_LOWER_037                                                 
         TRE   R2,R4                                                            
*                                                                               
STB#09_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB09                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKB09                      DS    4A                                           
*                                                                               
PARMSTROUTB09                DS    A                                            
*                                                                               
WORKDSAB09_SIZ               EQU   *-WORKDSAB09                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB AppendChar                                                               
*                                                                               
STB#10   CEEENTRY AUTO=WORKDSAB10_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB10,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         L     R15,4(,R1)        * Parm 2 is character to append                
         STC   R15,charB10       * Save as local var                            
*                                                                               
*        Is there room for another char?                                        
         L     R7,STB_nBufSize   * Get buffer size                              
         L     R6,STB_nStrLen    * Get current string length                    
         IF (CR,R7,LE,R6) THEN   * Nope, so enlarge                             
            LA    R7,3(,R7)      * Add 2 for halfword prefix + 1 zero           
            SLL   R7,2           * Buffer size x 4                              
*                                                                               
            LA    R1,WORKB10                                                    
            ST    R7,0(,R1)                                                     
            LA    R15,newBufB10                                                 
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            L     R4,newBufB10                                                  
            MVC   0(2,R4),=X'0000'                                              
            LA    R4,2(,R4)                                                     
            ST    R4,newBufB10                                                  
*                                                                               
            L     R5,STB_nStrLen                                                
            L     R2,STB_lpBuf                                                  
            LR    R3,R5                                                         
            MVCL  R4,R2                                                         
*                                                                               
            S     R7,=A(2)                                                      
            ST    R7,STB_nBufSize                                               
            MVC   STB_lpBuf,newBufB10                                           
         ENDIF                                                                  
*                                                                               
         L     R5,STB_lpBuf      * Get buffer pointer                           
         AR    R5,R6             * Advance current length                       
         MVC   0(1,R5),charB10   * Append char                                  
         LA    R6,1(,R6)         * Add 1 to string length                       
         ST    R6,STB_nStrLen    * And save as property                         
*                                                                               
STB#10_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB10                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKB10                      DS    3A                                           
newBufB10                    DS    A                                            
charB10                      DS    C                                            
*                                                                               
WORKDSAB10_SIZ               EQU   *-WORKDSAB10                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB AppendString                                                             
*                                                                               
STB#11   CEEENTRY AUTO=WORKDSAB11_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB11,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is incoming ISTR                      
         USING STR_obj,R7        * Address object DSECT                         
*                                                                               
         L     R6,STB_nBufSize   * Get stringbuffer size                        
         LR    R5,R6             * Copy it                                      
         S     R5,STB_nStrLen    * Subtract current string length               
         IF (C,R5,LT,STR_nStrLen) THEN                                          
            LA    R6,3(,R6)      * Add 2 for halfword prefix + 1 zero           
            DO UNTIL=(C,R5,GT,STR_nStrLen)                                      
               SLL   R6,2        * Buffer size x 4                              
               LR    R5,R6       * Copy it                                      
               S     R5,STB_nStrLen * Subtract current string length            
            ENDDO                                                               
*                                                                               
            LA    R1,WORKB11                                                    
            ST    R6,0(,R1)                                                     
            LA    R15,newBufB11                                                 
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            L     R4,newBufB11                                                  
            MVC   0(2,R4),=X'0000'                                              
            LA    R4,2(,R4)                                                     
            ST    R4,newBufB11                                                  
*                                                                               
            L     R5,STB_nStrLen                                                
            L     R2,STB_lpBuf                                                  
            LR    R3,R5                                                         
            MVCL  R4,R2                                                         
*                                                                               
            S     R6,=A(2)                                                      
            ST    R6,STB_nBufSize                                               
            MVC   STB_lpBuf,newBufB11                                           
         ENDIF                                                                  
*                                                                               
         L     R4,STB_lpBuf                                                     
         A     R4,STB_nStrLen                                                   
         L     R2,STR_lpString                                                  
         L     R3,STR_nStrLen                                                   
         LR    R5,R3                                                            
         MVCL  R4,R2                                                            
         MVI   0(R4),X'00'                                                      
*                                                                               
         L     R6,STB_nStrLen                                                   
         A     R6,STR_nStrLen                                                   
         ST    R6,STB_nStrLen                                                   
*                                                                               
STB#11_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB11                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKB11                      DS    4A                                           
*                                                                               
newBufB11                    DS    A                                            
*                                                                               
WORKDSAB11_SIZ               EQU   *-WORKDSAB11                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB AppendZString                                                            
*                                                                               
STB#12   CEEENTRY AUTO=WORKDSAB12_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB12,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   NEWSTRB12,4(R1)   * Parm 2 is zero terminated str ptr            
*                                                                               
         XR    R0,R0             * Search for terminating zero                  
         L     R2,NEWSTRB12      * Point R1 to incoming zero str                
         LR    R1,R2             * Point R1 to next byte of str to srch         
         LA    R2,4095(,R1)      * Search for max 4K                            
         SRST  R2,R1             * Scan for zero terminator                     
         BRC   1,*-4             * Scan was incomplete, try again               
         BRC   2,*-14            * Not found, try another 4K                    
*                                                                               
         L     R1,NEWSTRB12      * Point R1 to incoming zero str                
         SR    R2,R1             * Calculate length                             
         ST    R2,NEWSTRLENB12   * Save it                                      
*                                                                               
         L     R6,STB_nBufSize   * Get stringbuffer size                        
         LR    R5,R6             * Copy it                                      
         S     R5,STB_nStrLen    * Subtract current string length               
         IF (C,R5,LE,NEWSTRLENB12) THEN                                         
            LA    R6,3(,R6)      * Add 2 for halfword prefix + 1 zero           
            DO UNTIL=(C,R5,GT,NEWSTRLENB12)                                     
               SLL   R6,2        * Buffer size x 4                              
               LR    R5,R6       * Copy it                                      
               S     R5,STB_nStrLen * Subtract current string length            
            ENDDO                                                               
*                                                                               
            LA    R1,WORKB12                                                    
            ST    R6,0(,R1)                                                     
            LA    R15,newBufB12                                                 
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            L     R4,newBufB12                                                  
            MVC   0(2,R4),=X'0000'                                              
            LA    R4,2(,R4)                                                     
            ST    R4,newBufB12                                                  
*                                                                               
            L     R5,STB_nStrLen                                                
            L     R2,STB_lpBuf                                                  
            LR    R3,R5                                                         
            MVCL  R4,R2                                                         
*                                                                               
            S     R6,=A(2)                                                      
            ST    R6,STB_nBufSize                                               
            MVC   STB_lpBuf,newBufB12                                           
         ENDIF                                                                  
*                                                                               
         L     R4,STB_lpBuf                                                     
         A     R4,STB_nStrLen                                                   
         L     R2,NEWSTRB12                                                     
         L     R3,NEWSTRLENB12                                                  
         LR    R5,R3                                                            
         MVCL  R4,R2                                                            
         MVI   0(R4),X'00'                                                      
*                                                                               
         L     R6,STB_nStrLen                                                   
         A     R6,NEWSTRLENB12                                                  
         ST    R6,STB_nStrLen                                                   
*                                                                               
STB#12_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB12                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKB12                      DS    4A                                           
*                                                                               
newBufB12                    DS    A                                            
*                                                                               
NEWSTRB12                    DS    A                                            
NEWSTRLENB12                 DS    F                                            
*                                                                               
WORKDSAB12_SIZ               EQU   *-WORKDSAB12                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* ISTB ToString                                                                 
*                                                                               
STB#13   CEEENTRY AUTO=WORKDSAB13_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAB13,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING STB_obj,R8        * Address object DSECT                         
*                                                                               
         L     R7,4(,R1)         * Parm 2 is ptr to output ISTR                 
         ST    R7,PARMSTROUTB13  * Save in a local var                          
         IF (CLC,0(4,R7),EQ,=A(0)) THEN                                         
            MINSTANT GUID=G_ISTR_GUID,WORK=WORKB13,OBJPTR=0(,R7)                
         ENDIF                                                                  
*                                                                               
         IF (CLC,STB_nStrLen,NE,=A(0)) THEN                                     
            L     R3,STB_lpBuf                                                  
         ELSE                                                                   
            LA    R3,=A(0)                                                      
         ENDIF                                                                  
*                                                                               
         ISTR_Set OBJECT=0(,R7),WORK=WORKB13,STR=0(,R3)                         
*                                                                               
STB#13_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAB13                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKB13                      DS    2A                                           
*                                                                               
PARMSTROUTB13                DS    A                                            
*                                                                               
ISTR_B13                     DS    A                                            
*                                                                               
WORKDSAB13_SIZ               EQU   *-WORKDSAB13                                 
*                                                                               
LWZMSTR  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* Translate table for uppercase IBM-037                                         
*                                                                               
TRT_UPPER_037      DS    0AD                                                    
*                           0 1 2 3 4 5 6 7 8 9 A B C D E F                     
                   DC    X'000102030405060708090A0B0C0D0E0F' 0                  
                   DC    X'101112131415161718191A1B1C1D1E1F' 1                  
                   DC    X'202122232425262728292A2B2C2D2E2F' 2                  
                   DC    X'303132333435363738393A3B3C3D3E3F' 3                  
                   DC    X'404162636465666768694A4B4C4D4E4F' 4                  
                   DC    X'507172737475767778595A5B5C5D5E5F' 5                  
                   DC    X'606162636465666768696A6B6C6D6E6F' 6                  
                   DC    X'707172737475767778797A7B7C7D7E7F' 7                  
                   DC    X'80C1C2C3C4C5C6C7C8C98A8BACADAE8F' 8                  
                   DC    X'90D1D2D3D4D5D6D7D8D99A9B9E9D9E9F' 9                  
                   DC    X'A0A1E2E3E4E5E6E7E8E9AAABACADAEAF' A                  
                   DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF' B                  
                   DC    X'C0C1C2C3C4C5C6C7C8C9CAEBECEDEEEF' C                  
                   DC    X'D0D1D2D3D4D5D6D7D8D9DAFBFCFDFEDF' D                  
                   DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF' E                  
                   DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF' F                  
*                                                                               
* Translate table for lowercase IBM-037                                         
*                                                                               
TRT_LOWER_037      DS    0AD                                                    
*                           0 1 2 3 4 5 6 7 8 9 A B C D E F                     
                   DC    X'000102030405060708090A0B0C0D0E0F' 0                  
                   DC    X'101112131415161718191A1B1C1D1E1F' 0                  
                   DC    X'202122232425262728292A2B2C2D2E2F' 0                  
                   DC    X'303132333435363738393A3B3C3D3E3F' 0                  
                   DC    X'404142434445464748494A4B4C4D4E4F' 0                  
                   DC    X'505152535455565758595A5B5C5D5E5F' 0                  
                   DC    X'606142434445464748496A6B6C6D6E6F' 0                  
                   DC    X'705152535455565758597A7B7C7D7E7F' 0                  
                   DC    X'808182838485868788898A8B8C8D8E8F' 0                  
                   DC    X'909192939495969798999A9B9C9D9C9F' 0                  
                   DC    X'A0A1A2A3A4A5A6A7A8A9AAAB8C8D8EAF' 0                  
                   DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF' 0                  
                   DC    X'C0818283848586878889CACBCCCDCECF' 0                  
                   DC    X'D0919293949596979899DADBDCDDDEDF' 0                  
                   DC    X'E0E1A2A3A4A5A6A7A8A9EACBCCCDCECF' 0                  
                   DC    X'F0F1F2F3F4F5F6F7F8F9FADBDCDDDEFF' 0                  
*                                                                               
         COPY  REGS              * Register equates                             
*                                                                               
         END   LWZMSTR                                                          
