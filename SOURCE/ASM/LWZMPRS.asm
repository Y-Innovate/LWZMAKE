*---------------------------------------------------------------------*         
* Program    : LWZMPRS                                                *         
* Description: COM object for Parser and ParserState                  *         
*---------------------------------------------------------------------*         
         TITLE 'LWZMPRS'                                                        
*                                                                               
         COPY  ASMMSP            * Enable HLASM struct.prog.macro's             
*                                                                               
         COPY  IFACES            * Object interfaces                            
*                                                                               
         COPY  MINSTANT          * Macro to instantiate new object              
*                                                                               
* Main routine creates a new PRS or PSS object                                  
*                                                                               
LWZMPRS  CEEENTRY AUTO=WORKDSA_SIZ,MAIN=NO,BASE=R10                             
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
*        Was a new PRS object requested?                                        
         IF (CLC,0(16,R6),EQ,G_IPRS_GUID) THEN                                  
            MNEWOBJ OBJTYPE=PRS,WORK=WORK * Alloc new object                    
*                                                                               
*           Init obj attributes                                                 
            LR    R4,R14                                                        
            USING PRS_obj,R4                                                    
*                                                                               
*           Instantiate a new IAV2 object                                       
            MINSTANT GUID=G_IAV2_GUID,WORK=WORK,OBJPTR=IAV2_statements          
*                                                                               
*           Instantiate a new IAVL object                                       
            MINSTANT GUID=G_IAVL_GUID,WORK=WORK,OBJPTR=IAVL_variables           
*                                                                               
*           Instantiate a new IAVL object                                       
            MINSTANT GUID=G_IAVL_GUID,WORK=WORK,OBJPTR=IAVL_targets             
*                                                                               
*           Instantiate a new IAVL object                                       
            MINSTANT GUID=G_IAVL_GUID,WORK=WORK,OBJPTR=IAVL_phonies             
*                                                                               
*           MVI   firstBuild,C'Y'                                               
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
               ZSTR=MAK501D_PRS                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORK,         X        
               ZSTR=G_ZONED8                                                    
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORK,LINE=0(,R2),    ,    X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
            ENDIF                                                               
*                                                                               
*        Was a new PSS object requested?                                        
         ELSEIF (CLC,0(16,R6),EQ,G_IPSS_GUID) THEN                              
            MNEWOBJ OBJTYPE=PSS,WORK=WORK * Alloc new object                    
*                                                                               
*           Init obj attributes                                                 
            LR    R4,R14                                                        
            USING PSS_obj,R4                                                    
*                                                                               
            MINSTANT GUID=G_IAV2_GUID,WORK=WORK,OBJPTR=PSS_IAV2_state           
*                                                                               
            LA    R2,VARIANT_INITSTATE                                          
            MVI   0(R2),X'00'                                                   
            MVC   1(VARIANT_SIZ-1,R2),0(R2)                                     
            MVC   vt-VARIANT(4,R2),=A(VT_UI4)                                   
*                                                                               
            IAV2_Insert OBJECT=PSS_IAV2_state,WORK=WORK,INDEX_IN==A(0),X        
               VARIANT_IN=VARIANT_INITSTATE                                     
*                                                                               
            MVI   PSS_cRecipePrefix,C'-'                                        
            MVI   PSS_cPhase,C'1'                                               
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
               ZSTR=MAK501D_PSS                                                 
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
LWZMPRS_RET EQU   *                                                             
         CEETERM                 * Return to caller                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
PRS#01A                      DC    A(PRS#01)   * QueryInterface                 
PRS#02A                      DC    A(PRS#02)   * AddRef                         
PRS#03A                      DC    A(PRS#03)   * Release                        
PRS#04A                      DC    A(PRS#04)   * ParseParameter                 
PRS#05A                      DC    A(PRS#05)   * ParseStatement                 
PRS#06A                      DC    A(PRS#06)   * ResolveParameter               
PRS#07A                      DC    A(PRS#07)   * BuildTargets                   
*                                                                               
                             DS    0F                                           
PSS#01A                      DC    A(PSS#01)   * QueryInterface                 
PSS#02A                      DC    A(PSS#02)   * AddRef                         
PSS#03A                      DC    A(PSS#03)   * Release                        
PSS#04A                      DC    A(PSS#04)   * IsExpectedTokenType            
PSS#05A                      DC    A(PSS#05)   * GetLastState                   
PSS#06A                      DC    A(PSS#06)   * AlterLastState                 
PSS#07A                      DC    A(PSS#07)   * Push                           
PSS#08A                      DC    A(PSS#08)   * Pop                            
PSS#09A                      DC    A(PSS#09)   * SetPhase                       
PSS#10A                      DC    A(PSS#10)   * SetRecipePrefix                
*                                                                               
                             DS    0F                                           
MAK501D_PRS                  DC    C'MAK501D Created IPRS object '              
                             DC    X'00'                                        
                             DS    0F                                           
MAK501D_PSS                  DC    C'MAK501D Created IPSS object '              
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
VARIANT_INITSTATE            DS    CL(VARIANT_SIZ)                              
*                                                                               
WORKDSA_SIZ                  EQU   *-WORKDSA                                    
*                                                                               
         MGLOBAL                 * Global area DSECT                            
*                                                                               
         COPY  DSCOMOBJ          * Generic COM obj DSECT                        
*                                                                               
         COPY  DSPRS             * IPRS obj DSECT                               
         COPY  DSPSS             * IPSS obj DSECT                               
         COPY  DSTFO             * ITFO obj DSECT                               
         COPY  DSFFO             * IFFO obj DSECT                               
         COPY  DSSTB             * ISTB obj DSECT                               
         COPY  DSSTR             * ISTR obj DSECT                               
         COPY  DSST1             * IST1 obj DSECT                               
         COPY  DSST2             * IST2 obj DSECT                               
         COPY  DSST5             * IST5 obj DSECT                               
         COPY  DSST6             * IST6 obj DSECT                               
*                                                                               
         COPY  VARIANT           * DSECT for typed value                        
*                                                                               
         DROP                                                                   
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
* IPRS QueryInterface                                                           
*                                                                               
PRS#01   MQRYIFCE SUF=P01,IFACE=IPRS                                            
*                                                                               
* IPRS AddRef                                                                   
*                                                                               
PRS#02   MADDREF                                                                
*                                                                               
* IPRS Release                                                                  
*                                                                               
PRS#03   CEEENTRY AUTO=WORKDSAP03_SIZ,MAIN=NO,BASE=(R10)                        
*                                                                               
         USING WORKDSAP03,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global DSECT                         
*                                                                               
         L     R6,0(,R1)         * Param 1 points to this object                
         USING COM_obj,R6                                                       
*                                                                               
         LT    R5,count          * Load current ref count                       
         BZ    PRS#03_RET        * Should never happen....                      
         S     R5,=A(1)          * Decrease ref count                           
         ST    R5,count          * Put new ref count back                       
*                                                                               
*        If reference count dropped to 0, object can be freed                   
         IF (Z) THEN                                                            
            DROP  R6                                                            
            USING PRS_obj,R6                                                    
*                                                                               
            IAVL_Release OBJECT=IAVL_variables,WORK=WORKP03                     
            MVC   IAVL_variables,=A(0)                                          
*                                                                               
            IAVL_Release OBJECT=IAVL_phonies,WORK=WORKP03                       
            MVC   IAVL_phonies,=A(0)                                            
*                                                                               
            IAVL_Release OBJECT=IAVL_targets,WORK=WORKP03                       
            MVC   IAVL_targets,=A(0)                                            
*                                                                               
            IAV2_Release OBJECT=IAV2_statements,WORK=WORKP03                    
            MVC   IAV2_statements,=A(0)                                         
*                                                                               
            DROP  R6                                                            
            USING COM_obj,R6                                                    
*                                                                               
            MVC   lpVtbl,=A(0)                                                  
*                                                                               
*           L     R5,lpVtbl      * Get ptr to Vtbl                              
*           S     R5,=A(8)       * Go back 8 bytes for eye catcher              
*           ST    R5,OBJPTRP03   * Put ptr in variable                          
*           CALL  CEEFRST,(OBJPTRP03,FCP03),MF=(E,WORKP03)                      
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
               TR    G_ZONED8(8),0(R15)                                         
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP03                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP03,      X        
               ZSTR=MAK502D_PRS                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP03,      X        
               ZSTR=G_ZONED8                                                    
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKP03,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_DEBUG2                                        
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
PRS#03_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK502D_PRS                  DC    C'MAK502D Deleted IPRS object '              
                             DC    X'00'                                        
*                                                                               
WORKDSAP03                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
OBJPTRP03                    DS    A                                            
WORKP03                      DS    3A                                           
*                                                                               
WORKDSAP03_SIZ               EQU   *-WORKDSAP03                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS ParseParameter                                                           
*                                                                               
PRS#04   CEEENTRY AUTO=WORKDSAP04_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP04,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P04,4(R1) * Parm 2 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P04,8(R1) * Parm 3 is Tokenizer                   
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP04,LINE=MAK414D_P04,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVC   ITFO_ti1_P04,=A(0) * Initialize ti1                              
         MVC   ITFO_ti2_P04,=A(0) * Initialize ti2                              
         MVC   ITFO_ti3_P04,=A(0) * Initialize ti3                              
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P04,WORK=WORKP04,     X        
               IPSS=IPSS_ps_P04,ITFOPTR=ITFO_ti1_P04                            
*                                                                               
         L     R7,ITFO_ti1_P04                                                  
ti1      USING TFO_obj,R7                                                       
*                                                                               
         IF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_LOGSWITCH)) THEN            
            IPSS_Push OBJECT=IPSS_ps_P04,WORK=WORKP04,                 X        
               STATE_IN==A(PARSER_STATE_IN_LOGPARM)                             
*                                                                               
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P04,               X        
               WORK=WORKP04,IPSS=IPSS_ps_P04,ITFOPTR=ITFO_ti2_P04               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#04_RET                                                    
*                                                                               
            L     R6,ITFO_ti2_P04                                               
            L     R6,TFO_ISTB_token-TFO_obj(,R6)                                
            L     R5,STB_lpBuf-STB_obj(,R6)                                     
*                                                                               
            IF (CLC,STB_nStrLen-STB_obj(4,R6),EQ,=A(1)),AND,           X        
               (CLI,0(R5),GE,C'0'),AND,(CLI,0(R5),LE,C'6') THEN                 
               XR    R4,R4                                                      
               IC    R4,0(,R5)                                                  
               N     R4,=X'0000000F'                                            
               ST    R4,logLevel_P04                                            
               ILOG_SetLogLevel OBJECT=G_ILOG,WORK=WORKP04,            X        
               LOGLEVEL=logLevel_P04                                            
*                                                                               
               IPSS_Pop OBJECT=IPSS_ps_P04,WORK=WORKP04                         
*                                                                               
               B     PRS#04_RET                                                 
            ELSE                                                                
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP04                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP04,      X        
               ZSTR=MAK117E_P04                                                 
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP04,      X        
               ZSTR=STB_lpBuf-STB_obj(,R6)                                      
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKP04,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
               MVC   G_RETCODE,=A(8)                                            
               B     PRS#04_RET                                                 
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_TARGETSWITCH)) THEN         
            IPSS_Push OBJECT=IPSS_ps_P04,WORK=WORKP04,                 X        
               STATE_IN==A(PARSER_STATE_IN_TARGETPARM1)                         
*                                                                               
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P04,               X        
               WORK=WORKP04,IPSS=IPSS_ps_P04,ITFOPTR=ITFO_ti2_P04               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#04_RET                                                    
*                                                                               
            L     R6,ITFO_ti2_P04                                               
ti2         USING TFO_obj,R6                                                    
*                                                                               
            IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))               
               LA    R1,WORKP04                                                 
               MVC   0(4,R1),IPSS_ps_P04                                        
               MVC   4(4,R1),ITOK_tokenizer_P04                                 
               MVC   8(4,R1),ITFO_ti2_P04                                       
               XR    R14,R15                                                    
               IC    R14,=C'N'                                                  
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),=A(0)                                             
               L     R15,PRS#98A_P04 * parseVariable                            
               BASR  R14,R15                                                    
            ENDIF                                                               
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P04,WORK=WORKP04,       X        
               STATE_IN==A(PARSER_STATE_IN_TARGETPARM2)                         
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*                                                                               
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P04,            X        
               WORK=WORKP04,IPSS=IPSS_ps_P04,ITFOPTR=ITFO_ti3_P04               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#04_RET                                                 
*                                                                               
               L     R5,ITFO_ti3_P04                                            
ti3            USING TFO_obj,R5                                                 
*                                                                               
               IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,    X        
               (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               LA    R1,WORKP04                                                 
               MVC   0(4,R1),ITFO_ti2_P04                                       
               MVC   4(4,R1),ITFO_ti3_P04                                       
               MVC   8(4,R1),=A(2) * include all spaces                         
               L     R15,PRS#80A_P04 * appendToken                              
               BASR  R14,R15                                                    
            ENDDO                                                               
*                                                                               
            IPSS_Pop OBJECT=IPSS_ps_P04,WORK=WORKP04                            
*                                                                               
            MVC   G_ISTB_buildTargets,ti2.TFO_ISTB_token                        
         ENDIF                                                                  
*                                                                               
PRS#04_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK414D_P04                  DC    C'MAK414D Parsing parameter',X'00'           
                             DS    0F                                           
MAK117E_P04                  DC    C'MAK117E Invalid log level ',X'00'          
*                                                                               
                             DS    0F                                           
PRS#80A_P04                  DC    A(PRS#80)                                    
PRS#98A_P04                  DC    A(PRS#98)                                    
*                                                                               
WORKDSAP04                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP04                      DS    5A                                           
*                                                                               
IPSS_ps_P04                  DS    A                                            
ITOK_tokenizer_P04           DS    A                                            
ITFO_ti1_P04                 DS    A                                            
ITFO_ti2_P04                 DS    A                                            
ITFO_ti3_P04                 DS    A                                            
*                                                                               
logLevel_P04                 DS    F                                            
*                                                                               
WORKDSAP04_SIZ               EQU   *-WORKDSAP04                                 
*                                                                               
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS ParseStatement                                                           
*                                                                               
PRS#05   CEEENTRY AUTO=WORKDSAP05_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP05,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P05,4(R1) * Parm 2 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P05,8(R1) * Parm 3 is Tokenizer                   
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP05,LINE=MAK401D_P05,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVI   recipeStatement_P05,C'N' * Init recipeStatement flag             
*                                                                               
         MVC   ITFO_ti1_P05,=A(0) * Initialize ti1                              
         MVC   ITFO_ti2_P05,=A(0) * Initialize ti2                              
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P05,WORK=WORKP05,     X        
               IPSS=IPSS_ps_P05,ITFOPTR=ITFO_ti1_P05                            
*                                                                               
         L     R7,ITFO_ti1_P05                                                  
ti1      USING TFO_obj,R7                                                       
*                                                                               
         DO WHILE=(CLC,G_RETCODE,EQ,=A(0),AND,                         X        
               CLC,ti1.TFO_tokenType,NE,=A(TOKEN_TYPE_EOF))                     
            IF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMENT)),OR,   X        
               (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P05,            X        
               WORK=WORKP05,IPSS=IPSS_ps_P05,ITFOPTR=ITFO_ti1_P05               
*                                                                               
               L     R7,ITFO_ti1_P05                                            
            ELSE                                                                
               ASMLEAVE                                                         
            ENDIF                                                               
         ENDDO                                                                  
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#05_RET                                                       
*                                                                               
         IF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_RECIPEPREFIX)) THEN         
            MVI   recipeStatement_P05,C'Y'                                      
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P05,WORK=WORKP05,  X        
               IPSS=IPSS_ps_P05,ITFOPTR=ITFO_ti1_P05                            
*                                                                               
            L     R7,ITFO_ti1_P05                                               
         ELSE                                                                   
            IPSS_GetLastState OBJECT=IPSS_ps_P05,WORK=WORKP05,         X        
               STATE_OUT=lastState_P05                                          
*                                                                               
            IF (CLC,lastState_P05,EQ,=A(PARSER_STATE_IN_RECIPE)) THEN           
               IPSS_Pop OBJECT=IPSS_ps_P05,WORK=WORKP05                         
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (CLC,G_RETCODE,EQ,=A(0)),AND,                              X        
               (CLC,ti1.TFO_tokenType,NE,=A(TOKEN_TYPE_EOF)) THEN               
            IF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_SPECIAL)) THEN           
               LA    R1,WORKP05                                                 
               MVC   0(4,R1),IPSS_ps_P05                                        
               MVC   4(4,R1),ITOK_tokenizer_P05                                 
               MVC   8(4,R1),ITFO_ti1_P05                                       
               L     R15,PRS#99A_P05 * parseSpecial                             
               BASR  R14,R15                                                    
            ELSEIF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))           
               LA    R1,WORKP05                                                 
               MVC   0(4,R1),IPSS_ps_P05                                        
               MVC   4(4,R1),ITOK_tokenizer_P05                                 
               MVC   8(4,R1),ITFO_ti1_P05                                       
               XR    R14,R15                                                    
               IC    R14,=C'Y'                                                  
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),=A(0)                                             
               L     R15,PRS#98A_P05 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               IPSS_Push OBJECT=IPSS_ps_P05,WORK=WORKP05,              X        
               STATE_IN==A(PARSER_STATE_IN_RULE1)                               
*                                                                               
               MVC   ITFO_ti2_P05,=A(0)                                         
*                                                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P05,            X        
               WORK=WORKP05,IPSS=IPSS_ps_P05,ITFOPTR=ITFO_ti2_P05               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#05_RET                                                 
*                                                                               
               DROP  ti1                                                        
               L     R7,ITFO_ti2_P05                                            
ti2            USING TFO_obj,R7                                                 
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               )                                                                
                  LA    R1,WORKP05                                              
                  MVC   0(4,R1),IPSS_ps_P05                                     
                  MVC   4(4,R1),ITOK_tokenizer_P05                              
                  MVC   8(4,R1),ITFO_ti2_P05                                    
                  L     R15,PRS#97A_P05 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#05_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP05                                              
                  MVC   0(4,R1),IPSS_ps_P05                                     
                  MVC   4(4,R1),ITOK_tokenizer_P05                              
                  MVC   8(4,R1),ITFO_ti2_P05                                    
                  XR    R14,R14                                                 
                  IC    R14,=C'Y'                                               
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),=A(0)                                          
                  L     R15,PRS#98A_P05 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#05_RET                                              
               ENDIF                                                            
*                                                                               
               LA    R1,WORKP05                                                 
               MVC   0(4,R1),IPSS_ps_P05                                        
               MVC   4(4,R1),ITOK_tokenizer_P05                                 
               MVC   8(4,R1),ITFO_ti1_P05                                       
               MVC   12(4,R1),ITFO_ti2_P05                                      
               L     R15,PRS#96A_P05 * parseRule                                
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#05_RET                                                 
*                                                                               
               DROP  ti2                                                        
ti1            USING TFO_obj,R7                                                 
*                                                                               
            ELSEIF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_NORMAL))             
               IPSS_Push OBJECT=IPSS_ps_P05,WORK=WORKP05,              X        
               STATE_IN==A(PARSER_STATE_IN_NORMAL)                              
*                                                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P05,            X        
               WORK=WORKP05,IPSS=IPSS_ps_P05,ITFOPTR=ITFO_ti2_P05               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#05_RET                                                 
*                                                                               
               DROP  ti1                                                        
               L     R7,ITFO_ti2_P05                                            
ti2            USING TFO_obj,R7                                                 
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               )                                                                
                  LA    R1,WORKP05                                              
                  MVC   0(4,R1),IPSS_ps_P05                                     
                  MVC   4(4,R1),ITOK_tokenizer_P05                              
                  MVC   8(4,R1),ITFO_ti2_P05                                    
                  L     R15,PRS#97A_P05 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#05_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_EQUALS)),OR, X        
               (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_IMMEDEQUALS)),ORX        
               ,(CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONDEQUALS))            
                  LA    R1,WORKP05                                              
                  MVC   0(4,R1),IPSS_ps_P05                                     
                  MVC   4(4,R1),ITOK_tokenizer_P05                              
                  MVC   8(4,R1),ITFO_ti1_P05                                    
                  MVC   12(4,R1),ITFO_ti2_P05                                   
                  XR    R15,R15                                                 
                  IC    R15,recipeStatement_P05                                 
                  ICM   R15,2,=C'1'                                             
                  ST    R15,16(,R1)                                             
                  L     R15,PRS#95A_P05 * parseAssignment                       
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#05_RET                                              
               ELSEIF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_RULE)),ORX        
               ,(CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)),OR, X        
               (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_NORMAL)) THEN            
                  IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))         
                     LA    R1,WORKP05                                           
                     MVC   0(4,R1),IPSS_ps_P05                                  
                     MVC   4(4,R1),ITOK_tokenizer_P05                           
                     MVC   8(4,R1),ITFO_ti2_P05                                 
                     XR    R14,R14                                              
                     IC    R14,=C'Y'                                            
                     ST    R14,12(,R1)                                          
                     MVC   16(4,R1),=A(0)                                       
                     L     R15,PRS#98A_P05 * parseVariable                      
                     BASR  R14,R15                                              
*                                                                               
                     CLC   G_RETCODE,=A(0)                                      
                     BNE   PRS#05_RET                                           
                  ENDIF                                                         
*                                                                               
                  LA    R1,WORKP05                                              
                  MVC   0(4,R1),IPSS_ps_P05                                     
                  MVC   4(4,R1),ITOK_tokenizer_P05                              
                  MVC   8(4,R1),ITFO_ti1_P05                                    
                  MVC   12(4,R1),ITFO_ti2_P05                                   
                  L     R15,PRS#96A_P05 * parseRule                             
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#05_RET                                              
               ENDIF                                                            
*                                                                               
               DROP  ti2                                                        
ti1            USING TFO_obj,R7                                                 
*                                                                               
            ELSEIF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_CALL)) THEN          
               LA    R1,WORKP05                                                 
               MVC   0(4,R1),IPSS_ps_P05                                        
               MVC   4(4,R1),ITOK_tokenizer_P05                                 
               XR    R15,R15                                                    
               IC    R15,recipeStatement_P05                                    
               ST    R15,8(,R1)                                                 
               L     R15,PRS#94A_P05 * parseCall                                
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#05_RET                                                 
*                                                                               
            ELSEIF (CLC,ti1.TFO_tokenType,EQ,=A(TOKEN_TYPE_SH)) THEN            
               LA    R1,WORKP05                                                 
               MVC   0(4,R1),IPSS_ps_P05                                        
               MVC   4(4,R1),ITOK_tokenizer_P05                                 
               XR    R15,R15                                                    
               IC    R15,recipeStatement_P05                                    
               ICM   R15,2,=C'1'                                                
               ST    R15,8(,R1)                                                 
               L     R15,PRS#93A_P05 * parseSh                                  
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#05_RET                                                 
            ELSE                                                                
               LA    R1,WORKP05                                                 
               MVC   0(4,R1),ITFO_ti1_P05                                       
               MVC   0(4,R1),=A(0)                                              
               L     R15,PRS#90A_P05 * ParserException                          
               BASR  R14,R15                                                    
*                                                                               
               B     PRS#05_RET                                                 
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti1_P05,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti1_P05,WORK=WORKP05                       
            MVC   ITFO_ti1_P05,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti2_P05,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti2_P05,WORK=WORKP05                       
            MVC   ITFO_ti2_P05,=A(0)                                            
         ENDIF                                                                  
*                                                                               
PRS#05_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
PRS#90A_P05                  DC    A(PRS#90) * ParserException                  
PRS#93A_P05                  DC    A(PRS#93) * parseSh                          
PRS#94A_P05                  DC    A(PRS#94) * parseCall                        
PRS#95A_P05                  DC    A(PRS#95) * parseAssignment                  
PRS#96A_P05                  DC    A(PRS#96) * parseRule                        
PRS#97A_P05                  DC    A(PRS#97) * parseContinuation                
PRS#98A_P05                  DC    A(PRS#98) * parseVariable                    
PRS#99A_P05                  DC    A(PRS#99) * parseSpecial                     
*                                                                               
                             DS    0F                                           
MAK401D_P05                  DC    C'MAK401D Parsing statement',X'00'           
*                                                                               
WORKDSAP05                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP05                      DS    5A                                           
*                                                                               
IPSS_ps_P05                  DS    A                                            
ITOK_tokenizer_P05           DS    A                                            
ITFO_ti1_P05                 DS    A                                            
ITFO_ti2_P05                 DS    A                                            
*                                                                               
lastState_P05                DS    F                                            
recipeStatement_P05          DS    C                                            
*                                                                               
WORKDSAP05_SIZ               EQU   *-WORKDSAP05                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS ResolveParameter                                                         
*                                                                               
PRS#06   CEEENTRY AUTO=WORKDSAP06_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP06,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   PARMISTBPARMSP06,4(R1) * Parm 2 is ISTB parameters               
*                                                                               
         MVC   PARMIAV2TGTSP06,8(R1) * Parm 3 is IAV2 targets ptr               
*                                                                               
         MVC   ISTB_resolved_P06,=A(0)                                          
*                                                                               
PRS#06_RETRY EQU   *                                                            
         LT    R5,PARMISTBPARMSP06                                              
         IF (NZ) THEN                                                           
            LA    R1,WORKP06                                                    
            MVC   0(4,R1),STB_lpBuf-STB_obj(R5)                                 
            LA    R15,ISTB_resolved_P06                                         
            ST    R15,4(,R1)                                                    
            MVC   8(4,R1),=X'80000001' * Line no 1 + last parm                  
            L     R15,PRS#60A_P06 * Resolve CALL statement                      
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#06_RET                                                    
*                                                                               
            MVC   currIndex_P06,=A(0)                                           
*                                                                               
*           Instantiate a new ISTR object for target name                       
            MINSTANT GUID=G_ISTR_GUID,WORK=WORKP06,OBJPTR=ISTR_tgt_P06          
*                                                                               
            L     R5,ISTB_resolved_P06                                          
            L     R6,STB_lpBuf-STB_obj(,R5)                                     
            L     R7,STB_nStrLen-STB_obj(,R5)                                   
*                                                                               
            L     R1,G_TRT_ANY_BUT_SPACE                                        
*                                                                               
            LR    R4,R6                                                         
*                                                                               
            TRTE  R6,R3,0                                                       
            BC    1,*-4                                                         
*                                                                               
            IF (4) THEN                                                         
               L     R3,STB_nStrLen-STB_obj(,R5)                                
               LA    R2,1(,R3)                                                  
               ST    R2,G_GTSTSIZ                                               
*                                                                               
               LA    R1,WORKP06                                                 
               MVC   0(4,R1),G_GTSTSIZ                                          
               LA    R15,stringToSplit_P06                                      
               ST    R15,4(,R1)                                                 
               L     R15,G_GTST                                                 
               BASR  R14,R15                                                    
*                                                                               
               L     R0,stringToSplit_P06                                       
               LR    R1,R2                                                      
               LR    R14,R4                                                     
               LR    R15,R1                                                     
               MVCL  R0,R14                                                     
*                                                                               
               L     R4,stringToSplit_P06                                       
               L     R7,STB_nStrLen-STB_obj(,R5)                                
*                                                                               
               LR    R6,R4                                                      
*                                                                               
               L     R1,G_TRT_ANY_BUT_SPACE                                     
*                                                                               
               TRTE  R6,R3,0                                                    
               BC    1,*-4                                                      
*                                                                               
               DO WHILE=(4)                                                     
                  MVI   0(R6),X'00'                                             
*                                                                               
                  ISTR_Set OBJECT=ISTR_tgt_P06,WORK=WORKP06,STR=0(,R4)          
*                                                                               
                  MVI   varBuildTgtP06,X'00'                                    
                  MVC   varBuildTgtP06+1(VARIANT_SIZ-1),varBuildTgtP06          
                  MVC   varBuildTgtP06+(vt-VARIANT)(4),=A(VT_UNKNOWN)           
                  MVC   varBuildTgtP06+(value-VARIANT)(4),ISTR_tgt_P06          
*                                                                               
                  IAV2_Insert OBJECT=PARMIAV2TGTSP06,WORK=WORKP06,     X        
               INDEX_IN=currIndex_P06,VARIANT_IN=varBuildTgtP06                 
*                                                                               
                  ASI   currIndex_P06,1                                         
*                                                                               
                  LA    R6,1(,R6)                                               
                  BCTR  R7,R0                                                   
*                                                                               
                  L     R1,G_TRT_ONLY_SPACE                                     
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
*                                                                               
                  LR    R4,R6                                                   
*                                                                               
                  L     R1,G_TRT_ANY_BUT_SPACE                                  
*                                                                               
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
               ENDDO                                                            
*                                                                               
               MVI   0(R6),X'00'                                                
*                                                                               
               ISTR_Set OBJECT=ISTR_tgt_P06,WORK=WORKP06,STR=0(,R4)             
*                                                                               
               MVI   varBuildTgtP06,X'00'                                       
               MVC   varBuildTgtP06+1(VARIANT_SIZ-1),varBuildTgtP06             
               MVC   varBuildTgtP06+(vt-VARIANT)(4),=A(VT_UNKNOWN)              
               MVC   varBuildTgtP06+(value-VARIANT)(4),ISTR_tgt_P06             
*                                                                               
               IAV2_Insert OBJECT=PARMIAV2TGTSP06,WORK=WORKP06,        X        
               INDEX_IN=currIndex_P06,VARIANT_IN=varBuildTgtP06                 
*                                                                               
               ASI   currIndex_P06,1                                            
            ELSE                                                                
               ISTR_Set OBJECT=ISTR_tgt_P06,WORK=WORKP06,STR=0(,R4)             
*                                                                               
               MVI   varBuildTgtP06,X'00'                                       
               MVC   varBuildTgtP06+1(VARIANT_SIZ-1),varBuildTgtP06             
               MVC   varBuildTgtP06+(vt-VARIANT)(4),=A(VT_UNKNOWN)              
               MVC   varBuildTgtP06+(value-VARIANT)(4),ISTR_tgt_P06             
*                                                                               
               IAV2_Insert OBJECT=PARMIAV2TGTSP06,WORK=WORKP06,        X        
               INDEX_IN=currIndex_P06,VARIANT_IN=varBuildTgtP06                 
*                                                                               
               ASI   currIndex_P06,1                                            
            ENDIF                                                               
*                                                                               
            ISTR_Release OBJECT=ISTR_tgt_P06,WORK=WORKP06                       
            MVC   ISTR_tgt_P06,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IAV2_Count OBJECT=PARMIAV2TGTSP06,WORK=WORKP06,               X        
               COUNT_OUT=buildtgtsCount_P06                                     
*                                                                               
         IF (CLC,buildtgtsCount_P06,EQ,=A(0)) THEN                              
            IF (CLC,firstRuleStatement,NE,=A(0)) THEN                           
               L     R2,firstRuleStatement                                      
               L     R2,ST5_ITFO_tiTargets-ST5_obj(,R2)                         
               MVC   PARMISTBPARMSP06,TFO_ISTB_token-TFO_obj(R2)                
*                                                                               
               MVC   firstRuleStatement,=A(0)                                   
*                                                                               
               B     PRS#06_RETRY                                               
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
PRS#06_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
EMPTYNAME_P06                DC    X'00'                                        
EMPTYNAMEPTR_P06             DC    A(EMPTYNAME_P06)                             
*                                                                               
                             DS    0F                                           
PRS#60A_P06                  DC    A(PRS#60)                                    
*                                                                               
WORKDSAP06                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP06                      DS    4A                                           
*                                                                               
PARMISTBPARMSP06             DS    A                                            
PARMIAV2TGTSP06              DS    A                                            
*                                                                               
ISTB_resolved_P06            DS    A                                            
ISTR_tgt_P06                 DS    A                                            
*                                                                               
stringToSplit_P06            DS    A                                            
currIndex_P06                DS    F                                            
nextIndex_P06                DS    F                                            
buildtgtsCount_P06           DS    F                                            
*                                                                               
varBuildTgtP06               DS    CL(VARIANT_SIZ)                              
*                                                                               
WORKDSAP06_SIZ               EQU   *-WORKDSAP06                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS BuildTargets                                                             
*                                                                               
PRS#07   CEEENTRY AUTO=WORKDSAP07_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP07,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   PARMIAV2TGTSP07,4(R1) * Parm 2 is IAV2 of target names           
*                                                                               
         MVI   varBuildTgtP07,X'00'                                             
         MVC   varBuildTgtP07+1(VARIANT_SIZ-1),varBuildTgtP07                   
*                                                                               
         MVI   varTargetP07,X'00'                                               
         MVC   varTargetP07+1(VARIANT_SIZ-1),varTargetP07                       
*                                                                               
         MVC   currIndexP07,=F'-1'                                              
*                                                                               
         IAV2_Next OBJECT=PARMIAV2TGTSP07,WORK=WORKP07,                X        
               INDEX_IN=currIndexP07,INDEXPTR_OUT=nextIndexP07,        X        
               VARIANT_OUT=varBuildTgtP07                                       
*                                                                               
         DO WHILE=(CLC,varBuildTgtP07+vt-VARIANT(4),NE,=A(VT_NULL))             
            MVC   currIndexP07,nextIndexP07                                     
*                                                                               
            L     R2,varBuildTgtP07+value-VARIANT                               
            L     R3,STR_lpString-STR_obj(,R2)                                  
*                                                                               
            IF (CLC,STR_nStrLen-STR_obj(4,R2),EQ,=A(1)),ANDIF,         X        
               (CLI,0(R3),EQ,C'1'),OR,(CLI,0(R3),EQ,C'0') THEN                  
               B     PRS#07_SKIP_TGT                                            
            ENDIF                                                               
*                                                                               
            MINSTANT GUID=G_IFFO_GUID,WORK=WORKP07,OBJPTR=IFFO_P07              
*                                                                               
            IAVL_Query OBJECT=IAVL_targets,WORK=WORKP07,               X        
               NAME_IN=STR_lpString-STR_obj(R2),                       X        
               VARIANT_OUT=varTargetP07                                         
*                                                                               
            IF (CLC,varTargetP07+vt-VARIANT(4),NE,=A(VT_NULL)) THEN             
               LA    R1,WORKP07                                                 
               MVC   0(4,R1),varTargetP07+(value-VARIANT)                       
               L     R15,varBuildTgtP07+value-VARIANT                           
               MVC   4(4,R1),STR_lpString-STR_obj(R15)                          
               LA    R15,shouldBuildP07                                         
               ST    R15,8(,R1)                                                 
               LA    R15,IFFO_P07                                               
               ST    R15,12(,R1)                                                
               L     R15,PRS#70A_P07 * See if target should be built            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#07_RET                                                 
*                                                                               
               IF (CLI,shouldBuildP07,EQ,C'Y') THEN                             
                  ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP07                      
*                                                                               
                  L     R15,IFFO_P07                                            
                  IF (CLI,FFO_ftype-FFO_obj(R15),EQ,C'.') THEN                  
                     LA    R7,MAK307I_P07                                       
                  ELSE                                                          
                     LA    R7,MAK306I_P07                                       
                  ENDIF                                                         
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP07,   X        
               ZSTR=0(,R7)                                                      
*                                                                               
                  ISTB_AppendString OBJECT=G_ISTB_tmp,WORK=WORKP07,    X        
               ISTR=varBuildTgtP07+(value-VARIANT)                              
*                                                                               
                  L     R2,G_ISTB_tmp                                           
                  L     R2,STB_lpBuf-STB_obj(,R2)                               
                  ILOG_Write OBJECT=G_ILOG,WORK=WORKP07,LINE=0(,R2),   X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
*                                                                               
                  LA    R1,WORKP07                                              
                  LA    R15,varTargetP07                                        
                  ST    R15,0(,R1)                                              
                  L     R15,varBuildTgtP07+value-VARIANT                        
                  MVC   4(4,R1),STR_lpString-STR_obj(R15)                       
                  MVC   8(4,R1),IFFO_P07                                        
                  L     R15,PRS#71A_P07                                         
                  BASR  R14,R15  * Perform build statements                     
               ENDIF                                                            
*                                                                               
            ELSE                                                                
*                                                                               
               L     R2,STR_lpString-STR_obj(,R2)                               
*                                                                               
               IFMG_Stat OBJECT=G_IFMG,WORK=WORKP07,FILE=0(,R2),       X        
               IFFO=IFFO_P07                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#07_RET                                                 
*                                                                               
               L     R3,IFFO_P07                                                
               IF (CLI,FFO_exists-FFO_obj(R3),NE,C'Y') THEN                     
                  ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP07                      
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP07,   X        
               ZSTR=MAK106E_P07                                                 
                  ISTB_AppendString OBJECT=G_ISTB_tmp,WORK=WORKP07,    X        
               ISTR=varBuildTgtP07+(value-VARIANT)                              
*                                                                               
                  L     R2,G_ISTB_tmp                                           
                  L     R2,STB_lpBuf-STB_obj(,R2)                               
                  ILOG_Write OBJECT=G_ILOG,WORK=WORKP07,LINE=0(,R2),   X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
                  MVC   G_RETCODE,=A(8)                                         
                  B     PRS#07_RET                                              
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
PRS#07_SKIP_TGT EQU   *                                                         
            IAV2_Next OBJECT=PARMIAV2TGTSP07,WORK=WORKP07,             X        
               INDEX_IN=currIndexP07,INDEXPTR_OUT=nextIndexP07,        X        
               VARIANT_OUT=varBuildTgtP07                                       
         ENDDO                                                                  
*                                                                               
*        MVI   firstBuild,C'N'                                                  
*                                                                               
PRS#07_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK106E_P07                  DC    C'MAK106E Target not found: ',X'00'          
                             DS    0F                                           
MAK306I_P07                  DC    C'MAK307I Building target: ',X'00'           
                             DS    0F                                           
MAK307I_P07                  DC    C'MAK307I Building phony target: ',XX        
               '00'                                                             
*                                                                               
                             DS    0F                                           
PRS#70A_P07                  DC    A(PRS#70)                                    
PRS#71A_P07                  DC    A(PRS#71)                                    
*                                                                               
WORKDSAP07                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
PARMIAV2TGTSP07              DS    A                                            
*                                                                               
WORKP07                      DS    4A                                           
*                                                                               
IFFO_P07                     DS    A                                            
currIndexP07                 DS    F                                            
nextIndexP07                 DS    F                                            
varBuildTgtP07               DS    CL(VARIANT_SIZ)                              
varTargetP07                 DS    CL(VARIANT_SIZ)                              
shouldBuildP07               DS    C                                            
*                                                                               
WORKDSAP07_SIZ               EQU   *-WORKDSAP07                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS resolve CALL statement                                                   
*                                                                               
PRS#60   CEEENTRY AUTO=WORKDSAP60_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP60,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   inputZStr_P60,0(R1) * Parm 1 is input zero term string           
*                                                                               
         L     R7,4(,R1)         * Parm 2 is output ISTR ptr                    
         ST    R7,outputISTBPtr_P60 * Save in local var                         
*                                                                               
         IF (TM,8(R1),X'80',Z) THEN                                             
            MVC   lineNo_P60,8(R1) * Parm 3 is line number                      
            L     R15,12(,R1)    * Parm 4 is IFFO object                        
            NILH  R15,X'7FFF'    * Turn off high order bit                      
            ST    R15,IFFO_P60   * Save in local var                            
         ELSE                                                                   
            L     R15,8(,R1)     * Parm 3 is line number                        
            NILH  R15,X'7FFF'    * Turn off high order bit                      
            ST    R15,lineNo_P60 * Save in local var                            
            MVC   IFFO_P60,=A(0) * No IFFO passed in parm 4                     
         ENDIF                                                                  
*                                                                               
         IF (CLC,0(4,R7),EQ,=A(0)) THEN                                         
            MINSTANT GUID=G_ISTB_GUID,WORK=WORKP60,OBJPTR=0(,R7)                
         ELSE                                                                   
            ISTB_Init OBJECT=0(,R7),WORK=WORKP60                                
         ENDIF                                                                  
*                                                                               
*        Instantiate a new IINS object                                          
         MINSTANT GUID=G_IINS_GUID,WORK=WORKP60,OBJPTR=IINS_P60                 
*                                                                               
*        Initialize input from string                                           
         IINS_Init OBJECT=IINS_P60,WORK=WORKP60,ZSTR=inputZStr_P60,    X        
               LINENO=lineNo_P60                                                
*                                                                               
*        Instantiate a new IIFO object                                          
         MINSTANT GUID=G_IIFO_GUID,WORK=WORKP60,OBJPTR=IIFO_P60                 
*                                                                               
*        Instantiate a new IPSS object                                          
         MINSTANT GUID=G_IPSS_GUID,WORK=WORKP60,OBJPTR=IPSS_P60                 
*                                                                               
         IPSS_SetPhase OBJECT=IPSS_P60,WORK=WORKP60,PHASE==C'0'                 
*                                                                               
*        Instantiate a new ITOK object                                          
         MINSTANT GUID=G_ITOK_GUID,WORK=WORKP60,OBJPTR=ITOK_P60                 
*                                                                               
         ITOK_Init OBJECT=ITOK_P60,WORK=WORKP60,IIN=IINS_P60,          X        
               IIFO=IIFO_P60                                                    
*                                                                               
         MVC   ITFO_ti_P60,=A(0)                                                
*                                                                               
         LA    R1,WORKP60                                                       
         MVC   0(4,R1),IPSS_P60                                                 
         MVC   4(4,R1),ITOK_P60                                                 
         XR    R15,R15                                                          
         IC    R15,=C'N'                                                        
         ST    R15,8(,R1)                                                       
         LA    R15,ITFO_ti_P60                                                  
         ST    R15,12(,R1)                                                      
         MVC   16(4,R1),IFFO_P60                                                
         L     R15,PRS#94A_P60 * parseCall                                      
         BASR  R14,R15                                                          
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#60_RET                                                       
*                                                                               
         L     R6,ITFO_ti_P60                                                   
*                                                                               
         ISTB_AppendString OBJECT=0(,R7),WORK=WORKP60,                 X        
               ISTR=TFO_ISTB_token-TFO_obj(R6)                                  
*                                                                               
         ITFO_Release OBJECT=ITFO_ti_P60,WORK=WORKP60                           
*                                                                               
         ITOK_Release OBJECT=ITOK_P60,WORK=WORKP60                              
*                                                                               
         IPSS_Release OBJECT=IPSS_P60,WORK=WORKP60                              
*                                                                               
         IIFO_Release OBJECT=IIFO_P60,WORK=WORKP60                              
*                                                                               
         IINS_Release OBJECT=IINS_P60,WORK=WORKP60                              
*                                                                               
PRS#60_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
PRS#94A_P60                  DC    A(PRS#94)                                    
*                                                                               
WORKDSAP60                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP60                      DS    5A                                           
*                                                                               
inputZStr_P60                DS    A                                            
outputISTBPtr_P60            DS    A                                            
lineNo_P60                   DS    F                                            
IFFO_P60                     DS    A                                            
*                                                                               
IINS_P60                     DS    A                                            
IIFO_P60                     DS    A                                            
IPSS_P60                     DS    A                                            
ITOK_P60                     DS    A                                            
*                                                                               
ITFO_ti_P60                  DS    A                                            
*                                                                               
WORKDSAP60_SIZ               EQU   *-WORKDSAP60                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS resolve recipe assignment statement                                      
*                                                                               
PRS#61   CEEENTRY AUTO=WORKDSAP61_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP61,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IST1_P61,0(R1)    * Parm 1 is assignment statement               
*                                                                               
         MVC   IFFO_P61,4(R1)    * Parm 2 is IFFO object                        
*                                                                               
         L     R6,IST1_P61                                                      
*                                                                               
*        Instantiate a new IINS object                                          
         MINSTANT GUID=G_IINS_GUID,WORK=WORKP61,OBJPTR=IINS_P61                 
*                                                                               
         L     R5,ST1_ITFO_tiValue-ST1_obj(,R6)                                 
         L     R4,TFO_ISTB_token-TFO_obj(,R5)                                   
*                                                                               
*        Initialize input from string                                           
         IINS_Init OBJECT=IINS_P61,WORK=WORKP61,                       X        
               ZSTR=STB_lpBuf-STB_obj(R4),                             X        
               LINENO=TFO_nLine-TFO_obj(R5)                                     
*                                                                               
*        Instantiate a new IIFO object                                          
         MINSTANT GUID=G_IIFO_GUID,WORK=WORKP61,OBJPTR=IIFO_P61                 
*                                                                               
*        Instantiate a new IPSS object                                          
         MINSTANT GUID=G_IPSS_GUID,WORK=WORKP61,OBJPTR=IPSS_P61                 
*                                                                               
*        Set phase 2                                                            
         IPSS_SetPhase OBJECT=IPSS_P61,WORK=WORKP61,PHASE==C'2'                 
*                                                                               
*        Push IN_RECIPE state                                                   
         IPSS_Push OBJECT=IPSS_P61,WORK=WORKP61,                       X        
               STATE_IN==A(PARSER_STATE_IN_RECIPE)                              
*                                                                               
*        Push IN_ASSIGNMENT1 state                                              
         IPSS_Push OBJECT=IPSS_P61,WORK=WORKP61,                       X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT1)                         
*                                                                               
*        Instantiate a new ITOK object                                          
         MINSTANT GUID=G_ITOK_GUID,WORK=WORKP61,OBJPTR=ITOK_P61                 
*                                                                               
         ITOK_Init OBJECT=ITOK_P61,WORK=WORKP61,IIN=IINS_P61,          X        
               IIFO=IIFO_P61                                                    
*                                                                               
         MVC   ITFO_ti_P61,=A(0)                                                
*                                                                               
         LA    R1,WORKP61                                                       
         MVC   0(4,R1),IPSS_P61                                                 
         MVC   4(4,R1),ITOK_P61                                                 
         MVC   8(4,R1),ST1_ITFO_tiName-ST1_obj(R6)                              
         MVC   12(4,R1),ST1_ITFO_tiOperator-ST1_obj(R6)                         
         XR    R15,R15                                                          
         IC    R15,=C'N'                                                        
         ST    R15,16(,R1)                                                      
         MVC   20(4,R1),IFFO_P61                                                
         L     R15,PRS#95A_P61   * parseAssignment                              
         BASR  R14,R15                                                          
*                                                                               
         ITOK_Release OBJECT=ITOK_P61,WORK=WORKP61                              
*                                                                               
         IPSS_Release OBJECT=IPSS_P61,WORK=WORKP61                              
*                                                                               
         IIFO_Release OBJECT=IIFO_P61,WORK=WORKP61                              
*                                                                               
         IINS_Release OBJECT=IINS_P61,WORK=WORKP61                              
*                                                                               
PRS#61_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
PRS#95A_P61                  DC    A(PRS#95)                                    
*                                                                               
WORKDSAP61                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP61                      DS    6A                                           
*                                                                               
IST1_P61                     DS    A                                            
IFFO_P61                     DS    A                                            
*                                                                               
IINS_P61                     DS    A                                            
IIFO_P61                     DS    A                                            
IPSS_P61                     DS    A                                            
ITOK_P61                     DS    A                                            
*                                                                               
ITFO_ti_P61                  DS    A                                            
*                                                                               
WORKDSAP61_SIZ               EQU   *-WORKDSAP61                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS determineShouldBuild                                                     
*                                                                               
PRS#70   CEEENTRY AUTO=WORKDSAP70_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP70,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         ST    R8,THISP70        * Save in local var                            
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   PARMSTMTIXP70,0(R1) * Parm 1 is rule statement index             
*                                                                               
         MVC   PARMFNAMEP70,4(R1) * Parm 2 is file name                         
*                                                                               
         L     R15,8(,R1)        * Parm 3 is ptr to should build flag           
         ST    R15,PARMSHOULDBUILDP70 * Save in local var                       
         MVI   0(R15),C'N'       * Initialize to don't build                    
*                                                                               
         L     R7,12(,R1)        * Parm 4 is ptr to IFFO object                 
         ST    R7,PARMIFFOPTRP70 * Save in local var                            
*                                                                               
         L     R7,0(,R7)                                                        
TGTFFO   USING FFO_obj,R7                                                       
*                                                                               
*        See if the target is a phony                                           
         MVI   varP70,X'00'                                                     
         MVC   varP70+1(VARIANT_SIZ-1),varP70                                   
*                                                                               
         IAVL_Query OBJECT=IAVL_phonies,WORK=WORKP70,                  X        
               NAME_IN=PARMFNAMEP70,VARIANT_OUT=varP70                          
*                                                                               
         IF (CLC,varP70+vt-VARIANT(4),EQ,=A(VT_UI4)) THEN                       
            MVI   TGTFFO.FFO_ftype,C'.'                                         
            L     R15,PARMSHOULDBUILDP70                                        
            MVI   0(R15),C'Y'                                                   
         ENDIF                                                                  
*                                                                               
         L     R2,PARMFNAMEP70                                                  
         L     R3,PARMIFFOPTRP70                                                
*                                                                               
         IFMG_Stat OBJECT=G_IFMG,WORK=WORKP70,FILE=0(,R2),IFFO=0(R3)            
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#70_RET                                                       
*                                                                               
         IF (CLI,TGTFFO.FFO_exists,EQ,C'Y') THEN                                
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_INFO) THEN                             
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP70                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,      X        
               ZSTR=MAK313I_P70                                                 
               L     R2,TGTFFO.FFO_fname                                        
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,      X        
               ZSTR=0(,R2)                                                      
               IF (CLI,TGTFFO.FFO_ftype,EQ,C'M') THEN                           
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,   X        
               ZSTR==X'4D00'                                                    
                  L     R2,TGTFFO.FFO_member                                    
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,   X        
               ZSTR=0(,R2)                                                      
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,   X        
               ZSTR==X'5D00'                                                    
               ENDIF                                                            
*                                                                               
               IF (CLI,TGTFFO.FFO_ftype,EQ,C'M'),OR,                   X        
               (CLI,TGTFFO.FFO_ftype,EQ,C'U') THEN                              
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,   X        
               ZSTR=MAK313I_2_P70                                               
                  LA    R1,TGTFFO.FFO_alterDate                                 
                  BAL   R14,PRS#70_TIMEDATEZ                                    
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,   X        
               ZSTR=G_TIMEDATEZ                                                 
               ENDIF                                                            
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKP70,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
            ENDIF                                                               
         ELSE                                                                   
            IF (CLI,G_BUILDWHEN+2,EQ,C'M') THEN                                 
               L     R15,PARMSHOULDBUILDP70                                     
               MVI   0(R15),C'Y'                                                
*                                                                               
               L     R15,G_ILOG                                                 
               IF (CLI,13(R15),GE,LOG_LEVEL_INFO),AND,                 X        
               (CLI,TGTFFO.FFO_ftype,NE,C'.') THEN                              
                  ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP70                      
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,   X        
               ZSTR=MAK312I_P70                                                 
                  L     R2,TGTFFO.FFO_fname                                     
                  ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,   X        
               ZSTR=0(,R2)                                                      
                  IF (CLI,TGTFFO.FFO_ftype,EQ,C'M') THEN                        
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR==X'4D00'                                                    
                     L     R2,TGTFFO.FFO_member                                 
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR=0(,R2)                                                      
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR==X'5D00'                                                    
                  ENDIF                                                         
*                                                                               
                  L     R2,G_ISTB_tmp                                           
                  L     R2,STB_lpBuf-STB_obj(,R2)                               
                  ILOG_Write OBJECT=G_ILOG,WORK=WORKP70,LINE=0(,R2),   X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
               ENDIF                                                            
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         DROP  TGTFFO                                                           
*                                                                               
*        Get the rule statement                                                 
         IAV2_Query OBJECT=IAV2_statements,WORK=WORKP70,               X        
               INDEX_IN=PARMSTMTIXP70,VARIANT_OUT=varP70                        
*                                                                               
         L     R7,varP70+(value-VARIANT)                                        
         L     R6,ST5_ITFO_tiPrereqs-ST5_obj(,R7)                               
         L     R5,TFO_tokenType-TFO_obj(,R6)                                    
         IF (C,R5,EQ,=A(TOKEN_TYPE_NORMAL)),OR,                        X        
               (C,R5,EQ,=A(TOKEN_TYPE_VARIABLE)),OR,                   X        
               (C,R5,EQ,=A(TOKEN_TYPE_TARGETVAR)),OR,                  X        
               (C,R5,EQ,=A(TOKEN_TYPE_MEMBERVAR)) THEN                          
            L     R5,TFO_ISTB_token-TFO_obj(,R6)                                
*                                                                               
            MVC   ISTB_P70,=A(0)                                                
*                                                                               
            LA    R1,WORKP70                                                    
            MVC   0(4,R1),STB_lpBuf-STB_obj(R5)                                 
            LA    R15,ISTB_P70                                                  
            ST    R15,4(,R1)                                                    
            MVC   8(4,R1),TFO_nLine-TFO_obj(R6)                                 
            L     R15,PARMIFFOPTRP70                                            
            MVC   12(4,R1),0(R15)                                               
            OI    12(R1),X'80'                                                  
            L     R15,PRS#60A_P70 * Resolve CALL statement                      
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#70_RET                                                    
*                                                                               
            IST5_Release OBJECT=varP70+(value-VARIANT),WORK=WORKP70             
*                                                                               
            L     R5,ISTB_P70                                                   
            IF (CLC,STB_nStrLen-STB_obj(4,R5),EQ,=A(0)) THEN                    
               L     R15,PARMSHOULDBUILDP70                                     
               MVI   0(R15),C'Y'                                                
*                                                                               
               B     PRS#70_RET                                                 
            ENDIF                                                               
*                                                                               
*           Write info message about prereqs                                    
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_INFO) THEN                             
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP70                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,      X        
               ZSTR=MAK309I_P70                                                 
               L     R2,PARMFNAMEP70                                            
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,      X        
               ZSTR=0(,R2)                                                      
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,      X        
               ZSTR=MAK309I_2_P70                                               
               L     R2,ISTB_P70                                                
               L     R3,STB_lpBuf-STB_obj(,R2)                                  
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,      X        
               ZSTR=0(,R3)                                                      
               L     R2,G_ISTB_tmp                                              
               L     R3,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKP70,LINE=0(,R3),      X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
            ENDIF                                                               
*                                                                               
            MVI   varP70,X'00'                                                  
            MVC   varP70+1(VARIANT_SIZ-1),varP70                                
*                                                                               
*           Instantiate a new IAV2 object for targets to build                  
            MINSTANT GUID=G_IAV2_GUID,WORK=WORKP70,OBJPTR=IAV2_tgts_P70         
*                                                                               
            MVC   currIndex_P70,=A(0)                                           
*                                                                               
*           Instantiate a new ISTR object for target name                       
            MINSTANT GUID=G_ISTR_GUID,WORK=WORKP70,OBJPTR=ISTR_tgt_P70          
*                                                                               
            L     R5,ISTB_P70                                                   
            L     R6,STB_lpBuf-STB_obj(,R5)                                     
            L     R7,STB_nStrLen-STB_obj(,R5)                                   
*                                                                               
            L     R1,G_TRT_ANY_BUT_SPACE                                        
*                                                                               
            LR    R4,R6                                                         
*                                                                               
            TRTE  R6,R3,0                                                       
            BC    1,*-4                                                         
*                                                                               
            IF (4) THEN                                                         
               L     R3,STB_nStrLen-STB_obj(,R5)                                
               LA    R2,1(,R3)                                                  
               ST    R2,G_GTSTSIZ                                               
*                                                                               
               LA    R1,WORKP70                                                 
               MVC   0(4,R1),G_GTSTSIZ                                          
               LA    R15,stringToSplit_P70                                      
               ST    R15,4(,R1)                                                 
               L     R15,G_GTST                                                 
               BASR  R14,R15                                                    
*                                                                               
               L     R0,stringToSplit_P70                                       
               LR    R1,R2                                                      
               LR    R14,R4                                                     
               LR    R15,R1                                                     
               MVCL  R0,R14                                                     
*                                                                               
               L     R4,stringToSplit_P70                                       
               L     R7,STB_nStrLen-STB_obj(,R5)                                
*                                                                               
               LR    R6,R4                                                      
*                                                                               
               L     R1,G_TRT_ANY_BUT_SPACE                                     
*                                                                               
               TRTE  R6,R3,0                                                    
               BC    1,*-4                                                      
*                                                                               
               DO WHILE=(4)                                                     
                  MVI   0(R6),X'00'                                             
*                                                                               
                  ISTR_Set OBJECT=ISTR_tgt_P70,WORK=WORKP70,STR=0(,R4)          
*                                                                               
                  MVI   varP70,X'00'                                            
                  MVC   varP70+1(VARIANT_SIZ-1),varP70                          
                  MVC   varP70+(vt-VARIANT)(4),=A(VT_UNKNOWN)                   
                  MVC   varP70+(value-VARIANT)(4),ISTR_tgt_P70                  
*                                                                               
                  IAV2_Insert OBJECT=IAV2_tgts_P70,WORK=WORKP70,       X        
               INDEX_IN=currIndex_P70,VARIANT_IN=varP70                         
*                                                                               
                  ASI   currIndex_P70,1                                         
*                                                                               
                  LA    R6,1(,R6)                                               
                  BCTR  R7,R0                                                   
*                                                                               
                  L     R1,G_TRT_ONLY_SPACE                                     
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
*                                                                               
                  LR    R4,R6                                                   
*                                                                               
                  L     R1,G_TRT_ANY_BUT_SPACE                                  
*                                                                               
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
               ENDDO                                                            
*                                                                               
               MVI   0(R6),X'00'                                                
*                                                                               
               ISTR_Set OBJECT=ISTR_tgt_P70,WORK=WORKP70,STR=0(,R4)             
*                                                                               
               MVI   varP70,X'00'                                               
               MVC   varP70+1(VARIANT_SIZ-1),varP70                             
               MVC   varP70+(vt-VARIANT)(4),=A(VT_UNKNOWN)                      
               MVC   varP70+(value-VARIANT)(4),ISTR_tgt_P70                     
*                                                                               
               IAV2_Insert OBJECT=IAV2_tgts_P70,WORK=WORKP70,          X        
               INDEX_IN=currIndex_P70,VARIANT_IN=varP70                         
*                                                                               
               ASI   currIndex_P70,1                                            
            ELSE                                                                
               ISTR_Set OBJECT=ISTR_tgt_P70,WORK=WORKP70,STR=0(,R4)             
*                                                                               
               MVI   varP70,X'00'                                               
               MVC   varP70+1(VARIANT_SIZ-1),varP70                             
               MVC   varP70+(vt-VARIANT)(4),=A(VT_UNKNOWN)                      
               MVC   varP70+(value-VARIANT)(4),ISTR_tgt_P70                     
*                                                                               
               IAV2_Insert OBJECT=IAV2_tgts_P70,WORK=WORKP70,          X        
               INDEX_IN=currIndex_P70,VARIANT_IN=varP70                         
*                                                                               
               ASI   currIndex_P70,1                                            
            ENDIF                                                               
*                                                                               
            ISTR_Release OBJECT=ISTR_tgt_P70,WORK=WORKP70                       
            MVC   ISTR_tgt_P70,=A(0)                                            
*                                                                               
            IPRS_BuildTargets OBJECT=THISP70,WORK=WORKP70,             X        
               IAV2TGTS=IAV2_tgts_P70                                           
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#70_RET                                                    
*                                                                               
            L     R7,PARMIFFOPTRP70                                             
            L     R7,0(,R7)                                                     
TGTFFO      USING FFO_obj,R7                                                    
*                                                                               
            L     R15,PARMSHOULDBUILDP70                                        
*                                                                               
            IF (CLI,TGTFFO.FFO_ftype,NE,C'.'),AND,                     X        
               (CLI,0(R15),NE,C'Y') THEN                                        
               MVI   varP70,X'00'                                               
               MVC   varP70+1(VARIANT_SIZ-1),varP70                             
*                                                                               
               MVC   currIndex_P70,=F'-1'                                       
*                                                                               
               IAV2_Next OBJECT=IAV2_tgts_P70,WORK=WORKP70,            X        
               INDEX_IN=currIndex_P70,INDEXPTR_OUT=nextIndex_P70,      X        
               VARIANT_OUT=varP70                                               
*                                                                               
               DO WHILE=(CLC,varP70+vt-VARIANT(4),NE,=A(VT_NULL))               
                  MVC   currIndex_P70,nextIndex_P70                             
*                                                                               
                  L     R3,varP70+(value-VARIANT)                               
                  L     R2,STR_lpString-STR_obj(,R3)                            
*                                                                               
                  IF (CLC,STR_nStrLen-STR_obj(4,R3),EQ,=A(1)),AND,     X        
               (CLI,0(R2),EQ,C'1') THEN                                         
                     ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP70                   
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR=MAK315I_P70                                                 
                     L     R2,TGTFFO.FFO_fname                                  
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR=0(,R2)                                                      
                     IF (CLI,TGTFFO.FFO_ftype,EQ,C'M') THEN                     
                        ISTB_AppendZString OBJECT=G_ISTB_tmp,          X        
               WORK=WORKP70,ZSTR==X'4D00'                                       
                        L     R2,TGTFFO.FFO_member                              
                        ISTB_AppendZString OBJECT=G_ISTB_tmp,          X        
               WORK=WORKP70,ZSTR=0(,R2)                                         
                        ISTB_AppendZString OBJECT=G_ISTB_tmp,          X        
               WORK=WORKP70,ZSTR==X'5D00'                                       
                     ENDIF                                                      
*                                                                               
                     L     R2,G_ISTB_tmp                                        
                     L     R2,STB_lpBuf-STB_obj(,R2)                            
                     ILOG_Write OBJECT=G_ILOG,WORK=WORKP70,LINE=0(,R2),X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
*                                                                               
                     L     R15,PARMSHOULDBUILDP70                               
                     MVI   0(R15),C'Y'                                          
*                                                                               
                     ASMLEAVE                                                   
                  ENDIF                                                         
*                                                                               
                  IF (CLC,STR_nStrLen-STR_obj(4,R3),EQ,=A(1)),AND,     X        
               (CLI,0(R2),EQ,C'0') THEN                                         
                     B     PRS#70_NEXT_PREREQ                                   
                  ENDIF                                                         
*                                                                               
*                 Intantiate IFFO object for prereq                             
                  MINSTANT GUID=G_IFFO_GUID,WORK=WORKP70,              X        
               OBJPTR=IFFO_P70                                                  
*                                                                               
                  IFMG_Stat OBJECT=G_IFMG,WORK=WORKP70,FILE=0(,R2),    X        
               IFFO=IFFO_P70                                                    
*                                                                               
                  L     R6,IFFO_P70                                             
PREFFO            USING FFO_obj,R6                                              
*                                                                               
                  IF (CLI,PREFFO.FFO_exists,NE,C'Y') THEN                       
                     ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP70                   
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR=MAK116E_P70                                                 
                     L     R2,PREFFO.FFO_fname                                  
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR=0(,R2)                                                      
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR==X'4D00'                                                    
                     L     R2,PREFFO.FFO_member                                 
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR=0(,R2)                                                      
                     ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP70,X        
               ZSTR==X'5D00'                                                    
*                                                                               
                     L     R2,G_ISTB_tmp                                        
                     L     R2,STB_lpBuf-STB_obj(,R2)                            
                     ILOG_Write OBJECT=G_ILOG,WORK=WORKP70,LINE=0(,R2),X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
                     MVC   G_RETCODE,=A(8)                                      
                     B     PRS#70_RET                                           
                  ENDIF                                                         
*                                                                               
                  IF (CLI,PREFFO.FFO_ftype,EQ,C'M'),OR,                X        
               (CLI,PREFFO.FFO_ftype,EQ,C'U') THEN                              
                     IF (CLI,TGTFFO.FFO_ftype,EQ,C'M'),OR,             X        
               (CLI,TGTFFO.FFO_ftype,EQ,C'U') THEN                              
                        IF (CLI,G_BUILDWHEN+1,EQ,C'O'),AND,            X        
               (CLC,TGTFFO.FFO_alterDate,LT,PREFFO.FFO_alterDate) THEN          
                           ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP70             
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=MAK314I_P70                                    
                           L     R2,PREFFO.FFO_fname                            
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=0(,R2)                                         
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR==X'4D00'                                       
                           L     R2,PREFFO.FFO_member                           
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=0(,R2)                                         
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR==X'5D4000'                                     
                           LA    R1,PREFFO.FFO_alterDate                        
                           BAL   R14,PRS#70_TIMEDATEZ                           
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=G_TIMEDATEZ                                    
*                                                                               
                           L     R2,G_ISTB_tmp                                  
                           L     R2,STB_lpBuf-STB_obj(,R2)                      
                           ILOG_Write OBJECT=G_ILOG,WORK=WORKP70,      X        
               LINE=0(,R2),LOGLEVEL=LOG_LEVEL_ERROR                             
*                                                                               
                           L     R15,PARMSHOULDBUILDP70                         
                           MVI   0(R15),C'Y'                                    
*                                                                               
                           ASMLEAVE                                             
                        ENDIF                                                   
*                                                                               
                        IF (CLI,G_BUILDWHEN+1,EQ,C'U'),AND,            X        
               (CLC,TGTFFO.FFO_alterDate,NE,PREFFO.FFO_alterDate) THEN          
                           ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP70             
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=MAK317I_P70                                    
                           L     R2,PREFFO.FFO_fname                            
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=0(,R2)                                         
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR==X'4D00'                                       
                           L     R2,PREFFO.FFO_member                           
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=0(,R2)                                         
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR==X'5D4000'                                     
                           LA    R1,PREFFO.FFO_alterDate                        
                           BAL   R14,PRS#70_TIMEDATEZ                           
                           ISTB_AppendZString OBJECT=G_ISTB_tmp,       X        
               WORK=WORKP70,ZSTR=G_TIMEDATEZ                                    
*                                                                               
                           L     R2,G_ISTB_tmp                                  
                           L     R2,STB_lpBuf-STB_obj(,R2)                      
                           ILOG_Write OBJECT=G_ILOG,WORK=WORKP70,      X        
               LINE=0(,R2),LOGLEVEL=LOG_LEVEL_ERROR                             
*                                                                               
                           L     R15,PARMSHOULDBUILDP70                         
                           MVI   0(R15),C'Y'                                    
*                                                                               
                           ASMLEAVE                                             
                        ENDIF                                                   
                     ENDIF                                                      
                  ENDIF                                                         
*                                                                               
                  IFFO_Release OBJECT=IFFO_P70,WORK=WORKP70                     
                  MVC   IFFO_P70,=A(0)                                          
*                                                                               
PRS#70_NEXT_PREREQ EQU   *                                                      
*                                                                               
                  IAV2_Next OBJECT=IAV2_tgts_p70,WORK=WORKP70,         X        
               INDEX_IN=currIndex_P70,INDEXPTR_OUT=nextIndex_P70,      X        
               VARIANT_OUT=varP70                                               
               ENDDO                                                            
            ENDIF                                                               
*                                                                               
            IAV2_Release OBJECT=IAV2_tgts_P70,WORK=WORKP70                      
            MVC   IAV2_tgts_P70,=A(0)                                           
         ENDIF                                                                  
*                                                                               
PRS#70_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
* Format a date                                                                 
*                                                                               
PRS#70_TIMEDATEZ DS    0H                                                       
         MVC   G_TIMEDATEZ(19),=C'XXXX-XX-XX-XX:XX:XX'                          
         MVI   G_TIMEDATEZ+19,X'00'                                             
         MVC   G_TIMEDATEZ(4),0(R1)                                             
         MVC   G_TIMEDATEZ+5(2),4(R1)                                           
         MVC   G_TIMEDATEZ+8(2),6(R1)                                           
         MVC   G_TIMEDATEZ+11(2),8(R1)                                          
         MVC   G_TIMEDATEZ+14(2),10(R1)                                         
         MVC   G_TIMEDATEZ+17(2),12(R1)                                         
*                                                                               
         BR    R14                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK309I_P70                  DC    C'MAK309I Checking target: ',X'00'           
                             DS    0F                                           
MAK309I_2_P70                DC    C' prereqs: ',X'00'                          
                             DS    0F                                           
MAK116E_P70                  DC    C'MAK116E Prerequisite does not exisX        
               t ',X'00'                                                        
                             DS    0F                                           
MAK312I_P70                  DC    C'MAK312I Target does not exist ',X'X        
               00'                                                              
                             DS    0F                                           
MAK313I_P70                  DC    C'MAK313I Pre-existing target ',X'00X        
               '                                                                
                             DS    0F                                           
MAK313I_2_P70                DC    C' was last updated on ',X'00'               
                             DS    0F                                           
MAK314I_P70                  DC    C'MAK314I Prereq is newer ',X'00'            
                             DS    0F                                           
MAK315I_P70                  DC    C'MAK315I Target should be built uncX        
               onditionally ',X'00'                                             
                             DS    0F                                           
MAK317I_P70                  DC    C'MAK317I Prereq is unequal ',X'00'          
*                                                                               
                             DS    0F                                           
PRS#60A_P70                  DC    A(PRS#60)                                    
*                                                                               
WORKDSAP70                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
THISP70                      DS    A                                            
WORKP70                      DS    4A                                           
*                                                                               
PARMSTMTIXP70                DS    F                                            
PARMFNAMEP70                 DS    A                                            
PARMSHOULDBUILDP70           DS    A                                            
PARMIFFOPTRP70               DS    A                                            
*                                                                               
ISTB_P70                     DS    A                                            
ISTR_tgt_P70                 DS    A                                            
IAV2_tgts_P70                DS    A                                            
IFFO_P70                     DS    A                                            
*                                                                               
stringToSplit_P70            DS    A                                            
currIndex_P70                DS    F                                            
nextIndex_P70                DS    F                                            
*                                                                               
varP70                       DS    CL(VARIANT_SIZ)                              
*                                                                               
WORKDSAP70_SIZ               EQU   *-WORKDSAP70                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS performBuildStatements                                                   
*                                                                               
PRS#71   CEEENTRY AUTO=WORKDSAP71_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP71,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   varTargetPtr_P71,0(R1) * Parm 1 is ptr to stmt nr var            
*                                                                               
         MVC   zstrTargetName_P71,4(R1) * Parm 2 is target name                 
*                                                                               
         MVC   IFFO_P71,8(R1)    * Parm 3 is IFFO object                        
*                                                                               
         L     R2,varTargetPtr_P71                                              
*                                                                               
         MVC   currIndex_P71,value-VARIANT(R2)                                  
*                                                                               
         MVI   varStatement_P71,X'00'                                           
         MVC   varStatement_P71+1(VARIANT_SIZ-1),varStatement_P71               
*                                                                               
         IAV2_Next2 OBJECT=IAV2_statements,WORK=WORKP71,               X        
               INDEX_IN=currIndex_P71,INDEXPTR_OUT=nextIndex_P71,      X        
               VARIANT_OUT=varStatement_P71                                     
*                                                                               
         DO WHILE=(CLC,varStatement_P71+vt-VARIANT(4),EQ,=A(VT_UNKNOWN)X        
               ,AND,CLC,G_RETCODE,EQ,=A(0))                                     
            L     R7,varStatement_P71+value-VARIANT                             
            IF (CLI,ST1_recipeStatement-ST1_obj(R7),NE,C'Y') THEN               
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            MVC   currIndex_P71,nextIndex_P71                                   
*                                                                               
            IF (CLI,ST2_statementType-ST2_obj(R7),EQ,C'C') THEN                 
               L     R6,ST2_ITFO_tiCall-ST2_obj(,R7)                            
               L     R5,TFO_ISTB_token-TFO_obj(,R6)                             
*                                                                               
               MVC   ISTB_exec_P71,=A(0)                                        
*                                                                               
               LA    R1,WORKP71                                                 
               MVC   0(4,R1),STB_lpBuf-STB_obj(R5)                              
               LA    R15,ISTB_exec_P71                                          
               ST    R15,4(,R1)                                                 
               MVC   8(4,R1),TFO_nLine-TFO_obj(R6)                              
               MVC   12(4,R1),IFFO_P71                                          
               OI    12(R1),X'80'                                               
               L     R15,PRS#60A_P71 * Resolve CALL statement                   
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#71_RET                                                 
*                                                                               
               IREX_Exec OBJECT=G_IREX,WORK=WORKP71,                   X        
               ISTBEXEC=ISTB_exec_P71,ISTBPTR_RETVAL==A(0)                      
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#71_RET                                                 
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
                  LR    R2,R1                                                   
*                                                                               
                  MVI   callExec_P71,X'00'                                      
                  MVC   callExec_P71+1(L'callExec_P71-1),callExec_P71           
*                                                                               
                  L     R5,ISTB_exec_P71                                        
                  L     R6,STB_lpBuf-STB_obj(,R5)                               
                  L     R7,STB_nStrLen-STB_obj(,R5)                             
*                                                                               
                  L     R1,G_TRT_ANY_BUT_SPACE                                  
*                                                                               
                  LR    R4,R6                                                   
*                                                                               
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
*                                                                               
                  IF (12) THEN                                                  
                     LR    R5,R6                                                
                     SR    R5,R4                                                
                     BCTR  R5,R0                                                
                     B     *+10                                                 
                     MVC   callExec_P71(1),0(R4)                                
                     EX    R5,*-6                                               
                  ENDIF                                                         
*                                                                               
                  MINSTANT GUID=G_ISTB_GUID,WORK=WORKP71,              X        
               OBJPTR=ISTB_error_P71                                            
*                                                                               
                  ISTB_AppendZString OBJECT=ISTB_error_P71,            X        
               WORK=WORKP71,ZSTR=MAK107E_P71                                    
*                                                                               
                  ISTB_AppendZString OBJECT=ISTB_error_P71,            X        
               WORK=WORKP71,ZSTR=callExec_P71                                   
*                                                                               
                  ISTB_AppendZString OBJECT=ISTB_error_P71,            X        
               WORK=WORKP71,ZSTR=MAK107E_P71_2                                  
*                                                                               
                  ISTB_AppendZString OBJECT=ISTB_error_P71,            X        
               WORK=WORKP71,ZSTR=0(,R2)                                         
*                                                                               
                  L     R6,ISTB_error_P71                                       
                  L     R6,STB_lpBuf-STB_obj(,R6)                               
                  ILOG_Write OBJECT=G_ILOG,WORK=WORKP71,LINE=0(,R6),   X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
                  MVC   G_RETCODE,=A(8)                                         
*                                                                               
                  ISTB_Release OBJECT=ISTB_error_P71,WORK=WORKP71               
                  MVC   ISTB_error_P71,=A(0)                                    
               ENDIF                                                            
*                                                                               
            ELSEIF (CLI,ST6_statementType-ST6_obj(R7),EQ,C'B') THEN             
               L     R6,ST6_ITFO_tiSh-ST6_obj(,R7)                              
               L     R5,TFO_ISTB_token-TFO_obj(,R6)                             
*                                                                               
               MVC   ISTB_exec_P71,=A(0)                                        
*                                                                               
               LA    R1,WORKP71                                                 
               MVC   0(4,R1),STB_lpBuf-STB_obj(R5)                              
               LA    R15,ISTB_exec_P71                                          
               ST    R15,4(,R1)                                                 
               MVC   8(4,R1),TFO_nLine-TFO_obj(R6)                              
               MVC   12(4,R1),IFFO_P71                                          
               OI    12(R1),X'80'                                               
               L     R15,PRS#60A_P71 * Resolve CALL statement                   
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#71_RET                                                 
*                                                                               
               IUSS_BPX1SPN OBJECT=G_IUSS,WORK=WORKP71,                X        
               ISTBSH=ISTB_exec_P71,ISTBPTR_RETVAL=ISTR_retval_P71              
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#71_RET                                                 
*                                                                               
            ELSEIF (CLI,ST1_statementType-ST1_obj(R7),EQ,C'A') THEN             
               LA    R1,WORKP71                                                 
               ST    R7,0(,R1)                                                  
               MVC   4(4,R1),IFFO_P71                                           
               L     R15,PRS#61A_P71 * Parse recipe assignment                  
               BASR  R14,R15                                                    
            ENDIF                                                               
*                                                                               
            IAV2_Next2 OBJECT=IAV2_statements,WORK=WORKP71,            X        
               INDEX_IN=currIndex_P71,INDEXPTR_OUT=nextIndex_P71,      X        
               VARIANT_OUT=varStatement_P71                                     
         ENDDO                                                                  
PRS#71_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK107E_P71                  DC    C'MAK107E REXX ',X'00'                       
MAK107E_P71_2                DC    C' returned ',X'00'                          
*                                                                               
                             DS    0F                                           
PRS#60A_P71                  DC    A(PRS#60) * resolve CALL statement           
PRS#61A_P71                  DC    A(PRS#61) * resolve recipe assignmnt         
*                                                                               
WORKDSAP71                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP71                      DS    4A                                           
*                                                                               
varTargetPtr_P71             DS    A                                            
zstrTargetName_P71           DS    A                                            
IFFO_P71                     DS    A                                            
currIndex_P71                DS    F                                            
nextIndex_P71                DS    F                                            
varStatement_P71             DS    CL(VARIANT_SIZ)                              
ISTB_exec_P71                DS    A                                            
ISTR_retval_P71              DS    A                                            
ISTB_error_P71               DS    A                                            
callExec_P71                 DS    CL9                                          
*                                                                               
WORKDSAP71_SIZ               EQU   *-WORKDSAP71                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS appendToken                                                              
*                                                                               
PRS#80   CEEENTRY AUTO=WORKDSAP80_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP80,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is ITFO object 1                      
ti1      USING TFO_obj,R8        * Addressability of ITFO                       
*                                                                               
         L     R7,4(,R1)         * Parm 2 is ITFO object 2                      
ti2      USING TFO_obj,R7        * Addressability of ITFO                       
*                                                                               
         L     R6,8(,R1)         * Parm 3 is indicator spaces                   
*                                                                               
         IF (C,R6,NE,=A(0)) THEN                                                
*                                                                               
*           If ti1 has length of 0 then don't bother                            
            L     R15,ti1.TFO_ISTB_token                                        
            IF (CLC,STB_nStrLen-STB_obj(4,R15),EQ,=A(0)) THEN                   
               B     PRS#80_NOSPACES                                            
            ENDIF                                                               
*                                                                               
            IF (CLC,ti2.TFO_nSpaces,NE,=A(0)),AND,                     X        
               (C,R6,GT,=A(1)) THEN                                             
               MVI   spaces_P80,C' '                                            
               MVC   spaces_P80+1(15),spaces_P80                                
               MVI   spaces_P80+15,X'00'                                        
*                                                                               
               IF (CLC,ti2.TFO_nSpaces,LE,=A(15)) THEN                          
                  LA    R2,spaces_P80                                           
                  A     R2,ti2.TFO_nSpaces                                      
                  MVI   0(R2),X'00'                                             
                  LA    R2,spaces_P80                                           
               ELSE                                                             
                  L     R3,ti2.TFO_nSpaces                                      
                  LA    R3,1(,R3)                                               
                  ST    R3,G_GTSTSIZ                                            
*                                                                               
                  LA    R1,WORKP80                                              
                  ST    R3,0(,R1)                                               
                  LA    R15,spaces_P80                                          
                  ST    R15,4(,R1)                                              
                  L     R15,G_GTST                                              
                  BASR  R14,R15                                                 
*                                                                               
                  L     R2,spaces_P80  * Initialize buffer                      
                  L     R3,ti2.TFO_nSpaces * to all                             
                  XR    R0,R0          * spaces                                 
                  XR    R1,R1                                                   
                  ICM   R1,8,=C' '                                              
                  MVCL  R2,R0                                                   
*                                                                               
                  AR    R2,R3                                                   
                  MVI   0(R2),X'00'                                             
*                                                                               
                  L     R2,spaces_P80                                           
               ENDIF                                                            
            ELSEIF (C,R6,EQ,=A(1)) THEN                                         
               MVI   spaces_P80,C' '                                            
               MVI   spaces_P80+1,X'00'                                         
               LA    R2,spaces_P80                                              
            ELSEIF (C,R6,EQ,=A(-1)) THEN                                        
               MVI   spaces_P80,C','                                            
               MVI   spaces_P80+1,X'00'                                         
               LA    R2,spaces_P80                                              
            ELSE                                                                
               B     PRS#80_NOSPACES                                            
            ENDIF                                                               
*                                                                               
            ISTB_AppendZString OBJECT=ti1.TFO_ISTB_token,WORK=WORKP80, X        
               ZSTR=0(,R2)                                                      
*                                                                               
PRS#80_NOSPACES EQU   *                                                         
*                                                                               
            IF (C,R6,GT,=A(1)),AND,                                    X        
               (CLC,ti2.TFO_nSpaces,GE,=A(16)) THEN                             
               MVC   spaces_P80(4),=A(0)                                        
*                                                                               
*              CALL  CEEFRST,(spaces_P80,FCP80),MF=(E,WORKP80)                  
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         L     R2,ti2.TFO_ISTB_token                                            
         L     R2,STB_lpBuf-STB_obj(,R2)                                        
         ISTB_AppendZString OBJECT=ti1.TFO_ISTB_token,WORK=WORKP80,    X        
               ZSTR=0(,R2)                                                      
*                                                                               
PRS#80_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAP80                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP80                      DS    4A                                           
*                                                                               
ITFO_ti1_P80                 DS    A                                            
ITFO_ti2_P80                 DS    A                                            
*                                                                               
spaces_P80                   DS    CL16                                         
*                                                                               
WORKDSAP80_SIZ               EQU   *-WORKDSAP80                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS strip extension                                                          
*                                                                               
PRS#81   CEEENTRY AUTO=WORKDSAP81_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP81,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is ISTB object                        
         ST    R8,PARMISTBINP81                                                 
*                                                                               
         L     R5,4(,R1)         * Parm 2 is ptr to ISTB out                    
         ST    R5,PARMISTBPTROUTP81 * Save in local var                         
*                                                                               
         IF (CLC,0(4,R5),EQ,=A(0)) THEN                                         
            MINSTANT GUID=G_ISTB_GUID,WORK=WORKP81,OBJPTR=0(,R5)                
         ELSE                                                                   
            ISTB_Init OBJECT=0(,R5),WORK=WORKP81                                
         ENDIF                                                                  
*                                                                               
         MVI   first_P81,C'Y'                                                   
*                                                                               
         L     R6,STB_lpBuf-STB_obj(,R8)                                        
         L     R7,STB_nStrLen-STB_obj(,R8)                                      
*                                                                               
         LR    R3,R7                                                            
         LA    R2,1(,R3)                                                        
         ST    R2,G_GTSTSIZ                                                     
*                                                                               
         LA    R1,WORKP81                                                       
         MVC   0(4,R1),G_GTSTSIZ                                                
         LA    R15,stringToSplit_P81                                            
         ST    R15,4(,R1)                                                       
         L     R15,G_GTST                                                       
         BASR  R14,R15                                                          
*                                                                               
         L     R0,stringToSplit_P81                                             
         L     R1,G_GTSTSIZ                                                     
         LR    R14,R6                                                           
         LR    R15,R1                                                           
         MVCL  R0,R14                                                           
*                                                                               
         L     R6,stringToSplit_P81                                             
         LR    R4,R6                                                            
*                                                                               
         L     R1,G_TRT_ANY_BUT_SPACE                                           
*                                                                               
         TRTE  R6,R3,0                                                          
         BC    1,*-4                                                            
*                                                                               
         DO WHILE=(12)                                                          
            MVI   0(R6),X'00'                                                   
*                                                                               
            XR    R14,R14                                                       
*                                                                               
            XR    R0,R0                                                         
            IC    R0,=C'.'                                                      
            LR    R2,R4                                                         
            LR    R3,R6                                                         
PRS#81_NEXT_POINT EQU   *                                                       
            SRST  R3,R2                                                         
            BRC   1,*-4                                                         
            IF (4) THEN                                                         
               LR    R14,R3                                                     
               LA    R2,1(,R3)                                                  
               LR    R3,R6                                                      
               B     PRS#81_NEXT_POINT                                          
            ENDIF                                                               
*                                                                               
            LTR   R14,R14                                                       
            IF (NZ) THEN                                                        
               MVI   0(R14),X'00'                                               
            ENDIF                                                               
*                                                                               
            IF (CLI,first_P81,EQ,C'Y') THEN                                     
               MVI   first_P81,C'N'                                             
            ELSE                                                                
               ISTB_AppendZString OBJECT=0(,R5),WORK=WORKP81,          X        
               ZSTR==X'4000'                                                    
            ENDIF                                                               
*                                                                               
            ISTB_AppendZString OBJECT=0(,R5),WORK=WORKP81,ZSTR=0(,R4)           
*                                                                               
            IF (C,R7,EQ,=A(0)) THEN                                             
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            LA    R6,1(,R6)                                                     
            BCTR  R7,R0                                                         
*                                                                               
            L     R1,G_TRT_ONLY_SPACE                                           
            TRTE  R6,R3,0                                                       
            BC    1,*-4                                                         
*                                                                               
            LR    R4,R6                                                         
*                                                                               
            L     R1,G_TRT_ANY_BUT_SPACE                                        
*                                                                               
            TRTE  R6,R3,0                                                       
            BC    1,*-4                                                         
         ENDDO                                                                  
*                                                                               
PRS#81_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAP81                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP81                      DS    4A                                           
*                                                                               
PARMISTBINP81                DS    A                                            
PARMISTBPTROUTP81            DS    A                                            
stringToSplit_P81            DS    A                                            
first_P81                    DS    C                                            
*                                                                               
WORKDSAP81_SIZ               EQU   *-WORKDSAP81                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parserException                                                          
*                                                                               
PRS#90   CEEENTRY AUTO=WORKDSAP90_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP90,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is ITFO object                        
         USING TFO_obj,R8        * Addressability of ITFO                       
*                                                                               
         MVC   EXCEPTIONTYPEP90,4(R1) * Parm 2 is exception type                
*                                                                               
         ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP90                               
*                                                                               
         IF (CLC,EXCEPTIONTYPEP90,EQ,=A(0)) THEN                                
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP90,         X        
               ZSTR=LIT01_0#90                                                  
         ELSE                                                                   
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP90,         X        
               ZSTR=LIT01_1#90                                                  
         ENDIF                                                                  
*                                                                               
         IF (CLC,TFO_ISTB_token,NE,=A(0)) THEN                                  
            ISTB_AppendString OBJECT=G_ISTB_tmp,WORK=WORKP90,          X        
               ISTR=TFO_ISTB_token                                              
*                                                                               
            LA    R7,LIT02#90                                                   
         ELSE                                                                   
            LA    R7,LIT02#90+1                                                 
         ENDIF                                                                  
*                                                                               
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP90,ZSTR=0(,R7)          
*                                                                               
         L     R15,TFO_nLine                                                    
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
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP90,ZSTR=0(,R7)          
*                                                                               
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP90,            X        
               ZSTR=LIT03#90                                                    
*                                                                               
         L     R15,TFO_pos                                                      
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
         ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP90,ZSTR=0(,R7)          
*                                                                               
TOK#90_WRITE EQU   *                                                            
*                                                                               
         L     R2,G_ISTB_tmp                                                    
         L     R2,STB_lpBuf-STB_obj(,R2)                                        
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP90,LINE=0(,R2),            X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
         MVC   G_RETCODE,=A(8)                                                  
*                                                                               
PRS#90_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
LIT01_0#90                   DC    C'MAK103E Unexpected token ',X'00'           
                             DS    0F                                           
LIT01_1#90                   DC    C'MAK118E Undefined variable ',X'00'         
                             DS    0F                                           
LIT02#90                     DC    C' at line ',X'00'                           
                             DS    0F                                           
LIT03#90                     DC    C' pos ',X'00'                               
*                                                                               
WORKDSAP90                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP90                      DS    3A                                           
*                                                                               
LINEPTRP90                   DS    A                                            
EXCEPTIONTYPEP90             DS    F                                            
*                                                                               
WORKDSAP90_SIZ               EQU   *-WORKDSAP90                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parseSh                                                                  
*                                                                               
PRS#93   CEEENTRY AUTO=WORKDSAP93_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP93,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P93,0(R1) * Parm 1 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P93,4(R1) * Parm 2 is Tokenizer                   
*                                                                               
         L     R15,8(,R1)        * Parm 3 is flag postponeRecipeStmt            
         STC   R15,postponeRecipeStmt_P93                                       
*                                                                               
         L     R15,IPSS_ps_P93                                                  
         MVC   phase_P93,PSS_cPhase-PSS_obj(R15)                                
         IF (CLI,phase_P93,NE,C'1') THEN                                        
            MVC   ITFO_tiPtr_P93,12(R1)                                         
            MVC   IFFO_P93,16(R1)                                               
         ELSE                                                                   
            MVC   ITFO_tiPtr_P93,=A(0)                                          
            MVC   IFFO_P93,=A(0)                                                
         ENDIF                                                                  
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP93,LINE=MAK416D_P93,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVC   ITFO_ti2_P93,=A(0)                                               
         MVC   ITFO_ti3_P93,=A(0)                                               
*                                                                               
         IPSS_Push OBJECT=IPSS_ps_P93,WORK=WORKP93,                    X        
               STATE_IN==A(PARSER_STATE_IN_SH1)                                 
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P93,WORK=WORKP93,     X        
               IPSS=IPSS_ps_P93,ITFOPTR=ITFO_ti2_P93                            
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#93_RET                                                       
*                                                                               
         L     R7,ITFO_ti2_P93                                                  
ti2      USING TFO_obj,R7                                                       
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))              
            LA    R1,WORKP93                                                    
            MVC   0(4,R1),IPSS_ps_P93                                           
            MVC   4(4,R1),ITOK_tokenizer_P93                                    
            MVC   8(4,R1),ITFO_ti2_P93                                          
            L     R15,PRS#97A_P93 * parseContinuation                           
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#93_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN             
            LA    R1,WORKP93                                                    
            MVC   0(4,R1),IPSS_ps_P93                                           
            MVC   4(4,R1),ITOK_tokenizer_P93                                    
            MVC   8(4,R1),ITFO_ti2_P93                                          
            XR    R14,R14                                                       
            IF (CLI,phase_P93,EQ,C'1') THEN                                     
               IC    R14,=C'N'                                                  
            ELSE                                                                
               IC    R14,=C'Y'                                                  
            ENDIF                                                               
            ST    R14,12(,R1)                                                   
            MVC   16(4,R1),IFFO_P93                                             
            L     R15,PRS#98A_P93 * parseVariable                               
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#93_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_TARGETVAR)),AND,   X        
               (CLI,phase_P93,NE,C'1'),AND,                            X        
               (CLC,IFFO_P93,NE,=A(0)) THEN                                     
            ISTB_Init OBJECT=ti2.TFO_ISTB_token,WORK=WORKP93                    
            L     R6,IFFO_P93                                                   
            L     R5,FFO_fname-FFO_obj(,R6)                                     
            ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,WORK=WORKP93, X        
               ZSTR=0(,R5)                                                      
            IF (CLI,FFO_ftype-FFO_obj(R6),EQ,C'M') THEN                         
               ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,           X        
               WORK=WORKP93,ZSTR==X'4D00'                                       
               L     R5,FFO_member-FFO_obj(,R6)                                 
               ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,           X        
               WORK=WORKP93,ZSTR=0(,R5)                                         
               ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,           X        
               WORK=WORKP93,ZSTR==X'5D00'                                       
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         L     R15,IPSS_ps_P93                                                  
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,   X        
               (CLI,phase_P93,NE,C'1'),AND,                            X        
               (CLC,IFFO_P93,NE,=A(0)) THEN                                     
            ISTB_Init OBJECT=ti2.TFO_ISTB_token,WORK=WORKP93                    
            L     R6,IFFO_P93                                                   
            L     R5,FFO_member-FFO_obj(,R6)                                    
            ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,WORK=WORKP93, X        
               ZSTR=0(,R5)                                                      
         ENDIF                                                                  
*                                                                               
         DROP  ti2                                                              
*                                                                               
         IPSS_AlterLastState OBJECT=IPSS_ps_P93,WORK=WORKP93,          X        
               STATE_IN==A(PARSER_STATE_IN_SH2)                                 
*                                                                               
         DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                      
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P93,WORK=WORKP93,  X        
               IPSS=IPSS_ps_P93,ITFOPTR=ITFO_ti3_P93                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#93_RET                                                    
*                                                                               
            L     R7,ITFO_ti3_P93                                               
ti3         USING TFO_obj,R7                                                    
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,       X        
               (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP93                                                 
               MVC   0(4,R1),IPSS_ps_P93                                        
               MVC   4(4,R1),ITOK_tokenizer_P93                                 
               MVC   8(4,R1),ITFO_ti3_P93                                       
               L     R15,PRS#97A_P93 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#93_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,       X        
               (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN          
               LA    R1,WORKP93                                                 
               MVC   0(4,R1),IPSS_ps_P93                                        
               MVC   4(4,R1),ITOK_tokenizer_P93                                 
               MVC   8(4,R1),ITFO_ti3_P93                                       
               XR    R14,R14                                                    
               IF (CLI,phase_P93,EQ,C'1') THEN                                  
                  IC    R14,=C'N'                                               
               ELSE                                                             
                  IC    R14,=C'Y'                                               
               ENDIF                                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P93                                          
               L     R15,PRS#98A_P93 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#93_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_TARGETVAR)),AND,X        
               (CLI,phase_P93,NE,C'1'),AND,                            X        
               (CLC,IFFO_P93,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti3.TFO_ISTB_token,WORK=WORKP93                 
               L     R6,IFFO_P93                                                
               L     R5,FFO_fname-FFO_obj(,R6)                                  
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP93,ZSTR=0(,R5)                                         
               IF (CLI,FFO_ftype-FFO_obj(R6),EQ,C'M') THEN                      
                  ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,        X        
               WORK=WORKP93,ZSTR==X'4D00'                                       
                  L     R5,FFO_member-FFO_obj(,R6)                              
                  ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,        X        
               WORK=WORKP93,ZSTR=0(,R5)                                         
                  ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,        X        
               WORK=WORKP93,ZSTR==X'5D00'                                       
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,X        
               (CLI,phase_P93,NE,C'1'),AND,                            X        
               (CLC,IFFO_P93,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti3.TFO_ISTB_token,WORK=WORKP93                 
               L     R6,IFFO_P93                                                
               L     R5,FFO_member-FFO_obj(,R6)                                 
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP93,ZSTR=0(,R5)                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),AND,  X        
               (CLC,ti3.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIX))           
               LA    R1,WORKP93                                                 
               MVC   0(4,R1),ITFO_ti2_P93                                       
               MVC   4(4,R1),ITFO_ti3_P93                                       
               MVC   8(4,R1),=A(2) * include all spaces                         
               L     R15,PRS#80A_P93 * appendToken                              
               BASR  R14,R15                                                    
            ENDIF                                                               
         ENDDO                                                                  
*                                                                               
         IPSS_Pop OBJECT=IPSS_ps_P93,WORK=WORKP93                               
*                                                                               
*        If phase is 1 create statement                                         
         IF (CLI,phase_P93,EQ,C'1') THEN                                        
*                                                                               
*           Instantiate a ShStatement                                           
            MINSTANT GUID=G_IST6_GUID,WORK=WORKP93,OBJPTR=IST6_sh_P93           
*                                                                               
*           Initialize ShStatement                                              
            IST6_Init OBJECT=IST6_sh_P93,WORK=WORKP93,                 X        
               ITFOSH=ITFO_ti2_P93,RECIPESTMT=postponeRecipeStmt_P93            
*                                                                               
            IAV2_Count OBJECT=IAV2_statements,WORK=WORKP93,            X        
               COUNT_OUT=count_P93                                              
*                                                                               
            MVI   varShToken_P93,X'00'                                          
            MVC   varShToken_P93+1(VARIANT_SIZ-1),varShToken_P93                
            MVC   varShToken_P93+(vt-VARIANT)(4),=A(VT_UNKNOWN)                 
            MVC   varShToken_P93+(value-VARIANT)(4),IST6_sh_P93                 
*                                                                               
            IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP93,           X        
               INDEX_IN=count_P93,VARIANT_IN=varShToken_P93                     
*                                                                               
            IST6_Release OBJECT=IST6_sh_P93,WORK=WORKP93                        
            MVC   IST6_sh_P93,=A(0)                                             
*                                                                               
            IF (CLC,ITFO_ti2_P93,NE,=A(0)) THEN                                 
               ITFO_Release OBJECT=ITFO_ti2_P93,WORK=WORKP93                    
               MVC   ITFO_ti2_P93,=A(0)                                         
            ENDIF                                                               
*                                                                               
*        Else if phase is 2 return ti2                                          
         ELSE                                                                   
            L     R15,ITFO_tiPtr_P93                                            
            MVC   0(4,R15),ITFO_ti2_P93                                         
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti3_P93,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti3_P93,WORK=WORKP93                       
            MVC   ITFO_ti3_P93,=A(0)                                            
         ENDIF                                                                  
*                                                                               
PRS#93_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK416D_P93                  DC    C'MAK416D Parsing SH statement'              
                             DC    X'00'                                        
*                                                                               
                             DS    0F                                           
PRS#80A_P93                  DC    A(PRS#80) * appendToken                      
PRS#97A_P93                  DC    A(PRS#97) * parseContinuation                
PRS#98A_P93                  DC    A(PRS#98) * parseVariable                    
*                                                                               
WORKDSAP93                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP93                      DS    5A                                           
*                                                                               
IPSS_ps_P93                  DS    A                                            
ITOK_tokenizer_P93           DS    A                                            
ITFO_tiPtr_P93               DS    A                                            
ITFO_ti2_P93                 DS    A                                            
ITFO_ti3_P93                 DS    A                                            
IST6_sh_P93                  DS    A                                            
IFFO_P93                     DS    A                                            
*                                                                               
count_P93                    DS    F                                            
varShToken_P93               DS    CL(VARIANT_SIZ)                              
postponeRecipeStmt_P93       DS    C                                            
phase_P93                    DS    C                                            
*                                                                               
WORKDSAP93_SIZ               EQU   *-WORKDSAP93                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parseCall                                                                
*                                                                               
PRS#94   CEEENTRY AUTO=WORKDSAP94_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP94,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P94,0(R1) * Parm 1 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P94,4(R1) * Parm 2 is Tokenizer                   
*                                                                               
         L     R15,8(,R1)        * Parm 3 is flag postponeRecipeStmt            
         STC   R15,postponeRecipeStmt_P94                                       
*                                                                               
         L     R15,IPSS_ps_P94                                                  
         MVC   phase_P94,PSS_cPhase-PSS_obj(R15)                                
*                                                                               
         IF (CLI,phase_P94,NE,C'1') THEN                                        
            MVC   ITFO_tiPtr_P94,12(R1)                                         
            MVC   IFFO_P94,16(R1)                                               
         ELSE                                                                   
            MVC   ITFO_tiPtr_P94,=A(0)                                          
            MVC   IFFO_P94,=A(0)                                                
         ENDIF                                                                  
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP94,LINE=MAK404D_P94,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVC   ITFO_ti2_P94,=A(0)                                               
         MVC   ITFO_ti3_P94,=A(0)                                               
*                                                                               
         IPSS_Push OBJECT=IPSS_ps_P94,WORK=WORKP94,                    X        
               STATE_IN==A(PARSER_STATE_IN_CALL1)                               
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P94,WORK=WORKP94,     X        
               IPSS=IPSS_ps_P94,ITFOPTR=ITFO_ti2_P94                            
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#94_RET                                                       
*                                                                               
         L     R7,ITFO_ti2_P94                                                  
ti2      USING TFO_obj,R7                                                       
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))              
            LA    R1,WORKP94                                                    
            MVC   0(4,R1),IPSS_ps_P94                                           
            MVC   4(4,R1),ITOK_tokenizer_P94                                    
            MVC   8(4,R1),ITFO_ti2_P94                                          
            L     R15,PRS#97A_P94 * parseContinuation                           
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#94_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN             
            LA    R1,WORKP94                                                    
            MVC   0(4,R1),IPSS_ps_P94                                           
            MVC   4(4,R1),ITOK_tokenizer_P94                                    
            MVC   8(4,R1),ITFO_ti2_P94                                          
            XR    R14,R14                                                       
            IF (CLI,phase_P94,EQ,C'1') THEN                                     
               IC    R14,=C'N'                                                  
            ELSE                                                                
               IC    R14,=C'Y'                                                  
            ENDIF                                                               
            ST    R14,12(,R1)                                                   
            MVC   16(4,R1),IFFO_P94                                             
            L     R15,PRS#98A_P94 * parseVariable                               
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#94_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_TARGETVAR)),AND,   X        
               (CLI,phase_P94,NE,C'1'),AND,                            X        
               (CLC,IFFO_P94,NE,=A(0)) THEN                                     
            ISTB_Init OBJECT=ti2.TFO_ISTB_token,WORK=WORKP94                    
            L     R6,IFFO_P94                                                   
            L     R5,FFO_fname-FFO_obj(,R6)                                     
            ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,WORK=WORKP94, X        
               ZSTR=0(,R5)                                                      
            IF (CLI,FFO_ftype-FFO_obj(R6),EQ,C'M') THEN                         
               ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,           X        
               WORK=WORKP94,ZSTR==X'4D00'                                       
               L     R5,FFO_member-FFO_obj(,R6)                                 
               ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,           X        
               WORK=WORKP94,ZSTR=0(,R5)                                         
               ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,           X        
               WORK=WORKP94,ZSTR==X'5D00'                                       
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,   X        
               (CLI,phase_P94,NE,C'1'),AND,                            X        
               (CLC,IFFO_P94,NE,=A(0)) THEN                                     
            ISTB_Init OBJECT=ti2.TFO_ISTB_token,WORK=WORKP94                    
            L     R6,IFFO_P94                                                   
            L     R5,FFO_member-FFO_obj(,R6)                                    
            ISTB_AppendZString OBJECT=ti2.TFO_ISTB_token,WORK=WORKP94, X        
               ZSTR=0(,R5)                                                      
         ENDIF                                                                  
*                                                                               
         DROP  ti2                                                              
*                                                                               
         IPSS_AlterLastState OBJECT=IPSS_ps_P94,WORK=WORKP94,          X        
               STATE_IN==A(PARSER_STATE_IN_CALL2)                               
*                                                                               
         DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                      
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P94,WORK=WORKP94,  X        
               IPSS=IPSS_ps_P94,ITFOPTR=ITFO_ti3_P94                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#94_RET                                                    
*                                                                               
            L     R7,ITFO_ti3_P94                                               
ti3         USING TFO_obj,R7                                                    
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,       X        
               (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP94                                                 
               MVC   0(4,R1),IPSS_ps_P94                                        
               MVC   4(4,R1),ITOK_tokenizer_P94                                 
               MVC   8(4,R1),ITFO_ti3_P94                                       
               L     R15,PRS#97A_P94 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#94_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,       X        
               (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN          
               LA    R1,WORKP94                                                 
               MVC   0(4,R1),IPSS_ps_P94                                        
               MVC   4(4,R1),ITOK_tokenizer_P94                                 
               MVC   8(4,R1),ITFO_ti3_P94                                       
               XR    R14,R14                                                    
               IF (CLI,phase_P94,EQ,C'1') THEN                                  
                  IC    R14,=C'N'                                               
               ELSE                                                             
                  IC    R14,=C'Y'                                               
               ENDIF                                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P94                                          
               L     R15,PRS#98A_P94 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#94_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_TARGETVAR)),AND,X        
               (CLI,phase_P94,NE,C'1'),AND,                            X        
               (CLC,IFFO_P94,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti3.TFO_ISTB_token,WORK=WORKP94                 
               L     R6,IFFO_P94                                                
               L     R5,FFO_fname-FFO_obj(,R6)                                  
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP94,ZSTR=0(,R5)                                         
               IF (CLI,FFO_ftype-FFO_obj(R6),EQ,C'M') THEN                      
                  ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,        X        
               WORK=WORKP94,ZSTR==X'4D00'                                       
                  L     R5,FFO_member-FFO_obj(,R6)                              
                  ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,        X        
               WORK=WORKP94,ZSTR=0(,R5)                                         
                  ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,        X        
               WORK=WORKP94,ZSTR==X'5D00'                                       
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,X        
               (CLI,phase_P94,NE,C'1'),AND,                            X        
               (CLC,IFFO_P94,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti3.TFO_ISTB_token,WORK=WORKP94                 
               L     R6,IFFO_P94                                                
               L     R5,FFO_member-FFO_obj(,R6)                                 
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP94,ZSTR=0(,R5)                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),AND,  X        
               (CLC,ti3.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIX))           
               LA    R1,WORKP94                                                 
               MVC   0(4,R1),ITFO_ti2_P94                                       
               MVC   4(4,R1),ITFO_ti3_P94                                       
               MVC   8(4,R1),=A(2) * include all spaces                         
               L     R15,PRS#80A_P94 * appendToken                              
               BASR  R14,R15                                                    
            ENDIF                                                               
         ENDDO                                                                  
*                                                                               
         IPSS_Pop OBJECT=IPSS_ps_P94,WORK=WORKP94                               
*                                                                               
*        If phase is 1 create statement                                         
         IF (CLI,phase_P94,EQ,C'1') THEN                                        
*                                                                               
*           Instantiate a CallStatement                                         
            MINSTANT GUID=G_IST2_GUID,WORK=WORKP94,OBJPTR=IST2_call_P94         
*                                                                               
*           Initialize CallStatement                                            
            IST2_Init OBJECT=IST2_call_P94,WORK=WORKP94,               X        
               ITFOCALL=ITFO_ti2_P94,RECIPESTMT=postponeRecipeStmt_P94          
*                                                                               
            IAV2_Count OBJECT=IAV2_statements,WORK=WORKP94,            X        
               COUNT_OUT=count_P94                                              
*                                                                               
            MVI   varCallToken_P94,X'00'                                        
            MVC   varCallToken_P94+1(VARIANT_SIZ-1),varCallToken_P94            
            MVC   varCallToken_P94+(vt-VARIANT)(4),=A(VT_UNKNOWN)               
            MVC   varCallToken_P94+(value-VARIANT)(4),IST2_call_P94             
*                                                                               
            IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP94,           X        
               INDEX_IN=count_P94,VARIANT_IN=varCallToken_P94                   
*                                                                               
            IST2_Release OBJECT=IST2_call_P94,WORK=WORKP94                      
            MVC   IST2_call_P94,=A(0)                                           
*                                                                               
            IF (CLC,ITFO_ti2_P94,NE,=A(0)) THEN                                 
               ITFO_Release OBJECT=ITFO_ti2_P94,WORK=WORKP94                    
               MVC   ITFO_ti2_P94,=A(0)                                         
            ENDIF                                                               
*                                                                               
*        Else if phase is 2 return ti2                                          
         ELSE                                                                   
            L     R15,ITFO_tiPtr_P94                                            
            MVC   0(4,R15),ITFO_ti2_P94                                         
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti3_P94,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti3_P94,WORK=WORKP94                       
            MVC   ITFO_ti3_P94,=A(0)                                            
         ENDIF                                                                  
*                                                                               
PRS#94_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK404D_P94                  DC    C'MAK404D Parsing CALL statement'            
                             DC    X'00'                                        
*                                                                               
                             DS    0F                                           
PRS#80A_P94                  DC    A(PRS#80) * appendToken                      
PRS#97A_P94                  DC    A(PRS#97) * parseContinuation                
PRS#98A_P94                  DC    A(PRS#98) * parseVariable                    
*                                                                               
WORKDSAP94                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP94                      DS    5A                                           
*                                                                               
IPSS_ps_P94                  DS    A                                            
ITOK_tokenizer_P94           DS    A                                            
ITFO_tiPtr_P94               DS    A                                            
ITFO_ti2_P94                 DS    A                                            
ITFO_ti3_P94                 DS    A                                            
IST2_call_P94                DS    A                                            
IFFO_P94                     DS    A                                            
*                                                                               
count_P94                    DS    F                                            
varCallToken_P94             DS    CL(VARIANT_SIZ)                              
postponeRecipeStmt_P94       DS    C                                            
phase_P94                    DS    C                                            
*                                                                               
WORKDSAP94_SIZ               EQU   *-WORKDSAP94                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parseAssignment                                                          
*                                                                               
PRS#95   CEEENTRY AUTO=WORKDSAP95_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP95,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P95,0(R1) * Parm 1 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P95,4(R1) * Parm 2 is Tokenizer                   
*                                                                               
         MVC   ITFO_ti1_P95,8(R1) * Parm 3 is ITFO token 1                      
*                                                                               
         MVC   ITFO_ti2_P95,12(R1) * Parm 4 is ITFO token 2                     
*                                                                               
         L     R15,16(,R1)       * Parm 5 is flag postponeRecipeStmt            
         STC   R15,postponeRecipeStmt_P95                                       
*                                                                               
         L     R15,IPSS_ps_P95                                                  
         MVC   phase_P95,PSS_cPhase-PSS_obj(R15)                                
*                                                                               
         IF (CLI,phase_P95,EQ,C'2') THEN                                        
            MVC   IFFO_P95,20(R1) * Parm 6 is IFFO object                       
         ELSE                                                                   
            MVC   IFFO_P95,=A(0) * No IFFO object as parm 6                     
         ENDIF                                                                  
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP95,LINE=MAK402D_P95,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVC   ITFO_ti3_P95,=A(0)                                               
         MVC   ITFO_ti4_P95,=A(0)                                               
*                                                                               
         IPSS_AlterLastState OBJECT=IPSS_ps_P95,WORK=WORKP95,          X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT2)                         
*                                                                               
         MVI   resolve_P95,C'N'                                                 
         IF (CLI,postponeRecipeStmt_P95,EQ,C'N') THEN                           
            L     R2,ITFO_ti2_P95                                               
            USING TFO_obj,R2                                                    
            ISTB_EqualsZStr OBJECT=TFO_ISTB_token,WORK=WORKP95,        X        
               ZSTR=IMMEDEQUALS_P95                                             
            IF (C,R15,EQ,=A(0)) THEN                                            
               ISTB_EqualsZStr OBJECT=TFO_ISTB_token,WORK=WORKP95,     X        
               ZSTR=CONDEQUALS_P95                                              
            ENDIF                                                               
            DROP  R2                                                            
            IF (C,R15,EQ,=A(1)) THEN                                            
               MVI   resolve_P95,C'Y'                                           
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P95,WORK=WORKP95,     X        
               IPSS=IPSS_ps_P95,ITFOPTR=ITFO_ti3_P95                            
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#95_RET                                                       
*                                                                               
         L     R7,ITFO_ti3_P95                                                  
ti3      USING TFO_obj,R7                                                       
*                                                                               
         IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))              
            LA    R1,WORKP95                                                    
            MVC   0(4,R1),IPSS_ps_P95                                           
            MVC   4(4,R1),ITOK_tokenizer_P95                                    
            MVC   8(4,R1),ITFO_ti3_P95                                          
            L     R15,PRS#97A_P95 * parseContinuation                           
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#95_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN             
            LA    R1,WORKP95                                                    
            MVC   0(4,R1),IPSS_ps_P95                                           
            MVC   4(4,R1),ITOK_tokenizer_P95                                    
            MVC   8(4,R1),ITFO_ti3_P95                                          
            XR    R14,R14                                                       
            IC    R14,resolve_P95                                               
            ST    R14,12(,R1)                                                   
            MVC   16(4,R1),IFFO_P95                                             
            L     R15,PRS#98A_P95 * parseVariable                               
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#95_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_TARGETVAR)),AND,   X        
               (CLI,phase_P95,NE,C'1'),AND,                            X        
               (CLC,IFFO_P95,NE,=A(0)) THEN                                     
            ISTB_Init OBJECT=ti3.TFO_ISTB_token,WORK=WORKP95                    
            L     R6,IFFO_P95                                                   
            L     R5,FFO_fname-FFO_obj(,R6)                                     
            ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,WORK=WORKP95, X        
               ZSTR=0(,R5)                                                      
            IF (CLI,FFO_ftype-FFO_obj(R6),EQ,C'M') THEN                         
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP95,ZSTR==X'4D00'                                       
               L     R5,FFO_member-FFO_obj(,R6)                                 
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP95,ZSTR=0(,R5)                                         
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP95,ZSTR==X'5D00'                                       
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,   X        
               (CLI,phase_P95,NE,C'1'),AND,                            X        
               (CLC,IFFO_P95,NE,=A(0)) THEN                                     
            ISTB_Init OBJECT=ti3.TFO_ISTB_token,WORK=WORKP95                    
            L     R6,IFFO_P95                                                   
            L     R5,FFO_member-FFO_obj(,R6)                                    
            ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,WORK=WORKP95, X        
               ZSTR=0(,R5)                                                      
         ENDIF                                                                  
*                                                                               
         DROP  ti3                                                              
*                                                                               
         IPSS_AlterLastState OBJECT=IPSS_ps_P95,WORK=WORKP95,          X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT3)                         
*                                                                               
         DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                      
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P95,WORK=WORKP95,  X        
               IPSS=IPSS_ps_P95,ITFOPTR=ITFO_ti4_P95                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#95_RET                                                    
*                                                                               
            L     R7,ITFO_ti4_P95                                               
ti4         USING TFO_obj,R7                                                    
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,       X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP95                                                 
               MVC   0(4,R1),IPSS_ps_P95                                        
               MVC   4(4,R1),ITOK_tokenizer_P95                                 
               MVC   8(4,R1),ITFO_ti4_P95                                       
               L     R15,PRS#97A_P95 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#95_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,       X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
               ASMLEAVE                                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN          
               LA    R1,WORKP95                                                 
               MVC   0(4,R1),IPSS_ps_P95                                        
               MVC   4(4,R1),ITOK_tokenizer_P95                                 
               MVC   8(4,R1),ITFO_ti4_P95                                       
               XR    R14,R14                                                    
               IC    R14,resolve_P95                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P95                                          
               L     R15,PRS#98A_P95 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#95_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_TARGETVAR)),AND,X        
               (CLI,phase_P95,NE,C'1'),AND,                            X        
               (CLC,IFFO_P95,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti4.TFO_ISTB_token,WORK=WORKP95                 
               L     R6,IFFO_P95                                                
               L     R5,FFO_fname-FFO_obj(,R6)                                  
               ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,           X        
               WORK=WORKP95,ZSTR=0(,R5)                                         
               IF (CLI,FFO_ftype-FFO_obj(R6),EQ,C'M') THEN                      
                  ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,        X        
               WORK=WORKP95,ZSTR==X'4D00'                                       
                  L     R5,FFO_member-FFO_obj(,R6)                              
                  ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,        X        
               WORK=WORKP95,ZSTR=0(,R5)                                         
                  ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,        X        
               WORK=WORKP95,ZSTR==X'5D00'                                       
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,X        
               (CLI,phase_P95,NE,C'1'),AND,                            X        
               (CLC,IFFO_P95,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti4.TFO_ISTB_token,WORK=WORKP95                 
               L     R6,IFFO_P95                                                
               L     R5,FFO_member-FFO_obj(,R6)                                 
               ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,           X        
               WORK=WORKP95,ZSTR=0(,R5)                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),AND,  X        
               (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIX))           
               LA    R1,WORKP95                                                 
               MVC   0(4,R1),ITFO_ti3_P95                                       
               MVC   4(4,R1),ITFO_ti4_P95                                       
               MVC   8(4,R1),=A(2)                                              
               L     R15,PRS#80A_P95 * appendToken                              
               BASR  R14,R15                                                    
            ENDIF                                                               
         ENDDO                                                                  
*                                                                               
         IPSS_Pop OBJECT=IPSS_ps_P95,WORK=WORKP95                               
*                                                                               
*        If phase is 1 create statement                                         
         IF (CLI,phase_P95,EQ,C'1') THEN                                        
*                                                                               
*           Instantiate a AssignmentStatement                                   
            MINSTANT GUID=G_IST1_GUID,WORK=WORKP95,                    X        
               OBJPTR=IST1_assign_P95                                           
*                                                                               
*           Initialize AssignmentStatement                                      
            IST1_Init OBJECT=IST1_assign_P95,WORK=WORKP95,             X        
               ITFONAME=ITFO_ti1_P95,ITFOOPER=ITFO_ti2_P95,            X        
               ITFOVALUE=ITFO_ti3_P95,RECIPESTMT=postponeRecipeStmt_P95         
*                                                                               
            IAV2_Count OBJECT=IAV2_statements,WORK=WORKP95,            X        
               COUNT_OUT=count_P95                                              
*                                                                               
            MVI   varAssignStmt_P95,X'00'                                       
            MVC   varAssignStmt_P95+1(VARIANT_SIZ-1),varAssignStmt_P95          
            MVC   varAssignStmt_P95+(vt-VARIANT)(4),=A(VT_UNKNOWN)              
            MVC   varAssignStmt_P95+(value-VARIANT)(4),IST1_assign_P95          
*                                                                               
            IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP95,           X        
               INDEX_IN=count_P95,VARIANT_IN=varAssignStmt_P95                  
*                                                                               
            IST1_Release OBJECT=IST1_assign_P95,WORK=WORKP95                    
            MVC   IST1_assign_P95,=A(0)                                         
         ENDIF                                                                  
*                                                                               
         IF (CLI,postponeRecipeStmt_P95,EQ,C'N') THEN                           
            L     R2,ITFO_ti2_P95                                               
            L     R3,ITFO_ti3_P95                                               
*                                                                               
            IF (CLC,TFO_tokenType-TFO_obj(4,R2),EQ,=A(TOKEN_TYPE_CONDEQX        
               UALS)) THEN                                                      
               L     R6,ITFO_ti1_P95                                            
               L     R6,TFO_ISTB_token-TFO_obj(,R6)                             
               IAVL_Exists OBJECT=IAVL_variables,WORK=WORKP95,         X        
               NAME_IN=STB_lpBuf-STB_obj(R6)                                    
*                                                                               
               LTR   R15,R15                                                    
               BNZ   PRS#95_SKIP_PUT                                            
            ENDIF                                                               
*                                                                               
            MVC   ISTR_P95,=A(0)                                                
            ISTB_ToString OBJECT=TFO_ISTB_token-TFO_obj(,R3),          X        
               WORK=WORKP95,ISTRPTR=ISTR_P95                                    
*                                                                               
            MVI   varAssignStmt_P95,X'00'                                       
            MVC   varAssignStmt_P95+1(VARIANT_SIZ-1),varAssignStmt_P95          
            MVC   varAssignStmt_P95+(vt-VARIANT)(4),=A(VT_UNKNOWN)              
            MVC   varAssignStmt_P95+(value-VARIANT)(4),ISTR_P95                 
*                                                                               
            L     R3,ITFO_ti1_P95                                               
            L     R3,TFO_ISTB_token-TFO_obj(,R3)                                
*                                                                               
            IAVL_Put OBJECT=IAVL_variables,WORK=WORKP95,               X        
               NAME_IN=STB_lpBuf-STB_obj(R3),                          X        
               VARIANT_IN=varAssignStmt_P95                                     
*                                                                               
            ISTR_Release OBJECT=ISTR_P95,WORK=WORKP95                           
            MVC   ISTR_P95,=A(0)                                                
*                                                                               
            L     R15,G_ILOG                                                    
            IF (CLI,13(R15),GE,LOG_LEVEL_INFO) THEN                             
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP95                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP95,      X        
               ZSTR=MAK305I_P95                                                 
*                                                                               
               L     R3,ITFO_ti1_P95                                            
*                                                                               
               ISTB_AppendString OBJECT=G_ISTB_tmp,WORK=WORKP95,       X        
               ISTR=TFO_ISTB_token-TFO_obj(R3)                                  
*                                                                               
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP95,      X        
               ZSTR=EQUALS_P95                                                  
*                                                                               
               L     R3,ITFO_ti3_P95                                            
*                                                                               
               ISTB_AppendString OBJECT=G_ISTB_tmp,WORK=WORKP95,       X        
               ISTR=TFO_ISTB_token-TFO_obj(R3)                                  
*                                                                               
               L     R3,G_ISTB_tmp                                              
               L     R3,STB_lpBuf-STB_obj(,R3)                                  
*                                                                               
               ILOG_Write OBJECT=G_ILOG,WORK=WORKP95,LINE=0(,R3),      X        
               LOGLEVEL=LOG_LEVEL_INFO                                          
            ENDIF                                                               
*                                                                               
PRS#95_SKIP_PUT EQU   *                                                         
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti3_P95,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti3_P95,WORK=WORKP95                       
            MVC   ITFO_ti3_P95,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti4_P95,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti4_P95,WORK=WORKP95                       
            MVC   ITFO_ti4_P95,=A(0)                                            
         ENDIF                                                                  
*                                                                               
PRS#95_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK305I_P95                  DC    C'MAK305I ',X'00'                            
                             DS    0F                                           
MAK402D_P95                  DC    C'MAK402D Parsing assignment statemeX        
               nt',X'00'                                                        
*                                                                               
                             DS    0F                                           
EQUALS_P95                   DC    C'=',X'00'                                   
                             DS    0F                                           
IMMEDEQUALS_P95              DC    C':=',X'00'                                  
                             DS    0F                                           
CONDEQUALS_P95               DC    C'?=',X'00'                                  
*                                                                               
                             DS    0F                                           
PRS#80A_P95                  DC    A(PRS#80) * appendToken                      
PRS#97A_P95                  DC    A(PRS#97) * parseContinuation                
PRS#98A_P95                  DC    A(PRS#98) * parseVariable                    
*                                                                               
WORKDSAP95                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP95                      DS    5A                                           
*                                                                               
IPSS_ps_P95                  DS    A                                            
ITOK_tokenizer_P95           DS    A                                            
ITFO_ti1_P95                 DS    A                                            
ITFO_ti2_P95                 DS    A                                            
ITFO_ti3_P95                 DS    A                                            
ITFO_ti4_P95                 DS    A                                            
IST1_assign_P95              DS    A                                            
ISTR_P95                     DS    A                                            
IFFO_P95                     DS    A                                            
*                                                                               
count_P95                    DS    F                                            
varAssignStmt_P95            DS    CL(VARIANT_SIZ)                              
postponeRecipeStmt_P95       DS    C                                            
phase_P95                    DS    C                                            
resolve_P95                  DS    C                                            
*                                                                               
WORKDSAP95_SIZ               EQU   *-WORKDSAP95                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parseRule                                                                
*                                                                               
PRS#96   CEEENTRY AUTO=WORKDSAP96_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP96,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P96,0(R1) * Parm 1 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P96,4(R1) * Parm 2 is Tokenizer                   
*                                                                               
         MVC   ITFO_ti1_P96,8(R1) * Parm 3 is ITFO token 1                      
*                                                                               
         L     R7,12(,R1)        * Parm 4 is ITFO token 2                       
ti2      USING TFO_obj,R7        * Addressability ITFO token                    
         ST    R7,ITFO_ti2_P96   * Save in local var                            
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP96,LINE=MAK403D_P96,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVC   ITFO_ti3_P96,=A(0)                                               
         MVC   ITFO_ti4_P96,=A(0)                                               
*                                                                               
         IF (CLC,ti2.TFO_tokenType,NE,=A(TOKEN_TYPE_RULE)) THEN                 
            IPSS_AlterLastState OBJECT=IPSS_ps_P96,WORK=WORKP96,       X        
               STATE_IN==A(PARSER_STATE_IN_RULE1)                               
*                                                                               
            DO UNTIL=(CLC,G_RETCODE,NE,=A(0),OR,                       X        
               CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_RULE))                    
               LA    R1,WORKP96                                                 
               MVC   0(4,R1),ITFO_ti1_P96                                       
               MVC   4(4,R1),ITFO_ti2_P96                                       
               MVC   8(4,R1),=A(2) * include all spaces                         
               L     R15,PRS#80A_P96 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P96,            X        
               WORK=WORKP96,IPSS=IPSS_ps_P96,ITFOPTR=ITFO_ti2_P96               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#96_RET                                                 
*                                                                               
               L     R7,ITFO_ti2_P96                                            
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP96                                              
                  MVC   0(4,R1),IPSS_ps_P96                                     
                  MVC   4(4,R1),ITOK_tokenizer_P96                              
                  MVC   8(4,R1),ITFO_ti2_P96                                    
                  L     R15,PRS#97A_P96 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#96_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP96                                              
                  MVC   0(4,R1),IPSS_ps_P96                                     
                  MVC   4(4,R1),ITOK_tokenizer_P96                              
                  MVC   8(4,R1),ITFO_ti2_P96                                    
                  XR    R14,R14                                                 
                  IC    R14,=C'Y'                                               
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),=A(0)                                          
                  L     R15,PRS#98A_P96 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#96_RET                                              
               ENDIF                                                            
            ENDDO                                                               
         ENDIF                                                                  
*                                                                               
         IPSS_AlterLastState OBJECT=IPSS_ps_P96,WORK=WORKP96,          X        
               STATE_IN==A(PARSER_STATE_IN_RULE2)                               
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P96,WORK=WORKP96,     X        
               IPSS=IPSS_ps_P96,ITFOPTR=ITFO_ti3_P96                            
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#96_RET                                                       
*                                                                               
         L     R6,ITFO_ti3_P96                                                  
ti3      USING TFO_obj,R6                                                       
*                                                                               
         IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)) THEN         
            LA    R1,WORKP96                                                    
            MVC   0(4,R1),IPSS_ps_P96                                           
            MVC   4(4,R1),ITOK_tokenizer_P96                                    
            MVC   8(4,R1),ITFO_ti3_P96                                          
            L     R15,PRS#97A_P96 * parseContinuation                           
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#96_RET                                                    
         ENDIF                                                                  
*                                                                               
         IF (CLC,ti3.TFO_tokenType,NE,=A(TOKEN_TYPE_EOL)),AND,         X        
               (CLC,ti3.TFO_tokenType,NE,=A(TOKEN_TYPE_EOF)) THEN               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN          
               LA    R1,WORKP96                                                 
               MVC   0(4,R1),IPSS_ps_P96                                        
               MVC   4(4,R1),ITOK_tokenizer_P96                                 
               MVC   8(4,R1),ITFO_ti3_P96                                       
               XR    R14,R14                                                    
               IC    R14,=C'N'                                                  
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),=A(0)                                             
               L     R15,PRS#98A_P96 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#96_RET                                                 
            ENDIF                                                               
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P96,WORK=WORKP96,       X        
               STATE_IN==A(PARSER_STATE_IN_RULE3)                               
*                                                                               
            DO UNTIL=(CLC,G_RETCODE,NE,=A(0))                                   
*                                                                               
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P96,            X        
               WORK=WORKP96,IPSS=IPSS_ps_P96,ITFOPTR=ITFO_ti4_P96               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#96_RET                                                 
*                                                                               
               L     R5,ITFO_ti4_P96                                            
ti4            USING TFO_obj,R5                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,    X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP96                                              
                  MVC   0(4,R1),IPSS_ps_P96                                     
                  MVC   4(4,R1),ITOK_tokenizer_P96                              
                  MVC   8(4,R1),ITFO_ti4_P96                                    
                  L     R15,PRS#97A_P96 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#96_RET                                              
               ENDIF                                                            
*                                                                               
               L     R5,ITFO_ti4_P96                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,    X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP96                                              
                  MVC   0(4,R1),IPSS_ps_P96                                     
                  MVC   4(4,R1),ITOK_tokenizer_P96                              
                  MVC   8(4,R1),ITFO_ti4_P96                                    
                  XR    R14,R14                                                 
                  IC    R14,=C'N'                                               
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),=A(0)                                          
                  L     R15,PRS#98A_P96 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#96_RET                                              
               ENDIF                                                            
*                                                                               
               LA    R1,WORKP96                                                 
               MVC   0(4,R1),ITFO_ti3_P96                                       
               MVC   4(4,R1),ITFO_ti4_P96                                       
               MVC   8(4,R1),=A(2) * include all spaces                         
               L     R15,PRS#80A_P96 * appendToken                              
               BASR  R14,R15                                                    
            ENDDO                                                               
         ENDIF                                                                  
*                                                                               
         DROP ti2,ti3,ti4                                                       
*                                                                               
         IPSS_AlterLastState OBJECT=IPSS_ps_P96,WORK=WORKP96,          X        
               STATE_IN==A(PARSER_STATE_IN_RECIPE)                              
*                                                                               
*        Instantiate a RuleStatement                                            
         MINSTANT GUID=G_IST5_GUID,WORK=WORKP96,OBJPTR=IST5_rule_P96            
*                                                                               
*        Initialize RuleStatement                                               
         IST5_Init OBJECT=IST5_rule_P96,WORK=WORKP96,                  X        
               ITFOTGTS=ITFO_ti1_P96,ITFOPREQ=ITFO_ti3_P96                      
*                                                                               
         IF (CLC,firstRuleStatement,EQ,=A(0)) THEN                              
            MVC   firstRuleStatement,IST5_rule_P96                              
         ENDIF                                                                  
*                                                                               
         IAV2_Count OBJECT=IAV2_statements,WORK=WORKP96,               X        
               COUNT_OUT=count_P96                                              
*                                                                               
         MVI   varRule_P96,X'00'                                                
         MVC   varRule_P96+1(VARIANT_SIZ-1),varRule_P96                         
         MVC   varRule_P96+(vt-VARIANT)(4),=A(VT_UNKNOWN)                       
         MVC   varRule_P96+(value-VARIANT)(4),IST5_rule_P96                     
*                                                                               
         IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP96,              X        
               INDEX_IN=count_P96,VARIANT_IN=varRule_P96                        
*                                                                               
         IST5_Release OBJECT=IST5_rule_P96,WORK=WORKP96                         
         MVC   IST5_rule_P96,=A(0)                                              
*                                                                               
*        split the targets and put them in this.targets                         
         L     R5,ITFO_ti1_P96                                                  
         L     R5,TFO_ISTB_token-TFO_obj(,R5)                                   
         L     R7,STB_nStrLen-STB_obj(,R5)                                      
         L     R6,STB_lpBuf-STB_obj(,R5)                                        
*                                                                               
         L     R1,G_TRT_ANY_BUT_SPACE                                           
*                                                                               
         LR    R4,R6                                                            
*                                                                               
         TRTE  R6,R3,0                                                          
         BC    1,*-4                                                            
*                                                                               
         IF (4) THEN                                                            
            L     R3,STB_nStrLen-STB_obj(,R5)                                   
            LA    R2,1(,R3)                                                     
            ST    R2,G_GTSTSIZ                                                  
*                                                                               
            LA    R1,WORKP96                                                    
            MVC   0(4,R1),G_GTSTSIZ                                             
            LA    R15,stringToSplit_P96                                         
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            L     R0,stringToSplit_P96                                          
            LR    R1,R2                                                         
            LR    R14,R4                                                        
            LR    R15,R1                                                        
            MVCL  R0,R14                                                        
*                                                                               
            L     R4,stringToSplit_P96                                          
            L     R7,STB_nStrLen-STB_obj(,R5)                                   
*                                                                               
            LR    R6,R4                                                         
*                                                                               
            L     R1,G_TRT_ANY_BUT_SPACE                                        
*                                                                               
            TRTE  R6,R3,0                                                       
            BC    1,*-4                                                         
*                                                                               
            DO WHILE=(4)                                                        
               ST    R4,targetString_P96                                        
               MVI   0(R6),X'00'                                                
*                                                                               
               MVI   varRule_P96,X'00'                                          
               MVC   varRule_P96+1(VARIANT_SIZ-1),varRule_P96                   
               MVC   varRule_P96+(vt-VARIANT)(4),=A(VT_UI4)                     
               MVC   varRule_P96+(value-VARIANT)(4),count_P96                   
*                                                                               
               IAVL_Insert OBJECT=IAVL_targets,WORK=WORKP96,           X        
               NAME_IN=targetString_P96,VARIANT_IN=varRule_P96                  
*                                                                               
               LA    R6,1(,R6)                                                  
               BCTR  R7,R0                                                      
*                                                                               
               L     R1,G_TRT_ONLY_SPACE                                        
               TRTE  R6,R3,0                                                    
               BC    1,*-4                                                      
*                                                                               
               LR    R4,R6                                                      
*                                                                               
               L     R1,G_TRT_ANY_BUT_SPACE                                     
*                                                                               
               TRTE  R6,R3,0                                                    
               BC    1,*-4                                                      
            ENDDO                                                               
*                                                                               
            ST    R4,targetString_P96                                           
            MVI   0(R6),X'00'                                                   
*                                                                               
            MVI   varRule_P96,X'00'                                             
            MVC   varRule_P96+1(VARIANT_SIZ-1),varRule_P96                      
            MVC   varRule_P96+(vt-VARIANT)(4),=A(VT_UI4)                        
            MVC   varRule_P96+(value-VARIANT)(4),count_P96                      
*                                                                               
            IAVL_Insert OBJECT=IAVL_targets,WORK=WORKP96,              X        
               NAME_IN=targetString_P96,VARIANT_IN=varRule_P96                  
         ELSE                                                                   
            MVI   varRule_P96,X'00'                                             
            MVC   varRule_P96+1(VARIANT_SIZ-1),varRule_P96                      
            MVC   varRule_P96+(vt-VARIANT)(4),=A(VT_UI4)                        
            MVC   varRule_P96+(value-VARIANT)(4),count_P96                      
*                                                                               
            IAVL_Insert OBJECT=IAVL_targets,WORK=WORKP96,              X        
               NAME_IN=STB_lpBuf-STB_obj(R5),VARIANT_IN=varRule_P96             
         ENDIF                                                                  
*                                                                               
PRS#96_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK403D_P96                  DC    C'MAK403D Parsing rule statement',X'X        
               00'                                                              
*                                                                               
                             DS    0F                                           
PRS#80A_P96                  DC    A(PRS#80) * appendToken                      
PRS#97A_P96                  DC    A(PRS#97) * parseContinuation                
PRS#98A_P96                  DC    A(PRS#98) * parseVariable                    
*                                                                               
WORKDSAP96                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP96                      DS    5A                                           
*                                                                               
IPSS_ps_P96                  DS    A                                            
ITOK_tokenizer_P96           DS    A                                            
ITFO_ti1_P96                 DS    A                                            
ITFO_ti2_P96                 DS    A                                            
ITFO_ti3_P96                 DS    A                                            
ITFO_ti4_P96                 DS    A                                            
IST5_rule_P96                DS    A                                            
*                                                                               
count_P96                    DS    F                                            
varRule_P96                  DS    CL(VARIANT_SIZ)                              
stringToSplit_P96            DS    A                                            
targetString_P96             DS    A                                            
*                                                                               
WORKDSAP96_SIZ               EQU   *-WORKDSAP96                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parseContinuation                                                        
*                                                                               
PRS#97   CEEENTRY AUTO=WORKDSAP97_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP97,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P97,0(R1) * Parm 1 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P97,4(R1) * Parm 2 is Tokenizer                   
*                                                                               
         L     R7,8(,R1)         * Parm 3 is ITFO token 1                       
ti       USING TFO_obj,R7        * Addressability ITFO token                    
         ST    R7,ITFO_ti1_P97   * Save in local var                            
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP97,LINE=MAK406D_P97,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         DO WHILE=(CLC,G_RETCODE,EQ,=A(0),AND,                         X        
               CLC,ti.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))             
            IPSS_Push OBJECT=IPSS_ps_P97,WORK=WORKP97,                 X        
               STATE_IN==A(PARSER_STATE_IN_CONTINUATION)                        
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P97,WORK=WORKP97,  X        
               IPSS=IPSS_ps_P97,ITFOPTR=ITFO_ti1_P97                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#97_RET                                                    
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0),AND,                      X        
               CLC,ti.TFO_tokenType,NE,=A(TOKEN_TYPE_EOL))                      
*                                                                               
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P97,            X        
               WORK=WORKP97,IPSS=IPSS_ps_P97,ITFOPTR=ITFO_ti1_P97               
            ENDDO                                                               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#97_RET                                                    
*                                                                               
            IPSS_Pop OBJECT=IPSS_ps_P97,WORK=WORKP97                            
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P97,WORK=WORKP97,  X        
               IPSS=IPSS_ps_P97,ITFOPTR=ITFO_ti1_P97                            
*                                                                               
         ENDDO                                                                  
*                                                                               
PRS#97_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK406D_P97                  DC    C'MAK406D Parsing continuation',X'00X        
               '                                                                
*                                                                               
WORKDSAP97                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP97                      DS    3A                                           
*                                                                               
IPSS_ps_P97                  DS    A                                            
ITOK_tokenizer_P97           DS    A                                            
ITFO_ti1_P97                 DS    A                                            
*                                                                               
WORKDSAP97_SIZ               EQU   *-WORKDSAP97                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parseVariable                                                            
*                                                                               
PRS#98   CEEENTRY AUTO=WORKDSAP98_SIZ,MAIN=NO,BASE=(R10,R11)                    
*                                                                               
         USING WORKDSAP98,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P98,0(R1) * Parm 1 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P98,4(R1) * Parm 2 is Tokenizer                   
*                                                                               
         L     R7,8(,R1)         * Parm 3 is ITFO token 1                       
ti       USING TFO_obj,R7        * Addressability ITFO token                    
         ST    R7,ITFO_ti1_P98   * Save in local var                            
*                                                                               
         L     R15,12(,R1)       * Parm 4 is flag resolve                       
         STC   R15,resolve_P98                                                  
*                                                                               
         MVC   IFFO_P98,16(R1)   * Parm 5 is IFFO object (optional)             
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP98,LINE=MAK407D_P98,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVC   ITFO_ti2_P98,=A(0)                                               
         MVC   ITFO_ti3_P98,=A(0)                                               
         MVC   ITFO_ti4_P98,=A(0)                                               
         MVC   ITFO_ti5_P98,=A(0)                                               
         MVC   ITFO_ti6_P98,=A(0)                                               
         MVC   ISTR_lower_P98,=A(0)                                             
*                                                                               
         IPSS_Push OBJECT=IPSS_ps_P98,WORK=WORKP98,                    X        
               STATE_IN==A(PARSER_STATE_IN_VARIABLE1)                           
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,WORK=WORKP98,     X        
               IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti2_P98                            
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#98_RET                                                       
*                                                                               
         L     R6,ITFO_ti2_P98                                                  
ti2      USING TFO_obj,R6                                                       
*                                                                               
         IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN             
            LA    R1,WORKP98                                                    
            MVC   0(4,R1),IPSS_ps_P98                                           
            MVC   4(4,R1),ITOK_tokenizer_P98                                    
            MVC   8(4,R1),ITFO_ti2_P98                                          
            XR    R14,R14                                                       
            IC    R14,=C'N'                                                     
            ST    R14,12(,R1)                                                   
            MVC   16(4,R1),IFFO_P98                                             
            L     R15,PRS#98A_P98 * parseVariable                               
            BASR  R14,R15                                                       
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#98_RET                                                    
         ENDIF                                                                  
*                                                                               
         LA    R1,WORKP98                                                       
         MVC   0(4,R1),ITFO_ti1_P98                                             
         MVC   4(4,R1),ITFO_ti2_P98                                             
         MVC   8(4,R1),=A(0)     * include no spaces                            
         L     R15,PRS#80A_P98   * appendToken                                  
         BASR  R14,R15                                                          
*                                                                               
         ISTB_ToLowerCase OBJECT=ti2.TFO_ISTB_token,WORK=WORKP98,      X        
               ISTRPTR=ISTR_lower_P98                                           
*                                                                               
         ISTR_EqualsZStr OBJECT=ISTR_lower_P98,WORK=WORKP98,           X        
               ZSTR=MEMBERLIST_P98                                              
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,       X        
               STATE_IN==A(PARSER_STATE_IN_MEMBERLIST1)                         
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,WORK=WORKP98,  X        
               IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti3_P98                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#98_RET                                                    
*                                                                               
            DROP  ti2                                                           
            L     R6,ITFO_ti3_P98                                               
ti3         USING TFO_obj,R6                                                    
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               L     R15,PRS#97A_P98 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               XR    R14,R14                                                    
               IC    R14,resolve_P98                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P98                                          
               L     R15,PRS#98A_P98 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            ISTB_CharAt OBJECT=ti.TFO_ISTB_token,WORK=WORKP98,         X        
               POS==A(1),CHAROUT=char_P98                                       
*                                                                               
            IF (CLI,char_P98,EQ,C'(') THEN                                      
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_MEMBERLIST2A)                        
            ELSE                                                                
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_MEMBERLIST2B)                        
            ENDIF                                                               
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,            X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti4_P98               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
*                                                                               
               L     R5,ITFO_ti4_P98                                            
ti4            USING TFO_obj,R5                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  L     R15,PRS#97A_P98 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  XR    R14,R14                                                 
                  IC    R14,resolve_P98                                         
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),IFFO_P98                                       
                  L     R15,PRS#98A_P98 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSEBRACKET)X        
               ),OR,                                                   X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSECURLY))             
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMA)) THEN          
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),ANDX        
               ,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIX))          
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti3_P98                                    
                  MVC   4(4,R1),ITFO_ti4_P98                                    
                  MVC   8(4,R1),=A(2) * include all spaces                      
                  L     R15,PRS#80A_P98 * apppendToken                          
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMA)) THEN             
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_MEMBERLIST3)                         
*                                                                               
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,            X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti5_P98               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
*                                                                               
               L     R4,ITFO_ti5_P98                                            
ti5            USING TFO_obj,R4                                                 
*                                                                               
               IF (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti5_P98                                    
                  L     R15,PRS#97A_P98 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti5_P98                                    
                  XR    R14,R14                                                 
                  IC    R14,resolve_P98                                         
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),IFFO_P98                                       
                  L     R15,PRS#98A_P98 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               ISTB_CharAt OBJECT=ti.TFO_ISTB_token,WORK=WORKP98,      X        
               POS==A(1),CHAROUT=char_P98                                       
*                                                                               
               IF (CLI,char_P98,EQ,C'(') THEN                                   
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_MEMBERLIST4A)                        
               ELSE                                                             
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_MEMBERLIST4B)                        
               ENDIF                                                            
*                                                                               
               DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                
*                 Get the next token                                            
                  ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,         X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti6_P98               
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
*                                                                               
                  L     R3,ITFO_ti6_P98                                         
ti6               USING TFO_obj,R3                                              
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATIX        
               ON)) THEN                                                        
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),IPSS_ps_P98                                  
                     MVC   4(4,R1),ITOK_tokenizer_P98                           
                     MVC   8(4,R1),ITFO_ti6_P98                                 
                     L     R15,PRS#97A_P98 * parseContinuation                  
                     BASR  R14,R15                                              
*                                                                               
                     CLC   G_RETCODE,=A(0)                                      
                     BNE   PRS#98_RET                                           
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))         
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),IPSS_ps_P98                                  
                     MVC   4(4,R1),ITOK_tokenizer_P98                           
                     MVC   8(4,R1),ITFO_ti6_P98                                 
                     XR    R14,R14                                              
                     IC    R14,resolve_P98                                      
                     ST    R14,12(,R1)                                          
                     MVC   16(4,R1),IFFO_P98                                    
                     L     R15,PRS#98A_P98 * parseVariable                      
                     BASR  R14,R15                                              
*                                                                               
                     CLC   G_RETCODE,=A(0)                                      
                     BNE   PRS#98_RET                                           
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSEBRACKX        
               ET)),OR,                                                X        
               (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSECURLY))             
                     ASMLEAVE                                                   
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),X        
               AND,(CLC,ti6.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIXX        
               ))                                                               
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),ITFO_ti5_P98                                 
                     MVC   4(4,R1),ITFO_ti6_P98                                 
                     MVC   8(4,R1),=A(2) * include all spaces                   
                     L     R15,PRS#80A_P98 * apppendToken                       
                     BASR  R14,R15                                              
                  ENDIF                                                         
               ENDDO                                                            
            ENDIF                                                               
*                                                                               
            IF (CLI,resolve_P98,EQ,C'N') THEN                                   
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti3_P98                                       
               MVC   8(4,R1),=A(1) * include one space                          
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti4_P98                                       
               MVC   8(4,R1),=A(1) * include one space                          
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSEBRACKET)X        
               ),AND,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSECURLYX        
               )) THEN                                                          
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti1_P98                                    
                  MVC   4(4,R1),ITFO_ti5_P98                                    
                  MVC   8(4,R1),=A(1) * include one space                       
                  L     R15,PRS#80A_P98 * appendToken                           
                  BASR  R14,R15                                                 
*                                                                               
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti1_P98                                    
                  MVC   4(4,R1),ITFO_ti6_P98                                    
                  MVC   8(4,R1),=A(1) * include one space                       
                  L     R15,PRS#80A_P98 * appendToken                           
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ELSE                                                                
               L     R2,ITFO_ti3_P98                                            
               L     R2,TFO_ISTB_token-TFO_obj(,R2)                             
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSEBRACKET)X        
               ),AND,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSECURLYX        
               )) THEN                                                          
                  L     R3,ITFO_ti5_P98                                         
                  L     R3,TFO_ISTB_token-TFO_obj(,R3)                          
                  L     R3,STB_lpBuf-STB_obj(,R3)                               
               ELSE                                                             
                  LA    R3,=A(0)                                                
               ENDIF                                                            
*                                                                               
               IFMG_Memberlist OBJECT=G_IFMG,WORK=WORKP98,             X        
               PDSNAME=0(,R2),FILTER=0(,R3),                           X        
               ISTBPTR_OUT=ti.TFO_ISTB_token                                    
            ENDIF                                                               
*                                                                               
            DROP  ti3,ti4,ti5,ti6                                               
*                                                                               
            B     PRS#98_WRAPUP                                                 
         ENDIF                                                                  
*                                                                               
         ISTR_EqualsZStr OBJECT=ISTR_lower_P98,WORK=WORKP98,           X        
               ZSTR=ADDPDSNAME_P98                                              
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            MVI   varType_P98,C'A'                                              
         ELSE                                                                   
            ISTR_EqualsZStr OBJECT=ISTR_lower_P98,WORK=WORKP98,        X        
               ZSTR=APPEND_P98                                                  
*                                                                               
            IF (C,R15,EQ,=A(1)) THEN                                            
               MVI   varType_P98,C'D'                                           
            ELSE                                                                
               ISTR_EqualsZStr OBJECT=ISTR_lower_P98,WORK=WORKP98,     X        
               ZSTR=PREPEND_P98                                                 
*                                                                               
               IF (C,R15,EQ,=A(1)) THEN                                         
                  MVI   varType_P98,C'P'                                        
               ENDIF                                                            
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IF (CLI,varType_P98,EQ,C'A') THEN                                   
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_ADDPDSNAME1)                         
            ELSEIF (CLI,varType_P98,EQ,C'D') THEN                               
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_APPEND1)                             
            ELSE                                                                
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_PREPEND1)                            
            ENDIF                                                               
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,WORK=WORKP98,  X        
               IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti3_P98                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#98_RET                                                    
*                                                                               
            L     R6,ITFO_ti3_P98                                               
ti3         USING TFO_obj,R6                                                    
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               L     R15,PRS#97A_P98 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               XR    R14,R14                                                    
               IC    R14,resolve_P98                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P98                                          
               L     R15,PRS#98A_P98 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,            X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti4_P98               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
*                                                                               
               L     R5,ITFO_ti4_P98                                            
ti4            USING TFO_obj,R5                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  L     R15,PRS#97A_P98 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMA))               
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  XR    R14,R14                                                 
                  IC    R14,resolve_P98                                         
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),IFFO_P98                                       
                  L     R15,PRS#98A_P98 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),ANDX        
               ,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIX))          
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti3_P98                                    
                  MVC   4(4,R1),ITFO_ti4_P98                                    
                  MVC   8(4,R1),=A(2) * include all spaces                      
                  L     R15,PRS#80A_P98 * apppendToken                          
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            ISTB_CharAt OBJECT=ti.TFO_ISTB_token,WORK=WORKP98,         X        
               POS==A(1),CHAROUT=char_P98                                       
*                                                                               
            IF (CLI,char_P98,EQ,C'(') THEN                                      
               IF (CLI,varType_P98,EQ,C'A') THEN                                
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_ADDPDSNAME2A)                        
               ELSEIF (CLI,varType_P98,EQ,C'D') THEN                            
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_APPEND2A)                            
               ELSE                                                             
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_PREPEND2A)                           
               ENDIF                                                            
            ELSE                                                                
               IF (CLI,varType_P98,EQ,C'A') THEN                                
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_ADDPDSNAME2B)                        
               ELSEIF (CLI,varType_P98,EQ,C'D') THEN                            
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_APPEND2B)                            
               ELSE                                                             
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_PREPEND2B)                           
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,WORK=WORKP98,  X        
               IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti4_P98                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#98_RET                                                    
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti4_P98                                       
               L     R15,PRS#97A_P98 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti4_P98                                       
               XR    R14,R14                                                    
               IC    R14,resolve_P98                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P98                                          
               L     R15,PRS#98A_P98 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,X        
               (CLI,resolve_P98,EQ,C'Y'),AND,                          X        
               (CLC,IFFO_P98,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti4.TFO_ISTB_token,WORK=WORKP98                 
               L     R2,IFFO_P98                                                
               L     R3,FFO_member-FFO_obj(,R2)                                 
               ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,           X        
               WORK=WORKP98,ZSTR=0(,R3)                                         
            ENDIF                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSEBRACKET)), X        
               AND,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSECURLY))         
               DO UNTIL=(CLC,G_RETCODE,NE,=A(0))                                
*                                                                               
*                 Get the next token                                            
                  ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,         X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti5_P98               
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
*                                                                               
                  L     R4,ITFO_ti5_P98                                         
ti5               USING TFO_obj,R4                                              
*                                                                               
                  IF (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATIX        
               ON)) THEN                                                        
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),IPSS_ps_P98                                  
                     MVC   4(4,R1),ITOK_tokenizer_P98                           
                     MVC   8(4,R1),ITFO_ti5_P98                                 
                     L     R15,PRS#97A_P98 * parseContinuation                  
                     BASR  R14,R15                                              
*                                                                               
                     CLC   G_RETCODE,=A(0)                                      
                     BNE   PRS#98_RET                                           
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSEBRACKX        
               ET)),OR,                                                X        
               (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSECURLY))             
                     ASMLEAVE                                                   
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))         
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),IPSS_ps_P98                                  
                     MVC   4(4,R1),ITOK_tokenizer_P98                           
                     MVC   8(4,R1),ITFO_ti5_P98                                 
                     XR    R14,R14                                              
                     IC    R14,resolve_P98                                      
                     ST    R14,12(,R1)                                          
                     MVC   16(4,R1),IFFO_P98                                    
                     L     R15,PRS#98A_P98 * parseVariable                      
                     BASR  R14,R15                                              
*                                                                               
                     CLC   G_RETCODE,=A(0)                                      
                     BNE   PRS#98_RET                                           
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)X        
               ),AND,(CLI,resolve_P98,EQ,C'Y'),AND,                    X        
               (CLC,IFFO_P98,NE,=A(0)) THEN                                     
                     ISTB_Init OBJECT=ti4.TFO_ISTB_token,WORK=WORKP98           
                     L     R2,IFFO_P98                                          
                     L     R3,FFO_member-FFO_obj(,R2)                           
                     ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,     X        
               WORK=WORKP98,ZSTR=0(,R3)                                         
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti5.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),X        
               AND,(CLC,ti5.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIXX        
               ))                                                               
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),ITFO_ti4_P98                                 
                     MVC   4(4,R1),ITFO_ti5_P98                                 
                     MVC   8(4,R1),=A(1) * include one space                    
                     L     R15,PRS#80A_P98 * apppendToken                       
                     BASR  R14,R15                                              
                  ENDIF                                                         
               ENDDO                                                            
            ENDIF                                                               
*                                                                               
            IF (CLI,resolve_P98,NE,C'Y') THEN                                   
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti3_P98                                       
               MVC   8(4,R1),=A(1) * include one space                          
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti4_P98                                       
               MVC   8(4,R1),=A(-1) * include comma                             
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
               IF (CLC,ITFO_ti5_P98,NE,=A(0)) THEN                              
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti1_P98                                    
                  MVC   4(4,R1),ITFO_ti5_P98                                    
                  MVC   8(4,R1),=A(0) * include no spaces                       
                  L     R15,PRS#80A_P98 * appendToken                           
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ELSE                                                                
               IF (CLC,ITFO_ti5_P98,NE,=A(0)) THEN                              
                  L     R3,ITFO_ti4_P98                                         
                  L     R3,TFO_ISTB_token-TFO_obj(,R3)                          
                  L     R3,STB_nStrLen-STB_obj(,R3)                             
               ENDIF                                                            
*                                                                               
               IF (CLC,ITFO_ti5_P98,NE,=A(0)),AND,(C,R3,NE,=A(0))               
                  L     R7,ITFO_ti1_P98                                         
                  ISTB_Init OBJECT=TFO_ISTB_token-TFO_obj(,R7),        X        
               WORK=WORKP98                                                     
*                                                                               
                  MVI   first_P98,C'Y'                                          
*                                                                               
                  L     R3,ITFO_ti3_P98                                         
                  L     R3,TFO_ISTB_token-TFO_obj(R3)                           
                  L     R2,STB_lpBuf-STB_obj(,R3)                               
                  L     R3,STB_nStrLen-STB_obj(,R3)                             
*                                                                               
                  L     R5,ITFO_ti4_P98                                         
                  L     R5,TFO_ISTB_token-TFO_obj(,R5)                          
                  L     R6,STB_lpBuf-STB_obj(,R5)                               
                  L     R7,STB_nStrLen-STB_obj(,R5)                             
*                                                                               
                  LR    R4,R6                                                   
*                                                                               
                  L     R1,G_TRT_ANY_BUT_SPACE                                  
*                                                                               
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
*                                                                               
                  DO WHILE=(12)                                                 
                     L     R3,ITFO_ti1_P98                                      
*                                                                               
                     IF (CLI,first_P98,EQ,C'Y') THEN                            
                        MVI   first_P98,C'N'                                    
                     ELSE                                                       
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR==X'4000'                                
                     ENDIF                                                      
*                                                                               
                     MVC   char_P98(1),0(R6)                                    
                     MVI   0(R6),X'00'                                          
*                                                                               
                     IF (CLI,varType_P98,EQ,C'A') THEN                          
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR=0(,R2)                                  
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR==X'4D00'                                
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR=0(,R4)                                  
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR==X'5D00'                                
                     ELSEIF (CLI,varType_P98,EQ,C'D') THEN                      
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR=0(,R4)                                  
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR=0(,R2)                                  
                     ELSE                                                       
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR=0(,R2)                                  
                        ISTB_AppendZString OBJECT=TFO_ISTB_token-TFO_obX        
               j(,R3),WORK=WORKP98,ZSTR=0(,R4)                                  
                     ENDIF                                                      
*                                                                               
                     IF (C,R7,LE,=A(0)) THEN                                    
                        ASMLEAVE                                                
                     ENDIF                                                      
*                                                                               
                     MVC   0(1,R6),char_P98                                     
*                                                                               
                     L     R1,G_TRT_ONLY_SPACE                                  
*                                                                               
                     TRTE  R6,R3,0                                              
                     BC    1,*-4                                                
*                                                                               
                     IF (4) THEN                                                
                        LR    R4,R6                                             
*                                                                               
                        L     R1,G_TRT_ANY_BUT_SPACE                            
*                                                                               
                        TRTE  R6,R3,0                                           
                        BC    1,*-4                                             
                     ENDIF                                                      
                  ENDDO                                                         
               ELSE                                                             
                  ISTB_Init OBJECT=ti.TFO_ISTB_token,WORK=WORKP98               
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
            DROP  ti3,ti4,ti5                                                   
*                                                                               
            B     PRS#98_WRAPUP                                                 
*                                                                               
         ENDIF                                                                  
*                                                                               
         ISTR_EqualsZStr OBJECT=ISTR_lower_P98,WORK=WORKP98,           X        
               ZSTR=FUNCTION_P98                                                
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,       X        
               STATE_IN==A(PARSER_STATE_IN_FUNCTION1)                           
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,WORK=WORKP98,  X        
               IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti3_P98                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#98_RET                                                    
*                                                                               
            L     R6,ITFO_ti3_P98                                               
ti3         USING TFO_obj,R6                                                    
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               L     R15,PRS#97A_P98 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               XR    R14,R14                                                    
               IC    R14,resolve_P98                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P98                                          
               L     R15,PRS#98A_P98 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            ISTB_CharAt OBJECT=ti.TFO_ISTB_token,WORK=WORKP98,         X        
               POS==A(1),CHAROUT=char_P98                                       
*                                                                               
            IF (CLI,char_P98,EQ,C'(') THEN                                      
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_FUNCTION2A)                          
            ELSE                                                                
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_FUNCTION2B)                          
            ENDIF                                                               
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,            X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti4_P98               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
*                                                                               
               L     R5,ITFO_ti4_P98                                            
ti4            USING TFO_obj,R5                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  L     R15,PRS#97A_P98 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  XR    R14,R14                                                 
                  IC    R14,resolve_P98                                         
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),IFFO_P98                                       
                  L     R15,PRS#98A_P98 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)), X        
               AND,(CLI,resolve_P98,EQ,C'Y'),AND,                      X        
               (CLC,IFFO_P98,NE,=A(0)) THEN                                     
                  ISTB_Init OBJECT=ti4.TFO_ISTB_token,WORK=WORKP98              
                  L     R2,IFFO_P98                                             
                  L     R3,FFO_member-FFO_obj(,R2)                              
                  ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,        X        
               WORK=WORKP98,ZSTR=0(,R3)                                         
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSEBRACKET)X        
               ),OR,                                                   X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSECURLY))             
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMA)) THEN          
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),ANDX        
               ,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIX))          
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti3_P98                                    
                  MVC   4(4,R1),ITFO_ti4_P98                                    
                  MVC   8(4,R1),=A(2) * include all spaces                      
                  L     R15,PRS#80A_P98 * apppendToken                          
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMA)) THEN             
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_FUNCTION3)                           
*                                                                               
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,            X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti5_P98               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
*                                                                               
               L     R4,ITFO_ti5_P98                                            
ti5            USING TFO_obj,R4                                                 
*                                                                               
               IF (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti5_P98                                    
                  L     R15,PRS#97A_P98 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti5.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti5_P98                                    
                  XR    R14,R14                                                 
                  IC    R14,resolve_P98                                         
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),IFFO_P98                                       
                  L     R15,PRS#98A_P98 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               ISTB_CharAt OBJECT=ti.TFO_ISTB_token,WORK=WORKP98,      X        
               POS==A(1),CHAROUT=char_P98                                       
*                                                                               
               IF (CLI,char_P98,EQ,C'(') THEN                                   
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_FUNCTION4A)                          
               ELSE                                                             
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_FUNCTION4B)                          
               ENDIF                                                            
*                                                                               
               DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                
*                 Get the next token                                            
                  ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,         X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti6_P98               
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
*                                                                               
                  L     R3,ITFO_ti6_P98                                         
ti6               USING TFO_obj,R3                                              
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATIX        
               ON)) THEN                                                        
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),IPSS_ps_P98                                  
                     MVC   4(4,R1),ITOK_tokenizer_P98                           
                     MVC   8(4,R1),ITFO_ti6_P98                                 
                     L     R15,PRS#97A_P98 * parseContinuation                  
                     BASR  R14,R15                                              
*                                                                               
                     CLC   G_RETCODE,=A(0)                                      
                     BNE   PRS#98_RET                                           
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))         
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),IPSS_ps_P98                                  
                     MVC   4(4,R1),ITOK_tokenizer_P98                           
                     MVC   8(4,R1),ITFO_ti6_P98                                 
                     XR    R14,R14                                              
                     IC    R14,resolve_P98                                      
                     ST    R14,12(,R1)                                          
                     MVC   16(4,R1),IFFO_P98                                    
                     L     R15,PRS#98A_P98 * parseVariable                      
                     BASR  R14,R15                                              
*                                                                               
                     CLC   G_RETCODE,=A(0)                                      
                     BNE   PRS#98_RET                                           
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)X        
               ),AND,(CLI,resolve_P98,EQ,C'Y'),AND,                    X        
               (CLC,IFFO_P98,NE,=A(0)) THEN                                     
                     ISTB_Init OBJECT=ti6.TFO_ISTB_token,WORK=WORKP98           
                     L     R2,IFFO_P98                                          
                     L     R3,FFO_member-FFO_obj(,R2)                           
                     ISTB_AppendZString OBJECT=ti6.TFO_ISTB_token,     X        
               WORK=WORKP98,ZSTR=0(,R3)                                         
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSEBRACKX        
               ET)),OR,                                                X        
               (CLC,ti6.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSECURLY))             
                     ASMLEAVE                                                   
                  ENDIF                                                         
*                                                                               
                  IF (CLC,ti6.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),X        
               AND,(CLC,ti6.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIXX        
               ))                                                               
                     LA    R1,WORKP98                                           
                     MVC   0(4,R1),ITFO_ti5_P98                                 
                     MVC   4(4,R1),ITFO_ti6_P98                                 
                     MVC   8(4,R1),=A(2) * include all spaces                   
                     L     R15,PRS#80A_P98 * apppendToken                       
                     BASR  R14,R15                                              
                  ENDIF                                                         
               ENDDO                                                            
            ENDIF                                                               
*                                                                               
            IF (CLI,resolve_P98,EQ,C'N') THEN                                   
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti3_P98                                       
               MVC   8(4,R1),=A(1) * include one space                          
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti4_P98                                       
               MVC   8(4,R1),=A(1) * include one space                          
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSEBRACKET)X        
               ),AND,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSECURLYX        
               )) THEN                                                          
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti1_P98                                    
                  MVC   4(4,R1),ITFO_ti5_P98                                    
                  MVC   8(4,R1),=A(1) * include one space                       
                  L     R15,PRS#80A_P98 * appendToken                           
                  BASR  R14,R15                                                 
*                                                                               
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti1_P98                                    
                  MVC   4(4,R1),ITFO_ti6_P98                                    
                  MVC   8(4,R1),=A(1) * include one space                       
                  L     R15,PRS#80A_P98 * appendToken                           
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ELSE                                                                
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSEBRACKET)X        
               ),AND,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_CLOSECURLYX        
               )) THEN                                                          
                  ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,        X        
               WORK=WORKP98,ZSTR==X'4000'                                       
*                                                                               
                  ISTB_AppendString OBJECT=ti3.TFO_ISTB_token,         X        
               WORK=WORKP98,ISTR=ti5.TFO_ISTB_token                             
               ENDIF                                                            
*                                                                               
               LA    R15,ti.TFO_ISTB_token                                      
               ST    R15,ADDR_P98                                               
*                                                                               
               IREX_Exec OBJECT=G_IREX,WORK=WORKP98,                   X        
               ISTBEXEC=ti3.TFO_ISTB_token,ISTBPTR_RETVAL=ADDR_P98              
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            DROP  ti3,ti4,ti5,ti6                                               
*                                                                               
            B     PRS#98_WRAPUP                                                 
         ENDIF                                                                  
*                                                                               
         ISTR_EqualsZStr OBJECT=ISTR_lower_P98,WORK=WORKP98,           X        
               ZSTR=SHFUNCTION_P98                                              
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            MVI   varType_P98,C'S'                                              
         ELSE                                                                   
            ISTR_EqualsZStr OBJECT=ISTR_lower_P98,WORK=WORKP98,        X        
               ZSTR=STRIPEXT_P98                                                
*                                                                               
            IF (C,R15,EQ,=A(1)) THEN                                            
               MVI   varType_P98,C'-'                                           
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IF (CLI,varType_P98,EQ,C'S') THEN                                   
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_SHFUNCTION1)                         
            ELSE                                                                
               IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,    X        
               STATE_IN==A(PARSER_STATE_IN_STRIPEXT1)                           
            ENDIF                                                               
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,WORK=WORKP98,  X        
               IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti3_P98                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#98_RET                                                    
*                                                                               
            L     R6,ITFO_ti3_P98                                               
ti3         USING TFO_obj,R6                                                    
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               L     R15,PRS#97A_P98 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),IPSS_ps_P98                                        
               MVC   4(4,R1),ITOK_tokenizer_P98                                 
               MVC   8(4,R1),ITFO_ti3_P98                                       
               XR    R14,R14                                                    
               IC    R14,resolve_P98                                            
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),IFFO_P98                                          
               L     R15,PRS#98A_P98 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)),AND,X        
               (CLI,resolve_P98,EQ,C'Y'),AND,                          X        
               (CLC,IFFO_P98,NE,=A(0)) THEN                                     
               ISTB_Init OBJECT=ti3.TFO_ISTB_token,WORK=WORKP98                 
               L     R2,IFFO_P98                                                
               L     R3,FFO_member-FFO_obj(,R2)                                 
               ISTB_AppendZString OBJECT=ti3.TFO_ISTB_token,           X        
               WORK=WORKP98,ZSTR=0(,R3)                                         
            ENDIF                                                               
*                                                                               
            ISTB_CharAt OBJECT=ti.TFO_ISTB_token,WORK=WORKP98,         X        
               POS==A(1),CHAROUT=char_P98                                       
*                                                                               
            IF (CLI,char_P98,EQ,C'(') THEN                                      
               IF (CLI,varType_P98,EQ,C'S') THEN                                
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_SHFUNCTION2A)                        
               ELSE                                                             
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_STRIPEXT2A)                          
               ENDIF                                                            
            ELSE                                                                
               IF (CLI,varType_P98,EQ,C'S') THEN                                
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_SHFUNCTION2B)                        
               ELSE                                                             
                  IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98, X        
               STATE_IN==A(PARSER_STATE_IN_STRIPEXT2B)                          
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,            X        
               WORK=WORKP98,IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti4_P98               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#98_RET                                                 
*                                                                               
               L     R5,ITFO_ti4_P98                                            
ti4            USING TFO_obj,R5                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               ) THEN                                                           
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  L     R15,PRS#97A_P98 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),IPSS_ps_P98                                     
                  MVC   4(4,R1),ITOK_tokenizer_P98                              
                  MVC   8(4,R1),ITFO_ti4_P98                                    
                  XR    R14,R14                                                 
                  IC    R14,resolve_P98                                         
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),IFFO_P98                                       
                  L     R15,PRS#98A_P98 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_MEMBERVAR)), X        
               AND,(CLI,resolve_P98,EQ,C'Y'),AND,                      X        
               (CLC,IFFO_P98,NE,=A(0)) THEN                                     
                  ISTB_Init OBJECT=ti4.TFO_ISTB_token,WORK=WORKP98              
                  L     R2,IFFO_P98                                             
                  L     R3,FFO_member-FFO_obj(,R2)                              
                  ISTB_AppendZString OBJECT=ti4.TFO_ISTB_token,        X        
               WORK=WORKP98,ZSTR=0(,R3)                                         
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSEBRACKET)X        
               ),OR,                                                   X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_CLOSECURLY))             
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT)),ANDX        
               ,(CLC,ti4.TFO_tokenType,NE,=A(TOKEN_TYPE_RECIPEPREFIX))          
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ITFO_ti3_P98                                    
                  MVC   4(4,R1),ITFO_ti4_P98                                    
                  MVC   8(4,R1),=A(2) * include all spaces                      
                  L     R15,PRS#80A_P98 * apppendToken                          
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            IF (CLI,resolve_P98,EQ,C'N') THEN                                   
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti3_P98                                       
               MVC   8(4,R1),=A(1) * include one space                          
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
*                                                                               
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti1_P98                                       
               MVC   4(4,R1),ITFO_ti4_P98                                       
               MVC   8(4,R1),=A(1) * include one space                          
               L     R15,PRS#80A_P98 * appendToken                              
               BASR  R14,R15                                                    
            ELSE                                                                
               IF (CLI,varType_P98,EQ,C'S') THEN                                
                  IUSS_BPX1SPN OBJECT=G_IUSS,WORK=WORKP98,             X        
               ISTBSH=ti3.TFO_ISTB_token,                              x        
               ISTBPTR_RETVAL=ti.TFO_ISTB_token                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ELSE                                                             
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),ti3.TFO_ISTB_token                              
                  LA    R15,ti.TFO_ISTB_token                                   
                  ST    R15,4(,R1)                                              
                  L     R15,PRS#81A_P98                                         
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ENDIF                                                               
*                                                                               
            DROP  ti3,ti4                                                       
*                                                                               
            B     PRS#98_WRAPUP                                                 
         ENDIF                                                                  
*                                                                               
ti2      USING TFO_obj,R6                                                       
*                                                                               
         ISTB_CharAt OBJECT=ti.TFO_ISTB_token,WORK=WORKP98,POS==A(1),  X        
               CHAROUT=char_P98                                                 
*                                                                               
         IF (CLI,char_P98,EQ,C'(') THEN                                         
            IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,       X        
               STATE_IN==A(PARSER_STATE_IN_VARIABLE2A)                          
         ELSE                                                                   
            IPSS_AlterLastState OBJECT=IPSS_ps_P98,WORK=WORKP98,       X        
               STATE_IN==A(PARSER_STATE_IN_VARIABLE2B)                          
         ENDIF                                                                  
*                                                                               
*        Get the next token                                                     
         ITOK_GetNextToken OBJECT=ITOK_tokenizer_P98,WORK=WORKP98,     X        
               IPSS=IPSS_ps_P98,ITFOPTR=ITFO_ti3_P98                            
*                                                                               
         CLC   G_RETCODE,=A(0)                                                  
         BNE   PRS#98_RET                                                       
*                                                                               
         LA    R1,WORKP98                                                       
         MVC   0(4,R1),ITFO_ti1_P98                                             
         MVC   4(4,R1),ITFO_ti3_P98                                             
         MVC   8(4,R1),=A(0)     * include no spaces                            
         L     R15,PRS#80A_P98   * appendToken                                  
         BASR  R14,R15                                                          
*                                                                               
         IF (CLI,resolve_P98,EQ,C'Y') THEN                                      
            MVI   varVariable_P98,X'00'                                         
            MVC   varVariable_P98+1(VARIANT_SIZ-1),varVariable_P98              
*                                                                               
            L     R2,ti2.TFO_ISTB_token                                         
*                                                                               
            IAVL_QUery OBJECT=IAVL_variables,WORK=WORKP98,             X        
               NAME_IN=STB_lpBuf-STB_obj(R2),                          X        
               VARIANT_OUT=varVariable_P98                                      
*                                                                               
            LTR   R15,R15                                                       
            IF (NZ) THEN                                                        
               L     R2,varVariable_P98+(value-VARIANT)                         
*                                                                               
               IF (CLC,STR_nStrLen-STR_obj(4,R2),GT,=A(0)) THEN                 
                  L     R3,ITFO_ti3_P98                                         
*                                                                               
                  LA    R1,WORKP98                                              
                  MVC   0(4,R1),STR_lpString-STR_obj(R2)                        
                  LA    R15,ti.TFO_ISTB_token                                   
                  ST    R15,4(,R1)                                              
                  MVC   8(4,R1),TFO_nLine-TFO_obj(R3)                           
                  OI    8(R1),X'80'                                             
                  L     R15,PRS#60A_P98 * Resolve string                        
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#98_RET                                              
               ELSE                                                             
                  ISTB_Init OBJECT=ti.TFO_ISTB_token,WORK=WORKP98               
               ENDIF                                                            
*                                                                               
               ISTR_Release OBJECT=varVariable_P98+(value-VARIANT),    X        
               WORK=WORKP98                                                     
            ELSE                                                                
               LA    R1,WORKP98                                                 
               MVC   0(4,R1),ITFO_ti2_P98                                       
               MVC   4(4,R1),=A(1)                                              
               L     R15,PRS#90A_P98 * ParserException                          
               BASR  R14,R15                                                    
*                                                                               
               B     PRS#98_RET                                                 
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
PRS#98_WRAPUP EQU   *                                                           
         IPSS_Pop OBJECT=IPSS_ps_P98,WORK=WORKP98                               
*                                                                               
         ISTR_Release OBJECT=ISTR_lower_P98,WORK=WORKP98                        
         MVC   ISTR_lower_P98,=A(0)                                             
*                                                                               
         ITFO_Release OBJECT=ITFO_ti2_P98,WORK=WORKP98                          
         MVC   ITFO_ti2_P98,=A(0)                                               
*                                                                               
         IF (CLC,ITFO_ti3_P98,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti3_P98,WORK=WORKP98                       
            MVC   ITFO_ti3_P98,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti4_P98,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti4_P98,WORK=WORKP98                       
            MVC   ITFO_ti4_P98,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti5_P98,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti5_P98,WORK=WORKP98                       
            MVC   ITFO_ti5_P98,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti6_P98,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti6_P98,WORK=WORKP98                       
            MVC   ITFO_ti6_P98,=A(0)                                            
         ENDIF                                                                  
*                                                                               
PRS#98_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK407D_P98                  DC    C'MAK407D Parsing variable',X'00'            
*                                                                               
                             DS    0F                                           
MEMBERLIST_P98               DC    C'memberlist',X'00'                          
                             DS    0F                                           
ADDPDSNAME_P98               DC    C'addpdsname',X'00'                          
                             DS    0F                                           
APPEND_P98                   DC    C'append',X'00'                              
                             DS    0F                                           
PREPEND_P98                  DC    C'prepend',X'00'                             
                             DS    0F                                           
STRIPEXT_P98                 DC    C'stripext',X'00'                            
                             DS    0F                                           
FUNCTION_P98                 DC    C'function',X'00'                            
                             DS    0F                                           
SHFUNCTION_P98               DC    C'sh',X'00'                                  
*                                                                               
                             DS    0F                                           
PRS#60A_P98                  DC    A(PRS#60) * resolveString                    
PRS#80A_P98                  DC    A(PRS#80) * appendToken                      
PRS#81A_P98                  DC    A(PRS#81) * stripExtension                   
PRS#90A_P98                  DC    A(PRS#90) * ParserException                  
PRS#97A_P98                  DC    A(PRS#97) * parseContinuation                
PRS#98A_P98                  DC    A(PRS#98) * parseVariable                    
*                                                                               
WORKDSAP98                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP98                      DS    5A                                           
*                                                                               
IPSS_ps_P98                  DS    A                                            
ITOK_tokenizer_P98           DS    A                                            
IFFO_P98                     DS    A                                            
ITFO_ti1_P98                 DS    A                                            
ITFO_ti2_P98                 DS    A                                            
ITFO_ti3_P98                 DS    A                                            
ITFO_ti4_P98                 DS    A                                            
ITFO_ti5_P98                 DS    A                                            
ITFO_ti6_P98                 DS    A                                            
ISTR_lower_P98               DS    A                                            
ADDR_P98                     DS    A                                            
*                                                                               
varVariable_P98              DS    CL(VARIANT_SIZ)                              
*                                                                               
resolve_P98                  DS    C                                            
char_P98                     DS    C                                            
first_P98                    DS    C                                            
vartype_P98                  DS    C                                            
*                                                                               
WORKDSAP98_SIZ               EQU   *-WORKDSAP98                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPRS parseSpecial                                                             
*                                                                               
PRS#99   CEEENTRY AUTO=WORKDSAP99_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAP99,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         USING PRS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   IPSS_ps_P99,0(R1) * Parm 1 is ParserState                        
*                                                                               
         MVC   ITOK_tokenizer_P99,4(R1) * Parm 2 is Tokenizer                   
*                                                                               
         L     R7,8(,R1)         * Parm 3 is ITFO token 1                       
ti1      USING TFO_obj,R7        * Addressability ITFO token                    
         ST    R7,ITFO_ti1_P99   * Save in local var                            
*                                                                               
         ILOG_Write OBJECT=G_ILOG,WORK=WORKP99,LINE=MAK405D_P99,       X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
*                                                                               
         MVC   ITFO_ti2_P99,=A(0)                                               
         MVC   ITFO_ti3_P99,=A(0)                                               
         MVC   ITFO_ti4_P99,=A(0)                                               
         MVC   IST3_phony_P99,=A(0)                                             
         MVC   IST4_recipeprf_P99,=A(0)                                         
         MVC   IST7_home_P99,=A(0)                                              
         MVC   IST8_buildwhen_P99,=A(0)                                         
*                                                                               
         ISTB_EqualsZStr OBJECT=ti1.TFO_ISTB_token,WORK=WORKP99,       X        
               ZSTR=RECIPEPREFIX_P99                                            
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IPSS_Push OBJECT=IPSS_ps_P99,WORK=WORKP99,                 X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT1)                         
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,            X        
               WORK=WORKP99,IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti2_P99               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
*                                                                               
               L     R6,ITFO_ti2_P99                                            
ti2            USING TFO_obj,R6                                                 
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_EQUALS)) THEN         
                  ASMLEAVE                                                      
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P99,WORK=WORKP99,       X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT2)                         
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,WORK=WORKP99,  X        
               IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti3_P99                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
             L     R5,ITFO_ti3_P99                                              
ti3          USING TFO_obj,R5                                                   
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P99,WORK=WORKP99,       X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT3)                         
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,            X        
               WORK=WORKP99,IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti4_P99               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
*                                                                               
               L     R4,ITFO_ti4_P99                                            
ti4            USING TFO_obj,R4                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMENT)),OR,X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            L     R3,ti3.TFO_ISTB_token                                         
            IF (CLC,STB_nStrLen-STB_obj(4,R3),GT,=A(1)) THEN                    
               ILOG_Write OBJECT=G_ILOG,WORK=WORKP99,LINE=MAK105E_P99, X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
               MVC   G_RETCODE,=A(12)                                           
               B     PRS#99_RET                                                 
            ENDIF                                                               
*                                                                               
            DROP  ti2,ti3,ti4                                                   
*                                                                               
            IPSS_Pop OBJECT=IPSS_ps_P99,WORK=WORKP99                            
*                                                                               
            L     R2,ITFO_ti3_P99                                               
            L     R2,TFO_ISTB_token-TFO_obj(,R2)                                
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
*                                                                               
            IPSS_SetRecipePrefix OBJECT=IPSS_ps_P99,WORK=WORKP99,      X        
               PREFIX=0(R2)                                                     
*                                                                               
*           Instantiate a RecipeprefixStatement                                 
            MINSTANT GUID=G_IST4_GUID,WORK=WORKP99,                    X        
               OBJPTR=IST4_recipeprf_P99                                        
*                                                                               
*           Initialize RecipeprefixStatement                                    
            IST4_Init OBJECT=IST4_recipeprf_P99,WORK=WORKP99,          X        
               ITFORECIP=ITFO_ti3_P99                                           
*                                                                               
            IAV2_Count OBJECT=IAV2_statements,WORK=WORKP99,            X        
               COUNT_OUT=count_P99                                              
*                                                                               
            MVI   varToken_P99,X'00'                                            
            MVC   varToken_P99+1(VARIANT_SIZ-1),varToken_P99                    
            MVC   varToken_P99+(vt-VARIANT)(4),=A(VT_UNKNOWN)                   
            MVC   varToken_P99+(value-VARIANT)(4),IST4_recipeprf_P99            
*                                                                               
            IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP99,           X        
               INDEX_IN=count_P99,VARIANT_IN=varToken_P99                       
*                                                                               
            IST4_Release OBJECT=IST4_recipeprf_P99,WORK=WORKP99                 
            MVC   IST4_recipeprf_P99,=A(0)                                      
*                                                                               
            B     PRS#99_WRAPUP                                                 
         ENDIF                                                                  
*                                                                               
         ISTB_EqualsZStr OBJECT=ti1.TFO_ISTB_token,WORK=WORKP99,       X        
               ZSTR=PHONY_P99                                                   
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IPSS_Push OBJECT=IPSS_ps_P99,WORK=WORKP99,                 X        
               STATE_IN==A(PARSER_STATE_IN_PHONY1)                              
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,WORK=WORKP99,  X        
               IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti2_P99                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            L     R6,ITFO_ti2_P99                                               
ti2         USING TFO_obj,R6                                                    
*                                                                               
            IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION))           
               LA    R1,WORKP99                                                 
               MVC   0(4,R1),IPSS_ps_P99                                        
               MVC   4(4,R1),ITOK_tokenizer_P99                                 
               MVC   8(4,R1),ITFO_ti2_P99                                       
               L     R15,PRS#97A_P99 * parseContinuation                        
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
            ENDIF                                                               
*                                                                               
            IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE)) THEN          
               LA    R1,WORKP99                                                 
               MVC   0(4,R1),IPSS_ps_P99                                        
               MVC   4(4,R1),ITOK_tokenizer_P99                                 
               MVC   8(4,R1),ITFO_ti2_P99                                       
               XR    R14,R14                                                    
               IC    R14,=C'Y'                                                  
               ST    R14,12(,R1)                                                
               MVC   16(4,R1),=A(0)                                             
               L     R15,PRS#98A_P99 * parseVariable                            
               BASR  R14,R15                                                    
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
            ENDIF                                                               
*                                                                               
            DROP  ti2                                                           
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P99,WORK=WORKP99,       X        
               STATE_IN==A(PARSER_STATE_IN_PHONY2)                              
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,            X        
               WORK=WORKP99,IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti3_P99               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
*                                                                               
               L     R5,ITFO_ti3_P99                                            
ti3            USING TFO_obj,R5                                                 
*                                                                               
               IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,    X        
               (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_CONTINUATION)X        
               )                                                                
                  LA    R1,WORKP99                                              
                  MVC   0(4,R1),IPSS_ps_P99                                     
                  MVC   4(4,R1),ITOK_tokenizer_P99                              
                  MVC   8(4,R1),ITFO_ti3_P99                                    
                  L     R15,PRS#97A_P99 * parseContinuation                     
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#99_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOF)),OR,    X        
               (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
*                                                                               
               IF (CLC,ti3.TFO_tokenType,EQ,=A(TOKEN_TYPE_VARIABLE))            
                  LA    R1,WORKP99                                              
                  MVC   0(4,R1),IPSS_ps_P99                                     
                  MVC   4(4,R1),ITOK_tokenizer_P99                              
                  MVC   8(4,R1),ITFO_ti3_P99                                    
                  XR    R14,R14                                                 
                  IC    R14,=C'Y'                                               
                  ST    R14,12(,R1)                                             
                  MVC   16(4,R1),=A(0)                                          
                  L     R15,PRS#98A_P99 * parseVariable                         
                  BASR  R14,R15                                                 
*                                                                               
                  CLC   G_RETCODE,=A(0)                                         
                  BNE   PRS#99_RET                                              
               ENDIF                                                            
*                                                                               
               IF (CLC,ti3.TFO_tokenType,NE,=A(TOKEN_TYPE_COMMENT))             
                  LA    R1,WORKP99                                              
                  MVC   0(4,R1),ITFO_ti2_P99                                    
                  MVC   4(4,R1),ITFO_ti3_P99                                    
                  MVC   8(4,R1),=A(2)                                           
                  L     R15,PRS#80A_P99 * appendToken                           
                  BASR  R14,R15                                                 
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            DROP ti1,ti3                                                        
*                                                                               
            IPSS_Pop OBJECT=IPSS_ps_P99,WORK=WORKP99                            
*                                                                               
*           Instantiate a PhonyStatement                                        
            MINSTANT GUID=G_IST3_GUID,WORK=WORKP99,                    X        
               OBJPTR=IST3_phony_P99                                            
*                                                                               
*           Initialize PhonyStatement                                           
            IST3_Init OBJECT=IST3_phony_P99,WORK=WORKP99,              X        
               ITFOPHONY=ITFO_ti2_P99                                           
*                                                                               
            IAV2_Count OBJECT=IAV2_statements,WORK=WORKP99,            X        
               COUNT_OUT=count_P99                                              
*                                                                               
            MVI   varToken_P99,X'00'                                            
            MVC   varToken_P99+1(VARIANT_SIZ-1),varToken_P99                    
            MVC   varToken_P99+(vt-VARIANT)(4),=A(VT_UNKNOWN)                   
            MVC   varToken_P99+(value-VARIANT)(4),IST3_phony_P99                
*                                                                               
            IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP99,           X        
               INDEX_IN=count_P99,VARIANT_IN=varToken_P99                       
*                                                                               
            IST3_Release OBJECT=IST3_phony_P99,WORK=WORKP99                     
            MVC   IST3_phony_P99,=A(0)                                          
*                                                                               
*           split the phonies and put them in this.phonies                      
            L     R5,ITFO_ti2_P99                                               
            L     R5,TFO_ISTB_token-TFO_obj(,R5)                                
            L     R7,STB_nStrLen-STB_obj(,R5)                                   
            L     R6,STB_lpBuf-STB_obj(,R5)                                     
*                                                                               
            L     R1,G_TRT_ANY_BUT_SPACE                                        
*                                                                               
            LR    R4,R6                                                         
*                                                                               
            TRTE  R6,R3,0                                                       
            BC    1,*-4                                                         
*                                                                               
            IF (4) THEN                                                         
               L     R3,STB_nStrLen-STB_obj(,R5)                                
               LA    R2,1(,R3)                                                  
               ST    R2,G_GTSTSIZ                                               
*                                                                               
               LA    R1,WORKP99                                                 
               MVC   0(4,R1),G_GTSTSIZ                                          
               LA    R15,stringToSplit_P99                                      
               ST    R15,4(,R1)                                                 
               L     R15,G_GTST                                                 
               BASR  R14,R15                                                    
*                                                                               
               L     R0,stringToSplit_P99                                       
               LR    R1,R2                                                      
               LR    R14,R4                                                     
               LR    R15,R1                                                     
               MVCL  R0,R14                                                     
*                                                                               
               L     R4,stringToSplit_P99                                       
               L     R7,STB_nStrLen-STB_obj(,R5)                                
*                                                                               
               LR    R6,R4                                                      
*                                                                               
               L     R1,G_TRT_ANY_BUT_SPACE                                     
*                                                                               
               TRTE  R6,R3,0                                                    
               BC    1,*-4                                                      
*                                                                               
               DO WHILE=(4)                                                     
                  ST    R4,targetString_P99                                     
                  MVI   0(R6),X'00'                                             
*                                                                               
                  MVI   varToken_P99,X'00'                                      
                  MVC   varToken_P99+1(VARIANT_SIZ-1),varToken_P99              
                  MVC   varToken_P99+(vt-VARIANT)(4),=A(VT_UI4)                 
                  MVC   varToken_P99+(value-VARIANT)(4),count_P99               
*                                                                               
                  IAVL_Insert OBJECT=IAVL_phonies,WORK=WORKP99,        X        
               NAME_IN=targetString_P99,VARIANT_IN=varToken_P99                 
*                                                                               
                  LA    R6,1(,R6)                                               
                  BCTR  R7,R0                                                   
*                                                                               
                  L     R1,G_TRT_ONLY_SPACE                                     
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
*                                                                               
                  LR    R4,R6                                                   
*                                                                               
                  L     R1,G_TRT_ANY_BUT_SPACE                                  
*                                                                               
                  TRTE  R6,R3,0                                                 
                  BC    1,*-4                                                   
               ENDDO                                                            
*                                                                               
               ST    R4,targetString_P99                                        
               MVI   0(R6),X'00'                                                
*                                                                               
               MVI   varToken_P99,X'00'                                         
               MVC   varToken_P99+1(VARIANT_SIZ-1),varToken_P99                 
               MVC   varToken_P99+(vt-VARIANT)(4),=A(VT_UI4)                    
               MVC   varToken_P99+(value-VARIANT)(4),count_P99                  
*                                                                               
               IAVL_Insert OBJECT=IAVL_phonies,WORK=WORKP99,           X        
               NAME_IN=targetString_P99,VARIANT_IN=varToken_P99                 
            ELSE                                                                
               MVI   varToken_P99,X'00'                                         
               MVC   varToken_P99+1(VARIANT_SIZ-1),varToken_P99                 
               MVC   varToken_P99+(vt-VARIANT)(4),=A(VT_UI4)                    
               MVC   varToken_P99+(value-VARIANT)(4),count_P99                  
*                                                                               
               IAVL_Insert OBJECT=IAVL_phonies,WORK=WORKP99,           X        
               NAME_IN=STB_lpBuf-STB_obj(R5),VARIANT_IN=varToken_P99            
            ENDIF                                                               
*                                                                               
            B     PRS#99_WRAPUP                                                 
         ENDIF                                                                  
*                                                                               
         L     R7,ITFO_ti1_P99                                                  
ti1      USING TFO_obj,R7                                                       
*                                                                               
         ISTB_EqualsZStr OBJECT=ti1.TFO_ISTB_token,WORK=WORKP99,       X        
               ZSTR=USSHOME_P99                                                 
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IPSS_Push OBJECT=IPSS_ps_P99,WORK=WORKP99,                 X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT1)                         
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,            X        
               WORK=WORKP99,IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti2_P99               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
*                                                                               
               L     R6,ITFO_ti2_P99                                            
ti2            USING TFO_obj,R6                                                 
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_EQUALS)) THEN         
                  ASMLEAVE                                                      
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P99,WORK=WORKP99,       X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT2)                         
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,WORK=WORKP99,  X        
               IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti3_P99                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
             L     R5,ITFO_ti3_P99                                              
ti3          USING TFO_obj,R5                                                   
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P99,WORK=WORKP99,       X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT3)                         
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,            X        
               WORK=WORKP99,IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti4_P99               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
*                                                                               
               L     R4,ITFO_ti4_P99                                            
ti4            USING TFO_obj,R4                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMENT)),OR,X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            DROP  ti1,ti2,ti3,ti4                                               
*                                                                               
            IPSS_Pop OBJECT=IPSS_ps_P99,WORK=WORKP99                            
*                                                                               
*           Instantiate a UssHomeStatement                                      
            MINSTANT GUID=G_IST7_GUID,WORK=WORKP99,                    X        
               OBJPTR=IST7_home_P99                                             
*                                                                               
*           Initialize UssHomeStatement                                         
            IST7_Init OBJECT=IST7_home_P99,WORK=WORKP99,               X        
               ITFOHOME=ITFO_ti3_P99                                            
*                                                                               
            IAV2_Count OBJECT=IAV2_statements,WORK=WORKP99,            X        
               COUNT_OUT=count_P99                                              
*                                                                               
            MVI   varToken_P99,X'00'                                            
            MVC   varToken_P99+1(VARIANT_SIZ-1),varToken_P99                    
            MVC   varToken_P99+(vt-VARIANT)(4),=A(VT_UNKNOWN)                   
            MVC   varToken_P99+(value-VARIANT)(4),IST7_home_P99                 
*                                                                               
            IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP99,           X        
               INDEX_IN=count_P99,VARIANT_IN=varToken_P99                       
*                                                                               
            IST7_Release OBJECT=IST7_home_P99,WORK=WORKP99                      
            MVC   IST7_home_P99,=A(0)                                           
*                                                                               
            L     R6,ITFO_ti3_P99                                               
            L     R6,TFO_ISTB_token-TFO_obj(,R6)                                
*                                                                               
            LA    R1,WORKP99                                                    
            L     R15,STB_nStrLen-STB_obj(R6)                                   
            LA    R15,6(,R15)                                                   
            ST    R15,0(,R1)                                                    
            LA    R15,G_HOME                                                    
            ST    R15,4(,R1)                                                    
            L     R15,G_GTST                                                    
            BASR  R14,R15                                                       
*                                                                               
            L     R14,G_HOME                                                    
            MVC   0(5,R14),=C'HOME='                                            
            LA    R14,5(,R14)                                                   
            L     R15,STB_nStrLen-STB_obj(R6)                                   
            LA    R15,1(,R15)                                                   
            L     R4,STB_lpBuf-STB_obj(,R6)                                     
            LR    R5,R15                                                        
            MVCL  R14,R4                                                        
*                                                                               
            L     R15,STB_nStrLen-STB_obj(R6)                                   
            LA    R15,5(,R15)                                                   
            ST    R15,G_HOMELEN                                                 
*                                                                               
            B     PRS#99_WRAPUP                                                 
         ENDIF                                                                  
*                                                                               
         L     R7,ITFO_ti1_P99                                                  
ti1      USING TFO_obj,R7                                                       
*                                                                               
         ISTB_EqualsZStr OBJECT=ti1.TFO_ISTB_token,WORK=WORKP99,       X        
               ZSTR=BUILDWHEN_P99                                               
*                                                                               
         IF (C,R15,EQ,=A(1)) THEN                                               
            IPSS_Push OBJECT=IPSS_ps_P99,WORK=WORKP99,                 X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT1)                         
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,            X        
               WORK=WORKP99,IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti2_P99               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
*                                                                               
               L     R6,ITFO_ti2_P99                                            
ti2            USING TFO_obj,R6                                                 
*                                                                               
               IF (CLC,ti2.TFO_tokenType,EQ,=A(TOKEN_TYPE_EQUALS)) THEN         
                  ASMLEAVE                                                      
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P99,WORK=WORKP99,       X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT2)                         
*                                                                               
*           Get the next token                                                  
            ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,WORK=WORKP99,  X        
               IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti3_P99                            
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            L     R5,ITFO_ti3_P99                                               
ti3         USING TFO_obj,R5                                                    
*                                                                               
            ISTB_EqualsZStr OBJECT=ti3.TFO_ISTB_token,WORK=WORKP99,    X        
               ZSTR=TOM_P99                                                     
*                                                                               
            IF (C,R15,NE,=A(1)) THEN                                            
               ISTB_EqualsZStr OBJECT=ti3.TFO_ISTB_token,WORK=WORKP99, X        
               ZSTR=TUM_P99                                                     
            ENDIF                                                               
*                                                                               
            IF (C,R15,NE,=A(1)) THEN                                            
               ISTB_EqualsZStr OBJECT=ti3.TFO_ISTB_token,WORK=WORKP99, X        
               ZSTR=TO_P99                                                      
            ENDIF                                                               
*                                                                               
            IF (C,R15,NE,=A(1)) THEN                                            
               ISTB_EqualsZStr OBJECT=ti3.TFO_ISTB_token,WORK=WORKP99, X        
               ZSTR=TU_P99                                                      
            ENDIF                                                               
*                                                                               
            IF (C,R15,NE,=A(1)) THEN                                            
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKP99                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP99,      X        
               ZSTR=MAK120E_P99                                                 
               L     R2,ti3.TFO_ISTB_token                                      
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKP99,      X        
               ZSTR=STB_lpBuf-STB_obj(,R2)                                      
*                                                                               
               L     R2,G_ISTB_tmp                                              
               L     R2,STB_lpBuf-STB_obj(,R2)                                  
               ILOG_Write OBJECT=G_ILOG,WORK=WORKP99,LINE=0(,R2),      X        
               LOGLEVEL=LOG_LEVEL_ERROR                                         
*                                                                               
               MVC   G_RETCODE,=A(8)                                            
               B     PRS#99_RET                                                 
            ENDIF                                                               
*                                                                               
            IPSS_AlterLastState OBJECT=IPSS_ps_P99,WORK=WORKP99,       X        
               STATE_IN==A(PARSER_STATE_IN_ASSIGNMENT3)                         
*                                                                               
            DO WHILE=(CLC,G_RETCODE,EQ,=A(0))                                   
*              Get the next token                                               
               ITOK_GetNextToken OBJECT=ITOK_tokenizer_P99,            X        
               WORK=WORKP99,IPSS=IPSS_ps_P99,ITFOPTR=ITFO_ti4_P99               
*                                                                               
               CLC   G_RETCODE,=A(0)                                            
               BNE   PRS#99_RET                                                 
*                                                                               
               L     R4,ITFO_ti4_P99                                            
ti4            USING TFO_obj,R4                                                 
*                                                                               
               IF (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_COMMENT)),OR,X        
               (CLC,ti4.TFO_tokenType,EQ,=A(TOKEN_TYPE_EOL)) THEN               
                  ASMLEAVE                                                      
               ENDIF                                                            
            ENDDO                                                               
*                                                                               
            CLC   G_RETCODE,=A(0)                                               
            BNE   PRS#99_RET                                                    
*                                                                               
            DROP  ti1,ti2,ti3,ti4                                               
*                                                                               
            IPSS_Pop OBJECT=IPSS_ps_P99,WORK=WORKP99                            
*                                                                               
*           Instantiate a BuildWhenStatement                                    
            MINSTANT GUID=G_IST8_GUID,WORK=WORKP99,                    X        
               OBJPTR=IST8_buildwhen_P99                                        
*                                                                               
*           Initialize BuildWhenStatement                                       
            IST8_Init OBJECT=IST8_buildwhen_P99,WORK=WORKP99,          X        
               ITFOBUILDWHEN=ITFO_ti3_P99                                       
*                                                                               
            IAV2_Count OBJECT=IAV2_statements,WORK=WORKP99,            X        
               COUNT_OUT=count_P99                                              
*                                                                               
            MVI   varToken_P99,X'00'                                            
            MVC   varToken_P99+1(VARIANT_SIZ-1),varToken_P99                    
            MVC   varToken_P99+(vt-VARIANT)(4),=A(VT_UNKNOWN)                   
            MVC   varToken_P99+(value-VARIANT)(4),IST8_buildwhen_P99            
*                                                                               
            IAV2_Insert OBJECT=IAV2_statements,WORK=WORKP99,           X        
               INDEX_IN=count_P99,VARIANT_IN=varToken_P99                       
*                                                                               
            IST8_Release OBJECT=IST8_buildwhen_P99,WORK=WORKP99                 
            MVC   IST8_buildwhen_P99,=A(0)                                      
*                                                                               
            XR    R0,R0          * Move until zero terminator                   
            LA    R1,G_BUILDWHEN                                                
            L     R2,ITFO_ti3_P99                                               
            L     R2,TFO_ISTB_token-TFO_obj(,R2)                                
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            MVST  R1,R2          * Move string                                  
            BC    1,*-4          * Move was incomplete, try again               
            MVI   0(R1),X'00'    * Add zero terminator                          
*                                                                               
            B     PRS#99_WRAPUP                                                 
         ENDIF                                                                  
*                                                                               
PRS#99_WRAPUP EQU   *                                                           
         IF (CLC,ITFO_ti2_P99,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti2_P99,WORK=WORKP99                       
            MVC   ITFO_ti2_P99,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti3_P99,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti3_P99,WORK=WORKP99                       
            MVC   ITFO_ti3_P99,=A(0)                                            
         ENDIF                                                                  
*                                                                               
         IF (CLC,ITFO_ti4_P99,NE,=A(0)) THEN                                    
            ITFO_Release OBJECT=ITFO_ti4_P99,WORK=WORKP99                       
            MVC   ITFO_ti4_P99,=A(0)                                            
         ENDIF                                                                  
*                                                                               
PRS#99_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK405D_P99                  DC    C'MAK405D Parsing special',X'00'             
*                                                                               
                             DS    0F                                           
MAK105E_P99                  DC    C'MAK105E .RECIPEPREFIX should be a X        
               single character',X'00'                                          
*                                                                               
                             DS    0F                                           
MAK120E_P99                  DC    C'MAK120E Invalid .BUILDWHEN value 'X        
               ,X'00'                                                           
*                                                                               
                             DS    0F                                           
RECIPEPREFIX_P99             DC    C'.RECIPEPREFIX',X'00'                       
*                                                                               
                             DS    0F                                           
PHONY_P99                    DC    C'.PHONY',X'00'                              
*                                                                               
                             DS    0F                                           
USSHOME_P99                  DC    C'.USSHOME',X'00'                            
*                                                                               
                             DS    0F                                           
BUILDWHEN_P99                DC    C'.BUILDWHEN',X'00'                          
*                                                                               
                             DS    0F                                           
TOM_P99                      DC    C'TOM',X'00'                                 
TUM_P99                      DC    C'TUM',X'00'                                 
TO_P99                       DC    C'TO',X'0000'                                
TU_P99                       DC    C'TU',X'0000'                                
*                                                                               
                             DS    0F                                           
PRS#80A_P99                  DC    A(PRS#80) * appendToken                      
PRS#97A_P99                  DC    A(PRS#97) * parseContinuation                
PRS#98A_P99                  DC    A(PRS#98) * parseVariable                    
*                                                                               
WORKDSAP99                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKP99                      DS    5A                                           
*                                                                               
IPSS_ps_P99                  DS    A                                            
ITOK_tokenizer_P99           DS    A                                            
ITFO_ti1_P99                 DS    A                                            
ITFO_ti2_P99                 DS    A                                            
ITFO_ti3_P99                 DS    A                                            
ITFO_ti4_P99                 DS    A                                            
IST3_phony_P99               DS    A                                            
IST4_recipeprf_P99           DS    A                                            
IST7_home_P99                DS    A                                            
IST8_buildwhen_P99           DS    A                                            
*                                                                               
count_P99                    DS    F                                            
varToken_P99                 DS    CL(VARIANT_SIZ)                              
stringToSplit_P99            DS    A                                            
targetString_P99             DS    A                                            
*                                                                               
WORKDSAP99_SIZ               EQU   *-WORKDSAP99                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPSS QueryInterface                                                           
*                                                                               
PSS#01   MQRYIFCE SUF=S01,IFACE=IPSS                                            
*                                                                               
* IPSS AddRef                                                                   
*                                                                               
PSS#02   MADDREF                                                                
*                                                                               
* IPSS Release                                                                  
*                                                                               
PSS#03   CEEENTRY AUTO=WORKDSAS03_SIZ,MAIN=NO,BASE=(R10)                        
*                                                                               
         USING WORKDSAS03,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global DSECT                         
*                                                                               
         L     R6,0(,R1)         * Param 1 points to this object                
         USING COM_obj,R6                                                       
*                                                                               
         LT    R5,count          * Load current ref count                       
         BZ    PSS#03_RET        * Should never happen....                      
         S     R5,=A(1)          * Decrease ref count                           
         ST    R5,count          * Put new ref count back                       
*                                                                               
*        If reference count dropped to 0, object can be freed                   
         IF (Z) THEN                                                            
            DROP  R6                                                            
            USING PSS_obj,R6                                                    
*                                                                               
            IAV2_Release OBJECT=PSS_IAV2_state,WORK=WORKS03                     
            MVC   PSS_IAV2_state,=A(0)                                          
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
               TR    G_ZONED8(8),0(R15)                                         
               MVI   G_ZONED8+8,X'00'                                           
*                                                                               
               ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKS03                         
               ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS03,      X        
               ZSTR=MAK502D_PSS                                                 
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
PSS#03_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK502D_PSS                  DC    C'MAK502D Deleted IPSS object '              
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
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPSS IsExpectedTokenType                                                      
*                                                                               
PSS#04   CEEENTRY AUTO=WORKDSAS04_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS04,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PSS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   TFO_ti_S04,4(R1)  * Parm 2 is TFO object ptr                     
*                                                                               
         L     R15,8(,R1)        * Parm 3 is ptr to expected flag               
         ST    R15,expectedPtr_S04 * Save as local var                          
         MVI   0(R15),C'N'       * Preset expected to false                     
*                                                                               
         IAV2_Count OBJECT=PSS_IAV2_state,WORK=WORKS04,                X        
               COUNT_OUT=count_S04                                              
*                                                                               
         LT    R2,count_S04       * Check for empty array                       
         BZ    PSS#04_RET         * Skip rest if empty                          
*                                                                               
         BCTR  R2,R0              * Decrease count to get last index            
         ST    R2,lastStateIndex_S04 * Save in local var                        
*                                                                               
*        Query the VARIANT value of the last index                              
         IAV2_Query OBJECT=PSS_IAV2_state,WORK=WORKS04,                X        
               INDEX_IN=lastStateIndex_S04,VARIANT_OUT=varState_S04             
*                                                                               
         L     R2,varState_S04+(value-VARIANT) * Get last state                 
         LR    R3,R2             * Leave R2 intact, continue with R3            
         SLL   R3,2              * Multiply by 4 (scan state table              
*                                * contains full words)                         
         LARL  R1,SCAN_STATE_TABLE * Point R1 to scan state table               
         AR    R1,R3             * Add offset for current scan state            
         MVC   expected_S04,0(R1) * Get the corresponding bitstring             
*                                                                               
         L     R7,TFO_ti_S04     * Get TFO object ptr                           
         L     R7,TFO_tokenType-TFO_obj(,R7) * Get token type                   
*                                                                               
*        AND to see if tokenType's bit is on                                    
         LR    R6,R7                                                            
         IF (N,R6,expected_S04,NZ) THEN                                         
            L     R15,expectedPtr_S04 * Point R15 to return flag                
            MVI   0(R15),C'Y'    * Return expected=Y                            
         ELSE                                                                   
*           For ASSIGNMENT2 and ASSIGNMENT3 the expected masks are              
*           different in SCAN_STATE_TABLE_IN_RECIPE                             
            IF (C,R2,EQ,=A(PARSER_STATE_IN_ASSIGNMENT2)),OR,           X        
               (C,R2,EQ,=A(PARSER_STATE_IN_ASSIGNMENT3)) THEN                   
*                                                                               
*              But should only be checked in we're in a recipe                  
               IAV2_Contains OBJECT=PSS_IAV2_state,WORK=WORKS04,       X        
               VARIANT_IN=varStateInRecipe_S04                                  
*                                                                               
*              If some level up in the state stack we're in recipe              
               IF (C,R15,EQ,=A(1)) THEN                                         
                  LARL  R1,SCAN_STATE_TABLE_IN_RECIPE * Point to table          
                  AR    R1,R3    * Add offset for current scan state            
                  MVC   expected_S04,0(R1) * Get the corresp bit string         
*                                                                               
*                 AND to see if tokenType's bit is on                           
                  LR    R6,R7                                                   
                  IF (N,R6,expected_S04,NZ) THEN                                
                     L     R15,expectedPtr_S04 * Point R15 to ret flag          
                     MVI   0(R15),C'Y' * Return expected=Y                      
                  ENDIF                                                         
               ENDIF                                                            
*           For ADDPDSNAME2A or ADDPDSNAME2B the expected masks are             
*           different in SCAN_STATE_TABLE_IN_RECIPE                             
            ELSEIF (C,R2,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2A)),OR,      X        
               (C,R2,EQ,=A(PARSER_STATE_IN_ADDPDSNAME2B)),OR,          X        
               (C,R2,EQ,=A(PARSER_STATE_IN_APPEND2A)),OR,              X        
               (C,R2,EQ,=A(PARSER_STATE_IN_APPEND2B)),OR,              X        
               (C,R2,EQ,=A(PARSER_STATE_IN_PREPEND2A)),OR,             X        
               (C,R2,EQ,=A(PARSER_STATE_IN_PREPEND2B)),OR,             X        
               (C,R2,EQ,=A(PARSER_STATE_IN_STRIPEXT1)),OR,             X        
               (C,R2,EQ,=A(PARSER_STATE_IN_STRIPEXT2A)),OR,            X        
               (C,R2,EQ,=A(PARSER_STATE_IN_STRIPEXT2B)),OR,            X        
               (C,R2,EQ,=A(PARSER_STATE_IN_FUNCTION2A)),OR,            X        
               (C,R2,EQ,=A(PARSER_STATE_IN_FUNCTION2B)),OR,            X        
               (C,R2,EQ,=A(PARSER_STATE_IN_FUNCTION4A)),OR,            X        
               (C,R2,EQ,=A(PARSER_STATE_IN_FUNCTION4B)),OR,            X        
               (C,R2,EQ,=A(PARSER_STATE_IN_SHFUNCTION2A)),OR,          X        
               (C,R2,EQ,=A(PARSER_STATE_IN_SHFUNCTION2B)) THEN                  
*                                                                               
*              But should only be checked in we're in RULE2 or RULE3,           
*              CALL1 or CALL2, SH1 or SH2                                       
               IAV2_Contains OBJECT=PSS_IAV2_state,WORK=WORKS04,       X        
               VARIANT_IN=varStateInRule2_S04                                   
*                                                                               
               IF (C,R15,NE,=A(1)) THEN                                         
                  IAV2_Contains OBJECT=PSS_IAV2_state,WORK=WORKS04,    X        
               VARIANT_IN=varStateInRule3_S04                                   
               ENDIF                                                            
*                                                                               
               IF (C,R15,NE,=A(1)) THEN                                         
                  IAV2_Contains OBJECT=PSS_IAV2_state,WORK=WORKS04,    X        
               VARIANT_IN=varStateInCall1_S04                                   
               ENDIF                                                            
*                                                                               
               IF (C,R15,NE,=A(1)) THEN                                         
                  IAV2_Contains OBJECT=PSS_IAV2_state,WORK=WORKS04,    X        
               VARIANT_IN=varStateInCall2_S04                                   
               ENDIF                                                            
*                                                                               
               IF (C,R15,NE,=A(1)) THEN                                         
                  IAV2_Contains OBJECT=PSS_IAV2_state,WORK=WORKS04,    X        
               VARIANT_IN=varStateInSh1_S04                                     
               ENDIF                                                            
*                                                                               
               IF (C,R15,NE,=A(1)) THEN                                         
                  IAV2_Contains OBJECT=PSS_IAV2_state,WORK=WORKS04,    X        
               VARIANT_IN=varStateInSh2_S04                                     
               ENDIF                                                            
*                                                                               
               IF (C,R15,EQ,=A(1)) THEN                                         
                  LARL  R1,SCAN_STATE_TABLE_IN_RECIPE * Point to table          
                  AR    R1,R3    * Add offset for current scan state            
                  MVC   expected_S04,0(R1) * Get the corresp bit string         
*                                                                               
*                 AND to see if tokenType's bit is on                           
                  LR    R6,R7                                                   
                  IF (N,R6,expected_S04,NZ) THEN                                
                     L     R15,expectedPtr_S04 * Point R15 to ret flag          
                     MVI   0(R15),C'Y' * Return expected=Y                      
                  ENDIF                                                         
               ENDIF                                                            
            ENDIF                                                               
         ENDIF                                                                  
*                                                                               
PSS#04_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
varStateInRecipe_S04         DC    A(VT_UI4)                                    
                             DC    A(PARSER_STATE_IN_RECIPE)                    
                             DC    A(0)                                         
                             DS    0F                                           
varStateInRule2_S04          DC    A(VT_UI4)                                    
                             DC    A(PARSER_STATE_IN_RULE2)                     
                             DC    A(0)                                         
                             DS    0F                                           
varStateInRule3_S04          DC    A(VT_UI4)                                    
                             DC    A(PARSER_STATE_IN_RULE3)                     
                             DC    A(0)                                         
                             DS    0F                                           
varStateInCall1_S04          DC    A(VT_UI4)                                    
                             DC    A(PARSER_STATE_IN_CALL1)                     
                             DC    A(0)                                         
                             DS    0F                                           
varStateInCall2_S04          DC    A(VT_UI4)                                    
                             DC    A(PARSER_STATE_IN_CALL2)                     
                             DC    A(0)                                         
                             DS    0F                                           
varStateInSh1_S04            DC    A(VT_UI4)                                    
                             DC    A(PARSER_STATE_IN_SH1)                       
                             DC    A(0)                                         
                             DS    0F                                           
varStateInSh2_S04            DC    A(VT_UI4)                                    
                             DC    A(PARSER_STATE_IN_SH2)                       
                             DC    A(0)                                         
*                                                                               
WORKDSAS04                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS04                      DS    4A                                           
*                                                                               
TFO_ti_S04                   DS    A                                            
expectedPtr_S04              DS    A                                            
*                                                                               
count_S04                    DS    F                                            
lastStateIndex_S04           DS    F                                            
*                                                                               
varState_S04                 DS    CL(VARIANT_SIZ)                              
expected_S04                 DS    AL4                                          
*                                                                               
WORKDSAS04_SIZ               EQU   *-WORKDSAS04                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPSS GetLastState                                                             
*                                                                               
PSS#05   CEEENTRY AUTO=WORKDSAS05_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS05,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PSS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   stateOutS05,4(R1) * Parm 2 is fword state ptr                    
*                                                                               
         IAV2_Count OBJECT=PSS_IAV2_state,WORK=WORKS05,                X        
               COUNT_OUT=countS05                                               
*                                                                               
         LT    R2,countS05       * Check for empty array                        
         BZ    PSS#05_RET        * Skip rest if empty                           
*                                                                               
         BCTR  R2,R0             * Decrease count to get last index             
         ST    R2,lastStateIndexS05 * Save in local var                         
*                                                                               
*        Query the VARIANT value of the last index                              
         IAV2_Query OBJECT=PSS_IAV2_state,WORK=WORKS05,                X        
               INDEX_IN=lastStateIndexS05,VARIANT_OUT=varStateS05               
*                                                                               
         L     R14,stateOutS05   * Get ptr to return fword state                
         MVC   0(4,R14),varStateS05+(value-VARIANT) * Copy to state out         
*                                                                               
PSS#05_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
WORKDSAS05                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS05                      DS    4A                                           
*                                                                               
stateOutS05                  DS    A                                            
*                                                                               
countS05                     DS    F                                            
lastStateIndexS05            DS    F                                            
*                                                                               
varStateS05                  DS    CL(VARIANT_SIZ)                              
*                                                                               
WORKDSAS05_SIZ               EQU   *-WORKDSAS05                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPSS AlterLastState                                                           
*                                                                               
PSS#06   CEEENTRY AUTO=WORKDSAS06_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS06,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PSS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   stateInS06,4(R1)  * Parm 2 is new fword state                    
*                                                                               
         IAV2_Count OBJECT=PSS_IAV2_state,WORK=WORKS06,                X        
               COUNT_OUT=countS06                                               
*                                                                               
         LT    R2,countS06       * Check for empty array                        
         BZ    PSS#06_RET        * Skip rest if empty                           
*                                                                               
         BCTR  R2,R0             * Decrease count to get last index             
         ST    R2,lastStateIndexS06 * Save in local var                         
*                                                                               
*        Update the VARIANT value of the last index                             
         MVI   varStateS06,X'00'                                                
         MVC   varStateS06+1(VARIANT_SIZ-1),varStateS06                         
         MVC   varStateS06+vt-VARIANT(4),=A(VT_UI4)                             
         MVC   varStateS06+value-VARIANT(4),stateInS06                          
*                                                                               
         IAV2_Update OBJECT=PSS_IAV2_state,WORK=WORKS06,               X        
               INDEX_IN=lastStateIndexS06,VARIANT_IN=varStateS06                
*                                                                               
         L     R15,G_ILOG                                                       
         IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG) THEN                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKS06                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS06,         X        
               ZSTR=MAK409D_S06                                                 
*                                                                               
            L     R5,stateInS06                                                 
            SLL   R5,2                                                          
            LARL  R7,STATNMTB                                                   
            AR    R7,R5                                                         
            L     R7,0(,R7)                                                     
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS06,         X        
               ZSTR=0(,R7)                                                      
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKS06,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
         ENDIF                                                                  
*                                                                               
PSS#06_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK409D_S06                  DC    C'MAK409D Altered last state to '            
                             DC    X'00'                                        
*                                                                               
WORKDSAS06                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS06                      DS    4A                                           
*                                                                               
stateInS06                   DS    F                                            
*                                                                               
countS06                     DS    F                                            
lastStateIndexS06            DS    F                                            
*                                                                               
varStateS06                  DS    CL(VARIANT_SIZ)                              
*                                                                               
WORKDSAS06_SIZ               EQU   *-WORKDSAS06                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPSS Push                                                                     
*                                                                               
PSS#07   CEEENTRY AUTO=WORKDSAS07_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS07,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PSS_obj,R8        * Address object DSECT                         
*                                                                               
         MVC   stateIn_S07,4(R1) * Parm 2 is state to add                       
*                                                                               
         IAV2_Count OBJECT=PSS_IAV2_state,WORK=WORKS07,                X        
               COUNT_OUT=countS07                                               
*                                                                               
         LT    R2,countS07       * Check for empty array                        
         BZ    PSS#07_RET        * Skip rest if empty                           
*                                                                               
         MVI   varNewStateS07,X'00'                                             
         MVC   varNewStateS07+1(VARIANT_SIZ-1),varNewStateS07                   
         MVC   varNewStateS07+(vt-VARIANT)(4),=A(VT_UI4)                        
         MVC   varNewStateS07+(value-VARIANT)(4),stateIn_S07                    
*                                                                               
*        Insert a new state                                                     
         IAV2_Insert OBJECT=PSS_IAV2_state,WORK=WORKS07,               X        
               INDEX_IN=countS07,VARIANT_IN=varNewStateS07                      
*                                                                               
         L     R15,G_ILOG                                                       
         IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG) THEN                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKS07                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS07,         X        
               ZSTR=MAK408D_S07                                                 
*                                                                               
            L     R5,stateIn_S07                                                
            SLL   R5,2                                                          
            LARL  R7,STATNMTB                                                   
            AR    R7,R5                                                         
            L     R7,0(,R7)                                                     
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS07,         X        
               ZSTR=0(,R7)                                                      
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKS07,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
         ENDIF                                                                  
*                                                                               
PSS#07_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK408D_S07                  DC    C'MAK408D Added last state of '              
                             DC    X'00'                                        
*                                                                               
WORKDSAS07                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS07                      DS    4A                                           
*                                                                               
stateIn_S07                  DS    F                                            
*                                                                               
countS07                     DS    F                                            
varNewStateS07               DS    CL(VARIANT_SIZ)                              
*                                                                               
WORKDSAS07_SIZ               EQU   *-WORKDSAS07                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* IPSS Pop                                                                      
*                                                                               
PSS#08   CEEENTRY AUTO=WORKDSAS08_SIZ,MAIN=NO,BASE=R10                          
*                                                                               
         USING WORKDSAS08,R13    * Address DSA and extra stg for vars           
*                                                                               
         USING GLOBAL,R9         * Address global area DSECT                    
*                                                                               
         L     R8,0(,R1)         * Parm 1 is object ptr                         
         USING PSS_obj,R8        * Address object DSECT                         
*                                                                               
         IAV2_Count OBJECT=PSS_IAV2_state,WORK=WORKS08,                X        
               COUNT_OUT=countS08                                               
*                                                                               
         LT    R2,countS08       * Check for empty array                        
         BZ    PSS#08_RET        * Skip rest if empty                           
*                                                                               
         BCTR  R2,R0             * Decrease count to get last index             
         ST    R2,lastStateIndexS08 * Save in local var                         
*                                                                               
*        Delete the last index                                                  
         IAV2_Delete OBJECT=PSS_IAV2_state,WORK=WORKS08,               X        
               INDEX_IN=lastStateIndexS08                                       
*                                                                               
         L     R15,G_ILOG                                                       
         IF (CLI,13(R15),GE,LOG_LEVEL_DEBUG),AND,                      X        
               (CLC,lastStateIndexS08,GT,=A(0)) THEN                            
            L     R2,lastStateIndexS08                                          
            BCTR  R2,R0          * Decrease last index                          
            ST    R2,lastStateIndexS08 * Save in local var                      
*                                                                               
*           Query the VARIANT value of the last index                           
            IAV2_Query OBJECT=PSS_IAV2_state,WORK=WORKS08,             X        
               INDEX_IN=lastStateIndexS08,VARIANT_OUT=varStateS08               
*                                                                               
            ISTB_Init OBJECT=G_ISTB_tmp,WORK=WORKS08                            
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS08,         X        
               ZSTR=MAK410D_S08                                                 
*                                                                               
            L     R5,varStateS08+(value-VARIANT)                                
            SLL   R5,2                                                          
            LARL  R7,STATNMTB                                                   
            AR    R7,R5                                                         
            L     R7,0(,R7)                                                     
            ISTB_AppendZString OBJECT=G_ISTB_tmp,WORK=WORKS08,         X        
               ZSTR=0(,R7)                                                      
*                                                                               
            L     R2,G_ISTB_tmp                                                 
            L     R2,STB_lpBuf-STB_obj(,R2)                                     
            ILOG_Write OBJECT=G_ILOG,WORK=WORKS08,LINE=0(,R2),         X        
               LOGLEVEL=LOG_LEVEL_DEBUG                                         
         ENDIF                                                                  
*                                                                               
PSS#08_RET EQU   *                                                              
         CEETERM                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
                             DS    0F                                           
MAK410D_S08                  DC    C'MAK410D Reverted to previous stateX        
                of ',X'00'                                                      
*                                                                               
WORKDSAS08                   DSECT                                              
*                                                                               
                             ORG   *+CEEDSASZ                                   
*                                                                               
WORKS08                      DS    4A                                           
*                                                                               
countS08                     DS    F                                            
lastStateIndexS08            DS    F                                            
varStateS08                  DS    CL(VARIANT_SIZ)                              
*                                                                               
WORKDSAS08_SIZ               EQU   *-WORKDSAS08                                 
*                                                                               
LWZMPRS  CSECT                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
PSS#09   MSETPB OBJ=PSS,PROP=PSS_cPhase                                         
*                                                                               
PSS#10   MSETPB OBJ=PSS,PROP=PSS_cRecipePrefix                                  
*                                                                               
* Scan state expected bit flags                                                 
*                                                                               
SCAN_EXPECTED_EOF            EQU   X'80'  *  1                                  
SCAN_EXPECTED_EOL            EQU   X'40'  *  2                                  
SCAN_EXPECTED_COMMENT        EQU   X'20'  *  3                                  
SCAN_EXPECTED_SPECIAL        EQU   X'10'  *  4                                  
SCAN_EXPECTED_EQUALS         EQU   X'08'  *  5                                  
SCAN_EXPECTED_IMMEDEQUALS    EQU   X'04'  *  6                                  
SCAN_EXPECTED_CONDEQUALS     EQU   X'02'  *  7                                  
SCAN_EXPECTED_RULE           EQU   X'01'  *  8                                  
SCAN_EXPECTED_CONTINUATION   EQU   X'80'  *  9                                  
SCAN_EXPECTED_NORMAL         EQU   X'40'  * 10                                  
SCAN_EXPECTED_VARIABLE       EQU   X'20'  * 11                                  
SCAN_EXPECTED_TARGETVAR      EQU   X'10'  * 12                                  
SCAN_EXPECTED_MEMBERVAR      EQU   X'08'  * 13                                  
SCAN_EXPECTED_CLOSEBRACKET   EQU   X'04'  * 14                                  
SCAN_EXPECTED_CLOSECURLY     EQU   X'02'  * 15                                  
SCAN_EXPECTED_RECIPEPREFIX   EQU   X'01'  * 16                                  
SCAN_EXPECTED_CALL           EQU   X'80'  * 17                                  
SCAN_EXPECTED_SH             EQU   X'40'  * 18                                  
SCAN_EXPECTED_COMMA          EQU   X'20'  * 19                                  
SCAN_EXPECTED_LOGSWITCH      EQU   X'10'  * 20                                  
SCAN_EXPECTED_TARGETSWITCH   EQU   X'08'  * 21                                  
*                                                                               
* Regular scan state table (outside of recipe)                                  
SCAN_STATE_TABLE             DS    0F                                           
*                           1         2         3                               
*                  12345678901234567890123456789012                             
         DC    BL4'11110000011000000000000000000000' * UNDETERMINED             
         DC    BL4'01101000000000000000000000000000' * ASSIGNMENT1              
         DC    BL4'00000000111000000000000000000000' * ASSIGNMENT2              
         DC    BL4'11100000111000000000000000000000' * ASSIGNMENT3              
         DC    BL4'00000000111000000000000000000000' * PHONY1                   
         DC    BL4'01100000111000000000000000000000' * PHONY2                   
         DC    BL4'00001111011000000000000000000000' * NORMAL                   
         DC    BL4'01100000000000000000000000000000' * CONTINUATION             
         DC    BL4'00000001111000000000000000000000' * RULE1                    
         DC    BL4'11000000111110000000000000000000' * RULE2                    
         DC    BL4'11000000111110000000000000000000' * RULE3                    
         DC    BL4'00000000111000000000000000000000' * VARIABLE1                
         DC    BL4'00000000000000000000000000000000' * VARIABLE2A               
         DC    BL4'00000000000000000000000000000000' * VARIABLE2B               
         DC    BL4'11110000111000011100000000000000' * RECIPE                   
         DC    BL4'00000000111110010000000000000000' * CALL1                    
         DC    BL4'11000000111110010000000000000000' * CALL2                    
         DC    BL4'00000000111000010000000000000000' * SH1                      
         DC    BL4'11000000111110010000000000000000' * SH2                      
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST1              
         DC    BL4'00000000111000010010000000000000' * MEMBERLIST2A             
         DC    BL4'00000000111000010010000000000000' * MEMBERLIST2B             
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST3              
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST4A             
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST4B             
         DC    BL4'00000000111000010000000000000000' * ADDPDSNAME1              
         DC    BL4'00000000111000010000000000000000' * ADDPDSNAME2A             
         DC    BL4'00000000111000010000000000000000' * ADDPDSNAME2B             
         DC    BL4'00000000111000010000000000000000' * APPEND1                  
         DC    BL4'00000000111000010000000000000000' * APPEND2A                 
         DC    BL4'00000000111000010000000000000000' * APPEND2B                 
         DC    BL4'00000000111000010000000000000000' * PREPEND1                 
         DC    BL4'00000000111000010000000000000000' * PREPEND2A                
         DC    BL4'00000000111000010000000000000000' * PREPEND2B                
         DC    BL4'00000000111000010000000000000000' * STRIPEXT1                
         DC    BL4'00000000111000010000000000000000' * STRIPEXT2A               
         DC    BL4'00000000111000010000000000000000' * STRIPEXT2B               
         DC    BL4'00000000111000010000000000000000' * FUNCTION1                
         DC    BL4'00000000111000010010000000000000' * FUNCTION2A               
         DC    BL4'00000000111000010010000000000000' * FUNCTION2B               
         DC    BL4'00000000111000010000000000000000' * FUNCTION3                
         DC    BL4'00000000111000010000000000000000' * FUNCTION4A               
         DC    BL4'00000000111000010000000000000000' * FUNCTION4B               
         DC    BL4'00000000111000010000000000000000' * SHFUNCTION1              
         DC    BL4'00000000111000010000000000000000' * SHFUNCTION2A             
         DC    BL4'00000000111000010000000000000000' * SHFUNCTION2B             
         DC    BL4'11000000000000000001100000000000' * PARAMETER                
         DC    BL4'00000000010000000000000000000000' * LOGPARM                  
         DC    BL4'00000000011000000000000000000000' * TARGETPARM1              
         DC    BL4'11000000011000000000000000000000' * TARGETPARM2              
*                                                                               
* Scan state when in recipe                                                     
SCAN_STATE_TABLE_IN_RECIPE   DS    0F                                           
*                           1         2         3                               
*                  12345678901234567890123456789012                             
         DC    BL4'11110000011000000000000000000000' * UNDETERMINED             
         DC    BL4'01101000000000000000000000000000' * ASSIGNMENT1              
         DC    BL4'00000000111110000000000000000000' * ASSIGNMENT2              
         DC    BL4'11100000111110000000000000000000' * ASSIGNMENT3              
         DC    BL4'00000000111000000000000000000000' * PHONY1                   
         DC    BL4'01100000111000000000000000000000' * PHONY2                   
         DC    BL4'00001111011000000000000000000000' * NORMAL                   
         DC    BL4'01100000000000010000000000000000' * CONTINUATION             
         DC    BL4'00000001111000000000000000000000' * RULE1                    
         DC    BL4'11000000111110000000000000000000' * RULE2                    
         DC    BL4'11000000111110000000000000000000' * RULE3                    
         DC    BL4'00000000111000000000000000000000' * VARIABLE1                
         DC    BL4'00000000000000000000000000000000' * VARIABLE2A               
         DC    BL4'00000000000000000000000000000000' * VARIABLE2B               
         DC    BL4'11110000111000011100000000000000' * RECIPE                   
         DC    BL4'00000000111110010000000000000000' * CALL1                    
         DC    BL4'11000000111110010000000000000000' * CALL2                    
         DC    BL4'00000000111000010000000000000000' * SH1                      
         DC    BL4'11000000111110010000000000000000' * SH2                      
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST1              
         DC    BL4'00000000111000010010000000000000' * MEMBERLIST2A             
         DC    BL4'00000000111000010010000000000000' * MEMBERLIST2B             
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST3              
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST4A             
         DC    BL4'00000000111000010000000000000000' * MEMBERLIST5B             
         DC    BL4'00000000111000010000000000000000' * ADDPDSNAME1              
         DC    BL4'00000000111010010000000000000000' * ADDPDSNAME2A             
         DC    BL4'00000000111010010000000000000000' * ADDPDSNAME2B             
         DC    BL4'00000000111000010000000000000000' * APPEND1                  
         DC    BL4'00000000111010010000000000000000' * APPEND2A                 
         DC    BL4'00000000111010010000000000000000' * APPEND2B                 
         DC    BL4'00000000111000010000000000000000' * PREPEND1                 
         DC    BL4'00000000111010010000000000000000' * PREPEND2A                
         DC    BL4'00000000111010010000000000000000' * PREPEND2B                
         DC    BL4'00000000111010010000000000000000' * STRIPEXT1                
         DC    BL4'00000000111010010000000000000000' * STRIPEXT2A               
         DC    BL4'00000000111010010000000000000000' * STRIPEXT2B               
         DC    BL4'00000000111000010000000000000000' * FUNCTION1                
         DC    BL4'00000000111010010010000000000000' * FUNCTION2A               
         DC    BL4'00000000111010010010000000000000' * FUNCTION2B               
         DC    BL4'00000000111000010000000000000000' * FUNCTION3                
         DC    BL4'00000000111010010000000000000000' * FUNCTION4A               
         DC    BL4'00000000111010010000000000000000' * FUNCTION5B               
         DC    BL4'00000000111000010000000000000000' * SHFUNCTION1              
         DC    BL4'00000000111010010000000000000000' * SHFUNCTION2A             
         DC    BL4'00000000111010010000000000000000' * SHFUNCTION2B             
         DC    BL4'11000000000000000001000000000000' * PARAMETER                
         DC    BL4'00000000010000000000000000000000' * LOGPARM                  
         DC    BL4'00000000011000000000000000000000' * TARGETPARM1              
         DC    BL4'11000000011000000000000000000000' * TARGETPARM2              
*                                                                               
         DS    0F                                                               
STATNM00 DC    C'UNDETERMINED',X'00'                                            
         DS    0F                                                               
STATNM01 DC    C'IN_ASSIGNMENT1',X'00'                                          
         DS    0F                                                               
STATNM02 DC    C'IN_ASSIGNMENT2',X'00'                                          
         DS    0F                                                               
STATNM03 DC    C'IN_ASSIGNMENT3',X'00'                                          
         DS    0F                                                               
STATNM04 DC    C'IN_PHONY1',X'00'                                               
         DS    0F                                                               
STATNM05 DC    C'IN_PHONY2',X'00'                                               
         DS    0F                                                               
STATNM06 DC    C'IN_NORMAL',X'00'                                               
         DS    0F                                                               
STATNM07 DC    C'IN_CONTINUATION',X'00'                                         
         DS    0F                                                               
STATNM08 DC    C'IN_RULE1',X'00'                                                
         DS    0F                                                               
STATNM09 DC    C'IN_RULE2',X'00'                                                
         DS    0F                                                               
STATNM10 DC    C'IN_RULE3',X'00'                                                
         DS    0F                                                               
STATNM11 DC    C'IN_VARIABLE1',X'00'                                            
         DS    0F                                                               
STATNM12 DC    C'IN_VARIABLE2A',X'00'                                           
         DS    0F                                                               
STATNM13 DC    C'IN_VARIABLE2B',X'00'                                           
         DS    0F                                                               
STATNM14 DC    C'IN_RECIPE',X'00'                                               
         DS    0F                                                               
STATNM15 DC    C'IN_CALL1',X'00'                                                
         DS    0F                                                               
STATNM16 DC    C'IN_CALL2',X'00'                                                
         DS    0F                                                               
STATNM17 DC    C'IN_SH1',X'00'                                                  
         DS    0F                                                               
STATNM18 DC    C'IN_SH2',X'00'                                                  
         DS    0F                                                               
STATNM19 DC    C'IN_MEMBERLIST1',X'00'                                          
         DS    0F                                                               
STATNM20 DC    C'IN_MEMBERLIST2A',X'00'                                         
         DS    0F                                                               
STATNM21 DC    C'IN_MEMBERLIST2B',X'00'                                         
         DS    0F                                                               
STATNM22 DC    C'IN_MEMBERLIST3',X'00'                                          
         DS    0F                                                               
STATNM23 DC    C'IN_MEMBERLIST4A',X'00'                                         
         DS    0F                                                               
STATNM24 DC    C'IN_MEMBERLIST4B',X'00'                                         
         DS    0F                                                               
STATNM25 DC    C'IN_ADDPDSNAME1',X'00'                                          
         DS    0F                                                               
STATNM26 DC    C'IN_ADDPDSNAME2A',X'00'                                         
         DS    0F                                                               
STATNM27 DC    C'IN_ADDPDSNAME2B',X'00'                                         
         DS    0F                                                               
STATNM28 DC    C'IN_APPEND1',X'00'                                              
         DS    0F                                                               
STATNM29 DC    C'IN_APPEND2A',X'00'                                             
         DS    0F                                                               
STATNM30 DC    C'IN_APPEND2B',X'00'                                             
         DS    0F                                                               
STATNM31 DC    C'IN_PREPEND1',X'00'                                             
         DS    0F                                                               
STATNM32 DC    C'IN_PREPEND2A',X'00'                                            
         DS    0F                                                               
STATNM33 DC    C'IN_PREPEND2B',X'00'                                            
         DS    0F                                                               
STATNM34 DC    C'IN_STRIPEXT1',X'00'                                            
         DS    0F                                                               
STATNM35 DC    C'IN_STRIPEXT2A',X'00'                                           
         DS    0F                                                               
STATNM36 DC    C'IN_STRIPEXT2B',X'00'                                           
         DS    0F                                                               
STATNM37 DC    C'IN_FUNCTION1',X'00'                                            
         DS    0F                                                               
STATNM38 DC    C'IN_FUNCTION2A',X'00'                                           
         DS    0F                                                               
STATNM39 DC    C'IN_FUNCTION2B',X'00'                                           
         DS    0F                                                               
STATNM40 DC    C'IN_FUNCTION3',X'00'                                            
         DS    0F                                                               
STATNM41 DC    C'IN_FUNCTION4A',X'00'                                           
         DS    0F                                                               
STATNM42 DC    C'IN_FUNCTION4B',X'00'                                           
         DS    0F                                                               
STATNM43 DC    C'IN_SHFUNCTION1',X'00'                                          
         DS    0F                                                               
STATNM44 DC    C'IN_SHFUNCTION2A',X'00'                                         
         DS    0F                                                               
STATNM45 DC    C'IN_SHFUNCTION2B',X'00'                                         
         DS    0F                                                               
STATNM46 DC    C'IN_PARAMETER',X'00'                                            
         DS    0F                                                               
STATNM47 DC    C'IN_LOGPARM',X'00'                                              
         DS    0F                                                               
STATNM48 DC    C'IN_TARGETPARM1',X'00'                                          
         DS    0F                                                               
STATNM49 DC    C'IN_TARGETPARM2',X'00'                                          
*                                                                               
         DS    0F                                                               
STATNMTB DC    A(STATNM00)                                                      
         DC    A(STATNM01)                                                      
         DC    A(STATNM02)                                                      
         DC    A(STATNM03)                                                      
         DC    A(STATNM04)                                                      
         DC    A(STATNM05)                                                      
         DC    A(STATNM06)                                                      
         DC    A(STATNM07)                                                      
         DC    A(STATNM08)                                                      
         DC    A(STATNM09)                                                      
         DC    A(STATNM10)                                                      
         DC    A(STATNM11)                                                      
         DC    A(STATNM12)                                                      
         DC    A(STATNM13)                                                      
         DC    A(STATNM14)                                                      
         DC    A(STATNM15)                                                      
         DC    A(STATNM16)                                                      
         DC    A(STATNM17)                                                      
         DC    A(STATNM18)                                                      
         DC    A(STATNM19)                                                      
         DC    A(STATNM20)                                                      
         DC    A(STATNM21)                                                      
         DC    A(STATNM22)                                                      
         DC    A(STATNM23)                                                      
         DC    A(STATNM24)                                                      
         DC    A(STATNM25)                                                      
         DC    A(STATNM26)                                                      
         DC    A(STATNM27)                                                      
         DC    A(STATNM28)                                                      
         DC    A(STATNM29)                                                      
         DC    A(STATNM30)                                                      
         DC    A(STATNM31)                                                      
         DC    A(STATNM32)                                                      
         DC    A(STATNM33)                                                      
         DC    A(STATNM34)                                                      
         DC    A(STATNM35)                                                      
         DC    A(STATNM36)                                                      
         DC    A(STATNM37)                                                      
         DC    A(STATNM38)                                                      
         DC    A(STATNM39)                                                      
         DC    A(STATNM40)                                                      
         DC    A(STATNM41)                                                      
         DC    A(STATNM42)                                                      
         DC    A(STATNM43)                                                      
         DC    A(STATNM44)                                                      
         DC    A(STATNM45)                                                      
         DC    A(STATNM46)                                                      
         DC    A(STATNM47)                                                      
         DC    A(STATNM48)                                                      
         DC    A(STATNM49)                                                      
*                                                                               
         COPY  REGS              * Register equates                             
*                                                                               
         END   LWZMPRS                                                          
