/* REXX */                                                                      
PARSE ARG g.arg                                                                 
PARSE SOURCE . . g.rexxname .                                                   
                                                                                
CALL init                                                                       
                                                                                
CALL parseArguments                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   CALL allocDDs                                                                
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   CALL invokeASMA90                                                            
END                                                                             
                                                                                
CALL freeDDs                                                                    
                                                                                
EXIT g.error                                                                    
                                                                                
/**********************************************************************/        
/* Initialize                                                         */        
/**********************************************************************/        
init: PROCEDURE EXPOSE g. SIGL                                                  
                                                                                
SAY COPIES('*',100)                                                             
SAY '* ASMA'                                                                    
SAY COPIES('*',100)                                                             
                                                                                
g.error = 0                                                                     
                                                                                
g.src = ""                                                                      
g.obj = ""                                                                      
g.syslib.0 = 0                                                                  
g.sysprint = ""                                                                 
                                                                                
g.SYSLIN.allocated = 0                                                          
g.SYSLIB.allocated = 0                                                          
g.SYSIN.allocated = 0                                                           
g.SYSPRINT.allocated = 0                                                        
g.SYSPUNCH.allocated = 0                                                        
g.SYSUT1.allocated = 0                                                          
g.SYSTERM.allocated = 0                                                         
g.SYSADATA.allocated = 0                                                        
g.ASMAOPT.allocated = 0                                                         
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Allocate DD's for invoking ASMA90                                  */        
/**********************************************************************/        
allocDDs: PROCEDURE EXPOSE g. SIGL                                              
                                                                                
_rc = BPXWDYN("ALLOC DSN('"g.obj"') SHR RTDDN(_ddn)")                           
                                                                                
IF _rc == 0 THEN DO                                                             
   g.SYSLIN.allocated = 1                                                       
   g.SYSLIN.ddname = _ddn                                                       
END                                                                             
ELSE DO                                                                         
   g.error = 8                                                                  
   IF _rc > 0 THEN _rc = D2X(_rc)                                               
   CALL log 'Dynamic allocation of 'g.obj' failed with '_rc                     
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   IF g.syslib.0 == 0 THEN DO                                                   
      _rc = BPXWDYN("ALLOC DUMMY SHR RTDDN(_ddn)")                              
                                                                                
      IF _rc == 0 THEN DO                                                       
         g.SYSLIB.allocated = 1                                                 
         g.SYSLIB.ddname = _ddn                                                 
      END                                                                       
      ELSE DO                                                                   
         g.error = 8                                                            
         IF _rc > 0 THEN _rc = D2X(_rc)                                         
         CALL log 'Dynamic allocation of 'g.obj' failed with '_rc               
      END                                                                       
   END                                                                          
   ELSE DO                                                                      
      _rc = BPXWDYN("ALLOC DSN('"g.syslib.1"') SHR RTDDN(_ddn)")                
                                                                                
      IF _rc == 0 THEN DO                                                       
         g.SYSLIB.allocated = 1                                                 
         g.SYSLIB.ddname = _ddn                                                 
      END                                                                       
      ELSE DO                                                                   
         g.error = 8                                                            
         IF _rc > 0 THEN _rc = D2X(_rc)                                         
         CALL log 'Dynamic allocation of 'g.syslib.1' failed with '_rc          
      END                                                                       
                                                                                
      DO i = 2 TO g.syslib.0 WHILE g.error == 0                                 
         _rc = BPXWDYN("ALLOC DSN('"g.syslib.i"') SHR RTDDN(_ddn)")             
                                                                                
         IF _rc == 0 THEN DO                                                    
            _rc = BPXWDYN("CONCAT DDLIST("g.SYSLIB.ddname","_ddn")")            
                                                                                
            IF _rc ¬= 0 THEN DO                                                 
               g.error = 8                                                      
               IF _rc > 0 THEN _rc = D2X(_rc)                                   
               CALL log 'Dynamic concatenation failed with '_rc                 
            END                                                                 
         END                                                                    
         ELSE DO                                                                
            g.error = 8                                                         
            IF _rc > 0 THEN _rc = D2X(_rc)                                      
            CALL log 'Dynamic allocation of 'g.syslib.i' failed with '_rc       
         END                                                                    
      END                                                                       
   END                                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _rc = BPXWDYN("ALLOC DSN('"g.src"') SHR RTDDN(_ddn)")                        
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSIN.allocated = 1                                                     
      g.SYSIN.ddname = _ddn                                                     
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Dynamic allocation of 'g.src' failed with '_rc                  
   END                                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _rc = BPXWDYN("ALLOC NEW RECFM(V,B,M) DSORG(PS) LRECL(133) CYL" || ,         
                 " SPACE(1,1) RTDDN(_ddn)")                                     
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSPRINT.allocated = 1                                                  
      g.SYSPRINT.ddname = _ddn                                                  
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Dynamic allocation of SYSPRINT failed with '_rc                 
   END                                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")                                     
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSPUNCH.allocated = 1                                                  
      g.SYSPUNCH.ddname = _ddn                                                  
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Dynamic allocation of SYSPUNCH failed with '_rc                 
   END                                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _rc = BPXWDYN("ALLOC NEW BLOCK(4096) SPACE(120,120) RTDDN(_ddn)")            
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSUT1.allocated = 1                                                    
      g.SYSUT1.ddname = _ddn                                                    
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Dynamic allocation of SYSUT1 failed with '_rc                   
   END                                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")                                     
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSTERM.allocated = 1                                                   
      g.SYSTERM.ddname = _ddn                                                   
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Dynamic allocation of SYSTERM failed with '_rc                  
   END                                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")                                     
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSADATA.allocated = 1                                                  
      g.SYSADATA.ddname = _ddn                                                  
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Dynamic allocation of SYSADATA failed with '_rc                 
   END                                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _rc = BPXWDYN("ALLOC DUMMY RTDDN(_ddn)")                                     
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.ASMAOPT.allocated = 1                                                   
      g.ASMAOPT.ddname = _ddn                                                   
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Dynamic allocation of ASMAOPT failed with '_rc                  
   END                                                                          
END                                                                             
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Free DD's after invoking ASMA90                                    */        
/**********************************************************************/        
freeDDs: PROCEDURE EXPOSE g. SIGL                                               
                                                                                
IF g.SYSLIN.allocated == 1 THEN DO                                              
   _rc = BPXWDYN("FREE FI("g.SYSLIN.ddname")")                                  
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSLIN.allocated = 0                                                    
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSLIN.ddname' failed with '_rc                 
   END                                                                          
END                                                                             
                                                                                
IF g.SYSLIB.allocated == 1 THEN DO                                              
   _rc = BPXWDYN("FREE FI("g.SYSLIB.ddname")")                                  
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSLIB.allocated = 0                                                    
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSLIB.ddname' failed with '_rc                 
   END                                                                          
END                                                                             
                                                                                
IF g.SYSIN.allocated == 1 THEN DO                                               
   _rc = BPXWDYN("FREE FI("g.SYSIN.ddname")")                                   
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSIN.allocated = 0                                                     
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSIN.ddname' failed with '_rc                  
   END                                                                          
END                                                                             
                                                                                
IF g.SYSPRINT.allocated == 1 THEN DO                                            
   _rc = BPXWDYN("FREE FI("g.SYSPRINT.ddname")")                                
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSPRINT.allocated = 0                                                  
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSPRINT.ddname' failed with '_rc               
   END                                                                          
END                                                                             
                                                                                
IF g.SYSPUNCH.allocated == 1 THEN DO                                            
   _rc = BPXWDYN("FREE FI("g.SYSPUNCH.ddname")")                                
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSPUNCH.allocated = 0                                                  
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSPUNCH.ddname' failed with '_rc               
   END                                                                          
END                                                                             
                                                                                
IF g.SYSUT1.allocated == 1 THEN DO                                              
   _rc = BPXWDYN("FREE FI("g.SYSUT1.ddname")")                                  
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSUT1.allocated = 0                                                    
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSUT1.ddname' failed with '_rc                 
   END                                                                          
END                                                                             
                                                                                
IF g.SYSTERM.allocated == 1 THEN DO                                             
   _rc = BPXWDYN("FREE FI("g.SYSTERM.ddname")")                                 
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSTERM.allocated = 0                                                   
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSTERM.ddname' failed with '_rc                
   END                                                                          
END                                                                             
                                                                                
IF g.SYSADATA.allocated == 1 THEN DO                                            
   _rc = BPXWDYN("FREE FI("g.SYSADATA.ddname")")                                
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.SYSADATA.allocated = 0                                                  
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.SYSADATA.ddname' failed with '_rc               
   END                                                                          
END                                                                             
                                                                                
IF g.ASMAOPT.allocated == 1 THEN DO                                             
   _rc = BPXWDYN("FREE FI("g.ASMAOPT.ddname")")                                 
                                                                                
   IF _rc == 0 THEN DO                                                          
      g.ASMAOPT.allocated = 0                                                   
   END                                                                          
   ELSE DO                                                                      
      g.error = 8                                                               
      IF _rc > 0 THEN _rc = D2X(_rc)                                            
      CALL log 'Free of file 'g.ASMAOPT.ddname' failed with '_rc                
   END                                                                          
END                                                                             
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Invoke ASMA90                                                      */        
/**********************************************************************/        
invokeASMA90: PROCEDURE EXPOSE g. SIGL                                          
                                                                                
_prog = 'ASMA90'                                                                
_parm = ''                                                                      
_ddlist = LEFT(g.SYSLIN.ddname,8) || ,                                          
          COPIES('00'X,8) || ,                                                  
          COPIES('00'X,8) || ,                                                  
          LEFT(g.SYSLIB.ddname,8) || ,                                          
          LEFT(g.SYSIN.ddname,8) || ,                                           
          LEFT(g.SYSPRINT.ddname,8) || ,                                        
          LEFT(g.SYSPUNCH.ddname,8) || ,                                        
          LEFT(g.SYSUT1.ddname,8) || ,                                          
          COPIES('00'X,8) || ,                                                  
          COPIES('00'X,8) || ,                                                  
          COPIES('00'X,8) || ,                                                  
          LEFT(g.SYSTERM.ddname,8) || ,                                         
          COPIES('00'X,8) || ,                                                  
          COPIES('00'X,8) || ,                                                  
          COPIES('00'X,8) || ,                                                  
          LEFT(g.SYSADATA.ddname,8) || ,                                        
          COPIES('00'X,8) || ,                                                  
          COPIES('00'X,8) || ,                                                  
          COPIES('00'X,8) || ,                                                  
          LEFT(g.ASMAOPT.ddname,8)                                              
                                                                                
ADDRESS LINKMVS _prog '_parm _ddlist'                                           
                                                                                
"EXECIO * DISKR "g.SYSPRINT.ddname" (STEM _sysprint. FINIS"                     
                                                                                
_rc = BPXWDYN("ALLOC SYSOUT(A) RTDDN(_ddn)")                                    
                                                                                
IF _rc == 0 THEN DO                                                             
   "EXECIO "_sysprint.0" DISKW "_ddn" (STEM _sysprint. FINIS"                   
END                                                                             
ELSE DO                                                                         
   SAY _rc                                                                      
END                                                                             
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Parse arguments                                                    */        
/**********************************************************************/        
parseArguments: PROCEDURE EXPOSE g. SIGL                                        
                                                                                
g.parser.EXPECTED_EOF = 1                                                       
g.parser.EXPECTED_NORMAL = 2                                                    
g.parser.EXPECTED_OPEN_BRACKET = 4                                              
g.parser.EXPECTED_CLOSE_BRACKET = 8                                             
g.parser.EXPECTED_DOT = 16                                                      
                                                                                
g.parser.SCAN_STATE_NOT_IN_PARM = 1                                             
g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM = g.parser.EXPECTED_EOF + ,             
                                          g.parser.EXPECTED_NORMAL              
g.parser.SCAN_STATE_IN_PARM1 = 2                                                
g.parser.EXPECTED_FOR_STATE_IN_PARM1 = g.parser.EXPECTED_OPEN_BRACKET           
g.parser.SCAN_STATE_IN_PARM2 = 3                                                
g.parser.EXPECTED_FOR_STATE_IN_PARM2 = g.parser.EXPECTED_NORMAL + ,             
                                       g.parser.EXPECTED_CLOSE_BRACKET          
g.parser.SCAN_STATE_IN_PARM3 = 4                                                
g.parser.EXPECTED_FOR_STATE_IN_PARM3 = g.parser.EXPECTED_NORMAL + ,             
                                       g.parser.EXPECTED_CLOSE_BRACKET          
g.parser.SCAN_STATE_IN_DSNAME1 = 5                                              
g.parser.EXPECTED_FOR_STATE_IN_DSNAME1 = g.parser.EXPECTED_NORMAL + ,           
                                         g.parser.EXPECTED_CLOSE_BRACKET        
g.parser.SCAN_STATE_IN_DSNAME2 = 6                                              
g.parser.EXPECTED_FOR_STATE_IN_DSNAME2 = g.parser.EXPECTED_OPEN_BRACKET + ,     
                                         g.parser.EXPECTED_CLOSE_BRACKET + ,    
                                         g.parser.EXPECTED_DOT                  
g.parser.SCAN_STATE_IN_DSNAME3 = 7                                              
g.parser.EXPECTED_FOR_STATE_IN_DSNAME3 = g.parser.EXPECTED_NORMAL               
g.parser.SCAN_STATE_IN_DSNAME4 = 8                                              
g.parser.EXPECTED_FOR_STATE_IN_DSNAME4 = g.parser.EXPECTED_NORMAL               
g.parser.SCAN_STATE_IN_DSNAME5 = 9                                              
g.parser.EXPECTED_FOR_STATE_IN_DSNAME5 = g.parser.EXPECTED_CLOSE_BRACKET        
                                                                                
g.parser.scanState = 1                                                          
g.parser.scanStateTable.1 = g.parser.EXPECTED_FOR_STATE_NOT_IN_PARM             
g.parser.scanStateTable.2 = g.parser.EXPECTED_FOR_STATE_IN_PARM1                
g.parser.scanStateTable.3 = g.parser.EXPECTED_FOR_STATE_IN_PARM2                
g.parser.scanStateTable.4 = g.parser.EXPECTED_FOR_STATE_IN_PARM3                
g.parser.scanStateTable.5 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME1              
g.parser.scanStateTable.6 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME2              
g.parser.scanStateTable.7 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME3              
g.parser.scanStateTable.8 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME4              
g.parser.scanStateTable.9 = g.parser.EXPECTED_FOR_STATE_IN_DSNAME5              
                                                                                
_parmName = ""                                                                  
                                                                                
CALL initLexer                                                                  
                                                                                
DO WHILE g.error == 0                                                           
   CALL lexerGetToken                                                           
                                                                                
   IF g.error ¬= 0 | g.scanner.currChar == 'EOF' THEN LEAVE                     
                                                                                
   _parmName = g.lexer.currToken                                                
                                                                                
   g.parser.scanState = g.parser.SCAN_STATE_IN_PARM1                            
   CALL lexerGetToken                                                           
   IF g.error ¬= 0 | g.scanner.currChar == 'EOF' THEN LEAVE                     
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME1                          
   CALL lexerGetToken                                                           
   IF g.error ¬= 0 | g.scanner.currChar == 'EOF' THEN LEAVE                     
   DO WHILE g.lexer.currToken ¬= ')'                                            
      _dsname = parseDsname()                                                   
      IF g.error ¬= 0 | g.scanner.currChar == 'EOF' THEN LEAVE                  
      SELECT                                                                    
         WHEN _parmName == 'SOURCE' THEN DO                                     
            g.src = _dsname                                                     
         END                                                                    
         WHEN _parmName == 'OBJECT' THEN DO                                     
            g.obj = _dsname                                                     
         END                                                                    
         WHEN _parmName == 'SYSLIB' THEN DO                                     
            _nextSyslib = g.syslib.0 + 1                                        
            g.syslib.0 = _nextSyslib                                            
            g.syslib._nextSyslib = _dsname                                      
         END                                                                    
         OTHERWISE                                                              
            NOP                                                                 
      END                                                                       
      g.parser.scanState = g.parser.SCAN_STATE_IN_PARM3                         
      CALL lexerGetToken                                                        
      IF g.error ¬= 0 | g.scanner.currChar == 'EOF' THEN LEAVE                  
      IF g.lexer.currToken ¬= ')' THEN DO                                       
         IF _parmName == 'SOURCE' | _parmName == 'OBJECT' THEN DO               
            CALL log 'Only single dataset allowed at pos 'g.scanner.colIndex    
            g.error = 8                                                         
            RETURN                                                              
         END                                                                    
      END                                                                       
   END                                                                          
   IF g.error ¬= 0 | g.scanner.currChar == 'EOF' THEN LEAVE                     
   g.parser.scanState = g.parser.SCAN_STATE_NOT_IN_PARM                         
END                                                                             
                                                                                
IF g.src == "" THEN DO                                                          
   CALL log 'SOURCE(...) expected but not found or specified wrong'             
   g.error = 8                                                                  
END                                                                             
                                                                                
IF g.error == 0 & g.obj == "" THEN DO                                           
   CALL log 'OBJECT(...) expected but not found or specified wrong'             
   g.error = 8                                                                  
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   SAY 'SOURCE:  'g.src                                                         
   SAY 'OBJECT:  'g.obj                                                         
   DO i = 1 to g.syslib.0                                                       
      SAY 'SYSLIB:  'g.syslib.i                                                 
   END                                                                          
END                                                                             
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Parse data set name                                                */        
/**********************************************************************/        
parseDsname: PROCEDURE EXPOSE g. SIGL                                           
                                                                                
_dsname = g.lexer.currToken                                                     
DO WHILE g.error == 0                                                           
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME2                          
   CALL lexerGetToken                                                           
   IF g.error ¬= 0 THEN LEAVE                                                   
   IF g.lexer.currToken ¬= '.' THEN LEAVE                                       
   _dsname = _dsname || g.lexer.currToken                                       
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME3                          
   CALL lexerGetToken                                                           
   IF g.error ¬= 0 THEN LEAVE                                                   
   _dsname = _dsname || g.lexer.currToken                                       
   IF g.scanner.peekChar ¬= '.' & g.scanner.peekChar ¬= '(' THEN LEAVE          
END                                                                             
                                                                                
IF g.lexer.currToken == '(' THEN DO                                             
   _dsname = _dsname || g.lexer.currToken                                       
   g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME4                          
   CALL lexerGetToken                                                           
   IF g.error == 0 THEN DO                                                      
      _dsname = _dsname || g.lexer.currToken                                    
      g.parser.scanState = g.parser.SCAN_STATE_IN_DSNAME5                       
      CALL lexerGetToken                                                        
   END                                                                          
   IF g.error == 0 THEN DO                                                      
      _dsname = _dsname || g.lexer.currToken                                    
   END                                                                          
END                                                                             
                                                                                
RETURN _dsname                                                                  
                                                                                
/**********************************************************************/        
/* Initialize lexer                                                   */        
/**********************************************************************/        
initLexer: PROCEDURE EXPOSE g. SIGL                                             
                                                                                
g.upperArg = TRANSLATE(g.arg)                                                   
g.upperArgLen = LENGTH(g.upperArg)                                              
                                                                                
g.scanner.colIndex = 0                                                          
                                                                                
g.lexer.IDENTIFIER_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                         
g.lexer.IDENTIFIER_STARTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                    
g.lexer.IDENTIFIER_CHARS = g.lexer.IDENTIFIER_STARTCHARS || "0123456789@#$"     
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Lexer get token                                                    */        
/**********************************************************************/        
lexerGetToken: PROCEDURE EXPOSE g. SIGL                                         
                                                                                
g.lexer.currToken = ''                                                          
                                                                                
CALL scannerGetChar                                                             
                                                                                
DO WHILE g.error == 0 & g.scanner.currChar == ' '                               
   CALL scannerGetChar                                                          
END                                                                             
                                                                                
IF g.error == 0 THEN DO                                                         
   _state = g.parser.scanState                                                  
                                                                                
   IF g.scanner.currChar == 'EOF' THEN DO                                       
      _expected = g.parser.scanStateTable._state                                
      IF C2D(BITAND(D2C(_expected), D2C(g.parser.EXPECTED_EOF))) ¬= 0 THEN DO   
         g.lexer.currToken = g.scanner.currChar                                 
      END                                                                       
      ELSE DO                                                                   
         CALL log 'Unexpected end of parameter at pos 'g.scanner.colIndex       
         g.error = 8                                                            
      END                                                                       
      SIGNAL lexerGetToken_complete                                             
   END                                                                          
                                                                                
   IF g.scanner.currChar == '.' THEN DO                                         
      _expected = g.parser.scanStateTable._state                                
      IF C2D(BITAND(D2C(_expected), D2C(g.parser.EXPECTED_DOT))) ¬= 0 THEN DO   
         g.lexer.currToken = g.scanner.currChar                                 
      END                                                                       
      ELSE DO                                                                   
         CALL log 'Unexpected "." at pos 'g.scanner.colIndex                    
         g.error = 8                                                            
      END                                                                       
      SIGNAL lexerGetToken_complete                                             
   END                                                                          
                                                                                
   IF g.scanner.currChar == '(' THEN DO                                         
      _expected = g.parser.scanStateTable._state                                
      IF C2D(BITAND(D2C(_expected), ,                                           
                    D2C(g.parser.EXPECTED_OPEN_BRACKET))) ¬= 0 THEN DO          
         g.lexer.currToken = g.scanner.currChar                                 
      END                                                                       
      ELSE DO                                                                   
         CALL log 'Unexpected "(" at pos 'g.scanner.colIndex                    
         g.error = 8                                                            
      END                                                                       
      SIGNAL lexerGetToken_complete                                             
   END                                                                          
                                                                                
   IF g.scanner.currChar == ')' THEN DO                                         
      _expected = g.parser.scanStateTable._state                                
      IF C2D(BITAND(D2C(_expected), ,                                           
                    D2C(g.parser.EXPECTED_CLOSE_BRACKET))) ¬= 0 THEN DO         
         g.lexer.currToken = g.scanner.currChar                                 
      END                                                                       
      ELSE DO                                                                   
         CALL log 'Unexpected ")" at pos 'g.scanner.colIndex                    
         g.error = 8                                                            
      END                                                                       
      SIGNAL lexerGetToken_complete                                             
   END                                                                          
                                                                                
   IF VERIFY(g.scanner.currChar, g.lexer.IDENTIFIER_STARTCHARS) == 0 THEN DO    
      _expected = g.parser.scanStateTable._state                                
      IF C2D(BITAND(D2C(_expected), ,                                           
                    D2C(g.parser.EXPECTED_NORMAL))) == 0 THEN DO                
         CALL log 'Unexpected identifier at pos 'g.scanner.colIndex             
         g.error = 8                                                            
         SIGNAL lexerGetToken_complete                                          
      END                                                                       
      g.lexer.currToken = g.scanner.currChar                                    
      DO WHILE g.error == 0 & g.scanner.currChar ¬= 'EOF' & ,                   
               VERIFY(g.scanner.peekChar, g.lexer.IDENTIFIER_CHARS) == 0        
         CALL scannerGetChar                                                    
                                                                                
         g.lexer.currToken = g.lexer.currToken || g.scanner.currChar            
      END                                                                       
      SIGNAL lexerGetToken_complete                                             
   END                                                                          
                                                                                
   CALL log 'Unexpected character at 'g.scanner.colIndex                        
   g.error = 8                                                                  
                                                                                
END                                                                             
                                                                                
lexerGetToken_complete:                                                         
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Scanner of parameter characters                                    */        
/**********************************************************************/        
scannerGetChar: PROCEDURE EXPOSE g. SIGL                                        
                                                                                
g.scanner.colIndex = g.scanner.colIndex + 1                                     
g.scanner.peekChar = ''                                                         
                                                                                
IF g.scanner.colIndex > g.upperArgLen THEN DO                                   
   g.scanner.currChar = 'EOF'                                                   
END                                                                             
ELSE DO                                                                         
   g.scanner.currChar = SUBSTR(g.upperArg, g.scanner.colIndex, 1)               
   IF g.scanner.colIndex < g.upperArgLen THEN DO                                
      g.scanner.peekChar = SUBSTR(g.upperArg, g.scanner.colIndex + 1, 1)        
   END                                                                          
END                                                                             
                                                                                
RETURN                                                                          
                                                                                
/**********************************************************************/        
/* Log a message                                                      */        
/**********************************************************************/        
log: PROCEDURE EXPOSE g. SIGL                                                   
                                                                                
PARSE ARG _msg                                                                  
                                                                                
SAY g.rexxname SIGL _msg                                                        
                                                                                
RETURN                                                                          
