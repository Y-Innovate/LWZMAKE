//*                                                                             
//STEP001 EXEC PGM=IEBGENER                                                     
//SYSUT1    DD *                                                                
Why did the {animal} cross the road?                                            
{reason}                                                                        
//SYSIN     DD DUMMY                                                            
//SYSUT2    DD SYSOUT=*                                                         
//SYSPRINT  DD SYSOUT=*                                                         
