/* REXX*/
PARSE ARG _myArg
PARSE VAR _myArg _ds'*'

_nr = "0000001"

/*********************************************************************/
/* INITIALIZE THE PARM LIST                                          */
/*********************************************************************/
MODRSNRC = SUBSTR(' ',1,4)          /*   CLEAR MODULE/RETURN/REASON  */
CSIFILTK = SUBSTR(_myArg,1,44)      /*   MOVE FILTER KEY INTO LIST   */
CSICATNM = SUBSTR(' ',1,44)         /*   CLEAR CATALOG NAME          */
CSIRESNM = SUBSTR(' ',1,44)         /*   CLEAR RESUME NAME           */
CSIDTYPS = SUBSTR(' ',1,16)         /*   CLEAR ENTRY TYPES           */
CSICLDI  = SUBSTR(' ',1,1)          /*   INDICATE DATA AND INDEX     */
CSIRESUM = SUBSTR(' ',1,1)          /*   CLEAR RESUME FLAG           */
CSIS1CAT = SUBSTR(' ',1,1)          /*   INDICATE SEARCH > 1 CATALOGS*/
CSIRESRV = SUBSTR(' ',1,1)          /*   CLEAR RESERVE CHARACTER     */
CSINUMEN = '0000'X                  /*   INIT NUMBER OF FIELDS       */
/*********************************************************************/
/* BUILD THE SELECTION CRITERIA FIELDS PART OF PARAMETER LIST        */
/*********************************************************************/
CSIOPTS  = CSICLDI || CSIRESUM || CSIS1CAT || CSIRESRV
CSIFIELD = CSIFILTK || CSICATNM || CSIRESNM || CSIDTYPS || CSIOPTS
CSIFIELD = CSIFIELD || CSINUMEN

/*********************************************************************/
/* INITIALIZE AND BUILD WORK ARE OUTPUT PART OF PARAMETER LIST       */
/*********************************************************************/
WORKLEN = 64000
DWORK = '0000FA00'X || COPIES('00'X,WORKLEN-4)

/*********************************************************************/
/* INITIALIZE WORK VARIABLES                                         */
/*********************************************************************/
RESUME = 'Y'
PREVNAME = ''
DNAMET = SUBSTR(' ',1,44)

/*********************************************************************/
/* SET UP LOOP FOR RESUME (IF A RESUME IS NCESSARY)                  */
/*********************************************************************/
DO WHILE RESUME = 'Y'
   /******************************************************************/
   /*  ISSUE LINK TO CATALOG GENERIC FILTER INTERFACE                */
   /******************************************************************/
   ADDRESS LINKPGM 'IGGCSI00 MODRSNRC CSIFIELD DWORK'

   RESUME = SUBSTR(CSIFIELD,150,1)  /* GET RESUME FLAG FOR NEXT LOOP */
   USEDLEN = C2D(SUBSTR(DWORK,9,4)) /* GET AMOUNT OF WORK AREA USED  */
   POS1=15                          /* STARTING POSITION             */

   /******************************************************************/
   /* PROCESS DATA RETURNED IN WORK AREA                             */
   /******************************************************************/
   DO WHILE POS1 < USEDLEN          /* DO UNTIL ALL DATA IS PROCESSED*/

                                    /* IF CATALOG, EXTRACT CATALOG   */
      IF SUBSTR(DWORK,POS1+1,1) = '0' THEN DO
         CATNAME=SUBSTR(DWORK,POS1+2,44)
         POS1 = POS1 + 50
      END
                                     /* IF STILL MORE DATA           */
      IF POS1 < USEDLEN THEN DO      /* CONTINUE WITH NEXT ENTRY     */
         DNAME = SUBSTR(DWORK,POS1+2,44)  /* GET ENTRY NAME          */

         /************************************************************/
         /* ASSIGN ENTRY TYPE NAME                                   */
         /************************************************************/
         SELECT
            WHEN SUBSTR(DWORK,POS1+1,1) = 'C' THEN DTYPE = 'CLUSTER'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'D' THEN DTYPE = 'DATA'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'I' THEN DTYPE = 'INDEX'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'A' THEN DTYPE = 'NONVSAM'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'H' THEN DTYPE = 'GDS'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'B' THEN DTYPE = 'GDG'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'R' THEN DTYPE = 'PATH'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'G' THEN DTYPE = 'AIX'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'X' THEN DTYPE = 'ALIAS'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'U' THEN DTYPE = 'UCAT'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'L' THEN DTYPE = 'ATLLIB'
            WHEN SUBSTR(DWORK,POS1+1,1) = 'W' THEN DTYPE = 'ATLVOL'
            OTHERWISE     /*  NO ENTRIES IN THE CATALOG - LOOK AT    */
               ITERATE    /*  NEXT CATALOG NAME - GENERIC HLQ ONLY   */
         END

         IF DTYPE = 'NONVSAM' THEN DO
            _nr = SUBSTR(DNAME,LENGTH(_ds)+1)
            _nr = _nr + 1
            _nr = RIGHT(_nr,7,"0")
         END

         POS1 = POS1 + 46

         /************************************************************/
         /* GET POSITION OF NEXT ENTRY                               */
         /************************************************************/
         POS1 = POS1 + C2D(SUBSTR(DWORK,POS1,2))
      END                           /* OF ROOM LEFT IN WORKAREA      */
   END

   IF RESUME = 'Y' &,               /* IF WE'VE TRIED THIS ENTRY     */
      PREVNAME = DNAME THEN DO      /* TWICE, WE'VE GOT TO QUIT      */
      SAY STRIP(DNAME) 'CANNOT BE PROCESSED WITH THE WORK AREA SIZE ',
          'PROVIDED - YOU MUST INCREASE THE WORK AREA AND RETRY'
      RETURN
   END

   PREVNAME = DNAME                 /* SAVE FOR NEXT ITERATION       */
END

RETURN _nr
