         MACRO
         MINSTANT &GUID=,&WORK=,&OBJPTR=
         LA    R1,&WORK     * Address execute storage
         LA    R15,&GUID    * Get ptr to GUID
         ST    R15,0(,R1)   * Set ptr to GUID as parm 1
         LA    R15,&OBJPTR  * Get ptr to ptr to return object
         ST    R15,4(,R1)   * Set ptr to ptr to return object as parm 2
.*
         AIF   ('&GUID' NE 'G_ILOG_GUID').CHK_IIND_GUID
         L     R15,=V(LWZMLOG) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IIND_GUID ANOP
         AIF   ('&GUID' NE 'G_IIND_GUID').CHK_IINF_GUID
         L     R15,=V(LWZMINP) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IINF_GUID ANOP
         AIF   ('&GUID' NE 'G_IINF_GUID').CHK_IINS_GUID
         L     R15,=V(LWZMINP) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IINS_GUID ANOP
         AIF   ('&GUID' NE 'G_IINS_GUID').CHK_IIFO_GUID
         L     R15,=V(LWZMINP) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IIFO_GUID ANOP
         AIF   ('&GUID' NE 'G_IIFO_GUID').CHK_IPRS_GUID
         L     R15,=V(LWZMINP) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IPRS_GUID ANOP
         AIF   ('&GUID' NE 'G_IPRS_GUID').CHK_IPSS_GUID
         L     R15,=V(LWZMPRS) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IPSS_GUID ANOP
         AIF   ('&GUID' NE 'G_IPSS_GUID').CHK_ITOK_GUID
         L     R15,=V(LWZMPRS) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_ITOK_GUID ANOP
         AIF   ('&GUID' NE 'G_ITOK_GUID').CHK_ITFO_GUID
         L     R15,=V(LWZMTOK) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_ITFO_GUID ANOP
         AIF   ('&GUID' NE 'G_ITFO_GUID').CHK_IAVL_GUID
         L     R15,=V(LWZMTOK) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IAVL_GUID ANOP
         AIF   ('&GUID' NE 'G_IAVL_GUID').CHK_IAV2_GUID
         L     R15,=V(LWZMAVL) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IAV2_GUID ANOP
         AIF   ('&GUID' NE 'G_IAV2_GUID').CHK_ISTR_GUID
         L     R15,=V(LWZMAVL) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_ISTR_GUID ANOP
         AIF   ('&GUID' NE 'G_ISTR_GUID').CHK_ISTB_GUID
         L     R15,=V(LWZMSTR) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_ISTB_GUID ANOP
         AIF   ('&GUID' NE 'G_ISTB_GUID').CHK_IST1_GUID
         L     R15,=V(LWZMSTR) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST1_GUID ANOP
         AIF   ('&GUID' NE 'G_IST1_GUID').CHK_IST2_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST2_GUID ANOP
         AIF   ('&GUID' NE 'G_IST2_GUID').CHK_IST3_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST3_GUID ANOP
         AIF   ('&GUID' NE 'G_IST3_GUID').CHK_IST4_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST4_GUID ANOP
         AIF   ('&GUID' NE 'G_IST4_GUID').CHK_IST5_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST5_GUID ANOP
         AIF   ('&GUID' NE 'G_IST5_GUID').CHK_IST6_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST6_GUID ANOP
         AIF   ('&GUID' NE 'G_IST6_GUID').CHK_IST7_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST7_GUID ANOP
         AIF   ('&GUID' NE 'G_IST7_GUID').CHK_IST8_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IST8_GUID ANOP
         AIF   ('&GUID' NE 'G_IST8_GUID').CHK_IREX_GUID
         L     R15,=V(LWZMSTM) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IREX_GUID ANOP
         AIF   ('&GUID' NE 'G_IREX_GUID').CHK_IFMG_GUID
         L     R15,=V(LWZMREX) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IFMG_GUID ANOP
         AIF   ('&GUID' NE 'G_IFMG_GUID').CHK_IFFO_GUID
         L     R15,=V(LWZMFMG) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IFFO_GUID ANOP
         AIF   ('&GUID' NE 'G_IFFO_GUID').CHK_IUSS_GUID
         L     R15,=V(LWZMFMG) * Get csect entry point
         AGO   .GO_BRANCH
.*
.CHK_IUSS_GUID ANOP
         AIF   ('&GUID' NE 'G_IUSS_GUID').GO_BRANCH
         L     R15,=V(LWZMUSS) * Get csect entry point
         AGO   .GO_BRANCH
.*
.GO_BRANCH ANOP
.*
         BASR  R14,R15      * Branch to module entry point
         MEND
