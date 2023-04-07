     **************************************************
     ** CALL PGM(AIP998RP) PARM('TF' X'0000017F')    **
     ** CALL PGM(AIP998RP) PARM('FS' X'0002302F')    **
     **************************************************
     ‚* EIXL55 LOGICO CREADO EN BPCDTA
     H datfmt(*iso)  datedit(*ymd)

     ‚* Declara ARCHIVOS DEL INTERFACE
     FRFAVCAB   O    E             DISK
     FRFAVDET   UF A E           K DISK
     FRFAVCLI   UF A E           K DISK
     FRFAVPRD   UF A E           K DISK
     FAIPDIPtmp UF A E           K DISK
     FYAIPLTE   UF A E           K DISK
     FYQVLCNL   IF   E           K DISK
     FyEIXL55   IF   E           K DISK                                         logico de bpcdta
     FETQALBARANUF A E           K DISK
      *--
     ‚* ARCHIVOS AGREGADOS POR CARLOS MESTA
     Fyiiml77   if   e           k disk
     Fiicl01    if   e           k disk
      *--
     D* Estructura de datos del sistema
     D pgstat         sds
     D  progst           *status
     D  iopgm                  1     10
     D  user                 254    263


     ‚* Declara variables PARAMETROS
     D vCodErr$        S              7A

     ‚* Declara variables HOST
     D N               S              2S 0 inz
     D vTipPed         S              1A   inz
     D vClsPed         S              3P 0 inz
     D vTotBko         S              3P 0 inz
     D vCodAlm         S              3A
     D
     D vHSHPR          S             30A
     D vCia            S              2S 0 inz
     D vHcpo           S             23A   inz
     D vHwhse          S              3A   inz
     D vNroPed         S              8P 0 inz
     D vCodClt         S              8P 0
     D vHsal           S              6P 0
     D vCodCltA        S             12A
     D vCodCltA30      S             30A
     D vPtoEnv         S              4S 0
     D vPtoEnvA        S              3A
     D vDirEnv1        S             50A
     D vDirEnv2        S             50A
     D vDirEnv3        S             50A
     D vALoc           S             10A
     D vAlot           S             25A
     D vAlot10         S             10A
     D vCodItm         S             35A
     D vCodItm18       S             18A
     D vLine           S              4S 0
     D vCanItm         S             11P 3
     D
     D vNomClt         S             50A
     D vDirClt1        S             50A
     D vDirClt2        S             50A
     D vCodPai         S              2A
     D vCodSta         S              3A
     D vCodPos         S              9A
     D vCodEst         S              2A
     D vHSDTE          S              8S 0
     D vHRDTE          S              8S 0
     D vCHSSTM         S              6S 0
     D vCPHON          S             25A
     D vCMFTXC         S             16A
     D vCTYPE          S              4A
     D vTRANSAC        S              5A   inz(' ')
     D vCTR            S              2A
     D
     D vDesItm         S             50A
     D vLTELOT         S              2S 0
     D
     D vNroPedBase     S              8P 0 inz( -1 )
     D vNroPedFac      S             15A   inz
     D vFac            S              8P 0
     D vFacS           S              8A   inz

      *--------------------------------------------------
      *-- Fin Grupo de variables definidas x Carlos Mesta
      *--------------------------------------------------
     d wetqcla         s              2a   inz
     d wetqclad        s             25a   inz
     d wetqsku         s             18a   inz
     d wetqbul         s              8s 0 inz
     d wetqcan         s              8s 0 inz
     d wetqpro         s             15a   inz
      *--------------------------------------------------
      *-- Fin Grupo de variables definidas x Carlos Mesta
      *--------------------------------------------------
     ‚* Variables de Control
     D vFlg            S              1A   inz( *off )

     ‚* Recibe parametros
     C     *entry        plist
     C                   parm                    pAlm              3
     C                   parm                    pPrc              6 0

     c     kPrd          klist
     c                   kfld                    CABCIA
     c                   kfld                    CABFPR
     c                   kfld                    CABLOE
     c                   kfld                    vCodItm

     c     kDet          klist
     c                   kfld                    CABCIA
     c                   kfld                    CABFPR
     c                   kfld                    CABLOE
     c                   kfld                    CABCTR
     c                   kfld                    CABNRO
     c                   kfld                    vCodItm

     c     kCli          klist
     c                   kfld                    CABCIA
     c                   kfld                    CABFPR
     c                   kfld                    CABLOE
     c                   kfld                    vCodCltA30

     c     kYAIPLTE      klist
     c                   kfld                    CABCIA
     c                   kfld                    CABFPR

     c     kETQALB       klist
     c                   kfld                    vHsdte
     c                   kfld                    nLoe              2 0
     c                   kfld                    vCodItm2         15
     c                   kfld                    CABCTR
     c                   kfld                    CABNRO

      *- Definicion de Clave x Carlos Mesta
      *--
     c     kEixl55       klist
     c                   kfld                    vcoditm
     c                   kfld                    vcodclt
      *- Definición de Clave x Carlos Mesta
     c     kAipdip       klist
     c                   kfld                    vDipcla           2
     c                   kfld                    CABCTR
     c                   kfld                    CABNRO
     c                   kfld                    vCodItm
     c                   kfld                    CABALM
     c                   kfld                    vAloc
     c                   kfld                    vAlot
     c                   move      'P1'          vDipcla

     ‚* Obtiene datos del Usuario para el Proceso
     C                   eval      vCodErr$ = 'ORD0000'

     ‚* Asigna informacion en YELA
     C/EXEC SQL
     C+ DELETE FROM YELA
     C/END-EXEC

     C/EXEC SQL
     C+ INSERT INTO YELA
     C+ SELECT 'LA','C', PED , LIN  , SEQ ,
     C+ PRD  , ALM , LOC ,  LOT,QTY  , 0,
     C+ ' ', 0, 0 FROM
     C+ xcomela A
     C+ WHERE
     C+ A.ALM=:pAlm and A.PRC=:pPrc
     C/END-EXEC

     ‚* Prepara select totalizado del ELA y ECH
     C/EXEC SQL
     C+ DELETE FROM aipdta/RFAVCAB
     C/END-EXEC
     c
     C/EXEC SQL
     C+ DELETE FROM aipdta/RFAVDET
     C/END-EXEC
     c
     C/EXEC SQL
     C+ DELETE FROM aipdta/RFAVCLI
     C/END-EXEC
     c
     C/EXEC SQL
     C+ DELETE FROM bpcdta/AIPDIPtmp
     C/END-EXEC
     c
     C/EXEC SQL
     C+ DELETE FROM aipdta/RFAVPRD
     C/END-EXEC
     c
     C/EXEC SQL
     C+ DECLARE DYT CURSOR FOR
     C+     SELECT HCOMP, HCPO, AORD, HWHSE, HCUST, HSHIP, HSAL,
     C+            HAD1, HAD2, HAD3, HRDTE, HSDTE, CHSSTM,
     C+            APROD, ALOT, ALOC, ALINE,
     C+            decimal(sum(LQALL),11,3)
     C+         FROM
     C+                      YELA,         ECH
     C+         WHERE
     C+               AORD=HORD
     C+         GROUP BY
     C+            HCOMP, HCPO, AORD, HWHSE, HCUST, HSHIP, HSAL,
     C+            HAD1, HAD2, HAD3, HRDTE, HSDTE, CHSSTM,
     C+            APROD, ALOT, ALOC, ALINE
     C+         ORDER BY
     C+            HCOMP, HCPO, AORD, HWHSE, HCUST, HSHIP, HSAL,
     C+            HAD1, HAD2, HAD3, APROD, ALOT, ALOC, ALINE
     C/END-EXEC

     ‚* Empieza proceso
     C/EXEC SQL OPEN DYT
     C/END-EXEC
     C                   if        SQLCOD<>0
     C                   eval      vCodErr$ = 'ORD0003'
     C                   goto      ePgmEnd
     C                   endif

     c                   setoff                                       88
     C                   clear                   RFAVCABR
     C/EXEC SQL FETCH NEXT FROM DYT INTO
     C+  :vCia, :vHcpo, :vNroPed, :vHwhse,
     C+  :vCodClt, :vPtoEnv,  :vHsal,
     C+  :vDirEnv1, :vDirEnv2, :vDirEnv3,
     C+  :vHRDTE, :vHSDTE, :vCHSSTM,
     C+  :vCodItm, :vAlot, :vAloc, :vLine, :vCanItm
     C/END-EXEC
     C                   dow       SQLCOD = 0
     C                   eval      vCodItm18=vCodItm
     C* GRABA RFAVCAB
     C                   if        vNroPed <> vNroPedBase
     C                   if        vFlg = *on
     C                   exsr      sGraCab
     C                   endif
     C                   clear                   RFAVCABR
     c* CABCIA
     c                   select
     c                   when      vCia=1
     C                   eval      CABCIA = 'FAV'
     c                   when      vCia=2
     C                   eval      CABCIA = 'LAR'
     c                   when      vCia=3
     C                   eval      CABCIA = 'GLA'
     c                   when      vCia=7
     C                   eval      CABCIA = 'YCH'
     c                   when      vCia=8
     C                   eval      CABCIA = 'CTC'
     c                   endsl
     c* CABFPR
     c     *iso          movel     *date         CABFPR
     c* CABLOE
     c* Solo una vez
     c                   if                      *in88=*off
     c                   seton                                        88
     c     kYAIPLTE      chain     YAIPLTER
     c                   if        %found
     c                   eval      LTELOT=LTELOT +1
     c                   UPDATE    YAIPLTER
     c                   else
     c                   eval      LTECIA=CABCIA
     c                   eval      LTEFPR=CABFPR
     c                   eval      LTELOT= 1
     c                   WRITE     YAIPLTER
     c                   endif
     c                   eval      vLTELOT=LTELOT
     c                   endif
     c*
     c                   z-add     vLTELOT       CABLOE
     c                   z-add     0             CABSCR
     C                   movel     '1M'          CABCTR
      *
     c                   exsr      sInfCtr
      *
     C                   eval      vNroPedBase = vNroPed
     C                   exsr      sNroPedFac
     C                   movel(p)  vCodClt       vCodCltA
     C                   move      vPtoEnv       vPtoEnvA
     C                   eval      vCodCltA = 'D'  +  %trim(vCodCltA) + vPtoEnvA
     C                   movel(p)  vCodCltA      vCodCltA30
     C                   movel     vCodCltA      CABCCL
     C                   z-add     vHRDTE        CABFCH
     c                   z-add     0             CABTIT
     c                   z-add     0             CABTUN
     C                   z-add     vHSDTE        CABFEE
     C                   movel     vCHSSTM       CABHOE
     C                   z-add     0             CABFPC
     C                   movel     *blanks       CABHOP
     C                   movel     vHCPO         CABNOC
     C                   movel     vHWHSE        CABALM
     C                   movel     *zero         CABCHK
     C                   movel     vHSAL         CABCVD
     C                   movel     *blanks       CABDPK
     C                   movel     *blanks       CABFR1
     C                   movel     *blanks       CABFR2
     C                   movel     *blanks       CABF01
     C                   movel     *blanks       CABA01
     C                   movel     *blanks       CABN01

     C* GRABA RFAVCLI
     C                   exsr      sRFAVCLI
     C                   eval      vFlg = *on
     C                   MOVEL     VLINE         XLINE             3 0
     C                   MOVEL     VNROPED       XNROPED           6 0
     C                   MOVEL     VCODITM       XCODITM          15
     C                   endif
     C
     c     kDet          chain     RFAVDETR
     c                   if        not %found
     C                   add       1             CABTIT
     C                   add       vCanItm       CABTUN
     C                   eval      DETCIA = CABCIA
     C                   eval      DETFPR = CABFPR
     C                   eval      DETLOE = CABLOE
     C                   eval      DETSCR = 0
     C                   eval      DETCTR = CABCTR
     C                   eval      DETNRO = CABNRO
     C                   eval      DETCPR = vCodItm
     C                   eval      DETCP2 = vCodItm
     C                   eval      DETCTD = vCanItm
     C                   eval      DETCPK = 0
     C                   eval      DETLOT = *blanks
     C                   eval      DETALM = *blanks
     C                   eval      DETFDF = *blanks
     C                   eval      DETF01 = *blanks
     C                   eval      DETA01 = *blanks
     C                   eval      DETN01 = 0
     C                   write     RFAVDETR
     c                   else
     C                   eval      DETCTD = DETCTD + vCanItm
     C                   add       vCanItm       CABTUN
     C                   update    RFAVDETR
     c                   endif
     C                   exsr      sRFAVPRD
     c*
     c                   if        vHSHPR<>*blanks
     C                   exsr      sETQALBARAN
     c                   endif
     C*
     c                   movel     vAlot         vAlot10
     c* AIPDIP ...
     c     kAipdip       chain     Aipdipr
     c                   If        not %found
     C                   clear                   AIPDIPR
     C                   eval      DIPCLA = 'P1'
     C                   eval      DIPFPR = CABFPR
     C                   eval      DIPLOE = CABLOE
     C                   eval      DIPCTR = CABCTR
     C                   eval      DIPNRO = CABNRO
     C                   eval      DIPPRD = vCodItm
     C                   eval      DIPCTD = vCanItm
     C                   eval      DIPALM = CABALM
     C                   eval      DIPUBI = vAloc
     C                   eval      DIPLOT = vAlot
     C                   eval      DIPCOM = 'Transfer'
     C                   eval      DIPADV = *blanks
     C                   eval      DIPAGP = *blanks
     C                   eval      DIPLPR = *blanks
     C                   eval      DIPOBS = *blanks
     C                   eval      DIPTIP = '1'
     C                   eval      DIPF01 = *blanks
     C                   eval      DIPF02 = *blanks
     C                   eval      DIPA01 = *blanks
     C                   eval      DIPA02 = *blanks
     C                   eval      DIPN01 = *zeros
     C                   eval      DIPN02 = *zeros
     c                   MOVE      UDATE         DIPFEC
     c                   TIME                    DIPHOR
     C                   eval      DIPUSR = USER
     C                   write     AIPDIPR
     c                   else
     C                   eval      DIPCTD = DIPCTD + vCanItm
     C                   update    AIPDIPR
     c                   Endif
     C
     C/EXEC SQL FETCH NEXT FROM DYT INTO
     C+  :vCia, :vHcpo, :vNroPed, :vHwhse,
     C+  :vCodClt, :vPtoEnv,  :vHsal,
     C+  :vDirEnv1, :vDirEnv2, :vDirEnv3,
     C+  :vHRDTE, :vHSDTE, :vCHSSTM,
     C+  :vCodItm, :vAlot, :vAloc, :vLine, :vCanItm
     C/END-EXEC
     C                   enddo
     C                   if        vFlg = *on
     C                   write     RFAVCABR
     C                   exsr      sRFAVCLI
     C                   endif

     ‚* Cierra cursor de join ELA, ECH
     C/EXEC SQL
     C+ CLOSE   DYT
     C/END-EXEC

     ‚* Finaliza programa
     C     ePgmEnd       tag
     C                   eval      *inlr = *on
     C                   return



     ‚* Obtiene datos del Pedido
     C     sGraCab       begsr
     C                   write     RFAVCABR
     C                   endsr


     C     sETQALBARAN   begsr
      *--------------------------------------------
      *-  Lineas Adicionales por Carlos Mesta
      *--------------------------------------------
     c                   eval      vCodItm2=vCodItm
     c     kETQALB       chain     ETQALBAR
     c                   if        not %found
     c                   movel     *blanks       wetqsku
     C     kEixl55       chain     IPE150IX
     c                   if        %found
     c                   if        ixitem<>' '
     c                   movel     ixitem        wetqsku
     c                   endif
     c                   endif
      *--------------------------------------------
      *-  Fin Lineas Adicionales por Carlos Mesta
      *--------------------------------------------
     C                   CLEAR                   ETQALBAR
     C                   EVAL      ETQFEC = vHsdte
     C                   EVAL      ETQLOT = 1
     C                   EVAL      ETQOC  = vHCPO
     C                   EVAL      ETQALB = vHSHPR
     c                   EVAL      ETQCTR = CABCTR
     c                   EVAL      ETQNRO = CABNRO
      *--------------------------------------
      *- Datos agregados por Carlos Mesta
      *--------------------------------------
     c                   eval      etqsku  = wetqsku
     c                   eval      etqbul  = *zeros
     c                   eval      etqcan  = vcanitm
     c                   eval      etqpro  = vcoditm
     c                   move      *blanks       wetqcla
     c                   move      *blanks       wetqclad
     c     vcoditm       chain     IPI100IM
      *    etqpro        chain     IPI100IM
     c                   if        %found
     c     iclas         chain     IPI160IC
     c                   if        %found
     c                   movel     iclas         wetqcla
     c                   movel     ICDES         wetqclad
     c                   endif
     c                   endif
     c                   eval      etqcla  = wetqcla
     c                   eval      etqclad = wetqclad
     c                   time                    etqhora
      *--------------------------------------
      *- Fin Datos agregados por Carlos Mesta
      *--------------------------------------
     c                   EVAL      ETQUSE   = 'XCOMDYTR'
     c                   MOVEL     *DATE         ETQFECHA
     C                   write     ETQALBAR
     c
     c                   else
     c
     c                   add       vcanitm       etqcan
     C                   update    ETQALBAR
     c
     c                   endif
     C                   endsr



     ‚* Obtiene datos del Pedido
     C     sNroPedFac    begsr
      *                  movel(p)  vNroPed       CABNRO
     C                   eval      CABNRO=%char(vNroPed)
     C/EXEC SQL
     C+ SELECT SIINVN INTO :vFac FROM SIH WHERE SIORD=:vNroPedBase and
     C+ SIINVD>20000000 ORDER BY SIINVN DESC
     C/END-EXEC
     C                   movel(p)  vFac          vFacS
     C                   move      pPrc          wPrc              6
     C                   eval      CABNRO = %trim(CABNRO)+
     c                                      '-'+wPrc
      *                                     '-'+pAlm+wPrc
     C                   endsr



     ‚* Obtiene datos del Cliente
     C     sInfClt       begsr
     C/EXEC SQL
     C+ SELECT CNME, CAD1, CAD2, CSTE, CZIP, CDISC, CPHON, CMFTXC INTO
     C+ :vNomClt, :vDirClt1,:vDirClt2, :vCodSta, :vCodPos, :vCodEst,
     C+ :vCPHON, :vCMFTXC FROM RCM WHERE CMID='CM' AND CCUST=:vCodClt
     C/END-EXEC
     c                   if        vPtoEnv <> 0
     C/EXEC SQL
     C+ SELECT TADR1, TADR2, TSTE, TPOST INTO :vDirClt1, :vDirClt2,
     C+ :vCodSta, :vCodPos FROM EST WHERE TID='ST' and TCUST=:vCodClt and
     C+ TSHIP=:vPtoEnv
     C/END-EXEC
     c                   endif
     C                   endsr

     ‚* Obtiene Transacción
     C     sInfCtr       begsr
     C                   movel     *blanks       CABCTR
     C                   movel     *blanks       VCTR
     C/EXEC SQL
     C+ SELECT CTYPE INTO :vCTYPE
     C+ FROM RCM WHERE CMID='CM' AND CCUST=:vCodClt
     C/END-EXEC
     C                   EVAL      vTRANSAC='D'+vCTYPE
     C/EXEC SQL
     c+ SELECT substr(char(ccnot1),1,2) INTO :VCTR FROM fvbpcsf/zcc WHERE
     c+ cctabl='YQVLCTR' and cccode=:vTRANSAC
     C/END-EXEC
      *-
     c                   If        vCtr=*blanks
     c                   Eval      vCtr='1M'
     c                   Endif
      *-
     C                   MOVEl     VCTR          CABCTR
      *-
     C                   endsr

     ‚* Obtiene datos del Articulo
     C     sIIM          begsr
     C/EXEC SQL
     C+ SELECT IDESC INTO :vDesItm FROM IIM WHERE IID='IM' and
     C+ IPROD=:vCodItm
     C/END-EXEC
     C                   endsr

     ‚* genera RFAVCLI
     C     sRFAVCLI      begsr
     C                   exsr      sInfClt
     C     kCli          chain(n)  RFAVCLIR                           99
     C                   if        *in99 = *on
     C                   eval      CLICIA = CABCIA
     C                   eval      CLIFPR = CABFPR
     C                   eval      CLILOE = CABLOE
     C                   eval      CLICCL = CABCCL
     C                   eval      CLITTM = *BLANKS
     C                   eval      CLIAPP = %SUBST(vNomClt:1:25)
     C                   eval      CLIAPM = %SUBST(vNomClt:26:5)
     C                   eval      CLIAPC = *BLANKS
     C                   eval      CLINB1 = *BLANKS
     C                   eval      CLINB2 = *BLANKS
     C                   eval      CLITVI = *BLANKS
     C                   eval      CLINVI = %trim(vDirEnv1)+' '+%trim(vDirEnv2)
     C                   eval      CLINRP = *BLANKS
     C                   eval      CLINRI = *BLANKS
     C                   eval      CLIMZN = *BLANKS
     C                   eval      CLILOT = *BLANKS
     C                   eval      CLIKMT = *BLANKS
     C                   eval      CLIBLQ = *BLANKS
     C                   eval      CLIREF = *BLANKS
     C                   eval      CLITLF = vCPHON
     C                   eval      CLIUBG = %subst( vCodPos:1:6 )
     C                   eval      CLIRUC = vCMFTXC
     C                   eval      CLICEL = *BLANKS
     C                   eval      CLINED = *BLANKS
     C                   eval      CLIDED = *BLANKS
     C                   eval      CLIUEM = %subst( vCodPos:1:6 )
     C                   eval      CLITIP = *BLANKS
     C                   eval      CLIF01 = *BLANKS
     C                   eval      CLIA01 = *BLANKS
     C                   eval      CLIN01 = *ZERO
     c**************************************************
     C*                  eval      CLIZON = vCodPai+vCodSta
     C*                  eval      CLISZN = %subst( vCodPos:5:2 )
     C     vCodEst       chain     YQVLCNLR                           70
     c                   if        *in70='0'
     c                   eval      CLITIP= YCLIES
     c                   else
     c                   eval      CLITIP= *blanks
     c                   endif
     c**************************************************

     C                   write     RFAVCLIR
     C                   endif
     C                   endsr

     ‚* genera RFAVPRD
     C     sRFAVPRD      begsr
     C     kPRD          chain     RFAVPRDR                           99
     C                   if        *in99 = *off
     C                   eval      PRDTUN = PRDTUN+vCanItm
     C                   eval      PRDTIT = PRDTIT+vCanItm
     C                   update    RFAVPRDR
     C                   else
     C                   exsr      sIIM
     C                   eval      PRDCIA = CABCIA
     C                   eval      PRDFPR = CABFPR
     C                   eval      PRDLOE = CABLOE
     C                   eval      PRDCPR = vCodItm
     C                   eval      PRDDPR = vDesItm
     C                   eval      PRDTUN = vCanItm
     C                   eval      PRDTIT = vCanItm
     C                   eval      PRDE14 = *blanks
     C                   eval      PRDUXE = *zeros
     C                   eval      PRDE13 = *blanks
     C                   eval      PRDVOL = 0
     C                   eval      PRDLRG = 0
     C                   eval      PRDANC = 0
     C                   eval      PRDALT = 0
     C                   eval      PRDPES = 0
     C                   eval      PRDCP1 = *blanks
     C                   eval      PRDCP2 = *blanks
     C                   eval      PRDCP3 = *blanks
     C                   eval      PRDF01 = *blanks
     C                   eval      PRDA01 = *blanks
     C                   eval      PRDN01 = *zeros
     C                   write     RFAVPRDR
     C                   endif
     C                   endsr
