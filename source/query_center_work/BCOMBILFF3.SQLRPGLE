      * --------------------------------------------------------------------------------------------
      * Ejecucion: CALL PGM(BPCPGM/XCOMBILFF3) PARM(X'003F' X'000009692F' 'FA' '004'
      *                 X'000015559F' X'0F' '  ' '   ' X'000000000F')
      * --------------------------------------------------------------------------------------------
      * Modifi.  : Se modifica logica de calculo de anticipos - R001796                            *
      * Fecha    : 15 de octubre del 2013                                                          *
     ‚*--------------------------------------------------------------------------------------------*
      *
      * Coloca formatos implicitos
     h datfmt( *iso ) datedit( *ymd ) alwnull(*usrctl)

      * Declara Formatos de Factura1
     fFA_GLA_AD O    E             PRINTER
      * ----------------------------------------------------------
     FYsilf01   if   e           k Disk    Rename(IPS100IL:silf01r)
      * ----------------------------------------------------------
     fxanticipo uf a e           k disk
     fxanticsld1uf   e           k disk

      * Declara Constantes

      * Declara variables de entrada
     d cCia            s              2p 0 inz( 3 )
     d cTip            s              1p 0 inz( 1 )
     d cPre            s              2a   inz( 'AD' )
     d cLpP            s              2p 0 inz( 41 )                            Lineas por Pagina
     d $RecDat       E DS                  EXTNAME( esnl01 )
      *
     d MTODETRAC       s             10p 2 inz(700.00)
     d MTODETRAC4      s             10p 2 inz(400.00)

      * Variables Host

      * Declara variables de entrada
     d pAno            s              2p 0
     d pNum            s              8p 0
     d pTipS           s              2a
     d pSerS           s              3a
     d pNumS           s              8p 0
     d pErr            s              1p 0

      * Host de Control
     d hFlg            s              1a   inz
     d hVia            s              4a   inz
     d hNt1            s             50a   inz
     d hNt2            s             50a   inz
     d hNt3            s             50a   inz
     d hNt4            s             50a   inz
     d hNt5            s             50a   inz
     d hNt6            s             50a   inz
     d hNt1F           s              2b 0
     d hNt2F           s              2b 0
     d hNt3F           s              2b 0
     d hNt4F           s              2b 0
     d hNt5F           s              2b 0
     d hNt6F           s              2b 0
     d hNt1D           s             20a   inz
     d hNt2D           s             20a   inz
     d hNt3D           s             20a   inz
     d hNt4D           s             20a   inz
     d hNt5D           s             20a   inz
     d hNt6D           s             20a   inz
     d hNt1T           s              1a   inz
     d hNt2T           s              1a   inz
     d hNt3T           s              1a   inz
     d hNt4T           s              1a   inz
     d hNt5T           s              1a   inz
     d hNt6T           s              1a   inz

      * Host de Cabecera
     d hClt            s              8p 0 inz
     d hCltD           s             50a   inz
     d hCltD30         s             30a   inz
     d hDir1           s             50a   inz
     d hDir2           s             50a   inz
     d hDir3           s             50a   inz
     d hPai            s              4a   inz
     d hCiu            s              3a   inz
     d hPos            s              9a   inz
     d hPosD           s             30a   inz
     d wPosD           s             30a   inz
     d hRuc            s             16a   inz
     d hMPa            s              1a   inz
     d hMPaD           s             12a   inz
     d hMon            s              3a   inz
     d hCPa            s              2a   inz
     d hCPaD           s             15a   inz
     d hCPaT           s              5p 0 inz
     d hCPaDFT         s              5p 0 inz
     d hCPaDFP         s              5p 2 inz
     d hFFa            s              8p 0 inz
     d hFVe            s              8p 0 inz
     d hVnd            s              6p 0 inz
     d hVndD           s             30a   inz
     d hDirE1          s             50a   inz
     d hDirE2          s             50a   inz
     d hDirE3          s             50a   inz
     d hPaiE           s              4a   inz
     d hCiuE           s              3a   inz
     d hPosE           s              9a   inz
     d hPosED          s             30a   inz
     d hTCam           s             15p 7 inz
     d xFec            s              8p 0 inz
     d wseq            s              4p 0 inz
     d wGuia           s             50a   inz

      * Host de Detalle
     d hLin            s              4p 0 inz
     d hPrd            s             35a   inz
     d hPrdD           s             50a   inz
     d hPrdA           s             35a   inz
     d hPrdAD          s             50a   inz
     d hPrdAF          s              1p 0 inz
     d hAra            s             20a   inz
     d hCls            s              2a   inz
     d hCan            s             11p 3 inz
     d hUMd            s              2a   inz
     d hPBa            s             14p 4 inz
     d hPFa            s             14p 4 inz
     d hPNe            s             14p 4 inz
     d hImp            s              5a   inz
     d hImpM           s             17p 4 inz
     d hPed            s              8p 0 inz
     d hSeq            s              4p 0 inz
     d hOC             s             15a   inz
     d hRev            s             15p 2 inz

      * Host de Calculo automatico
     d hCanT           s              9p 3 inz
     d hVV             s             11p 3 inz
     d hVVT            s             11p 3 inz
     d hVD             s             11p 3 inz
     d hPD             s              7p 2 inz
     d hVDT            s             11p 3 inz
     d hPV             s             11p 3 inz
     d hPVT            s             11p 3 inz
     d hImpT           s             17p 4 inz
     d hBImp           s             11p 3 inz
     d hmex            s             11p 3 inz
     d hOtro           s             11p 3 inz
     d hsre            s              5a   inz

     d hFacTot         s             15p 2 inz
     d hFacTax         s             15p 2 inz
     d hPrcAlm         s              2a   inz
     d hPrcNum         s              8p 0 inz

      * Host de Calculo automatico
     d hnom1           s             35a   inz
     d hruc1           s             30a   inz
     d cDire1          c                   const('CALLE SAN GENARO N. 150')
     d cDire2          c                   const('LOS OLIVOS    LIMA')
     d cDire3          c                   const('           LIMA')
     d cDire4          c                   const('URB.MOLITALIA')

     d hrdte1          s              8s 0
     d chsstm1         s              6s 0

      * Variables de Trabajo
     d wPagAct         s              3p 0 inz(0)
     d wLinAct         s              3p 0 inz(0)
     d wAra            s             20a   inz(*all'*')
     d wFlgOC          s              1a   inz(*off)
     d wInTerm1        s              3a   dim(3) CTDATA PERRCD(1)
     d wInTerm2        s              3a   dim(3) CTDATA PERRCD(1)
     d wFlgP1          s              1a   inz( *on )
     d wImpT           s             15p 2 inz
     d hOCXLIN         s             15a   inz( ' ' )
     d hPosi           s              4s 0 inz( 0 )
     d xVDT            s             11p 3 inz

      * Nuevas Variables
     d Vletras         s            190a
     d Vletra1         s            100a
     d Vletra2         s             90a

      * Variables del Sistema
     d                sds
     d  $UsrNam              254    263A

      * Recibe parametros
     c     *entry        plist
     c                   parm                    pAno
     c                   parm                    pNum
     c                   parm                    pTipS
     c                   parm                    pSerS
     c                   parm                    pNumS
     c                   parm                    pErr
mcs  c                   parm                    pTipG             2
 |   c                   parm                    pSerG             3
 |   c                   parm                    pnGui             8 0
      *
     c     k_sih         klist
     c                   kfld                    cCia
     c                   kfld                    cTip
     c                   kfld                    cPre
     c                   kfld                    pAno
     c                   kfld                    pNum
      *
     c     KAnticipo     klist
     c                   kfld                    cCia
     c                   kfld                    cTip
     c                   kfld                    cPre
     c                   kfld                    pAno
     c                   kfld                    pNum
      *
     c     KAnticsldo    klist
     c                   kfld                    hClt
     c                   kfld                    hMon
     c                   kfld                    Secuencia         4 0
      *
     c     KAnticsldo2   klist
     c                   kfld                    hClt
     c                   kfld                    hMon
      *
     c                   eval      pErr = 0
     c                   eval      hNt2='** KK  **'
      * Lee registro de factura
     c                   if        pAno=-1
     c                   exsr      sStrPgm
     c                   else
     C/EXEC SQL
     C+ SELECT FLG, VIA, NT1, NT2, NT3, NT4, NT5, NT6 INTO :hFlg, :hVia,
     C+ :hNt1:hNt1F, :hNt2:hNt2F, :hNt3:hNt3F, :hNt4:hNt4F, :hNt5:hNt5F,
     C+ :hNt6:hNt6F FROM XCOMBIL1 WHERE CIA=:cCia and TIP=:cTip and
     C+ PRE=:cPre and ANO=:pAno and NUM=:pNum
     C/END-EXEC
     c                   if        SQLCOD=0
     c                   exsr      sStrPgm
     c                   else
     c                   eval      pErr = -6
     c                   endif

     c                   if        pErr = 0
      * Actualiza como ya impreso
     C/EXEC SQL
     C+ UPDATE XCOMBIL1 SET FLG = '*' WHERE CIA=:cCia and
     C+ TIP=:cTip and PRE=:cPre and ANO=:pAno and NUM=:pNum
     C/END-EXEC
     c                   endif
     c                   endif

     c                   eval      *inlr = *on
     c                   return



      * Arranca programa de impresion
     c     sStrPgm       begsr

      * Accesa a cabecera de Documento
     C/EXEC SQL
     C+ SELECT CLTCOD, CLTDES, CLTDIR1, CLTDIR2, CLTDIR3, CLTRUC, CLTPAI,
     C+ CLTCIU, CLTPOS, CLTPOSD, DOCMON, DOCMPAC, DOCMPAD, DOCCPAC,
     C+ DOCCPAD, DOCCPAT, DOCCPADFT, DOCCPADFP, DOCFFA, DOCFVE, VENCOD,
     C+ VENDES, CLTPAIE, CLTCIUE, CLTPOSE, CLTPOSED, CLTDIRD1, CLTDIRD2,
     C+ CLTDIRD3, DOCTCA, FACTOT, FACTAX INTO :hClt, :hCltD, :hDir1,
     C+ :hDir2, :hDir3, :hRuc, :hPai, :hCiu, :hPos, :hPosD, :hMon, :hMPa,
     C+ :hMPaD, :hCPa, :hCPaD, :hCPaT, :hCPaDFT, :hCPaDFP, :hFFa, :hFVe,
     C+ :hVnd, :hVndD, :hPaiE, :hCiuE, :hPosE, :hPosED, :hDirE1, :hDirE2,
     C+ :hDirE3, :hTCam, :hFacTot, :hFacTax FROM bpcdta/XCOMFAC
     C/END-EXEC
     c                   if        SQLCOD<>0
     c                   eval      pErr = -7
     c                   goto      eStrPgm
     c                   endif
      *
      * -----------------------------------------------------------------------
      * Deteminar campos adicionales para el registro de ventas - PARCIAL
      * -----------------------------------------------------------------------
     c                   movel     *zeros        hCabCns           6 0
     c                   movel     *zeros        hCabCls           3 0
     c                   movel     ' '           hCabIde           1
     c                   movel     '  '          hCabAlm           2
     c     k_sih         chain     silf01r
     c                   if        %found
     c                   movel     ILCONS        hCabCns
     c                   movel     ILOCLS        hCabCls
     c                   movel     ILWHS         hCabAlm
     c                   movel     'S'           hCabIde
     c                   endif
      *
     c                   z-add     *zeros        wLinAct                        Linea Actual
     c                   z-add     1             wPagAct                        Pagina Actual
      * Accesa a cabecera de Documento
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT DOCLIN, PRDCOD, PRDDES, PRDCODA,
     C+ PRDDESA, PRDFLGA, PRDARA, PRDCLS, PRDCAN, PRDUND, PRDPBA, PRDPFA,
     C+ PRDPNE, PRDIMP, PRDIMPM, ORDCOD, ORDSEQ, ORDCOM, LINOCC, OCXLIN
     C+ FROM bpcdta/XCOMLIN ORDER BY OCXLIN, LINOCC
     C/END-EXEC

     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC

     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO
     C+ :hLin, :hPrd, :hPrdD, :hPrdA, :hPrdAD, :hPrdAF,
     C+ :hAra, :hCls, :hCan, :hUMD, :hPBa, :hPFa,
     C+ :hPNe, :hImp, :hImpM, :hPed, :hSeq, :hOC, :hPosi, :hOCXLIN
     C/END-EXEC
      *
      * Marcar el proceso para Anticipos
      *
     c                   movel     *off          *in99
     c                   if        %subst(hoc:1:4)='@AAA'                       Aplic.Autom.Anticipo
     c                   movel     *blanks       hoc
     c                   movel     *on           *in99
     c                   endif
      *
     c*                  exsr      sPrtCab
     c                   dow       sqlcod=0
     c
      * ***** Imprime Detalle de Anticipos********************************

     c                   MOVEL     *ZEROS        ANTICIPO@        13 2

     c                   MOVEL     *BLANKS       NROEXTFAC1       20
     c                   MOVEL     *ZEROS        ANTICIPO$1       13 2
     c                   MOVEL     *BLANKS       NROEXTFAC2       20
     c                   MOVEL     *ZEROS        ANTICIPO$2       13 2
     c                   MOVEL     *BLANKS       NROEXTFAC3       20
     c                   MOVEL     *ZEROS        ANTICIPO$3       13 2
     c/EXEC SQL
     c+ SELECT  NROEXTFAC1,  ANTICIPO$1  ,
     c+         NROEXTFAC2,  ANTICIPO$2  ,
     c+         NROEXTFAC3,  ANTICIPO$3    INTO
     c+        :NROEXTFAC1, :ANTICIPO$1  ,
     c+        :NROEXTFAC2, :ANTICIPO$2  ,
     c+        :NROEXTFAC3, :ANTICIPO$3
     c+ FROM XANTICIPO WHERE
     c+ CIA=:cCia and TIP=:cTip and PRE=:cPre and ANO=:pAno and NUM=:pNum
     c/END-EXEC
      * ***** Imprime Detalle de Anticipos********************************

     c                   if        wLinAct>cLpP or wFlgP1=*on
     c******************
     c                   eval      ANTICIPO@=ANTICIPO$1+ANTICIPO$2+ANTICIPO$3

     c                   eval      ANTICIPO@ = ANTICIPO@ * -1
     c                   if        ANTICIPO@<>0
     c                   eval      hVVT=hVVT+ANTICIPO@
     c                   eval      wVTDT=%dec(hVVT-hVDT:11:2)
     c                   eval      hImpT=%dec( (hVVT-hVDT)*0.18 :15 :2)
     c                   endif
     c******************

     c                   if        wFlgP1=*off
     c                   eval      hBImp = hVVT-hVDT
     c                   eval      hPVT = hBImp+hImpT
     c                   eval      hCltD30 = %subst(hCltD:1:30)
     c
     c                   eval      wguia=*blanks
     c
     C                   if        (cTip<>1) or
     C                             (cCia=2 and cTip=1 and cPre='LQ')
     c*     Recupera Nro de Factura para N/C desde la 3ra linea de Notas de Pedido ...
     C/EXEC SQL
     C+ SELECT snseq,sndesc
     C+ INTO :wseq,:wguia FROM FVBPCSF/ESNL01 WHERE
     C+ sntype='O' AND SNINV='Y' and sncust=:hped ORDER BY snseq desc
     C/END-EXEC
     c                   endif
      *
     C                   SELECT
     c                   WHEN      ( ccia=2 and cpre='LQ' )
     c                   Z-ADD     hBImp         hMex
     c                   z-add     *ZEROS        hOtro
     c                   z-add     *ZEROS        hBimp
     c                   WHEN      ( ccia=1 and cpre='AD' ) or
     c                             ( ccia=3 and cpre='AE' )
     c                   Z-ADD     *ZEROS        hMex
     c                   Z-ADD     hBImp         hOtro
     c                   z-add     *ZEROS        hBimp
     c                   OTHER
     c                   z-add     *ZEROS        hMex
     c                   z-add     *ZEROS        hOtro
     c                   ENDSL
     C/EXEC SQL
     C+ INSERT INTO BPCDTA/MCRV VALUES('BH', 'BIL', :cCia, :wAnoMes,
     C+ :wSec, :hFFa, :hFVe, '  ', :wguia, '  ', :pTipS, :pSerS, :pNumS,
     C+ :hRuc, :hCltD  , :hBImp, :hmex, 0, :hImpT, :hOtro, 0, 0, 0, :hPVT,
     C+ :hMon, :hTCam, 1, :hPed, :hClt, :hImp, ' ', ' ', ' ', ' ', ' ',
     C+ :pAno, :cPre, :cTip, :pNum, :hDir1, :hDir2, :hDir3, ' ', ' ',
     C+ ' ', :hCPa, :hCPaD, :hVnd, 0, '1', 4, 0, :cTip, :cPre, :pAno,
     C+ :pNum, ' ', 'XCOMBIL', :wUsrEnt, :wFecEnt, :wHorEnt, ' ', 0, 0,0)
     C/END-EXEC

     C                   IF        ANTICIPO$1 <> 0
     c*-      Imprime el Registro de Anticipo ...
     c                   write     ANTICIPOD1
     c*-      Incrementa en 1 Linea de Imresion de Detalle de Anticipo
     c                   eval      wLinAct=wLinAct+1
     C                   ENDIF

     C                   IF        ANTICIPO$2 <> 0
     c*-      Imprime el Registro de Anticipo ...
     c                   write     ANTICIPOD2
     c*-      Incrementa en 1 Linea de Imresion de Detalle de Anticipo
     c                   eval      wLinAct=wLinAct+1
     C                   ENDIF

     C                   IF        ANTICIPO$3 <> 0
     c*-      Imprime el Registro de Anticipo ...
     c                   write     ANTICIPOD3
     c*-      Incrementa en 1 Linea de Imresion de Detalle de Anticipo
     c                   eval      wLinAct=wLinAct+1
     C                   ENDIF
     c                   exsr      sPrtPie
     c                   add       1             pNumS
     c                   add       1             wPagAct
     c                   endif
     c                   z-add     0             wSec              8 0
     c                   z-add     0             wSec2             8 0
     c                   movel     hFFa          wAnoMes           6 0
     c                   movel     *date         wFecEnt           8 0
     c                   time                    wHorEnt           6 0
     c                   movel     *blanks       wUsrEnt          10
     C/EXEC SQL
     C+ DECLARE C2 CURSOR FOR SELECT SIPXSC FROM msqil01 WHERE
     C+ SICOMP=:cCia and SISBST='B' and SIANMS=:wAnoMes FOR UPDATE OF
     C+ SIPXSC
     C/END-EXEC
     c
     C/EXEC SQL
     C+ OPEN C2
     C/END-EXEC
     c
     C/EXEC SQL
     C+ FETCH C2 INTO :wSec
     C/END-EXEC
     c                   eval      wSec2 = wSec +1
     C/EXEC SQL
     C+ UPDATE BPCDTA/MSQI SET SIPXSC = :wSec2 WHERE CURRENT OF C2
     C/END-EXEC
     c
     C/EXEC SQL
     C+ CLOSE C2
     C/END-EXEC
     c                   exsr      sPrtCab
     c                   eval      hCanT = 0
     c                   eval      hVVT = 0
     c                   eval      hVDT = 0
     c                   eval      hPVT = 0
     c                   eval      hImpT = 0
     c
     c                   endif
     c
     c                   eval      wFlgP1 = *off
     c                   add       hCan          hCanT
     c                   eval      hVV = %dech(hPFa*hCan:11:3)
     c                   add       hVV           hVVT
     c                   eval      hVD = %dech((hPFa-hPNe)*hCan:11:3)
     c                   add       hVD           hVDT
     c                   if        hPFa = 0
     c                   eval      hPD = 0
     c                   else
     c                   eval      hPD = %dech((hPFa-hPNe)*100/hPFa:7:2)
     c                   endif
     c                   add       hImpM         hImpT
     c                   eval      hPV = hVV+hImpM-hVD
     c
     c                   select
     c                   when      hPrdAF = -1
     c                   eval      wPrd  = *blanks
     c                   eval      wPrdD = %subst(hPrdD:1:30)
     c                   when      hPrdAF = 0
     c                   eval      wPrd  = hPrd
     c                   eval      wPrdD = %subst(hPrdD:1:30)
     c                   when      hPrdAF = 1
     c                   eval      wPrd  = hPrdA
     c                   eval      wPrdD = %subst(hPrdAD:1:30)
     c                   other
     c                   eval      wPrd  = hPrd
     c                   eval      wPrdD = %subst(hPrdAD:1:30)
     c                   endsl
     c*
     c*                  write     PRTTIT
     c*
     c                   exsr      sPrtDet
     c                   add       1             wLinAct
      *-------------------------------------------------------------------------
      * DETERMINAR LA LINEA DEL PEDIDO QUE SE FACTURO PARCIAL
      *-------------------------------------------------------------------------
     C                   MOVE      *ZEROS        HLINORD           4 0
     c                   Move      *zeros        SECITH            8 0
     c                   Move      *zeros        HQTYORD          11 3
     c                   Move      *zeros        HFECFAC           8 0
     c                   Move      *zeros        HCLIENTE          8 0
      *
     C/EXEC SQL
     C+ SELECT ILSEQ , ILQTY , ILDATE , ILCUST,ILPKGG
     C+ INTO :HLINORD , :HQTYORD , :HFECFAC , :HCLIENTE,:SECITH
     C+ FROM BPCDTA/YSILF01 WHERE ILCOMP = :cCia  AND
     C+ ILDTYP = :cTip AND  ILDPFX = :cPre  AND ILDYR = :pAno
     C+ AND ILDOCN = :pNum AND  ILLINE = :HLIN
     C/END-EXEC
      *
     C/EXEC SQL
     C+ INSERT INTO BPCDTA/MDRV VALUES('BH', 'BIL', :cCia, :wAnoMes,
     C+ :wSec, :pTipS, :pSerS, :pNumS, :wLinAct, ' ', :wPrd, :wPrdD,
     C+ :hOC, 0, 0, 0, 0, :hCan, :hUMD, 0, ' ', ' ', ' ', 0, 0, 0, :hClt,
     C+ :wLinAct, :cPre, :pNum, :pAno, :cTip, :cTip, :cPre, :pNum, :pAno,
     C+ 0, ' ', ' ', ' ', ' ', ' ', ' ', 0, 0, ' ', 0, 0, :hPed ,
     C+ :hCabCns , :HLINORD, :hCabAlm, :hCls , :hCabCls, :hCabIde ,
     C+ :HQTYORD , :HFECFAC , :hPFa ,  :SECITH  )
     C/END-EXEC

     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO
     C+ :hLin, :hPrd, :hPrdD, :hPrdA, :hPrdAD, :hPrdAF,
     C+ :hAra, :hCls, :hCan, :hUMD, :hPBa, :hPFa,
     C+ :hPNe, :hImp, :hImpM, :hPed, :hSeq, :hOC, :hPosi, :hOCXLIN
     C/END-EXEC
     c                   enddo

      * ***** Imprime Detalle de Anticipos********************************

     c                   MOVEL     *ZEROS        ANTICIPO@        13 2

     c                   MOVEL     *BLANKS       NROEXTFAC1       20
     c                   MOVEL     *ZEROS        ANTICIPO$1       13 2
     c                   MOVEL     *BLANKS       NROEXTFAC2       20
     c                   MOVEL     *ZEROS        ANTICIPO$2       13 2
     c                   MOVEL     *BLANKS       NROEXTFAC3       20
     c                   MOVEL     *ZEROS        ANTICIPO$3       13 2
     c/EXEC SQL
     c+ SELECT  NROEXTFAC1,  ANTICIPO$1  ,
     c+         NROEXTFAC2,  ANTICIPO$2  ,
     c+         NROEXTFAC3,  ANTICIPO$3    INTO
     c+        :NROEXTFAC1, :ANTICIPO$1  ,
     c+        :NROEXTFAC2, :ANTICIPO$2  ,
     c+        :NROEXTFAC3, :ANTICIPO$3
     c+ FROM XANTICIPO WHERE
     c+ CIA=:cCia and TIP=:cTip and PRE=:cPre and ANO=:pAno and NUM=:pNum
     c/END-EXEC
      * ***** Imprime Detalle de Anticipos********************************


     c                   if        wLinAct > 0

     c***IF ANTICIPO******
     c                   eval      ANTICIPO@=ANTICIPO$1+ANTICIPO$2+ANTICIPO$3

     c                   eval      ANTICIPO@ = ANTICIPO@ * -1
     c                   if        ANTICIPO@<>0
     c                   if        wPagAct = 1
     c                   eval      hVVT = hVVT+ANTICIPO@
     c                   eval      hFacTax=%dech( (hVVT-hVDT)*0.18 :15 :2)
     c                   eval      hImpT = hFacTax
     c                   eval      hFacTot = hVVT+hImpT-hVDT
     c                   eval      hPVT = hFacTot
     c                   eval      hBImp = hVVT-hVDT
     c                   else
     c                   eval      hVVT=hVVT+ANTICIPO@
     c                   eval      hImpT=%dech( (hVVT-hVDT)*0.18 :15 :2)
     c                   eval      hBImp = hVVT-hVDT
     c                   eval      hPVT = hBImp+hImpT
     c                   endif
     c***ELSE ANTICIPO****
     c                   else
     c                   if        wPagAct = 1
     c                   eval      hPVT = hFacTot
     c                   eval      hImpT = hFacTax
     c                   eval      hVVT = hPVT-hImpT+hVDT
     c                   eval      hBImp = hVVT-hVDT
     c                   else
     c                   eval      hBImp = hVVT-hVDT
     c                   eval      hPVT = hBImp+hImpT
     c                   endif
     c                   endif
     c***ENDIF ANTICIPO***
     c                   eval      hCltD30 = %subst(hCltD:1:30)
     c
     c                   eval      wguia=*blanks
     C                   if        (cTip<>1) or
     C                             (cCia=2 and cTip=1 and cPre='LQ')
     C/EXEC SQL
     C+ SELECT snseq,sndesc INTO :wseq,:wguia FROM FVBPCSF/ESNL01 WHERE
     C+ sntype='O' AND SNINV='Y' and sncust=:hped ORDER BY snseq desc
     C/END-EXEC
     C                   ENDIF
     c
     C                   SELECT
     c                   WHEN      ( ccia=2 and cpre='LQ' )
     c                   Z-ADD     HBIMP         HMEX
     c                   z-add     *ZEROS        hOtro
     c                   z-add     *ZEROS        HBIMP
     c                   WHEN      ( ccia=1 and cpre='AD' ) or
     c                             ( ccia=3 and cpre='AE' )
     c                   Z-ADD     *ZEROS        hmex
     c                   Z-ADD     HBIMP         hOtro
     c                   z-add     *ZEROS        HBIMP
     c                   OTHER
     c                   z-add     *zeros        hmex
     c                   z-add     *ZEROS        hOtro
     c                   ENDSL
     C
     C/EXEC SQL
     C+ INSERT INTO BPCDTA/MCRV VALUES('BH', 'BIL', :cCia, :wAnoMes,
     C+ :wSec, :hFFa, :hFVe, :hOC, :wguia, '  ', :pTipS, :pSerS, :pNumS,
     C+ :hRuc, :hCltD  , :hBImp, :hmex, 0, :hImpT, :hOtro, 0, 0, 0, :hPVT,
     C+ :hMon, :hTCam, 1, :hPed, :hClt, :hImp, ' ', 'P', ' ', ' ', ' ',
     C+ :pAno, :cPre, :cTip, :pNum, :hDir1, :hDir2, :hDir3, ' ', ' ',
     C+ ' ', :hCPa, :hCPaD, :hVnd, 0, '1', 4, 0, :cTip, :cPre, :pAno,
     C+ :pNum, ' ', 'XCOMBIL', :$UsrNam, :wFecEnt, :wHorEnt, ' ', 0, 0,0)
     C/END-EXEC
     c                   IF        ANTICIPO$1 <> 0
     c*-      Imprime el Registro de Anticipo ...
     c                   write     ANTICIPOD1
     c*-      Incrementa en 1 Linea de Imresion de Detalle de Anticipo
     c                   eval      wLinAct=wLinAct+1
     C                   ENDIF

     c                   IF        ANTICIPO$2 <> 0
     c*-      Imprime el Registro de Anticipo ...
     c                   write     ANTICIPOD2
     c*-      Incrementa en 1 Linea de Imresion de Detalle de Anticipo
     c                   eval      wLinAct=wLinAct+1
     C                   ENDIF

     c                   IF        ANTICIPO$3 <> 0
     c*-      Imprime el Registro de Anticipo ...
     c                   write     ANTICIPOD3
     c*-      Incrementa en 1 Linea de Imresion de Detalle de Anticipo
     c                   eval      wLinAct=wLinAct+1
     C                   ENDIF

     c                   exsr      sPrtPie
     c                   add       1             pNumS
     c
     c                   endif

     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC

     c     eStrPgm       endsr



      ***********************************************************
      *******           Rutinas de Impresion              *******
      ***********************************************************

      ******* Imprime Cabecera de Factura  ****************************
     c     sPrtCab       begsr
     c                   eval      wCLTD = %subst(hCltD:1:45)
     c                   eval      wDIR = %subst(%trim(hDir1)+' '+
     c                                           hDir2:1)
     c*                  eval      wNT2 = %subst(hNt2:1:20)
     c                   eval      wDIRE1 = %subst(%trim(hDirE1)+' '+
     c                                           hDirE2:1)
      *-
     c                   eval      wPosd  = hPosd
     c                   If        wDire1 <> *blanks
     c                   eval      wPosd  = hPosED
     c                   Endif
      *-
     c                   write     PRTHDR
     c                   write     PRTTIT
     c                   eval      wLinAct = 14
     c     ePrtCab       endsr

      * ***** Imprime Detalle de Factura  ********************************
     c     sPrtDet       begsr
     c
     c                   exsr      sclase
     c                   exsr      sclase4
     c                   eval      wCan=%DEC(hCan:11:2)
     c                   eval      wHVV=%DEC(hPNe*hCan:11:2)
     c*                  eval      hOC=hPosi
     c                   eval      OCCYLIN=%editw(hPosi : '   0')+'-'+hOCXLIN
     c                   write     PRTDET
     C
     c     ePrtDet       endsr

      * ***** Imprime Pie de Factura  ***************************************
     c     sPrtPie       begsr
     c                   movel     *blanks       vLetras
     c                   movel     *blanks       vLetra1
     c                   movel     *blanks       vLetra2
     C                   z-add     *Zeros        nDifMnt          11 2
     C                   z-add     *Zeros        whpvt1           11 2

     c                   if        hmon='PEN'
     c                   eval      wmon='S/.'
     c*                  eval      wmon1='S/.'
     c                   else
     c                   eval      wmon='USD'
     c*                  eval      wmon1='USD'
     c                   endif
     c                   eval      wcant=%dec(hcant:11:2)
     c                   eval      wvvt=%dec(hvvt:11:2)
     c                   eval      wvdt=%dec(hvdt:11:2)
     c                   EVAL      WHPVT=%dec(HPVT:11:2)
     c                   EVAL      WHPVT1=%INT(HPVT)
     C                   MOVEL     *BLANKS       WWHPVT           11
     C                   MOVEL     WHPVT         WWHPVT           11
     c                   eval      wvtdt=%dec(wvvt-wvdt:11:2)
     c                   eval      wimpt=%dec(himpt:15:2)
     c
     c                   if        *in99
     c                   exsr      CALC_ANTICIPO
     c                   eval      ANTICIPO@=ANTICIPO$1+ANTICIPO$2+ANTICIPO$3
     c                   eval      ANTICIPO@ = ANTICIPO@ * -1
     c                   if        ANTICIPO@<>0
     c                   eval      wvvt  = %dec(hvvt+ANTICIPO@:11:2)
     c                   eval      wvtdt = wvtdt + ANTICIPO@
     c                   eval      WImpT=%dec(wvtdt*@IGV/100:15:2)
     c                   EVAL      WHPVT = %dec(wVtdt+wImpT:11:2)
     c                   EVAL      WHPVT1= %INT(wVtdt+wImpT)
     c                   eval      hBImp = wVVT-wVDT
     c                   endif
     c                   IF        ANTICIPO$1 <> 0
     c                   write     ANTICIPOD1
     c                   eval      wLinAct=wLinAct+1
     c                   ENDIF
     c                   IF        ANTICIPO$2 <> 0
     c                   write     ANTICIPOD2
     c                   eval      wLinAct=wLinAct+1
     c                   ENDIF
     c                   IF        ANTICIPO$3 <> 0
     c                   write     ANTICIPOD3
     c                   eval      wLinAct=wLinAct+1
     c                   ENDIF
     c                   endif
     C
     c                   Call      'NUMALETR'                                   /APLSTD
     c                   parm      WHPVT1        nCANT            15 5
     c                   parm      2             nNDECIM           1 0
     c                   parm      '1'           vMASFEM           1
     c                   parm      'CON'         vCOLETI          10
     c     vLetras       parm      *blanks       vRESUL          190
     c                   parm      '0'           vCRTN             1

     c                   if        hcpadfp=0
     c**                 eval      wpvt = 0
     c                   eval      *in90 = *off
     c                   else
     c                   eval      *in90 = *on
     C                   Z-ADD     *ZEROS        X                11 2
     c*                  eval      wpvt=%dec(hpvt*(1-hcpadfp/100):11:2)
     c*                  eval      X=%dech(wpvt:11:1)
     c*                  eval      wpvt=x
     c                   if        hcpadfp=0
     c                   eval      *in90 = *off
     c                   endif
     c                   endif

     c                   eval      nDifMnt =%dec(hpvt-whpvt1:11:2)
     c                   move      '00000000000' vTxt             11
     c                   move      nDifMnt       vTxt
     c                   eval      vLetras=%trim(vLetras) + ' con'
     c                   eval      vLetras=%trim(vLetras)+' '+%SUBST(vTxt:10:2)
     c
     c                   Select
     c                   When      HMON = 'PEN'
     c                   eval      vLetras=%trim(vLetras) + '/100 Nuevos Soles'
     c                   When      HMON = 'USD'
     c                   eval      vLetras = %trim(vLetras) +
     c                                       '/100 Dolares Americanos'
     c                   endsl
     C
     c                   eval      Vletra1 = %subst(vletras:1:100)
     c                   eval      Vletra2 = %subst(vletras:101:90)

      * Trae parametros de Prefijo
     C/EXEC SQL
     C+ SELECT NT1D, NT1F, NT2D, NT2F,
     C+        NT3D, NT3F, NT4D, NT4F, NT5D, NT5F, NT6D, NT6F  INTO
     C+ :hNt1D, :hNt1T, :hNt2D, :hNt2T,
     C+ :hNt3D, :hNt3T, :hNt4D, :hNt4T, :hNt5D, :hNt5T, :hNt6D, :hNt6T
     C+ FROM XCOMBIL WHERE CIA=:cCia and TIP=:cTip and PRE=:cPre
     C/END-EXEC
     c                   if        hNt1T = 'Y'
     c                   eval      hNt1 = %trim(hNt1D) + ' ' + hNt1
     c                   endif
     c                   if        hNt2T = 'Y'
     c                   eval      hNt2 = %trim(hNt2D) + ' ' + hNt2
     c                   endif
     c                   if        hNt3T = 'Y'
     c                   eval      hNt3 = %trim(hNt3D) + ' ' + hNt3
     c                   endif
     c                   if        hNt4T = 'Y'
     c                   eval      hNt4 = %trim(hNt4D) + ' ' + hNt4
     c                   endif
     c                   if        hNt5T = 'Y'
     c                   eval      hNt5 = %trim(hNt5D) + ' ' + hNt5
     c                   endif
     c                   if        hNt6T = 'Y'
     c                   eval      hNt6 = %trim(hNt6D) + ' ' + hNt6
     c                   endif
     c
      * Obtiene importe de IGV
 |   c                   z-add     hFFa          w_fechad          8 0
     ‚*------------------------------------------------------------------*
     ‚* Linea Standar de Obtencion de IGV                                *
     ‚*------------------------------------------------------------------*
      * Obtiene importe de IGV
     c                   z-add     0             w_rcedte          8 0
     c                   z-add     0             w_rccrte          7 4
     c                   z-add     0             w_rcnrte          7 4
     c/exec sql
     c+ Select  B.RCCRTE,  B.RCEDTE,  B.RCNRTE
     c+ Into   :w_rccrte, :w_rcedte, :w_rcnrte
     c+ From   SIHL01 A, ZRCL01 B
     c+ Where  (A.IHDOCN=:pNum) And (A.SIORD=:hPed) And (A.SIVATC=B.RCRTCD)
     c/end-exec
     c                   if        w_fechad < w_rcedte
     c                   z-add     w_rccrte      @IGV
     c                   else
     c                   z-add     w_rcnrte      @IGV
     c                   endif
      * Detracciones
     c                   exsr      sprtntx
      *
     c                   write     PRTPIE
     c                   z-add     0             nsw1              1 0
     c                   z-add     0             nsw4              1 0
     c                   eval      *in91 = *off
      *
     C/EXEC SQL
     C+ DECLARE T1 CURSOR FOR SELECT SNCUST,SNSEQ,SNDESC,sntype FROM
     C+ esnl01 WHERE sntype='O' AND SNINV='Y' and sncust=:hped
     C+ ORDER BY SNTYPE,sncust,snseq
     C/END-EXEC
     c
     C/EXEC SQL
     C+ OPEN T1
     C/END-EXEC

     C/EXEC SQL
     C+ FETCH NEXT FROM T1 into
     C+ :SNCUST,:SNSEQ,:SNDESC,
     C+ :SNTYPE
     C/END-EXEC
     c                   DOW       ( SQLCOD <> 100 )
     c                   movel     sndesc        nota
     c                   write     PRTPIE1
     C/EXEC SQL
     C+ FETCH NEXT FROM T1 into
     C+ :sncust,:snseq,:sndesc,
     C+ :SNTYPE
     C/END-EXEC
     c                   enddo
     C/EXEC SQL
     C+ CLOSE T1
     C/END-EXEC
     c     ePrtPie       endsr
      *---------------------------------------------------------------------
      * Busca Clase; si existe pone nsw1 = 1
      *---------------------------------------------------------------------
     c     sClase        begsr
     c                   move      *blanks       vcode            15
     C/EXEC SQL
     C+ DECLARE CFILE1 CURSOR FOR
     C+ SELECT CCCODE FROM fvbpcsf/zccl01 WHERE CCTABL ='YDETRAC'
     C+ AND CCSDSC='700' AND    CCCODE =:hcls
     C/END-EXEC
      *
     c/EXEC SQL
     c+ Open cFile1
     c/END-EXEC
      *
     c/EXEC SQL
     c+ FETCH NEXT FROM CFILE1 INTO :vcode
     c/END-EXEC
     c
     c                   if        sqlcod = 0
     c                   eval      nsw1 = 1
     c                   endif
     c/EXEC SQL
     c+ Close cFile1
     c/END-EXEC
     c                   endsr
      *-----------------------------------------------------------------
      * Busca Clase; si existe pone nsw4 = 1
     C*-----------------------------------------------------------------
     c     sClase4       begsr
     c                   move      *blanks       vcode2           15
     C/EXEC SQL
     C+ DECLARE CFILE2 CURSOR FOR
     C+ SELECT CCCODE FROM fvbpcsf/zccl01 WHERE CCTABL ='YDETRAC'
     C+ AND CCSDSC='400' AND    CCCODE =:hcls
     C/END-EXEC
      *
     c/EXEC SQL
     c+ Open cFile2
     c/END-EXEC
      *
     c/EXEC SQL
     c+ FETCH NEXT FROM CFILE2 INTO :vcode2
     c/END-EXEC
     c
     c                   if        sqlcod = 0
     c                   eval      nsw4 = 1
     c                   endif
     c/EXEC SQL
     c+ Close cFile2
     c/END-EXEC
     c                   endsr
      *---------------------------------------------------------------------
      * Imprime Nota                                                   -----
      *---------------------------------------------------------------------
     c     sprtntx       begsr
     c                   if        nsw1 = 1  or nsw4=1
     c                   if        nsw1 = 1
     c                   select
     c                   when      hmon = 'PEN'
     c                   if        WHPVT >= MTODETRAC
     c                   eval      *in91 = *on
     c                   endif
     c                   when      hmon = 'USD'
     c                   if        WHPVT*HTCAM >= MTODETRAC
     c                   eval      *in91 = *on
     c                   endif
     c                   endsl
     c                   endif
     c                   if        nsw4 = 1
     c                   select
     c                   when      hmon = 'PEN'
     c                   if        WHPVT >= MTODETRAC4
     c                   eval      *in91 = *on
     c                   endif
     c                   when      hmon = 'USD'
     c                   if        WHPVT*HTCAM >= MTODETRAC4
     c                   eval      *in91 = *on
     c                   endif
     c                   endsl
     c                   endif
      *
     c                   if        *in91
     c                   eval      vDetra = 'Operación sujeta al Sistema ' +
     c                                      'de Pago de Obligaciones ' +
     c                                      'Tributarias con el Gobierno ' +
     c                                      'Central'
     c                   exsr      sPrcDetrac
     c                   if        vPrcD <> *blanks
     c                   eval      vDetra = %trim(vDetra) + vPrcD
     c                   else
     c                   eval      vDetra = %trim(vDetra) + '.'
     c                   endif
     c                   endif
     c*
     c                   endif
     c                   endsr
     ‚*---------------------------------------------------------------------
     ‚* Recupera si maneja % de detraccion                                 -
     ‚*---------------------------------------------------------------------
     c     sPrcDetrac    begsr
     c                   movel     *blanks       vPrcD            15
     c/exec sql
     C+ SELECT char(CCDESC) into :vPrcD
     c+ FROM fvbpcsf/zccl01
     c+ WHERE CCTABL = 'YDETRCL' and CCCODE = :hClt
     c/end-exec
     c                   endsr
      *************************************************************************
      *  Rutina de Calculo de Anticipos                                       *
      *************************************************************************
     c     Calc_Anticipo Begsr
      *
     c                   movel     *zeros        anticipo@        13 2
     c                   movel     *zeros        wanticipo        13 2
     c                   movel     *blanks       wnroextfac       20
     c                   movel     *blanks       vnroextfac1      20
     c                   movel     *zeros        vanticipo$1      13 2
     c                   movel     *blanks       vnroextfac2      20
     c                   movel     *zeros        vanticipo$2      13 2
     c                   movel     *blanks       vnroextfac3      20
     c                   movel     *zeros        vanticipo$3      13 2
     c                   z-add     *zeros        Secuencia
     c                   z-add     0             c                 1 0
     c                   z-add     0             subtotal         13 2
     c                   eval      subtotal = hVVT-xVDT
     c     KAnticsldo    setll     ranticsldo
     c     KAnticsldo2   Reade     ranticsldo
     c                   Dow       not %eof
     c                   eval      c=c+1
     c                   select
     c                   when      subtotal >= saldo
     c                   eval         subtotal   = subtotal - saldo
     c                   eval         wanticipo  = saldo
     c                   eval         wnroextfac = nroextfac
     c                   eval         saldo      = 0
     c                   when      subtotal <  saldo
     c                   eval         saldo      = saldo - subtotal
     c                   eval         wanticipo  = subtotal
     c                   eval         wnroextfac = nroextfac
     c                   eval         subtotal   = 0
     c                   endsl
     c                   Exsr      Carga_Antic
     c                   UPDATE    ranticsldo
     c                   If        Subtotal <= 0 Or C=3
     c                   leave
     c                   Endif
     c     KAnticsldo2   reade     ranticsldo
     c                   Enddo
      * Calculo el Anticipo desde el saldo
     c     KAnticipo     Chain     xanticipor
     c                   if        not %found
     c                   movel     cCia          cia
     c                   movel     cTip          tip
     c                   movel     cPre          pre
     c                   z-add     pAno          ano
     c                   z-add     pNum          num
     c                   movel     vnroextfac1   nroextfac1
     c                   z-add     vanticipo$1   anticipo$1
     c                   movel     vnroextfac2   nroextfac2
     c                   z-add     vanticipo$2   anticipo$2
     c                   movel     vnroextfac3   nroextfac3
     c                   z-add     vanticipo$3   anticipo$3
     c                   write     xanticipor
     c                   else
     c                   movel     vnroextfac1   nroextfac1
     c                   z-add     vanticipo$1   anticipo$1
     c                   movel     vnroextfac2   nroextfac2
     c                   z-add     vanticipo$2   anticipo$2
     c                   movel     vnroextfac3   nroextfac3
     c                   z-add     vanticipo$3   anticipo$3
     c                   update    xanticipor
     c                   endif
      *
     c                   Endsr
      *************************************************************************
      *  Rutina de Carga de Anticipos                                         *
      *************************************************************************
     c     Carga_Antic   Begsr
      *
     c                   select
     c                   when      c=1
     c                   eval      vnroextfac1=wnroextfac
     c                   eval      vanticipo$1=wanticipo
     c                   when      c=2
     c                   eval      vnroextfac2=wnroextfac
     c                   eval      vanticipo$2=wanticipo
     c                   when      c=3
     c                   eval      vnroextfac3=wnroextfac
     c                   eval      vanticipo$3=wanticipo
     c                   endsl
      *
     c                   Endsr

**
EXW
FOB
CIF
**
EXW
FCA
CIP
