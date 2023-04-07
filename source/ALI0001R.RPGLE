     H DATFMT(*ISO) DATEDIT(*YMD-)
     ‚**********************************************************************
     ‚* Modulo     : Proyecto de Laptop Alicorp                            *
     ‚* Aplicativo : Obtener parámetros de acceso al servidor FTP y        *
     ‚*              Ejecuta Transferencia de archivo Formulas.            *
     ‚* Fecha      : 18/01/2008                                            *
     ‚**********************************************************************
     faliftp    if   e           k disk
      *
     C
     c                   ExSr      Sr_Proceso
     c                   Eval      *Inlr = *On
     ‚*--------------------< Rutina a Proceso de Programa >-----------------------
     c     Sr_Proceso    BegSr
      * Accesar a la configuracion del FTP por compañia del cliente
     c     k_aliftp      chain     aliftpr                            40
     c                   Dow       *in40 ='0'
     c                   if        alifun = 'RECEPFORM'                         Recepcionar Fórmulas
     c                   exsr      Sr_MovConfig
     c                   Call      'ALI0001C'
     c                   parm                    p_fac
     c                   parm                    w_alicia
     c                   parm                    w_alifun
     c                   parm                    w_aliipc
     c                   parm                    w_alicft
     c                   parm                    w_alifmt
     c                   parm                    w_alifar
     c                   parm                    w_aliuft
     c                   parm                    w_alipft
     c                   parm                    w_aliaft
     c                   parm                    w_alicmd
     c                   parm                    w_aliipa
     c                   parm                    w_alicar
     c                   parm                    w_aliara
     c                   endif
     c     k_aliftp      reade     aliftpr                                40
     c                   enddo
     c
     c                   EndSr
     ‚*--------------------< Rutina mover datos de Config.>-----------------------
     c     Sr_MovConfig  BegSr
     c                   eval      w_alicia = alicia                            ALI
     c                   eval      w_alifun = alifun                            RECEPFORM
     c                   eval      w_aliipc = aliipc                            172.30.32.32
     c                   eval      w_alicft = alicft                            Carpeta en FTP
     c                   eval      w_alifmt = alifmt                            NAMEFMT 1
     c                   eval      w_alifar = alifar                            ASCII
     c                   eval      w_aliuft = aliuft                            usuario ftp
     c                   eval      w_alipft = alipft                            clave ftp
     c                   eval      w_aliaft = aliaft                            Arch:Formula
     c                   eval      w_alicmd = alicmd                            Get/Put
     c                   eval      w_aliipa = aliipa                            IP AS400
     c                   eval      w_alicar = alicar                            Carpeta AS400
     c                   eval      w_aliara = aliara                            Recep.MANTXT/TXTLPC
     c                   EndSr
     ‚*--------------------< Rutina Inicial del Programa >-----------------------
     c     *Inzsr        BegSr
     c     *entry        plist
     c                   parm                    p_fac             3            Facility (FA,GL)
     c                   parm                    p_alicia                       Cod.CiaCliente (ALI)
     c**                 parm                    p_alifun                       Funcion Config FTP:
     c*                                                                         Recepcion,Export,etc
     c     k_aliftp      klist
     c                   kfld                    p_alicia
     c*                  kfld                    p_alifun
     c
     ‚*- Define Variables
     c     *like         define    alicia        p_alicia
     c     *like         define    alicia        w_alicia
     c     *like         define    alifun        p_alifun
     c     *like         define    alifun        w_alifun
     c     *like         define    aliipc        w_aliipc
     c     *like         define    alicft        w_alicft
     c     *like         define    alifmt        w_alifmt
     c     *like         define    alifar        w_alifar
     c     *like         define    aliuft        w_aliuft
     c     *like         define    alipft        w_alipft
     c     *like         define    aliaft        w_aliaft
     c     *like         define    alicmd        w_alicmd
     c     *like         define    aliipa        w_aliipa
     c     *like         define    alicar        w_alicar
     c     *like         define    aliara        w_aliara
     c                   EndSr
