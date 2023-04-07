     ‚*------------------------------------------------------------------*
     ‚*                          AQL                                     *
     ‚* Modulo:                                                          *
     ‚* Aplicativo:  Ingresa Facility y Usuario                          *
     ‚* Fecha:       19/05/2009                                          *
     ‚*------------------------------------------------------------------*
     ‚* Aplicativo : Adaptacion al ERP LX                                *
     ‚* Fecha . . .: 03/05/2011                                          *
     ‚*------------------------------------------------------------------*
     ‚* Creando PGM: CRTPGM PGM(BPCPGM/AQL001RP) MODULE(BPCPGM/AQL001RP
     ‚*              BPCPGM/INVY404D) BNDSRVPGM(SFLSRVPGM)
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     faql001dp  cf   e             workstn infds(infds)
     fzmfl01    if   e           k disk    rename(IPZ100MF:zmfl01r)
     fscmaql    if   e           k disk    rename(AQLMDCR:aqll01r)
     fiiml01    if   e           k disk    rename(IPI100IM:iiml01r)
     fiicl01    if   e           k disk
     fzccl01    if   e           k disk    rename(IPZ100CC:zccl01r)
      *
     d w_fecha         s               d   datfmt(*iso)
     d                sds
     d   $User               254    263
      *
     d infds           ds
     d  tecla                369    369
     d  intro          c                   const(X'F1')
     ‚*--------------------< Rutina Execución de Programa >---------------------
     c     RutPad        begsr
      *
      *                  movel     $user         w_user
     ‚* Proceso Principal
     c                   dow       (not *In03) and (not *in12)
      *->

     c                   exfmt     wdw01
     c                   eval      vmsgerr = *all' '
      *->
     ‚* Salida del Programa x función
     c                   if        *in03 or *in12
     c                   leave
     c                   endif
      *->
     ‚* Función de Consulta F4
     c                   if        *in04
     c                   exsr      sr_funlista
     c                   eval      *in04   = *off
     c                   iter
     c                   endif
     c*->
     ‚* Valida lo ingresado
     c                   exsr      sValIngDat
     ‚* sin error
     c                   If        vTodoOK<>'NO'
     c                   exsr      sr_Call
     c                   endif
     c
     c                   Enddo
      *
     c     fin_progr     Endsr
      *
     ‚*--------------------< Rutina q' invoca Ste Programa >--------------------
     c     sr_call       begsr
      *
     c                   call      'AQL002RP'
     c                   parm      vcia          p_fac
     c                   parm      vdes          p_DescFac
     C                   PARM      w_item        p_Pro
     C                   PARM      w_desc        p_DesPro
     C                   PARM                    p_Cla
     C                   PARM      w_planner     p_Pla
     C                   PARM      w_fam         p_Fam
     C                   PARM      w_sfam        p_Sfam
     c     *in03         parm      *off          PrmF03            1
      *
     c                   endsr
      *-
     ‚*--------------------< Valida Ingreso de Datos >--------------------------
     c     sValIngDat    Begsr
     c*-
     ‚* Inicializa Variables de rutina
     c                   movel     *blanks       vTodoOk           2
     c                   movea     '0000000'     *in(60)
      *-
     c                   do
      * Ingrese cia
     c                   call      'OPE705RP'
     c                   parm      vcia          p_fac
     c                   parm      *all' '       p_DescFac
     c                   parm      '1'           p_nivel           1
     c                   parm      *all' '       p_Mensaje        30
     c                   parm      ' '           p_Ok              2
     c*-
     c                   if        (p_ok<>'OK')
     c                   eval      vmsgerr = p_mensaje
     c                   eval      vTodoOK = 'NO'
     c                   eval      vdes    = *all' '
     c*                  eval      *in60 = *on
     c                   move      '1000000'     *in60
     c                   leave
     c                   else
     c                   eval      vdes    = p_DescFac
     c                   endif
      *-
      * Ingresa Producto
     c                   if        %trim(w_item) <> ''
     c                   eval      k_cod  = %trim(w_item)
     c     k_iiml01      chain     iiml01r
     c                   if        not %found()
     c                   movel     w_item        cod              20
     c                   eval      vmsgerr  ='Codigo de Producto ('+%trim(cod)+
     c                             ') No Existe en el maestro'
     c                   eval      vTodoOK = 'NO'
     c                   eval      w_item= *blanks
     c                   move      '0010000'     *in60
     c                   leave
     c                   endif
     c                   endif
      *
      * Ingresa Clase
     c*                  if        %trim(w_clase) <> ''
     c*                  eval      k_clase= %trim(w_clase)
     c*    w_clase       chain     IPI160IC
     c*                  if        not %found()
     c*                  eval      *in69=*on
     c*                  eval      vmsgerr  ='Codigo de Clase ('+%trim(clas)+
     c*                            ') No Existe en el maestro'
     c*                  eval      vTodoOK = 'NO'
     c*                  eval      w_clase= *blanks
     c*                  move      '0001000'     *in60
     c*                  leave
     c*                  endif
     c*                  endif
      *
      * Ingresa Planeador
     c                   if        %trim(w_planner) <> ''
     c                   eval      k_cctabl = 'PLANNER'
     c                   eval      k_cccode = %trim(w_planner)
     c     k_zccl01      chain     zccl01r
     c                   if        not %found()
     c                   movel     w_planner     plan              3
     c                   eval      vmsgerr='Codigo de Planeador ('+%trim(plan)+
     c                             ') No Existe en el maestro'
     c                   eval      vTodoOK = 'NO'
     c                   eval      w_planner = *blanks
     c                   move      '0000100'     *in60
     c                   leave
     c                   endif
     c                   endif
      *
      * Ingresa Familia
     c                   if        %trim(w_fam) <> ''
     c                   eval      k_cctabl = 'SIRF3'
     c                   eval      k_cccode = %trim(w_fam)
     c     k_zccl01      chain     zccl01r
     c                   if        not %found()
     c                   movel     w_fam         fami              5
     c                   eval      vmsgerr ='Codigo de Familia ('+%trim(fami)+
     c                             ') No Existe en el maestro'
     c                   eval      vTodoOK = 'NO'
     c                   eval      w_fam  = *blanks
     c                   move      '0000010'     *in60
     c                   leave
     c                   endif
     c                   endif
      *
      * Ingresa Sub Familia
     c                   if        %trim(w_sfam) <> ''
     c                   eval      k_cctabl = 'SIRF3'
     c                   eval      k_cccode = %trim(w_sfam)
     c     k_zccl01      chain     zccl01r
     c                   if        not %found()
     c                   movel     w_sfam        sfami             5
     c                   eval      vmsgerr  ='Codigo de Sub Familia (' +
     c                             %trim(w_sfam)+') No existe en el maestro'
     c                   eval      vTodoOK = 'NO'
     c                   eval      w_sfam = *blanks
     c                   move      '0000001'     *in60
     c                   leave
     c                   endif
     c                   endif
     c                   enddo
      *-
     c                   Endsr
      *-
     ‚*----------------< F4: Lista pantallas de seleccion por campo >-----------
     c     sr_funlista   Begsr
      *-
     c                   movel     *blanks       vTodoOk           2
     c                   eval      *in04 = *off
     c                   movea     '00'          *in(60)
     ‚* Pantalla
     c                   if        $rec  = 'WDW01'
      *-
     c                   select
     ‚*            Consulta de Activo
     c                   when      $Fld  = 'VCIA'
     c                   call      'RAT903RP'
     c                   parm      '  '          p_fac
      *-
     c                   if        p_fac  <> *all' '
     c                   movel     p_fac         vcia
     c     p_fac         chain     zmfl01r
     c                   if        %found
     c                   eval      Vdes   = %trim(mfdesc)
     c                   endif
     c                   eval      *in61 = *on
     c                   else
     c                   eval      *in60 = *on
     c                   endif
      *-
     c                   when      $Fld = 'W_ITEM'
     c                   callB     'INVY404D'
     c                   parm                    p_codpro
     c                   parm                    p_despro
     c*
     c                   if        (p_codpro <>*all' ' and p_despro <>*all' ')
     c                   movel     p_codpro      w_item
     c                   movel     p_despro      w_desc
     c                   eval      *in61 = *on
     c                   else
     c                   eval      *in60 = *on
     c                   endif
      *
     c                   WHEN      $Fld = 'W_CLASE'
     c
     c
     c
     c
      *
     c                   WHEN      $Fld = 'W_PLANNER'
     c                   Call      'LXC623RP'
     c                   parm                    vcia
     c                   parm                    vdes
     c                   parm      'P'           tipo                           Planeadores
     c                   parm                    p_codpb
     c                   parm                    p_despb
     c
     c                   if        p_codpb <> ' '
     c                   movel     p_codpb       w_planner
     c                   eval      *in61 = *on
     c                   else
     c                   eval      *in60 = *on
     c                   endif
      *
     c                   WHEN      $Fld = 'W_FAM'
     c                   eval      p_titcab = 'FAMILIA DEL PRODUCTO'
     c                   call      'SCM650RP'
     c                   parm      'SIRF3'       p_cctabl          8
     c                   parm                    p_titcab         20
     c                   parm                    p_cod            15
     c                   parm                    p_desfam         30
     c
     c                   if        (p_cod <> '')
     c                   eval      w_fam = p_cod
     c                   eval      *in61 = *on
     c                   else
     c                   eval      *in60 = *on
     c                   endif
      *
     c                   WHEN      $Fld = 'W_SFAM'
     c                   eval      p_titcab = 'SUB.FAMILIA DEL PRODUCTO'
     c                   call      'SCM650RP'
     c                   parm      'SIRF2'       p_cctabl          8
     c                   parm                    p_titcab         20
     c                   parm                    p_cod            15
     c                   parm                    p_dessfam        30
     c
     c                   if        (p_cod <> '')
     c                   eval      w_sfam = p_cod
     c                   eval      *in61 = *on
     c                   else
     c                   eval      *in60 = *on
     c                   endif
     c                   Other
     c                   eval      *in60 = *on
      *-
     c                   endsl
     c                   endif
     c*-
     c                   endsr
     ‚*--------------------< Rutina Usuario             >-----------------------
     c     sr_user       begsr
      *-
     c                   call      'SCM911RP'
     c                   parm      $User         p_user
     c                   parm      '  '          p_fac
      *-
     c                   if        (p_fac<>' ')
     c                   eval      Vcia = p_fac
     c     vcia          chain(n)  zmfl01r
     c                   movel(p)  mfdesc        vdes
     c                   movea     '01'          *in(60)
     c                   else
     c                   movea     '10'          *in(60)
     c                   endif
     c*-
     c                   endsr
     ‚*--------------------< Rutina Inicial de Programa >-----------------------
     c     *inzsr        begsr
      *-
     ‚*- Definición de Variables                                            ----
     c     *like         define    mffacl        p_fac
     c     *like         define    mfdesc        p_DescFac
     c     *like         define    mfdesc        p_DescPro
     c     *like         define    AQLITEM       p_Pro
     c     *like         define    AQLCLAS       p_Cla
     c     *like         define    AQLPLAN       p_Pla
     c     *like         define    AQLFAM        p_Fam
     c     *like         define    AQLSFAM       p_sfam
     c     *like         define    $User         p_User
     c     *like         define    cctabl        k_cctabl
     c     *like         define    cccode        k_cccode
     c     *like         define    iprod         k_cod
     c     *like         define    iprod         p_codpro
     c     *like         define    idesc         p_despro
     c     *like         define    idesc         w_desc
     c     *like         define    mffwa         tipo
     c     *like         define    mfste         p_codpb
     c     *like         define    mfad1         p_despb
      *-
     ‚*- Claves de Acceso                                                   ----
     c     k_zccl01      klist
     c                   kfld                    k_cctabl
     c                   kfld                    k_cccode
      *
     c     k_iiml01      klist
     c                   kfld                    k_cod
     ‚* Facility x Default                                                  ----
      *-
     c                   exsr      sr_user
      *-
     ‚* Executa Programa                                                    ----
     c                   exsr      RutPad
     c                   eval      *inlr = *on
      *-
     c                   Endsr
