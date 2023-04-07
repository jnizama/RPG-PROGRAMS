     ‚*------------------------------------------------------------------*
     ‚*                    YOBEL SCM                                     *
     ‚* Modulo . . : AQL                                                 *
     ‚* Aplicativo : Emite Informe final                                 *
     ‚* Fecha  . . : 07/04/2010                                          *
     ‚*------------------------------------------------------------------*
     ‚* Descripci. : Migrar para que soporte - LX                        *
     ‚* Fecha  . . : 18/05/2012                                          *
     ‚* Requerimien: R0001192                                            *
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     faql060dp  cf   e             workstn infds(infds)
     faqlsga    if   e           k disk
     fiiml02    if   e           k disk    rename(IPI100IM:iiml02r)
     fzccl01    if   e           k disk    rename(ipz100cc:zccl01r)
     frcml01    if   e           k disk    rename(IPR100CM:rcml01r)
     fzmfl01    if   e           k disk    rename(IPZ100MF:zmfl01r)
     f                                     prefix(L31_ )
      *-
     d w_fecha         s               d   datfmt(*iso)
     d                sds
     d   $User               254    263
      *-
     d infds           ds
     d  tecla                369    369
     d  intro          c                   const(X'F1')
      *-
     d w_dprod         s             50a
     d
     ‚*--------------------< Rutina Execución de Programa >---------------------
     c     sr_Imprime    begsr
     c                   Dow       (not *In03) and (not *in12)
     c                   exfmt     wdw01
     c                   eval      vmsgerr = *all' '
     c                   if        *in03 or *in12
     c                   leave
     c                   endif
     ‚* Función de Consulta F4
     c                   if        *in04
     c                   exsr      sr_funlista
     c                   eval      *in04   = *off
     c                   iter
     c                   endif
     ‚* Valida lo ingresado
     c                   exsr      sValIngDat
     ‚* sin error
     c                   If        vTodoOK<>'NO'
     c                   exsr      sr_Call
     c                   if        %trim(vmsgerr) <> ''
     c                   iter
     c                   endif
     c                   endif
     c
     c                   Enddo
      *
     c                   Endsr

     ‚*--------------------< Rutina q' invoca Ste Programa >--------------------
     c     sr_Call       begsr
      *
     c                   movel     w_cli         p_cli             8
     c                   movel     w_numa        p_numana          8
     c
     c                   Call      'AQL130CL2'
     c                   parm                    vcia
     c                   parm                    vdes
     c                   parm                    p_cli
     c                   parm                    w_dcli
     c                   parm                    p_numana
     c                   parm                    w_prod
     c                   parm                    w_dprod
      *
     c                   eval      vmsgerr = 'El reporte se encuentra en su -
     c                             Spool'
     c                   endsr

     ‚*--------------------< Valida Ingreso de Datos >--------------------------
     c     sValIngDat    Begsr
      *
     ‚* Inicializa Variables de rutina
     c                   movel     *blanks       vTodoOk           2
     c                   movea     '0000'        *in(60)
      *-
     c                   do
      * Ingrese cia
     c                   if        (vcia   = *all' ')
     c                   eval      vmsgerr = 'Ingrese código Facility'
     c                   eval      vTodoOK = 'NO'
     c                   eval      vdes    = *all' '
     c                   eval      *in60 = *on
     c                   else
     c     vcia          chain(n)  zmfl01r
     c                   if        not %found
     c                   eval      vmsgerr = 'Codigo ('+ vcia +
     c                             ') No se encuentra en tabla '
     c                   eval      vTodoOK = 'NO'
     c                   eval      vdes    = *all' '
     c                   eval      *in60 = *on
     c                   else
     c                   movel(p)  L31_mfdesc    vdes
     c                   endif
     c                   endif
     c   60              leave
      *-
     c                   If        vcia = 'GL'
     c                   eval      vmsgerr = 'Facility ('+ vcia +
     c                             ') No Habilitado para esta Consulta'
     c                   eval      vTodoOK = 'NO'
     c                   eval      vdes    = *all' '
     c                   eval      *in60 = *on
     c                   EndIf
     c   60              leave
      *-
     c                   eval      w_dcli  = *all' '
     c                   if        (w_cli = 0)
     c                   eval      vmsgerr = 'Ingrese Código de Cliente'
     c                   eval      vTodoOK = 'NO'
     c                   eval      *in61 = *on
     c                   else
     c     k_rcml01      chain     rcml01r
     c                   if        not %found
     c                   eval      vmsgerr = 'Código de Cliente no existe'
     c                   eval      vTodoOK = 'NO'
     c                   eval      *in61 = *on
     c                   else
     c                   eval      w_dcli = %trim(cnme)
     c                   endif
     c                   endif
     c   61              leave
      *- Análisis
     c                   if        w_numa = 0
     c                   eval      vmsgerr = 'Debe ingresar Número de Análisis'
     c                   eval      vTodoOK = 'NO'
     c                   eval      *in62 = *on
     c                   else
     c     k_aqlsga      chain     aqlsgar                            40
     c                   if        *in40='0'
     c                   movel     sgaite        w_prod
     c     w_prod        chain     iiml02r
     c                   eval      w_dprod = idesc
     c                   else
     c                   eval      vmsgerr = 'Número análisis no existe'
     c                   eval      vTodoOK = 'NO'
     c                   eval      *in62 = *on
     c                   eval      w_dprod=*all' '
     c                   endif
     c                   endif
     c   62              leave
     c
     c                   enddo
      *-
     c                   Endsr
      *-
     ‚*----------------< F4: Lista pantallas de seleccion por campo >-----------
     c     sr_funlista   Begsr
      *-
     c                   movel     *blanks       vTodoOk           2
     c                   eval      *in04 = *off
     c                   movea     '0000'        *in(60)
     ‚* Pantalla
     c                   if        $rec  = 'WDW01'
      *-
     c                   select
     ‚*            Consulta de Activo
     c                   when      $Fld  = 'VCIA'
     c                   call      'RAT903RP'
     c                   parm      '  '          p_val             3
      *-
     c                   if        p_val  <> *all' '
     c                   movel     p_val         vcia
     c     p_val         chain     zmfl01r
     c                   if        %found
     c                   eval      Vdes   = L31_mfdesc
     c                   endif
     c                   eval      *in61 = *on
     c                   else
     c                   eval      *in60 = *on
     c                   endif
      *-
     c                   when      $Fld  = 'W_CLI'
     c                   Call      'INVY412D'
     c                   parm      *all' '       p_cli             8
     c                   parm      *all' '       p_desc           30
     c                   if        p_desc <> *all' '
     c                   movel     p_cli         w_cli
     c                   eval      w_dcli  = p_desc
     c                   eval      *in62 = *on
     c                   else
     c                   eval      *in61 = *on
     c                   endif
      *-
     c                   Other
     c                   eval      *in60 = *on
      *-
     c                   endsl
     c                   endif
     c*-
     c                   endsr
     ‚*--------------------< Rutina Inicial de Programa >-----------------------
     c     *inzsr        begsr
      *-
     ‚* Estructura de Claves                                                ----
     c     k_rcml01      klist
     c                   kfld                    w_cli
      *-
     c     k_aqlsga      klist
     c                   kfld                    vcia
     c                   kfld                    w_cli
     c                   kfld                    w_numa
      *-
     c     k_zccl01      klist
     c                   kfld                    k_cctabl
     c                   kfld                    k_cccode
      *-
     ‚* Facility x Default                                                  ----
     c                   Call      'SCM911RP'
     c                   parm      $User         p_user           10
     c                   parm      '  '          p_fac             3
     c                   if        (p_fac<>' ')
     c                   eval      vcia = p_fac
     c     vcia          chain(n)  zmfl01r
     c                   movel(p)  L31_mfdesc    vdes
     c                   movea     '0100'        *in(60)
     c                   else
     c                   movea     '1000'        *in(60)
     c                   endif
      *-
     c                   eval      k_cctabl = 'AQL_TPRO'
     ‚*- Declaración de Variables                                           ----
     c     *like         define    cctabl        k_cctabl
     c     *like         define    cccode        k_cccode
     c     *like         define    sgaite        w_prod
     c*    *like         define    ccdesc        w_dprod
     ‚* Executa Programa                                                    ----
     c                   exsr      sr_Imprime
     c                   eval      *inlr = *on
      *-
     c                   Endsr
      *-
