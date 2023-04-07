     ‚*------------------------------------------------------------------*
     ‚* Modulo     : INV                                                 *
     ‚* Aplicativo : Ingresa Facility para Movimiento entre Almacenes    *
     ‚* Fecha:       07/05/2008                                          *
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     fAUD200DP  cf   e             workstn infds(infds)
     flwkhst    if   e           k disk    rename(IPL100WK:lwkhst01r)
      *-
     d w_fecha         s               d   datfmt(*iso)
     d                sds
     d   $User               254    263
      *-
     d infds           ds
     d  tecla                369    369
     d  intro          c                   const(X'F1')
      *-
     ‚*--------------------< Rutina Execución de Programa >---------------------
     c     RutPad        begsr
      *-
     ‚* Proceso Principal
     c                   Dow       (not *In03) and (not *in12)
      *->
     c                   exfmt     wdw01
     c                   eval      vmsgerr = *all' '
      *->
     ‚* Salida del Programa x función
     c                   if        *in03 or *in12
     c                   leave
     c                   endif
     c*->
     ‚* Valida lo ingresado
     c                   exsr      sValIngDat
     ‚* sin error
     c                   If        vTodoOK<>'NO'
     c                   ExSr      sr_Call
     c                   endif
     c
     c                   Enddo
      *
     c                   Endsr

     ‚*--------------------< Rutina q' invoca Ste Programa >--------------------
     c     sr_Call       begsr
      *
      *  Historico de Centros de Trabajo  - Interactivo
     c                   call      'AUD201RP'
     c                   parm                    p_periodo
     c*
      *  Historico de Costos del periodo que termina - Batch
     c                   call      'CEA277RP2'
     c                   parm                    p_periodo
      *
     c                   endsr

     ‚*--------------------< Valida Ingreso de Datos >--------------------------
     c     sValIngDat    Begsr
     c*-
     ‚* Inicializa Variables de rutina
     c                   movel     *blanks       vTodoOk           2
     c                   movel     w_periodo     p_periodo         6
      *-
     c                   do
      * Ingrese Periodo
     c                   if        (p_periodo  = *zeros )
     c                   eval      vmsgerr = 'Ingrese Periodo'
     c                   eval      vTodoOK = 'NO'
     c                   eval      *in60 = *on
     c                   else
     c                   eval      vmsgerr = 'Periodo Generado...'
     c                   endif
     c   60              leave
      *-
     c                   enddo
      *-
     c                   Endsr
     ‚*--------------------< Rutina Inicial de Programa >-----------------------
     c     *inzsr        begsr
      *-
     c     k_lwkhst      klist
     c                   kfld                    k_periodo
      *
     c     *like         define    PERIODO       k_periodo
      *
      *-
     c                   exsr      RutPad
     c                   eval      *inlr = *on
      *-
     c                   Endsr
      *-
