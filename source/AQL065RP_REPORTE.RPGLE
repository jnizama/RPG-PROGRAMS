     ‚*------------------------------------------------------------------*
     ‚* Modulo . . : AQL                                                 *
     ‚* Aplicativo : Registro de Tipos de Análisis                       *
     ‚* Fecha  . . : 07/04/2010                                          *
     ‚* Nota . . . : Standar propio del Subfile                          *
     ‚*------------------------------------------------------------------*
     ‚* Descripci. : Migrar para que soporte - LX                        *
     ‚* Fecha  . . : 18/05/2012                                          *
     ‚* Requerimien: R001192                                             *
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     faql065dp  cf   e             workstn Infds(ds_fileds)
     f                                     sfile(sfl01:w_sln01)
      *
     fzccl01    uf a e           k disk    rename(IPZ100cc:zccl01r)
     ‚*- Variables de Control de Programa                                   ----
     d PgmTit@         c                   const('Yobel SCM')
     d sds_estado     sds
     d   sds_proced      *proc
     d   $User               254    263
      *
     d ds_fileds       ds
     d  cursor               370    371b 0
     d  rrelat               378    379b 0
      *
     d w_next          s              3p 0
     d w_sln01         s              5p 0
     d t_sln01         s              5p 0
     d sf_inicio       s              2s 0  inz(07)
     d sf_lineas       s              2s 0  inz(12)
      *-
     ‚*--------------------< Rutina Execución de Programa >---------------------
     C                   dow       (Not *In03) and (Not *In12)
      *-
     c     w_next        caseq     10            sr_IniciSubf
     c     w_next        caseq     20            sr_CargaSubf
     c     w_next        caseq     30            sr_DisplSubf
     c                   Endcs
     C
     c                   enddo
      *-
     c                   Eval      *Inlr = *On
     ‚*--------------------< Rutina de Inicio del Programa >---------------------
     c     Sr_IniciSubf  BegSr
      *
     c                   eval      d_Ubica = 1
     c                   eval      w_Sln01 = 0
     c                   eval      t_sln01 = 0
     c                   eval      *in50   = *off
     c                   eval      *in54   = *on
     c                   write     sflctl01
     C                   eval      *in54   = *off
     c                   eval      w_next  = 20
      *
     c                   EndSr
     ‚*--------------------< Rutina de Carga de Subfile >-----------------------
     c     Sr_CargaSubf  Begsr
     c
     c                   eval      *In53 = *off
     c                   eval      *In55 = *off
     c
     C     k_cctabl      chain     zccl01r                            81
     c                   dow       (Not *In81)
     c                   clear                   sfl01
     c
     c                   eval      w_opc  = ' '
     c                   eval      w_cod = cccode
     c                   eval      w_des = ccdesc
      *
     c                   add       1             w_sln01
     c                   eval      *in53     = *On
     c                   eval      t_sln01   = w_sln01
      *
     c                   write     sfl01
     c     k_cctabl      reade     zccl01r                                81
     c                   enddo
     c   81              eval      *in55     = *on
     c                   eval      *in52     = *on
     c                   eval      w_next    = 30
     c                   z-add     0             W_linea           5 0
     c
     c                   EndSr
     ‚*-----------------< Rutina Ejecuta Subfile >--------------------
     c     sr_DisplSubf  Begsr
     c                   write     header
     c                   write     footer
      *
     c                   if        w_fila > 0
     c                   eval      d_ubica = w_fila
     c                   endif
      *
     c                   exfmt     sflctl01
     c                   eval      w_error = *all' '
      *
     c                   Exsr      sr_cursor
     c                   Exsr      sr_valopc
      *
     C                   Select
     C                   When      *In06 = *On
     c                   Eval      *In06 = *Off
     c                   exsr      sr_Adicionar
     C                   EndSl
      *
     c                   EndSr
     ‚*----------------< Adiciona Códigos Análisis >------------------
     c     sr_Adicionar  Begsr
     c                   clear                   win01
     c                   exsr      sr_Genera
     c                   eval      w_tit ='Adiciona'
     c                   dow       *in12 = '0'
     c                   exfmt     win01
     c   12              leave
     c                   if        *in10='1'
     c                   clear                   zccl01r
     c                   eval      ccid='CC'
     c                   eval      cctabl = 'AQL_TIPO'
     c                   movel     y_cod         cccode
     c                   eval      ccdesc = y_des
     c                   movel     *date         ccendt
     c                   time                    ccentm
     c                   movel     $User         ccenus
     c                   movel     *date         ccmndt
     c                   time                    ccmntm
     c                   movel     $User         ccmnus
     c                   write     zccl01r
     c
     c                   z-add     10            w_next
     c
     c                   leave
     c                   endif
     c                   enddo
     c                   eval      *in10='0'
     c                   eval      *in12='0'
     c                   Endsr
     ‚*-----------------< Rutina Modifica >-----------------
     c     sr_Modifica   Begsr
     c                   clear                   win01
     c                   movel     w_cod         y_cod
     c                   eval      w_tit ='Modifica'
     c     pky2          chain     zccl01r
     c                   if        %found()
     c                   eval      y_des = ccdesc
     c                   endif
     c                   dow       *in12 = '0'
     c                   exfmt     win01
     c   12              leave
     c                   if        *in10='1'
     c     pky2          chain     zccl01r                            83
     c                   if        *in83='0'
     c                   eval      ccdesc = y_des
     c                   movel     *date         ccmndt
     c                   time                    ccmntm
     c                   movel     $User         ccmnus
     c                   update    zccl01r
     c                   endif
     c                   leave
     c                   endif
     c                   enddo
     c                   eval      *in10='0'
     c                   eval      *in12='0'
     c                   Endsr
     ‚*--------------------< Rutina Genera Correlativo >---------------------
     c     sr_Genera     begsr
     c                   z-add     *zeros        w_codn            1 0
     C     k_cctabl      chain     zccl01r                            82
     c                   dow       *in82='0'
     c                   movel     cccode        w_codn
     c     k_cctabl      reade     zccl01r                                82
     c                   enddo
     c                   if        w_codn = 0
     c                   z-add     1             w_codn
     c                   else
     c                   eval      w_codn = w_codn + 1
     c                   endif
     c                   movel     w_codn        y_cod
     c                   endsr
     ‚*----------------< Posicion Relativa de cursor >--------------------------
     c     sr_cursor     Begsr
     c
     c     cursor        div       256           fila              5 0
     c                   mvr                     columna           4 0
     c
     c                   select
     c                   when      fila < sf_inicio
     c                   eval      fila = sf_inicio
     c                   when      fila > (sf_inicio + sf_lineas)
     c                   eval      fila = (sf_inicio + sf_lineas - 1)
     c                   endsl
     c
     c                   eval      fila = fila + rrelat - sf_inicio
     c                   if        fila > t_sln01
     c                   eval      fila = t_sln01
     c                   endif
     c                   EndSr
     ‚*--------------------< Rutina Validar Opciones >-----------------------
     c     sr_valopc     Begsr
      *
     c                   eval      w_fila = 0
     c                   eval      w_error = *all' '
     c                   eval      *in98 = *off
     c                   eval      *in99   = *off
     c                   if        (w_linea > 0) and (w_linea <= t_sln01)
     c     w_linea       chain     sfl01                              81
     c                   eval      *in60   = *off
     c                   update    sfl01
     c                   endif
     c
     c                   eval      w_linea = 0
     c
     c     1             do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              81
     c                   eval      *in60   = *off
     c                   if        %trim(w_opc) <> '2' AND
     c                             %trim(w_opc) <> '4' AND
     C                             %trim(w_opc) <> ' '
     c                   eval      w_linea = w_sln01
     c                   eval      *in60   = *on
     c                   eval      w_error = 'Valor ingresado es incorrecto'
     c                   eval      *in99   = *on
     c                   endif
     c
     C                   update    sfl01
     C                   if        *in99
     C                   eval      d_ubica = w_sln01
     C                   leave
     c                   endif
     c                   endDo
      *
     c                   if        not *in99
     c     1             do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              81
     c                   select
     c                   when      (%trim(W_Opc) = '2' )
     c                   movel     w_cod         k_cod
     c                   exsr      sr_Modifica
     c                   z-add     10            w_Next
      *
     c                   When      (%trim(W_Opc) = '4' )
     c                   movel     w_cod         k_cod
     c     pky2          chain     zccl01r                            83
     c                   if        *in83='0'
     c                   delete    zccl01r
     c                   endif
     c                   z-add     10            w_Next
      *
     C                   EndSl
     c
     C                   eval      w_opc = ' '
     C                   update    sfl01
     C                   enddo
     c
     c                   if        w_linea > 0
     c                   eval      d_ubica = w_linea
     c                   else
     c                   eval      d_ubica = fila
     c                   endif
     c                   endif
     c                   EndSr
     ‚*--------------------< Rutina Inicial de Programa >-----------------------
     c     *inzsr        Begsr
      *-
     c     Pky1          klist
     c                   kfld                    K_cctabl
     c                   kfld                    K_cccode
      *-
     c     Pky2          klist
     c                   kfld                    K_cctabl
     c                   kfld                    k_cod            15
      *-
     c                   eval      k_cctabl='AQL_TIPO'
      *-
     c                   movel(p)  PgmTit@       $PgmTit
     c                   movel     sds_proced    $pgmcod
     c                   z-add     10            w_next
      *-
     c     *like         define    w_sln01       x_sln01
     c     *like         define    cctabl        p_cctabl
     c     *like         define    cctabl        K_cctabl
     c     *like         define    cccode        p_cccode
     c     *like         define    cccode        k_cccode
     c     *like         define    ccdesc        p_ccdesc
     c     *like         define    ccdesc        p_ccsdsc
     c     *like         define    ccnot1        p_ccnot1
     c     *like         define    ccnot2        p_ccnot2
     c     *like         define    ccudc1        p_ccudc1
     c     *like         define    ccudc2        p_ccudc2
     c     *like         define    ccudc3        p_ccudc3
     c                   z-add     *zeros        w_fila            5 0
     C                   Endsr
