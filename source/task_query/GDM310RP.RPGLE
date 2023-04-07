     ‚*---------------------------------------------------------------------*
     ‚* PROGRAMA      : Consulta F4 de Tareas o Procesos                    *
     ‚* REQUERIMIENTO : R001441                                             *
     ‚* FECHA         : 13/12/2012                                          *
     ‚*---------------------------------------------------------------------*
     FGDM310DP  CF   E             WORKSTN SFILE(sfl01:nrr)
     FGDMTARL1  IF   E           K DISK
      *-
     ‚* Recupera Datos de sistema                                           ----
     d sds_estado     sds
     d   sds_proced      *proc
     d   $User               254    263
      *-
     d ds_fileds       ds
     d  cursor               370    371b 0
     d  rrelat               378    379b 0
      *-
     ‚* Variables de Control de Programa                                    ----
     dnrr              s              4s 0
     d PgmTit@         c                   const('YOBEL SCM')
     d w_next          s              3p 0
     d k_ubica         s              5p 0
     d w_sln01         s              5p 0
     d t_sln01         s              5p 0
     d sf_inicio       s              2s 0  inz(08)
     d sf_lineas       s              2s 0  inz(10)
     d w_linea         s              5s 0  inz(0)
     d w_fecha         s               d   datfmt(*iso)
      *
     ‚* Variables de Control de Programa                                    ----
     ‚* ------------------------------                                      ----
     ‚* Rutina Principal de Programa |                                      ----
     ‚* ------------------------------                                      ----
     c                   exsr      sr_inicisubf
     c                   exsr      sr_claresubf
     c                   exsr      sr_cargasubf
      *
     c                   dow       not *in03 and not *in12
     c                   exsr      sr_displsubf
     c                   enddo
     c                   write     keep
     c                   close     *all
     c                   eval      *inlr=*on
      *
     ‚*--------------------< Rutina de Inicial de Subfile >---------------------
     c     sr_inicisubf  begsr
     c                   eval      *in50=*on
     c                   write     control
     c                   eval      *in50=*off
     c                   eval      nrr=0
     c                   endsr
      *
     ‚*--------------------< Rutina de Clarea Subfile >-----------------------
     c     sr_claresubf  begsr
     c                   eval      *in30=*on
     c                   write     control
     c                   eval      *in30=*off
     c                   eval      nrr=0
     c                   endsr
      *
     ‚*--------------------< Rutina de Carga Subfile >-----------------------
     c     sr_cargasubf  begsr
     c*
     c     k_tar01       CHAIN     TARFORR                            40
     c                   dow       *in40='0'
     c                   exsr      sr_CargaDat
     c                   eval      nrr = nrr + 1
     c                   eval      t_sln01 = nrr
     c                   write     sfl01                                  39
     c     k_tar01       READE     TARFORR                                40
     c                   enddo
     c*
     c                   if        *in40='1' and nrr = 0
     c                   z-add     1             nrr
     c                   if        nrr=1
     c                   eval      w_destar='No existen datos a Mostrar'
     c                   eval      *in41='1'
     c                   write     sfl01                                  39
     c                   endif
     c                   endif
     c
     c                   eval      *in61 = *off
      *
     c                   endsr
     ‚*--------------------< Rutina Displaya Subfile >-----------------------
     c     sr_displsubf  begsr
      *
     c                   seton                                        1020
     c                   write     control
     c                   write     pie
     c                   exfmt     control
     c                   setoff                                       1020
     c*
     c                   if        *in12 or *in03
     c                   eval      *inlr = *on
     c                   return
     c                   endif
     c*
     c                   exsr      sr_valopc
     c
     c                   endsr
      *
     ‚* ---------------------------                                         ----
     ‚* Valida Opciones Ingresadas|                                         ----
     ‚* ---------------------------                                         ----
     c     sr_valopc     begsr
      *
     c                   eval      w_error = *all' '
     c                   eval      *in99   = *off
     c                   eval      k_ubica = 0
     c                   eval      w_fila = 0
     c                   eval      w_linea = 0
      *
     c     1             do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              81
     c                   eval      *in60   = *off
     c                   if        w_opc <> '1' and
     c                             w_opc <> ' '
     c                   eval      w_linea = w_sln01
     c                   eval      *in60   = *on
     c                   eval      w_error = 'Valor ingresado (' + w_opc  +
     c                                       ') es incorrecto'
     c                   eval      *in99 = *on
     c                   endif
      *
     c                   update    sfl01
     c                   if        *in99
     c                   eval      puntero = w_sln01
     c                   leave
     c                   endif
      *
     c                   enddo
      *
     c                   if        not *in99
     c     1             do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              81
      *
     c                   select
     c                   when      (w_opc='1')
     c                   movel     w_codtar      p_car
     c                   movel     w_destar      p_destar
     c                   eval      *inlr = *on
     c                   return
     c                   endsl
     c                   enddo
      *
     c                   eval      w_linea = w_sln01
     c                   eval      *in60   = *on
      *
     c                   endif
      *
     c                   endsr
     ‚* ------------------------------------------------------------------------
     ‚* Rutina mueve datos de archivo a subfile data                        ----
     ‚* ------------------------------------------------------------------------
     c     sr_CargaDat   begsr
     c                   eval      w_codtar= tarcar
     c                   eval      w_destar= tardes
     c                   endsr
     c
     ‚* --------------------                                                ----
     ‚* Rutinas Iniciales  |                                                ----
     ‚* --------------------                                                ----
     c     *inzsr        begsr
      *-
     c     *entry        plist
     c     k_fac         parm                    p_fac
     c                   parm                    p_des            30
     c                   parm                    p_tit            12
     c     k_car         parm                    p_car
     c                   parm                    p_destar
     c*
     c     k_tar01       klist
     c                   kfld                    k_fac
     c*                  kfld                    k_car
     c*
     c     *like         define    tarins        p_fac
     c     *like         define    tarins        k_fac
     c     *like         define    tarcar        p_car
     c     *like         define    tarcar        k_car
     c     *like         define    tardes        p_destar
     c*
     c                   movel(p)  PgmTit@       $PgmTit
     c                   movel     sds_proced    $pgmcod
     c                   eval      w_fac = %trim(k_fac) +' - ' + %trim(p_des)
     c*
     c                   movel     p_tit         W_TIT
     c
     c                   z-add     10            w_next
     c                   eval      *in99 = *off
     c                   eval      *in41='0'
     c                   z-add     0             w_fila            5 0
      *-
     c                   endsr
