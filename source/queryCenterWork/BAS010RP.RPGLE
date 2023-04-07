      *************************************************************
      *  Función    : Consultar Centros de Trabajo x Instalación  *
      *                                                           *
      *                                                           *
      *  Fecha      : 20/02/2007                                  *
      *                                                           *
      *------------------------------------------------------------
      *  Modificado : xxxxxxxxxxxxxxxxxxxxx | Fecha : xx/xx/xxxxx *
      *************************************************************
     fbas010dp  cf   e             workstn infds(ds_fileds)
     f                                     sfile(sfl01:w_sln01)
     flwkl05    if   e           k disk    rename(ipl100wk:lwkl05r)
     d sds_estado     sds
     d  sds_proced       *proc
     d  sds_usuari           254    263

     d ds_fileds       ds
     d  cursor               370    371b 0
     d  rrelat               378    379b 0

     d cadena          c                   ' 0123456789'
     d nlineas         c                   08
     d w_next          s              3p 0
     d w_sln01         s              5s 0
     d t_sln01         s              5s 0
     d sf_inicio       s              2s 0 inz(11)
     d sf_lineas       s              2s 0 inz(08)
      *
     d a_centro        s              6s 0 dim(100) inz(0)

     c                   dow       not *in12
     c     w_next        caseq     10            sr_inicisubf
     c     w_next        caseq     20            sr_cargasubf
     c     w_next        caseq     30            sr_displsubf
     c                   endcs
     c                   enddo

     ‚* ---------------------                                               ----
     ‚* Inicializa Sub- File |                                              ----
     ‚* ---------------------                                               ----
     c     sr_inicisubf  begsr
      *
      * Parametros de inicializacion
     c                   eval      w_sln01 = 0
     c                   eval      d_ubica = 1
     c                   eval      *in51 = *off
      *
      * Inicializa Sub-File-1
     c                   eval      *in54 = *on
     c                   write     sflctl01
     c                   eval      *in54 = *off
      *
      * Sgte Secuencia
     c                   eval      w_next = 20
     c                   endsr
     ‚*----------------------                                               ----
     ‚* Carga del Sub - File |                                              ----
     ‚*----------------------                                               ----
     c     sr_cargasubf  begsr
      *
     c                   eval      *in53 = *off
     c                   eval      *in55 = *off
     c     k_lwkl05      chain     lwkl05r                            81
      *
     c                   dow       not *in81
     c                   clear                   sfl01
      *
     c                   eval      w_opc      = ''
     c                   eval      w_centro   = wwrkc
     c                   eval      w_dcentro  = wdesc
      *
     c  n53              eval      *in53 = *on
     c                   add       1             w_sln01
     c                   eval      t_sln01 = w_sln01
     c                   write     sfl01
     c     k_lwkl05      reade     lwkl05r                                81
     c                   enddo
      *
      * Salida del subfile
     c                   eval      *in52 = *on
     c   81              eval      *in55 = *on
      *
     c                   eval      w_next = 30
      *
     c                   endsr
     ‚* -------------------                                                 ----
     ‚* Displaya  Sub File |                                                ----
     ‚* -------------------                                                 ----
     c     sr_displsubf  begsr
      *
      * Displaya SubFile
     c                   write     win01
     c                   write     footer
     c                   exfmt     sflctl01
      *
      * Anterior
     c                   if        *in12
     c*                  eval      p_retorno = '12'
     c                   eval      *inlr = *on
     c                   return
     c                   endif
      *
      * Posicion Relativa de cursor
     c                   exsr      sr_cursor
      *
      * Validar Opciones
     c   53              exsr      sr_val_opc
      *
      * Control de Funciones
     c  n99              select
      *
      * F05: Actualiza info
     c                   when      *in05
     c                   eval      *in05 =*off
     c                   exsr      *inzsr
      *
      * F13: Selecciona todos
     c                   when      *in13
     c   53              exsr      sr_seleccion
      *
      * F19: Retroceder pagina
     c                   when      *in19
     c   53              exsr      sr_ret_sf1
      *
      * F18: Avanzar pagina
     c                   when      *in18
     c   53              exsr      sr_avp_sf1
      *
      * Intro: Posicionamiento
     c                   other
     c                   eval      d_ubica = fila
     c                   endsl
      *
     c                   endsr
      *--------------------------------
      * Selecciona todos los Registros |
      *--------------------------------
     c     sr_seleccion  begsr
      *
     c                   eval      *in13 = *off
      * Control Principal
     c                   do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              21
     c                   eval      w_opc = '1'
     c                   update    sfl01
     c                   enddo
      *
     c                   endsr
      *------------------------------
      * Valida Selección de Opciones |
      *------------------------------
     c     sr_val_opc    begsr
      *
      * Inicializa
     c                   eval      w_error = *all' '
     c                   eval      *in99   = *off
     c                   move      *zeros        w_tot_opc         2 0
      *
      * Control Principal
     c                   do        t_sln01       w_sln01
      *
      * Opción No Válida
     c     w_sln01       chain     sfl01                              21
      *
     c                   eval      *in61 = *off
      *
     c                   if        w_opc <> '1'  and
     c                             w_opc <> ' '
     c                   eval      *in61 = *on
     c  n99              eval      w_error = 'Opción Seleccionada ' +
     c                                        w_opc +
     c                                       ' No es Valida'
     c  n99              eval      *in99 = *on
     c                   endif
      * Actualiza Subfile
     c                   update    sfl01
     c                   enddo
      * Almacena Datos
     c                   if        not *in99
      *
     c                   clear                   a_centro
     c                   z-add     0             w_x               5 0
     c                   do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              21
     c                   if        w_opc = '1'
     c                   add       1             w_x
     c                   eval      a_centro(w_x) = w_centro
     c                   endif
     c                   enddo
     c                   eval      p_qty = w_x
      *
     c                   endif
      *
     c                   endsr
      *----------------------
      * F18: Avanzar Imagen |
      *----------------------
     c     sr_avp_sf1    begsr
      *
     c                   eval      d_ubica = fila
      * Estamos en la Ultima imagen
     c                   if        *in55
     c                   eval      w_error = 'Avanzar de pagina y no existen ma-
     c                             s datos'
     c                   eval      w_next = 30
     c                   endif
      *
     c                   endsr
     ‚*-------------------                                                  ----
     ‚* Retroceder Imagen |                                                 ----
     ‚*-------------------                                                  ----
     c     sr_ret_sf1    begsr
      *
     c                   eval      d_ubica = fila
     c                   if        fila   <= sf_lineas
     c                   eval      w_error = 'Se pretende retroceder de pag-
     c                             ina y estamos al inicio de los datos'
     c                   endif
      *
     c                   endsr
      *-------------------------------
      *  Posición Relativa del Cursor |
      *-------------------------------
     c     sr_cursor     begsr
      *
      * Posicion Actual
     c     cursor        div       256           fila              4 0
     c                   mvr                     columna           4 0
      *
      * Si fila es menor al 1er rigistro del subfile por pagina
     c                   select
     c                   when      fila < sf_inicio
     c                   eval      fila = sf_inicio
      *
      * Si fila es mayor a la tamaño de la pagina del subfile
     c                   when      fila > (sf_inicio + sf_lineas)
     c                   eval      fila = (sf_inicio + sf_lineas - 1)
     c                   endsl
      *
      * Posición Relativa en que estuvo el Cursor
     c                   eval      fila = fila + rrelat - sf_inicio
      *
      * Si Posici¢n Relativa en que estuvo el Cursor es mayor al tamaño del Subfile
     c                   if        fila > t_sln01
     c                   eval      fila = t_sln01
     c                   endif
      *
     c                   endsr
      *--------------------
      *  Rutinas Iniciales |
      *--------------------
     c     *inzsr        begsr
      *
      * Parametros a ser Analizados
     c     *entry        plist
      *
     c                   parm                    p_fac
     c                   parm                    p_qty             3 0
      *
     c     *like         define    wfac          p_fac
      *
     c     k_lwkl05      klist
     c                   kfld                    p_fac
      *
     c   81              eval      *in99 = *on
     c                   eval      w_next = 10
      *
     c                   endsr

