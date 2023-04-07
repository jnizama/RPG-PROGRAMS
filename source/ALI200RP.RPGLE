     ‚*------------------------------------------------------------------*
     ‚* Aplicativo : Mantenimiento tabla Parametros de Configuracion FTP *
     ‚* Fecha  . . : 23/01/2008                                          *
     ‚* Nota . . . : Standar propio del Subfile                          *
     ‚*------------------------------------------------------------------*
     fali200dp  cf   e             workstn infds(ds_fileds)
     f                                     sfile(sfl01:w_sln01)
     faliftp    uf a e           k disk
      *
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
     d PgmTit@         c                   const('Yobel SCM')
     d w_next          s              3p 0
     d ww_dat          s              8a
     d k_ubica         s              5p 0
     d x_ubica         s              5p 0
     d w_sln01         s              5p 0
     d t_sln01         s              5p 0
     d sf_inicio       s              2s 0  inz(08)
     d sf_lineas       s              2s 0  inz(12)
      *-
     ‚* Rutina principal de programa                                        ----
     c                   dow       not *in03
     c     w_next        caseq     10            sr_IniciSubf
     c     w_next        caseq     20            sr_CargaSubf
     c     w_next        caseq     30            sr_DisplSubf
     c                   endcs
     c                   Enddo
     c*-
     c                   eval      *inlr = *on
     c                   return
      *
     ‚*--------------------< Rutina de Inicial de Subfile >---------------------
     c     sr_inicisubf  begsr
      *
     c                   eval      d_ubica = 1
     c                   eval      w_sln01 = 0
     c                   eval      *in50   = *off
     c                   eval      *in54   = *on
     c                   write     sflctl01
     c                   eval      *in54   = *off
     c                   eval      w_next  = 20
      *
     c                   endsr
      *
     ‚*--------------------< Rutina de Carga de Subfile >-----------------------
     c     sr_cargasubf  Begsr
      *-
     c                   eval      *in53 = *off
     c                   eval      *in55 = *off
      *
     c     k_aliftp      chain(n)  aliftpr                            81
      *
     c                   dow       not *in81
      *
     c                   clear                   sfl01
     c                   eval      w_fun = alifun
     c                   eval      w_ipc = aliipc
     c                   eval      w_cft = alicft
     c                   eval      w_aft = aliaft
     c                   eval      w_ara = aliara
     c                   eval      sh_car= alicar
      *
     c                   add       1             w_sln01
     c                   eval      *in53     = *on
     c                   eval      t_sln01   = w_sln01
     c                   write     sfl01
      *
     c     k_aliftp      reade(n)  aliftpr                                81
     c                   enddo
      *
     c                   eval      *in55     = *on
     c                   eval      *in52     = *on
     c                   eval      w_next    = 30
     c                   z-add     0             w_linea           5 0
      *
     c                   Endsr
      *
     ‚*--------------------< Rutina de Carga de Subfile >-----------------------
     c     sr_displSubf  Begsr
     c*-
     c                   if        *in53 and (k_ubica > 0)
     c                   if        k_ubica <= t_sln01
     c                   eval      d_ubica = k_ubica
     c                   else
     c                   eval      d_ubica = t_sln01
     c                   endif
     c                   eval      k_ubica = 0
     c                   endif
      *
     c                   write     header
     c                   write     footer
     c                   exfmt     sflctl01
     c                   eval      w_error = *all' '
     c                   move      '0'           *in70
      * Si Salio
     c                   If        *in03 or *in12
     c                   eval      *inlr = *on
     c                   return
     c                   Endif
      *
     c                   Exsr      sr_cursor
     c*-   Opción digitada
     c                   exsr      sr_valopc
      *         Valida Lo ingresado
     c                   if        x_ubica>0
     c                   if        x_ubica<t_sln01
     c                   eval      k_ubica=x_ubica
     c                   else
     c                   eval      k_ubica=t_sln01
     c                   endif
     c                   else
     c                   eval      k_ubica=fila
     c                   endif
      *
     c                   Select
     c                   when      *in06
     c                   exsr      sr_add
     c                   eval      w_next = 10
     c                   when      *in18
     c   53              exsr      sr_ret_sf1
     c                   when      *in19
     c   53              exsr      sr_avp_sf1
     c                   other
     c                   exsr      sr_UbicaCursor
     c                   eval      w_next = 10
     c                   Endsl
      *
     c     fin_displ     Endsr
     ‚*----------------< Adicionar Parámetros  >-------------------------------
     c     sr_add        BegSr
     c                   exsr      sr_movfmt
     c                   Dow       *in12 = '0'
     c                   Exfmt     Win01
     c   12              leave
     c                   if        *in10 = '1'
     c                   exsr      sr_graba
     c                   leave
     c                   endif
     c                   EndDo
     c                   eval      *in12='0'
     c                   eval      *in10='0'
     c                   eval      *in06='0'
     c                   EndSr
     c
     ‚*----------------< Valida Opciones Ingresadas >---------------------------
     c     sr_valopc     Begsr
     c                   eval      w_fila = 0
     c                   eval      w_error = *all' '
     c                   eval      *in99 = *off
     c                   if        (w_linea > 0) and (w_linea <= t_sln01)
     c     w_linea       chain     sfl01                              81
     c                   eval      *in60 = *off
     c                   update    sfl01
     c                   endif
      *-
     c                   eval      w_linea = 0
      *-
     c     1             do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              81
     c                   eval      *in60   = *off
     c                   if        (w_opc <> '2') and
     c                             (w_opc<> '5') and
     c                             (w_opc<> '4') and
     c                             (w_opc<> ' ')
     c                   eval      w_linea = w_sln01
     c                   eval      *in60   = *on
     c                   eval      w_error = 'Valor ingresado (' + w_opc +
     c                                       ') es incorrecto'
     c                   eval      *in99   = *on
     c                   endif
     c                   update    sfl01
     c                   if        *in99
     c                   eval      d_ubica = w_sln01
     c                   leave
     c                   endif
     c                   enddo
     c*-
     c                   if        not *in99
     c     1             do        t_sln01       w_sln01
     c     w_sln01       chain     sfl01                              81
     c*
     c                   clear                   Win02
     c                   select
     c                   when      w_opc = '5'
     c                   eval      *in25='1'
     c                   eval      w_tit = 'CONSULTAR'
     c                   exsr      sr_move
     c                   DoW       *In12='0'
     c                   Exfmt     Win02
     c   12              Leave
     c                   EndDo
     c                   eval      *in12='0'
     c                   eval      *in98='1'
     c                   z-add     w_sln01       w_fila            5 0
     c                   eval      *in25='0'
     c
  mvsc                   when      w_opc = '2'
     c                   eval      *in25='0'
     c                   eval      w_tit = 'ACTUALIZAR'
     c                   exsr      sr_move
     c                   DoW       *In12='0'
     c                   Exfmt     Win02
     c   12              Leave
     c                   if        *in10 = '1'
     c     k_aliftp2     chain     aliftpr
     c                   if        %found()
     c                   eval      aliipc = v_ipc
     c                   eval      alicft = v_cft
     c                   eval      aliaft = v_aft
     c                   eval      alicmd = v_cmd
     c                   eval      aliipa = v_ipa
     c                   eval      alicar = v_car
     c                   eval      aliara = v_ara
     c                   update    aliftpr
     c                   leave
     c                   endif
     c                   endif
     c                   EndDo
     c                   eval      *in10='0'
     c                   eval      *in12='0'
     c                   eval      *in98='1'
     c                   z-add     w_sln01       w_fila            5 0
     c                   when      w_opc = '4'
     c                   eval      *in25='1'
     c                   eval      w_tit = 'ELIMINAR'
     c                   exsr      sr_move
     c                   DoW       *In12='0'
     c                   Exfmt     Win02
     c   12              Leave
     c                   if        *in10 = '1'
     c     k_aliftp2     chain     aliftpr
     c                   if        %found()
     c                   delete    aliftpr
     c                   leave
     c                   endif
     c                   endif
     c                   EndDo
     c                   eval      *in12='0'
     c                   eval      *in98='1'
     c                   z-add     w_sln01       w_fila            5 0
     c                   eval      *in25='0'
     c
     c                   when      w_opc = '9'
     c                   endsl
     c*-
     c                   eval      w_opc  = ' '
     c                   update    sfl01
     c                   enddo
     c*-
     c                   if        w_linea > 0
     c                   eval      d_ubica = w_linea
     c                   else
     c                   eval      d_ubica = fila
     c                   endif
     c                   endif
     c                   endsr
      *-
     ‚*----------------< Mover datos a windows >-------------------------------
     c     sr_movfmt     BegSr
     c                   eval      v_cia = w_cia
     c                   eval      v_des = w_des
     c                   eval      v_ipc = '172.30.32.32'
     c                   eval      v_cft = w_cft
     c                   eval      v_aft = 'ejemplo.txt'
     c                   eval      v_cmd = 'GET'
     c                   eval      v_ipa = '172.30.45.90'
     c                   eval      v_ara = 'TXTXXX        '
     c                   eval      v_car = sh_car
     c                   eval      v_far = 'ASCII'
     c                   eval      v_fun = w_fun
     c                   Endsr
     ‚*----------------< Mover datos a windows >-------------------------------
     c     sr_move       BegSr
     c                   movel     w_fun         k_fun
     c     k_aliftp2     chain     aliftpr
     c                   if        %found()
     c                   eval      v_cia = alicia
     c                   eval      v_des = w_des
     c                   eval      v_ipc = aliipc
     c                   eval      v_cft = alicft
     c                   eval      v_aft = aliaft
     c                   eval      v_cmd = alicmd
     c                   eval      v_ipa = aliipa
     c                   eval      v_ara = aliara
     c                   eval      v_car = alicar
     c                   eval      v_far = alifar
     c                   eval      v_fun = alifun
     c                   endif
     c                   EndSr
     ‚*----------------< Mover datos a Actualizar >-------------------------------
     c     sr_movact     BegSr
     c*                  eval      v_cia = w_cia
     c*                  eval      v_des = w_des
     c*                  eval      v_ipc = w_ipc
     c*                  eval      v_cft = w_cft
     c*                  eval      v_aft = w_aft
     c*                  eval      v_cmd = 'GET'
     c*                  eval      v_ipa = '172.30.45.90'
     c*                  eval      v_ara = w_ara
     c*                  eval      v_car = sh_car
     c*                  eval      v_far = 'ASCII'
     c*                  eval      v_fun = w_fun
     c                   EndSr
     ‚*----------------< Graba datos Configurados >-------------------------------
     c     sr_graba      BegSr
     c                   eval      alicia = v_cia
     c                   eval      alifun = v_fun
     c                   eval      aliipc = v_ipc
     c                   eval      alicft = v_cft
     c                   eval      alifmt = 'NAMEFMT 1'
     c                   eval      alifar = v_far
     c                   eval      aliuft = 'alicorp'
     c                   eval      alipft = 'acli1ic3'
     c                   eval      aliaft = v_aft
     c                   eval      alicmd = v_cmd
     c                   eval      aliipa = v_ipa
     c                   eval      alicar = sh_car
     c                   eval      aliara = v_ara
     c
     c                   Write     aliftpr
     c
     c                   EndSr
     ‚*----------------< Ubica Cursor por Enter >-------------------------------
     c     sr_UbicaCursorBegsr
      *
     c                   eval      w_opc  = ' '
     c                   eval      k_ubica=fila
     c                   if        (x_ubica>0) and (x_ubica<t_sln01)
     c     x_ubica       chain     sfl01
     c                   move      '0'           *in70
     c                   update    sfl01
     c                   endif
     c                   eval      x_ubica=k_ubica
     c
     c                   if        k_ubica > 0
     c     k_ubica       chain     sfl01                              81
     c                   move      '1'           *in70
     c                   update    sfl01
     c                   eval      d_ubica=k_ubica
     c                   endif
      *-
     c                   Endsr
     ‚*----------------< Avanzar Imagen >---------------------------------------
     c     sr_avp_sf1    Begsr
      *-
     c                   eval      d_ubica = fila
     c                   if        *in55
     c                   eval      w_error = 'Se pretende avanzar de pag-
     c                             ina y no existen mas datos'
     c                   endif
      *-
     c                   Endsr
      *-
     ‚*----------------< Retroceder de imagen >---------------------------------
     c     sr_Ret_sf1    Begsr
      *-
     c                   eval      d_ubica = fila
     c                   if        fila   <= sf_lineas
     c                   eval      w_error = 'Se pretende retroceder de pag-
     c                             ina y estamos al inicio de los datos'
     c                   endif
      *-
     c                   Endsr
      *-
     ‚*----------------< Posicion Relativa de cursor >--------------------------
     c     sr_cursor     Begsr
      *-
     c     cursor        div       256           fila              4 0
     c                   mvr                     columna           4 0
     c*
     c                   select
     c                   when      fila < sf_inicio
     c                   eval      fila = sf_inicio
     c                   when      fila > (sf_inicio + sf_lineas)
     c                   eval      fila = (sf_inicio + sf_lineas - 1)
     c                   endsl
     c*-
     c                   eval      fila = fila + rrelat - sf_inicio
     c                   if        fila > t_sln01
     c                   eval      fila = t_sln01
     c                   endif
      *
     c                   endsr
      *
     ‚*--------------------< Rutina Inicial de Programa >-----------------------
     c     *inzsr        begsr
      *
     ‚* Parametros de Entrada
     c     *entry        plist
     c                   parm                    p_cia             3
     c
     c                   eval      k_codcli = p_cia
     c                   eval      w_cia    = p_cia
     ‚* Claves de acceso
     c     k_aliftp      klist
     c                   kfld                    k_codcli          3            ALI = Alicorp
      *
     c     k_aliftp2     klist
     c                   kfld                    w_cia                          ALI = Alicorp
     c                   kfld                    k_fun
      *
     c     *like         define    alifun        k_fun
     c*    *like         define    item          k_item
     c
     ‚* Datos de Cabecera
     c                   Select
     c                   When      w_cia = 'ALI'
     c                   eval      w_des = '109383 - ALICORP'
     c                   When      w_cia = 'XXX'                                Futuras Cias.
     c                   eval      w_des = ' '
     c                   EndSl
     ‚* Primera Lectura                                                     ----
     c                   movel(p)  PgmTit@       $PgmTit
     c                   movel     sds_proced    $pgmcod
     c                   z-add     10            w_next
     c                   z-add     0             w_fila            5 0
      *-
     c                   Endsr
