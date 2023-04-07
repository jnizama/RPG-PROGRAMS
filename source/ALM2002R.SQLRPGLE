     ‚*------------------------------------------------------------------*
     ‚*                    GRUPO YOBEL                                   *
     ‚* Modulo:      Comercialización                                    *
     ‚* Aplicativo:  Crea Tabla de mercaderia vencida a 9 y 12 meses     *
     ‚* Fecha:       16/12/2002                                          *
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     fLotven    if a e           k disk
      *
     ‚* Variables de Programa                                               ----
     d w_fecha         s               d   datfmt(*iso)
     d w_saldo         s             11s 3 inz(0)
      *
     ‚* Variables de SQL                                                    ----
     d Xlprod          s             15a
     d Xlexdt          s              8s 0
     d Xidesc          s             30a
     d Xiclas          s              2a
     d Xsaflg          s              1s 0
     d Xllot           s             10a
     d Xlwhs           s              2a
     d Xlloc           s              6a
     d Xlopb           s             11s 3
     d Xlissu          s             11s 3
     d Xladju          s             11s 3
     d Xlrct           s             11s 3
     ‚*--------------------< Rutina Inicial de Programa >-----------------------
     c     *inzsr        begsr
      *-
     ‚* Formato de claves para tabla de mercaderia                          ----
     c     k_merc        klist
     c                   kfld                    Xllot
     c                   kfld                    Xlprod
      *-
     ‚* fecha actual                                                        ----
     c                   move      *date         w_fecha
     c                   move      w_fecha       w_fecI            8 0
     ‚* 09 meses                                                            ----
     c                   adddur    9:*m          w_fecha
     c                   move      w_fecha       w_fecF            8 0
     ‚* 12 meses                                                            ----
     c                   adddur    4:*m          w_fecha
     c                   move      w_fecha       w_fecX            8 0
      *-
     ‚* Inicializa tabla de mercaderia vencida
     c/EXEC SQL
     c+ delete from bpcdta/lotven
     c/end-exec
      *-
     ‚* Crea cursor
     c/exec sql
     c+ declare C1 cursor for select
     c+ a.Lprod, a.Lexdt, b.Idesc, b.iclas, b.saflg, c.Llot,                    Recupera rango de -
     c+ c.Lwhs,  c.Lloc,  c.Lopb,  c.Lissu, c.Ladju, c.Lrct                     Recupera rango de -
     c+ from   ilnl01 a inner join iiml01 b on (a.lprod = b.iprod)              Recupera rango de -
     c+ inner join ilil01 c  on (a.lprod = c.lprod and a.llot = c.llot)         Recupera rango de -
     c+ inner join iwm d on (c.lwhs = d.lwhs )                                  Recupera rango de -
     c+ where ((b.iclas in ('P6','PO') and (a.lexdt  between :w_fecI and        pedidos
     c+          :w_fecX)) or (b.iclas not in ('P6','PO') and                   pedidos
     c+                       a.lexdt  between :w_fecI and :w_fecF))            pedidos
     c+ and   (b.saflg = 2 )                                                    pedidos
     c+ and   (d.lnetw = 'Y' or d.lalwh = 'Y')                                  pedidos
     c+ order by b.iclas,c.llot, a.Lprod, a.lexdt
     c/end-exec
      *
     ‚* Open a Cursor                                                       ----
     c/exec sql
     c+ open C1
     c/end-exec
      *-
     ‚* Llena File de Trabajo                                               ----
     c                   exsr      sr_Ini
     ‚* Close Cursor                                                        ----
     c/exec sql
     c+ close C1
     c/end-exec
      *-
     ‚* Fin-Programa                                                        ----
     c*Actualiza Favel
     c                   exsr      sUpdFlgFavel
     c                   exsr      sDltJohnson
     c                   exsr      sUpdMatPrim
     c*
     c*
     c                   eval      *inlr = *on
      *-
     c                   Endsr
      *-
     ‚*--------------------< Rutina de Principal de Grabación de Datos >--------
     c     sr_Ini        Begsr
     c*-
     c/exec sql
     c+ fetch C1 into
     c+ :Xlprod, :Xlexdt, :Xidesc, :Xiclas, :Xsaflg, :Xllot,                    Recupera rango de -
     c+ :Xlwhs,  :Xlloc,  :Xlopb,  :Xlissu, :Xladju, :Xlrct                     Recupera rango de -
     c/end-exec
     c                   dow       (sqlcod <> 100)
     c*-
     c                   eval      w_saldo = Xlopb - Xlissu + Xladju
     c                                              + Xlrct
     c*-
     c*-   si no es un bien
     c                   if        (w_saldo <> 0)
     c
     c     k_merc        chain     lotvenr                            81
     c                   if        *in81
     c                   clear                   lotvenr
     c                   move      Xlprod        producto
     c                   moveL     Xidesc        descripc
     c                   move      Xiclas        clase
     c                   move      Xllot         lote
     c                   move      Xlexdt        fechaven
     c                   move      Xlwhs         almacen
     c                   move      Xlloc         ubicacion
     c                   eval      saldo   = w_saldo
     c*
     c                   write     lotvenr
     c                   endif
     c                   endif
     c/exec sql
     c+ fetch C1 into
     c+ :Xlprod, :Xlexdt, :Xidesc, :Xiclas, :Xsaflg, :Xllot,                    Recupera rango de -
     c+ :Xlwhs,  :Xlloc,  :Xlopb,  :Xlissu, :Xladju, :Xlrct                     Recupera rango de -
     c/end-exec
     c*-
     c                   enddo
     c*-
     c                   Endsr
     ‚*--------------------< Actualiza Flag DE FAVEL        >--------
     c     sUpdFlgFavel  begsr
     ‚* ACTUALIZA
     c/exec sql
     c+ UPDATE BPCDTA/LOTVEN  SET  FLG = 'F'
     c+  where  clase  in ('PA','PB','PD','PS','PX','P4','P6','PC','PW',
     C+               'PY','SX','PO','AX','NX','DX','HE','PH','LX','CE',
     C+                'P5','P9','FX')
     c/end-exec
     c                   endsr
     c
     ‚*--------------------< ELIMINA JOHNSON                >--------
     c     sDltJohnson   begsr
     c/EXEC SQL
     c+ delete from bpcdta/lotven where clase = 'JJ'
     c/end-exec
     c                   endsr
     ‚*--------------------< Actualiza Flag DE MATERIAS PRIMAS>--------
     c     sUpdMatPrim   begsr
     ‚* ACTUALIZA
     c/exec sql
     c+ UPDATE BPCDTA/LOTVEN  SET  FLG = 'M'
     c+  where  clase  like 'M%'
     c/end-exec
     c                   endsr
     c
