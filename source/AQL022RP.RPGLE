     ‚*------------------------------------------------------------------*
     ‚*                    YOBEL SCM                                     *
     ‚* Modulo . . : AQL                                                 *
     ‚* Aplicativo : Inicializa patrón de análisis segun Producto        *
     ‚* Fecha  . . : 12/04/2010                                          *
     ‚*------------------------------------------------------------------*
     ‚* Descripci. : Migrar para que soporte - LX                        *
     ‚* Fecha  . . : 18/05/2012                                          *
     ‚* Requerimien: R001192                                             *
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     faqlmap    uf   e           k disk
      *-
     d w_fecha         s               d   datfmt(*iso)
     d                sds
     d   $User               254    263
      *-
     d infds           ds
     d  tecla                369    369
     d  intro          c                   const(X'F1')
      *-
     ‚*--------------------< Rutina inicializa Patron >---------------------
     c     sr_Inicializa begsr
     c*
     c     k_aqlmap      chain     aqlmapr                            40
     c                   dow       *in40='0'
     c                   delete    aqlmapr
     c     k_aqlmap      reade     aqlmapr                                40
     c                   enddo
     c*
     c                   Endsr
     ‚*--------------------< Rutina Inicial de Programa >-----------------------
     c     *inzsr        begsr
     ‚*- Parametros de Entrada                                              ----
     c     *entry        plist
     c     k_fac         parm                    p_fac
     c     k_cli         parm                    p_cli
     c     k_prod        parm                    p_prod
     c     k_tipo        parm                    p_tipo
     c*
     ‚*- Declaración de Variables                                           ----
     c     *like         define    mapfac        p_fac
     c     *like         define    mapfac        k_fac
     c     *like         define    mapcli        p_cli
     c     *like         define    mapcli        k_cli
     c     *like         define    mapite        p_prod
     c     *like         define    mapite        k_prod
     c     *like         define    maptip        p_tipo
     c     *like         define    maptip        k_tipo
     ‚* Estructura de Claves
     c     k_aqlmap      klist
     c                   kfld                    k_fac
     c                   kfld                    k_cli
     c                   kfld                    k_prod
     c                   kfld                    k_tipo
     ‚* Executa Programa
     c                   exsr      sr_Inicializa
     c                   eval      *inlr = *on
      *-
     c                   Endsr
      *-
