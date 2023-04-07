      *****************************************************
      * Módulo   : AIP                                    *
      * Programa : Ingreso de Nuevas Etiquetas EAN13      *
      * Fecha    : 24 Junio 2004                          *
      *****************************************************
     hdatfmt(*iso)
     faip2004d  cf   e             Workstn
     fetqsis    UF A E           k disk
     FIIML01    IF   E           K disk
     FIIcL01    IF   E           K disk
      *
     d                sds
     dnompgm                   1     10
     dcabusr                 254    263
      *--------------------< Proceso Principal >-------------------------*
     c                   select
     c                   when      vtip = 'A'
     c                   exsr      sIngreso
     c                   when      vtip = 'M'
     c                   exsr      sCargaDato
     c                   exsr      sIngreso
     c                   endsl
     c
     c                   eval      *inlr='1'
      *--------------------< Proceso Principal >-------------------------*
     c     sIngreso      begsr
     c                   dow       not *in03 and not *in12
     c     lbl1          tag
     c                   exfmt     RMOD01
     c                   if        *in12 or *in03
     c                   leave
     c                   endif
     c                   exsr      sValida
     c                   if        not *in90
     c                   exsr      sGrabaSacador
     c                   endif
     c                   enddo
     c                   endsr
      *----------------------< Valida Rango >-------------------------*
     c     sValida       begsr
     c
     c                   movel(p)  *blanks       MSGCXC
     c                   move      '0'           *in90
     c                   movea     '00'          *in(80)
     c                   setoff                                       79
     c
     c* Valida Canal
     c                   if        not *in90 and vsac=' '
     c                   eval      msgcxc ='Producto No Puede Ser Blanco'
     c                   move      '1'           *in90
     c                   move      '1'           *in79
     c                   endif
     c* Producto
     C                   If        not *in90 and vtip='A'
     c     vsac          chain     IPI100IM
     c                   if        not %found
     c                   eval      msgcxc='Producto No Existe'
     c                   move      '1'           *in90
     c                   move      '1'           *in79
     C                   endif
     C                   Endif
     c* Valida Duplicado
     c                   if        not *in90 and vtip = 'A'
     c     kEtqsis       chain     etqsisr
     c                   if        %found
     c                   eval      msgcxc='Clase/Producto ya Existen'
     c                   move      '1'           *in90
     c                   move      '1'           *in79
     c                   endif
     c                   endif
     c* Valida Unidades
     c                   if        not *in90 and undxep=0
     c                   eval      msgcxc='Unidades En Cero'
     c                   move      '1'           *in90
     c                   move      '1'           *in81
     c                   endif
     c                   endsr
     c

      *----------------------< Valida Producto >-------------------------*

     c     sCargaDato    begsr
     c     kEtqsis       chain(n)  etqsisr
     c                   if        %found
     c                   eval      *in75='1'
     c                   z-add     etquxep       undxep
     c                   endif
     c                   endsr

      *----------------------< Valida Producto >-------------------------*

     c     sGrabaSacador begsr
     c
     c                   select
     c                   when      vtip='A'
     c     kEtqsis       chain     etqsisr
     c                   if        not %found
     c                   movel(p)  vcia          etqcla
     c                   movel(p)  vsac          etqpro
     c                   z-add     undxep        ETQUXEP
     c                   movel     cabusr        etqUSE
     c                   movel     *date         etqFEC
     c                   time                    etqHOR
     c                   write     etqsisr
     c                   eval      vflg='1'
     c                   endif
     c
     c                   when      vtip='M'
     c     kEtqsis       chain     etqsisr
     c                   if        %found
     c                   z-add     undxep        ETQUXEP
     c                   movel     cabusr        etqUSE
     c                   movel     *date         etqFEC
     c                   time                    etqHOR
     c                   update    etqsisr
     c                   eval      vflg='1'
     c                   endif
     c                   endsl
     c
     c                   eval      *in03 = '1'
     c                   endsr

      *-------------------< Subrutina Inicial >-------------------------*

     c     *Inzsr        begsr
     c     *Entry        plist
     c                   parm                    vcia              2
     c                   parm                    vsac             15
     c                   parm                    vtip              1
     c                   parm                    vflg              1
     c
     c     kEtqsis       klist
     c                   kfld                    vcia
     c                   kfld                    vsac
     c
     c* Clase
     c     vcia          chain     IPI160IC
     c                   if        %found
     c                   movel     ICDES         vdcia
     c                   else
     c                   movel     *blanks       vdcia
     c                   endif
     c* Producto
     c     vsac          chain     IPI100IM
     C                   if        %found()
     c                   movel(p)  idesc         vdsac
     C                   endif
     C                   EVAL      TITULO='MANTENIMIENTO DE EAN13'
     c                   endsr
