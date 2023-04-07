     ‚*------------------------------------------------------------------*
     ‚* Selección de Clases para el envio de mail automatico
      * Fecha : Agosto 2004
     ‚*------------------------------------------------------------------*
     ‚* Modificación : Migracion de ERP BPCS v6 - BPCS LX
     ‚* Fecha        : 15 Febrero 2011
     ‚* Autor        : Ricardo Jonathan Mejia Rodriguez
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     fiicl01    if   e           k disk    rename(IPI160IC:rIIC)
     floterr1   uf   e           k disk
     d*
     C*ELIMINA ARCHIVO DE LOTES VENCIDOS
     C     *LOVAL        SETLL     LOTERRR
     C                   READ      LOTERRR                                90
     C                   DOW       NOT *IN90
     C                   DELETE    LOTERRr
     C                   READ      LOTERRR                                90
     C                   ENDDO
     C*---
     C
     c     *loval        setll     rIIC
     c                   read      rIIC                                   99
     c                   dow       not *in99
     C*
     c
     c                   exsr      sSelGrpMail
     c
     C                   MOVEL     *BLANKS       VDESCLA          30
     C                   MOVEL     *BLANKS       VDES             30
     C                   EVAL      VDES = %TRIM(ICLAS) + ' ' + %TRIM(VDESCLA)
     C                   MOVEL     ICDES         VDESCLA
     C                   call      'XCTL108C'
     C                   PARM                    ICLAS
     C                   PARM                    VDESCLA
     C                   PARM      VGRP          vMail             3
     C
     c                   read      rIIC                                   99
     c                   enddo
     c                   eval      *inlr = *on
      *-
      *-
     c
     c*------------------------------------------------------
     c* Selecciona Grupo de Mail
     c*------------------------------------------------------
     c
     c     sSelGrpMail   begsr
     c                   movel     *blanks       vGrp              3
     c                   select
     c                   when      iClas = 'AX' or iclas = 'DX'
     c                             or iClas='FX' or iclas='HE'
     c                             or iclas='MV'
     c                   movel     'EAX'         vGrp
     c                   when      iClas = 'CT'
     c                   movel     'ECT'         vGrp
     c                   when      iClas = 'CX'
     c                   movel     'ECX'         vGrp
     c                   when      iClas = 'ES' or iclas = 'E4'
     c                   movel     'EES'         vGrp
     c                   when      iClas = 'GN' or iclas='GL' or iclas='EJ' or
     c                             iclas='K4' or iclas='JA' or iclas='GP' or
     c                             iclas='MJ' or  iclas='MX'
     c
     c                   movel     'EGL'         vGrp
     c
     c                   when      iclas='PZ' or iclas='PM' or iclas='PI'
     c                   movel     'EIN'         vGrp
     c                   when      iclas='JD' or iclas='PA' or iclas='PB'
     c                   movel     'EJD'         vGrp
     c
     c                   when      iclas='CX' or iclas='C4' or iclas='BY'
     c                             or iclas='MA' or iclas='MB' or iclas='MF'
     c                             or iclas='MQ' or iclas='MS' or iclas='MU'
     c                             or iclas='MG' OR iclas='M4' or iclas='MY'
     c                             or iclas='MX'
     c
     c                   movel     'EMA'         vGrp
     c
     c                   when      iclas='PD'
     c                   movel     'EPD'         vGrp
     c                   when      iclas='PF'
     c                   movel     'EPF'         vGrp
     c                   when      iclas='PH'
     c                   movel     'EPH'         vGrp
     c                   when      iclas='PL'
     c                   movel     'EPL'         vGrp
     c                   when      iclas='JJ'
     c                   movel     'LVJ'         vGrp
     c                   when      iclas='PO'
     c                   movel     'EPO'         vGrp
     c                   when      iclas='PR'
     c                   movel     'EPR'         vGrp
     c                   when      iclas='PS' OR ICLAS='PW'
     c                   movel     'EPS'         vGrp
     c                   when      iclas='PX' OR ICLAS='PO'
     c                   movel     'EPX'         vGrp
     c                   when      iclas='P4' OR ICLAS='P5'
     c                   movel     'EP4'         vGrp
     c                   when      iclas='P6'
     c                   movel     'EP6'         vGrp
     c                   when      iclas='P8'
     c                   movel     'EP8'         vGrp
     c                   when      iclas='P9'
     c                   movel     'EP9'         vGrp
     c                   when      iclas='SA'
     c                   movel     'ESA'         vGrp
     C                   OTHER
     C
     C
     c                   endsl
     c                   endsr
