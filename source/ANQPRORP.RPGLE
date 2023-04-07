     F* -------------------------------------------------------------------
     F* Programa      : ANQPRORP
     F* Creacion      : 19/09/2000
     F* Funcion       : Parametros
     F* -------------------------------------------------------------------
     HDATFMT(*ISO)
     F
     F* Archivos de Busqueda
     FIIML01    IF   E           K DISK
     FANQPRODDM UF A E           K DISK    USROPN
     F
     C*---------------------------------------------------------------------
     C* Inicio de Programa
     C* --------------------------------------------------------------------
     C                   EXSR      @MAIN
     C                   SETON                                          LR
     C
     C* --------------------------------------------------------------------
     C* Inicializacion de Variables
     C* --------------------------------------------------------------------
     C     *Inzsr        BEGSR
     C
     C     *ENTRY        PLIST
     C                   PARM                    CLASE             2
     C                   PARM                    ITEM             15
     C**** CLAVE DE IIML01
     C
     C     kIIM          klist
     C                   kfld                    ITEM             15
     C*
     C                   ENDSR
     C* --------------------------------------------------------------------
     c     @MAIN         begsr
     C* --------------------------------------------------------------------
     c                   open      anqproddm
     C     KIIM          chain     IPI100IM
     c                   IF        %found
     C     kiim          chain     anqproddm
     C                   IF        %FOUND                                          ----------+
     c                   MOVEL     iprod         PROCPR                                      |
     c                   MOVEL     idesc         PRODPR                                      |
     C                   IF        IMLUOM<>*BLANKS or IMWDUM<>*BLANKS or           --------+ |
     C                             IMHUOM<>*BLANKS or IMWTUM<>*BLANKS                      | |
     C                   IF        IMLUOM='CM'                                     -----+  | |
     c                   Z-ADD     imlong        PROLRG                                 |  | |
     C                   ELSE                                                      ELSE |  | |
     C                   EVAL      PROLRG=IMLONG/1000                                   |  | |
     C                   ENDIF                                                     <----+  | |
     C                   IF        IMWDUM='CM'                                     -----+  | |
     c                   Z-ADD     IMWIDE        PROANC                                 |  | |
     C                   ELSE                                                      ELSE |  | |
     C                   EVAL      PROANC=IMWIDE/1000                                   |  | |
     C                   ENDIF                                                     <----+  | |
     C                   IF        IMHUOM='CM'                                     -----+  | |
     c                   Z-ADD     imhigh        PROALT                                 |  | |
     C                   ELSE                                                      ELSE |  | |
     C                   EVAL      PROALT=Imhigh/1000                                   |  | |
     C                   ENDIF                                                     <----+  | |
     C                   IF        Imwtum='GR'                                    -----+   | |
     c                   z-add     iwght         procdc                                |   | |
     c                   else                                                     ELSE |   | |
     c                   eval      procdc=%DEC(iwght/1000:7:3)                         |   | |
     c                   endif                                                    <----+   | |
     c                   UPDATE    anqpror                                                 | |
     C                   ENDIF                                                    <--------+ |
     c                   else                                                     ELSE       |
     c                   movel     IPROD         procpr                                      |
     c                   movel     idesc         prodpr                                      |
     C                   IF        iclas='PL' OR iclas='P7' OR iclas='PI' OR     --------+   |
     C                             iclas='PF' OR iclas='PZ' OR iclas='P9' OR             |   |
     C                             iclas='PT' OR iclas='P2' OR ICLAS='P8'                |   |
     c                   eval      procmr=%subst(IPROD:1:2)                              |   |
     C                   ELSE                                                            |   |
     C                   MOVEL     'P1'          PROCMR                                  |   |
     C                   ENDIF                                                   <-------+   |
     c                   movel     *blanks       procsc                                      |
     c                   movel     *blanks       proccj                                      |
     c                   z-add     *zeros        proqxc                                      |
     C                   IF        IMLUOM<>*BLANKS OR  IMWDUM<>*BLANKS OR       ---------+   |
     C                             IMHUOM<>*BLANKS OR  IMWTUM<>*BLANKS                   |   |
     C                   IF        IMLUOM='CM'                                  -----+   |   |
     c                   Z-ADD     imlong        PROLRG                              |   |   |
     C                   ELSE                                                        |   |   |
     C                   EVAL      PROLRG=IMLONG/1000                                |   |   |
     C                   ENDIF                                                  <----+   |   |
     C                   IF        IMWDUM='CM'                                   -----+  |   |
     c                   Z-ADD     IMWIDE        PROANC                               |  |   |
     C                   ELSE                                                         |  |   |
     C                   EVAL      PROANC=IMWIDE/1000                                 |  |   |
     C                   ENDIF                                                   <----+  |   |
     C                   IF        IMHUOM='CM'                                    -----+ |   |
     c                   Z-ADD     imhigh        PROALT                                | |   |
     C                   ELSE                                                          | |   |
     C                   EVAL      PROALT=Imhigh/1000                                  | |   |
     C                   ENDIF                                                    <----+ |   |
     C                   IF        Imwtum='GR'                                    -----+ |   |
     c                   z-add     iwght         procdc                                | |   |
     c                   else                                                          | |   |
     c                   eval      procdc=%DEC(iwght/1000:7:3)                         | |   |
     c                   endif                                                    <----+ |   |
     c                   write     ANQPROR                                               |   |
     c                   ENDIF                                                     <-----+   |
     c                   endif                                                     <---------+
     c                   endif                                                     <---------+
     c                   ENDSR
