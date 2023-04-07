
     ‚* Define archivos adicionales.
      * File donde se guardan las ordenes con fecha de cierre
     FIIM       IF   E           K DISK

     ‚* Define Estructura Externa del Archivo.
     D $RecDat       E DS                  EXTNAME( IIM )
     D                                     BASED( xRecDat )
     D                                     prefix( IIM_ )


     ‚* Definicion de variables GLOBALES DE Usuario.
     D*  ---< Insertar aqui las lineas >---


     ‚* Inserta la parte fija de  DECLARACION del TRIGGER.
     D/COPY STDAPL/QRPGLESRC,TRGDECCPY


     ‚* Parte del PROGRAMA global del USUARIO.
     C

      * Define archivo de Mensajes
     C                   eval      $MsgFile = 'SALMSGF'
     C                   eval      $MsgLib = 'BPCPGM'

     ‚* Inserta la parte fija de PROGRAMA del TRIGGER.
     C/COPY STDAPL/QRPGLESRC,TRGPGMCPY


     ‚* Define tus informes y salidas.
     O*  ---< Insertar aqui las lineas >---


     ‚*-------------------------------------------------------------------------
      * ƒpInsBfr:€Accion de ANTES de INSERTAR el registro al archivo.
     ‚*-------------------------------------------------------------------------

     P pInsBfr         B
     D pInsBfr         PI
     D  xDat                           *   VALUE
     C
     C                   eval      xRecDat = xDat
     C
     P pInsBfr         E


     ‚*-------------------------------------------------------------------------
      * ƒpInsAft:€Accion de DESPUES de INSERTAR el registro al archivo.
     ‚*-------------------------------------------------------------------------

     P pInsAft         B
     D pInsAft         PI
     D  xDat                           *   VALUE
     C
     C                   eval      xRecDat = xDat
     c
     c                   Call      'ANQPROCL'
     c                   parm      IIM_ICLAS     xClas             2
     c                   parm      IIM_IPROD     xProd            15
     C
     P pInsAft         E


     ‚*-------------------------------------------------------------------------
      * ƒpDltBfr:€Accion de ANTES de ELIMINAR el registro al archivo.
     ‚*-------------------------------------------------------------------------

     P pDltBfr         B
     D pDltBfr         PI
     D  xDat                           *   VALUE
     C
     C                   eval      xRecDat = xDat
     C
     P pDltBfr         E


     ‚*-------------------------------------------------------------------------
      * ƒpDltAft:€Accion de DESPUES de ELIMINAR el registro al archivo.
     ‚*-------------------------------------------------------------------------

     P pDltAft         B
     D pDltAft         PI
     D  xDat                           *   VALUE
     C
     C                   eval      xRecDat = xDat
     C
     P pDltAft         E


     ‚*-------------------------------------------------------------------------
      * ƒpUpdBfr:€Accion de ANTES de ACTUALIZAR el registro al archivo.
     ‚*-------------------------------------------------------------------------

     P pUpdBfr         B
     D pUpdBfr         PI
     D  xDatAnt                        *   value
     D  xDatDes                        *   value
     C                   eval      xRecDat = xDatAnt
     C                   eval      xRecDat = xDatDes
     P pUpdBfr         E


     ‚*-------------------------------------------------------------------------
      * ƒpUpdAft:€Accion de DESPUES de ACTUALIZAR el registro al archivo.
     ‚*-------------------------------------------------------------------------

     P pUpdAft         B
     D pUpdAft         PI
     D  xDatAnt                        *   VALUE
     D  xDatDes                        *   VALUE
     D Clase           S              2A
     D Producto        S             15A
     D ximlong         S             11S 3
     D ximwide         S             11S 3
     D ximhigh         S             11S 3
     D ximnnwu         S             13S 3
     C
     ** Antes
     C                   eval      xRecDat = xDatAnt
     C                   eval      Clase    = IIM_ICLAS
     C                   eval      Producto = IIM_IPROD
     c                   z-add     IIM_IMLONG    ximlong
     c                   z-add     IIM_IMWIDE    ximwide
     c                   z-add     IIM_IMHIGH    ximhigh
     c                   z-add     IIM_IMNNWU    ximnnwu
     c
     ** Despues
     C                   eval      xRecDat = xDatDes
     c                   If        IIM_IMLONG <> ximlong or
     c                             IIM_IMWIDE <> ximwide or
     c                             IIM_IMHIGH <> ximhigh or
     c                             IIM_IMNNWU <> ximnnwu
     c                   Call      'ANQPROCL'
     c                   parm                    Clase
     c                   parm                    Producto
     c                   endif
     C
     P pUpdAft         E
