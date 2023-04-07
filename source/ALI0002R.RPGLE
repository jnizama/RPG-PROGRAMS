     H DATFMT(*ISO) DATEDIT(*YMD-)
     ‚**********************************************************************
     ‚* Modulo:      Proyecto de Laptop Alicorp                          *
     ‚* Aplicativo:  Genera sentencias para accesar al FTP               *
     ‚*              y obtener archivo texto de Fórmulas                 *
     ‚* Fecha:       27/12/2007                                          *
     ‚**********************************************************************
     fLAPCMD    o  a f  120        disk
     ‚*---------------------------------------------------------------------
     d                sds
     dvUser                  254    263
     ‚* WRTSRC en texto
     d SRCDS           DS                  INZ
     d  SRCSEQ                 1      6  0
     d  SRCDAT                 7     12  0
     d  SRCDTA                13    120
      *
     c
     ‚*------------------------------------------------------------------
     c     *entry        plist                                                  Ejemplo:
     c                   parm                    p_fac             3            Facility
     c                   parm                    p_cia             3            ALI
     c                   parm                    p_fun            15            RECEPFORM
     c                   parm                    p_ipc            15            172.30.32.32
     c                   parm                    p_cft            70            Carpeta en FTP
     c                   parm                    p_fmt            20            NAMEFMT 1
     c                   parm                    p_far            10            ASCII
     c                   parm                    p_uft            20            usuario ftp
     c                   parm                    p_pft            20            clave ftp
     c                   parm                    p_aft            25            Archivo:Formulas.txt
     c                   parm                    p_cmd            20            Get/Put
     c                   parm                    p_ipa            15            IP AS400
     c                   parm                    p_car           100            Carpeta AS400
     c                   parm                    p_ara            10            Archivo recep MANTXT
      *                                                                         en el as400
     c                   movel     p_fmt         w_fmt            20
     c                   movel     p_far         w_far            10
     c                   movel     p_uft         w_uft            20
     c                   movel     p_pft         w_pft            20
     c                   movel     p_car         w_car           100
     c                   movel     p_cmd         w_cmd            20
     c                   movel     p_aft         w_aft            25
     c                   movel     p_ara         w_ara            10
     c                   movel     p_cft         w_cft            70
     c
     c
     c                   exsr      sproceso
     c                   eval      *inlr = *on
     ‚*---------------------------------------------------------------------
      *šInicia Paso de Parametros del FTP
     ‚*---------------------------------------------------------------------
     c     sProceso      begsr
      /Free
       //Conectarse Usuario y Clave FTP
        SRCDTA = %trim(w_uft)+' '+%trim(w_pft);
        exsr WRTSRC;

       //Formato de archivo a envia (NA=formato 1)
        SRCDTA = w_fmt;
        exsr WRTSRC;

       //Formato de archivo que envia (BI=Binario , ASCII)
        SRCDTA = w_far;
        exsr WRTSRC;

       //Se posiciona en la Biblioteca donde se Copiara los
       //Archivos Texto.
         SRCDTA = w_car;
         exsr WRTSRC;

         exsr sGet;

         SRCDTA =  'quit';
            exsr  WRTSRC;
      /End-Free
     C                   EndSr
     C
     ‚*---------------------------------------------------------------------
      *šInicia Copia de Carpeta FTP a AS400
     ‚*---------------------------------------------------------------------
     c     sGet          BegSr
      /Free
       SRCDTA = %trim(w_cmd) + ' ' + %trim(w_cft) + %trim(w_aft) +
                ' ' + %trim(w_ara) + '.FILE' +
                ' (REPLACE';
       exsr WRTSRC;
      /End-Free
     c                   EndSr
     ‚*---------------------------------------------------------------------
     ‚* Inicia Copia de Carpeta FTP a AS400
     ‚*---------------------------------------------------------------------
     c     WRTSRC        BegSr
      * WRTSRC   ESCRITURA DE LA FUENTE PARA TCP
     C                   ADD       1             SRCSEQ
     C                   Z-ADD     0             SRCDAT
     C                   EXCEPT    SRCADD
     C                   MOVE      *BLANKS       SRCDTA
     C                   ENDSR
     ‚*---------------------------------------------------------------------
      * Texto output
     OLAPCMD    EADD         SRCADD
     O                       SRCDS              120
