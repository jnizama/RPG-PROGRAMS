     H DATFMT(*ISO) DATEDIT(*YMD-)
      *----------------------------------------------------------
      * Carga FICTICDDM
      * Realizado : 10-Jun-2004
      *----------------------------------------------------------
     fFicTicDDM uf a e           K Disk    Prefix( WRK_ )
     f                                     Rename(FicTicr: rFic )
     fFicTic    uf a e           k Disk    Prefix(x_)
      *--
     d $RecDat       E DS                  EXTNAME( FICTIC  )
     d                                     Prefix(x_)
     d $RecDat2      E DS                  EXTNAME( FICTICDDM )
     d                                     Prefix(WRK_)
     d                sds
     dvUser                  254    263
      *
     c                   Exsr      sProceso
     c                   Seton                                        LR
      *----------------------------------------------------------           *****
      *    Extrae informacion Requerida de TRASAP
      *----------------------------------------------------------           *****
     c     sProceso      Begsr
     c
     c     *Loval        Setll     ficticr
     c                   Read      ficticr                              4040
     c     *In40         Doweq     '0'
     c                   If        x_FICFPR=D'0001-01-01' And
     c                             x_FICFOF=D'0001-01-01' And
     c                             x_FICFEC=D'0001-01-01'
     c                   Else
     c                   Move      $RecDat       $RecDat2
     c                   Write     rFic
     c                   Endif
     c                   Read      ficticr                              4040
     c                   Enddo
     c
     c                   Endsr
