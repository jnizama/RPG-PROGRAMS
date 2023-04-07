     ‚*------------------------------------------------------------------*
     ‚* ENVIA MAIL POR CLASE NO INCLUYE FAVEL,MATERIAS PRIMAS, JOHNSON
     ‚*------------------------------------------------------------------*
     h datfmt(*iso) datedit(*ymd-)
     d*
     D $RecDat       E DS                  extname( LOTVEN  )
     D $RecDat1      E DS                  extname( IICL01 )
     ‚* Crea cursor
     c/exec sql
     c+ declare c1 cursor for select
     c+ clase,icdes  from  bpcdta/lotven  inner join FVBPCSF/iicl01
     c+ on (clase =iclas)
     c+  where  FLG NOT IN ('F','M')
     c+ group by clase,icdes
     c+ order by clase
     c/end-exec
      *
     ‚* Open a Cursor                                                       ----
     c/exec sql
     c+ open C1
     c/end-exec
      *-
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :CLASE,:ICDES
     C/END-EXEC
     c                   dow       sqlcod = 0
     C
     C*
     C                   IF        CLASE <> 'JJ'
     C                   MOVEL     *BLANKS       VDESCLA          20
     C                   MOVEL     ICDES         VDESCLA
     C                   call      'XCTL104C'
     C                   PARM                    CLASE
     C                   PARM                    VDESCLA
     C                   ENDIF
     C
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :CLASE,:ICDES
     C/END-EXEC
     c                   enddo
     ‚* Close Cursor                                                        ----
     c/exec sql
     c+ close C1
     c/end-exec
      *-
     ‚* Fin-Programa                                                        ----
     c                   eval      *inlr = *on
      *-
      *-
     c
