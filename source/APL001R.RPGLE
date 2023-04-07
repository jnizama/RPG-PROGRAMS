   ** ******************************************************************
   ** * CALCULA EL DIA DE LA SEMANA
   ** * ULTIMA MODIFICACI¢N 940510
   ** * PARAMETROS :    FECHA DE 8 DIGITOS FORMATO DDMMAAA NUMERICO
   ** *                 DIA DE LA SEMANA DE 10 CARACTERES ALFANUM.
   ** * SISTEMAS IMPL. SP1AS401     S44002472
      * www.recursos-as400.com   ===> :JZB
   ** ******************************************************************
     D DIAS            S             10    DIM(7) CTDATA PERRCD(1)
     D FECHA           DS
     D  DD                     1      2  0
     D  MM                     3      4  0
     D  AA                     5      8  0
     D  DMAA                   1      8  0
     C                   EXSR      VERDIA
     C                   SETON                                        LR
     C     *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    FECHAW            8 0          aaaammdd
     C                   PARM                    DIASEM           10
     C                   PARM                    NUMDIA            1 0
     c* convierte a ddmmaaa
     c                   move      fechaw        fechaw2           8 0
     c                   move      fechaw        vT                8
     c                   eval      vT= %subst(vT:7:2)+%subst(vT:5:2)+
     c                                 %subst(vT:1:4)
     c                   move      vT            FECHAW
     C                   ENDSR

     C     VERDIA        BEGSR
     C                   MOVE      FECHAW        DMAA
     C                   Z-ADD     0             A1                5 0
     C                   Z-ADD     0             A2                5 0
     C                   Z-ADD     0             A3                5 0
     C                   Z-ADD     0             A4                5 0
     C                   Z-ADD     0             A5                5 0
     C     MM            IFGT      2
     C                   Z-ADD     AA            J                 4 0
     C                   Z-ADD     MM            K                 2 0
     C                   ELSE
     C     AA            SUB       1             J
     C     MM            ADD       12            K
     C                   END
     C     DD            SUB       1             A1
     C     5             MULT      J             A2
     C     A2            DIV       4             A2
     C     A1            ADD       A2            A2
     C     J             DIV       100           A3
     C     A2            SUB       A3            A3
     C     J             DIV       400           A4
     C     A3            ADD       A4            A4
     C     K             ADD       1             A5
     C     13            MULT      A5            A5
     C     A5            DIV       5             A5
     C     A4            ADD       A5            A5
     C     A5            DIV       7             A5
     C                   MVR                     RESTO             1 0
     C     RESTO         IFEQ      0
     C                   Z-ADD     7             RESTO
     C                   END
     C                   Z-ADD     RESTO         R                 1 0
     C                   MOVE      DIAS(R)       DIASEM
     c                   z-add     R             NUMDIA
     c* regrega a aaaammdd
     c                   move      fechaw2       fechaw
     C                   ENDSR
**
LUNES
MARTES
MIERCOLES
JUEVES
VIERNES
SABADO
DOMINGO
