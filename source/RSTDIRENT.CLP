 BEGIN:      PGM        PARM(&OLDSYSNAME)
             DCLF       FILE(BCRS/SDDENTRIES)
             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
             DCL        VAR(&OLDSYSNAME) TYPE(*CHAR) LEN(8)
   /* */
             IF         COND(&OLDSYSNAME = ' ') THEN(GOTO CMDLBL(END))
   /* */
             RTVNETA    SYSNAME(&SYSNAME)
   /* */

 IMPORT:     RCVF
             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(END))
             IF         COND(&WOSDUSRP *EQ ' ') THEN(DO)
             ADDDIRE    USRID(&WOSDDEN &WOSDDGN) USRD(&WOSDDESC) +
                          USER(*NONE) SYSNAME(&WOSDREN &WOSDRGN)
             MONMSG     MSGID(CPF0000)
             ENDDO

             IF         COND(&WOSDUSRP *NE ' ') THEN(DO)
             CHGVAR     VAR(&WOSDREN) VALUE(&SYSNAME)
             IF         COND(&WOSDREN = &OLDSYSNAME) THEN(CHGVAR +
                          VAR(&WOSDREN) VALUE(&OLDSYSNAME))
             CHKOBJ     OBJ(&WOSDUSRP) OBJTYPE(*USRPRF)
             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(IMPORT))
             ADDDIRE    USRID(&WOSDDEN &WOSDREN) USRD(&WOSDDESC) +
                          USER(&WOSDUSRP)
             MONMSG     MSGID(CPF0000)
             ENDDO
             GOTO       CMDLBL(IMPORT)
 END:        ENDPGM
