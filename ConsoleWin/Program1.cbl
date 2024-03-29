      $set ilusing"System.Configuration"
      $set ilusing"System.Data.SqlClient"

       id division.
       program-id. Program1 as "CobolConsole.Program1".
       special-names.
            crt status is key-status.
       data division.
       WORKING-STORAGE SECTION.

      *-----------------------*
      *01 CLEANING PIC X(4) VALUE "[2J".

       01 flag pic 9(2) comp-x value 1.
       01 user-key-control.
           05 enable-fn-keys pic 9(2) comp-x value 1.
           05 filler pic x value "1".
           05 first-user-key pic 9(2) comp-x value 1.
           05 number-of-keys pic 9(2) comp-x value 10.
       01 key-status.
           05 key-type pic x.
           05 key-code-1 pic 9(2) comp-x.
           05 filler pic x.
      *01 any-data pic x.
      *01 key-code-1-display pic z9.


      *============================================================
      * INCLUDE SQLCA
      *============================================================
       01 FILLER PIC X(20) VALUE 'SQLCA678901234567890'.
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
      *============================================================
      * TABLA PARA BBDD
      *============================================================
       01 TABLA.
           03 COLCIF PIC X(9).
           03 FILLER PIC X.
           03 COLNOM PIC X(20).
           03 FILLER PIC X.
           03 COLDIR PIC X(35).
           03 FILLER PIC X.
           03 COLTLF PIC X(9).

      *============================================================
      * ************CURSOR****************************************
      *============================================================
           EXEC SQL
               DECLARE CURS1 SCROLL CURSOR FOR
                   SELECT CIF
                        , NOMBRE
                        , DIRECCION
                        , TLF
                   FROM USRDATOS
                   ORDER BY CIF
           END-EXEC.

      * CAMPOS PARA ERROR DE DB2
       01 FILLER PIC X(20) VALUE 'SQLER678901234567890'.
       01 DB2-ERR.
         03 DB2-SQLCODE PIC S9(9).
         03 DB2-SQLCODE-Z PIC -ZZZZZZZZ9.
         03 DB2-ERROR.
           05 DB2-ERR-MSG PIC X(50).
           05 DB2-ERR-CODE PIC X(20).
      *

      * CAMPOS DE PANTALLA
       01 WM-00.
           03 WM00-OPC PIC X       value spaces.

       01 WM-DATOS.
         03 WM-DATOS-CIF   PIC X(09)   value spaces.
         03 WM-DATOS-NOM   PIC X(20)   value spaces.
         03 WM-DATOS-DIR   PIC X(35)   value spaces.
         03 WM-DATOS-TLF   PIC X(09)   value spaces.
         03 WM-DATOS-COR   PIC X(20)   value spaces.

       01 WM-TABLAFILA.
         03 WM-FILA PIC X(76) OCCURS 10 TIMES.

      * CAMPO PARA MENSAJE DE ERROR
       01 MSG-ERR PIC X(74) value spaces.


      * CAMPOS ADICIONALES
       01 numfila PIC 9(2) value 1.
       01 I PIC 9(2) value 01.
       01 comodin pic x.
       01 WM-03-primeravez PIC X(1) value 'Y'.
           88 primeravez-si VALUE 'Y'.
           88 primeravez-no VALUE 'N'.
       01 FINCURSOR PIC X(1) value 'N'.
           88 FIN-CURSOR VALUE 'Y'.
           88 NFIN-CURSOR VALUE 'N'.


      *============================================================
      * ************PANTALLAS**************************************
      *============================================================
       SCREEN SECTION.

       01 MENU00 BLANK SCREEN.
         05 LINE 1 COLUMN 1 VALUE IS 'MENU00' FOREGROUND-COLOR 1.
         05 LINE 1 COLUMN 65 VALUE IS 'VER: 08/07/2023' FOREGROUND-COLOR 1.
         05 LINE 2 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 3 COLUMN 28 VALUE IS 'VILANOVA S.L.U' FOREGROUND-COLOR 4.
         05 LINE 4 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 7 COLUMN 26 VALUE IS 'SELECCIONA UNA OPCION:' FOREGROUND-COLOR 2.
         05 LINE 9 COLUMN 26 VALUE IS '1-INTRODUCIR NUEVO REGISTRO' FOREGROUND-COLOR 2.
         05 LINE 11 COLUMN 26 VALUE IS '2-BUSCAR REGISTRO' FOREGROUND-COLOR 2.
         05 LINE 13 COLUMN 26 VALUE IS '3-LISTAR REGISTROS' FOREGROUND-COLOR 2.
         05 LINE 15 COLUMN 26 VALUE IS 'OPCION:' FOREGROUND-COLOR 2.
         05 WI00-OPC PIC X LINE 15 COLUMN 34 USING WM00-OPC FOREGROUND-COLOR 3.
         05 LINE 22 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 23 COLUMN 1 VALUE IS 'MSG:' FOREGROUND-COLOR 1.
         05 WI00-ERR PIC X(74) LINE 23 COLUMN 6 FROM MSG-ERR FOREGROUND-COLOR 7.
         05 LINE 24 COLUMN 1 VALUE IS 'F3=SALIR' FOREGROUND-COLOR 1.

       01 MENU01 BLANK SCREEN.
         05 LINE 1 COLUMN 1 VALUE IS 'MENU01' FOREGROUND-COLOR 1.
         05 LINE 1 COLUMN 65 VALUE IS 'VER: 08/07/2023' FOREGROUND-COLOR 1.
         05 LINE 2 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 3 COLUMN 28 VALUE IS 'VILANOVA S.L.U' FOREGROUND-COLOR 4.
         05 LINE 4 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 7 COLUMN 26 VALUE IS 'NUEVO REGISTRO' FOREGROUND-COLOR 2.
         05 LINE 10 COLUMN 21 VALUE IS 'CIF:' FOREGROUND-COLOR 2.
         05 WI01-CIF PIC X(9) LINE 10 COLUMN 26 USING WM-DATOS-CIF FOREGROUND-COLOR 3.
         05 LINE 11 COLUMN 10 VALUE IS '        NOMBRE:' FOREGROUND-COLOR 2.
         05 WI01-NOM PIC X(20) LINE 11 COLUMN 26 USING WM-DATOS-NOM FOREGROUND-COLOR 3.
         05 LINE 12 COLUMN 10 VALUE IS '     DIRECCIÓN:' FOREGROUND-COLOR 2.
         05 WI01-DIR PIC X(35) LINE 12 COLUMN 26 USING WM-DATOS-DIR FOREGROUND-COLOR 3.
         05 LINE 13 COLUMN 10 VALUE IS '           TLF:' FOREGROUND-COLOR 2.
         05 WI01-TLF PIC X(9) LINE 13 COLUMN 26 USING WM-DATOS-TLF FOREGROUND-COLOR 3.
         05 LINE 14 COLUMN 10 VALUE IS '        CORREO:' FOREGROUND-COLOR 2.
         05 WI01-COR PIC X(20) LINE 14 COLUMN 26 USING WM-DATOS-COR FOREGROUND-COLOR 3.
         05 LINE 22 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 23 COLUMN 1 VALUE IS 'MSG:' FOREGROUND-COLOR 1.
         05 WI01-ERR PIC X(74) LINE 23 COLUMN 6 FROM MSG-ERR FOREGROUND-COLOR 7.
         05 LINE 24 COLUMN 1 VALUE IS 'F3=SALIR' FOREGROUND-COLOR 1.

       01 MENU02 BLANK SCREEN.
         05 LINE 1 COLUMN 1 VALUE IS 'MENU02' FOREGROUND-COLOR 1.
         05 LINE 1 COLUMN 65 VALUE IS 'VER: 08/07/2023' FOREGROUND-COLOR 1.
         05 LINE 2 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 3 COLUMN 28 VALUE IS 'VILANOVA S.L.U' FOREGROUND-COLOR 4.
         05 LINE 4 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 7 COLUMN 26 VALUE IS 'BUSCAR REGISTRO' FOREGROUND-COLOR 2.
         05 LINE 10 COLUMN 7 VALUE IS 'INTRODUZCA EL CIF:' FOREGROUND-COLOR 2.
         05 WI02-CIF PIC X(9) LINE 10 COLUMN 26 USING WM-DATOS-CIF FOREGROUND-COLOR 3.
         05 LINE 12 COLUMN 10 VALUE IS '        NOMBRE:' FOREGROUND-COLOR 2.
         05 WI02-NOM PIC X(20) LINE 12 COLUMN 26 FROM WM-DATOS-NOM FOREGROUND-COLOR 14.
         05 LINE 13 COLUMN 10 VALUE IS '     DIRECCION:' FOREGROUND-COLOR 2.
         05 WI02-DIR PIC X(35) LINE 13 COLUMN 26 FROM WM-DATOS-DIR FOREGROUND-COLOR 14.
         05 LINE 14 COLUMN 10 VALUE IS '           TLF:' FOREGROUND-COLOR 2.
         05 WI02-TLF PIC X(9) LINE 14 COLUMN 26 FROM WM-DATOS-TLF FOREGROUND-COLOR 14.
         05 LINE 15 COLUMN 10 VALUE IS '        CORREO:' FOREGROUND-COLOR 2.
         05 WI02-COR PIC X(20) LINE 15 COLUMN 26 FROM WM-DATOS-COR FOREGROUND-COLOR 14.
         05 LINE 22 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 23 COLUMN 1 VALUE IS 'MSG:' FOREGROUND-COLOR 1.
         05 WI02-ERR PIC X(74) LINE 23 COLUMN 6 FROM MSG-ERR FOREGROUND-COLOR 7.
         05 LINE 24 COLUMN 1 VALUE IS 'F3=SALIR' FOREGROUND-COLOR 1.

       01 MENU03 BLANK SCREEN.
         05 LINE 1 COLUMN 1 VALUE IS 'MENU03' FOREGROUND-COLOR 1.
         05 LINE 1 COLUMN 65 VALUE IS 'VER: 08/07/2023' FOREGROUND-COLOR 1.
         05 LINE 2 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 3 COLUMN 28 VALUE IS 'VILANOVA S.L.U' FOREGROUND-COLOR 4.
         05 LINE 4 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 6 COLUMN 26 VALUE IS 'LISTADO DE REGISTROS' FOREGROUND-COLOR 2.
         05 LINE 9 COLUMN 1 VALUE IS 'CIF' FOREGROUND-COLOR 2.
         05 LINE 9 COLUMN 11 VALUE IS 'NOMBRE' FOREGROUND-COLOR 2.
         05 LINE 9 COLUMN 32 VALUE IS 'DIRECCION' FOREGROUND-COLOR 2.
         05 LINE 9 COLUMN 68 VALUE IS 'TLF' FOREGROUND-COLOR 2.
         05 WI03-FILA1 PIC X(76) LINE 10 COLUMN 1 FROM WM-FILA(1) FOREGROUND-COLOR 7.
         05 WI03-FILA2 PIC X(76) LINE 11 COLUMN 1 FROM WM-FILA(2) FOREGROUND-COLOR 7.
         05 WI03-FILA3 PIC X(76) LINE 12 COLUMN 1 FROM WM-FILA(3) FOREGROUND-COLOR 7.
         05 WI03-FILA4 PIC X(76) LINE 13 COLUMN 1 FROM WM-FILA(4) FOREGROUND-COLOR 7.
         05 WI03-FILA5 PIC X(76) LINE 14 COLUMN 1 FROM WM-FILA(5) FOREGROUND-COLOR 7.
         05 WI03-FILA6 PIC X(76) LINE 15 COLUMN 1 FROM WM-FILA(6) FOREGROUND-COLOR 7.
         05 WI03-FILA7 PIC X(76) LINE 16 COLUMN 1 FROM WM-FILA(7) FOREGROUND-COLOR 7.
         05 WI03-FILA8 PIC X(76) LINE 17 COLUMN 1 FROM WM-FILA(8) FOREGROUND-COLOR 7.
         05 WI03-FILA9 PIC X(76) LINE 18 COLUMN 1 FROM WM-FILA(9) FOREGROUND-COLOR 7.
         05 WI03-FILA0 PIC X(76) LINE 19 COLUMN 1 FROM WM-FILA(10) FOREGROUND-COLOR 7.
         05 LINE 22 COLUMN 1 VALUE IS '-------------------------------------------------------------------------------' FOREGROUND-COLOR 1.
         05 LINE 23 COLUMN 1 VALUE IS 'MSG:' FOREGROUND-COLOR 1.
         05 WI03-ERR PIC X(74) LINE 23 COLUMN 6 FROM MSG-ERR FOREGROUND-COLOR 7.
         05 LINE 24 COLUMN 1 VALUE IS 'F3=SALIR' FOREGROUND-COLOR 1.
         05 LINE 24 COLUMN 13 VALUE IS 'INTRO=CARGAR' FOREGROUND-COLOR 1.
         05 LINE 24 COLUMN 30 VALUE IS 'F4=RETROCEDER' FOREGROUND-COLOR 1.
         05 LINE 24 COLUMN 47 VALUE IS 'F5=SIGUIENTES' FOREGROUND-COLOR 1.
         05 WI02-CIF PIC X(1) LINE 21 COLUMN 1 USING comodin FOREGROUND-COLOR 0.




       PROCEDURE DIVISION.

           exec sql
             connect 'sa' identified by 'Pas$123456' at 'cobolDB' using 'SQLADO32'
           end-exec

           EXEC SQL
                WHENEVER  SQLERROR    CONTINUE
           END-EXEC.
      *
           EXEC SQL
                WHENEVER  SQLWARNING  CONTINUE
           END-EXEC.
      *
           EXEC SQL
                WHENEVER  NOT FOUND   CONTINUE
           END-EXEC.

           IF SQLCODE not = 0 THEN
               PERFORM G999-ERROR-DB2
               display "Ha ocurrido un error en la conexión con la base de datos:"
               display MSG-ERR
               stop ""
               PERFORM  CERRAR-PROGRAMA
           END-IF.

      *============================================================
      * -----------MENU00-----------------------------------------
      *============================================================
       PARRAFO-MENU00.

           initialize WM00-OPC.

           call x"af" using flag
                            user-key-control.

           display MENU00.

           accept MENU00.

           INITIALIZE MSG-ERR.
           evaluate key-type
               when 0
                   continue

               when 1
                   if key-code-1 = 3 THEN
                   perform CERRAR-PROGRAMA
                   ELSE perform PARRAFO-MENU00
                   end-if
       
           end-evaluate.

           evaluate WM00-OPC
               WHEN 1
                   initialize WM-DATOS
                   perform PARRAFO-MENU01
               WHEN 2
                   initialize WM-DATOS
                   perform PARRAFO-MENU02
               WHEN 3
                   initialize WM-TABLAFILA
                   perform PARRAFO-MENU03
               WHEN other
                   MOVE 'INTRODUZCA UN VALOR DEL 1 AL 3' TO MSG-ERR
                   perform PARRAFO-MENU00

           end-evaluate.


      *============================================================
      * -----------MENU01-----------------------------------------
      *============================================================
       PARRAFO-MENU01.

.
           initialize WM-DATOS.

           call x"af" using flag
                            user-key-control.

           display MENU01.

           accept MENU01.

           INITIALIZE MSG-ERR.
           evaluate key-type
               when 0
                   continue

               when 1
                   if key-code-1 = 3 THEN
                       perform PARRAFO-MENU00
                   ELSE
                       perform PARRAFO-MENU01
                   end-if
           end-evaluate.

           EXEC SQL
             INSERT
               INTO USRDATOS(
                    CIF
                  , NOMBRE
                  , DIRECCION
                  , TLF
                  , CORREO)
               VALUES(
                    :WM-DATOS-CIF
                  , :WM-DATOS-NOM
                  , :WM-DATOS-DIR
                  , :WM-DATOS-TLF
                  , :WM-DATOS-COR)
           END-EXEC.
           EXEC SQL COMMIT END-EXEC.


           IF SQLCODE = 0
               MOVE 'GUARDADO CORRECTAMENTE.' TO MSG-ERR
           ELSE
               MOVE 'ERROR AL GUARDAR.' TO MSG-ERR
               PERFORM G999-ERROR-DB2
           END-IF.

           perform PARRAFO-MENU01.


      *============================================================
      * -----------MENU02-----------------------------------------
      *============================================================
       PARRAFO-MENU02.
           call x"af" using flag
                            user-key-control.

           display MENU02.

           accept MENU02.

           INITIALIZE MSG-ERR.
           evaluate key-type
               when 0
                   continue

               when 1
                   if key-code-1 = 3 THEN
                       perform PARRAFO-MENU00
                   ELSE
                       perform PARRAFO-MENU02
                   end-if
           end-evaluate.


           EXEC SQL
                SELECT
                     CIF
                   , NOMBRE
                   , DIRECCION
                   , TLF
                   , CORREO
                INTO
                     :WM-DATOS-CIF
                   , :WM-DATOS-NOM
                   , :WM-DATOS-DIR
                   , :WM-DATOS-TLF
                   , :WM-DATOS-COR
                FROM USRDATOS
                WHERE CIF = :WM-DATOS-CIF
           END-EXEC.

           EVALUATE TRUE
               WHEN SQLCODE = 0
                   MOVE 'REGISTRO ENCONTRADO' TO MSG-ERR

               WHEN SQLCODE = +100
                   MOVE 'LA EMPRESA NO EXISTE.' TO MSG-ERR
                   initialize WM-DATOS

               WHEN OTHER
      *            RECUPERAR POSIBLE ERROR DE DB2
                   MOVE 'ERROR DESCONOCIDO. CONTACTE CON SOPORTE: correo@moisescampaña.es' TO MSG-ERR
                   PERFORM G999-ERROR-DB2
           END-EVALUATE.

           perform PARRAFO-MENU02.

      *============================================================
      * -----------MENU03-----------------------------------------
      *============================================================
       PARRAFO-MENU03.

           IF primeravez-si then
               EXEC SQL OPEN CURS1 END-EXEC
               SET primeravez-no TO TRUE
           END-IF.


           call x"af" using flag
                            user-key-control.

           display MENU03.

           accept MENU03.

           INITIALIZE MSG-ERR.
           evaluate key-type
               when 0
                   continue

               when 1
                   evaluate key-code-1
                       when 3
                           EXEC SQL CLOSE CURS1 END-EXEC
                           SET primeravez-si TO TRUE
                           perform PARRAFO-MENU00
                       when 4
                           IF numfila > 10 then
                               subtract 10 from numfila
                               SET NFIN-CURSOR TO TRUE
                           END-IF
                       when 5
                           IF NFIN-CURSOR then ADD 10 to numfila END-IF
                       when other
                           continue
                   end-evaluate
           end-evaluate.

           IF NFIN-CURSOR then
               perform LEER-CARGAR-REGISTRO
           END-IF.

           perform PARRAFO-MENU03.

  


       
       LEER-CARGAR-REGISTRO.

           INITIALIZE  TABLA
                       WM-TABLAFILA.
           move 1 to I.

      *PRIMER FETCH PARA POSICIONAR CURSOR EN numfila
           IF NFIN-CURSOR then
               EXEC SQL
                 FETCH ABSOLUTE :numfila
                 FROM CURS1 INTO :COLCIF, :COLNOM, :COLDIR, :COLTLF
               END-EXEC
               PERFORM EVALUAR-FINAL
               perform CARGAR-REGISTRO
           END-IF

           PERFORM UNTIL FIN-CURSOR OR I > 10
               EXEC SQL
                   FETCH CURS1 INTO :COLCIF, :COLNOM, :COLDIR, :COLTLF
               END-EXEC
               PERFORM EVALUAR-FINAL
               perform CARGAR-REGISTRO
           END-PERFORM.

       CARGAR-REGISTRO.
           IF NFIN-CURSOR then
               MOVE TABLA TO WM-FILA(I)
               ADD 1 to I
           END-IF.
           

           
       EVALUAR-FINAL.

           EVALUATE SQLCODE
               WHEN 0
                   SET NFIN-CURSOR TO TRUE
               WHEN 100
                   SET FIN-CURSOR TO TRUE
               WHEN other
                   PERFORM G999-ERROR-DB2
           END-EVALUATE.


       G999-ERROR-DB2.
           MOVE SQLCODE TO DB2-SQLCODE
           MOVE DB2-SQLCODE TO DB2-SQLCODE-Z
           MOVE DB2-SQLCODE-Z TO DB2-ERR-CODE
           MOVE SQLERRMC TO DB2-ERR-MSG.
           MOVE DB2-ERROR TO MSG-ERR.


       CERRAR-PROGRAMA.

           exec sql
             disconnect current
           end-exec.

           STOP RUN.


