       IDENTIFICATION DIVISION.
       PROGRAM-ID. assur.
       Author.Thomas/Sibory/Benoit.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT FICHIER-ASSURANCES 
           ASSIGN TO "assurances-68259db4e2e6f768575516.csv"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

       FD FICHIER-ASSURANCES.
       01 F-ASSURANCE                      PIC X(120).


       WORKING-STORAGE SECTION.
       
       01 WS-ASSURANCES-TAB.
           05 WS-ASSURANCES                OCCURS 36 TIMES.
               10 WS-CODE                  PIC 9(8).
               10 WS-NOM-CONTRAT           PIC X(14).
               10 WS-NOM-PRODUIT           PIC X(14).
               10 WS-NOM-CLIENT            PIC X(41).
               10 WS-STATUT                PIC X(8).
               10 WS-DATE-DEBUT.
                   15 WS-DEBUT-ANNEE       PIC 9(4).
                   15 WS-DEBUT-MOIS        PIC 99.
                   15 WS-DEBUT-JOUR        PIC 99.
               10 WS-DATE-FIN.
                   15 WS-FIN-ANNEE         PIC 9(4).
                   15 WS-FIN-MOIS          PIC 99.
                   15 WS-FIN-JOUR          PIC 99.    
               10 WS-PRIX                  PIC 9(6)V99.
               10 WS-DEVISE                PIC X(3).

       01 AFFICHAGE.
           05 FILLER                   PIC X(7)  VALUE "Code : ".
           05 AFF-CODE                 PIC 9(8).
           05 FILLER                   PIC X(15)  
               VALUE " Nom Contrat : ".
           05 AFF-NOM-CONTRAT          PIC X(14).
           05 FILLER                   PIC X(15)  
               VALUE " Nom Produit : ".
           05 AFF-NOM-PRODUIT          PIC X(14).
           05 FILLER                   PIC X(14)  
               VALUE " Nom Client : ".
           05 AFF-NOM-CLIENT           PIC X(41).
           05 FILLER                   PIC X(10)  
               VALUE " Statut : ".
           05 AFF-STATUT               PIC X(8).
           05 FILLER                   PIC X(17)  
               VALUE " Date de debut : ".
           05 AFF-DATE-DEBUT.
               10 AFF-DEBUT-JOUR       PIC 99.
               10 FILLER               PIC X               VALUE "/".
               10 AFF-DEBUT-MOIS       PIC 99.
               10 FILLER               PIC X               VALUE "/".
               10 AFF-DEBUT-ANNEE      PIC 9(4).
           05 FILLER                   PIC X(15)  
               VALUE " Date de fin : ".    
           05 AFF-DATE-FIN.
               10 AFF-FIN-JOUR         PIC 99.
               10 FILLER               PIC X               VALUE "/".
               10 AFF-FIN-MOIS         PIC 99.
               10 FILLER               PIC X               VALUE "/".
               10 AFF-FIN-ANNEE        PIC 9(4).    
           05 FILLER                   PIC X(8)  
               VALUE " Prix : ".     
           05 AFF-PRIX                 PIC 9(6)V99.
           05 AFF-DEVISE               PIC X(3).

       77 WS-INDEX                         PIC 99          VALUE 1.
       77 WS-MAX-TAB                       PIC 99          VALUE 36.
       77 WS-FIN-FICHIER                   PIC X           VALUE 'F'.

       PROCEDURE DIVISION.
       
           OPEN INPUT FICHIER-ASSURANCES.

           PERFORM UNTIL WS-FIN-FICHIER = 'T'

               READ FICHIER-ASSURANCES
                   AT END 
                       MOVE 'T' TO WS-FIN-FICHIER

                   NOT AT END 
                       UNSTRING F-ASSURANCE DELIMITED BY "*"
                           INTO WS-CODE(WS-INDEX) 
                                WS-NOM-CONTRAT(WS-INDEX) 
                                WS-NOM-PRODUIT(WS-INDEX) 
                                WS-NOM-CLIENT(WS-INDEX) 
                                WS-STATUT(WS-INDEX) 
                                WS-DATE-DEBUT(WS-INDEX) 
                                WS-DATE-FIN(WS-INDEX) 
                                WS-PRIX(WS-INDEX) 
                                WS-DEVISE(WS-INDEX) 
                       ADD 1 TO WS-INDEX                 
               END-READ                     
                           

           END-PERFORM.

           CLOSE FICHIER-ASSURANCES.

           PERFORM VARYING WS-INDEX FROM 1 BY 1 
               UNTIL WS-INDEX > WS-MAX-TAB

               PERFORM 0100-AFFICHAGE THRU 0100-AFFICHAGE-END

           END-PERFORM.

           IF WS-MAX-TAB >= 3

              DISPLAY "Affichage de l'enregistrement 3"
              MOVE 3 TO WS-INDEX

              PERFORM 0100-AFFICHAGE THRU 0100-AFFICHAGE-END

           END-IF.
           

           IF WS-MAX-TAB >= 7

              DISPLAY "Affichage de l'enregistrement 7"
              MOVE 7 TO WS-INDEX

              PERFORM 0100-AFFICHAGE THRU 0100-AFFICHAGE-END
              
           END-IF.

           STOP RUN.

           

      ****************************************************************** 

       0100-AFFICHAGE.
           MOVE WS-CODE(WS-INDEX) TO AFF-CODE 
           MOVE WS-NOM-CONTRAT(WS-INDEX) TO AFF-NOM-CONTRAT 
           MOVE WS-NOM-PRODUIT(WS-INDEX) TO AFF-NOM-PRODUIT 
           MOVE WS-NOM-CLIENT(WS-INDEX) TO AFF-NOM-CLIENT
           MOVE WS-STATUT(WS-INDEX) TO AFF-STATUT 
           MOVE WS-DEBUT-JOUR(WS-INDEX) TO AFF-DEBUT-JOUR
           MOVE WS-DEBUT-MOIS(WS-INDEX) TO AFF-DEBUT-MOIS
           MOVE WS-DEBUT-ANNEE(WS-INDEX) TO AFF-DEBUT-ANNEE
           MOVE WS-FIN-JOUR(WS-INDEX) TO AFF-FIN-JOUR
           MOVE WS-FIN-MOIS(WS-INDEX) TO AFF-FIN-MOIS
           MOVE WS-FIN-ANNEE(WS-INDEX) TO AFF-FIN-ANNEE
           MOVE WS-PRIX(WS-INDEX) TO AFF-PRIX
           MOVE WS-DEVISE(WS-INDEX) TO AFF-DEVISE

           DISPLAY AFFICHAGE
       .

       0100-AFFICHAGE-END.
           EXIT 
       .        


           


