       IDENTIFICATION DIVISION.
       PROGRAM-ID. sample.

       DATA DIVISION.
          WORKING-STORAGE SECTION.
             77 EXITED PIC 9 VALUE 0.
             77 CHOICE PIC 9.
             77 IT PIC 99.
             77 inputStdNo PIC X(30).
             77 newCourse PIC X(30).
             77 newMobile PIC X(11).
             77 newLandline PIC X(8).
             77 newAge PIC 99.
             01 students.
                02 student-info OCCURS 5 TIMES.
                   03 fullname PIC X(30).
                   03 sNo PIC X(30).
                   03 course PIC X(30).
                   03 contact-info.
                      04 mobile PIC X(11).
                      04 landline PIC X(8).
                   03 age PIC 99.

       PROCEDURE DIVISION.
          PERFORM MENU UNTIL EXITED = 1.
          STOP RUN.
          
          MENU.
             DISPLAY "MENU".
             DISPLAY "[1] ADD STUDENT".
             DISPLAY "[2] EDIT STUDENT INFO".
             DISPLAY "[3] DELETE STUDENT".
             DISPLAY "[4] VIEW STUDENT".
             DISPLAY "[5] VIEW ALL STUDENTS".
             DISPLAY "[6] EXIT".
             DISPLAY "CHOICE: " WITH NO ADVANCING.
             ACCEPT CHOICE.

                IF CHOICE = 1
                   PERFORM ADD-STUDENT VARYING IT FROM 1 BY 1 UNTIL IT>5
                ELSE
                   IF CHOICE = 2
                      MOVE 1 TO IT
                      PERFORM EDIT-STUDENT
                   ELSE
                      IF CHOICE = 4
                         MOVE 1 TO IT
                         PERFORM VIEW-STUDENT
                      ELSE
                         IF CHOICE = 5
                            MOVE 1 TO IT
                            PERFORM VIEW-ALL
                         ELSE
                            MOVE 1 TO EXITED
                      END-IF
                   END-IF
                END-IF.


          ADD-STUDENT.
             DISPLAY "***ADD STUDENT***".
             DISPLAY "ENTER FULL NAME: " WITH NO ADVANCING.
             ACCEPT fullname(IT).

             DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING.
             ACCEPT sNo(IT).

             DISPLAY "ENTER COURSE: " WITH NO ADVANCING.
             ACCEPT course(IT).

             DISPLAY "CONTACT NUMBER/S (Kindly type n/a if none):"
             DISPLAY "ENTER MOBILE NUMBER: " WITH NO ADVANCING.
             ACCEPT mobile(IT).
             DISPLAY "ENTER LANDLINE NUMBER: " WITH NO ADVANCING.
             ACCEPT landline(IT).

             DISPLAY "ENTER AGE: " WITH NO ADVANCING.
             ACCEPT age(IT).

          EDIT-STUDENT.
             DISPLAY "***EDIT STUDENT INFO***".
             DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING.
             ACCEPT inputStdNo.


             IF inputStdNo = sNo(IT)
                DISPLAY "STUDENT FOUND!"

                DISPLAY "ENTER NEW COURSE: " WITH NO ADVANCING
                ACCEPT newCourse
                MOVE newCourse to course(IT)

                DISPLAY "ENTER NEW MOBILE NUMBER: " WITH NO ADVANCING
                ACCEPT newMobile
                MOVE newMobile to mobile(IT)

                DISPLAY "ENTER NEW LANDLINE NUMBER: " WITH NO ADVANCING
                ACCEPT newLandline
                MOVE newLandline to landline(IT)

                DISPLAY "ENTER NEW AGE: " WITH NO ADVANCING
                ACCEPT newAge
                MOVE newAge to age(IT)
             ELSE
                DISPLAY "STUDENT NOT FOUND!"
             END-IF.

          VIEW-STUDENT.
             DISPLAY "***VIEW STUDENT INFO***".
             DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING.
             ACCEPT inputStdNo.

             IF inputStdNo = sNo(IT)
                DISPLAY "STUDENT FOUND!"

                DISPLAY "STUDENT INFORMATION"

                DISPLAY "FULL NAME: " WITH NO ADVANCING
                DISPLAY fullname(IT)

                DISPLAY "COURSE: " WITH NO ADVANCING
                DISPLAY course(IT)

                DISPLAY "MOBILE NUMBER: " WITH NO ADVANCING
                DISPLAY mobile(IT)

                DISPLAY "LANDLINE NUMBER: " WITH NO ADVANCING
                DISPLAY landline(IT)

                DISPLAY "AGE: " WITH NO ADVANCING
                DISPLAY age(IT)
             ELSE
                DISPLAY "STUDENT NOT FOUND!"
             END-IF.

          VIEW-ALL.
             DISPLAY "***VIEW ALL STUDENTS***".

             IF IT>0
                DISPLAY "FULL NAME: " fullname(IT)
                DISPLAY "STUDENT NUMBER: " sNo(IT)
                DISPLAY "COURSE: " course(IT)
                DISPLAY "MOBILE NUMBER: " mobile(IT)
                DISPLAY "LANDLINE NUMBER: " landline(IT)
                DISPLAY "AGE: " age(IT)
             ELSE
                DISPLAY "LIST IS EMPTY!"
             END-IF.


                 










          


