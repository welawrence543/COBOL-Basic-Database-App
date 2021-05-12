      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmployeeFile ASSIGN TO "C:\Users\welaw\employee.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EmployeeID.
       DATA DIVISION.
       FILE SECTION.
       FD EmployeeFile.
       01 EmployeeData.
           02 EmployeeID PIC 99.
           02 FirstName PIC X(15).
           02 LastName PIC X(15).
           88 WSEOF VALUE HIGH-VALUE.

       WORKING-STORAGE SECTION.
       01 UserInput PIC 9.
       01 StayOpen PIC X VALUE 'Y'.
       01 EmployeeExists PIC X.


       PROCEDURE DIVISION.
       StartPara.
           OPEN I-O EmployeeFile.
           PERFORM UNTIL StayOpen='N'
               DISPLAY " "
               DISPLAY "Employee Records"
               DISPLAY "1 to add employee"
               DISPLAY "2 to delete employee"
               DISPLAY "3 to update employee"
               DISPLAY "4 to get employee"
               DISPLAY "0 to exit program"
               DISPLAY " "
               ACCEPT UserInput
               EVALUATE UserInput
                   WHEN 1 PERFORM AddEmployee
                   WHEN 2 PERFORM DeleteEmployee
                   WHEN 3 PERFORM UpdateEmployee
                   WHEN 4 PERFORM GetEmployee
                   WHEN OTHER move 'N' TO StayOpen
               END-EVALUATE
           END-PERFORM.
           CLOSE EmployeeFile.
           STOP RUN.

       AddEmployee.
           DISPLAY " "
           DISPLAY "Enter Employee ID ".
           ACCEPT EmployeeID.
           DISPLAY "Enter first name ".
           ACCEPT FirstName.
           DISPLAY "Enter last name ".
           ACCEPT LastName.
           DISPLAY " "
           WRITE EmployeeData
               INVALID KEY DISPLAY "ID taken"
           END-WRITE.

       DeleteEmployee.
           DISPLAY " "
           DISPLAY "Enter employee ID to delete ".
           ACCEPT EmployeeID.
           DELETE EmployeeFile
               INVALID KEY DISPLAY "Key does not exist"
           END-DELETE.

       UpdateEmployee.
           MOVE 'Y' TO EmployeeExists.
           DISPLAY " "
           DISPLAY "Enter ID to update ".
           ACCEPT EmployeeID.
           READ EmployeeFile
               INVALID KEY MOVE 'N' TO EmployeeExists
           END-READ
           IF EmployeeExists='N'
               DISPLAY "Employee doesn't exist"
           ELSE
               DISPLAY "Enter the new first name "
               ACCEPT FirstName
               DISPLAY "Enter the new last name "
               ACCEPT LastName
           END-IF.
           REWRITE EmployeeData
               INVALID KEY DISPLAY "Employee Not Updated"
           END-REWRITE.

       GetEmployee.
           MOVE 'Y' TO EmployeeExists.
           DISPLAY " "
           DISPLAY "Enter ID to find ".
           ACCEPT EmployeeID.
           READ EmployeeFile
               INVALID KEY MOVE 'N' TO EmployeeExists
           END-READ
           IF EmployeeExists='N'
               DISPLAY "Employee doesn't exist"
           ELSE
               DISPLAY "ID " EmployeeID
               DISPLAY "First Name " FirstName
               DISPLAY "Last name " LastName
           END-IF.
