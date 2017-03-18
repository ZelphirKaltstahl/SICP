#lang racket

; racket -l errortrace -t exercise-...
(require rackunit)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; EXERCISE 2.74
;; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company’s computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable’s president, in her first aempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters’ needs while preserving the existing autonomy of the divisions.

;; --> Idea: The different divisions need to tag their data.
;;           They need to be instructed on what tg to use.
;;           Maybe tags should follow a pattern of location of the divisions.

;; Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division’s personnel records consist of a single file, which contains a set of records keyed on employees’ names. The structure of the set varies from division to division. Furthermore, each employee’s record is itself a set (structured differently from division to division) that contains information keyed under identifiers such as address and salary. In particular:

;; --> Loading a file should be the same everywhere, but the structure inside is not.
;;     Each division must provide a procedure to access their structure with an employee's name.
;;     Example: get-by-name, specific to each division, ((get 'divisionname 'get-record-by-name) "Albus")

;; a. 1. Implement for headquarters a get-record procedure that retrieves a specified employee’s record from a specified personnel file.
;;       The procedure should be applicable to any division’s file.
;;    2. Explain how the individual divisions’ files should be structured.
;;    3. In particular, what type information must be supplied?

;; a.1.
;; --> (get-record-from-personnel-file file employee)
(define (get-record-from-personnel-file an-origin a-file an-employee)
  ((get 'an-origin 'get-record) a-file an-employee))

;; a.2.
;; An individual division's files should be structured as follows:
;; (list TAG-OF-DIVISION rest-of-file-content)

;; a.3
;; Headquarters has to specify which division the file is from. (Tag)
;; Each division's file must be tagged with the tag for its origin.
;; Each division must tag their data / records and provide procedures to access records.
;; These procedures must be registered in the table of operations.

;; b. 1. Implement for headquarters a get-salary procedure that returns the salary information from a given employee’s record from any division’s personnel file.
;;    2. How should the record be structured in order to make this operation work?

;; b.1.
(define (get-salary-from-record an-origin a-record)
  ((get 'an-origin 'get-salaray) a-record))

;; b.2.
;; Each record must contain a tag, which tells us what division it is from.

;; c. 1. Implement for headquarters a find-employee-record procedure.
;;       This should search all the divisions’ files for the record of a given employee and return the record.
;;       Assume that this procedure takes as arguments an employee’s name and a list of all the divisions’ files.
(define (find-employee-record employee-name division-files)
  (let
    ;; first get the record from the current division's file
    ([record (get-record-from-personnel-file 'an-origin (car division-files))])
    (cond
      ;; then check whether the name in the record is equal to the name of the employee we are searching for
      [(eq? ((get 'an-origin 'get-name-from-record) record) employee-name)
        ;; if it is the name, return the record
        record]
      ;; if it is not the name, search the rest of the list of division files
      [else (find-employee-record employee-name (cdr division-files))])))


;; d. 1. When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?

;; It depends on to which division the personnel information is added and what structure their data has.
;; If the personnel is added to a record of an existing division, no changes are required.
;; Otherwise a new tag needs to be introduced, to reference procedures for the individual division of the newly taken over company.
;; Also if the structure of the data is different from any other structure already known in the system, new procedures must be written for processing it or existing ones need to be modified.
