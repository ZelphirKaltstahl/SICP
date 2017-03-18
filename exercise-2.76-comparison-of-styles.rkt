;; Exercise 2.76

;; As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies—generic operations with explicit dispatch, data-directed style, and message-passing-style describe the changes that must be made to a system in order to add new types or new operations.

;; A: Which organization would be most appropriate for a system in which new types must often be added?
;; B: Which would be most appropriate for a system in which new operations must often be added?

;; Thoughts about the Styles
;; =========================

;; Generic Procedures with explicit Dispatch for Type
;; --------------------------------------------------

;; What is its weakness?

;; When we add a new representation for data or a new type, we need to add it to all procedures, which do dispatch and could ever receive the new type as a tag for some parameter.
;; Furthermore the dispatch needs to be repeated in each procedure doing it, resulting in less readable code for each of these procedures.
;; This means it is not suited for a system, in which new types are often added.


;; Data-directed Style
;; -------------------

;; What is its weakness?

;; The lookup in the table of operations becomes more time consuming when more operations are added. One could use a data structure with O(1) access time though.
;; Each party adding operations needs to know a policy for reserving itself a tag, so that it does not conflict / collide with another party's tag (if their representations of data are different).

;; Message-passing Style
;; ---------------------

;; What is its weakness?

;; One can only pass one parameter at a time as a message. This means multiple parameters might need to be encapsuled into some data object before we can pass it. On the other hand this might force people to encapsulate properly.

;; 2.76.A:
;; For adding new types it seems message-passing style is minimalistic, since it only defines the object in terms of the operations it supports.
;; There is no overhead involved.

;; In data-directed style one would need to also "install" procedures in the table of operations, which can work with the new type or modify the ones already there.
;; It might be that existing operations can work with the type as well, so it might be one has to update the lists of supported types (tags) in the table of operations for each operation.

;; With generic operations dispatching on type themselves, one would have to update all procedures, which dispatch on types, in order either perform a real operation, or produce an error.

;; 2.76.B:
;; For adding new operations it seems the data-directed style is minimal. We only need to define our operations for each type (inside a package) and add them to the table of operations.
;; Since we know about our operations and their requirements, we probably know what kind of data they can work with.
;; With this knowledge we can add them to the table of operations with a list of types they support.
;; So maybe this is optimal in this scenario?

;; In message-passing style we might have to change all the objects and add a dispatch condition to them, in case they can perform an operation.
;; However, adding this one condition does not seem so difficult, so this might not really be a disadvantage,
;; because in data-directed programming we also need to check different data objects to see whether or not the procedures work on them or not.

;; With generic operations dispatching on type there does not seem to be anything in specific we need to do with existing code to enable a new operation.
