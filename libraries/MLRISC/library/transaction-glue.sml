(*
 * Basic kinds of undoable imperative data structures
 * -- Allen
 *)

structure Transaction = TransactionFn(TransactionLog) 

structure UndoableRef = UndoableRefFn(TransactionLog) 

structure UndoableArray = UndoableArrayFn(structure Array = Array
                                          structure Log = TransactionLog) 

