{- | Z3 integration |
   ==================

   The final step in implementing a verifier like Dafny is to
   check whether the verification conditions generated using
   weakest preconditions actually hold. To do that, we will
   use the Z3 theorem prover.

   We will do this by creating a so called
   "SMTLib" file with the extnension .smt2 for _every_ verification
   condition.

   For an example, consider the first verification condition of our
   "Square.dfy" program: 

     x > 0 && true ==> (0 <= x && 0 == 0 * x)

   This gives rise to the following .smt2 file (available as
   "Square.dfy1.smt2") on the course website.

; Declare Constant x
(declare-const x Int)
; assert the negation of the verification condition:
(assert (not (=> (and (> x 0) true) (and (<= 0 x) (= 0 (* 0 x))))))
; ask z3 to check it
(check-sat)

    Read below for additional details on the SMT2 format.

    Your task is to create such a file given a Predicate p. We provide
    you with a simple IO handler that relies on a function

      toSMT :: Predicate -> String

    Your final project is to implement this function.  To implement
    this function, we have imported the pretty printing library we
    used in a previous homework. It is highly recommended that you
    mimic the organization of your pretty printers to facilitate
    debugging your code.  That said, you are welcome to use any part
    of the standard library that doesn't require editing the cabal
    file of the project. 

========================
Declaration of Constants 
========================

    The first part of the file is a series of constant declarations with
    the following syntax:

(declare-const <variable-name> <type>)

    Variable names are just strings, and for the purposes of this final
    project, you can assume that all variables that appear in a predicate
    are integers. To lift that assumption we would need to implement a type
    checker for Dafny, which we haven't done.

    Before you begin constructing the .smt2 file, you should calculate
    the variables that appear in a given predicate p, and create a
    declaration such as the one in the example for each one. We have
    imported the Data.Set library for you --- it is recommended that
    you use it, but again, you are welcome to design and implement
    this final project in any way you see fit.
   

==========
Assertions
==========

    The second part of the file is the negation of the verification condition
    where every operation is in prefix form. For example, given the predicate:

      (x > 0 && true) ==> (0 <= x && 0 == 0 * x)

    we will assert it's negation: 

      (assert (not (=> (and (> x 0) true) (and (<= 0 x) (= 0 (* 0 x))))))

    Each operation appears in parentheses before its arguments:
    For example:

           x > 0              translates to              (> x 0)
           0 * x              translates to              (* 0 x)
       false && true          translates to              (and false true)
           0 == x             translates to              (= 0 x)

    While this format makes for syntax that is hard for humans to
    read, you should find that it's much more suitable for being
    automatically generated recursively from the AST of an expression.

=========
Check SAT
=========
   
    The final part of your file should be a single call to check the
    satisfiability of the assertion you created above:

(check-sat)

    Z3 (and similar solvers) are capable of finding satisfying assignments
    for a plethora of formulas involving integer arithmetic, or returning
    "unsat" if such an assignment doesn't exist. That's why we check for
    the satisfiability of the negation of the verification condition: if
    z3 says that the negation cannot be satisfied, then we are guaranteed
    that it is a tautology that holds in all contexts.

-}
module Z3 where

import Syntax
import Data.List(intersperse)
import Text.PrettyPrint ( (<+>), Doc )
import qualified Text.PrettyPrint as PP

import System.Process(readProcessWithExitCode)
import Data.Set(Set)
import qualified Data.Set as Set


predicate_vars :: Predicate -> Set.Set Name
predicate_vars (Forall binding_list e1) = Set.union (Set.fromList (map fst binding_list)) (expression_vars e1)
predicate_vars (PredOp p1 _ p2) = Set.union (predicate_vars p1) (predicate_vars p2)
predicate_vars _ = Set.empty

expression_vars :: Expression -> Set.Set Name
expression_vars (Var v) = case v of
  (Name n) -> Set.singleton n
  (Proj n e1) -> Set.singleton n
expression_vars (Op1 u1 e1) =  expression_vars e1
expression_vars (Op2 e1 b1 e2) = Set.union (expression_vars e1) (expression_vars e2)
expression_vars _ = Set.empty

predicate_to_SMT :: Predicate -> String
predicate_to_SMT (Forall binding_list e1) = expression_to_SMT e1
predicate_to_SMT (PredOp p1 b p2) =case b of
  Conj -> "(and " ++ (predicate_to_SMT p1) ++ " " ++ (predicate_to_SMT p2) ++ ")"
  Disj -> "(or " ++ (predicate_to_SMT p1) ++ " " ++ (predicate_to_SMT p2) ++ ")"
  Implies -> "(=> " ++ (predicate_to_SMT p1) ++ " " ++ (predicate_to_SMT p2) ++ ")"
  Iff -> "(= " ++ (predicate_to_SMT p1) ++ " " ++ (predicate_to_SMT p2) ++ ")"

expression_to_SMT :: Expression -> String
expression_to_SMT (Var v) = case v of
  (Name n) -> n
  (Proj n e) -> n
expression_to_SMT (Val v) = case v of
  (IntVal i) -> show i
  (BoolVal b) -> if b then "true" else "false"
expression_to_SMT (Op1 u e) = case u of 
  Neg -> "(- " ++ (expression_to_SMT e) ++ ")"
  Not -> "(not " ++ (expression_to_SMT e) ++ ")"
expression_to_SMT (Op2 e1 b e2) = case b of
  Plus      -> "(+ " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Minus     -> "(- " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Times     -> "(* " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Divide    -> "(div " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Modulo    -> "(mod " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Eq        -> "(= " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Neq       -> "(distinct " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Gt        -> "(> " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Ge        -> "(>= " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Lt        -> "(< " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Le        -> "(<= " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Conj      -> "(and " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Disj      -> "(or " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Implies   -> "(=> " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"
  Iff       -> "(= " ++ (expression_to_SMT e1) ++ " " ++ (expression_to_SMT e2) ++ ")"

list_to_string :: [String] -> String
list_to_string string_list = foldr (\x acc -> x ++ acc) "" string_list

toSMT :: Predicate -> String
toSMT p = (list_to_string real_declaration) ++ assertion ++ "\n" ++ "(check-sat)" where
    declaration = map (\s -> "(declare-const " ++ s ++ " Int)") (Set.toList (predicate_vars p))
    real_declaration = map (\s -> s ++ "\n") declaration
    assertion = "(assert (not " ++ (predicate_to_SMT p) ++ "))"

-- | The name of the z3 executable. Change this to whatever it is in your system:
--   In unix based systems, this is just "z3".
--   In Windows, it will be the name of the executable that was installed alongside Dafny.
z3 :: String
z3 = "z3"

-- | This function uses "toSMT" in order to write a file, and invoke z3 on it, checking its
--   output. You're welcome to modify this function as you see fit, the only thing we will
--   automatically test is your "toSMT" function.
convertAndCheck :: Predicate -> String -> IO Bool
convertAndCheck p fn = do
  writeFile fn (toSMT p)
  (_exitCode, stdout, _stderr) <- readProcessWithExitCode z3 [fn] ""
  case stdout of
    's':'a':'t':_ -> return False
    'u':'n':'s':'a':'t':_ -> return True
    _ -> error $ "Z3 output was neither sat or unsat: " ++ stdout