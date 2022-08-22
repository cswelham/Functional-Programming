{-|
The version with display added so we can do error diagnostics
NOTE WE CAN DO THIS ONLY FOR VARIABLES x, y and z

This is closely based on Robert D. Cameron's code
 www.cs.sfu.ca/~cameron

1.  Syntactic and Semantic Domains of TINY
     Syntactic Domains are Ide, Exp and Cmd

Note that we are about to import the parser for TINY as a module. For this to work
you need to have the parser in a file called TINYParser.lhs in the same directory
as this file. Then run ghci with just the filename of this file as the file to load.
So, type "ghci CW4_simple_error_TINY_denot.hs" to get the parser to load and then this file.
-}

import TINYParser

{-|
We had these definitions in TINYParser....
type Ide = String

data Exp = Zero | One | TT | FF |
           Read | I Ide | Not Exp |
           Equal Exp Exp | Plus Exp Exp
           deriving Show

data Cmd = Assign Ide Exp | Output Exp |
           IfThenElse Exp Cmd Cmd |
           WhileDo Exp Cmd |
           Seq Cmd Cmd
           deriving Show

Semantic Domains
-}

data Value = Numeric Integer | Boolean Bool | ERROR
             deriving Show

data MemVal = Stored Value | Unbound
              deriving Show

{-|
Here we use functional objects to represent memory.  Looking up
an identifier is thus function application.   But we will later
need to define functions to initialize and update memory objects,
as well.
-}

type Memory = ([Ide], Ide -> MemVal)
--type Memory = Ide -> MemVal
--Error Lines with memory change: 74, 89, 146
type Input = [Value]
type Output = [Value]
type State = (Memory, Input, Output)

{-|
2.  Signatures of semantic functions.

First, we need auxiliary types to represent the possible
results of expression evaluation or command execution.
-}

data ExpVal = OK Value State | Error
data CmdVal = OKc State | Errorc
exp_semantics :: Exp -> State -> ExpVal
cmd_semantics :: Cmd -> State -> CmdVal

{-|
Note: we can use this interpreter to show errors only in program
       with variables x, y and z---no others!
-}

display :: Memory -> String
--display m = "x = " ++ show (m "x") ++ ", y = " ++ show (m "y") ++ ", z = " ++ show ( m "z") ++ " "
display (m,n) = concat[x ++ "=" ++ show(n x) ++ " "| x <- m]

{-|
3. Semantic Equations defining the semantic functions
    Haskell's equational definition is similar but not
    identical to the equational style used in the mathematical semantics.
-}

exp_semantics Zero s = OK (Numeric 0) s
exp_semantics One s = OK (Numeric 1) s
exp_semantics TT s = OK (Boolean True) s
exp_semantics FF s = OK (Boolean False) s
exp_semantics Read ((m,n), [], o) = error (display (m,n) ++ "Input: " ++ "[] " ++ "Output: " ++ show o)
exp_semantics Read ((m,n), (i:is), o) = OK i ((m,n), is, o)
exp_semantics (I ident) ((m,n), i, o) =
 case (n ident) of
     Stored v  -> OK v ((m,n), i, o)
     Unbound   -> error (display (m,n) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)


--Case for not expression
exp_semantics (Not exp) s = 
 case (exp_semantics exp s) of
   --If v is boolean then change v to not v
      OK (Boolean v) s1 -> OK (Boolean (not v)) s1 
      --Else give an error and display memory                            
      _ -> error (display (m,n) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
            where ((m,n),i,o) = s   

exp_semantics (Equal exp1 exp2) s =
 case (exp_semantics exp1 s) of
   OK (Numeric v1) s1 -> case (exp_semantics exp2 s1) of 
                           OK (Numeric v2) s2 -> OK (Boolean (v1 == v2)) s2
                           OK (Boolean v2) s2 -> OK (Boolean False) s2
                           Error -> error (display (m,n) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
                                    where ((m,n),i,o) = s1
   OK (Boolean v1) s1 -> case (exp_semantics exp2 s1) of 
                           OK (Boolean v2) s2 -> OK (Boolean (v1 == v2)) s2
                           OK (Numeric v2) s2 -> OK (Boolean False) s2
                           Error -> error (display (m,n) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
                                    where ((m,n),i,o) = s1
   Error -> error (display (m,n) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
            where ((m,n),i,o) = s

--Case for plus expression
exp_semantics (Plus exp1 exp2) s = 
   case (exp_semantics exp1 s) of
     --If v1 is numeric then evaluate expression 2
    OK (Numeric v1) s1 -> case (exp_semantics exp2 s1) of 
                            --If v2 is numeric then do v1 + v2
                            OK (Numeric v2) s2 -> OK (Numeric (v1 + v2)) s2
                            --If v2 is boolean give an error
                            OK (Boolean v2) s2 -> Error
                            Error -> Error
    --If v1 is boolean give an error
    OK (Boolean v1) s1 -> error (display (m,n) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
            where ((m,n),i,o) = s
    --Error is diaply memory
    Error -> error (display (m,n) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
            where ((m,n),i,o) = s

{-|
Assignment statements perform a memory updating operation.
A memory is represented as a function which returns the
value of an identifier.   To update a memory with a new
identifier-value mapping, we return a function that will
return the value if given the identifier or will use the
original memory function to retrieve values associated with
other identifiers.
-}

find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs

--Changed update function
update (m,n) ide val =
  --if ide in memory list then first part of pair is the memory, else first part of memory is the memory with ide added to front
  --then second part of memory is original update function where ide -> Store Val
  (if find ide m then m else ide:m, \ide2 -> if ide == ide2 then Stored val else n ide2)

{-|
We will later need a function to initialize an "empty" memory
that returns Unbound for every identifier.
-}

emptyMem ide = Unbound

cmd_semantics (Assign ident exp) s =
  case (exp_semantics exp s) of
    OK v1 ((m1,n1), i1, o1) -> OKc (update (m1,n1) ident v1, i1, o1)
    Error -> Errorc

cmd_semantics (Output exp) s =
  case (exp_semantics exp s) of
    OK v1 ((m1,n1), i1, o1) -> OKc ((m1,n1), i1, o1 ++ [v1])
    Error -> Errorc

--Case for IfThenElse command
cmd_semantics (IfThenElse exp cmd1 cmd2) s = 
  case (exp_semantics exp s) of
    --If expression is true then do the first command
    OK (Boolean True) s1 -> cmd_semantics cmd1 s1
    --If expression is false then do the second command
    OK (Boolean False) s1 -> cmd_semantics cmd2 s1
    --Anything else give a command error
    _ -> Errorc

--Case for WhileDo command
cmd_semantics (WhileDo exp cmd) s = 
  case (exp_semantics exp s) of
    --If expression is true do the command and then repeat the WhileDo
    OK (Boolean True) s1 -> cmd_semantics (Seq cmd (WhileDo exp cmd)) s1
    --If the expression is false don't change the state
    OK (Boolean False) s1 -> OKc s1
    --Anything else give a command error
    _ -> Errorc

cmd_semantics (Seq cmd1 cmd2) s =
  case (cmd_semantics cmd1 s) of
    OKc s1 -> cmd_semantics cmd2 s1
    Errorc -> Errorc

{-|
 4.  Demo/Semantic Change/Demo

To demo the semantics in action, we use the following
"run" function to execute a TINY program for a given input.
(Note that the memory is initialized to empty, as is the output).
-}

run program input =
  case (cmd_semantics parsed_program (([],emptyMem), input, [])) of
    OKc ((m,n), i, o) -> o
    Errorc -> [ERROR]
  where parsed_program = cparse program

{-|
Test programs

Test data---the programs first, second, third, fourth and fifth are as in the module TINYParser

For first 
-}

input1 = [Numeric 1, Numeric 2]
input2 = [Numeric 1, Numeric 3]
input3 = [Boolean True, Numeric 2]

--- For second, which is gordon

input4 =  [Numeric 1, Numeric 2, Boolean True]
input5 =  [Numeric 1, Numeric 2, Numeric 3, Boolean True]

--For third, fourth and fifth we need just a single number input
--testprog6 for generating errors

testprog6 = "y := 1; y := y = x"

{-|
so try

   run testprog6 [ ]
 
to see the error reporting
-}

--Question 3
--a) C[skip]s = s
--b) C[if E then C]s = (E[E]s = (v,s')) -> (isBool v -> (v -> C[C]s'),s'),error
--c) C[repeat C until E]s = (C[C]s = s') -> ((E[E]s' = (v,s")) -> (isBool v -> (v -> C[repeat C until E]s",error),s'),error),error)

--Question 4
--a) C[output 1;output 2]({},<>,<>)
----(C5) C[output 1; output 2]({},<>,<>) = (C[output 1]({},<>,<>) = error) -> error,C[output 2]({},<>,<1>)
----(C2) C[output 1]({},<>,<>) = (E[1]({},<>,<>) = (1,({},<>,<>))) -> ({},<>,<1>),error
----(C2) C[output 2]({},<>,<1>) = (E[2]({},<>,<1>) = (2,({},<>,<1>))) -> error
----(E1) E[1] ({},<>,<>) = (1,({},<>,<>))
----(E1) E[2] ({},<>,<>) = error
--Final Result: error

----C[output 1; output 2]({},<>,<>) = 
----(C[output 1]({},<>,<>) = error) -> error, C[output 2](C[output 1]({},<>,<>)) (by C5) = 
----(E[1]({},<>,<>) = error) -> error,(E[2](C[output 1]({},<>,<>))) (by C2) = 
----(1,({},<>,<>)) -> error,(E[2](C[output 1]({},<>,<>))) (by E1) =
----({},<>,<1>) -> error, (E[2]({},<>,<1>)) (by C2 and conditional) =
----(2,({},<>,<1>)) -> error,({},<>,<v.1> (by E1 and conditional) =
----error

--b) C[output(read+read)]({},<1;2>,<>)
----(C2) C[output (read+read)]({},<1;2>,<>) = (E[read+read]({},<1;2>,<>) = (3,({},<>,<>))) -> ({},<>,<3>),error
----(E7) E[read+read]({},<1;2>,<>) = (E[read]({},<1;2>,<>) = (<1>,({},<2>,<>)) -> ((E[read]({},<2>,<>) = (<2>,({},<>,<>) -> (isNum 1 and isNum 2 -> (3,({},<>,<>),error),error),error
----(E3) E[read]({},<1;2>,<>) = null <1;2> -> error,(<1>,({},<2>,<>))
----(E3) E[read]({},<2>,<>) = null <2> -> error,(<2>,({},<>,<>))
--Final Result: ({},<>,<3>)

----C[output (read+read)]({},<1,2>,<>)
----(E[read+read]({},<1,2>,<>) (by C2) =
----(E[read]({},<1,2>,<>) -> E[read](E[read]({},<1,2>,<>)) (by E7) =
----null <1,2> -> error,(<1>,({},<2>,<>)) -> E[read](E[read]({},<1,2>,<>)) -> (isNum v1 and isNum v2) (by E3 and conditional) =
----E[read]({},<2>,<>) -> (isNum v1 and isNum v2) (by E7) =
----null <2> -> error,(<2>,({},<>,<>)) -> (isNum 1 and isNum 2) (by E3 and isNum) =
----(3,({},<>,<>)) -> ({},<>,<v.o>),error (by E7 and conditional) =
----({},<>,<3>)

--c) C[x:=read;output x]({},<1,2>,<3>)
----(C5) C[x:=read;output x]({},<1,2>,<3>) = (C[x:=read]({},<1,2>,<3>) = error) -> error,C[output x]({x->1},<2>,<3>)
----(C1) C[x:=read]({},<1,2>,<3>) = ((E[read]({},<1,2>,<3>) = (1,({},<2>,<3>))) -> ({x->1},<2>,<3>),error
----(E3) E[read]({},<1,2>,<3>) = null <1,2> -> error,(1,({}, <2>, <3>))
----(C2) C[output x]({x->1},<2>,<3>) = (E[x]({x->1},<2>,<3>) = (1,({x->1},<2>,<3>))) -> ({x->1},<2>,<1,3>)
----(E4) E[x]({x->1},<2>,<3>) = ({x->} = unbound) -> error,(1,({x->1},<2>,<3>))
--Final Result: ({x->1},<2>,<1,3>)

----C[x:=read;output x]({},<1,2>,<3>) = 
----(C[x:=read]({},<1,2>,<3>) = error) -> error,C[output x](C[x:=read]({},<1,2>,<3>)) (by C5) = 
----((E[read]({},<1,2>,<3>) = error) -> error,C[output x](C[x:=read]({},<1,2>,<3>)) (by C1) =
----((1,({},<2>,<3>)) = error) -> error,C[output x](C[x:=read]({},<1,2>,<3>)) (by E3) =
----({x->1},<2>,<3>),error = error) -> error,C[output x](C[x:=read]({},<1,2>,<3>)) (by C2 and conditional) =
----C[output x]({x->1},<2>,<3>) -> E[x]({x->1},<2>,<3>) (by C1 and conditional) =
----(1,({x->1},<2>,<3>)) (by E4) =
----({x->1},<2>,<1,3>)