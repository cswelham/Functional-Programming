-- Name: Connor Welham
-- ID: 1508018

import Parsing

--All types and data for TINY
type Ide = String
data Exp = Zero | One | TT | FF | Read | I Ide |
           Not Exp | Equal Exp Exp | Plus Exp Exp 
           deriving Show
data Cmd = Assign Ide Exp | Output Exp |
           IfThenElse Exp Cmd Cmd |
           WhileDo Exp Cmd | Seq Cmd Cmd
           deriving Show

--Parser for expressions. This is built up using expr, term and factor parsers
--First parser deals with <expr> ::= <term> + <expr> | <term> = <expr> | <term>
expr :: Parser Exp
expr =  do e1 <- term
           do symbol "+"
              e2 <- expr
              return (Plus e1 e2)
        +++
        do e1 <- term
           do symbol "="
              e2 <- expr
              return (Equal e1 e2)
        +++
        term

--Second parser deals with <term> ::= not <expr> | <factor>
term :: Parser Exp
term =  do symbol "not"
           e <- expr
           return (Not e)   
        +++
        factor

--Third parser deals with <factor> ::= read | false | true | 0 | 1 | <ide> | (<expr>)
factor ::  Parser Exp
factor =    do symbol "0"
               return Zero
            +++
            do symbol "1"
               return One
            +++
            do symbol "true"
               return TT
            +++
            do symbol "false"
               return FF
            +++
            do symbol "read"
               return Read
            +++
            do s <- identifier
               return (I s)
            +++
            do symbol "("
               e <- expr
               do symbol ")"
                  return e         

--Parser for commands. THi is built up using cmdr and cmdr2
--First parser deals with <cmdr> ::= <cmdr2>;<cmdr> | <cmdr2>
cmdr :: Parser Cmd
cmdr =   do c1 <- cmdr2
            do symbol ";"
               c2 <- cmdr
               return (Seq c1 c2)
         +++
         cmdr2
         
--Second parser deals with <cmdr2> ::= if <expr> then <cmdr> else <cmdr> | while <expr> do <cmdr> | <Ide>:=<expr> | output <expr> | (<cmdr>)
cmdr2 :: Parser Cmd
cmdr2 =  do string "if "
            e <- expr
            do string "then "
               c1 <- cmdr
               do string "else "
                  c2 <- cmdr
                  return (IfThenElse e c1 c2)
         +++
         do string "while "
            e <- expr
            do string "do "
               c <- cmdr
               return (WhileDo e c)
         +++
         do s <- identifier
            do string ":="
               e <- expr
               return (Assign s e)
         +++
         do string "output "
            e <- expr
            return (Output e)
         +++
         do symbol "("
            c <- cmdr
            do symbol ")"
               return c  

--Parser for testing the expression parser
eeval :: String -> Exp
eeval xs = case (parse expr xs) of
             [(n,[])] -> n
             [(_,out)] -> error ("unused input " ++ out)
             [] -> error "invalid input"

--Parser for testing the command parser
ceval :: String -> Cmd
ceval xs = case (parse cmdr xs) of
             [(n,[])] -> n
             [(_,out)] -> error ("unused input " ++ out)
             [] -> error "invalid input"

----Programs to test
--Gordon 2.3 Program
gordonprogram = "sum:=0;x:=read;while not (x=true) do sum:=sum+x;x:=read;output sum"

--Program One: This program correctly parses
--x and y have values 0 and 1 respectively. the variable count has the value read
--if count is true then x is outputted, else y is outputted
programone = "x:=0;y:=1;count:=read;if (count=true) then (output x) else (output y)"

--Program Two: This program correctly parses
--x has value 0, y has value 1, num has value read and count has value 0
--while the variable num is 0, y is added to count and num is again set to read
--when out of the while loop, count is outputted
programtwo = "y:=1;num:=read;count:=0;while (num=0) do (count:=count+y;num:=read);output count"

--Program Three: This program parses an error as in a whiledo it must specify a command to do and false is an expression
--x has the value of 0. while x is 0, false is done
--when out of the while loop, x is outputted
programthree = "x:=0;while (x=0) do (false);output x"

--Program Four: This program parses an error as it assigns a command to the variable x, when only expressions can be assigned to an Ide
--x has the value 1 and y has the value output x
--if x is 1, y is done
programfour = "x:=1;y:=(output x);if (x=1) do y"