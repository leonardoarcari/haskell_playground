{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr(zeroOrMore, oneOrMore,
             spaces, ident,
             parseSExpr) where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
-- First, let’s see how to take a parser for (say) widgets and turn it
-- into a parser for lists of widgets. In particular, there are two functions
-- you should implement: zeroOrMore takes a parser as input and runs
-- it consecutively as many times as possible (which could be none, if
-- it fails right away), returning a list of the results. zeroOrMore always
-- succeeds. oneOrMore is similar, except that it requires the input parser
-- to succeed at least once. If the input parser fails right away then
-- oneOrMore also fails.
-- For example, below we use zeroOrMore and oneOrMore to parse a
-- sequence of uppercase characters. The longest possible sequence of
-- uppercase characters is returned as a list. In this case, zeroOrMore
-- and oneOrMore behave identically:
--  *AParser> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
--      Just ("ABC","dEfgH")
--  *AParser> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
--      Just ("ABC","dEfgH")
-- The difference between them can be seen when there is not an uppercase
-- character at the beginning of the input. zeroOrMore succeeds
-- and returns the empty list without consuming any input; oneOrMore
-- fails.
--  *AParser> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
--      Just ("","abcdeFGh")
--  *AParser> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
--      Nothing
-- Implement zeroOrMore and oneOrMore with the following type
-- signatures:
-- zeroOrMore :: Parser a -> Parser [a]
-- oneOrMore :: Parser a -> Parser [a]
-- Hint: To parse one or more occurrences
-- of p, run p once and then parse zero or
-- more occurrences of p. To parse zero or
-- more occurrences of p, try parsing one
-- or more; if that fails, return the empty
-- list.

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
-- There are a few more utility parsers needed before we can accomplish
-- the final parsing task. First, spaces should parse a consecutive
-- list of zero or more whitespace characters (use the isSpace function
-- from the standard Data.Char module).
--      spaces :: Parser String
-- Next, ident should parse an identifier, which for our purposes
-- will be an alphabetic character (use isAlpha) followed by zero or
-- more alphanumeric characters (use isAlphaNum). In other words, an
-- identifier can be any nonempty sequence of letters and digits, except
-- that it may not start with a digit.
--      ident :: Parser String
-- For example:
--  *AParser> runParser ident "foobar baz"
--      Just ("foobar"," baz")
--  *AParser> runParser ident "foo33fA"
--      Just ("foo33fA","")
--  *AParser> runParser ident "2bad"
--      Nothing
--  *AParser> runParser ident ""
--      Nothing

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------
-- S-expressions are a simple syntactic format for tree-structured data,
-- originally developed as a syntax for Lisp programs. We’ll close out
-- our demonstration of parser combinators by writing a simple Sexpression
-- parser.
-- An identifier is represented as just a String; the format for valid
-- identifiers is represented by the ident parser you wrote in the previous
-- exercise.
--      type Ident = String
-- An “atom” is either an integer value (which can be parsed with
-- posInt) or an identifier.
--      data Atom = N Integer | I Ident
--                  deriving Show
-- Finally, an S-expression is either an atom, or a list of S-expressions.
-- Actually, this is slightly different than
-- the usual definition of S-expressions
-- in Lisp, which also includes binary
-- “cons” cells; but it’s good enough for
-- our purposes.
--      data SExpr = A Atom
--                 | Comb [SExpr]
--                 deriving Show

-- Textually, S-expressions can optionally begin and end with any
-- number of spaces; after throwing away leading and trailing spaces they
-- consist of either an atom, or an open parenthesis followed by one or
-- more S-expressions followed by a close parenthesis.
--      atom ::= int
--             | ident
--      S ::= atom
--          | (S*)

-- For example, the following are all valid S-expressions:
--  5
--  foo3
--  (bar (foo) 3 5 874)
--  (((lambda x (lambda y (plus x y))) 3) 5)
--  (    lots     of  (    spaces  in )    this ( one )   )

-- We have provided Haskell data types representing S-expressions in
-- SExpr.hs. Write a parser for S-expressions, that is, something of type
--      parseSExpr :: Parser SExpr

-- Hints: To parse something but ignore its output, you can use the
-- (*>) and (<*) operators, which have the types
--      (*>) :: Applicative f => f a -> f b -> f b
--      (<*) :: Applicative f => f a -> f b -> f a
-- p1 *> p2 runs p1 and p2 in sequence, but ignores the result of
-- p1 and just returns the result of p2. p1 <* p2 also runs p1 and p2 in
-- sequence, but returns the result of p1 (ignoring p2’s result) instead.
-- For example:
--  *AParser> runParser (spaces *> posInt) " 345"
--      Just (345,"")


-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
            deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
           deriving Show

parseAtom :: Parser SExpr
parseAtom = A <$> (N <$> posInt <|> I <$> ident)

parseComb :: Parser [SExpr]
parseComb = char '(' *> oneOrMore parseSExpr <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> (Comb <$> parseComb))  <* spaces