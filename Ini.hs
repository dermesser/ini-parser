module Ini where

import Data.List
import Data.Char

import Control.Monad

import Text.Parsec
import Text.Parsec.String

{- INI file parser

An INI file looks like this:

; comment
[section]
setting = No
setting2=Yes
setting3: True

[section2]
setting = 32.0

-}

type SectionName = String
type Key = (SectionName,String)
data Value = INIBool Bool | INIString String | ININumber Double

type INIStruct = [(Key, Value)]

instance Show Value where
    show (INIBool b) = show b
    show (INIString s) = s
    show (ININumber d) = show d

parseINI :: String -> String -> Either ParseError INIStruct
parseINI name cont = parse iniP name cont

iniP :: Parser INIStruct
iniP =   try (comment >> iniP) -- Discard comments
     <|> try (emptyLine >> iniP) -- Discard empty lines
     <|> try (do
                secsettings <- section -- Section begin detected? Read settings.
                rest <- iniP -- Parse rest of the file
                return (secsettings ++ rest)
             )
     <|> (eof >> return []) -- End of file detected.

emptyLine :: Parser ()
emptyLine = many (oneOf " \t") >> eol

comment :: Parser ()
comment = do
    char ';'
    many $ noneOf "\n"
    eol
    return ()

section :: Parser INIStruct
section = do
    char '['
    sec <- many1 alphaNum
    char ']'
    eol
    many (setting sec)

setting :: SectionName -> Parser (Key, Value)
setting sec = try $ do
    many emptyLine
    k <- key
    separator
    val <- value
    afterValue
    return ( (sec,k) , val)

key :: Parser String
key = many1 (noneOf " :=[\n")

separator :: Parser ()
separator = many1 (oneOf " :=") >> return ()

value :: Parser Value
value = (try quotedString >>= return . readValue)
    <|> (many1 (noneOf " \n;") >>= return . readValue)

afterValue :: Parser ()
afterValue = skipMany (oneOf " \t") >> choice [comment,eol,eof] >> return ()

quotedString :: Parser String
quotedString = do
    char '"'
    str <- many (noneOf "\"")
    char '"'
    return str

eol = newline >> return ()

readValue :: String -> Value
readValue str | s == "yes" || s == "true"  = INIBool True
              | s == "no"  || s == "false" = INIBool False
              | all isNumber s = ININumber (read s)
              | otherwise = INIString str
    where s = map toLower str


-- Output code


cmp = compare

-- Sort the settings by section,key
sortPredINI :: (Key,Value) -> (Key,Value) -> Ordering
sortPredINI ((sec,k),_) ((sec2,k2),_) = if EQ == (sec `cmp` sec2)
                                         then k `cmp` k2 
                                         else sec `cmp` sec2

-- Predicate for grouping settings in the same section together
groupPredINI :: (Key,Value) -> (Key,Value) -> Bool
groupPredINI ((sec,_),_) ((sec2,_),_) = sec == sec2

-- Render INI configuration text
printINI :: INIStruct -> String
printINI i = concatMap printSec sortedINI
    where sortedINI = groupBy groupPredINI $ sortBy sortPredINI i -- sort and group the settings
          printSec sets@(x:xs) = let ((sec,_),_) = x in    -- Obtain section name
                                    "[" ++ sec ++ "]\n" ++ -- Section title
                                    concatMap printKV sets -- Settings in this section
          printKV ((_,k),v) = k ++ " = " ++ show v ++ "\n" -- Render the single settings

