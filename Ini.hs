module Ini where

import Data.List
import Data.Char
import qualified Data.Map.Strict as Map

import Control.Monad

import Text.Parsec
import Text.Parsec.String

{- INI file parser

An INI file looks like this:

; comments go after semicolons

; Settings which are not in a section have "" (empty string) as section. (In the map: ".settingWithoutSection")
settingWithoutSection = "Quoted string"

[section]
setting = No
setting2=Yes
setting3: True

; a setting w/o value is given the value True.
implicitlyTrueSetting

[section2]
setting = 32.0

-}

type INIMap = Map.Map String Value

type SectionName = String
type Key = (SectionName,String)
data Value = INIBool Bool | INIString String | ININumber Double

type INIStruct = [(Key, Value)]

instance Show Value where
    show (INIBool b) = show b
    show (INIString s) = s
    show (ININumber d) = show d

parseINI :: String -> String -> Either ParseError INIStruct
parseINI = parse iniP

iniP :: Parser INIStruct
iniP =   try (comment >> iniP) -- Discard comments
     <|> try (emptyLine >> iniP) -- Discard empty lines
     <|> try (do
                secsettings <- section -- Section begin detected? Read settings.
                rest <- iniP -- Parse rest of the file
                return (secsettings ++ rest)
             )
     <|> try (do
                set <- setting ""
                rest <- iniP
                return (set : rest)
             )
     <|> (eof >> return []) -- End of file detected.

emptyLine :: Parser ()
emptyLine = many (oneOf " \t") >> eol

comment :: Parser ()
comment = do
    char ';'
    many $ noneOf "\n"
    eol <|> eof
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
    val <- (try settingAfterName <|> return (INIBool True))
    return ((sec,k),val)

settingAfterName :: Parser Value
settingAfterName = do
    separator
    val <- value
    afterValue
    return val

key :: Parser String
key = many1 (noneOf " :=[\n")

separator :: Parser ()
separator = void $ many1 (oneOf " :=")

value :: Parser Value
value = liftM readValue (try quotedString)
    <|> liftM readValue (many1 (noneOf " \n;"))

afterValue :: Parser ()
afterValue = void $ skipMany (oneOf " \t") >> choice [comment,eol,eof]

quotedString :: Parser String
quotedString = do
    char '"'
    str <- many (noneOf "\"")
    char '"'
    return str

eol = void newline

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

iniStructToMap :: INIStruct -> INIMap
iniStructToMap = foldl' (\m ((sec,k),v) -> Map.insert (sec++"."++k) v m) Map.empty

