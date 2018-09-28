{-  Name: Sophia Trump
    File: Lexer.hs
    Desc: A Java lexer written in Haskell. Takes a <filename>.java file and
          outputs line-separated tokens to a <filename>.lex file.
          Handles the following token forms (also disregarding comments):
            - Identifier
            - Boolean
            - Null
            - Separator
            - Operator
            - Integer Literal
                - Hexadecimal Literal
                - Binary Literal
                - Octal Literal
                - Decimal Literal
            - Character Literal
            - String Literal
-}

module Main where

import Data.Char

import System.Environment
import System.Exit
import System.FilePath

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ MAIN \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

main :: IO ()
main = do
 args <- getArgs
 filename <- checkArgs args
 input <- readFile filename
 let result = lexJava input
 writeFile (takeBaseName filename <.> "lex") (unlines result)

-- Check the comand-line arguments. Returns the filename
-- to lex upon success.
checkArgs :: [String] -> IO FilePath
checkArgs [path] = pure path
checkArgs _other = do
 putStrLn "Usage: ./Lexer <filename>.java"
 putStrLn "Writes to <filename>.lex"
 exitFailure



-- \\\\\\\\\\\\\\\\\\\\\\\\ CORE LEXING FUNCTIONS \\\\\\\\\\\\\\\\\\\\\\\\\\\

-- Takes Java code as input and returns a list of Strings.
-- Each String in the output list is one Java token.
-- Comments and whitespace are discarded.
lexJava :: String -> [String]
lexJava str = lexNoPrefix (findToken str) -- You will edit this line




-- Returns a tuple of the first, largest possible token at the
-- beginning of the input, and the rest of the string
lex1 :: Char -> String -> (String, String)
lex1 '\0' _ = error "Error: Attempting to lex an unknown token"
lex1 c s
 |(isAlpha first || first == '$' || first == '_') && identtoken /= "" = (identtoken, identrest) -- is it an identifier?
 | separatorToken /= "" = (separatorToken, sepRest) -- is it a separator?
 | optoken /= "" = (optoken, oprest) -- is it an operator?
 | intlitToken /= "" = (intlitToken, intlitRest) -- is it an integer literal?
 | charToken /= "" = (charToken, charTokenRest) -- is it a char literal?
 | strToken /= "" = (strToken, strTokenRest) -- is it a string literal?
 | otherwise = lex1 '\0' ""
 where
   strng = (c:s)
   first = head strng
   last_ = last strng
   (identtoken,identrest) = findIdentifier strng
   (optoken, oprest) = findOperator strng ""
   (separatorToken, sepRest) = findSeparator strng ""
   (intlitToken, intlitRest) = findintLiteral strng
   (charToken, charTokenRest) = findCharLiteral strng
   (strToken, strTokenRest) = findStringLiteral strng




-- Discards whitespace (including comments) at the beginning of
-- the string
findToken :: String -> String
findToken "" = ""
findToken (x:xs)
 | isSpace x = findToken xs
 | (x == '/') && (head xs == '/') = findToken (eolCommentRemove (x:xs))
 | (x == '/') && (head xs == '*') = findToken (multiCommentRemove x (x:xs))
 | otherwise = x : xs

-- Removes single line comments in the form //..comment..\n
eolCommentRemove :: String -> String
eolCommentRemove "" = ""
eolCommentRemove (x:xs)
 | x == '\n' = (xs)
 | otherwise = eolCommentRemove xs

-- Removes multi-line comments in the form /*..comment..*/
multiCommentRemove :: Char -> String -> String
multiCommentRemove _ "" = ""
multiCommentRemove c (x:xs)
 | (c == '*') && x == '/' = (xs)
 | otherwise = multiCommentRemove x xs




-- Assumes that the input	string	begins	with	a	token
-- calls lex1 to lex the first token and then recurs to	lexJava to lex the rest
lexNoPrefix :: String -> [String]
lexNoPrefix "" = []
lexNoPrefix s = alreadyTokenized : (lexJava stillNeedsToBe)
  where (alreadyTokenized, stillNeedsToBe) = lex1 (head s) (drop 1 s)




-- \\\\\\\\\\\\\\\\\\\\\\ HELPER FUNCTIONS FOR lex1 \\\\\\\\\\\\\\\\\\\\\\\\\\

-- INDENTIFIERS -------------------------------------------------------------
-- Returns the largest possible identifier followed by the rest of the string
-- Stops recursing once it encounters a character invalid for identifier
-- NOTE: Also lexes null and boolean literals, as "true(x)" --> "true", "(x)"
-- and "null(x)" --> "null", "(x)". "true1" will still be recognized as an
-- identifier ("true1", ""), as the algorithm is greedy
findIdentifier :: String -> (String,String)
findIdentifier "" = ("", "")
findIdentifier (x:xs)
 | isAlpha x || x == '$' || x == '_' || isDigit x = (x : left, right)
 | otherwise = ("", x : xs)
 where (left, right) = findIdentifier xs




-- OPERATORS -----------------------------------------------------------------
-- Employs an algorithm similar to maxMatch
-- Returns the largest possible operator in the string by checking if the
-- string is an element of a list of possible operators, dropping the last
-- element in the string until there is a match (or the string is empty)
findOperator :: String -> String -> (String, String)
findOperator "" _ = ("", "")
findOperator s t
 | s `elem` opList || s `elem` opList2 = (s, t)
 | otherwise = findOperator (init s) ([last s] ++ t)
   where
     opList = ["=", ">", "<", "!", "~", "?", ":", "->", "==", ">=", "<=", "!=", "&&", "||", "++", "--", "+", "-", "*"]
     opList2 = ["/", "&", "|", "^", "%", "<<", ">>", ">>>", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "%=", "<<=", ">>=", ">>>="]




-- SEPARATORS ---------------------------------------------------------------
-- Returns the largest possible separator in the string (Similar to findOperator)
findSeparator :: String -> String -> (String,String)
findSeparator "" _  = ("","")
findSeparator s t
 | s `elem` separatorList = (s, t)
 | otherwise = findSeparator (init s) ([last s] ++ t)
  where separatorList = ["(", ")", "{", "}", "[", "]", ";", ",", ".", "...", "@", "::"]




-- INTEGER LITERALS ---------------------------------------------------------
-- Returns a tuple of the largest integer literal followed by the rest of the
-- string. Calls helper functions and returns the tuple whose first element
-- is not null and whose original string passes the specifications of said
-- integer literal
findintLiteral :: String -> (String,String)
findintLiteral "" = ("","")
findintLiteral s
 | (passesHexRequirements s (init possibleHexVals)) && (hexToken /= "") =  (first : second : hexToken, hexRest) -- call init on the list passed for checking functions to drop the '_'
 | (passesBinRequirements s (init possibleBinVals)) && (binToken /= "") = (first : second : binToken, binRest)
 | (passesOctRequirements s (init possibleOctVals)) && (octToken /= "") = (first : octToken, octRest)
 | isDigit (first) && (decToken /= "") = (decToken, decRest)
 | otherwise = ("", s)
 where
   first = (head) s
   second = (head ((drop 1) s))
   (hexToken, hexRest) = findNumeral (drop 2 s) possibleHexVals3
   (binToken, binRest) = findNumeral (drop 2 s) possibleBinVals
   (octToken, octRest) = findNumeral (drop 1 s) possibleOctVals
   (decToken, decRest) = findNumeral s possibleDecVals
   possibleHexVals3 = possibleHexVals ++ possibleHexVals2
   possibleHexVals = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C']
   possibleHexVals2 = ['D', 'E', 'F', '_'] -- my text editor won't allow me to create a list long enough to fit all the possible hex vals :(
   possibleBinVals = ['0', '1', '_']
   possibleOctVals = ['0', '1', '2', '3', '4', '5', '6', '7', '_']
   possibleDecVals = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_']

-- The function that actually pulls out the integer literal from the rest
-- of the string. (Used for all int literal building -- hex, bin, octal,
-- and decimal) Takes the string and the list of valid characters for that
-- literal.
findNumeral :: String -> [Char] -> (String,String)
findNumeral "" _ = ("","")
findNumeral (x:xs) ye
 | ((x == 'l') || (x == 'L')) = (x : "", xs)
 | (x == '_') && ((length(xs) == 0) || (nonUnderScoreCount == 0)) = ("", x:xs)
 | (x `elem` ye) = (x : left, right)
 | otherwise = ("", x:xs)
  where
    (left, right) = findNumeral xs ye
    nonUnderScoreCount = nonUnderScoreLeft xs ye

-- Returns the number of valid characters for a certain literal by taking the
-- string and a list of valid characters. Used to determine if an underscore
-- should be added to the literal in findNumeral
nonUnderScoreLeft :: [Char] -> [Char] -> Int
nonUnderScoreLeft "" _ = 0
nonUnderScoreLeft (x:xs) list
 | (x `elem` list) && (x /= '_') = (nonUnderScoreLeft xs list) + 1
 | otherwise = nonUnderScoreLeft xs list

-- Takes a string and a list of valid hex characters
-- and returns True if that string matches the specifications for hex literals.
passesHexRequirements :: String -> [Char] -> Bool
passesHexRequirements "" _ = False
passesHexRequirements s ye
 | ((length s >= 3) && (first == '0') && ((second == 'X') || (second == 'x')) && (third /= '_') && ((third `elem` ye))) = True
 | otherwise = False
 where
   first = (head) s
   second = (head ((drop 1) s))
   third = (head ((drop 2) s))

-- Takes a string and a list of valid binary characters
-- and returns True if that string matches the specifications for bin literals
passesBinRequirements :: String -> [Char] -> Bool
passesBinRequirements "" _ = False
passesBinRequirements s ye
  | ((length s >= 3) && (first == '0') && ((second == 'B') || (second == 'b')) && (third /= '_') && (third `elem` ye)) = True
  | otherwise = False
  where
    first = (head) s
    second = (head ((drop 1) s))
    third = (head ((drop 2) s))

-- Takes a string and a list of valid octal characters
-- and returns True if that string matches the specifications for oct literals
passesOctRequirements :: String -> [Char] -> Bool
passesOctRequirements "" _ = False
passesOctRequirements s ye
 | ((length s >= 2) && (first == '0') && (((second == '_') && (third `elem` ye)) || (second `elem` ye))) = True
 | otherwise = False
  where
   first = (head) s
   second = (head ((drop 1) s))
   third = (head ((drop 2) s))




-- CHARACTER LITERALS ------------------------------------------------------
-- Returns the largest possible char literal and the rest of the string as a
-- tuple. Checks that the substring is valid for char literals, and calls
-- insertcharChar to pull out the literal if it does.
findCharLiteral :: String -> (String,String)
findCharLiteral "" = ("","")
findCharLiteral s
  | (first /= '\'') = ("", s)
  | (length s >= 3) && isAlphaNum second && (third == '\'') = ('\'' : charTok, (drop 3) s)
  | (length s >= 4) &&(second_third `elem` escapeSequence) && (fourth == '\'') = ('\'' : charTok, (drop 4) s)
  | (length s >= 4) && (second == '\\') && (third_octal) && (fourth == '\'') = ('\'' : charTok, theRest)
  | (length s >= 5) && (second == '\\') && (third_octal) && (fourth_octal) && (fifth == '\'') = ('\'' : charTok, theRest)
  | (length s >= 6) && (second == '\\') && (third_123) && (fourth_octal) && (fifth_octal) && (sixth == '\'') = ('\'' : charTok, theRest)
  | otherwise = ("", "")
     where
         first = head s
         second = head ((drop 1) s)
         third = head ((drop 2) s)
         second_third = [second] ++ [third]
         fourth = head ((drop 3) s)
         fifth = head ((drop 4) s)
         sixth = head ((drop 5) s)
         seventh = head ((drop 6) s)
         third_123 = (third == '1') || (third == '2') || (third == '3')
         third_octal = third `elem` octalDigs
         fourth_octal = fourth `elem` octalDigs
         fifth_octal = fifth `elem` octalDigs
         (charTok, theRest) = insertcharChar ((drop 1) s)
         octalDigs = ['1', '2', '3', '4', '5', '6', '7']
         escapeSequence = ["\\b", "\\n", "\\t", "\\f", "\\r", "\\\"", "\\'", "\\\\"]

-- Returns a tuple of the char literal and the rest of the string.
-- Called by findCharLiteral
insertcharChar :: String -> (String, String)
insertcharChar "" = ("", "")
insertcharChar (s:st)
 | s /= '\'' = (s : left, right)
 | otherwise = ("" ++ [s], st)
  where
     (left, right) = insertcharChar st




-- STRING LITERALS -----------------------------------------------------------
-- Returns the largest possible string literal and the rest of the string as a
-- tuple. Checks that the substring is valid for string literals, (including
-- that the string is terminated), and calls insertstrChars if it does.
findStringLiteral :: String -> (String, String)
findStringLiteral "" = ("", "")
findStringLiteral s
  | (first /= '\"') = ("", s)
  | invalid = ("", s)
  | (length s >= 4) && (second_third `elem` escapeSequence) && (fourth == '\"') = ('\"' : strTok, (drop 4) s)
  | (length s >= 4) && (second == '\\') && (third_octal) && (fourth == '\"') = ('\"' : strTok, theRest)
  | (length s >= 5) && (second == '\\') && (third_octal) && (fourth_octal) && (fifth == '\"') = ('\"' : strTok, theRest)
  | (length s >= 6) && (second == '\\') && (third_123) && (fourth_octal) && (fifth_octal) && (sixth == '\"') = ('\"' : strTok, theRest)
  | (strTok /= "") = ('\"' : strTok, theRest)
  | otherwise = ("", "")
   where
     (strTok, theRest) = insertstrChars ((drop 1) s) escapeSequence
     invalid = notTerminated guttedString
     guttedString = getOnlyQuotes s escapeSequence
     first = head s
     second = head ((drop 1) s)
     third = head ((drop 2) s)
     second_third = [second] ++ [third]
     fourth = head ((drop 3) s)
     fifth = head ((drop 4) s)
     sixth = head ((drop 5) s)
     seventh = head ((drop 6) s)
     third_123 = (third == '1') || (third == '2') || (third == '3')
     third_octal = third `elem` octalDigs
     fourth_octal = fourth `elem` octalDigs
     fifth_octal = fifth `elem` octalDigs
     octalDigs = ['1', '2', '3', '4', '5', '6', '7']
     escapeSequence = ["\\b", "\\n", "\\t", "\\f", "\\r", "\\\"", "\\'", "\\\\"]

-- Gets rid of escape sequences inside the string so that only the terminations
-- of the string are left.
-- Takes the string and a list of escape sequences, and returns the gutted string
getOnlyQuotes :: String -> [String] -> String
getOnlyQuotes "" _ = ""
getOnlyQuotes (x:xs) escapes
 | ([x] ++ [head xs]) `elem` escapes = getOnlyQuotes((drop 1) xs) escapes
 | isAlphaNum x = getOnlyQuotes xs escapes
 | otherwise = x : getOnlyQuotes xs escapes

-- Checks if a string is terminated
notTerminated :: String -> Bool
notTerminated "" = True
notTerminated s
 | length s < 2 = True
 | stringEndings /= 2 = True
 | otherwise = False
    where
      stringEndings = length(filter (== '\"') s)

-- Returns a tuple of the string literal and the rest of the string.
-- Called by findStringLiteral
insertstrChars :: String -> [String] -> (String, String)
insertstrChars "" _ = ("", "")
insertstrChars (s:st) escapes
  | (length st > 1) && [s] ++ [head st] `elem` escapes = (s : (head st) : left, (drop 1) right)
  | s /= '\"' = (s : left, right)
  | otherwise = ("" ++ [s], st)
  where
    (left, right) = insertstrChars st escapes
