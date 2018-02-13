{
{- 
  Scanner.y

  Archivo de expresiones regulares de Alex, el cual es un generador de scanners léxicos basado en Lex.
  El archivo contiene los tokens y las expresiones regulares del lenguaje Objective Oriented Haskell. Para poder correrlo, se ocupa GHCI instalado.

  Autor: Eduardo Serna
-}
module Scanner where
import Data.Decimal

}

%wrapper "posn"

$digit = 0-9			-- digitos
$alphaLower = [a-z]		-- caracteres alfabeticos
$alphaUpper = [A-Z]
-- Se definen los tokens de la gramática
tokens :-
  $white+	; --ignorar todos los espacios, newlines y caracteres vacios
  "="                               { \p s -> TEquals p }
  ","                               { \p s -> TComma p }
  "if"                              { \p s -> TIf p }
  "else"                            { \p s -> TElse p }
  "case"                            { \p s -> TCase p }
  "of"                              { \p s -> TOf p }
  "otherwise"                       { \p s -> TOtherwise p }
  "for"                             { \p s -> TFor p }
  "while"                           { \p s -> TWhile p }
  "read"                            { \p s -> TRead p }
  "display"                         { \p s -> TDisplay p }
  "+"                               { \p s -> TPlus p }
  "-"                               { \p s -> TMinus p }
  "*"                               { \p s -> TMultiply p }
  "/"                               { \p s -> TDivide p }
  "++"                              { \p s -> TPlusPlus p }
  "--"                              { \p s -> TMinusMinus p }
  "^"                               { \p s -> TPower p }
  "!"                               { \p s -> TNot p }
  "true"                            { \p s -> TTrue p }
  "false"                           { \p s -> TFalse p }
  ">"                               { \p s -> TGreaterThan p }
  "<"                               { \p s -> TLessThan p }
  ">="                              { \p s -> TGreaterEqualThan p }
  "<="                              { \p s -> TLessEqualThan p }
  "=="                              { \p s -> TDoubleEqual p }
  "!="                              { \p s -> TNotEqual p }
  "&&"                              { \p s -> TAnd p }
  "||"                              { \p s -> TOr p }
  "main"                            { \p s -> TMain p }
  "class"                           { \p s -> TClass p }
  ":"                               { \p s -> TColon p }
  "[+]"                             { \p s -> TPublic p }
  "[-]"                             { \p s -> TPrivate p }
  "=>"                              { \p s -> TEqualsRightArrow p }
  "->"                              { \p s -> TDashRightArrow p }
  "::"                              { \p s -> TDoubleColon p }
  "["                               { \p s -> TLeftBracket p }
  "]"                               { \p s -> TRightBracket p }
  "List"                            { \p s -> TList p }
  ".."                              { \p s -> TDoublePoint p }
  ";"                               { \p s -> TSemiColon p }
  "{"                               { \p s -> TLeftBrace p }
  "}"                               { \p s -> TRightBrace p }
  "("                               { \p s -> TLeftParen p }
  ")"                               { \p s -> TRightParen p }
  "Int"                             { \p s -> TInt p }
  "Integer"                         { \p s -> TInteger p }
  "Double"                          { \p s -> TDouble p }
  "Money"                           { \p s -> TMoney p }
  "String"                          { \p s -> TString p }
  $digit+\.$digit+                  { \p s -> TDecimalLiteral p (read ( (take 255 s) ) ) } -- numero decimal
  $digit+                           { \p s -> TIntegerLiteral p (read s) }                 -- numero entero
  [$alphaLower \_][$alphaLower $digit $alphaUpper \_]*   { \p s -> TVarIdent p s }
  [$alphaUpper][$alphaLower]*                            { \p s -> TClassIdent p s }      
  \"(\\. | [^\"\\])*\" 	       	                         { \p s -> TStringLiteral p (init (tail s)) } -- esto hace que se remuevan las dobles comillas al momento de procesar el string literal

{
-- Cada acción tiene un tipo de ::AlexPosn -> String -> Token

-- El tipo de dato token tiene el tipo correspondiente más el AlexPosN, lo cual ayuda a reporte de errores:
data Token =
      TEquals AlexPosn              |
      TComma AlexPosn               |
      TIf AlexPosn                  |
      TElse AlexPosn                |
      TCase AlexPosn                |
      TOtherwise AlexPosn           |
      TFor AlexPosn                 |
      TWhile AlexPosn               |
      TRead AlexPosn                |
      TDisplay AlexPosn             |
      TPlus AlexPosn                |
      TMinus AlexPosn               |
      TMultiply AlexPosn            |
      TDivide AlexPosn              |
      TPlusPlus AlexPosn            |
      TMinusMinus AlexPosn          |
      TPower AlexPosn               |
      TNot AlexPosn                 |
      TTrue AlexPosn                |
      TFalse AlexPosn               |
      TGreaterThan AlexPosn         |
      TLessThan AlexPosn            |
      TGreaterEqualThan AlexPosn    |
      TLessEqualThan AlexPosn       |
      TDoubleEqual AlexPosn         |
      TNotEqual AlexPosn            |
      TAnd AlexPosn                 |
      TOr AlexPosn                  |
      TMain AlexPosn                |
      TClass AlexPosn               |
      TColon AlexPosn               |
      TPublic AlexPosn              |
      TPrivate AlexPosn             |
      TEqualsRightArrow AlexPosn    |
      TDashRightArrow AlexPosn      |
      TDoubleColon AlexPosn         |
      TLeftBracket AlexPosn         |
      TRightBracket AlexPosn        |
      TList AlexPosn                |
      TDoublePoint AlexPosn         |
      TSemiColon AlexPosn           |
      TLeftBrace AlexPosn           |
      TRightBrace AlexPosn          |
      TLeftParen AlexPosn           |
      TRightParen AlexPosn          |
      TInt AlexPosn                 |
      TInteger AlexPosn             |
      TDouble AlexPosn              |
      TMoney AlexPosn               |
      TString AlexPosn              |
      TDecimalLiteral AlexPosn Decimal |
      TIntegerLiteral AlexPosn Int  |
      TVarIdent AlexPosn String     |
      TClassIdent AlexPosn String   |
      TStringLiteral AlexPosn String                 
  deriving (Eq,Show)

-- A continuacion se definen las funciones de tokenPosn para cada tipo de dato. Esto ayuda a reportar errores de léxico con precisión
tokenPosn (TEquals p) = p
tokenPosn (TComma p) = p
tokenPosn (TIf p) = p
tokenPosn (TElse p) = p
tokenPosn (TCase p) = p
tokenPosn (TOtherwise p) = p
tokenPosn (TFor p) = p
tokenPosn (TWhile p) = p
tokenPosn (TRead p) = p
tokenPosn (TDisplay p) = p
tokenPosn (TPlus p) = p
tokenPosn (TMinus p) = p
tokenPosn (TMultiply p) = p
tokenPosn (TDivide p) = p
tokenPosn (TPlusPlus p) = p
tokenPosn (TMinusMinus p) = p
tokenPosn (TPower p) = p
tokenPosn (TNot p) = p
tokenPosn (TTrue p) = p
tokenPosn (TFalse p) = p
tokenPosn (TGreaterThan p) = p
tokenPosn (TLessThan p) = p
tokenPosn (TGreaterEqualThan p) = p
tokenPosn (TLessEqualThan p) = p
tokenPosn (TDoubleEqual p) = p
tokenPosn (TNotEqual p) = p
tokenPosn (TAnd p) = p
tokenPosn (TOr p) = p
tokenPosn (TMain p) = p
tokenPosn (TClass p) = p
tokenPosn (TColon p) = p
tokenPosn (TPublic p) = p
tokenPosn (TPrivate p) = p
tokenPosn (TEqualsRightArrow p) = p
tokenPosn (TDashRightArrow p) = p
tokenPosn (TDoubleColon p) = p
tokenPosn (TLeftBracket p) = p
tokenPosn (TRightBracket p) = p
tokenPosn (TList p) = p
tokenPosn (TDoublePoint p) = p
tokenPosn (TSemiColon p) = p
tokenPosn (TLeftBrace p) = p
tokenPosn (TRightBrace p) = p
tokenPosn (TLeftParen p) = p
tokenPosn (TRightParen p) = p
tokenPosn (TInt p) = p
tokenPosn (TInteger p) = p
tokenPosn (TDouble p) = p
tokenPosn (TMoney p) = p
tokenPosn (TString p) = p
tokenPosn (TDecimalLiteral p num) = p
tokenPosn (TIntegerLiteral p num) = p
tokenPosn (TVarIdent p str) = p
tokenPosn (TClassIdent p str) = p
tokenPosn (TStringLiteral p str) = p

-- Esta funcion permite conseguir la linea actual donde está la cabeza lectora
getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum 

-- Esta funcion permite conseguir la columna actual donde está la cabeza lectora
getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn offset lineNum colNum) = colNum


-- A continuación, la siguiente función monad se encarga de leer el input de entrada str
alexScanTokens2 str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error ("lexical error @ line " ++ show (getLineNum(pos)) ++ " and column " ++ show (getColumnNum(pos)))
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
}
