module Jana.Parser (
    parseExprString, parseStmtsString, parseString, parseFile,
    parseProgram
    ) where

import Prelude hiding (GT, LT, EQ)
import Control.Monad (liftM, liftM2)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Error as ParsecError
import qualified Text.Parsec.Token as Token

import Jana.Ast
import Jana.Error


toJanaError :: ParsecError.ParseError -> JanaError
toJanaError err =
  newErrorMessage pos (Message $ trim msg)
  where pos = ParsecError.errorPos err
        msg = ParsecError.showErrorMessages
                "or" "Unknown parse error"
                "Expecting" "Unexpected" "end of input"
                (ParsecError.errorMessages err)
        trim = dropWhile isSpace


janaDef = Token.LanguageDef {
                Token.commentStart     = "/*"
              , Token.commentEnd       = "*/"
              , Token.commentLine      = "//"
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum <|> char '_'
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
              , Token.reservedNames    = [ "procedure"
                                         , "int"
                                         , "stack"
                                         , "bool"
                                         , "true"
                                         , "false"
                                         , "if"
                                         , "then"
                                         , "else"
                                         , "fi"
                                         , "from"
                                         , "do"
                                         , "loop"
                                         , "until"
                                         , "push"
                                         , "pop"
                                         , "local"
                                         , "delocal"
                                         , "call"
                                         , "uncall"
                                         , "error"
                                         , "skip"
                                         , "empty"
                                         , "top"
                                         , "size"
                                         , "show"
                                         , "print"
                                         , "printf"
                                         , "nil"
                                         ]
              , Token.caseSensitive    = True
  }

lexer = Token.makeTokenParser janaDef

t_identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
stringLit  = Token.stringLiteral lexer
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
brackets   = Token.brackets   lexer -- parses brackets
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
comma      = Token.comma      lexer -- parses a comma
symbol     = Token.symbol     lexer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

identifier :: Parser Ident
identifier =
  do pos   <- getPosition
     ident <- t_identifier
     return $ Ident ident pos

program :: Parser Program
program =
  do whiteSpace
     (mains, procs) <- liftM partitionEithers (many genProcedure)
     eof
     return $ Program mains procs

genProcedure :: Parser (Either ProcMain Proc)
genProcedure =
  do reserved "procedure"
     id <- identifier
     case id of
       (Ident "main" pos) -> liftM Left  $ mainProcedure pos
       _                  -> liftM Right $ procedure id

mainProcedure :: SourcePos -> Parser ProcMain
mainProcedure pos =
  do parens whiteSpace
     vdecls <- many vdecl
     stats  <- many1 statement
     return $ ProcMain vdecls stats pos

procedure :: Ident -> Parser Proc
procedure name =
  do params <- parens $ sepBy vdecl comma
     stats  <- many1 statement
     return Proc { procname = name, params = params, body = stats }

vdecl :: Parser Vdecl
vdecl =
  do pos    <- getPosition
     mytype <- atype
     ident  <- identifier
     case mytype of
       (Int _) ->     liftM2 (Array ident) (brackets $ optionMaybe integer) (return pos)
                  <|> return (Scalar mytype ident pos)
       _       -> return $ Scalar mytype ident pos


statement :: Parser Stmt
statement =   assignStmt
          <|> ifStmt
          <|> fromStmt
          <|> pushStmt
          <|> popStmt
          <|> localStmt
          <|> callStmt
          <|> uncallStmt
          <|> swapStmt
          <|> errorStmt
          <|> printsStmt
          <|> skipStmt
          <?> "statement"

assignStmt :: Parser Stmt
assignStmt =
  do assign <- try $
       do lv    <- lval
          pos   <- getPosition
          modop <- modOp
          return $ flip (Assign modop lv) pos
     expr  <- expression
     return $ assign expr

modOp :: Parser ModOp
modOp =   (reservedOp "+=" >> return AddEq)
      <|> (reservedOp "-=" >> return SubEq)
      <|> (reservedOp "^=" >> return XorEq)

ifStmt :: Parser Stmt
ifStmt =
  do pos   <- getPosition
     reserved "if"
     entrycond <- expression
     reserved "then"
     stats1    <- many1 statement
     stats2    <- option [] $ reserved "else" >> many1 statement
     reserved "fi"
     exitcond  <- expression
     return $ If entrycond stats1 stats2 exitcond pos

fromStmt :: Parser Stmt
fromStmt =
  do pos   <- getPosition
     reserved "from"
     entrycond <- expression
     stats1    <- option [] $ reserved "do"   >> many1 statement
     stats2    <- option [] $ reserved "loop" >> many1 statement
     reserved "until"
     exitcond  <- expression
     return $ From entrycond stats1 stats2 exitcond pos

pushStmt :: Parser Stmt
pushStmt =
  do pos   <- getPosition
     reserved "push"
     (x,y) <- parens twoArgs
     return $ Push x y pos

popStmt :: Parser Stmt
popStmt =
  do pos   <- getPosition
     reserved "pop"
     (x,y) <- parens twoArgs
     return $ Pop x y pos

twoArgs :: Parser (Ident, Ident)
twoArgs =
  do x <- identifier
     comma
     y <- identifier
     return (x,y)


localStmt :: Parser Stmt
localStmt =
  do pos   <- getPosition
     reserved "local"
     type1  <- atype
     ident1 <- identifier
     reservedOp "="
     expr1  <- expression
     stats  <- many1 statement
     reserved "delocal"
     type2  <- atype
     ident2 <- identifier
     reservedOp "="
     expr2  <- expression
     return $ Local (type1, ident1, expr1) stats (type2, ident2, expr2) pos

atype :: Parser Type
atype =   (reserved "int"   >> liftM Int getPosition)
      <|> (reserved "stack" >> liftM Stack getPosition)

callStmt :: Parser Stmt
callStmt =
  do pos   <- getPosition
     reserved "call"
     procname <- identifier
     args     <- parens $ sepBy identifier comma
     return $ Call procname args pos

uncallStmt :: Parser Stmt
uncallStmt =
  do pos   <- getPosition
     reserved "uncall"
     procname <- identifier
     args     <- parens $ sepBy identifier comma
     return $ Uncall procname args pos

swapStmt :: Parser Stmt
swapStmt =
  do swap <- try $
       do ident1 <- identifier
          pos    <- getPosition
          reservedOp "<=>"
          return $ flip (Swap ident1) pos
     ident2 <- identifier
     return $ swap ident2

errorStmt :: Parser Stmt
errorStmt =
  do pos <- getPosition
     reserved "error"
     liftM (flip UserError pos) (parens stringLit)

printsStmt :: Parser Stmt
printsStmt =   liftM2 (flip Prints) getPosition printStmt
           <|> liftM2 (flip Prints) getPosition printfStmt
           <|> liftM2 (flip Prints) getPosition showStmt

printStmt :: Parser Prints
printStmt =
  do reserved "print"
     str <- parens stringLit
     return $ Print str

printfStmt :: Parser Prints
printfStmt =
  do reserved "printf"
     (a, b) <- parens $ do str <- stringLit
                           identList <- option [] $ comma >> sepBy1 identifier comma
                           return (str, identList)
     return $ Printf a b

showStmt :: Parser Prints
showStmt =
  do reserved "show"
     identList <- parens $ sepBy1 identifier comma
     return $ Show identList

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> liftM Skip getPosition

expression :: Parser Expr
expression = buildExpressionParser binOperators term

addPos :: Parser (SourcePos -> a) -> Parser a
addPos parser = do pos <- getPosition
                   t   <- parser
                   return $ t pos

term :: Parser Expr
term =   parens expression
     <|> addPos numberExpr
     <|> addPos boolExpr
     <|> addPos lvalExpr
     <|> addPos emptyExpr
     <|> addPos topExpr
     <|> addPos sizeExpr
     <|> addPos nilExpr
     <?> "expression"

numberExpr :: Parser (SourcePos -> Expr)
numberExpr = liftM Number integer

boolExpr :: Parser (SourcePos -> Expr)
boolExpr =   (reserved "true"  >> return (Boolean True))
         <|> (reserved "false" >> return (Boolean False))

lvalExpr :: Parser (SourcePos -> Expr)
lvalExpr = liftM LV lval

lval ::  Parser Lval
lval =
  do ident <- identifier
     lookup <- optionMaybe (brackets expression)
     case lookup of
       Just expr -> return $ Lookup ident expr
       Nothing   -> return $ Var ident

nilExpr :: Parser (SourcePos -> Expr)
nilExpr = reserved "nil" >> return Nil

emptyExpr :: Parser (SourcePos -> Expr)
emptyExpr =
  do reserved "empty"
     ident <- parens identifier
     return $ Empty ident

topExpr :: Parser (SourcePos -> Expr)
topExpr =
  do reserved "top"
     ident <- parens identifier
     return $ Top ident

sizeExpr :: Parser (SourcePos -> Expr)
sizeExpr = reserved "size" >> liftM Size (parens identifier)

binOperators = [ [ notChain
                 ]
               , [ binop  "*"   Mul
                 , binop  "/"   Div
                 , binop  "%"   Mod
                 ]
               , [ binop  "+"   Add
                 , binop  "-"   Sub
                 ]
               , [ binop  "<="  LE
                 , binop  "<"   LT
                 , binop  ">="  GE
                 , binop  ">"   GT
                 , binop  "="   EQ
                 , binop  "!="  NEQ
                 ]
               , [ binop' "&"   And '&'
                 , binop' "|"   Or  '|'
                 , binop  "^"   Xor
                 ]
               , [ binop  "&&"  LAnd
                 , binop  "||"  LOr
                 ]
               ]
  where binop  op f     = Infix (reservedOp op >> return (BinOp f)) AssocLeft
        binop' op f not = Infix (try $ reservedOp op >> notFollowedBy (char not) >>
                                       return (BinOp f)) AssocLeft
        notChain        = Prefix $ chainl1 notOp $ return (.)
        notOp           = symbol "!" >> return (UnaryOp Not)

parseString :: Parser a -> String -> a
parseString parser str =
  case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r

parseExprString :: String -> Expr
parseExprString = parseString expression

parseStmtsString :: String -> [Stmt]
parseStmtsString = parseString (many1 statement)

parseFile :: String -> IO (Either JanaError Program)
parseFile file =
  do str <- readFile file
     case parse program file str of
       Left e  -> return $ Left $ toJanaError e
       Right r -> return $ Right r

parseProgram :: String -> String -> Either JanaError Program
parseProgram filename text =
  case parse program filename text of
    Left e  -> Left $ toJanaError e
    Right r -> Right r
