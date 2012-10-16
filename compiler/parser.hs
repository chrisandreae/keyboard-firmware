module Parser where
import Text.Printf

import Control.Applicative((<$>), (<*>), (<*), (*>))
import Control.Monad
import Control.Monad.Error

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Language as Language

import Data.Int
import Data.List(sortBy)

import BasicTypes
import Errors

data Declaration = MethodDeclaration Type Ident [(Type, Ident)] [Statement]
                 | GlobalVariableDeclaration Type Ident

data Statement = Block [Statement]
               | IfStatement Expression Statement (Maybe Statement)
               | WhileStatement Expression Statement
               | ForStatement Expression Expression Expression Statement
               | VariableDeclarationStatement Type Ident (Maybe Expression)
               | ExpressionStatement Expression
               | ReturnValueStatement Expression
               | ReturnStatement
               | ExitStatement
               | BreakStatement
               | ContinueStatement

data Expression = Assignment Expression Expression -- parser ensures LHS is lvalue
                | PrefixExpression PrefixOp Expression
                | PostfixExpression Expression PostfixOp
                | BinaryExpression Expression BinaryOp Expression
                | ShortLiteral Int16
                | ByteLiteral Int8
                | VariableAccess Ident
                | MethodInvocation Ident [Expression]
                | TypeCast Type Expression

data PrefixOp = Minus | Not | Complement | Predecrement | Preincrement
              deriving (Eq)
data PostfixOp = Postdecrement | Postincrement
               deriving (Eq)
data BinaryOp = Add | Subtract | Multiply | Divide | Mod
              | And | Or | Xor | Lshift | Rshift
              | Gt | Ge | Lt | Le | Eq | Ne | Conj | Disj
              deriving (Eq)

-- show instance
showlistwithsep :: Show a => String -> [a] -> String
showlistwithsep sep [] = ""
showlistwithsep sep l = (foldl1 ((++).(++ sep))) . (map show) $ l

instance Show Declaration where
  show (MethodDeclaration typ nam args body) = printf "%s %s(%s){\n%s\n}\n" (show typ) nam (showargs args) (showlistwithsep "\n" body)
    where showargs = (showlistwithsep ", ").(map (\ (a,b)-> (show a) ++ " " ++ (show b)))
  show (GlobalVariableDeclaration typ nam)   = printf "%s %s;\n" (show typ) nam

instance Show Statement where
  show (Block body)                                = printf "{\n%s\n}" (showlistwithsep "\n" body)
  show (IfStatement expr tbody fbody)              = printf "if(%s)%s%s" (show expr) (show tbody) (maybe "" (("else "++).show) fbody)
  show (WhileStatement expr body)                  = printf "while(%s)%s"  (show expr) (show body)
  show (ForStatement init cond iter body)          = printf "for(%s; %s; %s)%s" (show init) (show cond) (show iter) (show body)
  show (VariableDeclarationStatement typ nam init) = printf "%s %s%s;" (show typ) nam (maybe "" ((" = "++).show) init)
  show (ExpressionStatement expr)                  = printf "%s;" (show expr)
  show (ReturnValueStatement expr)                 = printf "return %s;" (show expr)
  show ReturnStatement                             = "return;"
  show ExitStatement                               = "exit;"
  show BreakStatement                              = "break;"
  show ContinueStatement                           = "continue;"

pshow :: (Ord a, Show a) => a -> a -> String
pshow me sub | me>=sub   = printf "(%s)" $ show sub
             | otherwise = show sub

instance Show Expression where
  show me@(Assignment l r)            = printf "%s = %s" (show l) (pshow me r)
  show me@(PrefixExpression op ex)    = (show me) ++ (pshow me ex)
  show me@(PostfixExpression ex op)   = (pshow me ex) ++ (show op)
  show me@(BinaryExpression l op r)   = printf "%s %s %s" (pshow me l) (show op) (pshow me r)
  show me@(ShortLiteral v)            = (show v) ++ "S"
  show me@(ByteLiteral b)             = show b
  show me@(MethodInvocation nam args) = printf "%s(%s)" nam ((showlistwithsep ", ") args)
  show me@(TypeCast typ expr)         = printf "(%s)%s" (show typ) (pshow me expr)
  show me@(VariableAccess v)          = show v

instance Show PrefixOp where
  show Not = "!"
  show Predecrement = "--"
  show Preincrement = "++"
  show Complement = "~"
  show Minus = "-"

instance Show PostfixOp where
  show Postdecrement = "--"
  show Postincrement = "++"

instance Show BinaryOp where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Mod = "%"
  show And = "&"
  show Or = "|"
  show Xor = "^"
  show Gt = ">"
  show Ge = ">="
  show Lt = "<"
  show Le = "<="
  show Eq = "=="
  show Ne = "!="
  show Lshift = "<<"
  show Rshift = ">>"
  show Conj = "&&"
  show Disj = "||"

-- expression priority
instance Eq Expression where
    a == b = (priority a) == (priority b)

instance Ord Expression where
  a <= b = (priority a) <= (priority b)

priority :: Expression -> Integer
priority (Assignment _ _)                = 0

priority (BinaryExpression _ Disj _)     = 1
priority (BinaryExpression _ Conj _)     = 2

priority (BinaryExpression _ Or _)       = 3
priority (BinaryExpression _ Xor _)      = 4
priority (BinaryExpression _ And _)      = 5

priority (BinaryExpression _ Eq _)       = 6
priority (BinaryExpression _ Ne _)       = 6

priority (BinaryExpression _ Gt _)       = 7
priority (BinaryExpression _ Ge _)       = 7
priority (BinaryExpression _ Lt _)       = 7
priority (BinaryExpression _ Le _)       = 7

priority (BinaryExpression _ Add _)      = 8
priority (BinaryExpression _ Subtract _) = 8

priority (BinaryExpression _ Multiply _) = 9
priority (BinaryExpression _ Divide _)   = 9
priority (BinaryExpression _ Mod _)      = 9

priority (PrefixExpression _ _)          = 10

priority _                               = 11

-- parser

convertError :: Either ParseError a -> ThrowsError a
convertError (Left p) = Left $ ParsingError (show p)
convertError (Right x) = Right x

parseProgram :: String -> ThrowsError [Declaration]
parseProgram p = do
  decs <- convertError $ parse program "" p
  sdecs <- return $ sortBy mainFirst decs
  unless (isMain $ head sdecs) $ throwError NoMainError
  return sdecs
  where
    isMain (MethodDeclaration _ n _ _) = n == "main"
    isMain _ = False
    mainFirst a _ | isMain a = LT
    mainFirst _ b | isMain b = GT
    mainFirst _ _ = EQ

-- Use Parsec.Token and Parsec.Language to generate low level parsers

languageDef = Language.emptyDef { Token.commentStart = "/*"
                                , Token.commentEnd = "*/"
                                , Token.commentLine = "//"
                                , Token.nestedComments = True
                                , Token.identStart = letter <|> char '_'
                                , Token.identLetter = alphaNum <|> char '_'
                                , Token.opStart = oneOf "+-*/%&|^><=!~"
                                , Token.opLetter = oneOf "+-*/%&|^><=!~"
                                , Token.reservedNames = ["byte", "short", "void", "for", "while", "if", "return", "break", "continue", "true", "false"]
                                , Token.reservedOpNames = ["++", "--", "!", "||", "&&",
                                                           "+", "-", "*", "/", "%",
                                                           "&", "|", "^", "~", "<<", ">>",
                                                           ">", ">=", "<", "<=", "==", "!=",
                                                           "="]
                                , Token.caseSensitive = True
                                }

lexer = Token.makeTokenParser languageDef

whitespace	= Token.whiteSpace lexer
lexeme		= Token.lexeme lexer
parens		= Token.parens lexer
braces		= Token.braces lexer
identifier	= Token.identifier lexer
integer		= Token.integer lexer
decimal         = Token.decimal lexer
hexadecimal     = Token.hexadecimal lexer
octal           = Token.octal lexer
charLiteral     = Token.charLiteral lexer
operator	= Token.operator lexer
reserved	= Token.reserved lexer
reservedOp      = Token.reservedOp lexer
semi		= Token.semi lexer
semiSep		= Token.semiSep lexer
semiSep1	= Token.semiSep1 lexer
commaSep	= Token.commaSep lexer
commaSep1	= Token.commaSep1 lexer

-- And build a parser with that

program :: GenParser Char st [Declaration]
program = do { whitespace;
               declarations <- many declaration;
               eof;
               return declarations;
             }

reservedp :: Show a => a -> GenParser Char st a
reservedp rt = (const rt) <$> reserved (show rt)

reservedpChoice :: Show a => [a] -> GenParser Char st a
reservedpChoice = choice . (map reservedp)

typep :: GenParser Char st Type
typep = reservedpChoice [Byte, Short, Void]

declaration :: GenParser Char st Declaration
declaration = do { typ <- typep;
                   nam <- identifier;
                   do { args <- parens $ commaSep $ (,) <$> typep <*> identifier;
                        body <- braces $ many statement;
                        return $ MethodDeclaration typ nam args body
                      }
                   <|>
                   (semi >> (return $ GlobalVariableDeclaration typ nam))
                 }

statement :: GenParser Char st Statement
statement = (Block <$> (braces $ many statement)
             <|> ifStatement
             <|> whileStatement
             <|> forStatement
             <|> returnStatement <* semi
             <|> varDeclStatement <* semi
             <|> (reserved "exit"     >> return ExitStatement)     <* semi
             <|> (reserved "break"    >> return BreakStatement)    <* semi
             <|> (reserved "continue" >> return ContinueStatement) <* semi
             <|> (ExpressionStatement <$> expression) <* semi) <* skipMany semi
  where
    ifStatement :: GenParser Char st Statement
    ifStatement = do { reserved "if";
                       expr <- parens expression;
                       body <- statement;
                       elsebody <- optionMaybe ((reserved "else") *> statement);
                       return $ IfStatement expr body elsebody
                     }
    whileStatement = reserved "while" >> WhileStatement <$> parens expression <*> statement
    forStatement = reserved "for" >> parens (ForStatement <$> expression <* semi <*> expression <* semi <*> expression) <*> statement
    returnStatement = reserved "return" *> (( ReturnValueStatement <$> expression) <|> return ReturnStatement)
    varDeclStatement = VariableDeclarationStatement <$> typep <*> identifier <*> optionMaybe (reservedOp "=" *> expression)

opP :: Show t => (t -> b) -> t -> GenParser Char st b
opP constr op = (const $ constr op) <$> reservedOp (show op)

opChoice :: Show t => (t -> b) -> [t] -> GenParser Char st b
opChoice constr xs = choice $ map (opP constr) xs

binOp = opP (flip BinaryExpression)
prefixOp = opP (PrefixExpression)
binOpChoice = opChoice (flip BinaryExpression)
prefixOpChoice = opChoice (PrefixExpression)
postfixOpChoice = opChoice (flip PostfixExpression)

prefix :: GenParser a b (c->c) -> GenParser a b c -> GenParser a b c -> GenParser a b c
prefix op ifp elsep = (op <*> ifp) <|> elsep

bindStar :: (a -> GenParser c b a) -> (a -> GenParser c b a)
bindStar p = \a -> ((p a) >>= bindStar p)
                   <|> return a

lvalue :: GenParser Char st Expression
lvalue = VariableAccess <$> identifier

expression :: GenParser Char st Expression
expression = eAssig
  where
    eAssig = prefix (Assignment <$> (try $ lvalue <* reservedOp "=")) eAssig eDisj
    eDisj = chainl1 eConj (binOp Disj)
    eConj = chainl1 e1 (binOp Conj)
    e1 = chainl1 e2 (binOp Or)
    e2 = chainl1 e3 (binOp Xor)
    e3 = chainl1 e4 (binOp And)
    e4 = chainl1 e5 $ binOpChoice [Eq, Ne]
    e5 = chainl1 e6 $ binOpChoice [Lt, Gt, Le, Ge]
    e6 = chainl1 e7 $ binOpChoice [Lshift, Rshift]
    e7 = chainl1 e8 $ binOpChoice [Add, Subtract]
    e8 = chainl1 e9 $ binOpChoice [Multiply, Divide, Mod]
    e9 = prefix (prefixOpChoice [Complement, Minus, Not, Predecrement, Preincrement]) e9 e10  -- bug: this prevents us from parsing -128, as we get (Minus (ByteLiteral 128)), which is out of range.
    e10 = prefix (TypeCast <$> (try $ parens typep)) e10 e11
    e11 = e12 >>= bindStar (\ex -> postfixOpChoice [Postincrement, Postdecrement] <*> return ex)
    e12 = parens expression <|> primary
    primary = literal <|> methorvar
    literal = do
      (val, isSigned) <-  do { u <- try $ char '0' >> (hexadecimal <|> octal ); return (u, False) }
                          <|>
                          do { i <- decimal; ((lexeme $ char 'u') >> return (i, False)) <|> return (i, True) }
      isShort <- ((lexeme $ char 's') >> return True) <|> return False
      whitespace
      let (lbound, ubound) | isShort && isSigned = (-2^15, 2^15)
                           | isShort             = (0,     2^16)
                           | isSigned            = (-2^7,  2^7)
                           | otherwise           = (0,     2^8)
      let constr | isShort  = ShortLiteral . fromIntegral
                 | otherwise = ByteLiteral . fromIntegral
      unless ((val >= lbound) && (val < ubound)) $ unexpected $ printf "out of bounds %s %s literal (%d)" (if isSigned then "signed" else "unsigned") (if isShort then "short" else "byte") val
      return $ constr val
    methorvar = do
      nam <- identifier
      ((MethodInvocation nam) <$> parens (commaSep expression)) <|> return (VariableAccess nam)
