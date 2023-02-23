module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

-- We use chain from the course note
chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a

-- Parse character from a to z
char :: Parser Char
char = oneof ['a'..'z'] 

-- Parse lam, also using spaces from Parser.hs to parse any spaces, this is also used in other parsers
consLongLam :: Parser Builder -> Parser Builder
consLongLam p = do
    spaces
    is '('
    spaces
    is 'λ'
    spaces
    c <- char
    spaces
    is '.'
    spaces
    l <- p
    spaces
    is ')'
    spaces
    pure (c `lam` l)

-- Term character
consTerm :: Parser Builder
consTerm = term <$> char

-- Parse brackets, instead of builder we use a to make it also compatible for other parsers
consBr :: Parser a -> Parser a
consBr p = do
    is '('
    spaces
    b <- p
    spaces
    is ')'
    pure b

-- So we can handle spaces between ap if any
apP :: Parser (Builder -> Builder -> Builder)
apP = do
    spaces
    pure ap

-- Combine above parsers 
consLongAtom :: Parser Builder
consLongAtom = consTerm ||| consBr consLongTot ||| consLongLam consLongTot

-- Use chain consAtom and ap to get the builder to be built
consLongTot :: Parser Builder
consLongTot = chain consLongAtom apP

-- Finally build the builder
longLambdaP :: Parser Lambda
longLambdaP = build <$> consLongTot

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

-- Adjust our previous lam parser to for short form
consShortLam :: Parser Builder -> Parser Builder
consShortLam p = do
    spaces
    is 'λ'
    spaces
    lc <- list1 char
    spaces
    is '.'
    spaces
    l <- p
    pure (foldr lam l lc)

-- Simply change to short form version
consShortAtom :: Parser Builder
consShortAtom = consTerm ||| consBr consShortTot ||| consShortLam consShortTot     

-- Same as above need to change to short form version
consShortTot :: Parser Builder
consShortTot = chain consShortAtom apP

-- Short form version
shortLambdaP :: Parser Lambda
shortLambdaP = build <$> consShortTot

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

-- General version for both long and short form, simply use |||
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

-- We use a customise data type for logic expressions
data LogicExpr
    = LoTrue
    | LoFalse
    | Or LogicExpr LogicExpr
    | And LogicExpr LogicExpr
    | If LogicExpr LogicExpr LogicExpr
    | Not LogicExpr
    deriving Show

-- Parse not
consNot :: Parser LogicExpr -> Parser LogicExpr
consNot p = do
    string "not"
    list1 space
    a <- p
    pure (Not a)

-- Parse or
pOr :: Parser (LogicExpr -> LogicExpr -> LogicExpr)
pOr = do
    list1 space
    string "or"
    list1 space
    pure Or

-- Parse and
pAnd :: Parser (LogicExpr -> LogicExpr -> LogicExpr)
pAnd = do
    list1 space
    string "and"
    list1 space
    pure And

pBin :: Parser (LogicExpr -> LogicExpr -> LogicExpr)
pBin = pAnd ||| pOr

-- Parse if
consIf :: Parser LogicExpr -> Parser LogicExpr
consIf p = do
    string "if"
    list1 space
    a <- p
    list1 space
    string "then"
    list1 space
    b <- p
    list1 space
    string "else"
    list1 space
    c <- p
    pure (If a b c)

-- Parse True
consTrue :: Parser LogicExpr
consTrue = do
    string "True"
    pure LoTrue

-- Parse False
consFalse :: Parser LogicExpr
consFalse = do
    string "False"
    pure LoFalse

consBase :: Parser LogicExpr
consBase = consTrue ||| consFalse

-- Similar to the ones in Part 1, change to handle logic expressions
logicAtomP :: Parser LogicExpr
logicAtomP = consBr logicTotP ||| consNot logicTotP ||| consBase ||| consIf logicTotP

-- Again like Part 1, use chain
logicTotP :: Parser LogicExpr
logicTotP = chain logicAtomP pBin

-- Translate logic operators into lambda expressions that can be built
lamTrue :: Builder
lamTrue = lam 't' (lam '_' (term 't'))

lamFalse :: Builder
lamFalse = lam '_' (lam 'f' (term 'f'))

lamIf :: Builder
lamIf = lam 'b' (lam 't' (lam 'f' (term 'b' `ap` term 't' `ap` term 'f')))

lamAnd :: Builder
lamAnd = lam 'x' (lam 'y' (lamIf `ap` term 'x' `ap` term 'y' `ap` lamFalse))

lamOr :: Builder
lamOr = lam 'x' (lam 'y' (lamIf `ap` term 'x' `ap` lamTrue `ap` term 'y'))

lamNot :: Builder
lamNot = lam 'x' (lamIf `ap` term 'x' `ap` lamFalse `ap` lamTrue)


-- From logic expression to builder
logicToLambda :: LogicExpr -> Builder
logicToLambda x = case x of
    LoTrue -> lamTrue
    LoFalse -> lamFalse
    Or a b -> lamOr `ap` logicToLambda a `ap` logicToLambda b
    And a b -> lamAnd `ap` logicToLambda a `ap` logicToLambda b
    If a b c -> foldl ap lamIf (fmap logicToLambda [a, b, c])
    Not a -> lamNot `ap` logicToLambda a

-- Finally build the builder
logicP :: Parser Lambda
logicP = build . logicToLambda <$> logicTotP

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

-- We again use a customise data type for arithmetic expressions
data ArithExpr
    = Const Int
    | Add ArithExpr ArithExpr
    | Sub ArithExpr ArithExpr
    | Mul ArithExpr ArithExpr
    | Exp ArithExpr ArithExpr
    deriving Show

-- Parse spaces
pBinOpSpace :: String -> (a -> a -> a) -> Parser (a -> a -> a)
pBinOpSpace s op = do
    _ <- spaces
    _ <- string s
    _ <- spaces
    pure op

-- Parse substraction
pSub :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
pSub = pBinOpSpace "-" Sub

-- Parse addition
pAdd :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
pAdd = pBinOpSpace "+" Add

-- Parse multiplication
pMul :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
pMul = pBinOpSpace "*" Mul

-- Parse exponentiation
pExp :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
pExp = pBinOpSpace "**" Exp

pAb :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
pAb = pAdd ||| pSub

-- Parse number
digitP :: Parser Char
digitP = oneof ['0'..'9']

-- Cast number string to arithmetic expression
consConst :: Parser ArithExpr
consConst = do
    ls <- list1 digitP
    pure (Const (read ls))

-- Adjust chain so that we can make exponentiation right associative
chainr :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr p op = do
    x <- p
    rest x
        where rest x = do {   f <- op
                            ; y <- chainr p op
                            ; rest (f x y) }
                       ||| pure x

-- Our basic arithmetic parser only needs addition and substraction, therefore only pAb is included
basicArithAtomP :: Parser ArithExpr
basicArithAtomP = consConst ||| consBr basicArithTotP

basicArithTotP :: Parser ArithExpr
basicArithTotP = chain basicArithAtomP pAb

-- Translate them into Lambda expression
succChurch :: Builder
succChurch = lam 'n' (lam 'f' (lam 'x' (term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x'))))

-- λnfx.n(λgh.h(gf))(λu.x)(λu.u)
predChurch :: Builder
predChurch = lam 'n' (lam 'f' (lam 'x' (term 'n'
    `ap` lam 'g' (lam 'h' (term 'h' `ap` (term 'g' `ap` term 'f')))
    `ap` lam 'u' (term 'x')
    `ap` lam 'u' (term 'u')
    )))

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx
addChurch :: Builder
addChurch = lam 'x' (lam 'y' (term 'x' `ap` succChurch `ap` term 'y'))

minusChurch :: Builder
minusChurch = lam 'x' (lam 'y' (term 'y' `ap` predChurch `ap` term 'x'))

mulChurch :: Builder
mulChurch = lam 'x' (lam 'y' (lam 'f' (term 'x' `ap` (term 'y' `ap` term 'f'))))

expChurch :: Builder
expChurch = lam 'x' (lam 'y' (term 'y' `ap` term 'x'))

-- From arithmetic expression to builder
arithToLam :: ArithExpr -> Builder
arithToLam x = case x of
    Const n -> intToLam n
    Add a b -> addChurch `ap` arithToLam a `ap` arithToLam b
    Sub a b -> minusChurch `ap` arithToLam a `ap` arithToLam b
    Mul a b -> mulChurch `ap` arithToLam a `ap` arithToLam b
    Exp a b -> expChurch `ap` arithToLam a `ap` arithToLam b

-- Finally build the builder
basicArithmeticP :: Parser Lambda
basicArithmeticP = build . arithToLam <$> basicArithTotP

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

-- General version of our arithmetic parser
arithAtomP :: Parser ArithExpr
arithAtomP = consConst ||| consBr arithTotP

-- Therefore needs include pMul and pExp which are multiplication and exponentiation by order of operations
arithExpP :: Parser ArithExpr
arithExpP =  chainr arithAtomP pExp

arithMulP :: Parser ArithExpr
arithMulP = chain arithExpP pMul

arithTotP :: Parser ArithExpr
arithTotP = chain arithMulP pAb

-- Finally build the builder
arithmeticP :: Parser Lambda
arithmeticP = build . arithToLam <$> arithTotP


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- |
--
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False

-- Need builder for arithmetic expression
arithBuilderP :: Parser Builder
arithBuilderP = arithToLam <$> arithTotP

-- Translate them into Lambda expression
isZeroChurch :: Builder
isZeroChurch = lam 'n' (term 'n' `ap` lam 'x' lamFalse `ap` lamTrue)

churchLEQ :: Builder
churchLEQ = lam 'm' (lam 'n' (isZeroChurch `ap` (minusChurch `ap` term 'm' `ap` term 'n')))

churchGEQ :: Builder
churchGEQ = lam 'm' (lam 'n' (isZeroChurch `ap` (minusChurch `ap` term 'n' `ap` term 'm')))

churchEQ :: Builder
churchEQ = lam 'm' (lam 'n' (lamAnd
                                `ap` (churchLEQ `ap` term 'm' `ap` term 'n')
                                `ap` (churchGEQ `ap` term 'm' `ap` term 'n')))

churchNEQ :: Builder
churchNEQ = lam 'm' (lam 'n' (lamNot `ap` (churchEQ `ap` term 'm' `ap` term 'n')))

churchLT :: Builder
churchLT = lam 'm' (lam 'n' (lamAnd
                                `ap` (churchLEQ `ap` term 'm' `ap` term 'n')
                                `ap` (churchNEQ `ap` term 'm' `ap` term 'n')))

churchGT :: Builder                                
churchGT = lam 'm' (lam 'n' (lamAnd
                                `ap` (churchGEQ `ap` term 'm' `ap` term 'n')
                                `ap` (churchNEQ `ap` term 'm' `ap` term 'n')))

-- Parse spaces
pBinFuncSpace :: Parser Builder -> String -> Builder -> Parser Builder
pBinFuncSpace p s f = do
    x <- p
    _ <- spaces
    _ <- string s
    _ <- spaces
    y <- p
    pure (f `ap` x `ap` y)

-- Parse >
pGT :: Parser Builder -> Parser Builder
pGT p = pBinFuncSpace p ">" churchGT

-- Parse <
pLT :: Parser Builder -> Parser Builder
pLT p = pBinFuncSpace p "<" churchLT

-- Parse <=
pLEQ :: Parser Builder -> Parser Builder
pLEQ p = pBinFuncSpace p "<=" churchLEQ

-- Parse >=
pGEQ :: Parser Builder -> Parser Builder
pGEQ p = pBinFuncSpace p ">=" churchGEQ

-- Parse ==
pEQ :: Parser Builder -> Parser Builder
pEQ p = pBinFuncSpace p "==" churchEQ

-- Parse !=
pNEQ :: Parser Builder -> Parser Builder
pNEQ p = pBinFuncSpace p "!=" churchNEQ

-- Use ||| for different situations
pCond :: Parser Builder -> Parser Builder
pCond p = pEQ p ||| pNEQ p ||| pLT p ||| pGT p ||| pLEQ p ||| pGEQ p

-- Parse if, adjusted from Part 2
pIf :: Parser Builder -> Parser Builder
pIf p = do
    string "if"
    spaces
    a <- p
    spaces
    string "then"
    spaces
    b <- p
    spaces
    string "else"
    spaces
    c <- p
    pure (lamIf `ap` a `ap` b `ap` c)

-- Parse not, adjusted from Part 2
pNot :: Parser Builder -> Parser Builder
pNot p = do
    string "not"
    spaces
    a <- p
    pure (lamNot `ap` a)

-- Parse or, adjusted from Part 2
pOrB :: Parser (Builder -> Builder -> Builder)
pOrB = do
    spaces
    string "or"
    spaces
    pure (\x y -> lamOr `ap` x `ap` y)

-- Parse and, adjusted from Part 2
pAndB :: Parser (Builder -> Builder -> Builder)
pAndB = do
    spaces
    string "and"
    spaces
    pure (\x y -> lamAnd `ap` x `ap` y)

pBinB :: Parser (Builder -> Builder -> Builder)
pBinB = pAndB ||| pOrB

-- Similar to previous ones
consCondBase :: Parser Builder
consCondBase = pCond arithBuilderP ||| consBr consTotCalc ||| pIf consTotCalc ||| pNot consTotCalc

-- Again chain
consTotCalc :: Parser Builder
consTotCalc = chain consCondBase pBinB

-- Finally build the builder
calcP :: Parser Lambda
calcP = build <$> consTotCalc

-- Our complexCalcP should be general
-- Therefore it should also handle what logicP and arithmeticP can handle
-- We include logicP and arithmeticP using |||
complexCalcP :: Parser Lambda
complexCalcP = calcP ||| logicP ||| arithmeticP


{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof

-- Translate them into lambda expressions
lamNull :: Builder
lamNull = lam 'c' (lam 'n' (term 'n'))

lamCons :: Builder
lamCons = lam 'h' (lam 't' (lam 'c' (lam 'n'
            (term 'c' `ap` term 'h' `ap`
                    (term 't' `ap` term 'c' `ap` term 'n')))))

lamIsNull :: Builder
lamIsNull = lam 'l' (term 'l' `ap`
                        (lam 'h' (lam 't' lamFalse)) `ap` lamTrue)

lamHead :: Builder
lamHead = lam 'l' (term 'l'
                        `ap` (lam 'h' (lam 't' (term 'h')))
                        `ap` lamFalse)

lamTail :: Builder
lamTail = lam 'l' (lam 'c' (lam 'n'
            (term 'l'
                `ap` lam 'h' (lam 't' (lam 'g'
                    (term 'g' `ap` term 'h' `ap`
                        (term 't' `ap` term 'c'))))
                `ap` lam 't' (term 'n')
                `ap` lam 'h' (lam 't' (term 't')))))


-- Parse []
pSquareBr :: Parser Builder -> Parser Builder
pSquareBr p = do { string "["
        ; b <- p
        ; string "]"
        ; pure b}

-- Parse ,
pCons :: Parser Builder
pCons = do
    x <- pListAtom
    rest x
            where rest x = do { spaces
                    ; string ","
                    ; spaces
                    ; y <- pListTot
                    ; pure (lamCons `ap` x `ap` y)}
                    ||| do { y <- pListTot
                    ; pure (lamCons `ap` x `ap` y)}

-- Parse null
pNull:: Parser Builder
pNull = pure lamNull

-- Parse string null
pSpecNull:: Parser Builder
pSpecNull = do
    string "null"
    pure lamNull

-- Need builder for logic expression
logicBuilderP :: Parser Builder
logicBuilderP = logicToLambda <$> logicTotP

-- Similar to previous ones, also we include arithBuilderP and logicBuilderP
-- So that our listOpP can handle the arithmetic expressions and logic expressions inside the list
pExprAtom :: Parser Builder
pExprAtom = arithBuilderP ||| logicBuilderP ||| consTotCalc

pListAtom :: Parser Builder
pListAtom = pExprAtom ||| pSquareBr pListTot

pListTot :: Parser Builder
pListTot = pCons ||| pNull

pListFull :: Parser Builder
pListFull = pSpecNull ||| pSquareBr pListTot 

-- Finally build the builder
listP :: Parser Lambda
listP = build <$> pListFull

-- |
--
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head tail [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False

-- Helper function 
pUniOp :: String -> Builder -> Parser Builder -> Parser Builder
pUniOp s f p = do
    string s
    list1 space
    x <- p
    pure (f `ap` x)

-- Parse head
pHead :: Parser Builder -> Parser Builder
pHead = pUniOp "head" lamHead

-- Parse isNull
pIsNull :: Parser Builder -> Parser Builder
pIsNull = pUniOp "isNull" lamIsNull

-- Parse tail
pTail :: Parser Builder -> Parser Builder
pTail = pUniOp "tail" lamTail

-- Parse cons
pListCons :: Parser Builder -> Parser Builder -> Parser Builder
pListCons p1 p2 = do
    string "cons"
    list1 space
    x <- p1
    list1 space
    xs <- p2
    pure (lamCons `ap` x `ap` xs)

-- Similar to previous ones
pUni :: Parser Builder -> Parser Builder
pUni p = pHead p ||| pIsNull p ||| pTail p ||| pListCons pExprAtom p

pListOpTot :: Parser Builder
pListOpTot = pUni pListOpTot ||| pUni pListFull

-- Finally build the builder
listOpP :: Parser Lambda
listOpP = build <$> pListOpTot


-- | Exercise 2

-- | Implement your function(s) of choice below!

-- |
--
-- >>> lamToInt <$> parse factorialP "(5)!"
-- Result >< Just 120
--
-- >>> lamToInt <$> parse factorialP "(2+3)!"
-- Result >< Just 120
--
-- >>> lamToInt <$> parse factorialP "(3)!"
-- Result >< Just 6

-- Factorial parser, it can parse such as (5)!, (2+3)!, but not 5! without brackets

-- Translate them into lambda expressions
lamPairBind :: Char -> Char -> Char -> Builder
lamPairBind x y z = lam x (lam y (lam z (term z `ap` term x `ap` term y)))

lamPair :: Builder
lamPair = lamPairBind 'k' 'o' 'q'

lamFac :: Builder
lamFac = lam 'i' (lam 'j' (lamPair `ap` ap succChurch (term 'i') `ap` (mulChurch `ap` term 'i' `ap` term 'j')))

lamFactorial :: Builder
lamFactorial = lam 'd' (term 'd'
                        `ap` (lam 'e' (term 'e' `ap` lamFac))
                        `ap` (lamPair `ap` (lam 'l' (term 'l')) `ap` (lam 'l' (term 'l')))
                        `ap` lamFalse)

-- Parse factorial
factorP :: Parser Builder -> Parser Builder
factorP p = do
    is '('
    c <- p
    is ')'
    string "!"
    pure (lamFactorial `ap` c)

-- Include arithBuilderP so our factorial parser can also handle arithmetic expressions inside brackets
factorArithP :: Parser Builder
factorArithP = factorP factorArithP ||| arithBuilderP

-- Finally build the builder
factorialP :: Parser Lambda
factorialP = build <$> factorArithP 

-- |
--
-- >>> lamToInt <$> parse listMapP "head map +2 [2,3,4]"
-- Result >< Just 4
--
-- >>> lamToInt <$> parse listMapP "head tail map +2 [2,3,4]"
-- Result >< Just 5
--
-- >>> lamToInt <$> parse listMapP "head tail tail map *2 [2,3,4]"
-- Result >< Just 8

-- map parser, map to the list

-- Translate them into lambda expressions
churchMap :: Builder
churchMap = lam 'f' (lam 'l' (lam 'c' (lam 'n'
                                            (term 'l'
                                            `ap` (lam 'x' (term 'c' `ap` (term 'f' `ap` term 'x')))
                                            `ap` term 'n'))))

lamApp2 :: Builder
lamApp2 = lam 'x' (lam 'y' (lam 'z' (term 'x' `ap` term 'y' `ap` term 'z')))

-- Parse map
pListMap:: Parser Builder -> Parser Builder -> Parser Builder
pListMap p1 p2 = do
    string "map"
    list1 space
    x <- p1
    list1 space
    xs <- p2
    pure (churchMap `ap` x `ap` xs)

-- Parse not
pNotMap :: Parser Builder
pNotMap = do
    string "not"
    pure lamNot


-- Parse addition
pAddMap :: Parser Builder
pAddMap = do
    spaces
    string "+"
    spaces
    x <- arithBuilderP
    pure (lamApp2 `ap` addChurch `ap` x)

-- Parse multiplication
pMulMap :: Parser Builder
pMulMap = do
    spaces
    string "*"
    spaces
    x <- arithBuilderP
    pure (lamApp2 `ap` mulChurch `ap` x)

-- Similar to previous ones
pMapFunc :: Parser Builder
pMapFunc = pNotMap ||| pAddMap ||| pMulMap ||| consShortTot

mapListTotP :: Parser Builder
mapListTotP = pListMap pMapFunc mapListTotP ||| pUni mapListTotP ||| pListMap pMapFunc pListFull

-- Finally build the builder
listMapP :: Parser Lambda
listMapP = build <$> mapListTotP