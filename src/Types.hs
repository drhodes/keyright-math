module Types (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Minify a where
  minify :: a -> a

data Var = Var Char
  deriving (Eq, Ord)

data Expr = EVar Var
          | Identity
          | Zero
          | Add Expr Expr
          | Sub Expr Expr
          | EInt Integer
          | Mul Expr Expr
          | Pow Expr Expr
          | Neg Expr
          deriving (Eq, Ord)

-- http://mathmistakes.info/mistakes/calculus/index.html
-- https://tutorial.math.lamar.edu/Extras/CommonErrors/CalculusErrors.aspx
-- https://magoosh.com/hs/ap/ap-calculus-ab-mistakes-avoid/

data MistakeType = SignError
                 | ChainRule 
                 | SymbolFlip

applyMistake expr mis =
  case mis of
    SignError -> Neg expr -- negate the expression.
    _ -> expr -- not sure how else ..

data RuleType = Factoring
              | Limit
              | LHopitals
              | Quotient
              | Chain
              | Product
              | Power

-- data Rule m = Rule (Expr -> m Expr)

deriv :: Expr -> Expr
deriv (Pow base expo) = do
  let newexpo = Sub expo Identity
  expo `Mul` (Pow base newexpo) `Mul` (deriv base)

deriv Identity = Zero
deriv Zero = Zero
deriv FuncApp1 expr = 

  



-- show_binop op e1 e2 = concat ["(", show e1, " ", op, " ", show e2, ")"]

-- sort (Add e1 e2) = Add (sort (min e1 e2)) (sort (max e1 e2))
-- sort (Mul e1 e2) = Mul (sort (min e1 e2)) (sort (max e1 e2))
-- sort (Var c) = Var c
-- sort (EInt n) = EInt n

-- instance Show Expr where
--   show (Mul (EInt n) (Var c)) = (show n) ++ [c]
--   show (Var c) = [c]
--   show (Add e1 e2) = show_binop "+" e1 e2
--   show (Mul e1 e2) = show_binop "*" e1 e2
--   show (EInt n) = show n

-- -- instance Minify Expr where
-- --   minify (Var v) = Evar v

-- answer = let twox = (Mul (EInt 2) (Var 'x'))
--              threex = (Mul (EInt 3) (Var 'x'))
--           in Add twox threex

-- example = let twox = (Mul (EInt 2) (Var 'x'))
--               three = EInt 3
--           in Add twox three
