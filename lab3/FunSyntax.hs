module FunSyntax where

import Data.Char(isAlpha)
import Data.List(intersperse)

data Phrase =                   -- Top-level phrase $p$
    Calculate Expr              -- \pg $e$;;
  | Define Defn                 -- \pg $d$;;
  deriving Show

data Expr =                     -- Expressions $e$
    Number Integer              -- $n$
  | Variable Ident              -- $x$
  | Apply Expr [Expr]           -- \pg $e_0$($e_1$, \dots, $e_n$)
  | If Expr Expr Expr           -- \pg if $e_1$ then $e_2$ else $e_3$
  | Lambda [Ident] Expr         -- \pg lambda ($x_1$, \dots, $x_n$) $e$
  | Let Defn Expr               -- \pg let $d$ in $e$
  | Assign Expr Expr            -- \pg $e_1$ := $e_2$
  | Sequence Expr Expr          -- \pg $e_1$; $e_2$
  | While Expr Expr             -- \pg while $e_1$ do $e_2$
  | Loop Expr
  | Exit
  deriving Show

data Defn =                     -- Definitions $d$
    Val Ident Expr              -- \pg val $x$ = $e$
  | Rec Ident Expr              -- \pg rec $x$ = $e$
  deriving Show

type Ident = String

def_lhs (Val x e) = x
def_lhs (Rec x e) = x


-- PRETTY PRINTER FOR EXPRESSIONS

-- pretty -- pretty-print an expression with ellipses
pretty :: Expr -> String
pretty exp = pp 8 2 exp ""

pp :: Int -> Int -> Expr -> ShowS

pp p d (Number n) = showString (show n)

pp p d (Variable x) = showName x

pp p 0 _ = showString "..."

pp p d (Let def body) =
  showParen (p < 8)
    (showString "let " . pp_def d def 
      . showString " in " . pp 8 (d-1) body)

pp p d (Lambda fps body) =
  showParen (p < 8)
    (showString "lambda (" . pp_list showName fps . showString ") "
      . pp 8 (d-1) body)

pp p d (Sequence e1 e2) =
  showParen (p < 8)
    (pp 7 d e1 . showString "; " . pp 7 (d-1) e2)

pp p d (If e1 e2 e3) =
  showParen (p < 7)
    (showString "if " . pp 7 (d-1) e1 . showString " then " 
      . pp 7 (d-1) e2 . showString " else " . pp 7 (d-1) e3)

pp p d (While e1 e2) =
  showParen (p < 7)
    (showString "while " . pp 7 (d-1) e1 
      . showString " do " . pp 7 (d-1) e2)

pp p d (Loop e) = 
  showParen (p < 7)
    (showString "loop " . pp 7 (d-1) e)

pp p d (Assign e1 e2) =
  showParen (p < 4)
    (pp 3 (d-1) e1 . showString " := " . pp 4 (d-1) e2)

pp p d (Apply f aps) = 
  showParen (p < 2)
    (pp 2 d f . showString "(" . pp_list (pp 8 (d-1)) aps . showString ")")

-- Catchall for debugging:
-- pp p d e =
--   showString "<" . shows e . showString ">"

showName x =
  let y = show x in
  if isAlpha (head y) then showString y
  else showString "(" . showString y . showString ")"

pp_def :: Int -> Defn -> ShowS

pp_def d (Val x e) =
  showString "val " . showName x . showString " = " . pp 8 (d-1) e

pp_def d (Rec x e) =
  showString "rec " . showName x . showString " = " . pp 8 (d-1) e

pp_list :: (a -> ShowS) -> [a] -> ShowS
pp_list p = compose . intersperse (showString ", ") . map p

compose = foldr (.) id
