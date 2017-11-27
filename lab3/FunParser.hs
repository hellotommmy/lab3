module FunParser(funParser) where

import Parsing
import FunSyntax
import Data.Char

-- Lexer

data Token = 
    IDENT IdKind Ident | NUMBER Integer | STRING String
  | LPAR | RPAR | COMMA | EQUAL | ASSIGN | SEMI | SSEMI | MINUS
  | IF | THEN | ELSE | LET | REC | VAL | LAMBDA | IN | WHILE | DO
  | BADTOK Char | LOOP |EXIT
  deriving Eq

data IdKind = 
  ID | MONOP | CONSOP | MULOP | ADDOP | RELOP 
  deriving (Eq, Show)

instance Show Token where
  show t = 
    case t of 
      IDENT k x -> show x; NUMBER n -> show n; 
      STRING s -> "\"" ++ s ++ "\""
      LPAR -> "("; RPAR -> ")"; COMMA -> ","; MINUS -> "-"
      EQUAL -> "="; SEMI -> ";"; SSEMI -> ";;"; ASSIGN -> ":="
      IF -> "if"; THEN -> "then"; ELSE -> "else"; LET -> "let"
      REC -> "rec"; VAL -> "val"; LAMBDA -> "lambda"; IN -> "in"
      WHILE -> "while"; DO -> "do"
      BADTOK c -> [c]; LOOP -> "loop"; EXIT -> "exit"

kwlookup = 
  make_kwlookup (IDENT ID)
    [("if", IF), ("then", THEN), ("else", ELSE), ("let", LET), ("in", IN),
      ("rec", REC), ("val", VAL), ("lambda", LAMBDA), ("while", WHILE), 
      ("do", DO),
      ("div", IDENT MULOP "div"), 
      ("mod", IDENT MULOP "mod")]

lexer =
  do 
    c <- nextch
    case c of
      _ | isAlpha c ->
        do 
          s <- star (\ c -> isAlphaNum c || c == '_')
          return (kwlookup (c:s))
      _ | isDigit c ->
        do s <- star isDigit; return (NUMBER (read (c:s)))
      '"' ->
        do s <- star (/= '"'); nextch; return (STRING s)
      '=' -> return EQUAL
      '+' -> return (IDENT ADDOP "+")
      '-' -> switch [('-', do scanComment; lexer)] (return MINUS)
      '*' -> return (IDENT MULOP "*")
      '!' -> return (IDENT MONOP "!")
      '~' -> return (IDENT MONOP "~")
      ',' -> return COMMA
      '<' -> switch [('=', return (IDENT RELOP "<=")),
                        ('>', return (IDENT RELOP "<>"))]
                (return (IDENT RELOP "<"))
      '>' -> switch [('=', return (IDENT RELOP ">="))]
                (return (IDENT RELOP ">"))
      '(' -> return LPAR
      ')' -> return RPAR
      ';' -> switch [(';', return SSEMI)] (return SEMI)
      ':' -> switch [('=', return ASSIGN)] (return (IDENT CONSOP ":"))
      ' ' -> lexer
      '\t' -> lexer 
      '\n' -> do incln; lexer
      _ -> return (BADTOK c)
              
scanComment =
  do 
    c <- nextch
    case c of
      '\n' -> incln
      _ -> scanComment


-- Parser

p_phrase =
  do e <- p_expr; eat SSEMI; return (Calculate e)
  <+> do d <- p_def; eat SSEMI; return (Define d)

p_def = 
  do eat VAL; (x, e) <- p_eqn; return (Val x e)
  <+> do eat REC; (x, e) <- p_eqn; return (Rec x e)

p_eqn =
  do x <- p_name; eat EQUAL; e <- p_expr; return (x, e)
  <+> do x <- p_name; xs <- p_formals; 
		eat EQUAL; e <- p_expr; return (x, Lambda xs e)

p_formals = 
  do eat LPAR; xs <- p_list0 p_name COMMA; eat RPAR; return xs

p_expr = 
  do eat LET; d <- p_def; eat IN; e1 <- p_expr; return (Let d e1)
  <+> do eat LAMBDA; xs <- p_formals; 
		e1 <- p_expr; return (Lambda xs e1)
  <+> p_sequence

p_sequence =
  do es <- p_list p_cond SEMI; return (foldr1 Sequence es)

p_cond = 
  do eat IF; e1 <- p_cond; eat THEN; e2 <- p_cond;
                eat ELSE; e3 <- p_cond; return (If e1 e2 e3)
  <+> do eat WHILE; e1 <- p_cond; eat DO; 
		e2 <- p_cond; return (While e1 e2)
  <+> do eat LOOP; e1<-p_cond; return (Loop e1)
  <+> p_term6

p_term6 =
  do es <- p_list p_term5 ASSIGN; return (foldr1 Assign es)
p_term5 = p_opchainl p_relop p_term4 
p_term4 = p_opchainl p_addop p_term3
p_term3 = p_opchainl p_mulop p_term2
p_term2 = p_opchainr (p_ident CONSOP) p_term1

p_relop = p_ident RELOP <+> (do eat EQUAL; return "=")
p_addop = p_ident ADDOP <+> (do eat MINUS; return "-")
p_mulop = p_ident MULOP

p_opchainl :: Parser t Ident -> Parser t Expr -> Parser t Expr
p_opchainl p_op p_rand = 
  do e0 <- p_rand; p_tail e0
  where
    p_tail e1 =
      do w <- p_op; e2 <- p_rand; p_tail (Apply (Variable w) [e1, e2])
      <+> return e1

p_opchainr :: Parser t Ident -> Parser t Expr -> Parser t Expr
p_opchainr p_op p_rand =
  do e1 <- p_rand; p_tail e1
  where
    p_tail e1 =
      do w <- p_op; e2 <- p_opchainr p_op p_rand; 
      				return (Apply (Variable w) [e1, e2])
      <+> return e1

p_term1 =
  do w <- p_monop; e <- p_term1; return (Apply (Variable w) [e])
  <+> p_term0

p_monop = p_ident MONOP <+> (do eat MINUS; return "~");

p_term0 =
  do e0 <- p_primary; p_qualifiers e0
  where
    p_qualifiers e1 =
      do aps <- p_actuals; p_qualifiers (Apply e1 aps)
      <+> return e1

p_actuals =
  do eat LPAR; aps <- p_list0 p_expr COMMA; eat RPAR; return aps

p_primary =
  do n <- p_number; return (Number n)
  <+> do x <- p_name; return (Variable x)
  <+> do eat LPAR; e <- p_expr; eat RPAR; return e
  <+> do eat EXIT; return Exit

p_number =
  do t <- scan; case t of NUMBER n -> return n; _ -> p_fail

p_name = p_ident ID <+> (do eat LPAR; x <- p_op; eat RPAR; return x)

p_op =
  p_ident MONOP <+> p_addop <+> p_mulop <+> p_relop

p_ident k =
  do t <- scan; case t of IDENT k' x | k == k' -> return x; _ -> p_fail

funParser :: Syntax Token Phrase
funParser = (lexer, p_phrase)
