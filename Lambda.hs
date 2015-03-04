module Lambda where

import Text.ParserCombinators.Parsec
import Data.Char

import qualified Data.Map as M

data Lambda b v
    = Var v
    | App (Lambda b v) (Lambda b v)
    | Lam b (Lambda b v)
    | AntiQ String
    | Fst (Lambda b v) | Snd (Lambda b v)
    | Pair (Lambda b v,Lambda b v)
    | Unit
{-
    | Inl (Lambda b v) | Inr (Lambda b v)
    | Case (Lambda b v) b (Lambda b v) b (Lambda b v)
-}
      deriving Show

stringToDebruin :: M.Map String Int -> Lambda String String -> Lambda () Int
stringToDebruin env (Var v)
    = Var (lookupV v env)
stringToDebruin env (App e1 e2)
    = App (stringToDebruin env e1) (stringToDebruin env e2)
stringToDebruin env (Lam b e)
    = Lam () (stringToDebruin (newVar b env) e)
stringToDebruin env (AntiQ s) = AntiQ s
stringToDebruin env (Fst e)
    = Fst (stringToDebruin env e)
stringToDebruin env (Snd e)
    = Snd (stringToDebruin env e)
stringToDebruin env (Pair (e1,e2)) 
    = Pair (stringToDebruin env e1, stringToDebruin env e2)
{-
stringToDebruin env (Inl e)
    = Inl (stringToDebruin env e)
stringToDebruin env (Inr e)
    = Inr (stringToDebruin env e)
stringToDebruin env (Case e b1 e1 b2 e2)
    = Case (stringToDebruin env e) 
           () (stringToDebruin (newVar b1 env) e1)
           () (stringToDebruin (newVar b2 env) e2)
-}
stringToDebruin env Unit = Unit

newVar v env = M.insert v 0 (M.map (+1) env)

lookupV v map = case M.lookup v map of
                  Nothing -> error ("Variable not in scope: " ++ v)
                  Just s  -> s

lexeme p    = do { x <- p; spaces; return x }
symbol name = lexeme (string name)
parens p    = between (symbol "(") (symbol ")") p

parenExpr = name "parenthesis"
             (parens (do es <- sepBy expr (symbol ",")
                         case length es of
                            0 -> return Unit
                            1 -> return (head es)
                            2 -> return (Pair (head es,head $ tail es))
                            3 -> error "Supports only pairs of length two"
                     ))

atom = {-
       do symbol "inl"
          l <- atom 
          return (Inl l)
       <|>
       do symbol "inr"
          l <- atom
          return (Inr l)
       <|>
       -}
       name "variable"  (fmap Var idParser)
       <|>
       parenExpr
       <|>
       name "quote" (fmap AntiQ (char '$' >> idParser))

expr = name "Lambda expression"
          (do symbol "\\" 
              ss <- many1 idParser
              symbol "->"
              e <- expr
              return (foldr Lam e ss))
       <|> parenExpr
{-
       <|>
       name "Case expression"
            (do symbol "case"
                e <- expr
                symbol "|"
                symbol "inl"
                b1 <- idParser
                symbol "->"
                e1 <- expr
                symbol "|"
                symbol "inr"
                b2 <- idParser
                symbol "->"
                e2 <- expr
                return (Case e b1 e1 b2 e2))
-}
       <|>
       do a <- atom
          args <- many atom
          return (foldl App a args)

idParser = do c  <- lower
              cs <- many idChar
              spaces
              return (c:cs)

idChar = lower <|> upper <|> digit

name nm p = label p nm

exprP s = case runParser (spaces >> expr) () "Command line" s of
            Left err -> error (show err)
            Right tree -> tree

parseExpr :: Monad m => (String, Int, Int) -> String -> m (Lambda () Int)
parseExpr (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- expr
            eof
            return (stringToDebruin M.empty e)
