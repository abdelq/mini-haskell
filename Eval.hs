{-|
Copyright : (c) Vincent Archambault-Bouffard, 2017
                Abdelhakim Qbaich, 2017
                David Boivin, 2017
License   : MIT
-}

module Eval (module Eval) where

import Parseur


-- Types
data Type = TInt
          | TArrow Type Type
          | TData Symbol
          deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show (TArrow t1 t2) = showParen' t1 ++ " -> " ++ show t2
    where showParen' x@(TArrow _ _) = "(" ++ show x ++ ")"
          showParen' x = show x
  show (TData sym) = sym

-- Expressions
data Exp = EInt Int
         | EVar Symbol
         | EApp Exp Exp
         | ELam Symbol Type Exp
         | ELet [(Symbol, Type, Exp)] Exp
         | EData [Value] Exp
         deriving (Show, Eq)

-- Valeurs
data Value = VInt Int
           | VLam Symbol Exp Env
           | VPrim (Value -> Value)
           | VData Type [Symbol]

instance Show Value where
  show (VInt n) = show n
  show (VData t s) = show t -- TODO
  show _ = "<function>"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  _ == _ = False

---------------------------------------------------------------------------
-- Pour ce TP, une erreur est simplement un chaîne de caractères
-- expliquant le problème
---------------------------------------------------------------------------
type Error = String

-- Environnements
type Env = [(Symbol, Value)]

env0 :: Env
env0 = [("+", prim (+)),
        ("-", prim (-)),
        ("*", prim (*))]
  where prim op =
          VPrim (\ (VInt x) -> VPrim (\ (VInt y) -> VInt (x `op` y)))

type Tenv = [(Symbol, Type)]
tenv0 :: Tenv
tenv0 = [("+", TArrow TInt (TArrow TInt TInt)),
         ("-", TArrow TInt (TArrow TInt TInt)),
         ("*", TArrow TInt (TArrow TInt TInt))]

---------------------------------------------------------------------------
-- Fonction de converstion des Sexp en Expressions (Exp)
-- Vous allez devoir modifier sexp2type et sexp2Exp
---------------------------------------------------------------------------
sexp2type :: Sexp -> Either Error Type
sexp2type (SSym "Int") = Right TInt
sexp2type (SSym sym) = Right (TData sym)
sexp2type (SList [x]) = sexp2type x
sexp2type (SList (SSym "->" : xs)) = sexp2type (SList xs)
sexp2type (SList (x : xs)) = do
  type1 <- sexp2type x
  type2 <- sexp2type (SList xs)
  return $ TArrow type1 type2
sexp2type _ = Left "Ill formed type"

reservedKeywords :: [Symbol]
reservedKeywords = ["lambda", "let", "case", "data", "Erreur"]

sexp2Exp :: Sexp -> Either Error Exp
sexp2Exp (SNum x) = Right $ EInt x
sexp2Exp (SSym ident) | ident `elem` reservedKeywords
  = Left $ ident ++ " is a reserved keyword"
sexp2Exp (SSym ident) = Right $ EVar ident

sexp2Exp (SList [SSym "lambda", SList [SList [SSym var, t]], body]) = do
  body' <- sexp2Exp body
  t' <- sexp2type t
  return $ ELam var t' body'
sexp2Exp (SList [SSym "lambda", SList (x : xs), body]) =
    let body' = SList [SSym "lambda", SList xs, body]
     in sexp2Exp (SList [SSym "lambda", SList [x], body'])
sexp2Exp (SList [SSym "lambda", SList [], _]) = Left "Syntax Error : No parameter"

sexp2Exp (SList [SSym "let", SList (x : xs), body]) = do
  body' <- sexp2Exp body
  args' <- makeArgs [] (x : xs)
  return $ ELet args' body'
      where makeArgs :: [(Symbol, Type, Exp)] -> [Sexp] -> Either Error [(Symbol, Type, Exp)]
            makeArgs env [] = Right env
            makeArgs env (SList [SSym var, t, exp] : xs) = do
                t' <- sexp2type t
                exp' <- sexp2Exp exp
                makeArgs ((var, t', exp') : env) xs
sexp2Exp (SList [SSym "let", SList [], _]) = Left "Syntax Error : No parameter"

-- TODO sexp2Exp data
sexp2Exp (SList [SSym "data", SList (x : xs), body]) = do
  body' <- sexp2Exp body
  types' <- makeTypes [] (x : xs)
  error $ show $ EData types' body'
      where makeTypes :: [Value] -> [Sexp] -> Either Error [Value]
            makeTypes env [] = Right env
            makeTypes env (SList (SSym sym : ys) : xs) = do
              typ' <- sexp2type (SSym sym)
              error $ show $ makeTypes (VData (typ') (makeSym [] ys):env) xs
                  where makeSym :: [Symbol] -> [Sexp] -> [Symbol]
                        makeSym env [] = env
                        makeSym env ((SSym x) :xs) = makeSym (x:env) xs
sexp2Exp (SList [SSym "data", SList [], _]) = Left "Syntax Error : No parameter"

sexp2Exp (SList [func, arg]) = do
  func' <- sexp2Exp func
  arg' <- sexp2Exp arg
  return $ EApp func' arg'

sexp2Exp (SList lst) = do
    init <- sexp2Exp (SList (init lst))
    last <- sexp2Exp (last lst)
    return $ EApp init last

{-sexp2Exp _ = Left "Syntax Error : Ill formed Sexp"-}

-- Évaluation
lookupVar :: [(Symbol, Value)] -> Symbol -> Value
lookupVar [] sym = error "Oups ..."
lookupVar ((s,v) : _) sym | s == sym = v
lookupVar (_ : xs) sym = lookupVar xs sym

eval :: Env -> Exp -> Value
eval _ (EInt x) = VInt x
eval env (EVar sym) = lookupVar env sym

eval env (EApp exp1 exp2) =
    let v1 = eval env exp1
        v2 = eval env exp2
     in case v1 of
          VPrim prim -> prim v2
          VLam sym exp env -> eval ((sym, v2) : env) exp

eval env (ELam sym typ exp) = VLam sym exp env

eval env (ELet args body) =
    let env' = map (\(var, _, exp) -> (var, eval env' exp)) args ++ env
     in eval env' body

-- TODO eval EData

{-eval _ _ = error "eval"-}

-- Vérification de type
lookupType :: [(Symbol, Type)] -> Symbol -> Either Error Type
lookupType [] sym = Left $ "Not in scope variable : " ++ show sym
lookupType ((s,v) : _) sym | s == sym = Right v
lookupType (_ : xs) sym = lookupType xs sym

typeCheck :: Tenv -> Exp -> Either Error Type
typeCheck _ (EInt x) = Right TInt
typeCheck env (EVar sym) = lookupType env sym

typeCheck env (EApp exp1 exp2) = do
    exp1Type <- typeCheck env exp1
    exp2Type <- typeCheck env exp2
    case exp1Type of
      t1 `TArrow` t2 | t1 == exp2Type -> Right t2
      _ -> Left "EApp"

typeCheck env (ELam sym t1 exp) = do
    t2 <- typeCheck ((sym, t1) : env) exp
    return $ t1 `TArrow` t2

typeCheck env (ELet args body) =
    let env' = map (\(var, t, exp) -> (var, t)) args ++ env
     in do args' <- mapM (\(var, t, exp) -> typeCheck env' exp) args
           typeCheck env' body

-- TODO typeCheck EData

{-typeCheck _ _ = error "typeCheck"-}
