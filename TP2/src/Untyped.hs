module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion = conversion' []
conversion' :: [String] -> LamTerm -> Term
conversion' vars (LVar xs) = case (elemIndex xs vars) of 
                               Nothing -> Free (Global xs)
                               Just n  -> (Bound n)
conversion' vars (App lt1 lt2)  = conversion' vars lt1  :@: conversion' vars lt2  
conversion' vars (Abs var lt)  = Lam (conversion' (var:vars) lt) 
-------------------------------
-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral neu)  y = VNeutral(NApp neu y) 

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free name) (nE, lEnv) = nameF name nE
eval' (x :@: y) (nE, lEnv) = vapp (eval' x (nE, lEnv)) (eval' y (nE, lEnv))
eval' (Lam x) (nE, lEnv) = VLam (\u -> eval' x (nE, u:lEnv))

nameF :: Name -> NameEnv Value -> Value
nameF x ((n,v):ns) = if x == n then v else nameF x ns

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------
quote :: Value -> Term
quote val = quote' val 0

quote' :: Value -> Int -> Term
quote' (VLam f) i = Lam ((quote' (f (VNeutral (NFree (Quote i)))) (i+1)))
quote' (VNeutral (NFree (Quote x))) i =  Bound (i - x - 1)
quote' (VNeutral (NFree (Global s))) i =  Free (Global s)
quote' (VNeutral (NApp neu val)) i = (quote' (VNeutral neu) i) :@: (quote' val i)