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
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined

