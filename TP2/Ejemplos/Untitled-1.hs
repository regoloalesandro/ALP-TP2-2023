is0 = \x -> if x == 0 then True else False

zero = 0


resta x y = foldr y pred x


modAux x y = foldr (is0 (resta x y)) x (modAux (resta x y) y)

mod x y = foldr (is0 (resta x y)) zero (modAux x y)