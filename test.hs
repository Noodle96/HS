data TQuebrado = AQuebrado Integer Integer deriving Show

euclides::Integer->Integer->Integer
euclides a 0 = a
euclides a b = euclides b (a `mod` b)

numerador::TQuebrado->Integer
numerador (AQuebrado  a b) = a

denominador::TQuebrado->Integer
denominador (AQuebrado a b) = b

--suma::TQuebrado->TQuebrado->TQuebrado
--suma (AQuebrado a b) (AQuebrado c d)  = (AQuebrado (div((a*d + b*c), euclides (a*d + b*c)  (b*d))) (div((b*d), euclides (a*d + b*c)  (b*d))))



--data TBooleano = Verdadero | Falso deriving Eq
--orO::TBooleano->TBooleano->Bool
--orO a b | (a==)= False
--		| otherwise = True




type Matriz = [[Float]]
type MatrizA = [[String]]
--ssuma Matriz->Matriz->Matriz

--dimensionMatriz::Matriz->Integer->Integer
array::MatrizA->Integer
array [[]] = 0
array (x:xs) = 1  + array(xs)

