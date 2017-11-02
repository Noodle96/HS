data TQuebrado = AQuebrado Integer Integer deriving Show

euclides::Integer->Integer->Integer
euclides a 0 = a
euclides a b = euclides b (a `mod` b)

numerador::TQuebrado->Integer
numerador (AQuebrado  a b) = a

denominador::TQuebrado->Integer
denominador (AQuebrado a b) = b


--suma de un quebrado
suma::TQuebrado->TQuebrado->TQuebrado
suma (AQuebrado a b) (AQuebrado c d)  | den == 0 = error "deno de la suma puede ser cero"
													| euclides num den > 1 =  (AQuebrado (newnum) (newden) )
													| otherwise = (AQuebrado (num) (den))
													where
													num = a*d+b*c
													den = b*d
													eu = euclides (num)(den)
													newnum = div (num) eu
													newden = div (den) eu




--resta de un quebrado
resta::TQuebrado->TQuebrado->TQuebrado
resta (AQuebrado a b) (AQuebrado c d)  |  den == 0 = error "denominador de la resta no puede ser cero"
													| euclides num den > 1 =  (AQuebrado (newnum) (newden) )
													| otherwise = (AQuebrado (num) (den))
													where
													num = a*d-b*c
													den = b*d
													eu = euclides (num)(den)
													newnum = div (num) eu
													newden = div (den) eu





--producto de un quebrado
producto::TQuebrado->TQuebrado->TQuebrado
producto (AQuebrado a b) (AQuebrado c d) | b == 0 || d == 0  = error "denominador no puede ser cero"
														| euclides num den > 1 = (AQuebrado  (newnum) (newden) )
														| otherwise = (AQuebrado (num) (den))
														where
														 num = a*c
														 den = b*d
														 eu = euclides (num) (den)
														 newnum = div (num) eu
														 newden = div (den) eu




-- division de un quebrado
division::TQuebrado->TQuebrado->TQuebrado
division (AQuebrado a b) (AQuebrado c d) | b == 0 || d == 0 = error "denominador no puede ser cero"
														| euclides num den > 1 = (AQuebrado (newnum) (newden) )
														| otherwise = (AQuebrado (num) (den))
														where
														num = a*d
														den = b*c
														eu = euclides (num) (den)
														newnum = div (num) eu
														newden = div  (den) eu





--data TBooleano = Verdadero | Falso deriving Eq
--orO::TBooleano->TBooleano->Bool
--orO a b | (a==)= False
--		| otherwise = True




type Matriz = [[Float]]

-- cantidad de filas de una matriz
filas::Matriz->Int
filas [] = 0
filas (x:xs) = 1 + filas xs


--calculando el tamanhio de un [Float]
len::[Float]->Int
len [] = 0
len (x:xs) = 1 + len xs



--cantidad de columnas de un matriz
columnas::Matriz->Int
columnas [] = 0
columnas (x:xs) = len x


--dimensiones de una matriz , resultara una tupla

--[1 2 3 4]
--[5 6 7 8]    = (3,4)
--[9,10,11,12]
dimensionMatriz::Matriz->(Int,Int)
dimensionMatriz  m = (filas m, columnas m)


--tamanhio de fila de una fila cualquiera
lenHead::Matriz->Int
lenHead (x:xs) = length x



--validando una matriz
--
--[1 2 3]
--[4 5 6] == True
--[7 8 9]

--[1 2 3]
--[4 5 6] == False
--[7 8]
validezMatriz::Matriz->Bool
validezMatriz [] = True
validezMatriz (x:xs) |  null xs =  True
							| len x /= lenHead xs = False
							| otherwise = validezMatriz xs




-- True si ambas tienen la misma cantidad de filas y columnas
compararMatrices::Matriz->Matriz->Bool
compararMatrices m n = dimensionMatriz m == dimensionMatriz n


--sumando dos vectores con proposito de sumar dos matriaces
--no se verificara que tengan la misma cantidad de columnas debido a que
--ello se verifica en la matriz

sumaV::[Float]->[Float]->[Float]
sumaV x y = [ e + j | (e,j) <-zip x y]


--validacion antes de poder sumarlas(solo para sumas)
prevalidacion::Matriz->Matriz->Bool
prevalidacion m n = validezMatriz m && validezMatriz n && compararMatrices m n





--sumar dos matrices con las medidas y restriciones ya validadas
sumarMatrices::Matriz->Matriz->Matriz
sumarMatrices [] [] = []
sumarMatrices (m:mx) (n:nx) | prevalidacion (m:mx) (n:nx) == False = error "no cumplelas condiciones nesesarias"
						| otherwise = [ sumaV m n | (m,n)<-zip (m:mx) (n:nx) ]




--
--	Producto de matrices
--

--devuelve vectores de las columnas
--[1,2,3]
--[4,5,6]
--
-- = [1,4], [2,5],[3,6]
-- formarColumnas::Matriz->Matriz
-- formarColumnas (m:mx) | validezMatriz (m:mx) == False = error "mala matriz"
-- 								|




















--
