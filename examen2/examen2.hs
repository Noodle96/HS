--
--	examen 2
--


-- 2. Definir las siguientes funciones sobre listas de caracteres:

--a. sacarTodos :: [Char] -> [Char] -> [Char], que elimina todas las apariciones de la primer
--lista en la segunda. Por ejemplo sacarTodos [c, a] [a, c, a, d, c, a] es [a, d].





--b. sacarBlancosRepetidos :: [Char] -> [Char], que reemplaza cada subsecuencia de
--blancos contiguos del primer parámetro por un solo blanco en el segundo parámetro.
--sacarBlancosRepetidos::[Char]->[Char]
--sacarBlancosRepetidos [] = []
--sacarBlancosRepetidos (x:y:zs) | x /=  ' '= [x] ++ sacarBlancosRepetidos (y:zs)
--										| y /= ' ' = [x] ++ sacarBlancosRepetidos (y:zs)
--										| y == ' ' = sacarBlancosRepetidos (y:zs)
--										| otherwise = error "error"




--c. contarPalabras :: [Char] ->Integer, que devuelve la cantidad de palabras del parámetro.
--cantPalaras::[Char]->Integer



--d. palabraMasLarga :: [Char] -> [Char], que devuelve la palabra más larga del parámetro.



--e. aplanar :: [[Char]] -> [Char], que a partir de una lista de palabras arma una lista de
--caracteres concatenándolas.


--f. aplanarConBlancos :: [[Char]] -> [Char], que a partir de una lista de palabras, arma una
--lista de caracteres concatenándolas e insertando un blanco entre cada par.


--g. aplanarConNBlancos :: [[Char]] -> Integer -> [Char], que a partir de una lista de
--palabras, arma una lista de caracteres concatenándolas e insertando n blancos entre
--cada par (n debe ser no negativo).
-- ¿Estas funciones son polimórficas o sobrecargadas?















































--4. Implementar el nuevo tipo Vector y las funciones:
type Vector = (Float,Float)
--a.abscisa :: Vectores -> Float
-- fromato predefiindo
abscisa::Vector->Float
abscisa (abscisa,ordenada) = abscisa



--b.ordenada :: Vectores -> Float
ordenada::Vector->Float
ordenada (abscisa,ordenada) = ordenada



--c.igualX (Vector v1 v2 ) con resultado un Bool.
igualX::Vector->Vector->Bool
igualX v1 v2 | abscisa v1 == abscisa v2 = True
					| otherwise = False




--d.igualY (Vector v1 v2 ) con resultado un Bool.
igualY::Vector->Vector->Bool
igualY v1 v2 | ordenada v1 == ordenada v2 = True
					|otherwise = False




--e.colineales (: Vector v1 v2) con un resultado un Bool.
colineales::Vector->Vector->Bool
colineales v1 v2 | (ordenada v1 / abscisa v1) == (ordenada v2 / abscisa v2) = True
						| otherwise = False







--f.norma ( Vector v)
-- o modulo
norma::Vector->Float
norma v = sqrt((abscisa v )^2 + (ordenada v)^2 )



--g.productoEscalar ( Vector v1, v2)
{-El producto escalar de dos vectores es un número real que resulta al multiplicar el producto de sus módulos por el coseno del ángulo que forman.-}
productoEscalar::Vector->Vector->Float->Float
--productoEscalar<v1,v2,angulo,productoEscalar>
productoEscalar v1 v2 angulo =  norma v1 * norma v2 * cos(angulo)








--6. Implementar el nuevo tipo Polinomio y las funciones :

--a. grado :: Polinomio -> Float
--b. coeficiente :: Polinomio -> Int -> Float
--c. evaluar :: Polinomio -> Float -> Float , que devuelve el valor del polinomio en el punto dado.
