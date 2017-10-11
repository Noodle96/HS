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
type Polinomio = (Int,[Float])-- <grado,[Lista de coeficientes]>
--a. grado :: Polinomio -> Float
grado::Polinomio->Int
grado (grado,_) = grado





--b. coeficiente :: Polinomio -> Int -> Float
coeficiente::Polinomio->Int->[Float]
-- <polinomio,posicion,Coeficiente
coeficiente (c,p) posicion = [l | (l,n) <- zip p [c - 1, c-2..0], n == posicion]






--c. evaluar :: Polinomio -> Float -> Float , que devuelve el valor del polinomio en el punto dado.
--evaluar::Polinomio->Float->Float
-- <polinomio,la x, resultado>
--evaluar (0,(x:px)) valor = 0
--evaluar (c,(x:px))  valor | length px == 0 = 0
--									otherwise = valor * evaluar


















--8. En un proyecto de desarrollo de un sistema de gestión para videoclubs, se te pide, como
--parte del staff de programadores, que definas ciertas funciones del sistema. Se te pide
--definir los tipos : VideoClub, Cliente y Película ; y las siguientes funciones:






--a. clientes :: VideoClub -> [Cliente], que devuelve una lista con los clientes registrados en el videoclub.




--b. películas :: VideoClub -> [Película], que devuelve una lista con las películas que están en el catálogo de videoclub, independientemente de si está disponible o no.






--c. cantidadDeCopias :: VideoClub -> Película -> Int, que devuelve cuántas copias de la
--película tiene el videoclub (esta función se indefine, o sea da error, si la película no
--pertenece al catálogo del videoclub).










--d. p elıcu lasA lq u ilad as :: Videoclub -> Cliente -> [Película], que devuelve una lista de
--películas que el cliente tiene alquiladas en el videoclub (esta función se indefine si el
--cliente no está registrado en el videoclub). El cliente puede alquilar más de una copia de
--la misma película.








--e. alquiló :: VideoClub -> Cliente -> Película -> Bool, que indica si el cliente alquiló la
--película.








--f. copiasAlquiladas :: VideoClub -> Película -> Int, que devuelve la cantidad de copias de
--la película dada que están alquiladas en el videoclub.






--g. disponibleParaAlquilar :: Videoclub -> Película -> Bool, que indica si una película está
--disponible para ser alquilada en el videoclub (por ejemplo, una película puede estar en
--un videoclub pero no estar disponible porque todas sus copias están alquiladas, o no
--estar disponible porque no está en el catálogo).
--Dada la crisis económica los videoclubs más pequeños han decidido unirse en una red para
--brindar servicios adicionales al cliente como, por ejemplo, distribución a domicilio y
--compartir el conjunto de películas que disponen. Debido a esto se pide que definas las
--funciones :







--h. disponibleEnLaRed :: [Videoclub] ->Película -> Bool, que dice si la película está
--disponible en al menos alguno de los videoclubs de la red.










--i. másAlquiladaEnLaRed :: [Videoclub] -> Película, que devuelve la película más alquilada
--en todos los videoclubs de la red.






--j. clientesRaros :: [Videoclub] ->[Cliente], que devuelve la lista de clientes que tienen
--alquilada la misma película en distintos videoclubs.

























--
