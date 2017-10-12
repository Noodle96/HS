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













































{-------------------------------------------Problema 4---------------------------------------------}
{--------------------------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------------------------}



data TVector = AVector Float Float deriving Show



--a.-abscisa :: Vectores -> Float
abscisa::TVector->Float
abscisa (AVector a b) = a


--b.-ordenada :: Vectores -> Float
ordenada::TVector->Float
ordenada (AVector a b) = b


--c.-igualX (Vector v1 v2 ) con resultado un Bool.
igualX::TVector->TVector->Bool
igualX (AVector a b ) (AVector c d) = abscisa (AVector a b) == abscisa (AVector c d)


--d.-igualY (Vector v1 v2 ) con resultado un Bool.
igualY::TVector->TVector->Bool
igualY (AVector a b ) (AVector c d) = ordenada (AVector a b) == ordenada (AVector c d)


--e.-colineales (: Vector v1 v2) con un resultado un Bool.(pendientes iguales)
colineales::TVector->TVector->Bool
colineales (AVector a b) (AVector c d) = ordenada (AVector a b) / abscisa (AVector a b) == ordenada (AVector c d) / abscisa (AVector c d)


--f.-norma ( Vector v)
norma::TVector->Float
norma (AVector a b) = sqrt(x^2 + b^2)
		where x = abscisa (AVector a b)


--g.- productoEscalar ( Vector v1, v2)
{-El producto escalar de dos vectores es un número real que resulta al multiplicar el producto
 de sus módulos por el coseno del ángulo que forman.-}
productoEscalar::TVector->TVector->Float->Float
productoEscalar (AVector a b) (AVector c d) angulo = norma (AVector a b) * norma (AVector c d) * cos(angulo)




{--------------------------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------------------------}













{------------------------------------------Problema 6 ---------------------------------------------}
{--------------------------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------------------------}





type Polinomio = [Float]

--a.- grado::Polinomio->Int
grado::Polinomio->Int
grado px = length px - 1



--b. coeficiente :: Polinomio -> Int -> Float
coeficiente::Polinomio->Int->Float
coeficiente px pos = head [l | (l,exp)<-zip px [(length px) - 1, (length px)-2..0], exp == pos]


posicion::Polinomio->Int->Float
posicion px pos = head[l | (l,exp)<-zip px [0..length px], exp == pos]


--c. evaluar :: Polinomio -> Float -> Float , que devuelve el valor del polinomio en el punto dado.
-- <polinomio,grado, la t, resultado>
evaluar::Polinomio->Int->Float->Float
evaluar px  gr  t | gr == 0 = head px
					| otherwise = (t * evaluar px (gr-1) t) + posicion px gr

--



{--------------------------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------------------------}












--8. En un proyecto de desarrollo de un sistema de gestión para videoclubs, se te pide, como
--parte del staff de programadores, que definas ciertas funciones del sistema. Se te pide
--definir los tipos : VideoClub, Cliente y Película ; y las siguientes funciones:



-- videojuego y pelicula se estarna enlazando por una llave primaria-foranea
type Pelicula = (Int,String,Int) -- <LlaveForanea con VideoClub,nombre de la pelicula,nro de copias que le estan sacando>
type Cliente = (String,[Pelicula]) -- <nombre del cliente, listas de peliculas>
type VideoClub = (Int,[Cliente],[Pelicula]) -- <[lista de todos los clientes], [catalogo de peliculas]>




--a. clientes :: VideoClub -> [Cliente], que devuelve una lista con los clientes registrados en el videoclub.
listarClientes::VideoClub->[Cliente]
listarClientes (_,clx,_) = [e | e <-clx]




--b. películas :: VideoClub -> [Película], que devuelve una lista con las películas que están en el catálogo de videoclub, independientemente de si está disponible o no.
listarPeliculas::VideoClub->[Pelicula]
listarPeliculas (_, _,pelix) = [e | e <-pelix]

-- example listarPeliculas (1,[],[(1,"viti",2),(1,"monos",4)])




--c. cantidadDeCopias :: VideoClub -> Película -> Int, que devuelve cuántas copias de la
--película tiene el videoclub (esta función se indefine, o sea da error, si la película no
--pertenece al catálogo del videoclub).

-- explicacion:
-- cada pelicula guardara el numero de copias que le estan haciendo
-- y ademas de que quien le esta haciendo , identificando cada videoclub con un a llave primaria
-- derivandola en la pelicula como llave foranea, por lo tanto si no coinciden las claves
-- simplemente no pertenecenny bota error->
cantidadCopias::VideoClub->Pelicula->Int
cantidadCopias  (idVideoClub,_,_ )(idForanea,_,nrocopias) | idVideoClub /= idForanea = error "dicha pelicula pertence a otro catalogo"
													| otherwise = nrocopias

-- example cantidadCopias (1,[],[]) (1,"viti",32)






--d. pelıculas Alquiladas :: Videoclub -> Cliente -> [Película], que devuelve una lista de
--películas que el cliente tiene alquiladas en el videoclub (esta función se indefine si el
--cliente no está registrado en el videoclub). El cliente puede alquilar más de una copia de
--la misma película.

--pertenece::VideoClub->Cliente->Bool
--pertenece (id,[], pelix) (name,pelixal) = False
--pertenece (id, (cli:clix) ,pelix ) (name, pelixal) |  (name == cli) = True
																	-- | otherwise = pertenece (id,(clix),pelix) (name,pelixal)



--peliculasAlquiladas::VideoClub->Cliente->[Pelicula]
--peliculasAlquiladas (idVideoClub,clix,pelix) (name,pelixalq) | not ( pertenece (idVideoClub,clix,pelix) (name,pelixalq) ) = error "el cliente no pertenece"
--																	| otherwise = [(1,"test",34)]




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
