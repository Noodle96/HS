--
--	Examen 1 de haskell
-- (dejar 10 espacios por problema)
--










--
--	Pregunta 2. Definir la función  esPositivo que verifica si el número ingresado es entero positivo.
--
esPositivo::Integer->Bool
esPositivo n | n > 0 = True
					| otherwise = False








--
--	Pregunta 4 .- El área de un triángulo con lados a , b , c es dado con la formula sqrt ( s ( s − a )( s − b )( s − c ) )
-- donde s = ( a + b + c ) / 2 .Definir la función areaTr
--
-- requisito problema 3











--
--	6. Definir la función sumatoria que recibe dos enteros con argumentos, que son el número
--inicial y final respectivamente de una sucesión de números uno a uno, y retorna la
--sumatoria.
--
sumatoria::Integer->Integer->Integer
sumatoria a b | a == b = a
					| otherwise = a + sumatoria (a+1) b














--
--	8. Definir una función para la proposición condicional p → q ,
--
proposicionCondicional::Bool->Bool->Bool
proposicionCondicional a b | a == True && b == False = False
									| otherwise = True







--
--	10. Defina una función que reciba una lista de enteros y retorne otra lista con sólo elementos
--que estuvieron en posición par de la lista de entrada (comiencen a contar las posiciones
--desde 1). Ejemplo : posPar [2,4,6,8,10] = [4,8] (porque estan en las posiciones 2,4
--respectivamente).
--
listaPosicionPar::[Integer]->[Integer]
listaPosicionPar xs = [ q | (p,q) <- zip [1 .. length xs] xs , even p  ]








--
--	Definir una función adjuntar :: Int -> Int -> [Int] -> [Int] donde el primer argumento es el
--entero a insertar a la lista , el segundo argumento es la posición donde el primer entero será
--insertado en la lista y el tercero ser la lista donde se insertará el primer entero. (comenzar a
--contar las posiciones desde cero) Ejemplo: adjuntar 789 2 [1,5,6,7,8] =[1,5,789,6,7,8]
--
--adjuntar::Int->Int->[Int]->[Int]
--adjuntar e p xs
adjunto::Int->Int->[Int]->[Int]
adjunto e pos (x:xs) = (e:x)






































-- End
