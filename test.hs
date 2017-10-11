data Complejos = Monos Float Float deriving Show
--data Person = Person String String Int Float String String deriving (Show)

real::Complejos->Float
real (Monos a b ) = a

suma::Complejos->Complejos->Complejos
suma (Monos a b) (Monos c d) = Monos (a+c) (b+d)

producto::Complejos->Complejos->Complejos
producto (Monos a b) (Monos c d) = Monos (a*c - b*d) (a*d + b*c)



data Dias = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo  deriving Eq
finSemana::Dias->Bool
{-
-- finSemana Sabado = True
-- finSemana Domingo = True
-- finSemana _ = False -}
finSemana dia | dia == Sabado || dia == Domingo = True
						| otherwise = False


-- Example FinSemana Sabado = true


data Dias2 = DiasTwo String deriving Eq
endWeekend::Dias2->Bool
endWeekend (DiasTwo a) | a == "Sabado" || a == "Domingo" = True
						| otherwise = False




type Polinomio = [Float]

grado::Polinomio->Int
grado px = length px - 1



coeficiente::Polinomio->Int->Float
coeficiente px pos = head [l | (l,exp)<-zip px [(length px) - 1, (length px)-2..0], exp == pos]


--evaluar::Polinomio->Float->Float
--evaluar px 

--type Matriz = [[Float]]


--f::Float->Float
f x = 2*x
--sf2::Float->Float
f2 = f . f