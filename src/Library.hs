module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

-- 1)
{- a) Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si
 el resultado de evaluar esa función sobre el primer valor es mayor o menor que el resultado de evaluarlo 
 sobre el segundo valor respectivamente.
 -}


mayor :: (a -> b) -> a -> a -> Bool
mayor f = f . > . f 

menor :: (a -> b) -> a -> a -> Bool
menor f v1 v2 = f v1 > f v2



















