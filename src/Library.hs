module Library where
import PdePreludat

limiteVelocidad = 60

-- esExceso:: Int -> Bool
esExceso velocidad = velocidad > limiteVelocidad

valorMulta velocidad
  | esExceso velocidad = (velocidad - limiteVelocidad) * 10000
  | otherwise = 0

puntosDeDescuento valor 
  | valor > 150000 = 10
  | valor > 100000 = 5
  | otherwise = 0

puntosPorVelocidad velocidad = (puntosDeDescuento . valorMulta)  velocidad
puntosPorVelocidad' = puntosDeDescuento . valorMulta

esPar n = mod n 2 == 0
esPar' n = n `mod` 2 == 0
esPar'' n = ((0==) . (`mod` 2)) n 
esPar''' = (0==) . (`mod` 2)

esMultiploDe n m = mod m n == 0

esPar'''' = esMultiploDe 2


