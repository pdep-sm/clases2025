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

-- primera definición de par
esPar n = mod n 2 == 0
-- convertimos al mod en infijo para pensar en separar el parámetro n
esPar' n = n `mod` 2 == 0
-- aplicamos parcialmente la igualdad (0==) y `mod` 2, y los componemos dejando el n afuera
esPar'' n = ((0==) . (`mod` 2)) n 
-- por último cambiamos la notación para que quede como simplemente una equivalencia
esPar''' = (0==) . (`mod` 2)


-- Por una gran idea de un estudiante, hacemos una generalización
esMultiploDe n m = mod m n == 0

-- Hacemos la versión de esPar haciendo una aplicación parcial de la función creada
esPar'''' = esMultiploDe 2


