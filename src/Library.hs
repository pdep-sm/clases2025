module Library where
import PdePreludat

tom :: Jugador
tom = Jugador "Tom" 15 6 True

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

head' :: [a] -> a
head' (cabeza:cola) = cabeza

f :: Number -> Number
f 10 = 5
f _ = 10

g :: Number -> Number
g n
    | n == 10 = 5
    | otherwise = 10

length' :: [a] -> Number
length' [] = 0
length' (_:xs) = 1 + length' xs

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n - 1)

take' :: Number -> [a] -> [a]
take' 0 _  = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

numerosDesde, numerosDesde' :: Number -> [Number]
numerosDesde n = n : numerosDesde (n+1)
numerosDesde' n = [n..]

data Jugador = Jugador {
    nombre :: String,
    goles :: Number,
    asistencias :: Number,
    suspendido :: Bool
} deriving Show

type Equipo = [Jugador]

-- Goles del equipo en conjunto
totalGoles, totalGoles', totalGoles'' :: Equipo -> Number
totalGoles   equipo = (goles . head) equipo + (totalGoles . tail) equipo
totalGoles'  equipo = (sum . golesEquipo') equipo
totalGoles''        = sum . golesEquipo'

-- Asistencias del equipo en conjunto
totalAsistencias :: Equipo -> Number
totalAsistencias = sum . asistenciasEquipo'

-- Nuestra propia definición de map: Obtener la lista resultante de transformar cada elemento de otra lista
map' :: (t -> a) -> [t] -> [a]
map' funcion [] = []
map' funcion (x : xs) = funcion x : map' funcion xs

-- Goles de cada jugador del equipo
golesEquipo', golesEquipo'' :: Equipo -> [Number]
golesEquipo'  equipo = map' goles equipo
golesEquipo''        = map goles

-- Asistencias de cada jugador del equipo
asistenciasEquipo' :: Equipo -> [Number]
asistenciasEquipo' = map asistencias

-- Nombres de los jugadores
plantel' :: Equipo -> [String]
plantel' = map nombre

-- Jugadores que hicieron goles
goleadores, goleadores', goleadores'' :: Equipo -> [Jugador]
goleadores   equipo = filter  hizoGoles      equipo
goleadores'  equipo = filter  ((>0) . goles) equipo
goleadores''        = filter' ((>0) . goles)

hizoGoles :: Jugador -> Bool
hizoGoles = (>0) . goles

-- Nombres de los jugadores que hicieron goles
plantelGoleador, plantelGoleador' :: Equipo -> [String]
plantelGoleador  equipo = (plantel' . goleadores) equipo 
plantelGoleador'        = plantel' . goleadores

-- Nuestra propia definición de filter: ¿qué elementos de una lista cumplen una condición?
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' condicion (x:xs) 
    | condicion x = x : filter' condicion xs
    | otherwise   = filter' condicion xs

-- Nombres de los jugadores que hicieron al menos 5 asistencias
plantelAsistidor :: Equipo -> [String]
plantelAsistidor = map nombre . filter ((>= 5) . asistencias) 
-- any
-- all

-- ¿El equipo hizo goles?
equipoHizoGoles, equipoHizoGoles' :: Equipo -> Bool
equipoHizoGoles  equipo = any hizoGoles equipo
equipoHizoGoles'        = any hizoGoles








{- Las funciones que descartamos rápidamente por ser poco declarativas y duplicar código:

golesEquipo :: Equipo -> [Number]
golesEquipo [] = []
golesEquipo (jugador : jugadores) = goles jugador : golesEquipo jugadores

asistenciasEquipo :: Equipo -> [Number]
asistenciasEquipo [] = []
asistenciasEquipo (jugador : jugadores) = asistencias jugador : asistenciasEquipo jugadores

plantel :: Equipo -> [String]
plantel [] = []
plantel (jugador : jugadores) = nombre jugador : plantel jugadores
-}