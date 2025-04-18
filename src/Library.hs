module Library where

import PdePreludat

------------------ 12.04.2025 ------------------

-- Data (record syntax)

data Jugador = Jugador {
    nombre :: String,
    goles :: Goles,
    asistencias :: Asistencias,
    suspendido :: Suspendido
} deriving (Show)

type Nombre = String
type Goles = Number
type Asistencias = Number
type Suspendido = Bool

{-

type Jugador = (Nombre, Goles, Asistencias, Suspendido)

tom :: Jugador
tom = ("Tom", 15, 6, True)

goles (_ , g , _ , _) = g

-}

tom = Jugador "Tom" 15 6 True -- sin record syntax

nico = Jugador {
    nombre = "Nico",
    goles = 16,
    asistencias = 10,
    suspendido = False
} -- con record syntax


-- hizoGoles gs (n, g, a, s) = (n, g + gs, a, s) {- opción fea -}

hizoGoles :: Goles -> Jugador -> Jugador
hizoGoles gs jug = jug {goles = goles jug + gs} {- opción más mantenible y expresiva -}

-- asistio as (n, g, a, s) = (n, g, a + as, s) {- opción fea -}

asistio :: Asistencias -> Jugador -> Jugador
asistio as jug = jug {asistencias = asistencias jug + as} {- opción más mantenible y expresiva -}

-- fueExpulsado exp (n, g, a, _) = (n, g, a, exp) {- opción fea -}

fueExpulsado :: Suspendido -> Jugador -> Jugador
fueExpulsado exp jug = jug {suspendido = exp} {- opción más mantenible y expresiva -}

-- (f . g) x = f (g x)

type Partido = (Goles, Asistencias, Suspendido)

-- jugoPartido (gs, as, exp) jugador = (fueExpulsado exp . asistio as . hizoGoles gs) jugador

jugoPartido :: Partido -> Jugador -> Jugador
jugoPartido (gs, as, exp) = fueExpulsado exp . asistio as . hizoGoles gs {- 'eta reduction' de lo anterior -}


{-

CONSOLA:

> hizoGoles 4 (Jugador "Mati" 4 4 False)
Jugador { 
    nombre = "Mati", 
    goles = 8, 
    asistencias = 4, 
    suspendido = False }

-}

------------------------------------------------

{- [5] ≡ 5 : [] , son equivalentes -}

-- head (x : _) = x
-- tail (_ : xs) = xs

{-
CONSOLA: 

> head [1, 2, 3]
1

> tail [1, 2, 3]
[2, 3]

-}

{-
CONSOLA: 

> length [1, 2, 4]
3

> length []
0

> length [1..10]
10

> length [1..]
* nunca termina *

> take 10 [1..]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-}

{-
length :: [a] -> Number
length [] = 0
length (_ : xs) = 1 + length xs
-}

length' :: [a] -> Number
length' lista
    | null lista = 0
    | otherwise = 1 + (length' . tail) lista

