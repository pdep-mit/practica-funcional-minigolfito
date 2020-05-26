module Lib where

import Text.Show.Functions

-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------

{-
    a. Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura.

    - El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
    - La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
    - Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.
-}

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {
  velocidad = 10,
  precision = precisionJugador habilidad * 2,
  altura = 0
  }
madera :: Palo
madera habilidad = UnTiro {
  velocidad = 100,
  precision = precisionJugador habilidad `div` 2,
  altura = 5
  }

-- Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.
hierro :: Int -> Palo
hierro n habilidad = UnTiro {
  velocidad = fuerzaJugador habilidad * n,
  precision = precisionJugador habilidad `div` n,
  altura = (n - 3) `max` 0
}
{-
    b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.
-}

palos :: [Palo]
palos = [putter , madera] ++ map hierro [1..10]

{-
Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.
-}

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

golpe' :: Palo -> Jugador -> Tiro
golpe' palo = palo . habilidad

golpe'' :: Jugador -> Palo -> Tiro
golpe'' jugador palo = (palo.habilidad) jugador