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

{-
Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo.

Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.

En principio necesitamos representar los siguientes obstáculos:
- Tunel con rampita
- Laguna
- Hoyo
-}
data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }
intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal
  | (puedeSuperar obstaculo) tiroOriginal = (efectoLuegoDeSuperar obstaculo) tiroOriginal
  | otherwise = tiroDetenido
{-
Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.
-}
tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

tiroDetenido = UnTiro 0 0 0

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo = (== 0).altura

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiroOriginal = UnTiro {
  velocidad = velocidad tiroOriginal *2,
  precision = 100,
  altura = 0 }
{-
Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.
-}
laguna :: Int -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {
    altura = altura tiroOriginal `div` largo
  }
{-
Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-}

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && vaAlRasDelSuelo tiro && precision tiro > 95
efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

{-
data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }
-}

{-
Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.
-}

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

palosUtiles' jugador obstaculo = filter (puedeSuperar obstaculo . golpe jugador) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo = puedeSuperar obstaculo . golpe jugador

{-
Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.

Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
-}

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos)
  | puedeSuperar obstaculo tiro
      = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos
  | otherwise = 0

{-
Ejemplos para probar:
*Lib Lib> cuantosObstaculosConsecutivosSupera (UnTiro {velocidad = 95, altura = 2, precision = 95}) [laguna 3, tunelConRampita, tunelConRampita, hoyo]
3
*Lib Lib> cuantosObstaculosConsecutivosSupera (UnTiro {velocidad = 95, altura = 2, precision = 95}) [laguna 3, tunelConRampita, laguna 1, tunelConRampita, hoyo]
2
-}

cuantosObstaculosConsecutivosSupera' :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera' tiro obstaculos
  = (length . takeWhile (\(obstaculo, tiroQueLeLlega) -> puedeSuperar obstaculo tiroQueLeLlega)
      . zip obstaculos . tirosSucesivos tiro) obstaculos

tirosSucesivos :: Tiro -> [Obstaculo] -> [Tiro]
tirosSucesivos tiroOriginal obstaculos
  = foldl (\tirosGenerados obstaculo ->
         tirosGenerados ++
           [efectoLuegoDeSuperar obstaculo (last tirosGenerados)]
      ) [tiroOriginal] obstaculos