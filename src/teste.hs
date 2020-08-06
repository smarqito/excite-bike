module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Tarefa0_2019li1g177

-- * Estado Jogo

type Estado = (Float,Float, Bool, Float, Float, Float)

estadoInicial :: Estado
estadoInicial = (negate (800/2) + 80,3, False, 0, 0, 0)

poligno :: Picture
poligno = Polygon [(0,0),(100,0),(100,200),(0,0)]

desenhaEstado :: Estado -> Picture
desenhaEstado (x,y, _, _, _, z) = Pictures [Scale 0.5 0.5 (Rotate z (Translate x m poligno)) | m <- (linear dm 4) ]

-- | reage a eventos do teclado
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (x,y, _, _, _, z) = (x,y+1, True, 0,0, z)
reageEvento (EventKey (SpecialKey KeyUp)    Up _ _) (x,y, _, _, _, z) = (x,y, False, 0, 0, z)
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (x,y, _, _, _, z) = (x,y-1, True, 0,0, z)
reageEvento (EventKey (SpecialKey KeyDown)  Up _ _) (x,y, _, _, _, z) = (x,y, False, 0, 0, z)

reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (x,y, _, _, _, z) = (x-5,y, True, (-5), 0, z)
reageEvento (EventKey (SpecialKey KeyLeft)  Up _ _) (x,y, _, _, _, z) = (x,y, False, 0, 0, z)

reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (x,y, _, _, _, z) = (x+5,y, True, 5, 0, z)
reageEvento (EventKey (SpecialKey KeyRight) Up _ _) (x,y, _, _, _, z) = (x,y, False, 0, 0, z)

reageEvento (EventKey (MouseButton LeftButton) Down _ _) (x,y, _, _, _, z) = (x+5,y+1, True, 5, 0, z)
reageEvento (EventKey (MouseButton LeftButton) Up _ _) (x,y, _, _, _, z) = (x+5,y+1, False, 0, 0, z)

reageEvento (EventKey (MouseButton WheelUp) Down _ _) (x,y, _, _, _, z) = (x,y+1, True, 0, 0, z)
reageEvento (EventKey (MouseButton WheelUp) Up _ _) (x,y, _, _, _, z) = (x,y, False, 0, 0, z)

reageEvento (EventKey (SpecialKey KeySpace) Down _ _) (x,y, a, b, c, z) = (x,y, a, b, c, z + 15)
reageEvento (EventKey (Char 'z') Down _ _) (x,y, a, b, c, z) = (x,y, a, b, c, z - 15)


-- todo será semelhante isto mas com uma chamada à tarefa 2
reageEvento _ s = s -- ignora qualquer outro evento

reageTempo :: Float -> Estado -> Estado
reageTempo n (x, y, True, a, b, z) = (x + a, y + b, True, a, b, z)
reageTempo n (x,y,w,a,b, z) = (x,y, w, a,b, z)

-- todo tarefa 4

-- * Estado Gloss

type EstadoGloss = (Estado,Picture)

estadoGlossInicial :: Picture -> EstadoGloss
estadoGlossInicial bola = (estadoInicial,bola)

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss ((x,y, _, _, _, z),bola) = Pictures ([(Translate x (encontraIndiceLista (floor y) l) (Rotate z (Scale 0.3 0.3 bola)) ) ] ++ [ ((Translate (-320) m (Scale 0.3 0.3 bola) ) ) | m <- (snd t) ]) -- Rotate (realToFrac z) (Translate x y (Scale 0.5 0.5 bola))
 where t = splitAt 1 l
       l = reverse (ordena (linear dm 4))

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss ev (e,bola) = (reageEvento ev e,bola)

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (e,bola) = (reageTempo t e,bola)


ordena :: Ord a => [a] -> [a]
ordena l = ordenaAux [] l

ordenaAux :: Ord a => [a] -> [a] -> [a]
ordenaAux l1 [] = l1
ordenaAux l1 (h : t) = ordenaAux (insert h l1) t
-- bola :: Picture
-- bola = Color blue (Text "MIEI #177")

cadaPista :: (Int, Int)
cadaPista = (200, 100)

linear :: Display -> Int -> [Float]
linear (InWindow _ (w, h) _) n = linearAux n cP tP
    where tP = tamanhoPista n
          cP = div tP 2 -- centro da pista
          hM = div (h-300) 2 -- altura máximo em cada parte (negativo e positivo)

linearAux :: Int -> Int -> Int -> [Float]
linearAux 0 acc _ = []
linearAux 1 acc d = [realToFrac (acc + d)]
linearAux n acc d = (realToFrac acc) : (negate (realToFrac acc)) : linearAux (n-2) (acc + d) d


tamanhoPista :: Int -> Int
tamanhoPista n = div (800 - 300) n -- tamanho da pista (diferença entre elas) 
-- linear = [100, 300, (-100), (-300)]

fr :: Int
fr = 50

dm :: Display
dm = InWindow "Novo Jogo" (800, 800) (0, 0)

main :: IO ()
main = do 
    bola <- loadBMP "./assets/images/bike.bmp"
    play dm                       -- janela onde irá correr o jogo
        (greyN 0.5)               -- côr do fundo da janela
        fr                        -- frame rate
        (estadoGlossInicial poligno) -- estado inicial
        desenhaEstadoGloss        -- desenha o estado do jogo
        reageEventoGloss          -- reage a um evento
        -- todo tarefa 2
        reageTempoGloss           -- reage ao passar do tempo
        -- todo tarefa 4




-- ! videos ano anterior (gloss tutorial - LI), página do cesium