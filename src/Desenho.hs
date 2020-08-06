-- | Módulo relacionado com o desenho do Jogo
module Desenho where

import Data
import Globals
import Data.Maybe
import LI11920
import Tarefa0_2019li1g177
import F_Aux
import Graphics.Gloss.Interface.Pure.Game


-- | Associa uma imagem diferente para cada 'Jogador'
--
-- É possível distinguir diferentes 'Jogador'es agora 

constroiJogador :: Estado -- ^ Estado do jogo
 -> [Picture] -- ^lista de pictures
 -> [Float] -- ^ lista de posições y
 -> Double -- ^ distância do último jogador
 -> PropJogadores -- ^ propriedades dos jogadores
 -> [Picture] -- ^ lista de pictures que são os jogadores (motas)
constroiJogador (Estado m (j: js)) (bike : bikes) py uP ((_, _ , name,_) : names) =
    (transformaJogador j m bike py uP name) : constroiJogador (Estado m js) bikes py uP names
constroiJogador _ _ _ _ _ = []

-- | Transformação geométrica do 'Jogador' no mapa cartesiano

transformaJogador :: Jogador -- ^ 'Jogador' a receber a transformação geométrica
 -> Mapa -- ^ 'Mapa' do jogo
 -> Picture -- ^ 'Picture' associada ao jogador
 -> [Float] -- ^ posições y das 'Pista's
 -> Double -- ^ posição x do último 'Jogador'
 -> String
 -> Picture -- ^ 'Picture' do 'Jogador' transformada geometricamente
transformaJogador j@(Jogador p d v c e) m mota py uP name = Pictures [Translate x y (Rotate z  ( Scale 0.3 0.3 mota) ), Translate (x - (realToFrac (length name)) * 4 ) (y+30)  (Scale 0.15 0.15 (Text name)) ]
 where x = ((realToFrac ((d - uP) * tPeca)) + iX) - (0.13 * (fromIntegral p) * (realToFrac tPeca))
       y = (encontraIndiceLista p py) + ((fst hz) * (realToFrac tPecaY)) +7
       z = negate $ normalizaAnguloM (snd hz) -- (negate (snd hz)) / normalizaEscala
       hz = alturaJ j m
       tPCentro = realToFrac (tPeca/2)

-- * Construção do Mapa

-- | Desenha um 'Mapa' através da função pistaMapa

constroiMapa :: Mapa -- ^ 'Mapa' que pretende-se desenhar 
 -> [Float]   -- ^ Coordenadas das imagens no eixo dos Y
 -> [Jogador] -- ^Lista de 'Jogador'es 
 -> Double -- ^ posição do último 'Jogador'
 -> [Picture]   -- ^ 'Mapa' final desenhado
constroiMapa m l js uP = mapa
                         where mapa = percorreMapa mPar 0 uP
                               mPar = zipWith (,) l m                

-- | Percorre o mapa adicionando (-0.13) à posição x das pistas para ficarem alinhadas
percorreMapa :: [(Float,Pista)] -> Float -> Double -> [Picture]
percorreMapa [] _ _ = []
percorreMapa ((y,p):t) x u = (pistaMapa p x y u) ++ percorreMapa t (x-0.13) u
-- | Desenha uma 'Pista' com as imagens associadas a cada 'Peca'
-- | Utliza a função desenhaPeca para obter as imagens de cada 'Peca'

pistaMapa :: Pista -- ^ 'Pista' que pretende-se desenhar
 -> Float -- ^ Coordenada no eixo do x
 -> Float -- ^ Coordenada no eixo do y
 -> Double -- ^ Posição último 'Jogador'
 -> [Picture] -- ^ 'Pista' final desenhada
pistaMapa [] x y u  = [meta x y u]
pistaMapa (h : t) x y u = (desenhaPeca h xR y) ++ (pistaMapa t (x + 1) y u)
 where xR = (((x - (realToFrac u)) * (realToFrac tPeca) ) + iX)

-- | Junta as duas funções ,desenhaPecaRampa e desnhaPecaReta, numa só que as escolhe de acordo com o tipo de 'Peca' 

desenhaPeca :: Peca -- ^ 'Peca' fornecida 
 -> Float -- ^ Coordenadas no eixo do x
 -> Float -- ^ Coordenadas no eixo do y
 -> [Picture] -- ^ Imagem associada a uma 'Peca'
desenhaPeca p x y = case p of
    Recta _ _   -> desenhaPecaReta p x y 
    Rampa _ _ _ -> desenhaPecaRampa p x y 

-- | Associa uma 'Peça' a uma determinada imagem 
-- | As peças sao todas do tipo Reta Piso Int 

desenhaPecaReta :: Peca -- ^ 'Peca' fornecida 
 -> Float -- ^ Coordenadas no eixo do x
 -> Float -- ^ Coordenadas no eixo do y
 -> [Picture] -- ^ Imagem associada a uma 'Peca' 
desenhaPecaReta p x y = case p of 
    Recta Terra _ -> [Color orange retaT, Color (dim orange) contorno]
    Recta Cola _  -> [Color white retaT, Color (dim white) contorno]
    Recta Boost _ -> [Color red retaT, Color (dim red) contorno]
    Recta Relva _ -> [Color green retaT, Color (dim green) contorno]
    Recta Lama _  -> [Color black retaT, Color (light black) contorno]
    _ -> [Blank]
    where tP = realToFrac tPeca
          (hi, hf) = verificaAlturaPeca p
          retaT = polygon [p1,p2,p3,p4,p1,p5,p6,p7,p8,p5,p1,p2,p6,p4]
          contorno = line [p5,p6,p7,p8]
          p1 = (x, yA)
          p2 = (x + tP, yA)
          p3 = (x + tP + xD, yA + yD)
          p4 = (x + xD, yA+yD)
          p5 = (x, yA + (fromIntegral hf) * tPecaY + 0.1)
          p6 = (x + tP, yA + (fromIntegral hf) * tPecaY + 0.1)
          p7 = (x + tP + xD, yA + yD + (fromIntegral hf) * tPecaY + 0.1)
          p8 = (x + xD, yA + yD + (fromIntegral hf) * tPecaY + 0.1)
          yA = y - 25
          xD = 20
          yD = 60

-- | Associa uma 'Peça' a uma imagem 
-- | As imagens são todas do tipo Rampa Piso Int Int 

desenhaPecaRampa :: Peca -- ^ 'Peca' fornecida 
 -> Float -- ^ Coordenadas no eixo do x
 -> Float -- ^ Coordenadas no eixo do y
 -> [Picture] -- ^ Imagem associada a uma 'Peca'
desenhaPecaRampa p x y = case p of
    Rampa Terra _ _ -> [Color orange rampaT, Color (dim orange) contorno]
    Rampa Relva _ _ -> [Color green rampaT, Color (dim green) contorno]
    Rampa Lama _ _  -> [Color black rampaT, Color (light black) contorno]
    Rampa Cola _ _  -> [Color white rampaT , Color (dim white) contorno]
    Rampa Boost _ _ -> [Color red rampaT , Color (dim red) contorno]
    _ -> [Blank]
    where incP = (inclinacaoPeca p) > 0
          (hi, hf) = verificaAlturaPeca p
          h = if incP then hi else hf
          tP = realToFrac tPeca
          rampaT = polygon [p0,p1,p2,p3,p0, p4,p5,p6,p7,p6, p2,p3,p7,p6,p5,p1,p2,p6]
          contorno = line [p2,p3,p7,p6]
          p0 = (x,yA)
          p1 = (x + tP, yA)
          p2 = (x + tP, yA + ((fromIntegral hf) * tPecaY))
          p3 = (x, yA + ((fromIntegral hi) * tPecaY))
          p4 = (x+xD,yA+yD)
          p5 = (x + tP+xD, yA+yD)
          p6 = (x + tP+xD, yA + yD+ ((fromIntegral hf) * tPecaY))
          p7 = (x+xD, yA + yD + ((fromIntegral hi) * tPecaY))
          yA = y - 25
          xD = 20
          yD = 60


-- * Munições

-- | Percorre os jogadores para criar o número de munições

contadorMunicao :: [Jogador] -> Float -> Float -> Int -> Int -> [Picture]
contadorMunicao ((Jogador _ _ _ c _) : t) x y nj i = (contadorMunicaoAux c x y i) : contadorMunicao t (x + dc) y nj (succ i)
                                                 where dc = fromIntegral (div ((fromIntegral width) - 200) nj)
contadorMunicao _ _ _ _ _ = []

-- | Coloca um contador da quantidade de munições
contadorMunicaoAux :: Int -> Float -> Float -> Int -> Picture
contadorMunicaoAux c x y i = Translate x y (Scale 0.3 0.3 (Text ("Jogador " ++ (show i) ++ ": " ++ (show c)) ) )

-- | Cronómetro
contador :: Float -> Float -> Float -> Picture
contador t x y = Translate x y (Scale 0.2 0.2 (Text ("Tempo: " ++ (show t)) ) )

-- * Construção da Meta

-- | Construção da meta
meta :: Float -- ^ posição x da meta
 -> Float -- ^ posição y da meta
 -> Double -- ^ posição do último 'Jogador'
 -> Picture -- ^ desenho da meta
meta x y u = Color cyan $ polygon [p0, p1, p2, p3, p4, p5, p0]
 where tP = realToFrac tPeca
       uR = realToFrac u
       xR = (x - uR) * tP + iX
       yA = y - 25
       xD = 20
       yD = 60
       p0 = ( xR , yA )
       p1 = ( xR+tP , yA )
       p2 = ( xR + tP + xD, yA + yD)
       p3 = ( xR + tP, yA + yD )
       p4 = ( xR + xD , yA + yD )
       p5 = ( xR , yA )

-- | Dado a lista de jogadores, calcula a posição do último 'Jogador'
lastPlayer :: Double -> [Jogador] -> PropJogadores -> Double
lastPlayer dm (j : js) ((i, jog,_,_) : ij ) | jog == Perdeu = lastPlayer dm js ij
                                            | dm < d = lastPlayer dm js ij
                                            | dm > d = lastPlayer d js ij
                                            where d = distJ j
lastPlayer dm _ _ = dm

-- | Recebe uma lista de (Int, 'Joga') e cria uma lista de 'Picture's com os que não estão a jogar.
listaFimPlayer :: Int
 -> (PropJogadores, PropJogadores) 
 -> (Float, Float)
 -> Color
 -> [Picture]

listaFimPlayer n ((i, Fim ts, name, _) : t, l2) (x, y) color = 
    nome : tempo : posicao : listaFimPlayer (succ n) (t, l2) (x, y - 50) color
    where nome = Color color $ Translate x y (Scale 0.18 0.18 (Text name ) )
          tempo = Color color $ Translate (x + 500) y (Scale 0.18 0.18 (Text (show ts) ) )
          posicao = Color color $ Translate (x - 100) y (Scale 0.18 0.18 (Text  ((show (succ n)) ++ ".") ) )


listaFimPlayer n ((i, Perdeu, name, _) : t, l2 ) (x, y) color = 
    nome : lost : posicao : listaFimPlayer n (t, l2) (x, y - 50) color
    where nome = Color color $ Translate x y (Scale 0.18 0.18 (Text ( name ) ) )
          lost = Color color $ Translate (x + 500) y (Scale 0.18 0.18 (Text "Perdeu" ) )
          posicao = Color color $ Translate (x - 100) y (Scale 0.18 0.18 (Text  "-" ) )

listaFimPlayer n ([], []) xy c = []
listaFimPlayer n ([], l2) xy c = listaFimPlayer n (l2, []) xy c
listaFimPlayer n _ _ _ = []


-- | Constroi a posição dos jogadores durante o jogo
posicaoPlayers :: [(Jogador, (Int, Joga, String, Float))]
 -> (Float,Float)
 -> Int
 -> [Picture]
posicaoPlayers [] _ _ = []
posicaoPlayers ((j, (_,joga,name,_)):js) (x,y) n = player : perdeu : posicaoPlayers js (x, y-25) (succ n)
    where player = Translate x y (Scale 0.15 0.15 (Text ((show n) ++ ".  " ++ fName)))
          fName = if (length name) == 0 then "Jogador " ++ (show n) else name 
          perdeu = Translate (x + 300) y (Scale 0.15 0.15 (Text perdeuFim))
          perdeuFim = if jPerdeu joga then "Perdeu"
                      else if isJust (getFim joga) then show (fromJust (getFim joga))
                      else ""
-- * Semaforo

-- | Desenho do <Semáforo> com o contador inicial
criaSemaforo :: Float -> Picture
criaSemaforo cr | cr > 0 && cr < 2 = Translate (-600) 200 (Scale 0.2 0.2 (Text "Go!!!!!"))
                | cr == 0 = Translate (-600) 200 (Scale 0.2 0.2 (Text "Ready????"))
                | otherwise = Translate (-600) 200 (Scale 0.2 0.2 (Text ""))

-- * Aqecimento do motor
-- | Desenha o estado do motor dos 'Jogador's
estadoMotor :: PropJogadores
 -> (Float, Float)
 -> Int
 -> [Picture]
estadoMotor [] _ _ = []
estadoMotor ((_,_,_,m):mjs) (x,y) nj = (Color red bar) : limit : motor : estadoMotor mjs (x+dc,y) nj
    where bar = polygon [p0,p1,p2,p3,p0]
          limit = Color black $ line [p0,(x+200,y),(x+200,y-31),(x,y-31),p0]
          motor = Translate (x+10) (y + 20) (Scale 0.15 0.15 $ Text "Aquecimento Motor")
          p0 = (x,y)
          p1 = (x+(m*4),y)
          p2 = (x+(m*4), y - 30)
          p3 = (x, y - 30)
          dc = fromIntegral (div ((fromIntegral width) - 200) nj)
               