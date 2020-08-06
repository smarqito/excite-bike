-- | Funções auxiliares
module F_Aux where

import LI11920
import Tarefa0_2019li1g177
import Data.List
import Data.Maybe
import Globals
import System.Random

-- * Funções para retornar propriedades do 'Jogador'

-- | Devolve a 'Pista' do 'Jogador'

pistaJ :: Jogador -- ^ 'Jogador' que se quer a 'Pista'
 -> Int -- ^ 'Pista' do 'Jogador'
pistaJ (Jogador p d v c e ) = p

-- | Devolve a 'Distancia' do 'Jogador'

distJ :: Jogador -- ^ 'Jogador' que se quer a Distância
 -> Double -- ^ distância do 'Jogador'
distJ (Jogador p d v c e ) = d

-- | Devolve a 'Velocidade' do 'Jogador'

velocJ :: Jogador -- ^ 'Jogador' que se quer a velocidade
 -> Double -- ^ velocidade do 'Jogador'
velocJ (Jogador p d v c e ) = v

-- | Devolve a 'Velocidade' do 'Jogador'

colaJ :: Jogador -- ^ 'Jogador' que se quer a quantidade de cola disponível
 -> Int -- ^ quantidade de cola disponível no 'Jogador'
colaJ (Jogador p d v c e ) = c

-- | Dado um 'Jogador', retorna o seu 'Estado'

estadoJ :: Jogador -- ^ 'Jogador' que se quer o 'EstadoJogador'
 -> EstadoJogador -- ^ 'EstadoJogador' do 'Jogador' em questão
estadoJ (Jogador p d v c e) = e

-- | Dado um 'Jogador', retorna a sua inclinação
--
-- Quer o 'Jogador' esteja no Ar ou no Chao
-- 
-- Se o 'Jogador' estiver no 'Ar' devolve a sua inclinação.
--
-- Se o 'Jogador' estiver no 'Chao' devolve a inclinação da 'Peca' em que se encontra
inclinaJ :: Jogador -- ^ 'Jogador' que se quer a inclinação
 -> Peca -- ^ 'Peca' em que o 'Jogador' se encontra
 -> Double -- ^ inclinação do 'Jogador'
inclinaJ j@(Jogador p d v c e) pp = case e of
    Ar _ i _ -> i
    Chao _ -> radToDeg (inclinacaoPeca pp)
    Morto _ -> 0

-- | altura do 'Jogador' numa dada posição

alturaJ :: Jogador -- ^ 'Jogador' que se quer a altura
 -> Mapa -- ^ do 'Jogador'
 -> (Float, Float) -- ^ retorna um tuplo com a altura do jogador e a sua inclinação
alturaJ j@(Jogador p d v c e) m = case e of
    Ar h i g -> (realToFrac (max h 0), realToFrac i)
    _ -> (aposx, iPeca)
        where pp = if ePosicaoMatrizValida (p, dist) m then m !! p !! dist else  m !! p !! (dist - 1)
              dist = floor d
              apeca = verificaAlturaPeca pp
              iPeca = realToFrac $ inclinaJ j pp -- realToFrac (radToDeg (inclinacaoPeca pp))
              posx = d - (fromIntegral dist)
              aposx = realToFrac (calculaAlturaPosicao apeca posx)

-- | Verifica se o 'Jogador' está 'Morto'

isMorto :: Jogador
 -> Bool
isMorto j@(Jogador _ _ _ _ e@(Morto _)) = True
isMorto _ = False

-- | Função que devolve o jogador com o 'EstadoJogador' 'Morto'.
-- Utilizada para a propriedade motor.
mataJogador :: Jogador
 -> Jogador
mataJogador (Jogador p d v c e) = Jogador p d 0 c (Morto timeOutMorto)

-- | Calcula a altura do 'Jogador' numa 'Peca' dada uma distância x.
--

calculaAlturaPosicao :: (Int,Int) -- ^ (altura inicial, altura final) da 'Peca'
                     -> Double -- ^ distância percorrida na 'Peca' (0 a 1)
                     -> Double -- ^ altura em que o 'Jogador' se encontra
calculaAlturaPosicao (ai, af) distancia = abs $ fromIntegral (ai) + ( (fromIntegral (af - ai)) * distancia)

-- * Funções para retornar propriedades de 'Peca'

-- | Verifica se uma 'Peca' é do tipo 'Recta'

pRecta :: Peca
 -> Bool
pRecta (Recta _ _) = True
pRecta _ = False

-- | Verifica se a 'Peca' é Boost
isBoost :: Peca
 -> Bool
isBoost (Recta Boost _) = True
isBoost (Rampa Boost _ _) = True
isBoost _ = False

-- | Calcula a altura inicial e final de uma peça

verificaAlturaPeca :: Peca -- ^ Recebe uma 'Peca'
                   -> (Int, Int) -- ^ Devolve um par com (alturaInicial, alturaFinal) da 'Peca'
verificaAlturaPeca (Recta x a) = (a,a) -- @1
verificaAlturaPeca (Rampa x a b) = (a,b) -- @2

-- | Calcula a altura da 'Peca' num determinado ponto x da mesma

altPecaX :: Double -> Peca -> Double
altPecaX x p = abs ((fromIntegral a1) + x * (fromIntegral (a2 - a1)))
 where (a1, a2) = verificaAlturaPeca p

-- | Calcula a inclinação de uma 'Peca'

inclinacaoPeca :: Peca
               -> Double -- inclinação da 'Peca' @ radianos
inclinacaoPeca x = let (i,f) = verificaAlturaPeca x
    in atan ( fromIntegral (f - i) )

-- | Verifica a diferença de inclinação entre duas peças
-- 
-- Caso a 'Peca' seguinte tenha inclinação superior à anterior, devolve True

difInclPecas :: Peca 
 -> Peca
 -> Bool -- ^ True se a segunda 'Peca' tiver inclinação superior que à primeira
difInclPecas a b = aI <= bI
                 where (a1, a2) = verificaAlturaPeca a
                       (b1, b2) = verificaAlturaPeca b
                       aI = atan (fromIntegral (a2 - a1))
                       bI = atan (fromIntegral (b2 - b1))

-- | Indica a 'Peça' onde o 'Jogador' se encontra 

pecaJog :: Jogador 
 -> Mapa 
 -> Peca        
pecaJog j m = m !! p !! i
            where (p,i) = posicaoMJog j

-- | Calcula a posição do 'Jogador' na Matriz 
-- 
-- | Fornece um par com a 'Pista' e o indice da 'Peça' do 'Jogador'

posicaoMJog :: Jogador 
 -> PosicaoMatriz -- ^ Posição do 'Jogador' ('Pista', indice da 'Peça')
posicaoMJog (Jogador p d v c e) = (p, floor d)

-- | Recebe uma 'Peca' e devolve o 'Piso' da mesma

verificaPiso :: Peca -- ^ 'Peca' para verificar o 'Piso'
 -> Piso -- ^ 'Piso' da 'Peca'
verificaPiso p = case p of
     Recta l _ -> l
     Rampa l _ _ -> l

-- | Retorna o valor do atrito de uma 'Peca'
-- 
-- ? Os valores do atrito encontram-se no módulo __'Globals'__

atritoP :: Peca -- ^ 'Peca' para avaliar o atrito
 -> Double -- ^ atrito da 'Peca'
atritoP p = atritoPiso piso
    where piso = verificaPiso p

-- | Dado um 'Piso', retorna o seu atrito
atritoPiso :: Piso -> Double
atritoPiso Terra = atritoTerra
atritoPiso Relva = atritoRelva
atritoPiso Lama = atritoLama
atritoPiso Boost = atritoBoost
atritoPiso Cola = atritoCola

-- * Funções para converter ângulos

-- | Normaliza o ângulo da mota tendo em conta o tamanho da Peca no eixo X e no eixo Y
-- 
-- Isto é, dada uma escala diferente da proporção 1:1, recalcula o ângulo que a mota fica no mapa.
normalizaAnguloM :: Float
 -> Float
normalizaAnguloM a = radToDeg (atan (yEsc / wR) )
    where dTr = degToRad a
          x = wR / (cos dTr)
          y = x * (sin dTr)
          yEsc = y * (realToFrac heigth) / wR
          wR = realToFrac width

-- | Converter graus em radianos
degToRad :: (Num a, Floating a) => a -- ^ ângulo em graus
 -> a -- ^ o mesmo ângulo convertido em radianos
degToRad d = ( d * pi ) / 180

-- | Converte radianos em graus

radToDeg :: (Num a, Floating a) => a -- ^ ângulo em radianos
         -> a -- ^ devolve ângulo em graus
radToDeg r = (r * 180) / pi

-- * Funções para devolver propriedades do 'Jogador'

-- | Verifica  se o jogador está a acelerar
--

verificaAccelJog :: Jogador -> Bool
verificaAccelJog j@(Jogador p d v c (Chao True)) = True
verificaAccelJog _ = False


-- * Funções gerais para trabalhar com listas

-- | Ordena uma dada lista

ordena :: Ord a => [a] -> [a]
ordena l = ordenaAux [] l

-- | Auxiliar para ordenar lista

ordenaAux :: Ord a => [a] -> [a] -> [a]
ordenaAux l1 [] = l1
ordenaAux l1 (h : t) = ordenaAux (insert h l1) t

-- | Função Span alterada
-- Recebe uma função que é aplicada ao 2ª elemento do terno.
--
-- @ Devolve um par com os elementos True do lado esquerdo e False do lado direito

spanFull :: (b -> Bool) -- ^ Função a ser aplicada ao longo da lista no 2º elemento do quatruplo
 -> [(a, b, c,d)] -- ^ Lista de ternos
 -> ([(a,b,c,d)] , [(a,b,c,d)]) -- ^ Par com lista de ternos True do lado esquerdo e False do lado direito
spanFull f [] = ([], [])
spanFull f (h : t) | f (sndT h) = ( h : x , y)
                   | otherwise = (x , h : y)
                   where (x, y) = spanFull f t

-- * Funções Auxiliares Gerais

-- | Dado um terno, devolve o primeiro elemento
fstT :: (a,b,c,d) -> a
fstT (x,_ ,_,_) = x

-- | Dado um terno, devolve o segundo elemento
sndT :: (a,b,c,d) -> b
sndT (_,x,_,_) = x

-- | Dado um terno, devolve o terceiro elemento
thdT :: (a,b,c,d) -> c
thdT (_,_,x,_) = x

-- | Dado um terno, devolve o terceiro elemento
fthT :: (a,b,c,d) -> d
fthT (_,_,_,x) = x

-- * Funções do tipo random

-- | dado um intervalo, devolve um valor random entre ele
countdown :: Random a => (a, a) -> IO a
countdown (a,b) = randomRIO (a, b)

-- * Funções especiais para o jogo

-- | Criação de n 'Jogadores' na posição 0, velocidade 0, Chao False (sem acelerar), e 1 munição

jogadoresN :: Int -- ^ número de jogadores
 -> [Jogador] -- ^ lista de jogadores
jogadoresN n = [ Jogador n 0 0 1 (Chao False) | n <- [0 .. (n-1)] ]