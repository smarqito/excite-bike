-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
--
-- = Relatório Tarefa 6
-- Por forma a tornar o jogo mais interativo, foi necessário a criação de um bot para jogar contra o utilizador.
--
-- Para que o bot decida qual a melhor jogada, recebe a informação sobre o 'Estado' do jogo e, através de um indentificador, tem-se conhecimento sobre qual o 'Jogador'
-- que é o bot.
--
-- Após analisar o 'Estado' do jogo, a função /bot/ retorna uma /Maybe Jogada/.
--
-- == Metodologia
-- O 'EstadoJogador' pode variar entre, essencialmente, três estados. O 'Jogador' pode estar no 'Ar', no 'Chao' ou 'Morto'.
-- 
-- Mediante o 'EstadoJogador', dividiu-se em três funções distintas que serão abordadas posteriormente.
--
-- Para a realização desta tarefa, não houve qualquer limitação ou restrição por parte dos docentes. Assim, começou-se por realizar uma lista de tarefas a fazer e,
-- procurou-se subdividir a realização desta tarefa em várias pequenas tarefas.
--
-- === Controlar o Jogador quando está 'Morto'
-- Nesta situação apenas se devolve o resultado /Just Acelera/.
--
-- === Controlar o Jogador quando está no ar
-- Quando o jogador está no ar, procurou-se maximar a trajetória. Para tal, definiu-se que o 'Jogador' deve ter uma inclinação entre 5 e 25 graus
-- por forma a obter o melhor vetor deslocação.
--
-- @controlaAr :: Jogador -> [Jogador] -> Mapa -> Maybe Jogada @
--
-- Para garantir que o 'Jogador' não morre, aplicou-se a função __passo__ (definida na /Tarefa 4/) duas vezes.
-- Desta forma, consegue-se antecipar, se o jogador morre ou não no espaço de dois tempos.
--
-- Caso o 'Jogador' morra, então a decisão de 'Jogada' é ajustar-se à 'Peca' que se aproxima (/Movimenta E/ ou /Movimenda D/).
-- 
-- Caso contrário, então ajusta a sua inclinação para caber na amplitude entre 5 e 25º.
-- 
-- === Controlar o jogador quando está no 'Chao'
-- Esta função decide qual a melhor 'Jogada' quando o jogador se encontra no chão.
-- Para tal, começou-se por obter a transposta do 'Mapa' e dividi-la em 2 partes na 'Peca' em que o 'Jogador' se encontra.
--
-- Após tal divisão, utilizou-se a coluna em que o jogador se encontra e as três seguintes para calcular qual a melhor 'Pista' que o 'Jogador' deve seguir.
-- Para efetuar esse cálculo, utilizou-se a função:
--
-- @ bestLap :: Mapa -> Int @
-- 
-- Que utiliza a distância que o 'Jogador' terá de percorrer em cada pista para retornar aquela que tem a menor distância. A distância de cada pista foi
-- calculada através da função:
--
-- @ distancPeca :: Peca -> Double @
--
-- aplicada a cada elemento da 'Pista'.
--
-- Por último, a função:
-- 
-- @ controlaChao :: Int -> Jogador -> [Jogador] -> Mapa -> Maybe Jogada @
--
-- agrega a utilização das funções abordadas anteriormente e, efetua o movimento de passar para a 'Pista' superior ou inferior.
--
-- Antes de utilizar de as utilizar, verifica se a 'Peca' anterior tem como 'Piso' 'Boost'. Caso seja e ainda haja jogadores atrás,
-- dispara uma cola. Esta jogada pretende "estragar" aquela que seria a melhor 'Pista'.
--
-- Posteriormente, calcula a altura da 'Peca' que se encontra na melhor pista (/menor distância/) e verifica se a diferença entre a atual e a seguinte é
-- superior a 0,2. Caso seja, então o 'Jogador' não se desloca para essa 'Pista'.
--
-- == Função principal
-- A principal função desta tarefa é a __/bot/__.
-- 
-- @ bot :: Int -> Estado -> Maybe Jogada @
--
-- Esta função utiliza o identificador do e a lista de 'Jogador's para verificar o 'EstadoJogador' do 'Jogador' que irá efetuar a 'Jogada.
--
-- Posteriormente, utilizou-se um case of para encaminhar para a função adequada: /controlaAr/ ou /controlaChao/.



module Tarefa6_2019li1g177 where

import LI11920
import F_Aux
import Data
import Globals
import Tarefa0_2019li1g177
import Tarefa4_2019li1g177
import Tarefa2_2019li1g177
import Data.List
import Data.Maybe

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n e@(Estado m j) = case estado of
    Ar _ _ _ -> controlaAr p (bp ++ pp) m
    Chao _ -> controlaChao n p (bp ++ pp) m
    _ -> Just Acelera
    where (bp, p : pp) = splitAt n j
          estado = estadoJ p

-- * Controla o jogador no Ar

-- | Controla o 'Jogador' quando está no 'Ar'

controlaAr :: Jogador 
 -> [Jogador]
 -> Mapa
 -> Maybe Jogada
controlaAr j@(Jogador p d v c e) lj m | morre && (incJoga < incPeca) = Just $ Movimenta E
                                      | morre && (incJoga > incPeca) = Just $ Movimenta D
                                      | not morre && (incJoga <= 25 && incJoga >= 5) = Nothing
                                      | not morre && (incJoga > 25) = Just $ Movimenta D
                                      | not morre && (incJoga < 5) = Just $ Movimenta E
                                    --   | otherwise = Just (Movimenta D)
    where peca = m !! p !! (floor d)
          incPeca = radToDeg $ inclinacaoPeca peca
          incJoga = inclinaJ j peca
          morre = isMorto $ seMorre (2*(1/(fromIntegral fr))) m j -- prevê se o jogador vai morrer no prazo de 2 jogadas. Permite que se adeque à 'Peca' que vai encontrar

-- * Controla o jogador no Chão

-- | Controla o 'Jogador' quando este se encontra no 'Chao'
controlaChao :: Int
 -> Jogador
 -> [Jogador]
 -> Mapa
 -> Maybe Jogada
controlaChao n j@(Jogador _ _ _ _ e@(Chao False)) lj m = Just Acelera
controlaChao n j@(Jogador p d v c e) lj m =
    if (isBoost lastPeca) && c > 0 && playerAtras then Just Dispara 
    else if (p < bestIF) && (0.2 > (ap1 - ap0)) then Just (Movimenta B)
    else if (bestIF < p) && (0.2 > (ap2 - ap0)) then Just (Movimenta C)
    else Nothing
    where tm = transpose m -- transposta do mapa
          (bm, pa : pm) = splitAt (floor d) tm -- faz split da transposta na posição do jogador @ lista da coluna do 'Jogador'
          lastPeca = if (length bm) > 0 then (last bm) !! p else (Recta Cola 0)
          pmT = transpose (take 2 (pa:pm))
          lc = (length $ tm !! 0) - 1
          op1 = min ( p + 1 ) lc
          op2 = max ( p - 1 ) 0
          playerAtras = hasPlayerLap j lj
          op0A = encontraIndiceLista p pa
          op1A = encontraIndiceLista op1 pa
          op2A = encontraIndiceLista op2 pa
          ap0 = altPecaX (d - (fromIntegral (floor d))) op0A
          ap1 = altPecaX (d - (fromIntegral (floor d))) op1A
          ap2 = altPecaX (d - (fromIntegral (floor d))) op2A
          bestIF = bestLap pmT -- if isJust best then fromJust best else p

-- * Funções Auxiliares

-- | Verifica se tem jogadores com distância inferior à do próprio
hasPlayerLap :: Jogador
 -> [Jogador]
 -> Bool
hasPlayerLap j l = j > foldr (\h t -> if h < t then h else t) j l

-- | devolve a melhor pista considerando o 'Mapa' dado como input
bestLap :: Mapa
 -> Int
bestLap m = fromJust best
    where distancia = distancMapa m
          distMin = minimum distancia
          best = elemIndex distMin distancia

-- | Dado um mapa, calcula o valor de cada pista em termos de distância
distancMapa :: Mapa
 -> [Double]
distancMapa = map (foldr (\h t -> distancPeca h + t) 0)

-- | Calcula o correspondente de distância tendo em consideração o atrito da 'Peca' e a sua inclinação
distancPeca :: Peca
 -> Double
distancPeca p = dist
    where atritoPe = atritoP p
          iPeca = inclinacaoPeca p
          atritExtra = if isBoost p then 0.7 else 1
          dist = atritExtra * atritoPe * ( 1 / (cos iPeca) )


-- | Verifica se o jogador morre sendo-lhe aplicado 2 vezes a função passo (definida na tarefa 4)
seMorre :: Double
 -> Mapa
 -> Jogador
 -> Jogador
seMorre t m j | t > 0 = seMorre (t - (1/(fromIntegral fr))) m (passo t m j)
              | otherwise = j