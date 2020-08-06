-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
--
-- = Relatório da Tarefa 4 
--
-- __/Objetivo da Tarefa/__
--
-- Na Tarefa 4 foi desenvolvida a reação do 'Jogador' à passagem de um período de tempo, sendo alterado o
-- seu 'Estado'. Neste sentido, é calculado o efeito da passagem do tempo no 'Estado' dum Jogo . 
--
-- __Função Objetivo:__ /(passo)/
--
-- A função responsável por efetuar a mudança da posição do 'Jogador' é a /passo/. 
-- Deste modo, a Função Principal da Tarefa 4 :   
--
-- @ passo :: Double -> 'Mapa' -> 'Jogador' -> 'Jogador' @
--
-- tem o intuito de avançar o 'Jogador' um "passo" em frente, durante um determinado instante de tempo, com
-- isto, o seu 'Estado' é atualizado. 
-- Recebe como /input/ o tempo decorrido (Double), o 'Mapa' utilizado e o 'EstadoJogador' anterior, sendo o /output/ o EstadoJogador' 
--atualizado. 
--
-- == Condições pré-definidas pelos docentes 
--
-- Os /input/s terão de ser um número real positivo que denota o tempo que passou desde a última atualização, o 'Mapa' do Jogo, que permanece imutável
--durante o processo e o 'Jogador' para o qual vai ser processado o passar do tempo.
--
-- O resultado deve ser o 'Jogador' atualizado.
--
-- Para atingir este objetvo foi necessário desenvolver alguns complementos: 
--
--
-- == Funções Principais da Tarefa 4 /(acelera),(move)/
--
-- Para construir a função /passo/ foram criadas duas funções : 
--
--        - /acelera/, utilizada com o intuito de definir uma nova velocidade; 
--
--        - /move/, que utiliza esta velocidade e altera a posição do 'Jogador'. 
--
-- Assim, temos :
--
-- == Função /acelera/ 
--
-- Através desta função :
-- 
-- @ acelera :: Double -> 'Mapa' -> 'Jogador' -> 'Jogador' @
-- 
-- É alterada a velocidade que de um 'Jogador', num determinado instante.
-- O seu /input/ é o intervalo de tempo decorrido (Double), o 'Mapa' onde encontra-se o 'Jogador', bem como o seu 'EstadoJogador' anterior.
--
-- Dados estes argumentos, o /input/ será o 'Jogador', já com a velocidade atualizada. 
--
-- == Método  
--
-- De modo a obter a velocidade são utilizadas três funções para calcular/atualizar a velocidade do 'Jogador'.
-- 
-- Deste modo, estas são utilizadas para quando o 'Jogador' :
-- 
-- 1. Encontra-se no Chão: __/calculaVChao/__
--
-- 2. Encontra-se no Ar: __/calculaVAr/__, bemo como __/variacaoGrav/__.        
--
-- Para além destas duas situações, Ar e Chão, é, também, definido para caso o 'Jogador' encontre-se 'Morto'.
--
-- == Calcular a velocidade no Chão
--
-- @ calculaVChao :: 'Jogador' -> 'Mapa' -> Double -> Double -> Double @
--
-- A velocidade é obtida através da seguinte fórmula (fornecida pelos docentes), tendo em consideração o 'Piso' e a aceleração:
--
-- @ v' = v + (/accelMota/ - /atritoP/ * v ) * t@
--
-- com v' = velocidade atualizada e v = velocidade anterior, atritoP = Atrito da 'Peca' e accelMota = variável do campo aceleraJogador do estado Chao. 
--
-- == Cálculo do Atrito
-- 
-- Dependendo do tipo de 'Piso', o seu atrito correspondente vai ser diferente, afetando a velocidade do 'Jogador'.
-- Por isso, foi definida uma função que associa os diversos valores de atrito aos diferentes 'Piso's existentes, /atritoP/:
--
-- @ atritoP :: 'Peca' -> Double @
--
-- atribuindo-lhes um valor específico (Double), que está localizada no módulo /F_Aux.hs/.
--
-- Nota : Os valores de correspondentes a cada 'Piso' estão no módulo /Globals.hs/.
-- 
-- == Parâmetro /accelMota/
--
-- O parâmetro /accelMota/ está na forma de uma condição (fornecida pelos docentes), tal que:
-- Se a velocidade atual for menor que 2, e o 'Jogador' estiver a acelerar, então será atribuído o valor 1 à variável, caso contrário 0.
-- 
-- É comprovado que o 'Jogador' está a acelerar pela função /verificaAccelJog/, present no módulo F_Aux.hs
--
--
--Por fim, como a velocidade não pode ser negativa, foi utilizada a função /max/, para, caso o cálculo resultar num valor menor que 0, ser 
--escolhido 0, sendo essa a nova velocidade do 'Jogador'.
--
-- == Calcular a velocidade no Ar
--
-- @ calculaVAr :: Double -> Double -> Double @
--
-- A fórmula correspondente ao cálculo da velocidade (fornecida pelos docentes) expressa na função auxiliar calculaVAr é : 
-- 
-- @ v' = v - (resistAr * v * t) @
--
-- em que t = instante de tempo e resistenciaAr = Resistência do Ar, predefinida, no módulo /Globals.hs/, como um Double.
--
-- De modo semelhante ao 1º caso, relativo ao cálculo no Chão, a velocidade no 'Ar' não pode ser negativa, sendo novamnente 
--utilizada a função /max/, sendo escolhido 0, na chance de o resultado ser negativo.
--
-- Ainda relativo à velocidade no 'Ar',é necessário atualizar, também, a velocidade provocada pela gravidade.
--
-- == Calcular a velocidade da Gravidade 
--
-- @ variacaoGrav :: Double -> Double -> Double @
--
-- Sendo a equação definida pelos docentes a seguinte :
--
-- @ g' = g + accelGravidade * t @ 
--
--na qual g' = gravidade atualizada e g = gravidade anterior, sendo utlizado o valor /accelGravidade/ previamente definido 
--como um Double, no módulo /Globals.hs/.
--
-- == QUando o 'Jogador' está 'Morto'
--
-- Com estes dois casos definidos só falta para quando o 'jogador encontra-se 'Morto', sendo que nesta situação particular, o /input/ será o 
--'Jogador' atual, sem ser atualizado.
--
-- Esta função vai estar integrada na função principal, /gera/, para fornecer o último argumento da função auxiliar principal /move/.
--
--
-- __Função move__
--
-- Para além de alterar a velocidade do 'Jogador', é necesário mudar a posição dele, sendo este processo feito dentro  
--de um intervalo de tempo definido, pela função /move/.
--
-- @ move :: Double-> 'Mapa' -> 'Jogador' -> 'Jogador' @
--
-- == Método
--
-- Esta função é divida em três casos, sendo utilizado o "case of" para determinar esta alteração para diferentes ocasiões do Jogo :
--
-- - Quando o 'Jogador' está no Ar; 
-- - No Chão;
-- - E para quando este se encontra 'Morto'.
--
-- Neste sentido, para o primeiro caso, temos a função movAr :
--
-- == Calcular o movimento no Ar
--
-- @ movAr :: Double -> 'Jogador' -> Peca -> 'Mapa' -> 'Jogador' @
--
-- Para este caso foi necessário ter cuidado em alguns aspetos, nomeadamente, indicar onde o 'Jogador' vai estar quando atingir o Chão.
-- Para tal foram formadas trẽs funções auxiliares, desenvolvidas para :
--  
--  1 - Movimentar o 'Jogador' quando este não interseta ou ultrapassa a 'Peca'; 
--  2 - Movimentar o 'Jogador' quando este interseta a 'Peca';
--  3 - Movimentar o 'Jogador' quando este interseta o limite da 'Peca'.
--
-- 1 - Temos, então, a função :
--
-- @ movArLinear :: 'Jogador' -> Ponto -> Double -> 'Jogador'  @
--
-- Nesta função, verifica se a interseção da reta definida com a 'Peca' ocorre, sendo modificada a distância que o 'Jogador' se encontra na 'Peca' anterior, bem como a sua Gravidade.
--
-- 2 - A seguir vem :
--
-- @ moveArImpacto :: 'Jogador' -> 'Peca' -> Ponto -> Double -> 'Jogador' @
-- 
-- sendo que, nesta, o 'Jogador' interseta a 'Peca'.
-- Neste sentido, a reta definida interseta o 'Peca', colocando o 'Jogador' nessa 'Peca'.
--
-- 3 - Por fim, temos :
-- 
-- @ moveArExtra :: 'Jogador' -> Ponto -> Double -> 'Jogador' @
-- 
-- Nesta função, utilizada para quando o 'Jogador' interseta o limite da 'Peca', onde é somada na distância o índice onde este encontra-se antes, com a distância percorrida no eixo dos x.
-- É, também, atualizada a Gravidade do 'Jogador'.
--  
-- A segunda parte da função move está dedicada à função /movChao/ : 
--
-- == Calcular o movimento no Chão
--
-- @ movChao :: Double -> Double -> 'Jogador' -> 'Jogador' @
--
-- Relativamente a este movimento, como após o período de tempo ter passado, o 'Jogador' só pode, no máximo, estar no início da próxima 'Peca', foram definidas algumas condições: 
--
-- - Se a inclinação da 'Peca' seguinte for superior à do 'Jogador, o 'Jogador' permance no Chão, com a mesma velocidade.
--
-- - Caso tal não aconteça (Inclinação da próxima 'Peca' <= Inclinação atual), a velocidade mantêm-se, mas o 'EstadoJogador' passa para 'Ar', com a inclinação e altura da 'Peca'.
--
-- Neste sentido, a função tem como /input/ o tempo decorrido (Double), o 'Mapa' do Jogo, bem como o 'Jogador', sendo o /output/ a posição deste atualizada.
--
-- == Método 
--
-- Para aplicar os critérios definidos para desenvolver este movimento, foi calculada a posição final do 'Jogador'.
-- Para tal, usou-se a função /movChaoPosFinal/, que calcula a posição do 'Jogador', caso a 'Peca' seja uma 'Rampa' ou 'Recta'.
-- Isto vai devolver um número entre 0 e 1.0 (máximo da 'Peca'), usado na função /movChao/, devolvendo a posição atualizada.    
--
-- == Calcular a posição final do 'Jogador' 
--
-- @ movChaoPosFinal :: Double -> 'Mapa' -> 'Jogador' -> 'Double'
--
-- Esta função vai escolher entre duas funções auxiliares, para o caso de ser 'Recta' ou 'Rampa', que vão fornecer a posição final do 'Jogador' na 'Peca', após ter decorrido o /input/.
--
-- == Calcular a posição final numa 'Recta'
--
-- @distanciaPercRecta :: Double -> Double -> Double -> Double @
--
-- Nesta funçâo, cujo valor máximo é 1, é definida como o minímo entre o tamanho da 'Peca' e a distância percorrida pelo 'Jogador'.  
-- Para seguir os critérios em cima expostos, o 'Jogador', caso a distância percorrida ultrapasse a 'Peca' onde se encontra, tem de permanecer no início da pŕoxima 
--'Peca'. Por isso, é utilizada a função /min/, na qual vai escolher colocar o 'Jogador' no início da próxima 'Peca', caso a sua distância ultrapasse o valor estabelecido.
--
-- == Calcular a posição final do 'Jogador' numa 'Rampa'
-- 
-- @ distanciaPercRampa :: Int -> Double -> Double -> Double -> Double @
--
-- Semelhante à função anterior utiliza-se a função /min/ para escolher entre o tamanho da 'Peca' e a distância calculada com a velocidade e o instante dados. 
-- Deste modo, se ultrapassar a 'Peca' seguinte, o 'Jogador' é colocado no início dessa 'Peca'.
--

module Tarefa4_2019li1g177 where

import LI11920
import Tarefa0_2019li1g177
import Globals
import F_Aux
import Data.Maybe


-- * Testes

-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(4, [[Recta Terra 0, Rampa Terra 0 8,Recta Terra 8, Rampa Relva 8 0, Recta Boost 0, Recta Cola 0, Recta Terra 0]], c) | c <- jogadores]

-- | Jogadores para os testes
jogadores :: [Jogador]
jogadores = [Jogador 0 0 0 0 (Chao False), Jogador 0 0 0.5 1 (Chao True), Jogador 0 4 3 1 (Chao True), Jogador 0 1.1 0.5 1 (Chao True), Jogador 0 0 3 1 (Chao True), Jogador 0 0.95 5 1 (Chao True), Jogador 0 1.7 15 1 (Chao True), Jogador 0 2.8 10 1 (Chao True), Jogador 0 0 3 1 (Ar 15 0 2), Jogador 0 0 3 1 (Ar 15 45 2), Jogador 0 0 3 1 (Ar 15 80 2), Jogador 0 0.9 7 1 (Ar 15 0 2), Jogador 0 0.9 7 1 (Ar 15 45 2), Jogador 0 1.5 4 1 (Ar 7 15 5), Jogador 0 2.5 4 1 (Ar 5 0 4), Jogador 0 0 4 1 (Ar 1 0 4), Jogador 0 4.1 4 1 (Chao True), Jogador 0 3.1 4 1 (Chao True), Jogador 0 3.1 0 1 (Morto 1.0), Jogador 0 3.1 0 1 (Morto 0.03)]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j = case e of
     Chao _ -> Jogador p d (calculaVChao j m v t) c e
     Ar _ _ _ -> Jogador p d (calculaVAr v t) c (Ar h i (variacaoGrav g t))
      where Ar h i g = e
     Morto _ -> j
     where Jogador p d v c e = j

-- | Atualiza a velocidade do 'Jogador'
--
-- >> o 'Jogador tem de se encontrar no 'Chao'

calculaVChao :: Jogador -- ^ 'Jogador' cuja velocidade vai ser calculada 
 -> Mapa -- ^ 'Mapa' onde o 'Jogador' se encontra 
 -> Double -- ^ velocidade anterior 
 -> Double -- ^ período de tempo 
 -> Double -- ^ velocidade atualizada 
calculaVChao j m v t = max (v + (accelMota - (atritoP (pecaJog j m)) * v) * t) 0
                     where accelMota = if (v < 2 && verificaAccelJog j) then 1 else 0 

-- | Atualiza a velocidade do 'Jogador'
-- 
-- >> O 'Jogador' tem de se encontar no 'Ar'

calculaVAr :: Double -- ^ Velocidade Inicial do 'Jogador'
 -> Double -- ^ Instante de tempo em questão
 -> Double -- ^ Nova velocidade do 'Jogador'
calculaVAr v t =  max (v - (resistAr * v * t)) 0

-- | Calcula a velocidade causada pela Gravidade do 'Jogador'

variacaoGrav :: Double -- ^ Gravidade do 'Jogador' 
 -> Double -- ^ Instante de tempo em questão
 -> Double -- ^ Nova velocidade causada pela gravidade do 'Jogador'
variacaoGrav g t = g + (acelGravidade * t)

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j = case estadoJ j of
     Ar h i g -> movAr t j (pecaJog j m) m
     Chao _ -> movChao t m j
     Morto _ -> movMorto t j


-- * Funções para mover o Jogador quando está no Chao

-- | Movimenta o Jogador quando o Estado é Chao
--  Após o tempo decorrido, no máximo o 'Jogador' pode estar no inicio da 'Peca' seguinte
-- 
--  Caso isso aconteça, se a inclinação da 'Peca' seguinte for superior à atual, o 'Jogador' mantém-se no 'Chao' e mantém a velocidade.
--  Caso contrário, mantém a velocidade mas o 'EstadoJogador' passa para 'Ar' com a inclinação e altura da 'Peca' anterior.

movChao :: Double -- ^ tempo decorrido
 -> Mapa -- ^ 'Mapa' do jogo
 -> Jogador -- ^ 'Jogador' a ser atualizado
 -> Jogador -- ^ 'Jogador' atualizado

movChao t m j = if pFinal < 1 then Jogador p (pJ + pFinal) v c e
                else if difInclPecas pAtual pNext
                     then Jogador p (pJ + pFinal) v c e
                     else Jogador p (pJ + pFinal) v c (Ar (fromIntegral (snd (verificaAlturaPeca pAtual)) + 0.001) (radToDeg (inclinacaoPeca pAtual)) 0)
     where pFinal = movChaoPosFinal t m j
           pJ = fromIntegral (floor dX)
           dX = distJ j
           pFinalOK = atribuiPosicao (pJ + pFinal) m
           Jogador p d v c e = j
           pAtual = m !! p !! (floor d)
           pNext = m !! p !! lMapa
           lMapa = min (ceiling d) ((length (m !! 0) ) - 1)


-- | Calcula a posição final do 'Jogador' na 'Peca' decorrido o input do tempo
--  
-- ! Output máximo desta função é 1.0 (fim da 'Peca')

movChaoPosFinal :: Double -- ^ tempo
 -> Mapa -- ^ mapa do jogo
 -> Jogador -- ^ jogador a efetuar a 'Jogada'
 -> Double

movChaoPosFinal t m j = case pm of
     Recta _ _ -> (distanciaPercRecta t dPeca v)
     Rampa _ ai af -> distanciaPercRampa (af - ai) t dPeca v
     where pm = m !! p !! pJ
           pJ = floor dX
           dX = distJ j
           p = pistaJ j
           dPeca = dX - ( fromIntegral pJ)
           v = velocJ j

-- | Calcula a posição final do 'Jogador' na 'Recta' após um determinado tempo
-- 
-- ! Output máximo: 1.0

distanciaPercRecta :: Double -- ^ tempo percorrido
 -> Double -- ^ posição x na 'Peca'
 -> Double -- ^ velocidade do 'Jogador'
 -> Double -- ^ posição final do 'Jogador' na 'Peca'
distanciaPercRecta t d v = min tamanhoP (d + (t * v))

-- | Calcula a posição final do 'Jogador' na 'Rampa' após um determinado tempo
-- 
-- ! Output máximo: 1.0

distanciaPercRampa :: Int -- ^ altura da 'Rampa'
 -> Double -- ^ tempo
 -> Double -- ^ distancia percorrida na 'Peca'
 -> Double -- ^ velocidade do 'Jogador'
 -> Double -- ^ posição final na 'Peca'
distanciaPercRampa h t dx v = min tamanhoP dxJ
     where dR = dx / (cos incP)
           incP = atan (fromIntegral h )
           distPercRampa = dR + (v * t)
           dxJ = distPercRampa * (cos incP)

-- * Funções para mover o Jogador quando está no Ar.

-- | Move o 'Jogador' quando o 'EstadoJogador' é Ar

movAr :: Double -- ^ tempo decorrido
 -> Jogador -- ^ 'Jogador' a efetuar a jogada (com a valocidade e gravidade atualizada)
 -> Peca -- ^ 'Peca' em que o 'Jogador' se encontra
 -> Mapa
 -> Jogador
movAr t j@(Jogador p d v c e) pp m = case op of
     1 -> moveArLinear j posFinal distMax
     2 -> moveArImpacto j pp posFinal distMax
     3 -> moveArExtra j posFinal distMax
     where op = if xf < 1 && not haIntersecao then 1
                else if haIntersecao then 2
                else 3
           posFinal = posicaoFinal t j
           xf = posx posFinal
           (ai, af) = verificaAlturaPeca pp
           pi = (Cartesiano (d - ( fromIntegral $ floor d) ) (realToFrac $ fst $ alturaJ j m), posFinal)
           haIntersecao = intersetam (Cartesiano 0 (fromIntegral ai), Cartesiano 1 (fromIntegral af)) pi
           distMax = fromIntegral $ (length (m !! 0) ) - 1

-- | Movimentação do 'Jogador' quando não interseta nem ultrapassa a 'Peca'
moveArLinear :: Jogador
 -> Ponto
 -> Double
 -> Jogador
moveArLinear j@(Jogador p d v c e@(Ar h i g)) pf maxDist = 
     Jogador p  (( fromIntegral (floor d)) + (posx pf) ) v c (Ar (posy pf) i g)
     where dist = min ( (fromIntegral (floor d)) + (posx pf) ) maxDist

-- | Movimentação do 'Jogador' quando interseta a 'Peca'
moveArImpacto :: Jogador
 -> Peca
 -> Ponto
 -> Double
 -> Jogador
moveArImpacto j@(Jogador p d v c e@(Ar h _ _)) pp pf maxDist = 
     if (posx pfMapa) <= 1 then Jogador p (fd + (posx pfMapa)) vi c eu
     else moveArExtra j pf maxDist
     where pfMapa = fromJust $ intersetaPeca j (verificaAlturaPeca pp) pi pf
           fd = (fromIntegral (floor d))
           impactoMorre = impactoChao (inclinaJ j pp) pp
           (vi, eu) = if impactoMorre then (0, Morto timeOutMorto) else (v, Chao False)
           pi = Cartesiano (d - fd) h
           -- dist = min (fd + (posx pfMapa)) maxDist

-- | Movimentação do 'Jogador' quando interseta o limite da 'Peca'
moveArExtra :: Jogador
 -> Ponto -- ^ posição final do 'Jogador'
 -> Double
 -> Jogador
moveArExtra j@(Jogador p d v c e@(Ar h i g)) pf maxDist = 
     Jogador p (fd + (posx cInter)) v c (Ar (posy cInter) i g)
     where fd = fromIntegral $ floor d -- Indice da 'Posicao' do 'Jogador'
           x0 = d - fd -- posição 'Jogador' @ [0,1]
           pi = Cartesiano x0 h
           rVert = (Cartesiano 1 0, Cartesiano 1 1)
           cInter = intersecao (pi, pf) rVert
           -- dist = min (fd + (posx cInter)) maxDist


-- | Situação em que o 'Jogador' interseta a 'Peca'

impactoChao :: Double -- ^ inclinação do 'Jogador' @ graus
 -> Peca -- ^ 'Peca' @ em que o 'Jogador' se encontra
 -> Bool -- ^ True se a inclinação de impacto for inferior a 45º
impactoChao i pp = not $ (i <= (incPeca + difMaxImp)) && (i >= (incPeca - difMaxImp)) -- True morre  ao bater no 'Chao'
     where difMaxImp = 45.0 -- máximo de diferença entre a inclinação do 'Jogador' e a 'Peca' quando chega ao Chao
           incPeca = radToDeg (inclinacaoPeca pp) -- inclinação da 'Peca' em que o 'Jogador' se encontra

-- | Calcula a posição final do 'Jogador'

posicaoFinal :: Double -- ^ tempo decorrido
 -> Jogador
 -> Ponto -- ^ Posição final do 'Jogador'
posicaoFinal t j@(Jogador p d v c e@(Ar h i g)) = somaVetores (Cartesiano x0 h) vW
 where fd = fromIntegral $ floor d -- Indice da 'Posicao' do 'Jogador'
       x0 = d - fd -- posição 'Jogador' @ [0,1]
       iToRad = degToRad i -- inclinação de graus para radianos
       modG = g * t -- norma do vetor gravidade
       modV = v * t -- norma do vetor velocidade
       xVetorV = (cos iToRad) * modV -- distância percorrida no eixo do X @ vetor velocidade
       yVetorV = modV * (sin iToRad)
       vG = Cartesiano 0 (negate modG) -- vetor "gravidade"(u)
       vV = Cartesiano xVetorV  yVetorV -- vetor "velocidade" (v)
       vW = somaVetores vG vV -- vetor descolação da mota

-- | Calcula o ponto de interseção entre o 'Jogador' e a 'Peca'
intersetaPeca :: Jogador -- ^ 'Jogador' a efetuar a jogada
 -> (Int, Int) -- ^ par com altura inicial e final da 'Peca'
 -> Ponto -- ^ posição inicial da mota
 -> Ponto -- ^ posição final da mota
 -> Maybe Ponto -- ^ se houver ponto, devolve as suas coordenadas. Caso contrário: Nothing.
intersetaPeca j@(Jogador p d v c e) (ai, af) pi pf = Just iP 
     where retaPeca = (Cartesiano 0 (fromIntegral ai), Cartesiano 1 (fromIntegral af)) -- reta da 'Peca'
           iP = intersecao retaPeca (pf, pi) -- ponto de interseção (caso exista)

-- * Funções para mover o Jogador quando está Morto

-- | Movimenta o Jogador quando está Morto

movMorto :: Double -- ^ O tempo decorrido
 -> Jogador -- ^ O jogador que efetua a jogada e o seu estado é morto
 -> Jogador -- ^ O jogador atualizado com a jogada
movMorto t j | tF == 0 = Jogador p d v c (Chao False)
             | otherwise = Jogador p d v c (Morto tF)
     where Jogador p d v c (Morto t1) = j
           tF = max 0 (t1 - t)

-- * Funções Auxiliares



-- | Caso o jogador ultrapasse o mapa, coloca-o na peça final .999(9)
atribuiPosicao :: Double -> Mapa -> Double
atribuiPosicao n m | ePosicaoMatrizValida (0, floor n) m = n
                   | otherwise = fromIntegral (floor n) - 1e-10
     