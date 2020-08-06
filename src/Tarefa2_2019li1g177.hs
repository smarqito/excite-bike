-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
--
-- | Relatório Tarefa 2 
--
-- __/Objetivo da Tarefa/__
--
-- Esta tarefa __visiona__ efetuar uma 'Jogada', utilizando o nº do 'Jogador', a 'Jogada' que irá concretizar e o 'Estado' anterior (antes de ser atualizado).
-- Neste sentido, pretende-se determinar o efeito que a 'Jogada' de um 'Jogador' terá no 'Estado' do Jogo.
--
-- __Função Objetivo__ __/(jogada)__
--
-- A função principal, jogada :
--
-- @ jogada :: Int -> 'Jogada' -> 'Estado' -> 'Estado' @
--
--atualiza o 'Estado' do Jogo, de acordo com as 'Jogada's possiveis que um 'Jogador' pode efetuar - Movimenta C (para cima), Movimenta B (para baixo), 
--Movimenta E (para a esquerda),Movimenta D (para a direita), Acelerar/Desacelerar e, por fim, Disparar 'Cola'. 
--  
-- __/Específicos da Tarefa 2/__
--
-- /Funções Auxiliares - Principais/
--
-- __Função mudaPista__
--
-- Para mudar de 'Pista', o 'Jogador' tem de movimentar-se para cima (ou para baixo), sendo utilizada a função mudaPista para efetuar essa 'Jogada'.
--
-- Assim, temos a função :
--
-- @ mudaPista :: Int -> 'Mapa' -> 'Jogada' -> ['Jogador'] -> ['Jogador'] @
--
-- 1. Para que esta 'Jogada' ocorra é necessário o 'Jogador' encontrar-se no Chão ; 
-- 2. A 'Pista' seguinte (Pista de Baixo/Cima) tem de existir ;
-- 3. É, também, necessário que a diferença de alturas entre a 'Pista' atual e a seguinte seja menor que 0.2, na coordenada x ;
--
-- Caso contrário, nos primeiros dois pontos, a função devolve o 'Estado' sem alterações, enquanto que no terceiro ponto, o 'Jogador' 
--morre.
--
-- - Para verificar se o 'Jogador' encontra-se no chão foi utilizada a função posicaoChao :
-- 
-- @ posicaoChao :: 'Jogador' -> Bool @
--
-- Sendo devolvido "True" caso o 'Jogador' estivesse no chão.
--
-- - No que toca a verificar se a 'Pista' seguinte existe, recorre-se á função :
--
-- @  ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool @
--
-- Sendo devolvido, tal como na função anterior, "True" se existir a 'Pista' para a qual o 'Jogador' quer ir.
--
-- È, também, defindo que, se a 'Jogada' for Movimento B (para baixo), o índice da 'Pista' vai aumentar por 1 ('Pista' + 1) e, caso seja Movimento C (para cima),
--o índice diminua por 1 ('Pista' - 1).
--
-- - Por fim, é necessário ter em conta a Altura da nova 'Peca', sendo aplicada a condição : 
--
-- @ difAlturaValida = abs (difAltura) <= difAlturaMaxima @
--
-- Que indica que a diferença de Alturas (difAltura) entre as 'Peca's é menor (ou igual) que 0.2 (difAlturaMaxima).
--
-- Para calcular a diferença de Alturas entre 2 'Peca's é utilizada a função :
--
-- @ calculaDiferencaAlturas :: 'PosicaoMatriz' -> 'Mapa' -> Int -> Double -> (Double, Double, Double, Double) @
-- 
-- Se estes três critérios forem correspondidos, o 'Jogador' pode mudar, de facto, de 'Pista'.
--
-- Todavia, são, também, impostas condições para quando certos requesitos não são respeitados, ocorrendo alterações no 'Estado' 
--do 'Jogador', tais como :
--
-- - Quando a Altura atual é menor que a Altura seguinte, o 'Jogador' morre e é acionado um TimeOut (definido como 1 segundo);
-- - Se a Altura atual for maior que a seguinte, o 'Jogador' morre, Cai e fica com Gravidade 0.
--
-- __Função aceleracao__
--
-- Esta função tem como objetivo efetuar uma das duas 'Jogada's - 'Acelera'r ou 'Desacelera'r :
--
-- @ aceleracao :: Int -> 'Jogada' -> ['Jogador'] -> ['Jogador'] @
--
-- De acordo com esta função, para o 'Jogador' efetuar qualquer uma das duas possíveis 'Jogada's, este tem de encontrar-se no chão,
--sendo usada, de novo, a função /posicaoChao/. 
--
-- Caso o 'Jogador' esteja no chão, tem-se que :
--
-- - Se a 'Jogada' for 'Acelera', o 'Estado' do 'Jogador' passa para /Chao True/;
-- - Por outro lado, se for 'Desacelera', o 'Estado' passa para /Chao False/.
-- 
-- Se a condição não se verificar, é devolvido o 'Jogador' inalterado.
--
-- __Função alteraInclinacao__
--
-- Nesta função, utilizando as 'Jogada's Movimenta D (para a direita) e E (para a esquerda), podemos efetuar uma 'Jogada' para mudar a inclinação
--do 'Jogador' :
--
-- @ alteraInclinacao :: Int -> 'Jogada' -> ['Jogador'] -> ['Jogador'] @
--
-- Para tal ser acontecer, é necessário respeitar duas condições :
--
-- 1. O 'Jogador' em questão tem de encontrar-se no A;  
-- 2. O 'Jogador' não pode estar 'Morto'
-- 
-- Caso contrário, será devolvida a lista de 'Jogador'es inalterada.
-- 
-- Para verificar se o 'Jogador' encontra-se no Ar é utilizada a função : 
--
-- @ posicaoAr :: 'Jogador' -> Bool @
--
-- Devolve a resposta "True", se ele estiver no Ar 
--
-- É utiizada a função :
--
-- @ atualizaIndiceLista :: Int -> a -> [a] -> [a] @
--
-- Para modificar um elemento num dado índice (Um 'Jogador'), encontrando-se associada com a função :
--
-- @ inclina :: 'Jogada' -> 'Jogador' -> 'Jogador' @
--
-- Esta atualiza o 'Estado' do 'Jogador', permitindo-lhe inclinar (+15ª) ou (-15ª).
-- Se efetuar um Movimento E, inclina (+15ª) e, se efetuar um Movimento D, inclina (-15ª).
--
-- Esta inclinacão é utilizada para os 'Jogador'es se movimentarem pelas 'Peca's melhor, em específico ao sair de uma 'Rampa'. 
--
-- __Função disparaCola__ 
--
-- Nesta última função : 
--
-- @ disparaCola :: Int -> 'Estado' -> 'Estado' @
--
-- O seu objetivo é efetuar uma 'Jogada' 'Dispara'. A 'Cola' é disparada para a 'Peca' anterior á do 'Jogador'. 
--
-- Para um 'Jogador' disparar 'Cola' ele tem de estar no chão e, caso esta condição se verifique, são utilizadas as funções /dispara/
--e /municaoJogador/, para alterar o 'Estado' do Jogo :
--
-- @ dispara :: Int -> 'Mapa' -> ['Jogador'] -> 'Mapa' @
--
-- A primeira dispara na 'Peca', ficando coberta com 'Cola', sendo que o 'Jogador' tem de ter munição e encontrar-se no chão, mas não  
--na 1ª 'Peca'.
-- Para além disto, temos que não pode-se aplicar 'Cola' num piso 'Boost'.
-- 
-- A partir da função :
--
-- @ verficaMunicao :: Jogador -> Bool @ 
--
-- Verfica-se se o 'Jogador' tem ou não munição. Devolve "True" caso o 'Jogador' tenha 'Cola'.
--
-- Para colocar 'Cola' no 'Piso' anterior, recorre-se á /atualizaPosicaoMatriz/ para modificar a 'Peca' anterior e a função :
--
-- @ atiraCola :: Peca -> Peca @ 
--
-- Para converter o 'Piso' em 'Cola', havendo dois casos para qunado é uma 'Recta' e quando é uma 'Rampa'. 
--
-- No que toca á segunda função :
--
-- @ municaoJogador :: Int -> ['Jogador'] -> ['Jogador'] @
--
-- Serve para retirar a munição que foi usada pelo 'Jogador', modificando o 'Estado' deste 'Jogador'.
--
-- Para tal acontecer, tem de ter, pelo menos, uma munição para gastar.
--
--

module Tarefa2_2019li1g177 where

import LI11920
import Tarefa0_2019li1g177
import Tarefa1_2019li1g177
import F_Aux

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
-- testesT2 = [(0, m, Estado (gera 1 10 1) (jogador0 1)) | m <- jogadas] ++ [(0, m, Estado (gera 1 10 1) (jogador1 1)) | m <- jogadas] ++ 

testesT2 = [(1, m, Estado (gera 2 10 1) [(Jogador 0 4.3 0 0 (Chao True)), (Jogador 1 5.3 0 0 e)]) | m <- jogadas, e <- estadoJogadorT] ++ 
 [(0, m, Estado (gera 2 10 1) [(Jogador 0 5.1 0 0 e), (Jogador 1 4.5 0 0 (Chao True))]) | m <- jogadas, e <- estadoJogadorT] ++
 [(0, m, Estado (gera 2 10 1) [(Jogador 0 3.2 0 0 e), (Jogador 1 4.5 0 0 (Chao True))]) | m <- jogadas, e <- estadoJogadorT] ++
 [(0, m, Estado (gera 2 10 1) [(Jogador 0 3.2 0 1 e), (Jogador 1 4.5 0 0 (Chao True))]) | m <- jogadas, e <- estadoJogadorT]
 

-- | lista de jogadas possíveis
jogadas :: [Jogada]
jogadas = [Movimenta B, Movimenta C, Movimenta D, Movimenta E, Acelera, Desacelera, Dispara]

-- | lista por compreensão de jogadores
-- abrangendo __todos as jogadas__ 
-- jogador0 :: Int -> [Jogador]
-- jogador0 n = [ Jogador x 3.2 0 0 (estadoJogadorT !! 0) | x <- [0 .. (n-1)]]

-- jogador1 :: Int -> [Jogador]
-- jogador1 n = [ Jogador x 3.2 0 0 (estadoJogadorT !! 1) | x <- [0 .. (n-1)]]


-- | Lista com vários estados
estadoJogadorT :: [EstadoJogador]
estadoJogadorT = [Chao True, Chao False, Ar 15 45 4, Ar 15 80 2, Ar 9 (-87) 3, Ar 15 90 2, Ar 15 (-90) 3, Morto 1.0]

-- * Função Principal da Tarefa 2.

-- | Efetua uma jogada. Dado uma 'Jogada', n.º de jogador e 'Estado', devolve o novo 'Estado'
-- 
-- __Input:__ n.º do jogador
-- __->__ 'Jogada'
-- __->__ 'Estado'
--
-- __Output:__ 'Estado'

jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada n movimento (Estado m js) = if movimento == Movimenta B || movimento == Movimenta C then Estado m (mudaPista n m movimento js)
                                   else if movimento == Acelera || movimento == Desacelera then Estado m (aceleracao n movimento js)
                                   else if movimento == Movimenta E || movimento == Movimenta D then Estado m (alteraInclinacao n movimento js)
                                   else (disparaCola n (Estado m js))

-- * Funções auxiliares - Principais

-- | Efetua jogada em que muda de pista.
--
-- __ @1 __ jogador tem de estar no chão. Caso contrário devolve o estado sem alterações
--
-- __ @2 __  a pista seguinte (+-1) tem de existir. Caso contrário devolve o estado sem alterações
--
-- __ @3 __ a diferença de alturas entre a pista atual e a pista seguinte, na distância x tem de ser < 0.2. Caso contrário o jogador morre

mudaPista :: Int -- ^ Identificador do jogador
          -> Mapa -- ^ 'Mapa' do jogo
          -> Jogada -- ^ 'Jogada' a realizar. Aceita 'Movimenta C' ou 'Movimenta B'
          -> [Jogador] -- ^ 'Jogadores'
          -> [Jogador] -- ^ 'Jogadores' após a jogada
mudaPista 0 m move ( j : js ) | posicaoChao j && ePosicaoMatrizValida ( pistaSeguinte, colunaJogador) m =  -- verifica condição @1 e @2.
   if difAlturaValida then (Jogador pistaSeguinte distanciaX velocidade cola estado) : js -- verifica condição @3.
   else if alturaAtual < alturaSeguinte then (Jogador pista distanciaX gravidade cola (Morto timeout)) : js -- jogador morto - esbarra. set timeout
   else (Jogador pistaSeguinte distanciaX velocidade cola (Ar alturaAtual inclinacaoAtual gravidade)) : js -- jogador morto - Cai, fica com gravidade 0
                              | otherwise = j : js
  where (Jogador pista distanciaX velocidade cola estado) = j
        (alturaAtual, inclinacaoAtual, alturaSeguinte, difAltura) = calculaDiferencaAlturas (pista, colunaJogador) m pistaSeguinte distanciaX -- @3
        difAlturaValida = abs (difAltura) <= difAlturaMaxima
        pistaSeguinte = if move == Movimenta B then pista + 1 else pista - 1 -- define se sobe ou desce
        colunaJogador = floor distanciaX -- calcula a coluna da matriz em que o jogador se encontra
        timeout = 1.0 -- tempo de timeout quando morre, 1 segundo
        gravidade = 0
        difAlturaMaxima = 0.2
mudaPista n m move ( j : js ) = j : mudaPista (n-1) m move js

-- | Efetua uma jogada 'Acelera' ou 'Desacelera'.
--
-- Recebe o nº do 'Jogador' -> qual é a 'Jogada' que vai efetuar -> a lista de 'Jogador' e devolve a lista de 'Jogador' atualizada
--
-- __1*__ Se a 'Jogada' for acelerar e o 'Jogador' estiver no chão, então 'Acelera' (Chao True). 
--
-- __2*__ Se a 'Jogada' for desacelerar e o 'Jogador' estiver no chão, então 'Desacelera' (Chao False).
--
-- __*__ Caso contrário a lista de 'Jogadores' é devolvida sem alterações
 
aceleracao :: Int -- ^ nº do 'Jogador'
           -> Jogada -- ^ a 'Jogada' a ser efetuada 
           -> [Jogador] -- ^ a lista de 'Jogador'
           -> [Jogador] -- ^ a lista de 'Jogador' atualizada (caso tenha ocorrido uma 'Jogada')
aceleracao 0 x (j : js) | x == Acelera && posicaoChao j 
                        = (Jogador pista distancia velocidade cola (Chao True)) : js -- @1
                        | x == Desacelera && posicaoChao j 
                        = (Jogador pista distancia velocidade cola (Chao False)) : js -- @2
                        | otherwise = j : js -- @3
                        where Jogador pista distancia velocidade cola estado = j
aceleracao n x (j : js) = j : aceleracao (n - 1) x js

-- | Efetua uma jogada mudança de inclinação.
--
-- __1*__ Jogador tem de estar no 'Ar'.
--
-- __2*__ Jogador não pode estar 'Morto'
--
-- __*__ Caso a condição não se verifique, devolve 'Jogadores' inalterado.

alteraInclinacao :: Int -- ^ Identificador do 'Jogador'
                 -> Jogada -- ^ 'Jogada' a realizar (Movimenta D ou Movimenta E)
                 -> [Jogador] -- ^ 'Jogadores' em jogo
                 -> [Jogador] -- ^ lista 'Jogadores' atualizada após 'Jogada'
alteraInclinacao n move js | posicaoAr j = atualizaIndiceLista n (inclina move j) js
                           | otherwise = js
                           where j = encontraIndiceLista n js

-- | Efetua uma jogada 'Dispara'. 
-- Consiste em alterar a 'Peca' anterior à localização do jogador
--
-- __1*__ a 'Peca' anterior ao jogador
--
-- __2*__ O 'Jogador' tem de estar no 'Chao'
--
-- __*__ Caso a condição não se verifique, devolve o Estado inalterado.
disparaCola :: Int -- ^ Identificador do 'Jogador' que vai disparar a 'Cola'
            -> Estado -- ^ 'Estado'do jogo antes da 'Jogada'
            -> Estado -- ^ novo 'Estado' do jogo 
disparaCola n (Estado m js) | posicaoAr (js !! n) = Estado m js
                            | otherwise = Estado (dispara n m js) (municaoJogador n js)

-- * Funções Auxiliares

-- | Verifica se o 'EstadoJogador' do 'Jogador' é 'Chao'

posicaoChao :: Jogador -- ^ 'Jogador' para testar
            -> Bool -- ^ True se estiver no chão
posicaoChao (Jogador p d x c estado) = estado == (Chao True) || estado == (Chao False)

-- | Verifica se o 'EstadoJogador' do 'Jogador' é 'Ar'

posicaoAr :: Jogador -- ^ 'Jogador' para testar
          -> Bool -- ^ True se estiver no ar
posicaoAr (Jogador p d v c (Ar a b g)) = True 
posicaoAr j = False 

-- | Verifica se o jogador está morto

jogadorMorto :: Jogador -- ^ 'Jogador' para verificar se está morto
             -> Bool -- ^ True se estiver morto
jogadorMorto (Jogador p d v c (Morto x)) = True
jogadorMorto _ = False

-- | Dada a posição n de um jogador e uma lista de jogadores, devolve o jogador n dessa lista.

posicaoJogador :: Int -- ^ Identificador do 'Jogador'
               -> [Jogador] -- ^ Lista de 'Jogadores'
               -> PosicaoMatriz -- ^ Posição do jogador no 'Mapa' (m,n)
posicaoJogador n l1 = (p, floor d)
                      where Jogador p d x c e = encontraIndiceLista n l1


-- | Verifica se o jogador tem ou não 'colaJogador' para utilizar 
--

verificaMunicao :: Jogador -- ^ Recebe um 'Jogador'
                -> Bool -- True se tiver 'colaJogador'
verificaMunicao j = c > 0
                  where Jogador p d v c e = j  

-- | Calcula a diferença de altura entre 2 peças
-- dada uma posição x
-- e devolve a altura e inclinação atual,
-- altura seguinte e 
-- diferença de alturas entre as duas peças.
--
--
-- __@1__ calcula altura peça inicial
--
-- __@2__ calcula altura peça final
--
-- __@3__ calcula a distância percorrida na peça (valor entre 0 e 1)
--
-- __@4__ calcula a altura atual na posição x
--
-- __@5__ calcula a altura seguinte na posição x
--
-- __@6__ devolve a inclinacao da peça atual

calculaDiferencaAlturas :: PosicaoMatriz -- ^ Posição do 'Jogador' no 'Mapa' (nPista, coluna)
                        -> Mapa -- ^ 'Mapa'
                        -> Int -- ^ Número 'Pista' seguinte
                        -> Double -- ^ Distância percorrida no eixo x
                        -> (Double, Double, Double, Double) -- ^ (altura Atual, inclinacao atual, altura Seguinte, diferença Altura)
calculaDiferencaAlturas (p1, p2) m pSeguinte distancia = (alturaAtual, inclinacao, alturaSeguinte, difAltura)
                             where pecaAtual = encontraPosicaoMatriz (p1, p2) m
                                   (a1,a2) = verificaAlturaPeca(pecaAtual) -- @1
                                   (b1,b2) = verificaAlturaPeca(encontraPosicaoMatriz (pSeguinte, p2) m) -- @2
                                   distanciaPercorridaNaPeca = distancia - fromIntegral(p2) -- @3
                                   alturaAtual = calculaAlturaPosicao (a1,a2) distanciaPercorridaNaPeca -- @3
                                   alturaSeguinte = calculaAlturaPosicao (b1,b2) distanciaPercorridaNaPeca -- @4
                                   difAltura = alturaSeguinte - alturaAtual -- @5
                                   inclinacao = radToDeg (inclinacaoPeca pecaAtual) -- @6

-- | Altera o estado jogador, inclina (+15º) ou (-15º)
--

inclina :: Jogada -- ^ Recebe uma 'Jogada' - 'Movimenta' 'D' (-15º) ou 'Movimenta' 'E' (+15º)
        -> Jogador -- ^ Recebe o 'Jogador' que está a efetuar a 'Jogada'
        -> Jogador -- ^ Devolve o 'Jogador' com o 'EstadoJogador' alterado.

inclina (Movimenta x) (Jogador p d v c (Ar a i g)) = j
                                                   where j = Jogador p d v c (Ar a novaI g)
                                                         novaI = if x == D then max inclinacaoMaxDta ( i + inclinacao )
                                                                 else min inclinacaoMaxEsq (i + inclinacao)
                                                         inclinacao = if x == D then inclinaDta else inclinaEsq
                                                         inclinaDta = (-15)
                                                         inclinaEsq = (15)
                                                         inclinacaoMaxDta = (-90)
                                                         inclinacaoMaxEsq = 90


-- | Dispara na 'Peca' que fica coberta com 'Cola' 
--
-- __1*__ Verifica se o 'Jogador' tem munição.
--
-- __2*__ Verifica se o 'Jogador' se encontra no chão.
--
-- __3*__ Verifica se o 'Jogador' não se encontra na 1ª 'Peca'.
--
-- __4*__ Verfica se o 'Piso' anterior nao é um 'Boost'.
--
-- __5*__ Coloca a 'Cola' no 'Piso' anterior.
--
-- __*__ Caso não se verifique a condição, o 'Mapa' é devolvido inalterado.

dispara :: Int -- ^ Identificador do 'Jogador' que vai dispara
        -> Mapa -- ^ 'Mapa' 
        -> [Jogador] -- ^ lista de 'Jogadores'
        -> Mapa -- ^ mapa atualizado
dispara n m js | verificaMunicao j -- 1*
               && posicaoChao j -- 2*
               && c > 0 -- 3*
        --        && verificaPiso pecaAnterior /= Boost -- 4*
               = atualizaPosicaoMatriz (p, c - 1) novaPeca m -- 5*
               | otherwise = m
               where  (p, c) = posicaoJogador n js -- Indica a posição do 'Jogador'
                      j = encontraIndiceLista n js -- Indica qual é o 'Jogador'
                      novaPeca = atiraCola (pecaAnterior) -- A nova 'Peça' (com 'Cola')
                      pecaAnterior = encontraPosicaoMatriz (p, c - 1) m -- Posição da 'Peça' anterior


-- | Gastar uma munição de 'Cola' de um 'Jogador' 
--
-- __1*__ Indica se o 'Jogador' tem (pelo menos) uma munição de 'Cola'. 
--
-- __2__ Retira uma munição do 'Jogador'
--
-- __*__ Caso a condição não se verifique, devolve a lista de 'Jogadores' inalterada

municaoJogador :: Int -- ^ Identificador do 'Jogador'
               -> [Jogador] -- ^ lista de 'Jogador'
               -> [Jogador] -- ^ lista de 'Jogador' atualizada (-1 Munição) 
municaoJogador n js | c > 0 
                      && (floor d) > 0
                      && not (jogadorMorto jogador)  -- ·1*
                    = atualizaIndiceLista n (Jogador p d v (c - 1) e) js -- 2*
                    | otherwise = js 
                    where jogador = encontraIndiceLista n js -- Indica qual é o 'Jogador'
                          Jogador p d v c e = jogador -- destrutura o jogador


-- | Converte a 'Peca' anterior em 'Cola' ('Rampa' ou 'Recta')
-- 
-- __1__ Caso seja uma 'Rampa'
--
-- __2__ Caso seja uma 'Recta'

atiraCola :: Peca -- ^ 'Peca' original 
          -> Peca -- ^  'Peca' atualizada (com 'Cola')
atiraCola (Rampa x a b) = Rampa Cola a b -- 1
atiraCola (Recta x a) = Recta Cola a -- 2


