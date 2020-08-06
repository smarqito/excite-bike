-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
--
-- = Relatório da Tarefa 1
--
-- __/Objetivo da Tarefa/__
-- 
-- Nesta tarefa, __objetivo principal__ é construir um mapa para o jogo a ser desenvolvido (isto é, definir uma função que construa 'Mapa's)
-- em que o utilizador tenha a liberdade de escolher o número de 'Pista's, número de 'Peca's e um inteiro (designado /semente/) que servirá para a pseudo-aleatoriedade da função.
-- Os 'Mapa's são constituídos por 'Pista's que, por sua vez, são constituídas por 'Peca's, sendo estas limitadas aos tipos 'Piso's definidos pelos docentes.
-- 
-- __Função Objetivo:__ /(gera)/
-- 
-- @ gera :: Int -> Int -> Int -> 'Mapa' @
--
-- Esta função tem como /output/ um 'Mapa' (ou Matriz de 'Peca's). Para tal, é necessário que receba 3 argumentos (/input/: Número de pistas,
-- Nº de 'Peca's de cada 'Pista' e uma semente).
--
-- Para a realização desta tarefa, houve um conjunto de condições pré-definidas pelos docentes que serviram como linhas orientadoras que serão doravante descritas.
-- 
-- == Condições pré-definidas pelos docentes 
-- Como referido anteriormente, para a construção do 'Mapa', é necessário definir cada 'Pista'.
--
-- Será importante referir que cada 'Pista' começa com a 'Peca' __ Recta Terra 0 __
--
-- As linhas orientadoras definidas pelos docentes, compreendem a utilização de uma lista de 'Peca's para a construção de cada 'Pista':
--
-- @ data Peca = Rampa 'Piso' Int Int @
--
-- @           | Recta 'Piso' Int @
--
-- Este /data type/ define como deve ser construída cada 'Peca'. Há, portanto, dois tipos de 'Peca', em ambos é necessário definir o tipo de 'Piso'. 
-- No caso da 'Recta' é necessário dedicar um valor Inteiro que se refere à sua altura e
-- e em relação à 'Rampa', prevê a utilização de dois valores inteiros. O primeiro para definir a altura inicial e o segundo para a altura final.
-- 
-- Para que o mapa seja considerado válido, é necessário que obedeca aos seguintes critérios:
--
--
-- == Definir o 'Piso'
-- Para definir o 'Piso', é utilizado o primeiro número aleatório tendo por referência a seguinte tabela: 
-- 
-- __Tabela referência para 'Piso':__
--
-- +---------+-----------------------------+
-- | Gama    | Conteudo                    |
-- +=========+=============================+
-- | 0 a 1   | Piso de 'Terra'             |
-- +---------+-----------------------------+
-- | 2 a 3   | Piso de 'Relva'             |
-- +---------+-----------------------------+
-- | 4       | Piso de 'Lama'              |
-- +---------+-----------------------------+
-- | 5       | Piso de 'Boost'             |
-- +---------+-----------------------------+
-- | 6 a 9   | O 'Piso' da 'Peca' anterior |
-- +---------+-----------------------------+ 
--
-- === Função definePiso
-- Para cumprir os critérios definidos pelos docentes em relação ao 'Piso' da 'Peca', foi desenvolvida a função definePiso.
-- Esta função recebe dois argumentos (/input/: Gama e Piso anterior) e retorna (/output/) um 'Piso'
-- 
-- @definePiso:: Int -> 'Piso' -> 'Piso' @
--
-- == Definir a Peca
--
-- Conforme descrito, as 'Peca's podem ser do tipo 'Rampa' ou 'Recta'.
--
-- Tal como na definição do 'Piso', também neste caso existe uma tabela de referência para a criação de 'Peca's.
--
-- __Tabela de referência para 'Peca'__
--
-- +---------+------------------------------------------------+
-- | Gama    | Conteudo                                       |
-- +=========+================================================+
-- | 0 a 1   | 'Rampa' que sobe com diferenca                 |
-- |         | __Gama__ + 1 para a altura anterior            |
-- |         |                                                |
-- +---------+------------------------------------------------+
-- | 2 a 5   | 'Rampa' que desce com diferenca maxima         |
-- |         | __Gama__ - 1 para a altura anterior            |
-- |         | Altura minima: 0. Se altura anterior igual     |
-- |         | a atual, entao e uma 'Recta'                   |
-- +---------+------------------------------------------------+
-- | 6 a 9   | 'Recta' com altura anterior                    |
-- +---------+------------------------------------------------+ 
-- 
-- == Método
-- Por forma a aplicar os critérios definidos, começa-se por gerar uma lista de valores /Gama/ que permita posteriormente converter para 'Peca's.
-- Para tal, utilizou-se a função disponibilizada pelos docentes __geraAleatorios__ que dado o número de inteiros que se pretende e uma semente de aleatoriedade
-- devolve uma lista com o número de inteiros indicada e com valores entre 1 e 9 (delta Gama).
-- 
-- === Preparar a lista de inteiros para a conversão
-- Após obter a lista de inteiros que originará as 'Peca's, optou-se por dividir essa lista em __n__ listas (sendo n o número de 'Pista's que o 'Mapa' terá).
-- Esta divisão teve em consideração que seria necessário dois inteiros para cada 'Peca'.
--
-- @criaMatrizPistaInt :: Int -> Int -> [Int] -> [[Int]] @
-- 
-- === Converter a lista de lista de inteiror
--
-- @ converteMatrizPistaInt :: [[Int]] -> Mapa @
--
-- Depois de ter o 'Mapa' sob a forma de inteiros, foi necessário converter os inteiros em 'Peca's efetivas. Para tal, teve-se em consideração as condições descritas anteriormente.
--
--
-- Assim, ao percorrer a lista de 'Pista's sob a forma de /Inteiros/, aplicou-se a seguinte função a cada uma:
--
-- @ definePista :: [Int] -> Int -> 'Piso' -> 'Pista' @
-- 
-- Utilizando a função definePiso (/descrita anteriormente/), definiu-se o 'Piso' da 'Peca'.
--
-- Através do valor de /Gama/ indicado para a 'Peca', calculou-se a altura seguinte e com base na diferença entre a altura seguinte e
-- a atual, definiu-se se seria uma 'Rampa' ou 'Recta.
--
-- === Função gera
-- A título conclusivo, foi necessário englobar todas as funções em apenas uma: /gera/.
--
-- Para tal, começou por se definir variáveis locais:
--
-- @ pseudoAleatorio = geraAleatorios (npistas * comprimento * 2 - 2 * npistas) semente @
--
-- Uma variável para gerar a lista de inteiros que originará o 'Mapa'
--
-- @ matrizPista = criaMatrizPistaInt npistas comprimento pseudoAleatorio @
--
-- Depois de obter a lista de inteiros, dividi-la em várias 'Pista's de inteiros.
--
-- @ converteMatrizPistaInt matrizPista @
--
-- Por fim, converter o 'Mapa' de Inteiros em 'Mapa' de 'Peca's.

module Tarefa1_2019li1g177 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/, /comprimento de cada 'Pista' do 'Mapa'/, /semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(x,y,z) | x <- [1 .. 20], y <- [30 .. 50], z <- [1 .. 3]]

-- * Funções pré-definidas da Tarefa 1.

-- | Random para as 'Pista's
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Função principal da Tarefa 1.

-- | Cria um mapa de 'Peca', recebendo o número de 'Pista', o comprimento de cada 'Pista' e uma "semente" para a pseudo-aleatoriedade.
--
-- 
gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = let pseudoAleatorio = geraAleatorios (npistas * comprimento * 2 - 2 * npistas) semente
                                       matrizPista = criaMatrizPistaInt npistas comprimento pseudoAleatorio
                                    in converteMatrizPistaInt matrizPista

-- * Funções auxiliares da Tarefa 1
-- | Define o 'Piso' tendo em conta o 'Piso' anterior.
--
-- __Input:__  Inteiro do 'Piso' atual 
-- __->__ 'Piso' anterior 
--
-- __Output:__ 'Piso' da Peça seguinte

definePiso :: Int -> Piso -> Piso
definePiso x p | x == 0 || x == 1 = Terra
               | x == 2 || x == 3 = Relva
               | x == 4 = Lama
               | x == 5 = Boost
               | 6 <= x && x <= 9 = p -- se for entre 6 e 9 retorna o piso da peça anterior!

-- | Cria uma 'Pista' através de uma lista de inteiros. Tem em consideração a altura e a 'Peça' anterior.
--
-- Utiliza a função definePiso para definir o 'Piso' da nova 'Peça'
--
-- prop> piso = definePiso p pisoanterior
-- 
-- Define a nova altura através do #Gama. 
-- Não é permitido alturas negativas.
--
-- prop> inclinacao = if t == 0 || t == 1 then ( a + t + 1) else if 2 <= t && t <= 5 then (max 0 ( a - (t - 1) ) ) else a
-- 
-- Define o terreno através da inclinação
-- Utiliza o #Piso (Int) e o #Gama 
--
-- __Input:__ lista de Inteiros (cada 'Piso' é um par de inteiros [altura,gama,...])
-- __->__ Altura 'Piso' anterior
-- __->__ 'Piso' anterior
-- 
-- __Output:__ 'Pista'

definePista :: [Int] -> Int -> Piso -> Pista -- pistaEmInt -> altura anterior -> 'Piso'
definePista [] a b = []
definePista ( p : t : pt ) a pisoanterior -- p = pisoInt; t = gama; a = altura anterior
    = let piso = definePiso p pisoanterior -- 
          inclinacao = if t == 0 || t == 1 then ( a + t + 1)
           else if 2 <= t && t <= 5 then (max 0 ( a - (t - 1) ) )
           else a
          terreno = if inclinacao /= a then Rampa piso a inclinacao
           else Recta piso inclinacao
        in terreno : (definePista pt inclinacao piso)

-- | Cria um 'Mapa' de Inteiros com n 'Pista's.
--
-- __Input:__ nPistas
-- __->__ comprimento de cada 'Pista'
-- __->__ lista gerada pela função 'geraAleatorios'
--
-- __Output:__ Matriz mxn com m 'Pista' e n comprimento

criaMatrizPistaInt :: Int -> Int -> [Int] -> [[Int]] -- recebe npistas comprimento listaPseudoAleatorio (gerada pelo geraAleatorio)
criaMatrizPistaInt 0 b c = []
criaMatrizPistaInt a b ( c : cs ) = ( 1 : 6 : criaPecasInt (b - 1) ( c : cs ) ) : ( criaMatrizPistaInt (a - 1) b ( drop ( ( b - 1 ) * 2 ) ( c : cs) ) )

-- | Cria um mapa final utilizando o mapa de inteiros definido na 'criaMatrizPistaInt'
--
-- __Input:__ Matriz de Inteiros
--
-- __Output:__ Matriz de 'Peca'
--
-- Utiliza as funções auxiliares 'definePista' e 'converteMatrizPistaInt'

converteMatrizPistaInt :: [[Int]] -> Mapa
converteMatrizPistaInt [] = []
converteMatrizPistaInt ( x : xs ) = definePista x 0 Terra : converteMatrizPistaInt xs

-- | Recebe a lista para percorrer todos os seus elementos e retornar as 'Peca's sob a forma de inteiros ( cada peça são dois Inteiros )

criaPecasInt :: Int -> [Int] -> [Int]
criaPecasInt 0 l = []
criaPecasInt a ( b : c : bs ) = b : c : criaPecasInt ( a - 1 ) bs

