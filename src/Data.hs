-- | Este módulo tem definido os tipos de dados específicos para a ligação entre os menús e o gloss
module Data where

import LI11920
import F_Aux
import Graphics.Gloss

-- * Data Type

--  Definição do tipo 'EstadoGloss'

-- | Definição do 'EstadoGloss'
type EstadoGloss = (EstadoJ,[Picture])

-- | Definição do 'EstadoJ'
-- Consiste num composto com 4 elementos:
--
-- @1º 'Estado' do jogo
--
-- @2º lista de ternos com informação sobre o 'Jogador': identificador, 'Joga' e nome
--
-- @3º cronómetro
--
-- @4º Localização do 'Local' em que o utilizador se encontra
type EstadoJ = (Estado,PropJogadores, Float, Local)

type PropJogadores = [(Int, Joga, String, Float)]

-- | Tipo de 'Jogador' e informação se já chegou ao 'Fim' ou 'Perdeu' o jogo
data Joga = Player (Maybe Jogada)
          | Bot
          | Perdeu
          | Fim { tempo :: Float }
          deriving (Show, Eq)

-- | Local em que o utilizador se encontra
data Local = Menu TipoMenu
           | Game Start deriving Eq

-- | Tipo de menú em que o utilizador se encontra
data TipoMenu = 
    Home
    | NumeroJogadores Int
    | NomeJogadores Int String [String]
    | NumeroBots [String] Int
    | EscolheMapa [String] Int Int
    | Score
    | AboutUs
    deriving Eq 

-- | Permite saber se o jogo já começou ou se está em fase 'Countdown'
data Start = Countdown Float | Play deriving Eq

-- * Instâncias

-- | Instância para ordenar 'Peca's com base no seu 'Piso'
-- Utiliza a instância Ord Piso 
instance Ord Peca where
    compare (Recta p1 _) (Recta p2 _) = compare p1 p2
    compare (Recta p1 _) (Rampa p2 _ _) = compare p1 p2
    compare (Rampa p1 _ _) (Rampa p2 _ _) = compare p1 p2
    compare (Rampa p1 _ _) (Recta p2 _) = compare p1 p2

-- | Instância para ordenar 'Piso's com base no seu atrito
instance Ord Piso where
    compare p1 p2 = compare a1 a2
        where a1 = atritoPiso p1
              a2 = atritoPiso p2

-- | Instância para ordenadar 'Jogador's com base na sua distância
instance Ord Jogador where
    compare (Jogador _ d1 _ _ _) (Jogador _ d2 _ _ _) = compare d1 d2


-- * Funções específicas para os Data Type definidos neste ficheiro
-- | Verifica se o 'Jogador' está a jogar
jJoga :: Joga
 -> Bool
jJoga (Player (Just _) ) = True
jJoga Bot = True
jJoga _ = False

-- | Verifica se o 'Jogador' já 'Perdeu' ou chegou ao 'Fim'
jTerm :: Joga
 -> Bool
jTerm (Fim _) = True
jTerm Perdeu = True
jTerm _ = False

-- | Verifica se o 'Jogador' 'Perdeu' o jogo
jPerdeu :: Joga
 -> Bool
jPerdeu Perdeu = True
jPerdeu _ = False

-- | Verifica se é 'Bot'
isBot :: Joga
 -> Bool
isBot Bot = True
isBot _ = False

getFim :: Joga
 -> Maybe Float
getFim (Fim x) = Just x
getFim _ = Nothing