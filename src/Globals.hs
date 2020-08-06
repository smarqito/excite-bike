-- | Este módulo tem definido constantes globais do projeto.
module Globals where


-- * Propriedades de 'Peca'

-- * Valores dos atritos das peças

-- | Atrito do 'Piso' 'Terra'
atritoTerra :: Double
atritoTerra = 0.25

-- | Atrito do 'Piso' 'Relva'
atritoRelva :: Double
atritoRelva = 0.75

-- | Atrito do 'Piso' 'Lama'
atritoLama :: Double
atritoLama = 1.50

-- | Atrito do 'Piso' 'Boost'
atritoBoost :: Double
atritoBoost = -0.50

-- | Atrito do 'Piso' 'Cola'
atritoCola :: Double
atritoCola = 3.00

-- * Tamanho da 'Peca'

-- | Tamanho de cada 'Peca' no eixo X
tamanhoP :: Double
tamanhoP = 1.0

-- * Resistência do 'Ar'

-- | Valor da resistência do Ar
resistAr :: Double
resistAr = 0.125

-- * Aceleração da 'Gravidade'

-- | Valor da aceleração da Gravidade
acelGravidade :: Double
acelGravidade = 1.0

-- * Timeout quando morre

-- | Tempo que fica morto
timeOutMorto :: Double
timeOutMorto = 1.0

-- * Valores de Display

-- | Largura da Janela
width :: Int
width = 1500

-- | Altura da Janela
heigth :: Int
heigth = 1000

-- | Frame rate
fr :: Int
fr = 150

-- | Valor escalar no eixo Y
tPecaY :: Float
tPecaY = 75

-- | Valor escalar no eixo X
tPeca :: Double
tPeca = 150
-- tPeca = fromIntegral (div c l)
--     where (Estado m j, e) = estadoInicial
--           l = length (m !! 0)
--           c = width - 100

-- | Margem lateral da janela no eixo X
iX :: Float
iX = negate (fromIntegral (div width 2) - 100)