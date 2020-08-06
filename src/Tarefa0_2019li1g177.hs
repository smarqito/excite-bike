-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g177 where

    import Data.Fixed (mod')
    -- * Funções não-recursivas.
    
    -- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
    --
    -- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
    -- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
    --
    -- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
    data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show
    
    -- | Um ângulo em graus.
    type Angulo = Double
    
    -- ** Funções sobre vetores
    
    -- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
    type Vetor = Ponto
    -- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>
    
    -- | recuperar x de um ponto
    posx :: Ponto -- ^ Recebe um ponto
     -> Double -- ^ retorna o valor X desse ponto
    posx (Cartesiano x y) = x
    posx (Polar r a) = r * cos( (a * pi)/180  ) -- tem de se passar para radianos!
    
    -- | recuperar y de um ponto
    posy :: Ponto -- ^ Recebe um ponto
     -> Double -- ^ retorna o valor Y desse ponto
    posy (Cartesiano x y) = y
    posy (Polar r a) = r * sin((a * pi)/180)
    
    -- *** Funções gerais sobre 'Vetor'es.
    
    -- | Soma dois 'Vetor'es.
    somaVetores :: Vetor -> Vetor -> Vetor
    somaVetores p1 p2 = Cartesiano (posx p1 + posx p2 ) (posy p1 + posy p2) 
    
    -- | Subtrai dois 'Vetor'es.
    subtraiVetores :: Vetor -> Vetor -> Vetor
    subtraiVetores p1 p2 = Cartesiano (posx p1 - posx p2)(posy p1 - posy p2)
    
    -- | Multiplica um escalar por um 'Vetor'.
    multiplicaVetor :: Double -> Vetor -> Vetor
    multiplicaVetor x v = Cartesiano ( x * ( posx v ) ) ( x * (posy v) )
    
    -- ** Funções sobre rectas.
    
    -- | Um segmento de reta é definido por dois pontos.
    type Reta = (Ponto,Ponto)
    
    -- | Testar se dois segmentos de reta se intersetam.
    --
    -- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
    intersetam :: Reta -> Reta -> Bool
    intersetam ( p1 , p2 ) ( p3, p4 ) = 0.0 <= ta && ta <= 1.0 && 0.0 <= tb && tb <= 1.0
                                        where   ta = (((posy p3 - posy p4) * (posx p1 - posx p3)) + ((posx p4 - posx p3) * (posy p1 - posy p3))) / d
                                                tb = (((posy p1 - posy p2) * (posx p1 - posx p3)) + ((posx p2 - posx p1) * (posy p1 - posy p3))) / d
                                                d = ((posx p4 - posx p3) * (posy p1 - posy p2)) - ((posx p1 - posx p2) * (posy p4 - posy p3))
    
    -- | Calcular o ponto de intersecao entre dois segmentos de reta.
    -- | pode-se usar a referência p1@ para -> p1@(Cartesiano x1 y1) depois pode-se usar o p1 para definir uma referência.
    -- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
    intersecao :: Reta -> Reta -> Ponto
    intersecao (p1, p2) (p3, p4) = somaVetores  p1 ( multiplicaVetor ta ( subtraiVetores p2 p1 )  ) 
        where ta = (((posy p3 - posy p4) * (posx p1 - posx p3)) + ((posx p4 - posx p3) * (posy p1 - posy p3))) / d
              d = ((posx p4 - posx p3) * (posy p1 - posy p2)) - ((posx p1 - posx p2) * (posy p4 - posy p3))
    
    -- ** Funções sobre listas
    
    -- *** Funções gerais sobre listas.
    --
    -- Funções não disponíveis no 'Prelude', mas com grande utilidade.
    
    -- | Verifica se o indice pertence à lista.
    --
    -- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
    eIndiceListaValido :: Int -> [a] -> Bool
    eIndiceListaValido i l = i < length l && 0 <= i
    
    -- ** Funções sobre matrizes.
    
    -- *** Funções gerais sobre matrizes.
    
    -- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
    type DimensaoMatriz = (Int,Int)
    
    -- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
    -- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
    --
    -- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
    type PosicaoMatriz = (Int,Int)
    
    -- | Uma matriz é um conjunto de elementos a duas dimensões.
    --
    -- Em notação matemática, é geralmente representada por:
    --
    -- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
    type Matriz a = [[a]]
    
    -- | Calcula a dimensão de uma matriz.
    --
    -- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
    --
    -- __Sugestão:__ relembre a função 'length', referida anteriormente.
    dimensaoMatriz :: Matriz a -> DimensaoMatriz
    dimensaoMatriz [] = ( 0 , 0 )
    dimensaoMatriz ( [] : t ) = ( 0 , 0 )
    dimensaoMatriz (x : xs) =  ( length ( x : xs ), length x )
    
    
    -- | Verifica se a posição pertence à matriz.
    ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
    ePosicaoMatrizValida (m, n) [] = False
    ePosicaoMatrizValida (m, n) ( x : xs ) = let (a, b) = dimensaoMatriz (x : xs)
                                             in 0 <= m && m < a && 0 <= n && n < b
    
    -- * Funções recursivas.
    
    -- ** Funções sobre ângulos
    
    -- | Normaliza um ângulo na gama [0..360).
    --  Um ângulo pode ser usado para representar a rotação
    --  que um objecto efectua. Normalizar um ângulo na gama [0..360)
    --  consiste, intuitivamente, em extrair a orientação do
    --  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
    --
    -- prop> normalizaAngulo 360 = 0
    -- prop> normalizaAngulo 390 = 30
    -- prop> normalizaAngulo 720 = 0
    -- prop> normalizaAngulo (-30) = 330
    normalizaAngulo :: Angulo -> Angulo
    normalizaAngulo x = mod' x 360
    

    -- ** Funções sobre listas.
    
    -- | Devolve o elemento num dado índice de uma lista.
    --
    -- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
    encontraIndiceLista :: Int -> [a] -> a
    encontraIndiceLista 0 ( x : xs ) = x
    encontraIndiceLista i ( x : xs ) = encontraIndiceLista ( i - 1 ) xs
    encontraIndiceLista _ _ = error "não deu"
    
    -- | Modifica um elemento num dado índice.
    --
    -- __NB:__ Devolve a própria lista se o elemento não existir.
    atualizaIndiceLista :: Int -> a -> [a] -> [a]
    atualizaIndiceLista 0 add ( x : xs ) = add : xs 
    atualizaIndiceLista i add ( x : xs ) = x : atualizaIndiceLista ( i - 1 ) add xs
    -- atualizaIndiceLista i add ( h : t ) = encontraIndiceLista i (h:t)
    
    -- ** Funções sobre matrizes.
    
    -- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
    encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
    encontraPosicaoMatriz (p1, p2) l = encontraIndiceLista p2 (encontraIndiceLista p1 l)
    
    -- | Modifica um elemento numa dada 'Posicao'
    --
    -- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
    atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
    atualizaPosicaoMatriz (0, n) novo ( x : xs ) = (atualizaIndiceLista n novo x) : xs
    atualizaPosicaoMatriz (m, n) novo ( x : xs) =  x : atualizaPosicaoMatriz ( m - 1 , n) novo xs