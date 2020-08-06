-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
--
-- | Relatorio da Tarefa 3
--
-- O Propósito da Tarefa 3 é desconstruir o 'Mapa' em várias 'Instruções', por meio de vários padrões estabelecidos, para serem 
-- efetuados por um grupo de bulldozers (um por pista) que avançam da partida para construir o mapa em questão.
-- Procurou-se que o número de instruções seja o mínimo possível, por forma a maximizar a taxa de compressão das instruções.
--
-- __Função Objetivo__ __/(desconstroi)/
--
-- @ desconstroi :: 'Mapa' -> 'Instrucoes' @
-- 
-- Esta recebe o 'Mapa' atual e transforma-o numa sequência de 'Instrucoes', que pode ser efetuado por dois padrões especificos - 
-- Padrão Horizontal e o Padrão Vertical. A um nível mais avançado, pode ainda encontrar-se o padrão vertical e horizontal desfasado.
-- Este último compreende encontrar padrões no mapa de em posições diferentes no eixo do X.
--
-- Deste modo, o /input/ vai ser o 'Mapa' a ser descontruído e o /output/ vai ser um conjunto de instruções a serem efetuadas.
--
-- Para a realização desta tarefa foram fornecidos um conjunto de condições a serem efetuadas.
--
-- == Condições pré-definidas pelos docentes
--
-- Uma 'Instrucao' válida não pode ser repetir os identificadores dos Bulldozers. 
--
-- O comprimento da 'Pista' tem de ser maior que 1².
--
-- Deste modo, tendo em conta estes critérios, foram definidos dois Padrões.
--
-- == Definir o Padrão Horizontal
--
-- Para definir este Padrão
--
--
-- == Definir Padrão Vertical  
--
-- Para definir este Padrão, 'Peca's iguais em posições iguais sao aglomeradas, de modo a serem efetuadas ao mesmo tempo.
-- Neste sentido, são identificadas 'Pista's com o mesmo tipo de 'Peca', para serem descontruídas juntamente.
--
-- == Método 
--
-- Para desenvolver estes Padrões recorreu-se à função /recebeTransposta/ para construir as instruções. 
-- Esta função recebe o mapa transposto (Matriz de 'Peca's), sendo utilizado, para isso, a função /transpose/.
-- Com estas duas é definida a função /descontroiV/.
--
-- == Função /recebeTransposta/
--
-- @recebeTransposta :: 'Mapa' -> 'Instruções' @
--
-- Com o 'Mapa' transposto, é utilizada a função /padraHorizontalV/ :  
--
-- @padraoHorizontalV :: @
--
-- == Função /desconstroiV/ 
--
-- @ desconstroiV :: 'Mapa' -> 'Instrucoes'@
--
-- A principal dificuldade desta tarefa prendeu-se em encontrar padrões desfasados. 
-- Para encontrar tais padrões, foi utilizada a função /organizaMapa/ para, dado um mapa, após um conjunto de funções aplicadas, encontrou-se alguns padrões e utilizando a posição do bulldozer
-- conseguiu-se utilizar a Teleporta por forma a otimizar e construir o mapa com o mínimo de instruções possível.

module Tarefa3_2019li1g177 where

import LI11920
import Tarefa1_2019li1g177
import Tarefa2_2019li1g177
import F_Aux
import Data.List

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [gera x (y) z | x <- [5 .. 8], y <- [8 .. 10], z <- [7 .. 9]]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.

desconstroi :: Mapa 
            -> Instrucoes
desconstroi m = optRepete r
              where complexo = padraoHorizontalInst (desconstroiM (map tail m)) -- desconstroiV
                    vertical = padraoHorizontalInst (desconstroiV m)
                    tV = tamanhoInstrucoes vertical
                    tC = tamanhoInstrucoes complexo
                    r = if tC < tV then complexo else vertical

-- * Funções auxiliares principais
-- | Desconstroi o mapa utilizando padrões verticais

desconstroiV :: Mapa
             -> Instrucoes
-- desconstroiV [[]] = []
desconstroiV m = recebeTransposta (drop 1 (transpose m) )

-- | desconstroi o mapa reunindo peças iguais e utilizando teleporta

desconstroiM :: Mapa
             -> Instrucoes
desconstroiM m = criaInstrOrg (zipWith (,) [0 .. ((length m)- 1)] (repeat (-1))) [] (organizaMapa m)


-- * Funções auxiliares da Tarefa 3

-- | recebe o mapa transposto e utilizando uma função auxiliar constroi as instruções

recebeTransposta :: Mapa
                 -> Instrucoes
recebeTransposta [] = []
recebeTransposta m  = (padraoHorizontalV 0 (m !! 0)) ++ recebeTransposta (drop 1 m)

-- | auxiliar utilizada na função recebeTransposta para construir as instruções

padraoHorizontalV :: Int -- ^ identificador da 'Pista'
                  -> Pista -- ^ 'Pista' a percorrer (linha da transposta do mapa original)
                  -> Instrucoes -- ^ instruções
padraoHorizontalV i [] = []
padraoHorizontalV i p  = (defineInstrucao ii pp) : padraoHorizontalV (nInd + 1) t
                       where (ii, t, pp) = agrupaBull i p
                             -- proximoG = agrupaBull (maximum grupo + 1) p
                             nInd = (maximum ii)
                             b = if ( i - 1 ) < 0 then 0 else (i - 1)

-- | agrupa os bulldozers quando as peças são iguais

agrupaBull :: Int -- ^ identificador do bulldozer
           -> Pista -- ^ 'Pista' para percorrer e agrupar bz
           -> ([Int], Pista, Peca)
agrupaBull i [x] = ([i], [], x)
agrupaBull i (x : y : xy) | x == y = (i : a, b, x)
                          | otherwise = ([i], (y:xy), x)
                          where (a,b,c) = agrupaBull (i+1) (y:xy)    

-- | Auxiliar que aceita a lista de bulldozers e
-- | desconstroi a peça devolvendo uma instrução

defineInstrucao :: [Int] 
                -> Peca
                -> Instrucao
defineInstrucao b (Recta p a) = Anda b p
defineInstrucao b (Rampa p ai af) | ai < af = Sobe b p (af - ai)
                                  | otherwise = Desce b p (ai - af)

-- | recebe um mapa e cria um triplo
-- | devolve uma lista com (#bz, (iPeça, 'Peca'))
criaTriplo :: Mapa
           -> [(Int, (Int, Peca))]
criaTriplo = posTriplo 0 . triplo

-- | recebe o mapa e cria uma lista de duplos (indice, 'Peca')
triplo :: Mapa -- ^ 'Mapa'
       -> [[(Int, Peca)]]
triplo = map (\x -> zipWith (,) [0 ..] x)

-- | após criar um duplo, cria um triplo com a informação indicada na função 'criaTriplo'
posTriplo :: Int
          -> [[(Int, Peca)]]
          -> [(Int, (Int, Peca))]
posTriplo i [] = []
posTriplo i ( h : t ) = map (\(a,b) -> (i,(a,b)) ) h ++ posTriplo (i+1) t

-- | Dado uma 'Instrucao', extrai a lista de identificadores de bulldozers da mesma
extraiB :: Instrucao 
        -> [Int]
extraiB (Anda b p) = b
extraiB (Sobe b p i) = b
extraiB (Desce b p i) = b
extraiB (Teleporta b i) = b
extraiB (Repete x i) = nub (juntaAgrupa (map extraiB i))

-- | Dado uma 'Instrucao', devolve 'True' se a mesma for do tipo 'Repete'
eRepete :: Instrucao 
        -> Bool
eRepete (Repete x i) = True
eRepete _ = False

-- | Dada uma 'Instrucao' do tipo 'Repete', devolve as instruções associadas à mesma.
instRepete :: Instrucao 
           -> Instrucoes
instRepete (Repete x i) = i

-- | Dada uma 'Instrucao' do tipo 'Teleporta', extrai o movimento que o BZ deve fazer
extraiM :: Instrucao 
        -> Int
extraiM (Teleporta b i) = i

-- | Dado uma lista de listas, retorna o concat de todos os elementos
juntaAgrupa :: [[a]] 
            -> [a]
juntaAgrupa [] = []
juntaAgrupa ( h : t ) = h ++ (juntaAgrupa t)

-- | filtra todas as 'Peca' existentes num dado mapa
listaPecas :: [(Int, (Int, Peca))] 
           -> [Peca]
listaPecas [] = []
listaPecas ( (a, (b, p) ) : t) = [p] ++ (listaPecas t)

-- | Agrupa as peças que são iguais

mapaAgPecas :: [(Int, (Int, Peca))]
            -> [(Int, (Int, Peca))]
mapaAgPecas t = agrupaPecasMapa lp t
              where lp  = nub (listaPecas t)

-- | Dado um mapa, organiza-o para criar 'Instrucoes'.
-- 
-- @ devolve o uma lista de pares (indice, 'Instrucao')
organizaMapa :: Mapa 
             -> [(Int, Instrucao)]
organizaMapa m = sortOn (\x -> fst x) (la ++ ramI)
               where t = criaTriplo m
                     ram = encontraRampas t
                     ramI = map (\x -> criaRepeteR x) ram
                     sRam = semRampas (juntaAgrupa ram) t
                     l = 
                        (map ( \z -> 
                             ( map ( \ ( x, ( y,z ) ) -> 
                                   ( y,z ) ) ( map ( \ ( a, ( b, c ) ) ->  
                                         ( a, ( b, ( defineInstrucao a c ) ) ) ) (map agrupaBzInstr ( groupBy ( \x y -> 
                                              ( fst ( snd x ) ) == ( fst ( snd y ) ) )  ( sortOn ( \x -> 
                                                    fst ( snd  x ) ) z ) ) ) ) ) )  (agrIndice) )
                     la = juntaAgrupa l
                     agrupaPecas = groupBy (\x y -> (snd (snd x)) == (snd (snd y)) ) (mapaAgPecas sRam)
                     agrIndice = (map (\x -> sortOn (\y -> fst y ) x ) agrupaPecas)

-- | agrupa as as rampas desde o início até ao seu fim
encontraRampas :: [(Int, (Int, Peca))] 
               -> [[(Int, (Int, Peca))]]
encontraRampas m = (map (\x -> 
      juntaAgrupa x) (map (\x -> 
            agrPorPosicao x m ) (juntaAgrupa ( map (\x -> 
                  agrupaPosR x) (map (\x -> map (\(y1, y2) -> 
                        (y1, fst y2, len) ) x)  ( groupBy (\x y -> 
                              (fst x) == (fst y)) (rampasT m)))))))
                 where len = length (filter (\x -> fst x == 0) m)

-- | filtra as peças que são rampas

rampasT :: [(Int, (Int, Peca))] 
        -> [(Int, (Int, Peca))]
rampasT = filter (\x -> eRampaChao (snd (snd x)))

-- | filtra o mapa e retira as rampas (desde inicio a fim)
semRampas :: [(Int, (Int, Peca))] 
          -> [(Int, (Int, Peca))] 
          -> [(Int, (Int, Peca))]
semRampas a b = filter (\x -> (length (intersect a [x]) < 1 ) ) b

-- | junta 'Instrucoes' seguidas que seja possível agrupar posteriormente
seqP :: Instrucoes 
     -> Instrucoes
seqP [] = []
seqP ( x : t ) = (elemP x t) ++ (seqP (notElemP x t))

-- | caso se verifique as condições definidas (haver interseção entre os bulldozers utilizados), junta-os
elemP :: Instrucao 
      -> Instrucoes 
      -> Instrucoes
elemP i [] = [i]
elemP i ( h : t ) | i == h && ( length (intersect (extraiB i) (extraiB h)) > 0) = h : elemP i t
                  | i /= h && ( length (intersect (extraiB i) (extraiB h)) > 0) = [i]
                  | otherwise = elemP i t

-- | caso se verifique as condições definidas (haver interseção entre os bulldozers utilizados), junta-os
-- | 
-- | As funções elemP e notElemP poderiam ter sido juntas numa span... A alterar posteriormente.

notElemP :: Instrucao 
         -> Instrucoes 
         -> Instrucoes
notElemP i [] = []
notElemP i ( h : t ) | i == h && ( length (intersect (extraiB i) (extraiB h)) > 0) = notElemP i t
                     | i /= h && ( length (intersect (extraiB i) (extraiB h)) > 0) = (h : t)
                     | otherwise = h : notElemP i t

-- | depois de processar todo o mapa, percorre esta função para agrupar 'Instrucoes' repetidas num 'Repete'
padraoHorizontalInst :: Instrucoes 
                     -> Instrucoes
padraoHorizontalInst = map (\x -> criaPadraoH x) . groupBy (\x y -> x == y) . seqP

-- | dadas 'Instrucoes', devolve uma 'Instrucao' 'Repete' que agrupa a lista
criaPadraoH :: Instrucoes 
            -> Instrucao
criaPadraoH i | length i > 1 = Repete ((length i)) [(i !! 0)]
              | otherwise = i !! 0

-- | cria a instrução repete

criaRepeteR :: [(Int, (Int, Peca))] 
            -> (Int, Instrucao)
criaRepeteR lp = (fst (snd (lp !! 0)) , Repete 1 (map (\x -> defineInstrucao [fst x] (snd (snd x))) lp ))

-- | agrupa peças iguais de forma consecutiva
-- |
-- | esta função deveria estar a procurar padrões... a definir posteriormente!!

agrupaPecasMapa :: [Peca] 
                -> [(Int, (Int, Peca))] 
                -> [(Int, (Int, Peca))]
agrupaPecasMapa [] m = []
agrupaPecasMapa (p : t) m = (filter (\x -> p == (snd (snd x))) m) ++ (agrupaPecasMapa t m)

-- | agrupa os bulldozers consecutivos

agrupaBzInstr :: [(Int, (Int, Peca))] 
              -> ([Int], (Int, Peca))
agrupaBzInstr [(x, ( y , z))] = ([x], (y, z))
agrupaBzInstr ( h1 : t ) = ([a] ++ z1 , (b, c))
                         where (a, (b, c)) = h1
                               (z1, (z2, z3)) = agrupaBzInstr t

-- | Recebe a posição inicial dos bz (tudo com zeros), uma lista vazia e o mapa.
-- |
-- | Vai percorrer o mapa e a cada instrução, atualiza a posição dos BZ e a última instrução dos mesmos.
-- |
-- | Devolve 'Instrucoes'

criaInstrOrg :: [(Int, Int)] -- ^ posição atual dos bz (identif bz, posição atual)
             -> [(Int, Instrucao)] -- ^ ultima instrucao dos bz (indentificador bz, instrucao)
             -> [(Int, Instrucao)] -- ^ mapa
             -> Instrucoes
             -- -> [([(Int, Int)], Instrucao)]
criaInstrOrg ib ui [] = []
criaInstrOrg ib ui ((i, inst ) : t) = telep ++ [inst] ++ (criaInstrOrg nib ultInstr t)
          where b = extraiB inst
                nib = if (eRepete inst) then update (i + ((length (instRepete inst)) - 1) ) ib b else update i ib b
                nibTele = updateTele ib telep
                posB = getPositionB b ib
                telep = checkPosition i posB
                telepC = map (\x -> (nibTele, x)) telep
                ultInstr = updateInstr ui inst b

-- | recebendo um triplo criado na função anterior (das rampas)
-- | vai criar uma lista de duplos que correspondem:
-- | (x, y), x € identificador do BZ e y € indice da rampa
          
agrupaPosR :: [(Int, Int, Int)] 
           -> [[(Int, Int)]]
agrupaPosR [] = []
agrupaPosR [(a1, a2, a3)] = [zipWith (,) (repeat a1) [a2 .. a3]]
agrupaPosR ( (a1, a2, a3) : (b1, b2, b3) : t) = zipWith (,) (repeat a1) [a2 .. b2] : agrupaPosR t

-- | vai filtrar todos os elementos do mapa com coordenadas que correspondem à coordenada definida

agrPorPosicao :: [(Int, Int)] -- ^ lista de coordenadas para ir buscar ao mapa
              -> [(Int, (Int, Peca))]  -- ^ Mapa
              -> [[(Int, (Int, Peca))]]
agrPorPosicao a b = map (\x -> filter (\y -> fst y == fst x && (fst (snd y)) == snd x ) b) a

-- | A cada instrução, é atualizada a posição dos bulldozers

update :: Int -- ^ posição de destino
       -> [(Int, Int)] -- ^ posição atual dos bz
       -> [Int] -- ^ lista de bz
       -> [(Int, Int)] -- ^ lista de posições atualizadas
update pd pa bz = map (\x -> if (length (intersect [fst x] bz)) > 0 then (fst x, pd) else x) pa

-- | Atualiza a posição dos bz quando existe a instrução teleporta 
updateTele :: [(Int, Int)] -- ^ posicao bz
           -> Instrucoes -- ^ instruções Teleporta
           -> [(Int, Int)] -- ^ posição bz atualizada
updateTele pa inst = somaParIgual pa bzs
                   where bz = map (\x -> (extraiB x, extraiM x)) inst
                         bzs = juntaAgrupa (map (\(x, z) -> zipWith (,) x (repeat z)) bz)

-- | Atualiza a lista da última instrução realizada pelo bulldozer

updateInstr :: [(Int, Instrucao)] 
            -> Instrucao 
            -> [Int] 
            -> [(Int, Instrucao)]
updateInstr [] inst b = map (\x -> (x, inst)) b
updateInstr ( (x, y) : uis ) inst b | elem x b = [(x, inst)] ++ updateInstr uis inst (delete x b)
                                    | otherwise = [(x, y)] ++ updateInstr uis inst b

-- | Dá um par que indica o índice do Bulldozer e a sua posição na pista
                                    
getPositionB :: [Int] -- ^ lista de bulldozers que vão trabalhar
             -> [(Int, Int)] -- ^ atual posição dos bulldozers
             -> [(Int, Int)] -- ^ atual posição dos bulldozers que vão trabalhar (bulldozer, posicao dele)
getPositionB i m = filter (\(a,b) -> elem a i) m

-- | Verifica se o Bulldozer pode efetuar a Jogada Teleporta 

checkPosition :: Int -- ^ próxima posição
              -> [(Int, Int)] -- ^ (bulldozer, posição atual)
              -> (Instrucoes)
checkPosition y [] = []
checkPosition y (h : t) | (y-1) == x = checkPosition y t
                        | otherwise  = [criaTeleporta x y (fst h)] ++ (checkPosition y t)
                        where x = snd h

-- | Efetua a Jogada Teleporte 
-- |
-- | Calcula a posição para a qual o Bulldozer se vai teleportar

criaTeleporta :: Int -- ^ posição atual
              -> Int -- ^ posição seguinte
              -> Int -- ^ bulldozer que vai trabalhar
              -> Instrucao
criaTeleporta x y z = Teleporta [z] yx
                    where dif = if (y-x) <= 0 then (1) else (-1)
                          yx = (y - 1) - x

-- | Recebe duas listas de pares e soma as segundas coordenadas quando as primeiras são iguais

somaParIgual :: [(Int, Int)] 
             -> [(Int, Int)] 
             -> [(Int,Int)]
somaParIgual a b = map (\x ->
       (fst x, (snd x) + (snd (if (length (filter (\y ->
             fst x == (fst y)) b) > 0) then (filter (\y ->
                   fst x == (fst y)) b !! 0) else (0,0))) ) ) a
-- somaParIgual (h : t) b = (fst h, snd (filter (\x -> fst x == fst h) b)) 

-- | Indica se a peça é uma Rampa 

eRampa :: (Int, Instrucao) -- ^ (Posição, Instrução)
       -> Bool
eRampa (a, (Sobe _ _ _)) = True
eRampa (a, (Desce _ _ _)) = True
eRampa (a, (Teleporta x y) )= False
eRampa (a, (Anda x y) )= False
eRampa (a, (Repete x y) )= False

-- | Indica se a Rampa começa ou acaba no chão

eRampaChao :: Peca 
           -> Bool
eRampaChao (Rampa p i 0) = True
eRampaChao (Rampa p 0 f) = True
eRampaChao _ = False

-- | Diminui o número de Repetes utilizados (menos instruções)

optRepete :: Instrucoes -- ^ Instruções inicias 
          -> Instrucoes -- ^ Instruções finais otimizadas 
optRepete [] = []
optRepete l = juntaAgrupa (map (\x -> juntarRepete x) l1)
            where l1 = agrupaRepete l
 
-- | Agrupa numa lista os repetes consecutivos
-- | 
-- | São agrupados caso tenham coeficientes iguais 
-- |
-- | É preciso que sejam de bulldozers diferentes

agrupaRepete :: Instrucoes 
             -> [Instrucoes] -- ^ Instruções com os repetes consecutivos agrupados    
agrupaRepete [] = []             
agrupaRepete l1 = groupBy (\x y ->
       (coefRepete x == coefRepete y) && (eRepete x) && (eRepete y) && (length(intersect (extraiB x) (extraiB y)) == 0)) l1
                   
-- | Indica o número de vezes que vai repetir as instruções 

coefRepete :: Instrucao  
           -> Int -- ^ Coeficiente do Repete
coefRepete (Repete x i) = x
coefRepete _ = 0

-- | Recebe a lista do agrupaRepete e junta os repetes 

juntarRepete :: Instrucoes -- ^ Instruções ja organizadas pela função agrupaRepete
             -> Instrucoes -- ^ Instruções otimizadas 
juntarRepete l1 | eRepete x = [Repete (coefRepete x) (juntaAgrupa (map (\x -> instRepete x) l1))]
                | otherwise = l1
                where x = (l1 !! 0)

