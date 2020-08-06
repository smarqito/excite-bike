-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
-- = Relatório Tarefa 5
-- A realização da tarefa 5 compreendeu o desenvolvimento de toda a componente gráfica do jogo e a interatividade entre o utilizador e o jogo.
--
-- Houve algumas dificuldades iniciais que se prenderam com a compreensão do /Gloss/ e a construção da dinâmica do jogo.
-- As mesmas foram ultrapassadas com o apoio do Professor Tiago e a matéria lecionada na disciplina de Programação Funcional.
--
-- Por forma a melhorar a organização e qualidade do código, optou-se por utilizar uma segmentação modular entre as várias componentes do jogo.
--
-- Optou-se por dividir a tarefa, de uma forma /major/, em duas partes. A primeira, relacionada com a dinâmica do jogo e a segunda, com o estado gloss.
--
-- == Componente gráfica
-- A componentes gráfica está, em grande parte, definida na função:
--
-- @ reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss @
-- 
-- Posteriormente, as funções auxiliares utilizadas nesta função, podem ser encontradas no módulo __/Desenho/__.
--
-- Por forma a identificar o local em que o 'Jogador' se encontra, criou-se um novo tipo de dados designado 'Local'.
-- Este /data type/ permite armazenar qual o menú em que o jogador se encontra. Sendo que pode alternar entre:
--
-- @ data Local = Menu TipoMenu | Game Start @
--
-- e o TipoMenu é:
--
-- @ data TipoMenu = Home | NumeroJogadores Int | NomeJogadores Int String [String] | NumeroBots [String] Int | EscolheMapa [String] Int Int | Score | AboutUs @
--
-- Assim, com base no local, a função reageEventoGloss constrói o grafismo relacionado (ver módulo /Desenho/).
--
-- Focando no menu Game, para construir os jogadores, utilizou-se a função:
--
-- @ constroiJogador :: Estado -> [Picture] -> [Float] -> Double -> PropJogadores -> [Picture] @
--
-- É importante referir que o jogo está limitado a um máximo de quatro 'Jogador'es.
-- A sua construção é feita sendo definido no módulo /Globals/ a variável 'tPeca' e 'tPecaY' que definem a relação x:janela
-- i.e. estabelece a relação entre cada valor de x (distância) e a proporção da janela.
-- 
-- Assim, a função constroi jogador percorre a lista de jogadores ao mesmo tempo que percorre a lista de imagens e 
-- constrói uma mota para cada jogador, posicionando-o no local adequado na janela.
-- Os jogadores humanos são construídos em função do que se encontra em última posição.
--
-- O mapa foi construído de forma semelhante. Utilizou-se o valor da distância do último 'Jogador' humano e para fazer movimentar o mapa.
--
-- Para o cálculo das distâncias dos jogadores e dua sua relação entre si, definiu-se a /instância Ord/ para o /data type/ 'Jogador'.
-- Esta definição permitiu uma melhor qualidade de código.
--
-- == Dinâmica do Jogo
-- Para construir a dinâmica do jogo foi necessário, em primeiro, estruturar a relação entre o jogador e o jogo.
-- Assim, foi necessário criar alguns /tipos/ que permitiram estabelecer tal relação:
--
-- @ type EstadoGloss = (EstadoJ,[Picture]) @ 
--
-- Através deste tipo, foi possível construir as 'Picture's que permitem ao jogador interagir com o jogo.
--
-- @ type EstadoJ = (Estado,PropJogadores, Float, Local) @
-- 
-- Com o tipo 'EstadoJ', é possível saber o 'Estado' do jogo, quais são os 'Jogador's que se encontram no jogo e as respetivas propriedades
-- (/identificador, 'Joga', Nome do jogador, Aquecimento do motor/).
--
-- A partir destes tipos, a função desenhaEstadoGloss utiliza a função adequada para construir o grafismo.
--
-- A relação entre o jogador e o jogo foi estabelecida no módulo 'Player_Control'.
--
-- === Controlo do Jogo (Player_Control)
-- Este módulo define a relação entre o pressionar das teclas e o mundo do jogo.
--
--  __Menu Home__
--
-- +---------+-----------------------------+
-- | Tecla   | Acontecimento               |
-- +=========+=============================+
-- | a       | Abre menu sobre os autores  |
-- +---------+-----------------------------+
-- | Enter   | Passa ao menu seguinte      |
-- +---------+-----------------------------+
--
--
-- __Menu Numero de Jogadores__
--
-- +---------+-----------------------------+
-- | Tecla   | Acontecimento               |
-- +=========+=============================+
-- |  __1__  | Escolhe 1 jogador           |
-- +---------+-----------------------------+
-- |  __2__  | Escolhe 2 jogador           |
-- +---------+-----------------------------+
-- |  __3__  | Escolhe 3 jogador           |
-- +---------+-----------------------------+
-- |  __4__  | Escolhe 4 jogador           |
-- +---------+-----------------------------+
-- |  KeyUp  | Sobe no menu                |
-- +---------+-----------------------------+
-- | KeyDown | Desce no menu               |
-- +---------+-----------------------------+
-- |  Enter  | Escolhe o numero selecionado|
-- +---------+-----------------------------+
--
-- __Menu Nome Jogadores__
--
-- +---------+-----------------------------+
-- | Tecla   | Acontecimento               |
-- +=========+=============================+
-- |  Char   | Escreve o nome do jogador   |
-- +---------+-----------------------------+
-- |  Delete | Apaga o ultimo Char inserido|
-- +---------+-----------------------------+
-- |  Enter  | Insere o nome do jogador.   |
-- |         | Passa ao seguinte ou        |
-- |         |  muda menu                  |
-- +---------+-----------------------------+
--
-- __enu Numero Bots__
--
-- +---------+-----------------------------+
-- | Tecla   | Acontecimento               |
-- +=========+=============================+
-- |  __0__  | Escolhe 0 bot               |
-- +---------+-----------------------------+
-- |  __1__  | Escolhe 1 bots              |
-- +---------+-----------------------------+
-- |  __2__  | Escolhe 2 bots              |
-- +---------+-----------------------------+
-- |  __3__  | Escolhe 3 bots              |
-- +---------+-----------------------------+
-- |  KeyUp  | Sobe no menu                |
-- +---------+-----------------------------+
-- | KeyDown | Desce no menu               |
-- +---------+-----------------------------+
-- |  Enter  | Escolhe o numero selecionado|
-- +---------+-----------------------------+
--
-- __Menu Escolhe Mapa__
--
-- +---------+-----------------------------+
-- | Tecla   | Acontecimento               |
-- +=========+=============================+
-- | KeyRight| Muda para o mapa da direita |
-- +---------+-----------------------------+
-- | KeyLeft | Muda para o mapa da esquerda|
-- +---------+-----------------------------+
-- |  Enter  | Seleciona o mapa            |
-- +---------+-----------------------------+
--
-- __Reage ao modo jogo__
--
-- +---------+-----------------------------+
-- | Tecla   | Acontecimento               |
-- +=========+=============================+
-- | KeyUp   | Player 1 Move Cima          |
-- +---------+-----------------------------+
-- | KeyDown | Player 1 Move Baixo         |
-- +---------+-----------------------------+
-- | KeyLeft | Player 1 Move Esquerda      |
-- +---------+-----------------------------+
-- | keyRight| Player 1 Move Direita       |
-- +---------+-----------------------------+
-- | __,__   | Player 1 Acelera            |
-- +---------+-----------------------------+
-- | __.__   | Player 1 Dispara            |
-- +---------+-----------------------------+
-- | __w__   | Player 2 Move Cima          |
-- +---------+-----------------------------+
-- | __s__   | Player 2 Move Baixo         |
-- +---------+-----------------------------+
-- | __a__   | Player 2 Move Esquerda      |
-- +---------+-----------------------------+
-- | __d__   | Player 2 Move Direita       |
-- +---------+-----------------------------+
-- | __q__   | Player 2 Acelera            |
-- +---------+-----------------------------+
-- | __e__   | Player 2 Dispara            |
-- +---------+-----------------------------+
-- | __t__   | Player 3 Move Cima          |
-- +---------+-----------------------------+
-- | __g__   | Player 3 Move Baixo         |
-- +---------+-----------------------------+
-- | __f__   | Player 3 Move Esquerda      |
-- +---------+-----------------------------+
-- | __h__   | Player 3 Move Direita       |
-- +---------+-----------------------------+
-- | __r__   | Player 3 Acelera            |
-- +---------+-----------------------------+
-- | __y__   | Player 3 Dispara            |
-- +---------+-----------------------------+
-- | __i__   | Player 3 Move Cima          |
-- +---------+-----------------------------+
-- | __k__   | Player 3 Move Baixo         |
-- +---------+-----------------------------+
-- | __j__   | Player 3 Move Esquerda      |
-- +---------+-----------------------------+
-- | __l__   | Player 3 Move Direita       |
-- +---------+-----------------------------+
-- | __u__   | Player 3 Acelera            |
-- +---------+-----------------------------+
-- | __o__   | Player 3 Dispara            |
-- +---------+-----------------------------+
--
-- == Globals
-- Neste módulo são armazenadas variáveis globais, como o framerate, resistencia do ar, aceleração da gravidade, time out quando o jogador morre
-- altura e largura da janela, tamanho das 'Pecas', os atritos, etc.
--
-- == F_Aux
-- Este módulo foi criado para armazenar funções auxiliares que sejam de uso, na grande maioria, em mais do que uma tarefa. Ou, por outro lado
-- que sejam função básicas, aplicadas sobre os 'Jogador'es, 'Peca', etc.


module Main where

import LI11920
import Data
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa0_2019li1g177
import Tarefa2_2019li1g177
import Tarefa4_2019li1g177
import Tarefa6_2019li1g177
import Data.Maybe
import Data.List
import F_Aux
import Player_Control
import GameMenu
import Globals

-- | Propriedades da Janela do jogo
dm :: Display
dm = InWindow "Excite Me" (width,heigth) (0,0)

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

main :: IO ()
main =  do 
    b1  <- loadBMP "./assets/images/bike1.bmp"
    b2  <- loadBMP "./assets/images/bike2.bmp"
    b3  <- loadBMP "./assets/images/bike3.bmp"
    b4  <- loadBMP "./assets/images/bike4.bmp"
    bg0 <- loadBMP "./assets/images/bg_home.bmp"
    bg1 <- loadBMP "./assets/images/bg_game.bmp"
    m   <- loadBMP "./assets/autores/aboutus.bmp"
    m1  <- loadBMP "./assets/images/mapa1.bmp"
    m2  <- loadBMP "./assets/images/mapa2.bmp"
    m3  <- loadBMP "./assets/images/mapa3.bmp"
    m4  <- loadBMP "./assets/images/mapa4.bmp"
    m5  <- loadBMP "./assets/images/mapa5.bmp"
    m6  <- loadBMP "./assets/images/mapa6.bmp"
    play dm                                                      -- janela onde irá correr o jogo
         (greyN 0.5)                                             -- côr do fundo da janela
         fr                                                      -- frame rate
         (estadoGlossInicial [b1, b2, b3, b4, bg0, bg1, m, m1, m2, m3, m4, m5, m6])      -- estado inicial
         desenhaEstadoGloss                                      -- desenha o estado do jogo
         reageEventoGloss                                        -- reage a um evento
         reageTempoGloss                                         -- reage ao passar do tempo

-- * Construção do EstadoGloss inicial

-- | Definição do 'EstadoJ' inicial.
--
-- Consiste no Estado da tarefa 2, juntamente com um Maybe Jogada. Este Maybe Jogada vai permitir manter um botão pressionado e o jogo reagir!
--
-- O estado inicial é uma lista de Nothing's. I.e. Nenhum movimento para os 'Jogador's.

estadoInicial :: EstadoJ -- ^ 'EstadoJ' inicial
estadoInicial = (Estado [] [], [] , 0, Menu Home)

-- | Constroi a posição das pistas no eixo dos Y
linear :: Display -- ^ recebe a informação sobre o display
 -> Int -- ^ recebe o número de pistas
 -> [Float] -- ^ @ retorna a posição das pistas
linear (InWindow _ (w, h) _) n = [60,0,-60,-120,-180] --[50,0,-50, -100] -- linearAux n cP tP

-- * Dinâmica do Jogo

-- | Reage ao tempo utilizando a função passo da tarefa 4 aplicada a cada 'Jogador'.
--
-- ! A função é aplicada a cada 'Jogador' após ser atualizado o seu estado

reageTempo :: Float -- ^ tempo
 -> EstadoJ -- ^ 'EstadoJ' prévio
 -> EstadoJ -- ^ Novo 'Estado' atualizado após a função passo seguida da reageJogada
reageTempo _ (e, mj, cr, menu@(Menu _)) = (e, mj, cr, menu)

reageTempo n (e, mj, cr, Game (Countdown x) ) | x <= 0 = (e, mj, cr, Game Play)
                                                      | otherwise = (e, mj, cr, Game ( Countdown (x - n) ) )

reageTempo n ej@(e@(Estado m j), mj, cr, local) = 
    (fst apPasso, snd apPasso, crU, newLocal)
    where eUpdate = reageJogada n (Estado m j) mj
          apPasso = reagePasso n cr eUpdate mj maxP
          maxP = distJ (if (length humanP) > 0 then maximum humanP else maximum j)
          humanP = getHumanPlayer j mj
          (crU, newLocal) = if (length (filter (\x -> jTerm (sndT x) ) mj)) == (length mj) then (0, Menu Score) else (cr+n, local)


-- | Aplicação da tarefa 4 a todos os 'Jogador's após a aplicação da tarefa 2
reagePasso :: Float -- ^ tempo
 -> Float -- ^ cronómetro
 -> Estado -- ^ anterior do jogo
 -> PropJogadores -- ^ lista de 'Joga' e nomes dos 'Jogador's
 -> Double -- ^ distância do 'Jogador' que está em primeiro lugar @fp
 -> (Estado, PropJogadores)
reagePasso t cr e@(Estado m []) [] fp = (Estado m [], [])
reagePasso t cr e@(Estado m (j : js)) (mj:mjs) fp = 
    if jTerm joga then (Estado m (j: jR), mj : mjR)
    else (Estado m (finalP: jR) , (n,mjU,name,nMotor) : mjR) 
    where (n,joga,name,motor) = mj
          nMotor = aquecimentoMotor j t motor
          apPasso = passo (realToFrac t) m j
          finalP = if (isJust $ getFim mjU) then jogadorMeta apPasso
                   else apPasso
          mjU = atualizaJoga joga apPasso fp (t,cr) m
          (Estado um jR, mjR) = reagePasso t cr (Estado m js) mjs fp

-- | dada a lista de 'Joga', efetua uma Maybe Jogada e atualiza o estado do 'Jogo'
-- 
-- Se o 'Jogador' tiver o motor sobreaquecido, fica com 'EstadoJogador' == 'Desacelera'
--
-- Se o 'Jogador' não tiver nenhuma 'Jogada', devolve o 'Estado' igual

reageJogada :: Float -- ^ tempo 
 -> Estado           -- ^ anterior do Jogo
 -> PropJogadores    -- ^ lista de 'Joga' e nomes dos 'Jogador's
 -> Estado           -- ^ novo 'Estado' do jogo
reageJogada t e [] = e
reageJogada t e@(Estado m j) (mj:mjs) = reageJogada t (Estado um uj) mjs
    where (n, joga, _,motor) = mj
          nMotor = aquecimentoMotor (j!!n) t motor
          Estado um uj = if sobreAqueceu nMotor then jogada n Desacelera e 
                         else case joga of 
                            Player x -> if isJust x  then jogada n (fromJust x) e else e
                            Bot -> if isJust x then jogada n (fromJust x) e else e
                                where x = bot n e
                            _ -> e

-- | Define o estado joga
-- Utilizando a distância do 'Jogador' e o comprimento de cada 'Pista', define o 'Joga' associado ao 'Jogador'
atualizaJoga :: Joga -- ^ Recebe o 'Joga' a ser atualizado
 -> Jogador -- ^ recebe o 'Jogador'
 -> Double -- ^ distância do 'Jogador' que se encontra em primeiro
 -> (Float, Float) -- ^ (frameRate, cronómetro)
 -> Mapa -- ^ 'Mapa' do jogo
 -> Joga
atualizaJoga joga j fp (t, cr) m =
    if dJAtual >= (fromIntegral $ length $ m !! 0) then Fim (cr + t)
    else if fp > (dJAtual + 8) then Perdeu
    else joga
    where dJAtual = distJ j

-- | Verifica se o motor sobreaqueceu.
sobreAqueceu :: Float  -- ^ valor de aquecimento do motor do jogador
 -> Bool -- ^ se o valor for superior a 50 retorna > True
sobreAqueceu m = m > 50

-- | se estiver a acelerar, aumenta o aquecimento. caso contrário diminui
aquecimentoMotor :: Jogador -- ^ jogador
 -> Float -- ^ frameRate
 -> Float -- ^ valor do motor
 -> Float -- ^ novo valor do motor
aquecimentoMotor j t motor = 
    if ej == Chao True then motor + varMot
    else max 0 $ motor - varMot
    where ej = estadoJ j
          varMot = t * 5

-- | Quando um 'Jogador' chega ao fim, coloca-o em cima da meta
jogadorMeta :: Jogador -- ^ que já chegou ao 'Fim'
 -> Jogador -- ^ em cima da meta
jogadorMeta j@(Jogador p d v c e) = Jogador p (d+0.5) 0 c (Ar 0 0 0)

-- | Dada uma lista de jogadores e uma lista de propriedades de jogadores, retorna a lista de apenas humanos
getHumanPlayer :: [Jogador] -- ^ lista de jogadores
 -> PropJogadores -- ^ lista de propriedades dos jogadores
 -> [Jogador] -- ^ listsa de jogadores humanos
getHumanPlayer (j:js) ((_,Player _,_,_):mjs) = j : getHumanPlayer js mjs
getHumanPlayer (_:js) (_:mjs) = getHumanPlayer js mjs
getHumanPlayer _ _ = []

-- * Estado Gloss

-- | Define o 'EstadoGloss' inicial do jogo.

estadoGlossInicial :: [Picture] -- ^ Lista de 'Picture' utilizada para os gráficos
 -> EstadoGloss -- ^ 'EstadoGloss' inicial
estadoGlossInicial l = (estadoInicial,l)

-- | Desenho da componente gráfica do 'Jogo'
--
-- Utiliza o 'EstadoGloss' para construir o grafismo do Jogo

desenhaEstadoGloss :: EstadoGloss -- ^ 'EstadoGloss' a ser desenhado
 -> Picture -- ^ lista de 'Picture' com o 'Mapa', 'Jogadores' e 'Municao' dos 'Jogadores'
desenhaEstadoGloss eg@((e@(Estado m j), mj, cr, menu), images) = case menu of
    Menu Home -> menuHome images
    Menu (NumeroJogadores s) -> menuEscolheJogadores images s
    Menu (NomeJogadores nj input names ) -> menuNomeJogadores nj input names images
    Menu (NumeroBots names s) -> menuNumeroBots names images s
    Menu (EscolheMapa names nBots mapa) -> menuEscolheMapa mapa images
    Game _ -> menuJogo eg py
        where py = reverse (ordena (linear dm (length m)))
    Menu Score -> menuScore mj images
    Menu AboutUs -> menuAboutUs images



-- | Reação do 'EstadoGloss' a um evento
-- 
-- vai atualizar o 'EstadoGloss' e lista de 'Maybe Jogada' (caso se aplique)
--
-- ? por exemplo teclas definidas no módulo Player_Control

reageEventoGloss :: Event -- ^ 'Event' do teclado / rato
 -> EstadoGloss -- ^ 'EstadoGloss' prévio à reação do evento
 -> EstadoGloss -- ^ 'EstadoGloss' após a reação do evento
reageEventoGloss ev (e,images) = (reageEvento ev e,images)

-- | Reação do estado do jogo (estado gloss) à passagem do tempo

reageTempoGloss :: Float -- ^ tempo
 -> EstadoGloss -- ^ 'EstadoGloss' prévio à passagem do tempo
 -> EstadoGloss -- ^ 'EstadoGloss' após passagem do tempo
reageTempoGloss t ((e, mj, cr, local),images) = (reageTempo t (e, mj, cr, local),images)