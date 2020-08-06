-- | 
module GameMenu where
import Graphics.Gloss.Interface.Pure.Game
import LI11920
import Data
import F_Aux
import Desenho
import Globals
import Data.Char
import Data.List
import Data.Maybe

-- * Home

-- | Função para construir o menú Home
menuHome :: [Picture]
    -> Picture
menuHome images = Pictures [Scale 0.67 0.67 (images !! 4)]

-- | ajusta o fundo dos menús - excet o menú Home
scaleBg :: [Picture]
 -> Picture
scaleBg p = Scale 0.67 0.67 (p !! 5)

-- * Menú para escolher o número de 'Jogador's

-- | Função para construir o menú que permite escolher o número de 'Jogador's
--
-- ! ter em atenção que o jogo apenas permite 4 jogadores em simultâneo
menuEscolheJogadores :: [Picture]
 -> Int
 -> Picture
menuEscolheJogadores images s = Pictures $ [scaleBg images] ++ [njog] ++ opc
    where njog = Color corTexto $ Translate (-650) 200 (Scale 0.2 0.2 $ Text "Escolha o numero de jogadores")
          opc = take 4 $ criaOpcoes 4 (-450, -100) s
 
-- * Menú para escolher o nome dos 'Jogador's

-- | Função para construir o menú para escrever o nome dos 'Jogador's
menuNomeJogadores :: Int -- ^ identificador do número de 'Jogador's
 -> String -- ^ input que o utilizador está a fazer
 -> [String] -- ^ nome dos utilizadores já escolhidos
 -> [Picture] -- ^ imagens
 -> Picture -- ^ gráfico
menuNomeJogadores n input names images = Pictures $ [scaleBg images, titulo, jogador, inputPicture] ++ selecionados
    where titulo = Color corTexto $ Translate (-650) 200 (Scale 0.2 0.2 (Text "Escolha o nome para os jogadores!"))
          jogador = Color corTexto $ Translate (-620) 100 (Scale 0.2 0.2 (Text $ "Jogador " ++ [ intToDigit (1 + (length names) )] ++ ": "))
          inputPicture = Color corTexto $ Translate (-450) 100 (Scale 0.2 0.2 (Text input))
          selecionados = if (length names) > 0 then listaNomes (reverse names) 0 (400, 200) else []

-- * Menú para escolher o número de 'Bot's em jogo

-- | Função para construir o menú para selecionar contra quantos bots quer jogar
--
-- ! ter em atenção que o jogo apenas permite 4 jogadores em simultâneo
menuNumeroBots :: [String]
 -> [Picture]
 -> Int
 -> Picture
menuNumeroBots names images s = if ln == 4 then Pictures [scaleBg images, noOp, noOp2]
                              else Pictures $ (scaleBg images):titulo:op
    where ln = length names
          op = criaOpcoes (maxP - ln) (-450,-100) s
          noOp = Color corTexto $ Translate (-550) (150) (Scale 0.35 0.35 (Text "Ja escolheu o numero maximo de jogadores."))
          noOp2 = Color corTexto $ Translate (-360) (50) (Scale 0.35 0.35 (Text "Pressione Enter para continuar"))
          titulo = Color corTexto $ Translate (-650) (200) (Scale 0.35 0.35 (Text "Quer jogar contra quantos bot's ?"))
          maxP = 4

-- * Menú para escolhar o 'Mapa' do jogo!

-- | Função para construir o menú para escolher o Mapa

menuEscolheMapa :: Int -- ^ identificador do número do 'Mapa'
 -> [Picture]
 -> Picture
menuEscolheMapa n images = Pictures [scaleBg images, titulo, sMap]
    where sMap = Translate 0 0 (Scale 0.9 0.8 (i !! n))
          titulo = Color corTexto $ Translate (-200) (-400) ( Scale 0.3 0.3 $ Text "Escolha o mapa")
          (_, i) = splitAt 7 images

-- * Desenha o 'Jogo'

-- | Desenho do jogo
menuJogo :: EstadoGloss
    -> [Float]
    -> Picture
menuJogo ((e@(Estado m j), mj, cr, local),images) py =
    Pictures (mapa ++ js ++ contaM ++ [conta] ++ termText ++ [semaforo] ++ motor ++ posicaoPlayersA)
    where ppx = fromIntegral (div width 55)
          js = constroiJogador e images py uP mj
          mapa = constroiMapa m py j uP
          contaM = contadorMunicao j (posicX - 20) (-400) (length j) 1
          conta = contador cr (-600) (450)
          uP = lastPlayer 1000000 j mj -- posição do último 'Jogador'
          term =  filter (\x -> jTerm (sndT x)) mj
          termText = listaFimPlayer 0 (mj, []) (400, 300) black
          semaforo = criaSemaforo cr
          motor = estadoMotor mj (posicX, -320) (length j)
          posicX = (fromIntegral ((negate (div width 2)) + 150))
          posicaoPlayersA = rankingPlayers j mj

-- * Pontuação dos 'Jogador's no fim do 'Jogo'

-- | Função para desenhar o resultado dos 'Jogador's
menuScore :: PropJogadores
    -> [Picture]
    -> Picture
menuScore mj images = Pictures ([scaleBg images] ++ lFim ++ [titulo0, titulo1, titulo2])
    where lFim = listaFimPlayer 0 (fimOrd, perdeu) (-300,100) corTexto
          titulo0 = Color corTexto $ Translate (-400) 150 (Scale 0.2 0.2 (Text "#"))
          titulo1 = Color corTexto $ Translate (-300) 150 (Scale 0.2 0.2 (Text "Jogadores"))
          titulo2 = Color corTexto $ Translate (200) 150 (Scale 0.2 0.2 (Text "Tempo"))
          goHome = Color corTexto $ Translate 200 (-300) (Scale 0.25 0.25 (Text "Press Enter to Play again"))
          (perdeu, fim) = spanFull jPerdeu mj -- separa os que perderam dos que chegaram ao fim
          fimOrd = ordenaM fim []


-- | Menú sobre nós
menuAboutUs :: [Picture]
 -> Picture
menuAboutUs images = Scale 0.67 0.67 (images !! 6)

-- * Auxiliares
-- | Para apagar
-- !!!!!
desenhaE :: Jogador -> [Picture]
desenhaE j@(Jogador p d v c e@(Ar h i g)) = 
    [Translate (-600) (-300) (Scale 0.18 0.18 (Text ( " Altur: " ++ (show h)) ) ),
     Translate (-600) (-320) (Scale 0.18 0.18 (Text ( " incli: " ++ (show i)) ) ),
     Translate (-600) (-340) (Scale 0.18 0.18 (Text ( " Gravi: " ++ (show g) ) ) ),
     Translate (-600) (-360) (Scale 0.18 0.18 (Text ( " Veloc: " ++ (show v) ) ) ),
     Translate (-600) (-380) (Scale 0.18 0.18 (Text ( " Dista: " ++ (show d) ) ) ) ]
desenhaE j@(Jogador p d v c e) = 
    [Translate (-600) (-300) (Scale 0.18 0.18 (Text ( " Estad: " ++ (show e)) ) ),
     Translate (-600) (-320) (Scale 0.18 0.18 (Text ( " " ) ) ),
     Translate (-600) (-340) (Scale 0.18 0.18 (Text ( " " ) ) ),
     Translate (-600) (-360) (Scale 0.18 0.18 (Text ( " Veloc: " ++ (show v) ) ) ),
     Translate (-600) (-380) (Scale 0.18 0.18 (Text ( " Dista: " ++ (show d) ) ) ) ]

-- | cria o número de opções
criaOpcoes :: Int
 -> (Float, Float)
 -> Int
 -> [Picture]
criaOpcoes n (x, y) s | n == s = (Color corTexto $ Translate x y (Scale 0.2 0.2 $ Text (">" ++ [intToDigit n]))) : criaOpcoes (n-1) (x, y+50) s
                      | n >= 0 = (Color corTexto $ Translate x y (Scale 0.2 0.2 $ Text (" "++[intToDigit n]))) : criaOpcoes (n-1) (x, y+50) s
                      | otherwise = []
    
-- | listagem de nomes
listaNomes :: [String]
 -> Int
 -> (Float, Float)
 -> [Picture]
listaNomes names 0 (x, y) = (Color corTexto $ Translate x y (Scale 0.2 0.2 (Text "Nomes Escolhidos"))) : listaNomes names 1 (x, y-100)
listaNomes [] n _ = []
listaNomes (h : t) n (x, y) = (Color corTexto $ Translate x y (Scale 0.2 0.2 (Text ("Jogador " ++ numJ ++ ": " ++ h)))) : listaNomes t (succ n) (x, y-50)
    where numJ = [intToDigit n]

-- | Cor do Texto para os menús
corTexto :: Color
corTexto = white

-- | Ordenar os jogadores que já terminaram
ordenaM :: PropJogadores -> PropJogadores -> PropJogadores
ordenaM [] t = t
ordenaM (p1:t) l = ordenaM t (insere p1 l)

-- | insere as propriedades do jogador de forma ordenada de 'Fim'
insere :: (Int,Joga,String,Float)
 -> PropJogadores
 -> PropJogadores
insere p1 [] = [p1]
insere p1@(x, Fim y1, z,_) (h@(x2, Fim y2, z2,_) : t) | comp == LT || comp == EQ = p1 : h : t
                                                      | otherwise = h : insere p1 t
    where comp = compare y1 y2

-- | Cria o ranking de jogadores durante o jogo
rankingPlayers :: [Jogador]
 -> PropJogadores
 -> [Picture]
rankingPlayers j mj = posicaoPlayers ordem (-600, 400) 1
    where zipj = zipWith (,) j mj
          ordena = sortBy (\(x,_) (y,_) -> compare x y ) zipj
          ordem = reverse ordena
