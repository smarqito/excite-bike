-- | Módulo que associa o clique nas teclas com acontecimentos no jogo
module Player_Control where

import LI11920
import Data
import Graphics.Gloss.Interface.Pure.Game
import Tarefa2_2019li1g177
import F_Aux
import System.IO.Unsafe
import Mapas

-- | Reage a um clique no teclado
-- 
-- Esta função define a interação do jogador com o jogo.
--
-- Dado um clique numa tecla, reage com algum acontecimento (caso essa tecla seja apropriada)
reageEvento :: Event -- ^ evento -> clique numa tecla
 -> EstadoJ -- ^ 'EstadoJ' anterior
 -> EstadoJ -- ^ 'Estadoj' após a interação com o 'Jogador'

reageEvento (EventKey k Down _ _) ej@(e, mj, cr, local) = case local of
  Menu Home -> reageHome k ej
  Menu (NumeroJogadores _) -> reageNumeroJogadores k ej
  Menu (NomeJogadores _ _ _) -> reageNomeJogadores k ej
  Menu (NumeroBots _ _) -> reageNumeroBots k ej
  Menu (EscolheMapa names bots mapa) -> reageEscolheMapa k ej
  Game Play -> reageGamePlay k ej
  _ -> ej

-- 
reageEvento (EventKey k Up _ _) eg@(e, mj, cr, local@(Game Play)) = case k of
  -- @ 'Jogador' 0
  Char ',' -> moveN 0 Acelera
  -- @ 'Jogador' 1
  Char 'e' -> moveN 1 Acelera
  -- @ 'Jogador' 2
  Char 'y' -> moveN 2 Acelera
  -- @ 'Jogador' 3
  Char 'o' -> moveN 3 Acelera
  _ -> eg
  where moveN :: Int -> Jogada -> EstadoJ
        moveN n j = if ((length mj) - 1 ) < n then eg
                    else if jTerm jogaHuman || (isBot jogaHuman) then eg -- se tiver perdido / chegado ao fim / seja um Bot, não faz nada
                    else (jogada n j e, updateMB n (Just Desacelera) mj, cr, local)   
                    where jogaHuman = (map sndT mj) !! n -- @ recupera o tipo do n 'Jogador' -> 'Joga'

reageEvento (EventKey k Up _ _) eg@(e, mj, cr, local@(Menu Score)) = case k of
  SpecialKey KeyEnter -> (e, mj, cr, Menu Home)
  _ -> eg

reageEvento (EventKey k Up _ _) eg@(e, mj, cr, local@(Menu AboutUs)) = case k of
  SpecialKey KeyEnter -> (e, mj, cr, Menu Home)
  SpecialKey KeySpace -> (e, mj, cr, Menu Home)
  _ -> eg

reageEvento _ e = e

-- | Permite interagir com o menú Home
reageHome :: Key
 -> EstadoJ
 -> EstadoJ
reageHome k ej@(e,mj,cr,local) = case k of
  Char 'a' -> (e,mj,cr, Menu AboutUs)
  SpecialKey KeyEnter -> (e, mj, cr, Menu (NumeroJogadores 1))
  _ -> ej

-- | Interação com o menú em que se escolhe o número de 'Jogador's humanos
reageNumeroJogadores :: Key
 -> EstadoJ
 -> EstadoJ
reageNumeroJogadores k ej@(e,mj,cr,local@(Menu (NumeroJogadores x))) = case k of
  Char '1' -> (e, mj, cr, Menu $ NomeJogadores 1 [] [])
  Char '2' -> (e, mj, cr, Menu $ NomeJogadores 2 [] [])
  Char '3' -> (e, mj, cr, Menu $ NomeJogadores 3 [] [])
  Char '4' -> (e, mj, cr, Menu $ NomeJogadores 4 [] [])
  SpecialKey KeyUp -> (e, mj, cr, Menu $ NumeroJogadores (max (pred x) 1))
  SpecialKey KeyDown -> (e, mj, cr, Menu $ NumeroJogadores (min (succ x) 4))
  SpecialKey KeyEnter -> (e, mj, cr, Menu $ NomeJogadores x [] [])
  _ -> ej

-- | Interação com o menú em que se escolhe o nome dos 'Jogador's humanos
reageNomeJogadores :: Key
 -> EstadoJ
 -> EstadoJ
reageNomeJogadores k ej@(e,mj,cr,local@( Menu ( NomeJogadores nj input names) ) ) = case k of
  Char c -> (e, mj, cr, Menu ( NomeJogadores nj inputMax names ) )
    where inputMax = if (length input) < 14 then input ++ [c]
                     else input
  SpecialKey KeyDelete -> (e, mj, cr, Menu (NomeJogadores nj (deleteLast input) names))
  SpecialKey KeyEnter -> (e, mj, cr,  Menu goTo)
    where goTo = if (nj - 1) == 0 then NumeroBots updatedNames 0
                 else NomeJogadores (nj - 1) [] updatedNames
          updatedNames = input : names
  _ -> ej

-- | Interação com o menú em que se escolhe o número de bots contra se quer jogar.
-- ! O limite de jogadores, incluindo bots foi definido para 4
reageNumeroBots :: Key
 -> EstadoJ
 -> EstadoJ
reageNumeroBots k ej@(e,mj,cr,local@( Menu ( NumeroBots names x))) = case k of
  Char c -> if c >= '0' && c <= '3' && convert <= (4 - ln) then (e, mj, cr, Menu (EscolheMapa names convert 0) )
            else ej
    where ln = length names
          convert = (fromEnum c) - 48
  SpecialKey KeyUp -> (e, mj, cr, Menu (NumeroBots names (max (pred x) 0)))
  SpecialKey KeyDown -> (e, mj, cr, Menu (NumeroBots names (min (succ x) (4 - (length names)))))
  SpecialKey KeyEnter -> if (length names) == 4 then (e, mj, cr, Menu (EscolheMapa names 0 0))
                         else (e, mj, cr, Menu (EscolheMapa names x 0))
  _ -> ej

-- | Interação com o menú em que se escolhe o 'Mapa' do jogo
reageEscolheMapa :: Key
 -> EstadoJ
 -> EstadoJ
reageEscolheMapa k ej@(e, mj, cr, Menu (EscolheMapa names bots mapa)) = case k of
  SpecialKey KeyLeft -> (e, mj, cr, Menu (EscolheMapa names bots (max (mapa - 1) 0)))
  SpecialKey KeyRight -> (e, mj, cr, Menu (EscolheMapa names bots (min (mapa + 1) 4)))
  SpecialKey KeyEnter -> (Estado m j, mjUpdate, cr, Game (Countdown cc) )
  _ -> ej
  where startCC = 1 :: Float
        endCC = 3 :: Float
        cc = unsafeDupablePerformIO $ countdown (startCC, endCC)
        j = jogadoresN $ (length names) + bots
        m = mapas !! mapa
        mjUpdate = createMJ 0 names bots

-- | Interação com o jogo
reageGamePlay :: Key
 -> EstadoJ
 -> EstadoJ
reageGamePlay k ej@(e, mj, cr, local) = case k of
  -- @ 'Jogador 0'
  SpecialKey KeyUp -> moveN 0 (Movimenta C)
  SpecialKey KeyDown -> moveN 0 (Movimenta B)
  SpecialKey KeyLeft -> moveN 0 (Movimenta E)
  SpecialKey KeyRight -> moveN 0 (Movimenta D)
  Char '.' -> moveN 0 Dispara
  Char ',' -> moveN 0 Acelera
  -- @ Jogador 1
  Char 'w' -> moveN 1 (Movimenta C)
  Char 's' -> moveN 1 (Movimenta B)
  Char 'a' -> moveN 1 (Movimenta E)
  Char 'd' -> moveN 1 (Movimenta D)
  Char 'q' -> moveN 1 Dispara
  Char 'e' -> moveN 1 Acelera
  
  -- @ Jogador 2
  Char 't' -> moveN 2 (Movimenta C)
  Char 'g' -> moveN 2 (Movimenta B)
  Char 'f' -> moveN 2 (Movimenta E)
  Char 'h' -> moveN 2 (Movimenta D)
  Char 'y' -> moveN 2 (Acelera)
  Char 'r' -> moveN 2 (Dispara)
  
  -- @ Jogador 4
  Char 'i' -> moveN 3 (Movimenta C)
  Char 'k' -> moveN 3 (Movimenta B)
  Char 'j' -> moveN 3 (Movimenta E)
  Char 'l' -> moveN 3 (Movimenta D)
  Char 'o' -> moveN 3 (Acelera)
  Char 'u' -> moveN 3 (Dispara)
  _ -> ej
  where moveN :: Int -> Jogada -> EstadoJ
        moveN n j = if ((length mj) - 1 ) < n then ej
                    else if jTerm jogaHuman || (isBot jogaHuman) then ej -- @ se tiver perdido / chegado ao fim / seja um Bot, não faz nada
                    else case j of 
                      Acelera -> (jogada n j e, updateMB n (Just j) mj, cr, local)
                      _ -> (jogada n j e, mj, cr, local) -- (jogada n j e, updateMB n Nothing mj, cr, local)
                    where jogaHuman = (map sndT mj) !! n -- @ recupera o tipo do n 'Jogador' -> 'Joga'

-- | Faz update à lista de mj@(Int, Joga)

updateMB :: Int -- ^ identificador do 'Jogador'
 -> Maybe Jogada -- ^ 'Jogada' que o 'Jogador' efetuou
 -> PropJogadores -- ^ lista prévia de mj
 -> PropJogadores -- ^ lista de mj atualizada
updateMB n u [] = []
updateMB n u (h : t) | n == x = (x, Player u, z, w) : t
                     | otherwise = h : updateMB n u t
                     where (x,y,z,w) = h

-- | Apaga o último Char de uma String
deleteLast :: String
 -> String
deleteLast [] = []
deleteLast [x] = []
deleteLast (h : t) = h : deleteLast t

-- | Cria uma lista de propriedades de 'Jogador's para iniciar o jogo.
createMJ :: Int
 -> [String]
 -> Int
 -> PropJogadores
createMJ n [] 0 = []
createMJ n (h : t) b = (n , Player Nothing, h, 0) : createMJ (succ n) t b
createMJ n [] b = (n, Bot, botName, 0) : createMJ (succ n) [] (pred b)
  where botname = unsafeDupablePerformIO $ countdown (0, 11)
        botName = botsNames !! botname