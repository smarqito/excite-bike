-- | Módulo que armazena propriedades do jogo (mapa e nomes para os bots)
module Mapas where
    import LI11920
    import Tarefa1_2019li1g177
    
    -- | Mapas disponíveis
    mapas :: [Mapa]
    mapas = [
            [
            [Recta Terra 0, Recta Relva 0, Rampa Boost 0 1, Rampa Lama 1 2, Recta Boost 2, Recta Terra 2, Recta Terra 2, Rampa Relva 2 3, Rampa Lama 3 0, Recta Terra 0, Recta Terra 0, Rampa Boost 0 5, Recta Terra 5],
            [Recta Terra 0, Recta Terra 0, Rampa Terra 0 1, Recta Boost 1, Recta Relva 1, Recta Boost 1, Recta Lama 1, Rampa Boost 1 2, Rampa Lama 2 0, Recta Relva 0, Recta Terra 0, Rampa Terra 0 3, Recta Terra 3],
            [Recta Terra 0, Recta Lama 0, Rampa Boost 0 1, Recta Terra 1, Recta Boost 1, Recta Boost 1, Recta Lama 1, Rampa Lama 1 2, Rampa Boost 2 0, Recta Relva 0, Recta Terra 0, Rampa Relva 0 2, Recta Terra 2],
            [Recta Terra 0, Recta Boost 0, Rampa Boost 0 1, Rampa Terra 1 0, Recta Lama 0, Recta Lama 0, Rampa Boost 0 1, Rampa Terra 1 2, Rampa Terra 2 0, Recta Terra 0, Recta Relva 0, Recta Lama 0, Recta Terra 0]
            ],
            [
            [Recta Terra 0, Rampa Terra 0 2, Rampa Relva 2 5, Recta Terra 5, Recta Relva 5, Recta Boost 5, Recta Boost 5, Rampa Lama 5 4, Recta Lama 4, Recta Lama 4, Recta Terra 4, Rampa Relva 4 2, Recta Relva 2, Recta Terra 2, Rampa Terra 2 4, Rampa Boost 4 5, Recta Lama 5],
            [Recta Terra 0, Rampa Boost 0 2, Rampa Lama 2 3, Recta Terra 3, Recta Boost 3, Recta Relva 3, Rampa Relva 3 4, Recta Terra 4, Recta Boost 4, Recta Boost 4, Recta Terra 4, Rampa Relva 4 3, Rampa Terra 3 1, Rampa Boost 1 2, Recta Terra 2, Recta Boost 2, Recta Lama 2],
            [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Recta Terra 1, Recta Boost 1, Recta Relva 1, Rampa Relva 1 0, Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Rampa Boost 1 2, Recta Lama 2, Recta Relva 2, Recta Lama 2],
            [Recta Terra 0, Rampa Lama 0 1, Recta Terra 1, Recta Relva 1, Recta Boost 1, Recta Relva 1, Rampa Relva 1 0, Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Terra 0, Rampa Relva 0 1, Recta Terra 1, Recta Boost 1, Recta Lama 1, Recta Lama 1, Rampa Terra 1 2]
            ],
            [
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Lama 0, Recta Boost 0]) ++ (concat $ replicate 10  [Rampa Terra 0 1, Recta Relva 1, Rampa Boost 1 4, Recta Lama 4, Recta Lama 4, Rampa Boost 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]),
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Boost 0, Recta Terra 0]) ++ (concat $ replicate 10 [Rampa Lama 0 1, Recta Terra 1, Rampa Boost 1 4, Recta Terra 4, Recta Terra 4, Rampa Relva 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]),
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Lama 0, Recta Relva 0]) ++ (concat $ replicate 10  [Rampa Boost 0 1, Recta Terra 1, Rampa Lama 1 4, Recta Boost 4, Recta Boost 4, Rampa Terra 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]),
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Terra 0, Recta Lama 0]) ++ (concat $ replicate 10  [Rampa Relva 0 1, Recta Relva 1, Rampa Terra 1 4, Recta Boost 4 , Recta Boost 4 , Rampa Boost 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0])
            ],
            [
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Lama 0, Recta Boost 0]) ++ (concat $ replicate 10  [Rampa Terra 0 1, Recta Relva 1, Rampa Boost 1 4, Recta Lama 4, Recta Lama 4, Rampa Boost 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]) ++ [Recta Terra 0, Rampa Terra 0 2, Rampa Relva 2 5, Recta Terra 5, Recta Relva 5, Recta Boost 5, Recta Boost 5, Rampa Lama 5 4, Recta Lama 4, Recta Lama 4, Recta Terra 4, Rampa Relva 4 2, Recta Relva 2, Recta Terra 2, Rampa Terra 2 4, Rampa Boost 4 5, Recta Lama 5],
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Boost 0, Recta Terra 0]) ++ (concat $ replicate 10 [Rampa Lama 0 1, Recta Terra 1, Rampa Boost 1 4, Recta Terra 4, Recta Terra 4, Rampa Relva 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]) ++ [Recta Terra 0, Rampa Boost 0 2, Rampa Lama 2 3, Recta Terra 3, Recta Boost 3, Recta Relva 3, Rampa Relva 3 4, Recta Terra 4, Recta Boost 4, Recta Boost 4, Recta Terra 4, Rampa Relva 4 3, Rampa Terra 3 1, Rampa Boost 1 2, Recta Terra 2, Recta Boost 2, Recta Lama 2],
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Lama 0, Recta Relva 0]) ++ (concat $ replicate 10  [Rampa Boost 0 1, Recta Terra 1, Rampa Lama 1 4, Recta Boost 4, Recta Boost 4, Rampa Terra 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]) ++ [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Recta Terra 1, Recta Boost 1, Recta Relva 1, Rampa Relva 1 0, Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Rampa Boost 1 2, Recta Lama 2, Recta Relva 2, Recta Lama 2],
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Terra 0, Recta Lama 0]) ++ (concat $ replicate 10  [Rampa Relva 0 1, Recta Relva 1, Rampa Terra 1 4, Recta Boost 4 , Recta Boost 4 , Rampa Boost 4 0]) ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]) ++ [Recta Terra 0, Rampa Lama 0 1, Recta Terra 1, Recta Relva 1, Recta Boost 1, Recta Relva 1, Rampa Relva 1 0, Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Terra 0, Rampa Relva 0 1, Recta Terra 1, Recta Boost 1, Recta Lama 1, Recta Lama 1, Rampa Terra 1 2]
            ],
            [
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Lama 0, Recta Boost 0]) ++ (concat $ replicate 3  [Rampa Terra 0 1, Recta Relva 1, Rampa Boost 1 4, Recta Lama 4, Recta Lama 4, Rampa Boost 4 0]) ++ [Recta Terra 0, Rampa Terra 0 2, Rampa Relva 2 5, Recta Terra 5, Recta Relva 5, Recta Boost 5, Recta Boost 5, Rampa Lama 5 4, Recta Lama 4, Recta Lama 4, Recta Terra 4, Rampa Relva 4 2, Recta Relva 2, Recta Terra 2, Rampa Terra 2 4, Rampa Boost 4 5, Recta Lama 5]    ++ [Rampa Terra 5 2, Rampa Relva 2 0, Recta Terra 0] ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]),
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Boost 0, Recta Terra 0]) ++ (concat $ replicate 3 [Rampa Lama 0 1, Recta Terra 1, Rampa Boost 1 4, Recta Terra 4, Recta Terra 4, Rampa Relva 4 0]) ++ [Recta Terra 0, Rampa Boost 0 2, Rampa Lama 2 3, Recta Terra 3, Recta Boost 3, Recta Relva 3, Rampa Relva 3 4, Recta Terra 4, Recta Boost 4, Recta Boost 4, Recta Terra 4, Rampa Relva 4 3, Rampa Terra 3 1, Rampa Boost 1 2, Recta Terra 2, Recta Boost 2, Recta Lama 2] ++ [Rampa Terra 2 1, Rampa Relva 1 0, Recta Terra 0] ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]),
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Lama 0, Recta Relva 0]) ++ [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Recta Terra 1, Recta Boost 1, Recta Relva 1, Rampa Relva 1 0, Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Rampa Boost 1 2, Recta Lama 2, Recta Relva 2, Recta Lama 2] ++ [Rampa Boost 2 0] ++ (concat $ replicate 3  [Rampa Boost 0 1, Recta Terra 1, Rampa Lama 1 3, Recta Boost 3, Recta Boost 3, Rampa Terra 3 0])    ++ [Recta Terra 0, Recta Relva 0] ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0]),
            [Recta Terra 0] ++ (concat $ replicate 3 [Recta Terra 0, Recta Lama 0]) ++ [Recta Terra 0, Rampa Lama 0 1, Recta Terra 1, Recta Relva 1, Recta Boost 1, Recta Relva 1, Rampa Relva 1 0, Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Terra 0, Rampa Relva 0 1, Recta Terra 1, Recta Boost 1, Recta Lama 1, Recta Lama 1, Rampa Terra 1 2] ++  [Rampa Relva 2 0] ++ (concat $ replicate 3  [Rampa Relva 0 1, Recta Relva 1, Rampa Terra 1 3, Recta Boost 3 , Recta Boost 3 , Rampa Boost 3 0])  ++ [Recta Terra 0, Recta Relva 0] ++ (concat $ replicate 10 [Recta Terra 0, Recta Boost 0])
            ],
            [
            [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Recta Terra 1,Recta Terra 1, Recta Terra 1, Rampa Terra 1 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Rampa Terra 0 2, Rampa Terra 2 1, Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2, Rampa Terra 2 0, Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Rampa Terra 0 1, Rampa Terra 1 2, Rampa Terra 2 3, Rampa Terra 3 4, Recta Lama 4,Recta Lama 4,Recta Lama 4,Recta Lama 4,Recta Lama 4,Rampa Lama 4 3, Rampa Lama 3 2, Rampa Lama 2 1, Rampa Lama 1 0, Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0],
            [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Recta Terra 1,Recta Terra 1, Recta Terra 1, Rampa Terra 1 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Rampa Terra 0 2, Rampa Terra 2 1, Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 2, Rampa Terra 2 0, Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Rampa Terra 0 1, Rampa Terra 1 2, Rampa Terra 2 3, Recta Lama 3, Recta Lama 3,Recta Lama 3,Recta Lama 3,Recta Lama 3,Recta Lama 3,Rampa Lama 3 2, Rampa Lama 2 1, Rampa Lama 1 0, Recta Lama 0, Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],
            [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Recta Terra 1,Recta Terra 1, Recta Terra 1, Rampa Terra 1 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Rampa Terra 0 2, Rampa Terra 2 1, Recta Lama 1,Recta Lama 1,Recta Lama 1,Recta Lama 1,Recta Lama 1,Rampa Terra 1 2, Rampa Terra 2 0, Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Rampa Terra 0 1, Rampa Terra 1 2, Recta Lama 2, Recta Lama 2, Recta Lama 2,Recta Lama 2,Recta Lama 2,Recta Lama 2,Recta Lama 2,Rampa Lama 2 1, Rampa Lama 1 0, Recta Lama 0, Recta Lama 0, Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],
            [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1, Recta Terra 1,Recta Terra 1, Recta Terra 1, Rampa Terra 1 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Rampa Terra 0 2, Rampa Terra 2 1, Recta Lama 1,Recta Lama 1,Recta Lama 1,Recta Lama 1,Recta Lama 1,Rampa Terra 1 2, Rampa Terra 2 0, Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Rampa Terra 0 1, Recta Lama 1, Recta Lama 1, Recta Lama 1, Recta Lama 1,Recta Lama 1,Recta Lama 1,Recta Lama 1,Recta Lama 1,Rampa Lama 1 0, Recta Lama 0, Recta Lama 0, Recta Lama 0, Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],
            [Recta Terra 0, Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Boost 0, Recta Boost 0, Recta Boost 0, Recta Boost 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Boost 0, Recta Boost 0, Recta Boost 0, Recta Boost 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Boost 0, Recta Boost 0, Recta Boost 0, Recta Boost 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0]
            ]
            ]

    -- | Nomes disponíveis para os bots
    botsNames :: [String]
    botsNames = ["Cole",
                 "Proto",
                 "Sparky",
                 "Dotty",
                 "ebaoid",
                 "ahekoid",
                 "Azerty",
                 "Knave",
                 "Tera",
                 "Data",
                 "axmx",
                 "opi"]