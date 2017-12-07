module Damas ( Jogo, getMovimentos, mover, Mover, 
  getInicioJogo, gameOver, vencedorJogo ) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)

type Tamanho = (Int, Int) -- Largura, Altura
type Posicao = (Int, Int) -- linha, coluna
type PosicaoMapa = Map Posicao Marcador

data Player = Player Marcador deriving (Show, Eq)
data Jogo = Jogo GameEstado [Player] deriving (Show, Eq)
data Tabuleiro = Tabuleiro Tamanho PosicaoMapa deriving (Eq) -- Largura, Altura, marc
data GameEstado = GameEstado Tabuleiro Player deriving (Show, Eq) -- Estado, Tabuleiro, Jogador da vez
data Marcador = None | Preto | Vermelho | Dama Marcador deriving (Show, Eq)
data Mover = Mover Posicao Posicao deriving (Show, Eq, Read)

instance Show Tabuleiro where
  show tabuleiro@(Tabuleiro (largura, altura) posicaoTabuleiro) = join linhas where
    linhas = adicionarColunasNums 8 $ adicionarLinhaNums 1 $ parte largura marcadores
    adicionarLinhaNums inicio (r:rs) = (show inicio ++ " " ++ r) : adicionarLinhaNums (inicio + 1) rs
    adicionarLinhaNums _ [] = []
    adicionarColunasNums count rs = rs ++ ["  " ++ (foldr (\d s-> s ++ show d) "" (reverse [1..count]))]
    marcadores = concat $ map (\k -> showMarcador $ posicaoTabuleiro Map.! k) posicoes
    posicoes = posicoesTabuleiro tabuleiro
    parte _ [] = []
    parte n xs = y1 : parte n y2 where
      (y1, y2) = splitAt n xs
    join = foldr (\s ss -> ss ++ ('\n' : s)) ""

-- Exibe letra que representa o tipo da peca e sua cor
showMarcador :: Marcador -> String
showMarcador marcador = case marcador of 
                      None       -> "."
                      Preto      -> "p"
                      Vermelho        -> "v"
                      Dama Preto -> "P"
                      Dama Vermelho   -> "V"
                      _          -> "?"
--retorna posicoes do tabuleiro
posicoesTabuleiro :: Tabuleiro -> [Posicao]
posicoesTabuleiro (Tabuleiro (largura, altura) _) = [(c, r) | r <- [1..altura], c <- [1..largura]]

--retorna posicoes iniciais do jogo
getInicioJogo :: Jogo
getInicioJogo = Jogo estadoInicial [Player Preto, Player Vermelho] where
  estadoInicial = GameEstado getDefaultTabuleiro $ Player Preto

--retorna tabuleiro padrao inicial
getDefaultTabuleiro :: Tabuleiro
getDefaultTabuleiro = posicaoPreenchida where
  tabuleiroVazio = getTabuleiro (8, 8)
  posicaoPreenchida = atualizaPosicaoNoTabuleiro posicaoVermPreenchida posicoesPretos Preto
  posicaoVermPreenchida = atualizaPosicaoNoTabuleiro tabuleiroVazio posicoesVermelhos Vermelho
  posicoesPretos = geraPosic [1..3]
  posicoesVermelhos = geraPosic [6..8]
  geraPosic linhas = [(c, d) | c <- [1..8], d <- linhas, or [and [even c, even d], and [odd c, odd d]] ]

--retorna posicao da peca
posicaoMarcador :: PosicaoMapa -> Posicao -> Marcador
posicaoMarcador orig pos = maybe None id (Map.lookup pos orig)

--troca de posicao
troca :: PosicaoMapa -> Posicao -> Posicao -> PosicaoMapa 
troca orig de ate = novo where
  marc = posicaoMarcador orig
  novo = Map.insert de (marc ate) $ Map.insert ate (marc de) orig

--verifica se esta no alcance da peca
noAlcance :: Tabuleiro -> Posicao -> Bool
noAlcance tabuleiro pos = posX > 0 && posX <= tabuleiroX && posY > 0 && posY <= tabuleiroY where
  (tabuleiroX, tabuleiroY) = tamanho tabuleiro
  (posX, posY) = pos

--retorna a lista de marcadores que representam o tabuleiro
getTabuleiro :: Tamanho -> Tabuleiro
getTabuleiro sz = Tabuleiro sz $ getPosicaoNoTabuleiro sz

--retorna a posicao da peca no tabuleiro
getPosicaoNoTabuleiro :: Tamanho -> PosicaoMapa 
getPosicaoNoTabuleiro (largura, altura) = Map.fromList $ zip posicoes (repeat None) where
  posicoes = [(row, col) | row <- [1..altura], col <- [1..largura]]

--retorna tamanho
tamanho :: Tabuleiro -> Tamanho
tamanho (Tabuleiro s _) = s

-- Recupera os marcadores do tabuleiro
marcadores :: Tabuleiro -> PosicaoMapa
marcadores (Tabuleiro _ ms) = ms

-- Atualiza o marcador associado ao tabuleiro
trocarMarcadores :: Tabuleiro -> PosicaoMapa -> Tabuleiro
trocarMarcadores (Tabuleiro sz _) ms = Tabuleiro sz ms

-- atualiza o marcador no tabuleiro
atualizaTabuleiro :: Tabuleiro -> Posicao -> Marcador -> Tabuleiro
atualizaTabuleiro tabuleiro pos@(row, col) marc = novoTabuleiro where
  ms = marcadores tabuleiro
  novoTabuleiro = trocarMarcadores tabuleiro novoMarcador
  novoMarcador = Map.insert pos marc ms

atualizaPosicaoNoTabuleiro :: Tabuleiro -> [Posicao] -> Marcador -> Tabuleiro
atualizaPosicaoNoTabuleiro tabuleiro posicoes marcador = updatedBoard where
  updatedBoard = foldr updatePos tabuleiro posicoes
  updatePos pos tabuleiro = atualizaTabuleiro tabuleiro pos marcador

--retorna posicoes vazias
posicoesVazias :: Tabuleiro -> [Posicao]
posicoesVazias (Tabuleiro _ posicaoTabuleiro) = Map.keys $ Map.filter (== None) posicaoTabuleiro

--identifica se venceu
venceu :: Tabuleiro -> Marcador -> Bool
venceu _ None = False
venceu tabuleiro (Dama marcador) = venceu tabuleiro marcador
venceu (Tabuleiro _ posicaoTabuleiro) marcador = not existeOutroMarcador where
  existeOutroMarcador = any (eqMarcador outroMarcador) elems
  outroMarcador = marcadorBasico marcador
  eqMarcador m a = marcadorBasico a == m
  elems = Map.elems posicaoTabuleiro

jogoTabuleiro :: Jogo -> Tabuleiro
jogoTabuleiro (Jogo (GameEstado tabuleiro _) _) = tabuleiro

--fim de jogo
gameOver :: Jogo -> Bool
gameOver game = any (venceu tabuleiro) marcadores where
  tabuleiro = jogoTabuleiro game
  marcadores = [Vermelho, Preto]

--retorna o vencedor
vencedorJogo :: Jogo -> Marcador
vencedorJogo game@(Jogo (GameEstado b (Player m)) _) = vencedor where
  vencedor = if gameOver game then
                            if m == Vermelho then Preto
                                        else Vermelho
                            else None
-- Limpa a posicao
limpaEstado :: GameEstado -> Posicao -> GameEstado
limpaEstado orig posOrig = reposicionaEstado orig posOrig None

-- substitui o item na posicao por um marcador
reposicionaEstado :: GameEstado -> Posicao -> Marcador -> GameEstado
reposicionaEstado orig@(GameEstado origTabuleiro origPlayer) posOrig novoMarcador = 
  GameEstado novoTabuleiro origPlayer where
  novoTabuleiro = atualizaTabuleiro origTabuleiro posOrig novoMarcador

getEstadoMarcador :: GameEstado -> Posicao -> Marcador
getEstadoMarcador (GameEstado tabuleiro _) pos = 
  posicaoMarcador (marcadores tabuleiro) pos

-- atualiza o estado do jogo apos mover a peca
atualizaEstado :: GameEstado -> Posicao -> Posicao -> GameEstado
atualizaEstado orig@(GameEstado origTabuleiro origPlayer) posOrig posDes = 
  GameEstado novoTabuleiro proximo where
  orgMarcador = posicaoMarcador (marcadores origTabuleiro) posOrig
  removeTabuleiro = atualizaTabuleiro origTabuleiro posOrig None
  novoTabuleiro = atualizaTabuleiro removeTabuleiro posDes orgMarcador
  proximo = case origPlayer of 
    Player Preto        -> Player Vermelho
    Player (Dama Preto) -> Player Vermelho
    _                   -> Player Preto

-- dado o tabuleiro e a posicao nele, retorna as possiveis jogadas para o marcador. O marcador so pode se mexer na diagonal para frente,
-- pra tras ou frente. Os marcadores podem se mover dois espacos na diagonal se houver uma peca oposta no meio e o lugar de destino estiver vazio
movimentosTabuleiro :: Tabuleiro -> Posicao -> [Posicao]
movimentosTabuleiro tabuleiro src = andarEspaco tabuleiro src ++ puloNoEspaco tabuleiro src

-- dado o tabuleiro e a posicao nele, retorna os espacos possiveis para se mexer sem pular nenhuma casa
andarEspaco :: Tabuleiro -> Posicao -> [Posicao]
andarEspaco tabuleiro src = filter (noLimiteEVazio tabuleiro) (possiveisJogadas marcador) where
  marcador = posicaoMarcador (marcadores tabuleiro) src
  (x, y) = src
  possiveisJogadas (Dama _) = possiveisJogadas Preto ++ possiveisJogadas Vermelho
  possiveisJogadas Preto = [(x-1,y+1), (x+1,y+1)]
  possiveisJogadas Vermelho = [(x-1,y-1), (x+1,y-1)]
  possiveisJogadas _ = []

--verifica se o marcador esta na direcao
estaNaDirecao :: Marcador -> Posicao -> Posicao -> Bool
estaNaDirecao Vermelho (_, sy) (_, dy)   = dy > sy
estaNaDirecao Preto (_, sy) (_, dy) = dy < sy

--verifica se esta no alcance e o espaco esta vazio
noLimiteEVazio tabuleiro pos = noAlcance tabuleiro pos && tabuleiroVazioNaPosicao tabuleiro pos

--verifica se a posicao esta vazia
tabuleiroVazioNaPosicao :: Tabuleiro -> Posicao -> Bool
tabuleiroVazioNaPosicao tabuleiro pos = m == None where
  m = posicaoMarcador (marcadores tabuleiro) pos

-- pula for the marcador
puloNoEspaco :: Tabuleiro -> Posicao -> [Posicao]
puloNoEspaco tabuleiro src = pula where
  pula = filter (noLimiteEVazio tabuleiro) (possiveisJogadas marcador)
  marcador = posicaoMarcador (marcadores tabuleiro) src

  possiveisJogadas :: Marcador -> [Posicao]
  possiveisJogadas Preto        = possiveisMovimentosCor Preto
  possiveisJogadas Vermelho          = possiveisMovimentosCor Vermelho
  possiveisJogadas (Dama color) = possivelPulo
  possiveisJogadas _            = []

--possiveis movimentos da cor
  possiveisMovimentosCor :: Marcador -> [Posicao]
  possiveisMovimentosCor color = filter emDirecao jogadas where
    emDirecao = estaNaDirecao (alternaCor color) src
    jogadas = possivelPulo

  -- tuplas da posicao de partida e destino
  possivelPulo = map (\(_, a, _) -> a) pula where 
    pula = filter podePular tuplaPossivelPulo
    tuplaPossivelPulo = zip3 diag1 diag2 piecesAtDiagonals
    diag1 = diagonais src
    diag2 = diagonalN 2 src
    piecesAtDiagonals = map (posicaoMarcador (marcadores tabuleiro)) diag1
    -- uma peca pode pular a posicao se for sobre uma peca de cor contraria
    podePular :: (Posicao, Posicao, Marcador) -> Bool
    podePular (_, _, m) = marcadorPulaOutro marcador m

--alternando cores
alternaCor :: Marcador -> Marcador
alternaCor Preto        = Vermelho
alternaCor Vermelho          = Preto
alternaCor (Dama color) = Dama $ alternaCor color
alternaCor c            = c

--um marcador pula o outro
marcadorPulaOutro :: Marcador -> Marcador -> Bool
marcadorPulaOutro None _ = False
marcadorPulaOutro marcador (Dama m) = m /= None && marcadorPulaOutro marcador m
marcadorPulaOutro marcador Vermelho = marcador /= Vermelho
marcadorPulaOutro marcador Preto = marcador /= Preto
marcadorPulaOutro _ _ = False

-- retorna todas as posicoes que estao a esse parametro passado da posicao
diagonalN :: Int -> Posicao -> [Posicao]
diagonalN d (r, c) = [(r+d, c+d), (r-d, c-d), (r+d, c-d), (r-d, c+d)]

-- retorna todas as posicoes que estao a uma diagonal da posical
diagonais :: Posicao -> [Posicao]
diagonais = diagonalN 1

-- retorna todas as jogadas possiveis da posicao recebida
getJogadasEstadoAtual :: GameEstado -> Posicao -> [Mover]
getJogadasEstadoAtual state@(GameEstado tabuleiro@(Tabuleiro sz posicaoTabuleiro) player) posOrig = jogadas where
  jogadas = if (marcadorCorresponde player marcador) 
            then map (Mover posOrig) (movimentosTabuleiro tabuleiro posOrig) else []
  marcador = posicaoMarcador posicaoTabuleiro posOrig
  marcadorCorresponde (Player m1) m2 = marcadorBasico m1 == marcadorBasico m2

getPosicoesPlayer :: Player -> Tabuleiro -> [Posicao]
getPosicoesPlayer player tabuleiro = posicoes where
  marcador = case player of 
             (Player m) -> marcadorBasico m
  igualMarcador pos = marcador == marcadorBasico (posicaoMarcador (marcadores tabuleiro) pos)
  posicoes = filter igualMarcador $ posicoesTabuleiro tabuleiro

--retorna todos os movimentos desse estado
getTodosMovimentosEstado :: GameEstado -> [Mover]
getTodosMovimentosEstado state@(GameEstado tabuleiro player) = jogadas where
  jogadas = concat $ map (getJogadasEstadoAtual state) posicoes
  posicoes = posicoesTabuleiro tabuleiro

--retorna os movimentos possiveis
getMovimentos :: Jogo -> [Mover]
getMovimentos (Jogo gs _) = getTodosMovimentosEstado gs

-- é uma peça normal e nao dama
marcadorBasico :: Marcador -> Marcador
marcadorBasico (Dama m) = marcadorBasico m
marcadorBasico Preto = Preto
marcadorBasico Vermelho = Vermelho
marcadorBasico None = None

--move peca
mover :: Jogo -> Mover -> Jogo
mover game@(Jogo gs players) jogada@(Mover p1 p2) = newGame where
  newGame = Jogo novoEstado3 players
  novoEstado1 = (atualizaEstado gs p1 p2)
  novoEstado2 = if ehPulo jogada 
                then limpaEstado novoEstado1 pulouPos
                else novoEstado1
  novoEstado3 = if (viraDama moveuMarcador p2)
                then (damaNaPosicao novoEstado2 p2)
                else novoEstado2

  moveuMarcador = getEstadoMarcador novoEstado2 p2

  pulouPos = getPulouPosicao jogada

--verifica se tem uma dama na posicao
damaNaPosicao :: GameEstado -> Posicao -> GameEstado
damaNaPosicao gs pos = newState where
  newState = reposicionaEstado gs pos damaPeca
  damaPeca = pecaDama $ getEstadoMarcador gs pos

--verifica se foi é um pulo
ehPulo :: Mover -> Bool
ehPulo (Mover (sc,sr) (dc,dr)) = diff > 1 where 
  diff = abs $ sr - dr

-- retorna se pulou a posicao
getPulouPosicao :: Mover -> Posicao
getPulouPosicao m@(Mover (sc,sr) (dc,dr)) = mPos where
  mPos = (sc+deltaColumn,sr+deltaRow)
  deltaColumn = if dc > sc then 1 else -1
  deltaRow = if dr > sr then 1 else -1

-- determina se a peca deve virar dama de acordo com sua posicao
viraDama :: Marcador -> Posicao -> Bool
viraDama (Dama _) _ = False
viraDama Preto (_, 8) = True
viraDama Vermelho (_, 1) = True
viraDama _ _ = False

-- Torna peca uma dama ou retorna a peca normal
pecaDama :: Marcador -> Marcador
pecaDama Preto = Dama Preto
pecaDama Vermelho = Dama Vermelho
pecaDama m = m