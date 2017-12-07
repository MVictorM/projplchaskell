import Damas(Jogo, getMovimentos, mover, Mover, 
  getInicioJogo, gameOver, vencedorJogo )

playGame :: Jogo -> IO()
playGame game = do
  if not (gameOver game) 
    then continuarJogo
    else terminarJogo where
  possiveisJogadas = getMovimentos game
  jogadaValida jogada = jogada `elem` possiveisJogadas
  continuarJogo = do
    putStrLn $ show game
    putStrLn $ "Possiveis jogadas: \n " ++ (show $ possiveisJogadas) ++ "\nDigite sua jogada no formato: \"Mover (numero_coluna_origem, numero_linha_origem) (numero_coluna_destino, numero_linha_destino)\" (q para sair): "
    strJogada <- getLine
    if strJogada == "q"
      then putStrLn "Saindo do jogo..." 
      else if jogadaValida (getJogada strJogada)
        then do
          putStrLn $ "Fazendo jogada " ++ strJogada
          playGame $ aplicaJogada game strJogada 
        else putStrLn $ strJogada ++ " não é uma jogada permitida"
  terminarJogo = do 
    putStrLn $ "Fim de jogo. Resultado final: \n" ++ show game
    putStrLn $ show (vencedorJogo game) ++ " venceu"

getJogada :: String -> Mover
getJogada strJogada = read strJogada :: Mover

aplicaJogada :: Jogo -> String -> Jogo
aplicaJogada game strJogada = mover game $ getJogada strJogada

main :: IO ()
main = do
  putStrLn "Iniciando partida de damas"
  playGame getInicioJogo

