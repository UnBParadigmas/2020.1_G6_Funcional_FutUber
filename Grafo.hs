module Grafo (
        mostraCidadesArquivo,
        lerCidadesArquivo,
        lerEstradasArquivo,
        dijkstra,
        mostraCidade
) where 

import qualified Data.Maybe  as Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Tuple as Tuple
import qualified Data.List as List
import qualified Data.Function as Function
import qualified Data.Ord as Ord

mostraCidade :: (Show a, Ord k) => Map.Map k a -> k -> IO ()
mostraCidade cidades id = print (Maybe.fromJust (Map.lookup id cidades))

-- Separa cada string na linha do arquivo
separaValoresLinha :: [String] -> [[T.Text]]
separaValoresLinha [] = []
separaValoresLinha (linha:linhasRestantes) = T.splitOn (T.pack "\t") (T.pack linha) : (separaValoresLinha linhasRestantes)

lerCidadesArquivo :: IO (Map.Map Int String)
lerCidadesArquivo = do
        arquivo <- readFile "nodes.txt"
        let linhasSeparadas = T.splitOn (T.pack "\n") (T.pack arquivo)
        let linhasString = [T.unpack x | x <- linhasSeparadas]
        let cidades = separaValoresLinha linhasString
        return (Map.fromList [(read (T.unpack (x!!0)) :: Int, T.unpack (x!!1))|x<-cidades])

mostraCidadesArquivo :: [(Int, String)] -> IO ()
mostraCidadesArquivo = mapM_ (\(idCidade, nomeCidade) -> putStrLn ((show idCidade) ++" - "++nomeCidade))

lerEstradasArquivo :: IO [(Int, [(Int, Int)])]
lerEstradasArquivo = do
        arquivo <- readFile "edges.txt"
        let linhasSeparadas = T.splitOn (T.pack "\n") (T.pack arquivo)
        let linhasString = [T.unpack x | x <- linhasSeparadas]
        let mapEstradas = separaValoresLinha linhasString
        let estradas = [(read (T.unpack (x!!0)) :: Int,(read (T.unpack (x!!1)) :: Int,read (T.unpack (x!!2)) :: Int)) |x<-mapEstradas] ++ [(read (T.unpack (x!!1)) :: Int,(read (T.unpack (x!!0)) :: Int,read (T.unpack (x!!2)) :: Int)) |x<-mapEstradas]
        return (agrupaEstradas estradas)

agrupaEstradas :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
agrupaEstradas = map (\l -> (Tuple.fst . head $ l, map Tuple.snd l)) . List.groupBy ((==) `Function.on` Tuple.fst)
          . List.sortBy (Ord.comparing Tuple.fst)

ordenar :: Ord b => [(a, b)] -> [(a, b)]
ordenar = List.sortBy (compare `Function.on` Tuple.snd)

retornaProximaVisita :: [(Int, (Int, Int))] -> [Int] -> Int
retornaProximaVisita [] visitados =
        -1
retornaProximaVisita ((cidadeAtual, (proximaCidade,origem)):cauda) visitados
        | cidadeAtual `elem` visitados = retornaProximaVisita cauda visitados
        | otherwise =  cidadeAtual

calculaMenorCaminho :: [(Int, (Int,Int))] -> [(Int, [(Int, Int)])] -> Int -> [(Int, (Int, Int))]
calculaMenorCaminho menorCaminho estradas proximaVisita = do
        let proximoMenorCaminho = Maybe.fromJust (Map.lookup proximaVisita (Map.fromList estradas))
        let proximaDistancia = Tuple.fst (Maybe.fromJust (Map.lookup proximaVisita (Map.fromList menorCaminho)))
        let menorCaminhoDuplo = agrupaEstradas (menorCaminho ++ [(Tuple.fst x, (Tuple.snd x + proximaDistancia, proximaVisita)) | x <- proximoMenorCaminho])
        [(Tuple.fst x,(List.minimum(Tuple.snd x))) | x <- menorCaminhoDuplo]
        
dijkstra :: Int -> Int -> [(Int, [(Int, Int)])] -> Int
dijkstra origem destino estradas = do
        let menorCaminho = Maybe.fromJust (Map.lookup origem (Map.fromList estradas))
        let menorCaminhoComOrigem = [(Tuple.fst x,(Tuple.snd x, origem)) | x <- menorCaminho] ++ [(origem, (0, origem))]
        dijkstra' destino estradas menorCaminhoComOrigem [origem] (Tuple.fst((ordenar menorCaminhoComOrigem) !! 0))

dijkstra' :: Int -> [(Int, [(Int, Int)])] -> [(Int, (Int,Int))] -> [Int] -> Int -> Int
dijkstra' destino estradas menorCaminho visitados proximaVisita = do
        if destino `elem` visitados then
                --printarCaminho menorCaminho destino
                Tuple.fst (Maybe.fromJust (Map.lookup destino (Map.fromList menorCaminho)))
        else    
                dijkstra'
                        destino
                        estradas
                        (calculaMenorCaminho menorCaminho estradas proximaVisita)
                        (proximaVisita : visitados)
                        (retornaProximaVisita
                                (ordenar (calculaMenorCaminho menorCaminho estradas proximaVisita))
                                (proximaVisita : visitados)
                        )

-- printarCaminho menorCaminho destino
--         print (Tuple.fst (Maybe.fromJust (Map.lookup destino (Map.fromList menorCaminho))))
--         if (Tuple.fst (Maybe.fromJust (Map.lookup destino (Map.fromList menorCaminho)))) /= 0 then
--                 --let candidato = Tuple.snd (Maybe.fromJust(Map.lookup destino (Map.fromList menorCaminho)))
--                 printarCaminho menorCaminho Tuple.snd (Maybe.fromJust(Map.lookup destino (Map.fromList menorCaminho)))

-- printarCaminho menorCaminho destino 
        -- |(Tuple.fst (Maybe.fromJust (Map.lookup destino (Map.fromList menorCaminho)))) == 0 = (Tuple.snd (Maybe.fromJust (Map.lookup destino (Map.fromList menorCaminho))))
        -- | otherwise = printarCaminho menorCaminho (Tuple.snd (Maybe.fromJust(Map.lookup destino (Map.fromList menorCaminho))))