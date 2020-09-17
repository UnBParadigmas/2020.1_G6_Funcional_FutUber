module Main (
        main,
        grafo,
        inserirCidadeSemAdjacentes,
        adicionaCidadeAdjacente,
        mostraCidadesArquivo,
        lerCidadesArquivo,
        lerEstradasArquivo,
        dijkstra
) where 

import qualified Data.Maybe  as Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List ( groupBy, sortBy, minimum )
import Data.Function (on)
import Data.Ord (comparing)
import Data.Graph (Table)
import qualified Data.Tuple as Tuple

type Vertice = Int
type Distancia = Int
type Aresta = (Vertice, [(Vertice, Distancia)])
type Grafo = [Aresta]

grafo :: Aresta -> [Aresta]
grafo aresta = [aresta]

-- Exemplo
-- inserirCidadeSemAdjacentes "gama" []
inserirCidadeSemAdjacentes :: Vertice -> Grafo -> Grafo
inserirCidadeSemAdjacentes cidade grafo = (cidade, []): grafo

-- Exemplo
-- adicionaCidadeAdjacente "gama" "ponte alta" 10 [("gama", [("santa maria", 5)] )]
adicionaCidadeAdjacente :: Ord k => k -> a -> b -> [(k, [(a, b)])] -> [(a, b)]
adicionaCidadeAdjacente cidade novaCidadeAdjacente distancia grafo = do
                    let mapGrafo = Map.fromList grafo
                    let cidadesAdjacentesAtuais = Map.lookup cidade mapGrafo
                    let valorCidadesAdjacentesAtuais = Maybe.fromJust cidadesAdjacentesAtuais
                    (novaCidadeAdjacente, distancia) : valorCidadesAdjacentesAtuais

splitSublist :: [String] -> [[T.Text]]
splitSublist [] = []
splitSublist (x:xs) = T.splitOn (T.pack "\t") (T.pack x) : (splitSublist xs)

lerCidadesArquivo :: IO (Map.Map Int String)
lerCidadesArquivo = do
        arquivo <- readFile "nodes.txt"
        let nodesRaw = T.splitOn (T.pack "\n") (T.pack arquivo) --nodes = lista de T.Text. Ex: ["1\tMarginal-12","2\tMarginal-10B"]
        let parsedList = [T.unpack x | x <- nodesRaw]
        let nodes = splitSublist parsedList
        return (Map.fromList [(read (T.unpack (x!!0)) :: Int, T.unpack (x!!1))|x<-nodes])

mostraCidadesArquivo :: [(Int, String)] -> IO ()
mostraCidadesArquivo = mapM_ (\(a,b) -> putStrLn ((show a) ++" - "++b))

lerEstradasArquivo :: IO [(Int, [(Int, Int)])]
lerEstradasArquivo = do
        arquivo <- readFile "edges.txt"
        let edgesRaw = T.splitOn (T.pack "\n") (T.pack arquivo) --edges = lista de T.Text. Ex: ["1\tMarginal-12","2\tMarginal-10B"]
        let parsedList = [T.unpack x | x <- edgesRaw]
        let edges = splitSublist parsedList
        let estradas = [(read (T.unpack (x!!0)) :: Int,(read (T.unpack (x!!1)) :: Int,read (T.unpack (x!!2)) :: Int)) |x<-edges] ++ [(read (T.unpack (x!!1)) :: Int,(read (T.unpack (x!!0)) :: Int,read (T.unpack (x!!2)) :: Int)) |x<-edges]
        return (agrupaEstradas estradas)

--https://stackoverflow.com/questions/12398458/how-to-group-similar-items-in-a-list-using-haskell
agrupaEstradas :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
agrupaEstradas = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst)
          . sortBy (comparing fst)


-- Busca o id da cidade adjacente do nó a para encontrar os vizinhos
-- b == d
-- Gera nova estrada passando pela cidade adjacente até o vizinho
-- (a, e, c+f)
-- Verifica se o caminho existe e compara o custo mantendo o menor deles
-- (a, e)?

-- Primeiro valor da lista de estradas
-- let estradasMap = Map.fromList estradas
-- [(1,[(2,10),(15,3),(14,3)]),(2,[(3,3),(17,3)]),(3,[(4,2),(21,5)]),(4,[(5,3),(22,5)]),
-- (5,[(6,5),(23,5)]),(6,[(7,6)]),(7,[(8,7)]),(8,[(9,3),(27,6)]),(9,[(10,3),(34,5)]),
-- (10,[(11,7),(31,6)]),(11,[(12,8)]),(12,[(13,4)]),(13,[(14,2),(24,4)]),(14,[(18,3)]),
-- (15,[(16,4),(18,3)]),(16,[(17,5),(19,3)]),(17,[(20,3)]),(18,[(19,4)]),(19,[(20,5),(24,2)]),(20,[(21,3)]),
-- (21,[(22,2)]),(22,[(23,3),(25,3)]),(23,[(27,2)]),
-- (24,[(25,7),(28,2)]),(25,[(26,2),(29,3)]),(26,[(27,3),(34,4)]),
-- (28,[(29,5),(30,2)]),(29,[(35,1)]),(30,[(31,4)]),(31,[(32,2)]),(32,[(33,3),(35,1)]),(33,[(34,2),(35,2)])]

-- estradas <- lerEstradasArquivo
-- let estradasMap = Map.fromList estradas
-- let primeiro = Map.lookup 1 estradasMap
-- let valoresPrimeiro = Maybe.fromJust primeiro
-- valoresPrimeiroMap = Map.fromList valoresPrimeiro
-- let primeiroAdjacentes = Map.keys valoresPrimeiroMap
-- let estradasDosAdjacentesDoPrimeiro = [(x, Maybe.fromJust (Map.lookup x estradasMap)) |x<-primeiroAdjacentes]
-- let distanciaPrimeiro = Map.elems (Map.fromList estradasDosAdjacentesDoPrimeiro)
-- let todosCaminhos = [gerarNovaTupla x | x<-distanciaPrimeiro]
-- excluiCaminhosDuplicados distanciaPrimeiro todosCaminhos

-- [(2,10),(15,3),(14,3)]
-- [[(3,13),(17,13)],[(18,3)],[(16,4),(18,3)]]
-- x = (2,[(3,3),(17,3)])
gerarMenorCaminho estradasDosAdjacentesDoPrimeiro valoresPrimeiroMap = do
        let distanciaPrimeiro = Map.elems (Map.fromList estradasDosAdjacentesDoPrimeiro)
        print distanciaPrimeiro
        
type Poly = [(Int,Int)]

mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (compare `on` snd)

proximaVisita :: [(Int, Int)] -> [Int] -> Int
proximaVisita [] visitados =
        -1
proximaVisita ((a,b):t) visitados =
        if a `elem` visitados then
                proximaVisita t visitados
        else
                a

dijkstra :: Int -> Int -> [(Int, [(Int, Int)])] -> Int
dijkstra origem destino estradas = do
        let menorCaminho = Maybe.fromJust (Map.lookup origem (Map.fromList estradas)) ++ [(origem, 0)]
        dijkstra' destino estradas menorCaminho [origem] (Tuple.fst((mySort menorCaminho) !! 0))

dijkstra' :: Int -> [(Int, [(Int, Int)])] -> [(Int, Int)] -> [Int] -> Int -> Int
dijkstra' destino estradas menorCaminho visitados nextVisit = do
        if destino `elem` visitados then
                Maybe.fromJust (Map.lookup destino (Map.fromList menorCaminho))
        else    if nextVisit == -1 then
                        -1
                else
                        dijkstra'
                                destino
                                estradas
                                (calculaMenorCaminho menorCaminho estradas nextVisit)
                                (nextVisit : visitados)
                                (proximaVisita
                                        (mySort (calculaMenorCaminho menorCaminho estradas nextVisit))
                                        (nextVisit : visitados)
                                )

gerarNovaTupla :: Num b => [(a, b)] -> [(a, b)]
gerarNovaTupla distanciaPrimeiro = do
        let valoresPrimeiroMap = [(2,10),(14,3),(15,3)]
        let tamanhoListaPrimeiro = length valoresPrimeiroMap
        [(Tuple.fst x, Tuple.snd x + Tuple.snd y) | x<-distanciaPrimeiro, y<-valoresPrimeiroMap]

excluiCaminhosDuplicados :: Foldable t => t a1 -> [[a2]] -> [[a2]]
excluiCaminhosDuplicados distanciaPrimeiro todosCaminhos =
        [slice index (length x) (length distanciaPrimeiro) x  | (index,x) <- zip [0..] todosCaminhos]

takeStep :: Int -> [a] -> [a]
takeStep _ [] = []
takeStep n (x:xs) = x : takeStep n (drop (n-1) xs)

slice :: Int -> Int -> Int -> [a] -> [a]
slice start stop step = takeStep step . take (stop - start) . drop start

main :: IO ()
main = return ()