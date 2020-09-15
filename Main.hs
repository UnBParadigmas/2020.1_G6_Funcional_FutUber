module Main (main, grafo, inserirCidadeSemAdjacentes, adicionaCidadeAdjacente, mostraCidadesArquivo, lerCidadesArquivo, lerEstradasArquivo) where 

import qualified Data.Maybe  as Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List
import Data.Function (on)
import Data.Ord (comparing)
import Data.Graph (Table)



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
        let estradas = [(read (T.unpack (x!!0)) :: Int,(read (T.unpack (x!!1)) :: Int,read (T.unpack (x!!2)) :: Int)) |x<-edges]
        return (agrupaEstradas estradas)

--https://stackoverflow.com/questions/12398458/how-to-group-similar-items-in-a-list-using-haskell
agrupaEstradas :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
agrupaEstradas = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst)
          . sortBy (comparing fst)

main :: IO ()
main = return ()