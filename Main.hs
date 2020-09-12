import qualified Data.Maybe  as Maybe
import qualified Data.Map as Map


type Vertice = String
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