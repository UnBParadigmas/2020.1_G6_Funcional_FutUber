module Menu (menu) where

import System.IO
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

import Grafo (
        mostraCidade,
        lerCidadesArquivo,
        mostraCidadesArquivo,
        lerEstradasArquivo,
        dijkstra,
        inserirCidadeArquivo,
        construirEstradaArquivo,
        gerarArquivoMontaGrafo 
        )

menu :: IO()
menu = do { 
        putStrLn "Menu: ";
        putStrLn "1 - Listar cidades";
        putStrLn "2 - Passear pelas cidades";
        putStrLn "3 - Inserir nova cidade";
        putStrLn "4 - Construir nova estrada";
        putStrLn "5 - Gerar imagem do grafo";
        putStrLn "0 - Sair";
        opcao <- getLine;
        case opcao of 
            "1" -> listarCidade;
            "2" -> passeio;
            "3" -> inserirCidade;
            "4" -> construirEstrada;
            "5" -> gerarImagemGrafo;
            otherwise -> putStrLn "Sair...";
}

inserirCidade :: IO ()
inserirCidade = do {
        putStr "\nInsira uma nova cidade: ";
        cidade <- getLine;
        inserirCidadeArquivo cidade;

        putStrLn "\n- Tecle enter para voltar para o menu";
        opcao <- getLine;
        menu
}

construirEstrada :: IO ()
construirEstrada = do {
        cidades <- lerCidadesArquivo;

        putStrLn "\nConstruindo estrada...";
        putStr "Insira o id da cidade de origem: ";
        origem <- getLine;
        mostraCidade cidades (read origem :: Int);
        putStr "Insira o id da cidade de vizinha: ";
        destino <- getLine;
        mostraCidade cidades (read destino :: Int);
        putStr "Insira o custo: ";
        custo <- getLine;

        construirEstradaArquivo origem destino custo;

        putStrLn "\n- Tecle enter para voltar para o menu";
        opcao <- getLine;
        menu
}

listarCidade :: IO()
listarCidade = do { putStrLn "Listando cidades...";
        cidades <- lerCidadesArquivo;
        mostraCidadesArquivo (Map.toList cidades);
        menu
}

passeio :: IO()
passeio = do { 
        estradas <- lerEstradasArquivo;
        cidades <- lerCidadesArquivo;

        putStrLn "\n\nPasseando pela cidade...";
        putStr "Digite o id cidade de origem: ";
        origem <- getLine;
        putStr "Saindo de: ";
        mostraCidade cidades (read origem :: Int);
        putStr "Digite o id cidade de destino: ";
        destino <- getLine;
        putStr "Indo para: ";
        mostraCidade cidades (read destino :: Int);

        putStr "\nMenor custo: ";
        mostraCaminho cidades (dijkstra (read origem :: Int) (read destino :: Int) estradas);
        
        putStrLn "\n- Tecle enter para voltar para o menu";
        opcao <- getLine;
        menu
}

mostraCaminho :: (Ord k, Show a1, Show a2) => Map.Map k a1 -> [(a2, k)] -> IO ()
mostraCaminho cidades [] =
    print "Fim do passeio..."
mostraCaminho cidades ((a,b):t) = do
    print "Cidade:";
    mostraCidade cidades (b);
    print "-> Custo:";
    print (a);
    mostraCaminho cidades t;

gerarImagemGrafo :: IO ()
gerarImagemGrafo = do
        gerarArquivoMontaGrafo
        menu