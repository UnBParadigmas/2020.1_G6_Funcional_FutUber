module Menu (menu) where

import System.IO
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as Map

import Grafo
    ( mostraCidade,
      lerCidadesArquivo,
      mostraCidadesArquivo,
      lerEstradasArquivo,
      dijkstra,
      inserirCidadeArquivo,
      construirEstradaArquivo )

menu :: IO()
menu = do { 
        putStrLn "Menu: ";
        putStrLn "1 - Listar cidades";
        putStrLn "2 - Passear pelas cidades";
        putStrLn "3 - Inserir nova cidade";
        putStrLn "4 - Construir nova estrada";
        putStrLn "0 - Sair";
        opcao <- getLine;
        case opcao of 
            "1" -> listarCidade;
            "2" -> passeio;
            "3" -> inserirCidade;
            "4" -> construirEstrada;
            "0" -> putStrLn "Sair...";
        -- putStrLn "2 - Construir estrada (liga duas cidades)";
        -- putStrLn "4 - Excluir cidade";
        -- putStrLn "5 - Destruir estrada";
        --     "1" -> inserirCidade;
        --     "4" -> excluirCidade;
        --     "5" -> destruirEstrada;
}

inserirCidade :: IO ()
inserirCidade = do {
        putStr "\nInsira uma nova cidade: ";
        cidade <- getLine;
        inserirCidadeArquivo cidade;

        putStrLn "\n\n1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
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

        putStrLn "\n\n1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
}

listarCidade :: IO()
listarCidade = do { putStrLn "Listando cidades...";
        cidades <- lerCidadesArquivo;
        mostraCidadesArquivo (Map.toList cidades);
        putStrLn "\n\n1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
}

passeio :: IO()
passeio = do { 
        estradas <- lerEstradasArquivo;
        cidades <- lerCidadesArquivo;

        putStrLn "\nListando cidades...";
        mostraCidadesArquivo (Map.toList cidades);

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
        print (dijkstra (read origem :: Int) (read destino :: Int) estradas);
        
        putStrLn "\n1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
}
