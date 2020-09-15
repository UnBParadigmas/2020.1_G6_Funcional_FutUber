module Menu (menu) where

import System.IO
import Control.Monad
import Data.Maybe

import Main

start = do {
        Main.lerCidadesArquivo;
        Main.lerEstradasArquivo;
}
menu :: IO()
menu = do { 
        
        putStrLn "Menu: ";
        putStrLn "1 - Inserir nova cidade";
        putStrLn "2 - Construir estrada (liga duas cidades)";
        putStrLn "3 - Listar cidades";
        putStrLn "4 - Excluir cidade";
        putStrLn "5 - Destruir estrada";
        putStrLn "6 - Passear pelas cidades";
        putStrLn "0 - Sair";
        opcao <- getLine;
        case opcao of 
            "1" -> inserirCidade;
            "2" -> construirEstrada;
            "3" -> listarCidade;
            "4" -> excluirCidade;
            "5" -> destruirEstrada;
            "6" -> passeio;
            "0" -> putStrLn "Sair...";
}
-- Verificar série 2 de duvidas
inserirCidade :: IO()
inserirCidade = do { putStrLn " >> Inserir nova cidade <<";
        --putStrLn "Digite o nome da cidade a ser inserida";
        --cidade <- getLine;
        --let aresta = parseAresta (cidade,[]);
        --let arestaValue = Maybe.FromJust aresta
        --Main.inserirCidadeSemAdjacentes cidade [aresta]
        
        menu; 
}

construirEstrada :: IO()
construirEstrada = do { putStrLn "Construindo estrada...";
        putStrLn "1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of 
            "1" -> menu
}

listarCidade :: IO()
listarCidade = do { putStrLn "Listando cidades...";
        putStrLn "1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
}

excluirCidade :: IO()
excluirCidade = do { putStrLn "Excluindo cidades...";
        putStrLn "1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
}

destruirEstrada :: IO()
destruirEstrada = do { putStrLn "Destruindo estradas...";
        putStrLn "1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
}

passeio :: IO()
passeio = do { putStrLn "Passeando pela cidade...";
        putStrLn "1 - Voltar para o menu";
        opcao <- getLine;
        case opcao of
            "1" -> menu
}
