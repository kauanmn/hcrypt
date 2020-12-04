module Main where

import System.Directory
import System.IO
import System.Random
import System.Environment
import Data.List
import Rsa



dispatch :: [String] -> IO ()
dispatch ("help":_)                        = do
    putStrLn "Lista de argumentos para o programa:"
    putStrLn "help\t\t\t\tabre o menu de ajuda"
    putStrLn "rsa keygen\t\t\tgera um par de chaves RSA e salve em dois arquivos separados"
    putStrLn "rsa encrypt_text _\t\tcriptografa o conteudo inserido no terminal usando o algoritmo RSA"
    putStrLn "rsa encrypt_file filePath\tcriptografa o conteudo de um arquivo usando o algoritmo RSA"
    putStrLn "rsa decrypt_text _\t\tdescriptografa o conteudo inserido no terminal usando o algoritmo RSA"
    putStrLn "rsa decrypt_file filePath\tdescriptografa o conteudo de um arquivo usando o algoritmo RSA"
dispatch ("rsa":"keygen":_)                = rsa_keygen

dispatch ("rsa":"encrypt_text":_)          = rsa_encrypt_text
dispatch ("rsa":"encrypt_file":filePath:_) = rsa_encrypt_file filePath

dispatch ("rsa":"decrypt_text":_)          = rsa_decrypt_text
dispatch ("rsa":"decrypt_file":filePath:_) = rsa_decrypt_file filePath

dispatch ("rsa":_) = putStrLn "Faltam argumentos. Use o comando 'help' para saber mais."
dispatch _ = putStrLn "Comando não reconhecido. Use o comando 'help' para saber mais."



main :: IO ()
main = do
    argumentos <- getArgs
    dispatch argumentos



-- funcao IO() para gerar um par de chaves RSA
rsa_keygen :: IO ()
rsa_keygen = do
    chavePublica  <- openFile "chaveRSA_Publica.txt" WriteMode
    chavePrivada  <- openFile "chaveRSA_Privada.txt" WriteMode
    randGen <- getStdGen
    let (p,q) = obterPQ $ listaPrimos randGen
    hPutStrLn chavePublica $ criarChavePublica (p,q)
    hPutStrLn chavePrivada $ criarChavePrivada (p,q)
    putStr "Pronto. Suas chaves foram salvas em dois arquivos diferentes.\n"
    putStr "Chave pública: chaveRSA_Publica.txt\n"
    putStr "Chave privada: chaveRSA_Privada.txt\n"
    hClose chavePublica
    hClose chavePrivada



-- funcao IO() para descriptografia RSA de um texto
rsa_decrypt_text :: IO ()
rsa_decrypt_text = do
    chavePrivada <- openFile "chaveRSA_Privada.txt" ReadMode
    chave    <- hGetLine chavePrivada
    putStrLn "Insira o conteudo a ser descriptografado:"
    conteudo <- getLine
    let (n,d) = read (chave) :: (Integer, Integer)
    let conteudoDecifrado = descriptografar conteudo (n, d)
    putStrLn (conteudoDecifrado ++ "\n")
    putStrLn "Nome do arquivo para salvar os dados:"
    fileName <- getLine
    writeFile fileName (conteudoDecifrado ++ "\n")
    hClose chavePrivada



-- funcao IO() para descriptografia RSA de um arquivo
rsa_decrypt_file :: FilePath -> IO ()
rsa_decrypt_file filePath = do
    chavePrivada <- openFile "chaveRSA_Privada.txt" ReadMode
    chave    <- hGetLine chavePrivada
    secretTextHandle <- openFile filePath ReadMode
    conteudo <- hGetLine secretTextHandle
    let (n,d) = read (chave) :: (Integer, Integer)
    let conteudoDecifrado = descriptografar conteudo (n, d)
    putStrLn (conteudoDecifrado ++ "\n")
    putStrLn "Nome do arquivo para salvar os dados:"
    fileName <- getLine
    writeFile fileName (conteudoDecifrado ++ "\n")
    hClose chavePrivada



-- funcao IO() para criptografia RSA de um texto
rsa_encrypt_text :: IO ()
rsa_encrypt_text = do
    chavePublica <- openFile "chaveRSA_Publica.txt" ReadMode
    chave    <- hGetLine chavePublica
    putStrLn "Insira o conteudo a ser criptografado:"
    conteudo <- getLine
    let (n,e) = read (chave) :: (Integer, Integer)
    let conteudoCifrado = criptografar conteudo (n,e)
    putStrLn (conteudoCifrado ++ "\n")
    putStrLn "Nome do arquivo para salvar os dados:"
    fileName <- getLine
    writeFile fileName (conteudoCifrado ++ "\n")
    hClose chavePublica



-- funcao IO() para criptografia RSA de um arquivo
rsa_encrypt_file :: FilePath -> IO ()
rsa_encrypt_file filePath = do
    chavePublica <- openFile "chaveRSA_Publica.txt" ReadMode
    chave    <- hGetLine chavePublica
    plainTextHandle <- openFile filePath ReadMode
    conteudo <- hGetLine plainTextHandle
    let (n,e) = read (chave) :: (Integer, Integer)
    let conteudoCifrado = criptografar conteudo (n,e)
    putStrLn (conteudoCifrado ++ "\n")
    putStrLn "Nome do arquivo para salvar os dados:"
    fileName <- getLine
    writeFile fileName (conteudoCifrado ++ "\n")
    hClose chavePublica
