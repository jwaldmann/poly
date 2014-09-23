module P where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Text.Parsec.ByteString
import qualified Text.Parsec.Char as C
import Text.Parsec

import qualified Data.List ( sort )

import Control.Applicative ((<$>))

type Name = BS.ByteString
data Poly = Poly [ Mono ] deriving Show
data Mono = Mono Integer [ (Name,Integer) ] deriving Show

get :: FilePath -> IO Poly
get fp = do
    Right p <- parseFromFile poly fp
    return p

poly :: Parser Poly
poly = do C.spaces ; Poly <$> many mono

key s = do r <- C.string s ; C.spaces ; return r

sign = 
    (<|>) ( do key "+" ; return 1 )
          ( do key "-" ; return $ negate 1 )

digit = do
    d <- C.digit
    return $ fromIntegral $ fromEnum d - fromEnum '0'

nat = do
    ds <- many1 P.digit
    C.spaces
    return $ foldl ( \ l r -> 10 * l + r) 0 ds

mono = do
    s <- sign; c <- nat
    xs <- many $ do
        key "*"
        n <- name
        e <- ( do key "^" ; nat ) <|> do C.spaces ; return 1
        return (n,e)
    return $ mkMono (s * c) xs

mkMono c xs = Mono c $ Data.List.sort xs

name :: Parser Name
name = BS8.pack <$> many1 C.alphaNum
