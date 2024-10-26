import Data.Char (toLower)

{-
1. Input/Output
    Definite un’azione charCount :: IO () che legge un numero n da tastiera,
    poi n stringhe e alla fine stampa il numero di stringhe in cui appare ciascuna
    lettera.
-}
charCount :: IO ()
charCount = do  putStrLn "Quante stringhe vuoi leggere?"
                nS <- getLine
                let n = read nS :: Int
                strings <- readStrings n 
                let diz = zip ['a'..'z'] (repeat 0)
                let out = countOcc strings diz
                mapM_ (\(letter, count) -> putStrLn (letter : ": " ++ show count)) out

readStrings 0 = return []
readStrings n = do  putStrLn ("Inserisci una stringa (Rimanenti: " ++ show n ++ "):" )
                    x <- getLine 
                    xs <- readStrings (n-1)
                    return ((map toLower x):xs)

countOcc xs diz = merge (map (countOccAux diz) xs)
    where
        countOccAux [] _ = []
        countOccAux ((letter,count):ys) word
            |   inWord letter word  = (letter, count+1) : countOccAux ys word
            |   otherwise           = (letter, count) : countOccAux ys word

        merge (x:xs) = foldr mergeCounts x xs
            where
                mergeCounts = zipWith (\(c1,n1) (_, n2) -> (c1,n1+n2))

        inWord l [] = False
        inWord l (x:xs) = (l == x) || inWord l xs

{-
2. Nodi Equilibrati con Applicativi e Monadi
    Risolvere l’esercizio 3 dell’Homework 2 (Nodi Equilibrati) usando applicativi
    e monadi (in analogia con le funzioni che creano un albero o che rietichettano
    i nodi dei un albero visti nella Lezione ), in modo da evitare di dover usare
    (esplicitamente nel codice) parametri e risultati di ritorno ausiliari.
-}

data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving Show

newtype ST a = S (Int -> (Int, a))

app (S st) x = st x 

instance Functor ST where
    fmap f st = S (\s -> let (newState, a) = app st s in (newState, f a))

instance Applicative ST where
    pure x = S (\s -> (s, x))
    stf <*> stx = S (\s ->  let (s', f) = app stf s
                                (s'', x) = app stx s'
                            in (s'', f x))

instance Monad ST where
    return = pure
    st >>= f = S (\s ->let (s', x) = app st s
                        in app (f x) s')

nodiEquilibrati :: BinTree Int -> [Int]
nodiEquilibrati tree = evalState (nodiEquilibratiAux tree) 0
    where
        nodiEquilibratiAux :: BinTree Int -> ST [Int]
        nodiEquilibratiAux Empty = return []
        nodiEquilibratiAux (Node n sx dx) = do
            parentSum <- getState
            updateState (+ n)
            leftResult <-  nodiEquilibratiAux sx
            rightResult <- nodiEquilibratiAux dx
            updateState (subtract n)
            let subtreeSum = sommaNodi (Node n sx dx)
            let isBalanced = parentSum == subtreeSum
            let currentResult = if isBalanced then [n] else []
            return (currentResult ++ leftResult ++ rightResult)
        
        sommaNodi :: BinTree Int -> Int
        sommaNodi Empty = 0
        sommaNodi (Node n sx dx) = n + sommaNodi sx + sommaNodi dx
        
        evalState :: ST a -> Int -> a
        evalState (S st) s = snd (st s)

        getState :: ST Int
        getState = S (\s -> (s, s))

        updateState :: (Int -> Int) -> ST ()
        updateState f = S (\s -> (f s, ()))

{-
3. Monadi/Eccezioni
    Definire il tipo NatBin che rappresenta i numeri naturali come sequenze binarie.
    Potete definirlo come liste (di lunghezza fissata) di 0 e 1, oppure potete dare
    una definizione con data (ad esempio usando 3 costruttori, di cui uno sia la
    costante 0 e gli altri due. . . in ogni caso, immaginare di definire una “parola di
    memoria”, quindi prevedete una lunghezza massima costante).

    Definire un valutatore di espressioni aritmetiche su NatBin, analoghi a quel-
    li visti a lezione, ma considerare tutte le operazioni aritmetiche (+, ×, div, mod
    e -). Estendere il tipo Maybe in modo che il risultato di un’espressione possa
    essere eventualmente un’eccezione diversa a seconda dell’eventuale situazione
    anomala che si `e verificata: divisione per zero, numero negativo oppure overflow.

    Potete completare l’esercizio facendo in modo che il tipo NatBin sia un’istanza
    delle usuali classi Eq, Ord, Num, Show.
-}

newtype NatBin = NB [Bool]

-- Dimensione supportata: 16 bit
size :: Int
size = 16

maxInt :: Int
maxInt = 2^size -1

natBinToInt (NB []) = 0
natBinToInt (NB (b:bits)) = (if b then 2^(n-1) else 0) + natBinToInt (NB bits)
    where 
        n = length (b:bits)

intToNatBin n = NB (reverse (take size (intToBits n ++ repeat False)))
    where
        intToBits 0 = []
        intToBits n = 
            let (q, r) = n `divMod` 2 in (r == 1) : intToBits q


data NatBinError = DivByZero | NResult | Overf
    deriving (Show, Eq)

data MaybeNatBin a = JustNatBin a | Error NatBinError
    deriving (Show, Eq)

instance Functor MaybeNatBin where
    fmap f (JustNatBin a) = JustNatBin (f a)
    fmap _ (Error e) = Error e

instance Applicative MaybeNatBin where
    pure = JustNatBin
    (JustNatBin f) <*> (JustNatBin x) = JustNatBin (f x)
    (Error e) <*> _ = Error e
    _ <*> (Error e) = Error e

instance Monad MaybeNatBin where
    return = pure
    (JustNatBin x) >>= f = f x
    (Error e) >>= _ = Error e

data Expr = Const NatBin | Add Expr Expr | Mul Expr Expr | Div Expr Expr | Mod Expr Expr | Sub Expr Expr

eval :: Expr -> MaybeNatBin NatBin
eval (Const a) = if natBinToInt a > maxInt then Error Overf else JustNatBin a

eval (Add a b) =
    do
        x <- eval a
        y <- eval b
        let result = natBinToInt x + natBinToInt y
        if result > maxInt then Error Overf
        else return (intToNatBin result)

eval (Mul a b) = 
    do
        x <- eval a
        y <- eval b
        let result = natBinToInt x * natBinToInt y
        if result > maxInt then Error Overf
        else return (intToNatBin result)

eval (Div a b) =
    do
        x <- eval a
        y <- eval b
        if natBinToInt y == 0 then Error DivByZero
        else return (intToNatBin (natBinToInt x `div` natBinToInt y))

eval (Mod a b) = 
    do
        x <- eval a
        y <- eval b
        if natBinToInt y == 0 then Error DivByZero
        else return (intToNatBin (natBinToInt x `mod` natBinToInt y))

eval (Sub a b) = 
    do
        x <- eval a
        y <- eval b
        let result = natBinToInt x - natBinToInt y
        if result < 0 then Error NResult
        else return (intToNatBin result)

instance Show NatBin where
    show (NB bits) = concat (map (\b -> if b then "1" else "0") bits)

instance Eq NatBin where
    (NB xs) == (NB ys) = xs == ys

instance Ord NatBin where
    (NB xs) `compare` (NB ys) = xs `compare` ys