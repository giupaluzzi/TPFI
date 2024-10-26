{-
    1. mergeSort “iterativo”
        1.1     Definire una funzione Haskell che segue la seguente idea bottom-up per
                implementare l’algoritmo mergeSort:
                    Data una lista xs, creare una lista di liste lunghe 1, ciascuna contenente
                    un elemento di xs, poi fondere a due a due le liste ordinate
                    (eventualmente lasciando inalterata l’ultima lista quando il numero
                    delle liste `e dispari), finch`e non rimane un’unica lista ordinata.
                Ad esempio, cominciando con [5,3,4,2,1] la funzione calcola le seguenti
                liste: [[5],[3],[4],[2],[1]], poi [[3,5],[2,4],[1]], [[2,3,4,5],[1]] e
                infine [[1,2,3,4,5]] da cui viene estratto il risultato finale [1,2,3,4,5].
-}
mSortIterativo xs = mergeAll (subList xs)
    where
        subList = map (\x -> [x])

        mergeAll [] = []
        mergeAll [x] = x
        mergeAll xs = mergeAll (mPairs xs)

        mPairs [] = []
        mPairs [x] = [x]
        mPairs (x:y:xs) = merge x y : mPairs xs
        
        merge [] ys = ys
        merge xs [] = xs
        merge xs@(x:txs) ys@(y:tys)
            |   x <= y    = x:merge txs ys
            |   otherwise = y:merge xs tys
{-
        1.2     Accelerare la prima fase di questo algoritmo per trarre vantaggio da
                input “favorevoli”. La miglior`ıa dovrebbe assicurare un comportamento lineare
                in casi particolarmente fortunati.
-}
mSortIterativo' xs = mergeAll (subList' xs)
    where
        subList' [] = []
        subList' xs = subAux xs []
            where
                subAux  [] z = [z]
                subAux [x] z = [z ++ [x]]
                subAux (x:y:xs) z
                    |   x <= y      = subAux (y:xs) (z ++ [x])
                    |   otherwise   = (z ++ [x]) : subAux (y:xs) []

        mergeAll [] = []
        mergeAll [x] = x
        mergeAll xs = mergeAll (mPairs xs)

        mPairs [] = []
        mPairs [x] = [x]
        mPairs (x:y:xs) = merge x y : mPairs xs
        
        merge [] ys = ys
        merge xs [] = xs
        merge xs@(x:txs) ys@(y:tys)
            |   x <= y    = x:merge txs ys
            |   otherwise = y:merge xs tys




{-
    2. Alberi & funzionali sugli alberi
        Considerare le seguenti definizione di alberi binari:

        data BinTree a = Node a (BinTree a) (BinTree a) | Empty
        data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a

        2.1 Scrivere i funzionali mapBT, mapBT’, foldrBT, foldrBT’, foldlBT, e
            foldlBT’ che generalizzano agli alberi BinTree e BinTree’ gli analoghi 
            funzionali map, foldr e foldl sulle liste. Riflettete accuratamente sui tipi che devono
            avere e su quali siano, di fatto, i principi di ricorsione sugli alberi binari.
-}
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving Show

mapBT f Empty = Empty
mapBT f (Node n left right) = f n (mapBT f left) (mapBT f right)

foldrBT f e Empty = e
foldrBT f e (Node n left right) = f n (foldrBT f e right) (foldrBT f e left) 

foldlBT f e Empty = e
foldlBT f e (Node n left right) = foldlBT f (foldlBT f (f e n) left) right


data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a
    deriving Show

mapBT' f (Leaf n) = f n
mapBT' f (Node' left right) = Node' (mapBT' f left) (mapBT' f right)

foldrBT' f e (Leaf n) = f n e e
foldrBT' f e (Node' left right) = f e (foldrBT' f e right) (foldrBT' f e left) 

foldlBT' f e (Leaf n) = f e n
foldlBT' f e (Node' left right) = foldlBT' f (foldlBT' f e left) right

{-
        2.2 Scrivere poi le seguenti funzioni usando foldrBT e foldrBT’ (cercare di
            ottenere algoritmi lineari nel numero dei nodi):
            (a) numero dei nodi di un albero binario;
            (b) altezza dell’albero (= lunghezza in numero di archi del più lungo cammino 
            radice-foglia)
            (c) massimo indice di sbilanciamento (= massima differenza tra altezza
            sotto-albero destro/sinistro)

            Facoltativo: Gli alberi a branching illimitato si possono facilmente definire
            in Haskell come segue: data Tree a = R a [Tree a]. Come ai punti precedenti,
            scrivendo i funzionali mapT, foldrT e foldlT.
-}

nodes = foldrBT (\_ sx dx -> 1 + sx + dx) 0

hTree = foldrBT (\_ sx dx -> 1 + max sx dx) 0 

imbalance = foldrBT (\_ sx dx -> max (1 + abs (sx - dx)) (max sx dx)) 0 


nodes' = foldrBT' (\_ sx dx -> 1 + sx + dx) 0

hTree' = foldrBT' (\_ sx dx -> 1 + max sx dx) 0

imbalance' = foldrBT' (\_ sx dx -> max(1 + abs (sx-dx)) (max sx dx)) 0


data Tree a = R a [Tree a]
    deriving Show
mapT f (R n []) = R (f n) []
mapT f (R n xs) = R (f n) (map (mapT f) xs)


{-
    3. Nodi Equilibrati
        Un nodo u di un albero (considerare a piacere i BinTree oppure i Tree del-
        l’esercizio precedente) con valori numerici in tutti i nodi `e detto equilibrato se
        la somma delle chiavi nel cammino dalla radice a u (esclusa la chiave in u)
        `e esattamente uguale alla somma delle chiavi del sotto-albero radicato in u
        (compresa la chiave in u).
        Scrivere una funzione nodiEquilibrati :: Num a ⇒BinTree a → [a] che preso
        in input un albero, restituisce la lista (eventualmente vuota) contenente tutti
        i valori nei nodi equilibrati.
        Valutare la complessit`a della funzione.
-}
nodiEquilibrati tree = fst (nodiAux tree 0 [])
    where  
        nodiAux Empty c xs = (xs, c)
        nodiAux (Node n sx dx) c xs
            |   c == sommaNodi (Node n sx dx) = (n:xs, c)
            |   otherwise                     = (fst (nodiAux sx (c+n) xs) ++ fst (nodiAux dx (c+n) xs), c+n)
        sommaNodi Empty = 0
        sommaNodi (Node n left right) = n + sommaNodi left + sommaNodi right

-- La complessità della funzione è O(n) (con n = numero nodi) perché ogni nodo viene visitato una sola volta e 
-- le operazioni di concatenazione hanno costo O(m+p) con m = nodi albero sx, n = nodi albero dx 



{-
    4. Alberi Binari di Ricerca
        Scrivere una funzione Haskell listToABR :: Ord a ⇒ [a] → BinTree a che
        sistema i valori di una lista in un albero binario di ricerca.
        Determinare la complessit`a della funzione e chiedersi se si tratta di una
        complessit`a ottima rispetto al problema.
-}
listToABR [] = Empty
listToABR xs = abrAux (length xs) (qSort xs) 
    where  
        qSort [] = []
        qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger
            where
                smaller = [a | a <- xs, a < x]
                larger = [b | b <- xs, b >=  x] 
        abrAux _ [] = Empty
        abrAux l xs = Node x (abrAux l' sx) (abrAux l'' dx')
            where
                l' = l `div` 2
                l''=l-l'-1
                (sx,dx) = splitAt l' xs
                (x,dx') = (head dx, tail dx)

-- La complessità della funzione è O(n log n) (con n = length xs) e viene generato un albero bilanciato, 
-- permettendo quindi la ricerca in O(log n)



{-
    5. Derivazioni di programmi
        La funzione scanr :: (a → b) → b → [a] → [b] pu`o essere facilmente definita
        componendo map, foldr e tails (chiamata suffissi nell’Homework precedente):
                scanr f e = map (foldr f e) . tails
        Usare la definizione sopra come specifica per derivare una definizione efficiente
        (cio`e lineare nella lunghezza della lista) facendo manipolazioni algebriche, in
        analogia con quanto visto per scanl (slide lezione 9).


    Caso base
        scanr f e []                -- { definizione di scanr }
        map (foldr f e) . tails []  -- { definizione di tails }
        map (foldr f e) [[]]        -- { definizione di map }
        [foldr f e []]              -- { definizione di foldr }
        [e]

    Quindi:   scanr f e [] = [e]

    Caso Induttivo
        scanr f e (x:xs)                                        -- { definizione di scanr }
        map (foldr f e) . tails (x:xs)                          -- { definizione di tails }
        map (foldr f e) ((x:xs) : tails xs)                     -- { definizione di map }
        (foldr f e (x:xs)) : (map (foldr f e) . (tails xs))     -- { definizione di scanr }
        (foldr f e (x:xs)) : (scanr f e xs)                     -- { definizione di foldr }
        f x (foldr f e xs) : (scanr f e xs)                     -- { scanr (#) v [x,y,z] = [x#(y#(z#v)), y#(z#v), z#v, v], cioè
                                                                     scanr applica foldr a tutti i suffissi. Quindi
                                                                     head (scanr (#) v [x,y,z]) = foldr (#) v [x,y,z] }
        f x (head (scanr f e xs)) : (scanr f e xs)

    È quindi possibile definire le equazioni per scanr:

    scanr f e [] = [e]
    scanr f e (x:xs) = f x (head ys) : ys
        where ys = scanr f e xs 
-}