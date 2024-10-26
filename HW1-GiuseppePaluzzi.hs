--  1. Rimozione di Duplicati
--   1.1 Dare la definizione di myTakeWhile e myDropWhile;

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p   []   = []
myTakeWhile p (x:xs) = if p x then x:myTakeWhile p xs else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p   []   = []
myDropWhile p (x:xs) = if p x then myDropWhile p xs else x:xs

--   1.2 Scrivere una funzione ricorsiva myRemoveDupsOrd che rimuove i
--       duplicati da una lista ordinata xs di lunghezza n in tempo O(n).
myRemoveDupsOrd :: Eq a => [a] -> [a]
myRemoveDupsOrd []  = []
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:y:xs)
    |   x == y      = myRemoveDupsOrd (y:xs)
    |   otherwise   = x:myRemoveDupsOrd (y:xs)

--   1.3 Scrivere una funzione myRemoveDups che rimuove i duplicati da una
--       qualsiasi lista xs di lunghezza n in tempo O(n log n), preservando l’ordine
--       originale delle prime occorrenze degli elementi rimasti. Ad esempio:
--           Prelude> myRemoveDups [5,2,1,2,5,7,2,1,2,7]
--           [5,2,1,7]

myRemoveDups xs = map fst (qSort snd (myRemoveDupsOrd'(qSort fst (zip xs [1,2..]))))

myRemoveDupsOrd' [] = []
myRemoveDupsOrd' [(x,y)] = [(x,y)]
myRemoveDupsOrd' ((x,x'):(y,y'):xs)
    |   x == y              = myRemoveDupsOrd' ((x,x'):xs)
    |   otherwise           = (x,x'):myRemoveDupsOrd' ((y,y'):xs)

qSort f [] = []
qSort f (x:xs) = qSort f smaller ++ [x] ++ qSort f larger
    where
        smaller = [a | a <- xs, f a < f x]
        larger = [b | b <- xs, f b >= f x]



--  2. Interdefinibilit`a di Funzionali
--   2.1 Definire il funzionale zipWith f xs ys senza decomporre liste, ma usando
--       un’espressione che contenga zapp, f ed eventualmente xs e ys.
zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x: zapp fs xs
zapp _ _ = []

myZipWith :: (a->b->c) -> [a] -> [b] -> [c]
myZipWith f xs ys = zapp (map f xs) ys

--   2.2 Abbiamo visto che zipWith è più generale di zip. Tuttavia si può definire
--       zipWith f xs ys usando zip e un paio di altri funzionali visti nella Lezione 3.
myZipWith' f xs ys = map (uncurry f) (zip xs ys)

--   2.3 Definire il funzionale map f xs senza decomporre xs, ma usando un’espressione
--       che contenga foldr, f e xs. Fare lo stesso usando foldl.
myMap _ []  = []
myMap f xs = foldr (\x txs -> f x:txs) [] xs

myMap' _ [] = []
myMap' f xs = foldl (\txs x -> txs ++ [f x]) [] xs

--   2.4 Argomentare brevemente sul perché non sia possibile definire foldl e
--       foldr usando map.

--   Mentre map si limita ad applicare una funzione agli elementi di una lista, foldr e 
--   foldl utilizzano un accumulatore che viene combinato gli elementi della lista 
--   Definire foldr e foldl utilizzando solo map non è possibile in quanto non fornisce 
--   un modo per fare accumulating parameters, ovvero 


--  3. Segmenti e sottoliste
--   3.1 Scrivere una funzione prefissi :: [a] → [[a]] che ritorna tutti i segmenti
--       iniziali di una lista (vedi funzione suffissi nella Lezione 2).
prefissi :: [a] -> [[a]]
prefissi   []   = [[]]
prefissi (x:xs) = []: map (x:) (prefissi xs)

--   3.2 Senza preoccuparsi dell’efficienza, ma usando i funzionali prefissi, suffissi
--       e altri funzionali dello standard Prelude, scrivere una funzione segSommaS ::
--       (Num a) ⇒ [a] → a → [[a]] che data una lista numerica xs e un valore s
--       restituisce tutti i segmenti (cioè sottoliste di elementi consecutivi) di xs di
--       somma s.

suffissi :: [a] -> [[a]]
suffissi      []    = [[]]
suffissi xs@(_:txs) = xs:suffissi txs

segSommaS [] _ = []
segSommaS xs s = concat (map (subSum s) (prefissi xs))
                    where
                        subSum s ys = filter (\ys ->  sum ys == s) (suffissi ys)

--   3.3 Scrivere una funzione sublSommaS :: (Num a) ⇒ [a] → a → [[a]] che
--       data una lista numerica e un valore s restituisce tutte le sottoliste (anche di
--       elementi non consecutivi) di somma s.

powerset [] = [[]]
powerset (x:xs) = ps ++ map (x:) ps
                    where ps = powerset xs
sublSommaS [] _ = []
sublSommaS xs s = filter (\ys -> sum ys == s) (powerset xs)