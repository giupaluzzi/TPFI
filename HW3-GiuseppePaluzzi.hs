{-
1. Insomnia
    Scrivere una funzione Haskell che genera la lista infinita di caratteri
        insonnia = "1 sheep 2 sheep 3 sheep 4 sheep ...". 
    Provare a scrivere un “one-liner”, cioè un programma che semplicemente 
    compone opportunamente funzioni.
    Può essere utile la funzione show :: Show a => a => String che trasforma
    un elemento di qualsiasi tipo che implementa la classe Show in una stringa,
    cioè una lista di caratteri.
-}
insonnia = concat (map (\n -> show n ++ " sheep ") [1..])

{-
2. Triangolo di Tartaglia
    Definite in Haskell la lista infinita di liste finite tartaglia, tale che
    tartaglia!!n sia l’n-esima riga del triangolo di Tartaglia, e quindi
    tartaglia!!n!!k sia il coefficiente binomiale (n k) 
-}
tartaglia = iterate tartagliaAux [1]
    where
        tartagliaAux xs = 1 : zipWith (+) xs (tail xs) ++ [1] 

{-
3. Numeri Fortunati
    Esercizio 3 (Numeri Fortunati) I numeri fortunati, introdotti da Stanislaw Ulam,
    sono definiti come segue:
        1. Dalla sequenza dei numeri naturali (escluso lo zero) tolgo tutti i secondi
        numeri, cioè i pari.
        2. il secondo numero rimasto è il 3 e quindi si tolgono tutti i terzi numeri
        tra i sopravvissuti (5, 11, 17, . . . ).
        3. ora si considera il terzo numero rimasto cioè il 7 e rimuovo tutti i settimi
        numeri (il primo è il 19) e così via, fino a ottenere tutti i numeri sopravvissuti
        a tutte le operazioni di “filtraggio”.
    Scrivere una funzione Haskell che genera lo stream dei numeri fortunati.
-}
luckyN = 1 : luckyNAux [3,5..] 3
    where
        luckyNAux (x:xs) h = x : luckyNAux cs (h+1)
            where  
                cs = map fst (filter (\(_, y) -> y `mod` x /= 0) (zip xs [h..]))

