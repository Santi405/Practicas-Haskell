-- los tipos siempre empiezan con mayuscula
-- type es un "alias" que le damos a otro tipo de dato ya existente

-- Capo Bertoni

type Pos = (Int,Int)

type Par a = (a,a)

mult :: Par Int -> Int
mult (x,y) = x*y

copy :: a -> Par a
copy x = (x,x)

-- data es usado para crear un nuevo tipo de dato
-- con deriving (Show) creamos una instancia para hacerle saber al compilador que queremos que los nuevos tipos de dato sean imprimibles

data Answer = Yes | No | Unknown deriving (Show)

ans :: [Answer]
ans = [Yes,No,Unknown]

flip_ans :: Answer -> Answer 
flip_ans Yes = No
flip_ans No = Yes
flip_ans Unknown = Unknown


--1

type MyColor = (Float, Float, Float)

mezclar :: MyColor -> MyColor -> MyColor
mezclar (r1,g1,b1) (r2,g2,b2) = ( (r1+r2)/2, (g1+g2)/2, (b1+b2)/2 )

--2
-- ("Contenido de la linea", pos x del puntero)
type Linea = (String, Int) 

--2a
vacia :: Linea
vacia = ([],0)

--2b
moverIzq :: Linea -> Linea
moverIzq (l,p) = if (p >(length l)) then error "puntero excede tamano linea" else if (p == 0) then (l,0) else (l,p-1)

--2c
moverDer :: Linea -> Linea
moverDer (l,p) = if (p >(length l)) then error "puntero excede tamano linea" else if (p == (length l)) then (l,(length l)) else (l,p+1)

--2d
moverIni :: Linea -> Linea
moverIni (l,p) = if (p >(length l)) then error "puntero excede tamano linea" else (l,0)

--2e
moverFin :: Linea -> Linea
moverFin (l,p) = if (p >(length l)) then error "puntero excede tamano linea" else (l,(length l))

lineatest :: Linea
lineatest = ("linea de prueba",5)

--2f
insertar' :: Char -> Int -> Linea -> Linea
insertar' c i (l,p) = if (p >(length l)) then error "puntero excede tamano linea" else if (p /= i) then (insertar' c (i+1) (l,p)) else ((take i l)++ c:(drop i l),p+1)

insertar :: Char -> Linea -> Linea
insertar c (l,p) = insertar' c 0 (l,p)

--2g
borrar' :: Int -> Linea -> Linea
borrar' i (l,p) = if (p >(length l)) then error "puntero excede tamano linea" else if (p /= i) then (borrar' (i+1) (l,p)) else ((take (i-1) l) ++ (drop i l),p-1)

borrar :: Linea -> Linea
borrar (l,p) = borrar' 0 (l,p)


--3

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)


list1 = Consnoc 2 EmptyCL 3

list2 = Consnoc 2 (CUnit 3) 4

list3 = Consnoc 10 (Consnoc 20 EmptyCL 30) 40

list4 = Consnoc 9 (Consnoc 8 (CUnit 7) 6) 5

list5 = Consnoc EmptyCL (Consnoc (CUnit 1) EmptyCL (Consnoc 2 EmptyCL 3)) (Consnoc 4 (CUnit 5) 6)

list6 = Consnoc (Consnoc 1 EmptyCL 2) EmptyCL (Consnoc 3 EmptyCL 4)

--3a

headCL (EmptyCL) = error "empty list"
headCL (CUnit x) = x
headCL (Consnoc l ns r) = l

tailCL (EmptyCL) = error "empty list"
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc l EmptyCL r) = CUnit r
tailCL (Consnoc l ns r) = Consnoc (headCL ns) (tailCL ns) r

isEmptyCL EmptyCL = 1
isEmptyCL (CUnit x) = 0
isEmptyCL (Consnoc l ns r) = 0

isCUnit EmptyCL = 0
isCUnit (CUnit x) = 1
isCUnit (Consnoc l ns r) = 0


{- FUNCIONES AUX -}

-- agarra un elemento y lo pone por adelante
cons :: a -> CList a -> CList a
cons a EmptyCL = CUnit a
cons a (CUnit x) = Consnoc a EmptyCL x
cons a (Consnoc l ns r) = Consnoc a (cons l ns) r

-- agarra un elemento y lo pone por atras
snoc :: CList a -> a -> CList a
snoc EmptyCL a = CUnit a
snoc (CUnit x) a = Consnoc x EmptyCL a
snoc (Consnoc l ns r) a = Consnoc l (snoc ns r) a


borrarU EmptyCL = EmptyCL
borrarU (CUnit x) = EmptyCL
borrarU (Consnoc l ns r) = cons l ns

borrarP EmptyCL = EmptyCL
borrarP (CUnit x) = EmptyCL
borrarP (Consnoc l ns r) = snoc ns r


--3B
reverseCL :: CList a -> CList a 

reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc l ns r) = Consnoc r (reverseCL(ns)) l

--3C
--inits: [1,2,3,4] -> [ [], [1], [1,2], [1,2,3], [1,2,3,4] ]

inits' :: (CList a) -> CList (CList a) -> CList (CList a)
inits' EmptyCL ys = cons EmptyCL ys
inits' xs ys = (inits'(borrarU xs)) (cons xs ys)


inits :: CList a -> CList (CList a)
inits xs = inits' xs EmptyCL

--3D
--lasts: [1,2,3,4] -> [ [], [4], [3,4], [2,3,4], [1,2,3,4] ]
lasts' :: (CList a) -> CList (CList a) -> CList (CList a)
lasts' EmptyCL ys = cons EmptyCL ys
lasts' xs ys = (lasts'(borrarP xs)) (cons xs ys)

lasts :: (CList a) -> CList (CList a) 
lasts xs = lasts' xs EmptyCL

--3E
--concatCL: [[1,2,3,4],[5,10,15,20]] -> [1,2,3,4,5,10,15,10]

--concatenar 2 listas
concatCL' :: CList a -> CList a -> CList a
concatCL' EmptyCL EmptyCL = EmptyCL
concatCL' l1 EmptyCL = l1
concatCL' EmptyCL l2 = l2
concatCL' (CUnit x) (CUnit y) = Consnoc x EmptyCL y
concatCL' (CUnit x) (Consnoc l ns r) = Consnoc x (cons l ns) r 
concatCL' (Consnoc l ns r) (CUnit y) =  Consnoc l (snoc ns r) y
concatCL' (Consnoc l1 ns1 r1) (Consnoc l2 ns2 r2) = Consnoc l1 (concatCL' (snoc ns1 r1) (cons l2 ns2)) r2

--concatenar n listas
concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit x) = x
concatCL (Consnoc l ns r) = (concatCL' ((concatCL' l (concatCL ns))) r) 

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

-- data Maybe a = Nothing | Just a (ya definido en el preludio)

eval :: Aexp -> Int
eval (Num x) = x
eval (Prod en es) = (eval en) * (eval es)
eval (Div en es) = div (eval en) (eval es)

seval :: Aexp -> Maybe Int
seval (Num n) = Just n
seval (Prod n1 n2) = case ((seval n1), (seval n2)) of
                                                      (Nothing, v2) -> Nothing -- x*0 = 0*x = 0
                                                      (v1, Nothing) -> Nothing
                                                      ((Just v1), (Just v2)) -> Just (v1*v2)
seval (Div n1 n2) = case ((seval n1), (seval n2)) of
                                                      (Nothing, v2) -> Nothing
                                                      (v1, Nothing) -> Nothing
                                                      ((Just v1), (Just v2)) -> if v2 == 0 then Nothing else Just (div v1 v2)


-- Leaf (hoja) es un nodo vacio (como si fuera un NULL)
data Arbolbin a = Leaf | Nodo (Arbolbin a) a (Arbolbin a) deriving (Show)


-- la funcion member se fija si un elemento esta presente en un arbol binario
member a Leaf = False
member a (Nodo l n r) = (a==n) || (member a l) || (member a r)

-- lo mismo, ahora considerando al arbol pasado como argumento como un BST
memberBST a Leaf = False
memberBST a (Nodo l n r) | (a==n) = True
 | (a>n) = (member a r)
 | otherwise = (member a l) -- (a<n)

-- el mas a la izquierda del arbol
minimumBST :: Arbolbin a -> a
minimumBST (Nodo Leaf a r) = a
minimumBST (Nodo l a r) = minimumBST l

--5a (el mas a la derecha)
maximumBST :: Arbolbin a -> a
maximumBST (Nodo l a Leaf) = a
maximumBST (Nodo l a r) = maximumBST r


--5b (es BST?)
checkBST :: Ord a => Arbolbin a -> Bool
checkBST (Nodo Leaf a Leaf) = True
checkBST (Nodo Leaf a r) = True
checkBST (Nodo l a Leaf) = True
checkBST (Nodo l a r) = (checkBST l) && (checkBST r) && ( a > (maximumBST l)) && ( a <= (minimumBST r)) 

a = (Nodo Leaf 2 Leaf)  --es
a2 = (Nodo (Nodo Leaf 2 Leaf) 3 (Nodo Leaf 7 Leaf)) --es
a3 = (Nodo (Nodo Leaf 9 Leaf) 8 (Nodo Leaf 7 Leaf)) --no es


--checkBST a
--checkBST a2
--checkBST a3

--6a
completo :: a->Int->Arbolbin a

completo x 0 = Leaf
completo x d = let subArbol = (completo x (d-1))
    in (Nodo subArbol x subArbol)


--completo a2 3

--

--6b
balanceado :: a->Int->Arbolbin a

balanceado x 0 = Leaf
balanceado x n | odd n = let division = div (n-1) 2
                             subArbol = (balanceado x division)
                        in (Nodo subArbol x subArbol)
            | otherwise = let division = div (n-1) 2
                              subArbolL = (balanceado x division)
                              subArbolR = (balanceado x (division+1))
                        in (Nodo subArbolL x subArbolR)

--
--otra manera:

balanceado2 :: a->Int->Arbolbin a

balanceado2 x 0 = Leaf
balanceado2 x n | odd n = let division = div (n-1) 2
                              subArbol = (balanceado2 x division)
                        in (Nodo subArbol x subArbol)
            | otherwise = let division = div (n-1) 2
                              (l,r) = ((balanceado2 x division),(balanceado2 x (division+1)))
                        in (Nodo l x r)

--7 en carpeta

-- RBT

data Color = R | B deriving (Show)
data RBT a = E | T Color (RBT a) a (RBT a) deriving (Show)

memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT a E = False
memberRBT a (T _ l b r ) | a == b = True
 | a < b = memberRBT a l
 | a > b = memberRBT a r

makeBlack E = E
makeBlack (T _ l x r ) = T B l x r

-- le pasas los elementos del arbol por separado, te retorna el arbol armado
-- vemos que el arbol balanceado tiene la raiz roja, 
-- pero se soluciona luego con makeBlack()

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t) -- convierte en nodo negro a la raiz
    where 
ins x E = T R E x E
ins x (T c l y r ) | x < y = balance c (ins x l) y r --izq
 | x > y = balance c l y (ins x r) --der
 | otherwise = T c l y r --dos nodos no se repiten


--8

rbt_test = (T B E (3) E)

fromOrdList :: Ord a => [a] -> (RBT a)
fromOrdList [] = E
fromOrdList [y] = (T B E y E) -- este caso no hace falta, lo dejo solo para hacerlo mas grafico
fromOrdList (x:xs) = insert x (fromOrdList xs)

-- fromOrdList [1,2,3,4]


--9

lbalance :: Color -> RBT a -> a -> RBT a -> RBT a  -- no recursiva
lbalance B (T R (T R a x b) y c) z d = (T R (T B a x b) y (T B c z d))
lbalance B (T R a x (T R b y c)) z d = (T R (T B a x b) y (T B c z d))
lbalance c l a r = (T c l a r) 

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R b y (T R c z d)) = (T R (T B a x b) y (T B c z d))
rbalance B a x (T R (T R b y c) z d) = (T R (T B a x b) y (T B c z d))
rbalance c l a r = (T c l a r)


insert2 :: Ord a => a -> RBT a -> RBT a
insert2 x t = makeBlack (ins2 x t)
    where 
ins2 x E = T R E x E
ins2 x (T c l y r ) | x < y = lbalance c (ins2 x l) y r -- ahora, lbalance solo verificara el subarbol izquierdo (4->2comparaciones)
 | x > y = rbalance c l y (ins2 x r) -- ahora, rbalance solo verificara el subarbol derecho (4->2comparaciones)
 | otherwise = T c l y r 


--heaps / leftist heaps

type Rank = Int
data Heap a = EH | NH Rank a (Heap a) (Heap a) deriving (Show)


rank :: Heap a -> Rank
rank EH = 0
rank (NH r _ _ _) = r

makeH x a b = if rank a >= rank b then NH (rank b + 1) x a b
else NH (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 EH = h1
merge EH h2 = h2
merge h1@(NH _ x a1 b1) h2@(NH _ y a2 b2) =
 if x <= y then makeH x a1 (merge b1 h2)
 else makeH y a2 (merge h1 b2)

insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x h = merge (NH 1 x EH EH) h

findMin :: Ord a => Heap a -> a
findMin (NH _ x a b) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (NH _ x a b) = merge a b

 --10
-- merge llamada n veces

fromListN :: Ord a => [a] -> Heap a
fromListN [] = EH
fromListN (x:xs) = merge (NH 1 x EH EH) (fromListN xs)

-- merge llamada (log2 n) veces
fromList :: Ord a => [a] -> Heap a
fromList xs =
  let 
  
    joinByTwos rta (a:b:xs) = joinByTwos ((merge a b) : rta) xs
    joinByTwos rta (a:xs) = a : rta
    joinByTwos rta [] = rta

    toHeapList r [] = r
    toHeapList r (x:xs) = toHeapList ((NH 1 x EH EH) : r) xs

    fromList' len xs = fromList' (div (len+1) 2) (joinByTwos [] xs)

  in
    fromList' (length xs) (toHeapList [] xs)