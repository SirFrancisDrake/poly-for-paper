
import Control.Monad (join)
import Data.Function (on)
import Data.List ((\\), find, foldl', group, groupBy, intersperse, sort, sortBy)
import Data.Maybe (isJust, fromJust)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

run :: Show a => Parser a -> String -> IO ()
run p input
    = case (parse p "" input) of
        Left err -> putStr "parse error at " >> print err
        Right x -> print x

mono :: String -> Monomial
mono input = case (parse monoP "" input) of
                Left err -> error ("can't recognize: " ++ input)
                Right x -> x

poly :: String -> Polynomial
poly input =  case (parse polyP "" input) of
                Left err -> error ("can't recognize: " ++ input)
                Right x -> Polynomial x

polyP = sepBy monoP (char '+')

monoP = do
    skipMany space
    coeff <- coeffP
    skipMany space
    pairs <- sepBy pairP (char '*')
    skipMany space
    return (Monomial coeff (collectMon pairs))

coeffP = do
    (skipMany space >> char '+' >> skipMany space >> doubleP)
    <|>
    (skipMany space >> try (char '-') >> skipMany space >> doubleP >>= return . (* (-1)) )
    <|>
    (skipMany space >> char '-' >> skipMany space >> return (-1))
    <|>
    (skipMany space >> doubleP)
    <|>
    (return 1)
    <?> "coefficient"

intP = many1 digit >>= return . read

doubleP :: Parser Double
doubleP = do
    first <- many1 digit
    second <- ((char '.' >> many digit)
               <|> (return ""))
    if second /= "" then return $ read $ first ++ "." ++ second
                    else return $ read first

pairP = do
    var <- many1 (noneOf "*+^ ")
    skipMany space
    pow <- do ((try (char '^') >> skipMany space >> intP)
               <|> (return 1))
    skipMany space
    return (var,pow)

data Monomial = Monomial 
    { m_coeff :: Coefficient 
    , m_powers :: [(Variable, Power)]
    }
    deriving ()

nil :: Monomial -> Bool
nil (Monomial 0 _) = True
nil _ = False

cLIPSnaive :: [(Polynomial -> Polynomial)] -> Polynomial -> Polynomial
cLIPSnaive fns target =
    let untilFixed fn trg = if fn trg == trg then trg
                                             else untilFixed fn (fn trg)
    in foldl' (\acc fn -> untilFixed fn acc) target fns

clips :: [(Polynomial -> Polynomial)] -> Polynomial -> Polynomial
clips fns target =
    fst $ foldl' (\acc@(apoly, afns) fn ->
            (cLIPSnaive (afns ++ [fn]) apoly, afns ++ [fn])) (target,[]) fns

substituteAll = clips subs

subs = [sub1, sub2,sub3,sub4,sub5,sub6]

sub1 = substitute ((mono "1z1"), (poly "-1x1 + -1y1")) []
sub2 = substitute ((mono "1z2"), (poly "-1x2 + -1y2 + 1x1^2 + 1x1*y1 + 1y1^2")) []
sub3 = substitute ((mono "1y1^3"), (poly "-1x1^3 + -1x1^2*y1 + -1x1*y1^2 + 2x1*x2 + 2y1*y2 + 1x1*y2 + 1x2*y1")) []
sub4 = substitute ((mono "1x2^2"), (poly "-1y2^2 + -1x1^4 + -1x2*y2 + 1y2*y1^2 + 3x1^2*x2 + 1x1^2*y2 + 1y2*x1*y1")) []
sub5 = substitute ((mono "1x1^5"), (poly "-1.5x1*x2*y2 + 1.5x1*y1^2*y2 + -1.5x1*y2^2 + 1.5x1^2*y1*y2 + 2.5x1^3*x2 + 1.5x1^3*y2")) []
sub6 = substitute ((mono "1y2^3"), (poly "-1x1*x2*y1*y2 + 1x1*y1*y2^2 + 2x1^2*x2*y2 + 1x1^2*y2^2 + -1x1^4*y2 + -1x2*y1^2*y2 + 1y1^2*y2^2")) []

instance Ord Monomial where
    (<=) = on (<=) (sort . m_powers)
    (>=) = on (>=) (sort . m_powers)
    (<) = on (<) (sort . m_powers)
    (>) = on (>) (sort . m_powers)
    -- that's because I REALLY need ordering only for ppr
    -- and then I want them sorted alphabetically

instance Eq Monomial where
    (==) (Monomial c1 ps1) (Monomial c2 ps2) = ps1 == ps2 && c1 == c2

eq :: Monomial -> Monomial -> Bool
eq = on (==) (sort . m_powers)

instance Eq Polynomial where
    (==) = on (==) (sort . p_monomials)

instance Show Monomial where
    show (Monomial c mps) = 
        let mpshow = concatMap showOne mps
            showc = case c of
                -1 -> "-1"
                1 -> ""
                otherwise -> if fromInteger (round c) == c 
                                then show $ floor c
                                else show c
            showOne (v,p) = case compare p 1 of
                                GT -> v ++ "^" ++ show p ++ "*"
                                EQ -> v ++ "*"
                                LT -> v ++ "^(" ++ show p ++ ")*"
        in reverse . (drop 1) . reverse $ showc ++ mpshow

instance Show Polynomial where
    show (Polynomial ms) = 
        let sh = concat $ intersperse " + " (map show (sort ms))
        in if sh == "" then "0"
                       else sh

instance Ring Monomial where
    add a@(Monomial c1 p1) b@(Monomial c2 p2)
        | eq a b = Monomial (c1+c2) (cleanupM p2)
        | otherwise = error "Monomials you're trying to sum are not equal"
    subt a@(Monomial c1 p1) b@(Monomial c2 p2)
        | eq a b = Monomial (c1-c2) p2
        | otherwise = error "Monomials you're trying to sum are not equal"
    mult (Monomial c1 p1) (Monomial c2 p2) =
        Monomial (c1 * c2) (cleanupM $ collectMon powers)
        where powers = p1 ++ p2

type Variable = String
type Power = Int
type Coefficient = Double

data Polynomial = Polynomial 
                { p_monomials :: [Monomial]
                }
    deriving ()

cleanupM :: [(Variable,Power)] -> [(Variable, Power)]
cleanupM ps = sortBy (on compare fst) $ filter (\p -> snd p /= 0) ps

cleanupP :: [Monomial] -> [Monomial]
cleanupP = map (\(Monomial c ps) -> Monomial c (cleanupM ps)) 

instance Ring Polynomial where
    add (Polynomial p1) (Polynomial p2) = Polynomial $ cleanupP $ collectR $ p1 ++ p2
    subt p1 p2 = add p1 (multByCoeff (-1) p2)
    mult (Polynomial p1) (Polynomial p2) = Polynomial $ cleanupP $ collectR $
                foldl' (\acc mon -> acc ++ (fn mon)) [] p1
                where fn mon = map (mult mon) p2

toPower :: Polynomial -> Int -> Polynomial
toPower p 1 = p
toPower p i = mult p (toPower p (i-1))

            -- what in where
powerOf :: Monomial -> Monomial -> Power
powerOf (Monomial _ ps) (Monomial _ ts) =
    let
        c a b = filter (\t -> (fst t) `elem` (map fst a)) b
        as = c ts ps
        bs = c ps ts
        nas = map snd $ sortBy (on compare fst) as
        nbs = map snd $ sortBy (on compare fst) bs
        minDifference = minimum $ zipWith div nbs nas
    in minDifference 
    

eliminate :: Monomial -> Monomial -> Monomial
eliminate what@(Monomial _ pse) whatFrom@(Monomial c pst) =
    let pow = powerOf what whatFrom
        changed = filter (\t -> (fst t) `elem` (map fst pse)) pst
        unchanged = pst \\ changed
        p1 = sortBy (on compare fst) pse
        p2 = sortBy (on compare fst) changed
        powers = zipWith (\(v1,p1) (v2,p2) -> (v1, p2 - p1 * pow)) p1 p2
    in Monomial c (unchanged ++ powers)

collect :: Polynomial -> Polynomial
collect (Polynomial ms) =
    let groups = groupBy eq $ sort ms
        fold gr = foldl' add (head gr) (drop 1 gr)
    in Polynomial $ filter (not . nil) $ map fold groups

collectR :: [Monomial] -> [Monomial]
collectR ms =
    let groups = groupBy eq $ sort ms
        fold gr = foldl' add (head gr) (drop 1 gr)
    in filter (not . nil) $ map fold groups

partOf :: Monomial -> Monomial -> Bool
partOf p1@(Monomial _ ps1) p2@(Monomial _ ps2) =
    (and $ map (\t -> (fst t) `elem` (map fst ps2)) ps1)
    &&
    (powerOf p1 p2 >= 1)


substitute :: (Monomial, Polynomial) -> [Monomial] -> Polynomial -> Polynomial
substitute (l@(Monomial _ leftSide), r@(Polynomial rightSide)) banned t@(Polynomial target) =
    let changed = filter ( \x -> (partOf l x) && (not $ or $ map (flip partOf x) banned) ) target
        unchanged = target \\ changed
        subst targ = mult (Polynomial [(eliminate l targ)]) (toPower r (powerOf l targ))
        sub targ = foldl' add (Polynomial []) (map subst targ)
    in add (Polynomial unchanged) (sub changed)

multByCoeff :: Double -> Polynomial -> Polynomial
multByCoeff i (Polynomial ms) = Polynomial $ map (\(Monomial c ps) -> Monomial (c*i) ps) ms

collectMon :: [(Variable, Power)] -> [(Variable, Power)]
collectMon [] = []
collectMon ((mv, mp):ms) =
    let findSimilarTerms = filter (\a -> fst a == mv) ms
        collectMon' (cv, cp) = (cv, foldl' (\acc c -> acc + snd c) cp findSimilarTerms)
    in ( (collectMon' (mv,mp)): (collectMon $ filter (\t -> fst t /= mv) ms))

class Ring a where
    add :: a -> a -> a
    subt :: a -> a -> a
    mult :: a -> a -> a

substituteLetter :: (Char,Char) -> Polynomial -> Polynomial
substituteLetter p targ = substituteAll $ applyPerm [p] targ

spkP :: Polynomial -> [Char] -> Bool
spkP p letters = p == symmetrizeOver p letters

spkPS :: Polynomial -> [Char] -> Bool
spkPS p letters = p == substituteAll (symmetrizeOver p letters)

symmetrizeOver :: Polynomial -> [Char] -> Polynomial
symmetrizeOver target vars =
    let permutations = permutationTables vars
        symmetrize targ = map (flip applyPerm targ) permutations
    in foldl' add (head $ symmetrize target) (tail $ symmetrize target)

applyPerm perm targ = poly (applyPermutation perm (show targ))

applyPermutation :: (Eq a) => [(a,a)] -> [a] -> [a]
applyPermutation rules objs =
    let rule obj = filter (\t -> fst t == obj) rules
        mbApply obj = if not . null $ rule obj then applySingle obj
                                               else obj
        applySingle obj = snd . head $ rule obj
    in map mbApply objs

permutationTables :: (Ord a) => [a] -> [ [(a,a)] ]
permutationTables xs = zipWith zip (repeat xs) (allPermutations xs)

allPermutations xs = removeDuplicates $ concatMap permGen (allRotations xs)

permGen :: [a] -> [[a]]
permGen [a] = [[a]]
permGen (x:xs) = (map ((++) [x]) (permGen xs)) ++ (map (flip (++) [x]) (permGen xs))

removeDuplicates :: (Ord a) => [[a]] -> [[a]]
removeDuplicates = (map head) . group . sort

allRotations :: [a] -> [[a]]
allRotations xs = map (\i -> rotateTimes xs i) [0..(length xs - 1)] 

rotateTimes :: [a] -> Int -> [a]
rotateTimes xs 0 = xs
rotateTimes xs i = rotateTimes (rotate xs) (i-1)

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

-- Linear independency
setCoeffTo :: Coefficient -> Monomial -> Monomial
setCoeffTo i (Monomial c ps) = (Monomial i ps)

setCoeffTo1 :: Monomial -> Monomial
setCoeffTo1 = setCoeffTo 1

setCoeffTo0 :: Monomial -> Monomial
setCoeffTo0 = setCoeffTo 0

getMonomials :: [Polynomial] -> [Monomial]
getMonomials polys = 
    let getCoeffs (Polynomial ms) = map setCoeffTo1 ms
        newCoeffs acc pol = filter (\x -> not (x `elem` acc)) pol
        fn acc pol = sort (newCoeffs acc (getCoeffs pol) ++ acc) 
    in foldl' fn [] polys

polyInCoeffMatrix :: Polynomial -> [Monomial] -> [Double]
polyInCoeffMatrix (Polynomial mons) coeffMons =
    let filterFn m = filter (eq m) mons
        getCoeff m = if not . null $ filterFn m
            then m_coeff $ head $ filterFn m
            else 0
    in map getCoeff coeffMons

neo :: [Polynomial] -> IO ()
neo ps =
    let monos = getMonomials ps
        coeffs = map (flip polyInCoeffMatrix monos) ps
        toPrint = (map show monos):(map (map cshow) coeffs)
    in putStrLn $ pprShow toPrint

cshow c = if fromInteger (round c) == c then show (round c)
                                        else show c

jeez :: Polynomial -> [Monomial] -> IO ()
jeez p ms = putStrLn $ pprShow [(map show ms), map cshow (polyInCoeffMatrix p ms)]

getCoeffs :: [Polynomial] -> [[Double]]
getCoeffs ps = map (flip polyInCoeffMatrix (getMonomials ps)) ps

pprShow matr
 | not . and $ map (\t -> length t == length (head matr)) matr =
    let errmsg = "Matrix ppr from Ppr.hs: that ain't exactly a matrix"
    in error errmsg
 | otherwise =
    let makeLength ln = (take ln) . ( flip (++) (repeat ' ') )
        uniLn = length . head $ matr
        lnth x matr = (last . sort $ map (length . (!! x)) matr) + 1
        lns = map (\x -> lnth x matr) [0..(length (head matr) - 1)]
        joinStr strs = join $ intersperse " | " strs
        header = joinStr $ zipWith makeLength lns (head matr)
        breaker = "\n\t" ++ (take (length header) $ repeat '-')
        newMatr = [""]: (sortBy ( on compare (!! 0) ) (tail matr))
        makeMatrix = join $ intersperse "\n\t" 
                                (map (joinStr . (zipWith makeLength lns)) 
                                     newMatr)
    in "\n\t" ++ header ++ breaker ++ makeMatrix ++ "\n"

showMatr mtr = putStrLn $ pprShow $ map (map cshow) mtr

gaussStep :: [[Double]] -> [[Double]]
gaussStep [] = []
gaussStep (x:xs) = x: (map (\l -> zipWith (\a b -> b - (head l) * a) (map (/head x) x) l) xs)

gauss' :: [[Double]] -> [[Double]]
gauss' [] = []
gauss' m@(x:xs) = x:(map (0:) (gauss' $ drop 1 $ map (drop 1) (gaussStep m)))

gaussStepAlexander :: [[Double]] -> ([[Double]], [Double])
gaussStepAlexander [] = []
    let indus xs = map (\l -> zipWith (\a b -> b - (head l) * a) (map (/head x) x) l) xs
    foldl' (\acc x -> (x:

gaussAlexander' :: [[Double]] -> [[Double]]
gaussAlexander' [] = []
gaussAlexander' m@(x:xs) = x:(map (0:) (gauss' $ drop 1 $ map (drop 1) (gaussStep m)))

gauss = showMatr . gauss'

boolToInt False = 0
boolToInt True = 1

rank mtr =
    let sumRow row = boolToInt (or (map (/=0) row))
    in foldl' (+) 0 (map sumRow (gauss' mtr))
