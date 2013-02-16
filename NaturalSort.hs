module NaturalSort (nSort,test,natComp) where
    
import Data.Char
import Data.List

--
--
--
nSort   ::  [String] -> [String]
nSort s =   if (and . map allFloat) s then    
                let { readFloat = read :: String -> Float } 
                in  (map show . sortBy compare . map readFloat) s
            else    
                sortBy natComp s
    where allFloat  =   all (\x -> isDigit x || '.' == 'x')
    
--
--
--
natComp                                 ::  String -> String -> Ordering
natComp [] []                           =   EQ
natComp [] _                            =   LT
natComp _ []                            =   GT
natComp xxs@(x:xs) yys@(y:ys)
    | noDigit x && noDigit y && x == y  =   natComp xs ys
    | noDigit x || noDigit y            =   compare x y
    | nx == ny                          =   natComp rx ry
    | otherwise                         =   compare nx ny
    where   (nx,rx)     =   getNumber xxs
            (ny,ry)     =   getNumber yys
            noDigit     =   not . isDigit
            getNumber s =   let { digits = takeWhile isDigit s }
                            in (read digits :: Integer, drop (length digits) s)
 
test    =   nSort   ["def","abc"] == 
                    ["abc", "def"] &&
            nSort   ["1a", "2a", "10a", "3a"] == 
                    ["1a", "2a", "3a", "10a"] &&
            nSort   ["a","1a"] == 
                    ["1a", "a"] &&
            nSort   ["1a", "a"] == 
                    ["1a", "a"] &&
            nSort   ["abc 1 cde" , "abc 2 def", "abc 10ghi"] ==
                    ["abc 1 cde", "abc 2 def", "abc 10ghi"] &&
            nSort   ["media10.mysite.com", "media11.mysite.com",
                    "media1.mysite.com", "media2.mysite.com",
                    "media9999.mysite.com", "media0.mysite.com"] ==
                    ["media0.mysite.com", "media1.mysite.com", 
                    "media2.mysite.com", "media10.mysite.com", 
                    "media11.mysite.com", "media9999.mysite.com"] &&
            nSort   ["x2-y08", "x2-y7", "x2-g8", "x8-y8"] ==
                    ["x2-g8", "x2-y7", "x2-y08", "x8-y8"] &&
            nSort   ["1.00001", "1.1", "1.2"] ==
                    ["1.00001", "1.1", "1.2"]
                    
