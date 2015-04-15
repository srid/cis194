module Party where
import Employee
import Data.Monoid ((<>))
import Data.Tree
import Data.List (sort)    
    
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) $ empFun emp + fun

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 <> emps2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
        | fun1 > fun2 = gl1
        | otherwise   = gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tree = (f (rootLabel tree) $ map (treeFold f) (subForest tree))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = let with    = glCons boss $ (mconcat . map snd) gls
                         without = (mconcat . map fst) gls in
                     (with, without)

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry max) . treeFold f where
    f emp []      = (GL [emp] (empFun emp), GL [] 0)
    f emp glPairs = nextLevel emp glPairs

readEmployeeTree :: String -> Tree Employee
readEmployeeTree = read
                    
main :: IO ()
main = readFile "company.txt" >>= (printResult . maxFun . readEmployeeTree)


printResult :: GuestList -> IO ()
printResult (GL emps totalFun) = putStr "Total fun: " >> print totalFun >> printEmployees (sort $ map empName emps)

printEmployees :: [String] -> IO ()
printEmployees []     = putStr ""
printEmployees (x:xs) = putStrLn x >> printEmployees xs
