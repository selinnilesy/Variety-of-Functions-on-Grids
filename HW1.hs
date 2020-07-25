module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where

-- do not modify the module declaration above!
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code 
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
-- Fellowship of the Grid (25, 5, 5, 5 points)
form :: [a] -> (Int, Int) -> [[a]] 
makeList :: [a] -> Int -> [a]

drop1 0 y = y          
drop1 _ [] = []
drop1 n (x:y) = (drop1 (n-1) y)

makeList (x:xs) 0 = []
makeList [] 0 = []
makeList (x:xs) y = x: (makeList (xs) (y-1))

form _ (0,0) = []
form _ (0,y) = []
form (lst) (x,y) = (makeList (lst) y) : (form (drop1 y (lst)) (x-1,y))

constGrid :: a -> (Int, Int) -> [[a]]
makeList2 :: a -> Int -> [a]
makeList2 a 0 = [] 
makeList2 a y = a: (makeList2 (a) (y-1))

constGrid a (0,0) = [[]]
constGrid a (0,y) = []
constGrid a (x,y) = (makeList2 a y) : constGrid a (x-1,y)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = (x) ++ (flatten xs)

access :: [[a]] -> (Int, Int) -> a 
access (xs) (width,height)  = xs !! width !! height

-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice_col :: Int -> Int -> [a] -> [a]
slice_col from to xs = take (to - from ) (drop from xs)

slice_row (lst) (i1,i2) (j1,j2) | (i1==i2) = []
slice_row (lst) (i1,i2) (j1,j2) =  (slice_col j1 j2 (lst !! i1)): (slice_row (lst) (i1+1,i2) (j1,j2))

slice (lst) (i1,i2) (j1,j2) = slice_row (lst) (i1,i2) (j1,j2)

vcat :: [[a]] -> [[a]] -> [[a]]

vcat (lst1) (lst2) = lst1 ++ lst2

hcat :: [[a]] -> [[a]] -> [[a]]

hcat [] [] = []
hcat (x:xs) (x1:xs1) = (x++x1) : (hcat (xs) (xs1))
without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]

remove_col :: Int -> Int -> [a] -> [a]
remove_row :: Int -> Int -> [[a]] -> [[a]]

remove_col 0 0 (x) = x
remove_col 0 to (x:xs) = remove_col 0 (to-1) (xs)
remove_col from to (x:xs) = x : remove_col (from-1) (to-1) (xs)

remove_c [] (j1,j2) = []
remove_c (x:xs) (j1,j2) = (remove_col j1 j2 (x)) : (remove_c (xs) (j1,j2))

remove_row 0 0 (x) = x
remove_row 0 to (x:xs) = remove_col 0 (to-1) (xs)
remove_row from to (x:xs) = x : remove_col (from-1) (to-1) (xs)

without (x:xs) (i1,i2) (j1,j2) = remove_c (remove_row i1 i2 (x:xs)) (j1,j2)
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]

how_many_column :: [a] -> Int -> Int
how_many_row :: [[a]] -> Int -> Int

how_many_column [] j = j
how_many_column (x:xs) j = how_many_column (xs) (j+1)

how_many_row [] i = i
how_many_row (x:xs) i = how_many_row (xs) (i+1)

check_eq [] [] = [(0,0)]
check_eq (x:xs) (xp:xsp) = if (x==xp) then (check_eq (xs) (xsp))
                           else []
                           
check_next_rows total@(x:xs) pattern@(xp:xsp) row_p row_g column_p index_r index_r_p index index2 sub_count result | (sub_count==row_p || index_r==(row_g) || index_r_p==(row_p)) = (index_r-row_p,index):result
check_next_rows total@(x:xs) pattern@(xp:xsp) row_p row_g column_p index_r index_r_p index index2 sub_count result | (index2==column_p) = check_next_rows total pattern row_p row_g column_p (index_r+1) (index_r_p+1) (index-column_p) 0 (sub_count+1) result      
check_next_rows total@(x:xs) pattern@(xp:xsp) row_p row_g column_p index_r index_r_p index index2 sub_count result  = 
                                 if ((total!!index_r!!index)==(pattern!!index_r_p!!index2)) then check_next_rows total pattern row_p row_g column_p index_r index_r_p (index+1) (index2+1) sub_count result
                                                                                                    else result



find_row _ (total) (xp:xsp) row_g column_p column_g row_p index_r result index | index>column_g-column_p = result
find_row (x:xs) (total) (xp:xsp) row_g column_p column_g row_p index_r result index = 
                        if (take column_p (x:xs)==xp)
                              then if(row_p>1) then find_row (xs) (total) (xp:xsp) row_g column_p column_g row_p index_r ((check_next_rows (total) (xp:xsp) row_p row_g column_p (index_r+1) 1 index 0 1 result)) (index+1)
                                               else find_row (xs) (total) (xp:xsp) row_g column_p column_g row_p index_r ((index_r,index):result) (index+1)
                              else find_row (xs) (total) (xp:xsp) row_g column_p column_g row_p index_r result (index+1)


check_row (l) (xp:xsp)  row_g column_p column_g row_p index result | (row_g-index < row_p) = result
check_row (x:xs) (xp:xsp)  row_g column_p column_g row_p index result = (find_row (x) (x:xs) (xp:xsp) row_g column_p column_g row_p index result 0) ++ (check_row (xs) (xp:xsp) row_g column_p column_g row_p (index+1) result)
                        
matches2d (x:xs) (xp:xsp) = do
                           let row_g = (how_many_row (x:xs) 0)
                           let column_p = (how_many_column (xp) 0)
                           let row_p = (how_many_row (xp:xsp) 0)
                           let column_g = (how_many_column (x) 0)
                           if ((column_g < column_p) || (row_g < row_p))
                                    then []
                                    else if ((column_g * row_g)==(column_p * row_p))
                                                then (check_eq (x:xs) (xp:xsp) )
                                                else check_row (x:xs) (xp:xsp) row_g column_p column_g row_p 0 []
                                                          
                                                                                   
----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
