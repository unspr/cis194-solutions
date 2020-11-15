module Party where
import Employee
import Data.Tree
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e:list) (fun + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xlst xfun) (GL ylst yfun) = GL (xlst ++ ylst) (xfun + yfun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun xgl ygl = case compare xgl ygl of
                    GT -> xgl
                    _ -> ygl

treeFold :: Monoid b => (a -> b -> b) -> b -> Tree a -> b
treeFold f val t = foldr (\tree tmp -> treeFold f mempty tree <> tmp) 
                         (f (rootLabel t) val)
                         (subForest t)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e list = (foldr (<>) mempty (map (\(noBobList, hasBobList) ->
                                            moreFun noBobList hasBobList)
                                        list),
                    glCons e $ foldr (<>) mempty (map (\(noBobList, hasBobList) -> noBobList) list))

-- maxFun :: Tree Employee -> GuestList