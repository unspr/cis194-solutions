module Party where
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL list fun) = GL (e:list) (fun + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xlst xfun) (GL ylst yfun) = GL (xlst ++ ylst) (xfun + yfun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun xgl ygl = case compare xgl ygl of
                    GT -> xgl
                    _ -> ygl