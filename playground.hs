-- -- test push

-- data Pukavec = Bily | Cerny

-- -- class Show a of
-- --    show :: a -> String

-- class Functor f of
--     fmap :: f a -> (a -> b) -> f b

-- class Functor m => Monad m of
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b

-- class Functor m => Monad m of

--     -- fmap

--     return :: a -> m a
--     join :: m (m a) -> m a
    
--     (>>=) :: f a -> (a -> f b) -> m b
    
--     x >>= f = join (fmap x f)
--     join x = x >>= id
--     -- x :: f a
--     -- f :: (a -> f b)
--     -- fmap :: f a -> (a -> f b) -> f (f b)
--     -- fmap x f :: f (f b)

--     join x = x >>= (\y -> return y)
--     -- x == m (m a)
--     -- (\y -> return y) == (a -> m a)
--     -- \(m a) -> return (m a) => m (m a)

-- instance Monad [] where
--     return x = [x]
--     x >>= f = concat . (fmap f) $ x
--     join = concat