module State_Moi where

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    -- fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi (\s -> (f $ fst $ g s, snd $ g s))


-- I'm not quite convinced this is a valid Applicative, I should check it at some point
instance Applicative (Moi s) where
    --pure :: a -> Moi s a
    pure a = Moi (\s -> (a, s))

    -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi (\s -> ((fst $ f s) $ fst $ g s, snd $ g s ))


instance Monad (Moi s) where
    return = pure
    
    {- (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b -}
    (Moi f) >>= g =
        Moi h
        where h s = (runMoi $ g $ fst $ f s) $ snd $ f s


