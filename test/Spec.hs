import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import FunctorInstances

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity fa = fmap id fa == id fa 


functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x  =
  (fmap  g . fmap f) x == (fmap (g . f) x)


-- Identity instance of Arbitrary

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a


-- Pair instance of Arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

-- Three instance of Arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c 

-- Sum instance of Arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]


-- List instance of Arbitrary

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = sized $ \n -> do
      k <- choose (0,n)
      genList k Nil
    where
      genList :: Arbitrary a => Int -> List a -> Gen (List a)
      genList k xs
        | k <= 0 = return xs
        | otherwise = do
            a <- arbitrary
            genList (k - 1) (Cons a xs)


-- Ordered Tree instance of Arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = sized $ \n -> do
    k <- choose (0,n)
    genTree k Empty
    where
      genTree :: (Arbitrary a, Ord a) => Int -> Tree a -> Gen (Tree a)
      genTree k tree 
        | k <= 0 = return tree 
        | otherwise = do
          a <- arbitrary
          genTree (k - 1) (insert a tree)



      
main :: IO ()
main = hspec $ do
  describe "Identity Functor" $ do
    it "satisfies identity property" $ do
      property (functorIdentity :: Identity Int -> Bool)
    it "satisfies composition property" $ do
      property (functorCompose :: Fun String Int -> Fun Int Int -> Identity String -> Bool)  
  describe "Pair Functor" $ do
    it "satisfies identity property" $ do
      property (functorIdentity :: Pair String -> Bool)
    it "satisfies composition property" $ do
      property (functorCompose :: Fun String Int -> Fun Int Int -> Pair String -> Bool)
  describe "Three Functor" $ do
    it "satisfies identity property" $ do
      property (functorIdentity :: Three Int String -> Bool)
    it "satisfies composition property" $ do
      property (functorCompose :: Fun String Int -> Fun Int Int -> Three Int String -> Bool)
  describe "Sum Functor" $ do
    it "satisfies identity property" $ do
      property (functorIdentity :: Sum String Int -> Bool)
    it "satisfies composition property" $ do
      property (functorCompose :: Fun Int String -> Fun String Int -> Sum String Int -> Bool)
  describe "List Functor" $ do
    it "satisfies identity property" $ do
      property (functorIdentity :: List String -> Bool)
    it "satisfies composition property" $ do
      property (functorCompose :: Fun String Int -> Fun Int Int -> List String -> Bool)
  describe "Tree Functor" $ do
    it "satisfies identity property" $ do
      property (functorIdentity :: Tree String -> Bool)
    it "satisfies composition property" $ do
      property (functorCompose :: Fun String Int -> Fun Int Int -> Tree String -> Bool)
  
      
