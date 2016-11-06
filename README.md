# Testing Functor Instances With QuickCheck 

In this example I'm using the QuickCheck library to test some Functor properties.
It is interesting to notice the usage of Test.QuickCheck.Function to automatically
generate sample functions and also the way QuickCheck's combinators can be used 
in order to generate instances of the Arbitrary class for recursive data types like
Trees or Lists.

