module Utils
(
	putExprLn
) where

putExprLn :: (Show a) => a -> IO()
putExprLn = putStrLn . show
