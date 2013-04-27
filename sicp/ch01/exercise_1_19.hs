applyT (p,q) (a,b) =
	(a',b') where
		a' = b*q + a*q + a*p
		b' = b*p + a*q
	 
doubleT (p,q) =
	(p',q') where
		p' = p*p + q*q
		q' = 2*p*q + q*q
 
fib n = fibIter 1 0 0 1 n where
	fibIter a b p q count
		| count == 0 = b
		| even count = fibIter a b p' q' (count `div` 2)
		| otherwise = fibIter a' b' p q (count - 1)
		where
			(a',b') = applyT (p,q) (a,b)
			(p',q') = doubleT (p,q)

main = do
	putStrLn $ show $ map fst $ take 10 $ iterate (applyT (0,1)) (1,0)
	putStrLn $ show $ map fib [1..10]
