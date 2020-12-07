fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci(n - 2) + fibonacci(n - 1)