main = do
  putStrLn ""

--- Factorial Function (imperative)

fact 0 = 1
fact n = n * fact (n-1)

--- Factorial Function (Haskell-way)
fact2 n = product [1..n]

--- Fibbonaci
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


--- function that computes x^2 + 9

f x = x ^ 2 + 9

f2 = (+ 9) . (^ 2)


--- Lambdas in Haskell

f3 = \x -> x^2 + 9

f4 = \x y -> x * y - y

-- Write a function that takes a list of numbers and returns a list of just the postive ones.

  -- imperative (non-Haskell way)

empty list = (length list) == 0

positives list =
  if (empty list) then
    []
  else if (head list) > 0 then
    (head list) : (positives (tail list))
  else
    positives (tail list)
  --- Using pattern marching

positives2 [] = []
positives2 (h:t) =
  if h > 0 then h : (positives2 t)
  else (positives2 t)

  -- Using list comprehension
    -- This reads as:
      --list of x, where x comes from list and x > 0
positives3 list = [ x | x <- list, x > 0]

  -- Using the filter function
positives4 list = filter (> 0) list

  -- Most terse (using currying)
positives5 = filter (> 0)

-- sieve of eratosthenes
-- "/= is not equals"
sieve [] = []
sieve (p:nums) = p : sieve (filter (\x -> mod x p /= 0) nums)

-- -- lazy evaluation of primes
primes = sieve [2..]
-- first 50 primes
fifty = take 50 primes
-- first 100 primes
one_hundred = take 100 primes

-- quicksort in haskell (first element pivoting... BAD!)
quicksort [] = []
quicksort (x:xs) =
  quicksort [ y | y <- xs, y <= x]
  ++ [x]
  ++ quicksort [ y | y <- xs, y > x]

-- quicksort in haskell (middle element pivoting... better)
slice a b = (take (b - a)) . (drop a)
quicksort2 [] = []
quicksort2 xs =
  let n = length xs
      mid = quot n 2    -- index of pivot
      pivot = xs!!mid   -- value of pivot
      list = (slice 0 mid xs) ++ (slice (mid + 1) n xs) in
    quicksort2 [ y | y <- list, y <= pivot ]
    ++ [pivot]
    ++ quicksort2 [ y | y <- list, y > pivot ]

-- Expression tree fun!
data Node =
  Plus Node Node
  | Minus Node Node
  | Times Node Node
  | By Node Node
  | Num  Double
  deriving Show


evaluateTree node =
  case node of
    Plus n1 n2 -> (evaluateTree n1) + (evaluateTree n2)
    Minus n1 n2 -> (evaluateTree n1) - (evaluateTree n2)
    Times n1 n2 -> (evaluateTree n1) * (evaluateTree n2)
    By n1 n2 -> (evaluateTree n1) / (evaluateTree n2)
    Num x -> x
