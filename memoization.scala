def fib(n: Int): Int = 
    if n <= 1 then 1
    else fib(n-1) + fib(n-2)

def memofib(n: Int): Int =
    val memo = Array.fill(n+1)(-99)
    def helper(x: Int): Int =
        if n <= 1 then 1
        else if memo(x) == -99 then
            memo(x) = helper(x-1) + helper(x-2)
        memo(x)
    
    helper(n)


//find the most money you can get from an 2d array of numbers, you can only go right and down

def optimization(arr: Array[Array[Int]], i: Int, j: Int): Int =
    val memo = ???
    def helper(variables: Int): Int =
        if memo(i, j) == -99 then
            A(i, j) + max(optimization(A, i, j+1), optimization(A, i, j+1))
    