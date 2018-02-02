def factorial(n):
    if (n == 1) return 1
    else return n*factorial(n-1)
print( factorial(5) )

X = lambda fact, n : 1 if (n == 1) else n*fact(fact, n-1)
Y = lambda fact, n : fact(fact, n)
print( Y(X, 5) )

print((lambda fact, n : fact(fact, n)) \
      (lambda fact, n : 1 if (n == 1) else n*fact(fact, n-1) , 5)) 
