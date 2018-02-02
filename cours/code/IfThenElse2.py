myTrue = lambda x,y : x
myFalse = lambda x,y : y
myIf = lambda a,b,c : a(b,c)
print( myIf(myTrue,"b","c") )
print( myIf(myFalse,"b","c") )
