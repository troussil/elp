data IntList = Empty | AddToIntList Int IntList

emptyList = Empty
oneEltList = AddToIntList 5 emptyList
twoEltList = AddToIntList 2 oneEltList
