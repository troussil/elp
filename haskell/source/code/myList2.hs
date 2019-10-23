data MyList a = Empty | AddToMyList a (MyList a)

emptyList = Empty
oneEltIntList = AddToMyList 5 emptyList
twoEltIntList = AddToMyList 2 oneEltIntList
oneEltCharList = AddToMyList 'a' emptyList
twoEltCharList = AddToMyList 'e' oneEltCharList
