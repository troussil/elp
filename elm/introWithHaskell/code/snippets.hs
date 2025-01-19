si True valSiVrai valSiFaux = valSiVrai
si False valSiVrai valSiFaux = valSiFaux

numbersFrom x = x : (numbersFrom (x+1)) 
