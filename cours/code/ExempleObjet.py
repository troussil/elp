#/bin/python3

class Humain(object):

    def saluer(self):
        print("bonjour")

class Commercant(Humain):

    def demanderPaiement(self, x):
        print("cela nous fait " + str(x) + " euros, svp")
    
class Fleuriste(Commercant):

    def preparerBouquet(self, commande):
        self.commencer()
        for i in range(commande.quantite):
            self.prendreFleur(commande.fleur)
        self.finir()
        self.demanderPaiement(commande.quantite *
                              commande.fleur.prixUnitaire)

    def commencer(self):
        pass

    def finir(self):
        pass

    def prendreFleur(self, nom):
        pass

class Dentiste(Humain):
    pass
    
class Bouquet(object):

    def __init__(self, quantite, fleur):
        self.quantite = quantite
        self.fleur = fleur

class Fleur(object):

    def __init__(self, nom, prixUnitaire):
        self.nom = nom
        self.prixUnitaire = prixUnitaire

rose = Fleur("rose", 1.5)
bouquetStandard = Bouquet(3, rose)

fred = Fleuriste()
fred.saluer()
fred.preparerBouquet(bouquetStandard)

kenneth = Dentiste()
kenneth.saluer()
kenneth.preparerBouquet(bouquetStandard) 

