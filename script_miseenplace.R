# ----- ANSCOMBE ---------
# Données
data(anscombe)
layout(matrix(1:4,2), respect=TRUE)
plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)

# Construction des modèles
m1 <- lm(anscombe$y1~anscombe$x1)
m2 <- lm(anscombe$y2~anscombe$x2)
m3 <- lm(anscombe$y3~anscombe$x3)
m4 <- lm(anscombe$y4~anscombe$x4)

# Affichage des nuages de points avec la droite de régression linaire
layout(matrix(1:4,2), respect=TRUE)
plot(anscombe$x1,anscombe$y1)
abline(m1,col='red')
plot(anscombe$x2,anscombe$y2)
abline(m2,col='red')
plot(anscombe$x3,anscombe$y3)
abline(m3,col='red')
plot(anscombe$x4,anscombe$y4)
abline(m4,col='red')

# Coefficients et R^2
summary(m1)
# Res : y1 = 3.0001 + 0.5001*x1 et R^2 = 66.65%

summary(m2)
# Res : y1 = 3.0001 + 0.500*x1 et R^2 = 66.62%

summary(m3)
# Res : y1 = 3.0025 + 0.4997*x1 et R^2 = 66.63%

summary(m4)
# Res : y1 = 3.0017 + 0.4999*x1 et R^2 = 66.67%

#C'est la m^me chose à chaque fois.

# ------- CO2 ----------
# Données et première visualisation
data(CO2)
d = CO2[CO2$Type=="Quebec", ]
head(d)
layout(matrix(1:2, 1), respect=TRUE)
plot(d$conc, d$uptake, main="uptake~conc")
plot(log10(d$conc), d$uptake, main="uptake~log10(conc)")

# Construction des modèles
m1 <- lm(d$uptake~d$conc)
m2 <- lm(d$uptake~log10(d$conc))

# Nuages de points et droite de régression linéaire
layout(matrix(1:2, 1), respect=TRUE)
plot(d$conc, d$uptake, main="uptake~conc")
abline(m1,col='red')
plot(log10(d$conc), d$uptake, main="uptake~log10(conc)")
abline(m2,col='red')

#
summary(m1)
# Res : uptake = 23.5 + 0.02*conc et R2 = 50,45%

summary(m2)
# Res : uptake = -30.632 + 25.393*log10(conc) et R2 = 75,01%

# En passant au log10, on obtient un meilleur modèle de régression linaire 
# -> On voit que le nuage de point est + applati avec le log10 donc permet de se rapporoche + d'uen droite