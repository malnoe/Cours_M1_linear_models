# ----- 3.2 et 3.3 Estimation des paramètres et qualité du modèle ---------
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

# ---- CO2 ----------
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

# Infos des modèles
summary(m1)
# Res : uptake = 23.5 + 0.02*conc et R2 = 50,45%

summary(m2)
# Res : uptake = -30.632 + 25.393*log10(conc) et R2 = 75,01%

# En passant au log10, on obtient un meilleur modèle de régression linaire 
# -> On voit que le nuage de point est + applati avec le log10 donc permet de se rapporoche + d'uen droite

# On pourrait également passer par un modèle non linéaire de la forme
# Y = A(1-2^(x/k)) avec A le plateau en haut (pleine vie) et k le temps de demi-vie.
# Possibilité de faire de une descent de gradient ou de chercher un peu à la main.
# Regarder la fonction optim en R pour le faire faire par R.
layout(1, respect=TRUE)
results = list()
for(a in 30:50) {
  for(k in 0:400) {
    t = d$conc
    Y = a*(1-2^(-t/k))
    # points(t,Y, col=2)
    res = d$uptake - Y
    eqm = sum(res^2) / length(res)
    results[[length(results)+1]] = c(a=a, k=k, eqm=eqm)
  }
}
results = as.data.frame(do.call(rbind, results))


r = results[results$eqm==min(results$eqm),]
a = r[["a"]]
k = r[["k"]]

t = d$conc
Y = a*(1-2^(-t/k))



sc_reg = sum((d$uptake - mean(d$uptake))^2)
sc_res = sum((d$uptake - Y)^2)
sc_tot = sc_res + sc_reg
r2 = sc_reg / sc_tot
r2
a
k

plot(d$conc, d$uptake, main=paste0("uptake=", a, "*(1-2^(-conc/", k, ")) R^2=", signif(r2, 2)*100, "% "))
lines(t,Y, col=2)

# ------------ 3.4 Tests statistiques -----------

# Test de student avec mass::cats
#1. Modèle analytique : HWT = B0 + B1BWT + eps

#2. Hypothèse H0 et H1 :
# H0 : B1 =0 et H1 : B1=!0

#3. Modèle linaire + vérif que les résidus sont gaussiens
set.seed(1)
d = d_mass_cats_10 = MASS::cats[sample(1:nrow(MASS::cats),10),]
m = lm(Hwt~Bwt, d)
# Shapiro test on residuals
shapiro.test(m$residuals)
# --> p-valeur grande donc les résidus sont gaussiens (le test de shapiro teste si les résidus ne sont pas gaussiens)

#4, 5 et 6
# Student test
summary(m)
# --> t-value = 5.424, p-value = 0.000628
# p-value < 0.01 donc on peut conclure que les variables bwt et hwt sont liées. bwt explique hwt.


# Test de Fischer
#1. Modèle analytique : HWT = B0 + B1BWT + eps

#2. Hypothèse H0 et H1 :
# H0 : B1 =0 et H1 : B1=!0

#3. Modèle linaire + vérif que les résidus sont gaussiens
set.seed(1)
d = d_mass_cats_10 = MASS::cats[sample(1:nrow(MASS::cats),10),]
m = lm(Hwt~Bwt, d)
# Shapiro test on residuals
shapiro.test(m$residuals)
# --> p-valeur grande donc les résidus sont gaussiens (le test de shapiro teste si les résidus ne sont pas gaussiens)

#4,5,6
#Test de Fischer
anova(m)
# --> F-value = 29.417 p-valeur = 0.000628 < 0.05
# Le test est significatif. On rejette H0 et acceptons H1 au risque d'erreur de première espèce alpha = 0.05
# Il y a un lien entre HWT ET BWT


