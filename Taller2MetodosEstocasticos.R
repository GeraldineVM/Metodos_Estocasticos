#####TALLER 2 MÉTODOS ESTOCÁSTICOS#####
#####PRESENTADO POR GERALDINE VEGA MONTENEGRO#####
install.packages("mvtnorm")
library(mvtnorm)
###Punto 1####
Media_Y<-0.25
Varianza_Y<-1.5
# Cálculo de la media y la varianza de K
media_K <- exp(Media_Y + Varianza_Y / 2)
varianza_K <- exp(2 * Media_Y + Varianza_Y) * (exp(Varianza_Y) - 1)
cat("Media de K:", media_K, "\n")
cat("Varianza de K:", varianza_K, "\n")
####Punto 2####
Media_CH<-10 #m/d
Varianza_CH<-200 #m/d
DesviacionSt_CH<-sqrt(Varianza_CH)
# Probabilidad de que la conductividad sea mayor que 30 m/d
probabilidad_30 <- 1 - plnorm(30, meanlog = Media_CH, sdlog = DesviacionSt_CH)
cat(" La Probabilidad de que la conductividad sea mayor que 30 m/d:", probabilidad_30*100, '%')
####Punto 3####
# Probabilidades de clases de textura
prob_arena <- 0.7
prob_arcilla <- 0.2
prob_turba <- 0.1
# Probabilidades de clases de conductividad para cada textura
prob_conductividad <- matrix(c(
  0.0, 0.0, 0.0, 0.1, 0.4, 0.3, 0.1, 0.0,
  0.3, 0.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0,
  0.1, 0.3, 0.3, 0.2, 0.1, 0.0, 0.0, 0.0
), nrow = 3, byrow = TRUE)
# Calcula la distribución de probabilidad de la conductividad para todo el acuífero
prob_total <- c(
  sum(prob_arena * prob_conductividad[1,]),
  sum(prob_arcilla * prob_conductividad[2,]),
  sum(prob_turba * prob_conductividad[3,])
)

# Muestra la distribución de probabilidad total de la conductividad para todo el acuífero
names(prob_total) <- c("Arena", "Arcilla", "Turba")
cat(" La Probabilidad de la conductivdad para todo el acuifero es", "\n")
print(prob_total)
####Punto 4####
MediaZ1 <- 10
MediaZ2 <- 25
DesviacionStZ1 <- sqrt(300)
DesviacionStZ2 <- sqrt(450)
Coef_correl <- 0.7
# a. Calcular la covarianza entre Z1 y Z2
cov_Z1_Z2 <- Coef_correl * DesviacionStZ1 * DesviacionStZ2
cat(" La covarianza entre Z1 y Z2 es",cov_Z1_Z2, "\n")
# b. Calcular el valor esperado de Y = Z1 + Z2
E_Y <- MediaZ1 + MediaZ2
cat(" El valor esperaro de Y = Z1 + Z2 es",E_Y, "\n")
# c. Calcula la varianza de Y = Z1 + Z2
Var_Y <- DesviacionStZ1^2 + DesviacionStZ2^2 + 2 * cov_Z1_Z2
cat(" La varianza de Y = Z1 + Z2 es",Var_Y, "\n")
####Punto 5####
Media <- c(10, 25) # Medias de Z1 y Z2
DesviacionSt <- matrix(c(DesviacionStZ1^2, cov_Z1_Z2, cov_Z1_Z2, DesviacionStZ2 ^2), nrow = 2) # Matriz de covarianza
# a. Calcular Pr[Z1 < 30]
Pr_Z1_menor_30 <- pnorm(30, mean = Media[1], sd = sqrt(DesviacionSt[1,1]))
cat(" La probabilidad de que Z1 sea menor que 30 es",Pr_Z1_menor_30, "\n")
# b. Calcular Pr[Z2 < 40]
Pr_Z2_menor_40 <- pnorm(40, mean = Media[2], sd = sqrt(DesviacionSt[2,2]))
cat(" La probabilidad de que Z2 sea menor que 40 es",Pr_Z2_menor_40, "\n")
# c. Calcular la probabilidad Pr[Z1 + Z2 < 50]
Pr_Z1_Z2_menor_50 <- pmvnorm(lower = c(-Inf, -Inf), upper = c(50 - Media[1], 50 - Media[2]), mean = Media, sigma = DesviacionSt)
cat(" La probabilidad de que Z1+Z2 sea menor que 50 es",Pr_Z1_Z2_menor_50, "\n")
# d. Calcular la probabilidad Pr[Z1 < 30 ⋂ Z2 < 40]
Pr_Z1_menor_30_inter_Z2_menor_40 <- pmvnorm(lower = c(-Inf, -Inf), upper = c(30 - Media[1], 40 - Media[2]), mean = Media, sigma = DesviacionSt)
cat(" La probabilidad de que Z1 sea menor que 30 y a la vez Z2 sea menor que 40 es",Pr_Z1_menor_30_inter_Z2_menor_40, "\n")
# e. Calcular la probabilidad Pr[Z1 < 30 ⋃ Z2 < 40]
Pr_Z1_menor_30_union_Z2_menor_40 <- Pr_Z1_menor_30 + Pr_Z2_menor_40 - Pr_Z1_menor_30_inter_Z2_menor_40
cat(" La probabilidad de que Z1 sea menor que 30 o que Z2 sea menor que 40 es",Pr_Z1_menor_30_union_Z2_menor_40, "\n")
