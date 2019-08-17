#$#####################################
#### Ejercicio 3.15 Libro Amarillo ####
#$#####################################

rm(list=ls(all=TRUE))
setwd("C:/Users/Usuario/Desktop/08.06.2017/STSI")

library(survey)
library(sampling)

data(MU284)

# S82  Total de ediles de la municipalidad
# CS82 Total de ediles conservadores de la municipalidad
# SS82 Total de ediles social demócratas de la municipalidad

#---------------------------------------------------------------------#
# El objetivo es estimar la SS82, y compararla con el dato de la base #
# Dentro del estrato las observaciones deben ser lo más parecidas     #
# entre sí, y lo más heterogéneos entre los estratos                  #
#---------------------------------------------------------------------#

str(MU284)
sum(MU284$SS82)

#### Crea la variable estrato ####

# limites de los estratos
MU284$ST <- cut(MU284$S82, breaks= c(30,40,50,70,101))

(Nh <- table(MU284$ST))

(TS82 <- aggregate(MU284$S82, list(S82=MU284$ST), sum))
(SDS82 <- aggregate(MU284$S82, list(S82=MU284$ST), sd))

# Utilizamos el total de ediles S82 para estratificar, debido a la alta
# correlación entre ambas variables
plot(MU284$S82, MU284$CS82, pch=16, col="red")
cor(MU284$S82, MU284$CS82)

# Tamaño de muestra total
n <- 40 

#### Asignacion proporcional ####
(nhprop <- round(n*Nh/sum(Nh),0))
sum(nhprop)

# Tasas de muestreo por estrato
round(nhprop/Nh, 4)

#### Asignación óptima ####
(nhopt <- round(n*Nh*SDS82$x/sum(Nh*SDS82$x),0))

# Tasas de muestreo por estrato
round(nhopt/Nh,2)
sum(nhopt)

# Pega los tamaños de los estratos en MU284
MU284$Nh[MU284$ST=="(30,40]"]  = Nh[1]
MU284$Nh[MU284$ST=="(40,50]"]  = Nh[2]
MU284$Nh[MU284$ST=="(50,70]"]  = Nh[3]
MU284$Nh[MU284$ST=="(70,101]"] = Nh[4]

# Pega el tamaño de la poblacón
MU284$N = 284
View(MU284)

#### Muestra bajo diseño simple de tamaño 40 ####
set.seed(21062016)
s=sample(1:284, #etiquetas
          n, # n muestra
          replace = FALSE) # sin remplazo 

s <- MU284[s,]

#### Muestra bajo diseño STSI de tamaño 40 ####

# Para sacar muestras estratificadas conviene primero ordenar la base
# para asegurarse que el primer estrato es el primer estrato, etc.

MU284 <- MU284[order(MU284$S82),]

sprop <- strata(MU284, #marco
             "ST", #estrato
             nhprop, # tamaños de muestra
             method=c("srswor"), # diseño por estrato
             description=TRUE)
sprop <- getdata(MU284, sprop)
View(sprop)
Nh
nhprop

sopt <- strata(MU284,
            "ST",
            nhopt, 
            method=c("srswor"),
            description=TRUE)
View(sopt)
sopt <- getdata(MU284,sopt)
Nh
nhopt

# Carga la muestra SI
pSI <- svydesign(id=~1, # Muestra por conglo o directo (1 es directo)
                 # Puede definir con la tasa de muestreo o con los pesos
                 fpc=s$N, 
                 data=s)

svytotal(~SS82,pSI, deff=TRUE)
# nombre de la variable cuyo total quiero estimar
# el diseño
# el efecto diseño. Debería ser 1 en este caso.

# Carga diseño muestra STSI prop
pSTSIprop <- svydesign(id=~1, #id=1 muestreo directo
                    strata=~ST, #varaible que indica el estrato
                    fpc=~Nh,
                    data=sprop)

summary(pSTSIprop)
# otra forma
pSTSIprop <- svydesign(id=~1,
                    strata=~ST,
                    fpc=~ Prob,
                    probs = ~ Prob,
                    data=sprop)

# Total de la variable ediles social-demócratas (SS82)
svytotal(~SS82,pSTSIprop, deff=TRUE)
  # el deff es menor que uno entonces es más eficiente estratificar

# Obtengo estimaciones a nivel de subpoblaciones
svyby(~SS82, ~ST, pSTSIprop, svytotal, deff=TRUE)

# Ídem pero utilizando el óptimo
pSTSIopt <- svydesign(id=~1,
                   strata=~ST,
                   fpc=~Nh,
                   data=sopt)

svytotal(~SS82,pSTSIopt, deff=TRUE)
svyby(~SS82, ~ST, pSTSIopt, svytotal, deff=TRUE)

# ¿Cómo puede ser que el Deff con asignación óptima de más alto que con
# asignación proporcional?

#### Vuelve a tomar el STSI pero con muchos estratos ####

MU284 <- MU284[order(MU284$S82),]
MU284$ST <- c(rep(c(1:2),rep(16,2)),rep(c(3:20),rep(14,18)))
table(MU284$ST)
s20 <- strata(MU284, "ST", rep(2,20), method=c("srswor"),description=TRUE)
s20 <- getdata(MU284,s20)

s20$Nh <- c(16,16,rep(14,18))

pSTSI20 <- svydesign(id=~1,strata=~ST,fpc=~Nh,data=s20)
svytotal(~SS82,pSTSI20, deff=TRUE)

# Varianza en la población
round(aggregate(MU284$S82, list(MU284$ST), var),4)
sort(MU284$SS82)

#$################################
#### Construcción de estratos ####
#$################################

install.packages("stratification")
library(stratification)
n = 40
Ls = 4

# Construye los estratos segun dalenius
ST <- strata.cumrootf(MU284$S82, # Variable según la cual estratificar
                   n=n, # tamaño de muestra
                   Ls=Ls, # cantidad de estratos
                   nclass = 284) #
ST

# bh es el límite superior del estrato

MU284$STDH <- ST$stratumID

for (i in 1:Ls){
    MU284$Nh[MU284$STDH==i]<-ST$Nh[i]
  }

MU284 <- MU284[order(MU284$STDH),]

set.seed(1)
sDH=strata(MU284, "STDH", ST$nh , method=c("srswor"),description=TRUE)
sDH=getdata(MU284,sDH)

pSTSIDH=svydesign(id=~1, strata=~STDH, fpc=~Nh, data=sDH)
svytotal(~SS82, pSTSIDH, deff=TRUE)

#$##############################
#### FIN DE LA PROGRAMACIÓN ####
#$##############################