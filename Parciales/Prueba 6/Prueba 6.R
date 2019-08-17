#$#############################
#### MUESTREO I - PRUEBA 6 ####
#$#############################

library(sampling)
library(survey)
library(stratification)
library(dplyr)

#$############
#### BASE ####
#$############

U <- read.table("U.txt", header=T)
U <- arrange(U, id)
N <- dim(U)[1]

#$####################
#### PARTE 1: sSI ####
#$####################

ssi <- read.table("sSI.txt", header=T)
datossi <- U[ssi$id,]
n <- dim(datossi)[1]
datossi$fpc <- rep(N, n)
datossi$pw <- rep(N/n, n)
datossi$pik <- rep(n/N, n)

svytotal(~y, svydesign(id=~1, data=datossi, fpc=~fpc))
svytotal(~y, svydesign(id=~1, data=datossi, weights=~pw))
svytotal(~y, svydesign(id=~1, data=datossi, probs=~pik))

#$####################
#### PARTE 2: sSY ####
#$####################

Ux <- arrange(U, x)
Ux$idx <- seq(1, dim(Ux)[1], 1)

ssy <- read.table("sSY.txt", header=T)
datossy <- Ux[ssy$id,]
n <- dim(datossy)[1]

a <- vector("numeric", n-1)
for(i in 1:n-1){
      a[i] <- datossy$idx[i+1] - datossy$idx[i]
}
(a <- unique(a)) # Intervalo de muestreo
ssy[1,1] # Arranque aleatorio
Ux[Ux$idx==4,"id"]

# Estmador de t_y
a*sum(datossy$y)

# Estimador de la varianza de t_y
sqrt((N^2/n)*(1-n/N)*var(datossy$y))

# Estimador de la varianza de t_y usando V_0
datossy$pik <- rep(1/a, n)
svytotal(~y, svydesign(id=~1, data=datossy, probs=~pik))

datossy$pw <- rep(a, n)
svytotal(~y, svydesign(id=~1, data=datossy, weights=~pw))

# m = 90
# pk = 1/(a*m)
# sqrt((1/(m*(m-1))) * sum((datossy$y/pk - (1/m) * sum(datossy$y/pk))^2))

#$#####################
#### PARTE 3: PIPS ####
#$#####################

spips <- read.table("sPIPS.txt", header=T)
datospips <- U[spips$id,]
n <- dim(datospips)[1]

# Estimador del total
datospips$pik <- vector("numeric", n)
for(i in 1:n){
      datospips$pik[i] <- n * (datospips$x[i] / sum(U$x))
}

svytotal(~y, svydesign(id=~1, data=datospips, probs=~pik))

# m = n
# pk = datospips$pik/m
# sqrt((1/(m*(m-1))) * sum((datospips$y/pk - (1/m) * sum(datospips$y/pk))^2))

#$#####################
#### PARTE 3: STSI ####
#$#####################

sstsi <- read.table("sST1.txt", header=T)
datosstsi <- U[sstsi$id,]
datosstsi$ST1 <- sstsi$ST1
n <- dim(datosstsi)[1]

# Cantidad de observaciones por estrato (criterio: proporcional)
group_by(datosstsi, ST1) %>% summarise(obs=n(), varx=var(x), vary=var(y))
90/4

# Corroborando que no sea proporcional a la varianza por estrato de x
U$quantx <- vector("numeric", N)
for(i in 1:N){
      if(quantile(U$x)[1] <= U$x[i] & U$x[i] <= quantile(U$x)[2]){
            U$quantx[i] <- 1
      }else if(quantile(U$x)[2] < U$x[i] & U$x[i] <= quantile(U$x)[3]){
            U$quantx[i] <- 2
      }else if(quantile(U$x)[3] < U$x[i] & U$x[i] <= quantile(U$x)[4]){
            U$quantx[i] <- 3
      }else{
            U$quantx[i] <- 4
      }
}
grupos <- as.data.frame(group_by(U, quantx) %>% summarise(N_h=n(), varx=var(x), nvar=sum(n()*var(x))))
sumx = as.numeric(grupos[4,"nvar"])
group_by(U, quantx) %>% summarise(N=n(), varx=var(x), nx=round((N*n()*var(x))/sumx,2))

# Estimaci?n del total y su desv?o
datosstsi <- left_join(datosstsi, grupos[c("quantx","N_h")], by=c("ST1" = "quantx"))
svytotal(~y, svydesign(id=~1, strata=~ST1, data=datosstsi, fpc=~N_h))

# Estimaci?n de V_0
n_h <- as.data.frame(group_by(datosstsi, ST1) %>% summarise(n_h=n()))
datosstsi <- left_join(datosstsi, n_h, by="ST1")

m = n
datosstsi$pik <- datosstsi$n_h / datosstsi$N_h
datosstsi$pk <- datosstsi$pik / m
svytotal(~y, svydesign(id=~1, data=datosstsi, strata=~ST1, probs=~pik))

#$#####################
#### PARTE 4: STSI ####
#$#####################

U <- U[order(U$x),]
boundaries <- strata.cumrootf(U$x, n=90, Ls=6)
U$h6 <- boundaries$stratumID
Nh <- boundaries$Nh
nh <- boundaries$nh

set.seed(100)
ids <- sampling::strata(data=U, stratanames="h6", size=nh, method="srswor")
muestra <- U[ids$ID_unit,]

n_h <- NULL
for(i in 1:length(nh)){
      n_h <- c(n_h, rep(nh[i], nh[i]))
}

N_h <- NULL
for(i in 1:length(Nh)){
      N_h <- c(N_h, rep(Nh[i], nh[i]))
}

muestra <- cbind(muestra, n_h, N_h)
muestra$pik <- n_h/N_h

svytotal(~y, svydesign(id=~1, strata=~h6, data=muestra, fpc=~N_h))
svytotal(~y, svydesign(id=~1, strata=~h6, data=muestra, probs=~pik))

#$###################################
#### PARTE 5: Estimador de raz?n ####
#$###################################

as.numeric(svyratio(~y, ~x, svydesign(id=~1, data=datossi, fpc=~fpc))$ratio)*sum(U$x)
sqrt(as.numeric(svyratio(~y, ~x, svydesign(id=~1, data=datossi, fpc=~fpc))$var)*(sum(U$x)^2))
sqrt(as.numeric(svyratio(~y, ~x, svydesign(id=~1, data=datossi, weights=~pw))$var)*(sum(U$x)^2))

#$##############################
#### FIN DE LA PROGRAMACI?N ####
#$##############################