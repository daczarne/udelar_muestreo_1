#$##############################
#### El paquete Survey en R ####
#$##############################

rm(list=ls())
setwd("C:/Users/Daniel/Dropbox/UdelaR/CCEEA/Semestre 9/Muestreo I/Laboratorios/El paquete Survey en R")
setwd("C:/Users/dacza/Dropbox/UdelaR/CCEEA/Semestre 9/Muestreo I/Laboratorios/El paquete Survey en R")

library(survey)

data <- read.table("s.txt", header=TRUE)

#### Diseño Simple sin reemplazo ####

# Construye el objeto: ~1 indica una sola etapa de muestreo, fpc indica el
# tamaño de la población
p.s <- svydesign(id=~1, data=data, fpc=~fpc)
summary(p.s)
svytotal(~y, p.s) # Estimación del total de la variable y
svymean(~y, p.s) # Estimación de la media de la variable y
svyquantile(~y, p.s, quantile=c(.25, .5, .75), ci=TRUE) # Cuartiles estimados de y
svyratio(~y, ~x, p.s) # Estimador de la razón y/x
survey::svyby(~y,~d,p.s,svytotal) # Estimación del total por dominios

#### STSI ####
p.s <- svydesign(id=~1, strata=~st, data=data, fpc=~fpcst)
summary(p.s)
svytotal(~y, p.s) # Estimación del total de la variable y
svymean(~y, p.s) # Estimación de la media de la variable y
svyquantile(~y, p.s, quantile=c(.25, .5, .75), ci=TRUE) # Cuartiles estimados de y
svyratio(~y, ~x, p.s) # Estimador de la razón y/x


#### Diseño Simple con reemplazo ####
p.s <- svydesign(id=~1, strata=~st, data=data, weights=~pw)
summary(p.s)
svytotal(~y, p.s) # Estimación del total de la variable y (estimador pwr)
svymean(~y, p.s) # Estimación de la media de la variable y
svyquantile(~y, p.s, quantile=c(.25, .5, .75), ci=TRUE) # Cuartiles estimados de y
svyratio(~y, ~x, p.s) # Estimador de la razón y/x

#### Diseño Simple con reemplazo y factores de corrección ####
p.s <- svydesign(id=~1, strata=~st, data=data, weights=~pw, fpc=~fpcst)
summary(p.s)
svytotal(~y, p.s) # Estimación del total de la variable y (estimador pwr)
svymean(~y, p.s) # Estimación de la media de la variable y
svyquantile(~y, p.s, quantile=c(.25, .5, .75), ci=TRUE) # Cuartiles estimados de y
svyratio(~y, ~x, p.s) # Estimador de la razón y/x

#$##############################
#### FIN DE LA PROGRAMACIÓN ####
#$##############################