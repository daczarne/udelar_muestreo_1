#$###########
#### SIC ####
#$###########

rm(list=ls(all=TRUE))
setwd("C:/Users/Usuario/Desktop/08.06.2017/SIC")

options(stringsAsFactors = FALSE)

library(foreign)
library(survey)

d <- read.spss("Ejemplo.sav", to.data.frame=TRUE)

str(d)
# cada fila representa un persona
# numero es el número del hogar
# secc y segm no las vamos a usar
# e26 es el sexo
# e27 es la edad
# pobpcoac población por condición de actividad
# YHOG ingreso del hogar

# calcula el máximo del nper del hogar
perhog <- aggregate(d$nper, by=list(d$numero), max)

dim(perhog)
head(perhog)

boxplot(perhog$x)
hist(perhog$x)

#------------------------#
# Comentarios de la base #
# ECH 2015 Montevideo    #
#------------------------#

head(d,10)

(N_I=nrow(perhog)) # 17931 hogares
(N=nrow(d)) # 46216 personas 

# e26 Sexo, e27 Edad
round(table(d$e26)/N*100, 2)

# Los datos sin considerar el diseño no necesariamente estiman bien.
round(sum(d$pesoano*(d$e26=="Hombre"))/sum(d$pesoano)*100, 2)

# edadt5 edad en tramos de a 5 hasta 99
d$edadt5 <- cut(d$e27,
                c(seq(0,100,5),Inf),
                include.lowest=TRUE,
                right=FALSE)
table(d$edadt5)

# edadt3 edad en 3 tramos [0,18) [18,65) [65,+Inf)
d$edadt3 <- cut(d$e27,
                c(0,18,65,+Inf),
                include.lowest=TRUE,
                right=FALSE)
table(d$edadt3)

# d21_16 ... conexión a Internet
addmargins(table(d$d21_16))
table(d$d21_16)/sum(table(d$d21_16))

# dsestred13 Estratos según nivel socioeconómico
data.frame(addmargins(table(d$dsestred13[d$nper==1])))

# Siempre quiero tener un 1 en la base
d$uno = 1

# pobpcoac PoblaciÃ²n por condiciÃ²n de actividad
data.frame(addmargins(table(d$pobpcoac)))

#Recodifico pobpcoac
d$actividad<-as.character(d$pobpcoac)
d$actividad[d$actividad=="Desocupados buscan trab. por 1a. vez"]        <-"Desocupado"
d$actividad[d$actividad=="Desocupados propiamente dichos"]              <-"Desocupado"
d$actividad[d$actividad=="Desocupados en seguro de paro"]               <-"Desocupado"
d$actividad[d$actividad=="Inactivo: jubilado"]   						            <-"Inactivo"
d$actividad[d$actividad=="Inactivo: pensionista"]						            <-"Inactivo"
d$actividad[d$actividad=="Inactivo: estudiante"] 						            <-"Inactivo"
d$actividad[d$actividad=="Inactivo: rentista"]   						            <-"Inactivo"
d$actividad[d$actividad=="Inactivo: otro"]							  	            <-"Inactivo"
d$actividad[d$actividad=="Inactivo: realiza los quehaceres del hogar"] 	<-"Inactivo"

data.frame(addmargins(round(table(d$actividad)/N,2)))

# Desocupacion
0.04/0.53

# DiseÃ±o SIC
n_I <- 1000
set.seed(21062016)
s_I = cluster(d,'numero',n_I,method='srswor')
m.sic = getdata(d,s_I)
m.sic$N_I = N_I

sic<-svydesign(id=~numero, data=m.sic, fpc=~N_I)
summary(sic)

svytotal(~e26, sic, deff=TRUE)

#Para calcular el estimador y su desvÃ­o estimado tengo que crear la variable
y.M<-(m.sic$e26=="Mujer")
t.M<-aggregate(y.M,by=list(m.sic$numero),sum)$x

(N_I/n_I)*sum(t.M) #El estimador pi del total
sqrt(N_I^2*(1-n_I/N_I)*var(t.M)/n_I) #El desvÃ­o del estimador pi del total

#El Deff

#S2yW
S2yU_i<-aggregate(d$e26=="Mujer",by=list(d$numero),var)$x
S2yU_i[is.na(S2yU_i)]<-0
N_i<-aggregate(d$uno,by=list(d$numero),sum)$x
S2yW<-sum((N_i-1)*S2yU_i/sum(N_i-1))
#delta
delta<-1-S2yW/var(d$e26=="Mujer")
#Cov
YU_i<-S2yU_i<-aggregate(d$e26=="Mujer",by=list(d$numero),mean)$x
Cov<-(1/(N_I-1))*sum((N_i-N/N_I)*N_i*YU_i^2)

1+(N-N_I)/(N_I-1)*delta+Cov/(N/N_I*var(d$e26=="Mujer"))

#Como estima la  media (proporciÃ³n)
svymean(~e26, sic)
#Distinto de
25121/N
#Igual a
sum(m.sic$e26=="Mujer")/n_s
# O sea, utiliza el estimador alternativo 

svytotal(~d21_16, sic, deff=TRUE)


#DiseÃ±o SI

m.sic$N<-N
si<-svydesign(id=~1, data=m.sic, fpc=~N)
summary(si)

svytotal(~e26,si)
sqrt(N^2*(1-n_s/N)*var(y.M)/n_s) 

deff<-((526.5088/441.0228)^2)


svytotal(~d21_16, si, deff=TRUE)



