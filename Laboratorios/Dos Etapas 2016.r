
rm(list=ls(all=TRUE))

library(survey)

# SISI Ejemplo 4.3.2 del libro amarillo
data(mu284)
mu284
dmu284<-svydesign(id=~id1+id2,fpc=~n1+n2, data=mu284)
(ytotal<-svytotal(~y1, dmu284))
vcov(ytotal)

# SISI si se censaron las PSU es un ST
mu284$uno<-1
aux<-aggregate(mu284$uno,list(mu284$id1),sum)
mu284$fpcST<-nrow(aux)
mu284

dosST<-svydesign(id=~id1+id2, fpc=~fpcST+n2, data=mu284)
svytotal(~y1, dosST)

ST<-svydesign(id=~1, strata=~id1, fpc=~n2, data=mu284)
svytotal(~y1, ST)

# SISI si se censaron las SSU es un SIC
names(aux)<-c("id1","fpcSIC")
aux
mu284<-merge(mu284,aux)
mu284
dosSIC<-svydesign(id=~id1+id2, fpc=~n1+fpcSIC, data=mu284)
svytotal(~y1, dosSIC,na.rm=TRUE)

SIC<-svydesign(id=~id1, fpc=~n1, data=mu284)
svytotal(~y1, SIC, na.rm=TRUE)

# Uso del estimador de varianza aproximado.
# Por defecto
# options("survey.ultimate.cluster" = FALSE)

#Repito el resultado
(dmu284<-svydesign(id=~id1+id2,fpc=~n1+n2, data=mu284))
(ytotal<-svytotal(~y1, dmu284))
vcov(ytotal)

#Calculo a mano
#Previos
t_gi<-aggregate(mu284$n2/3*mu284$y1, by=list(id1=mu284$id1), sum)
S2i<-aggregate(mu284$y1, by=list(id1=mu284$id1), var)
fi<-c(5*5*(1-3/5)/3,7*7*(1-3/7)/3,8*8*(1-3/8)/3,5*5*(1-3/5)/3,9*9*(1-3/9)/3)

#VPSU
(VPSU_g<-50*45/5*var(t_gi$x)-(50/5)*(50/5-1)*sum(fi*S2i$x))
#VSSU
(VSSU_g<-(50/5)^2*sum(fi*S2i$x))
#V2ST
(V2ST<-VPSU_g+VSSU_g)

#V*
(Valt_g<-50*45/5*var(t_gi$x))
Valt_g/V2ST

# Si quiero que use de entrada el aproximado, cambio las opciones
options("survey.ultimate.cluster" = TRUE)
(dmu284<-svydesign(id=~id1+id2,fpc=~n1+n2, data=mu284))
(ytotal<-svytotal(~y1, dmu284))
vcov(ytotal)

# Lo apago para quedar de nuevo como por defecto.
options("survey.ultimate.cluster" = FALSE)

# Si especifico dos etapas pero no doy fpc hace el V_0
mu284$w<-mu284$n2/3*50/5
(dmu284<-svydesign(id=~id1+id2,weights=mu284$w, data=mu284))
(ytotal<-svytotal(~y1, dmu284))
vcov(ytotal)
# El V_0, lo que usa el survey
(1/5/4)*sum((t_gi$x*5/(5/50)-15080)^2)

