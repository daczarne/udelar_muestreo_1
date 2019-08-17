
rm(list=ls(all=TRUE))
load("ECH2015.RData")

library(survey)

#Población con N_i homogéneos
	Perhog<-aggregate(d$uno,by=list(d$numero),sum)
	head(Perhog)

	U_Ihh<-d[d$numero %in% Perhog$Group.1[Perhog$x %in% c(4)],] # c(4)
	head(U_Ihh)
	d<-U_Ihh
	#Esto lo tengo que volver a hacer
	(N_I<-length(hog<-unique(d$numero))) # 12650 (17931) hogares
	(N<-nrow(d)) # 37407 (46216) personas
	d$uno=1
###############################

#Diseño SIC
n_I<-1000
set.seed(21062016)
s_I<-sample(hog,n_I,replace=FALSE)
m.sic<-d[d$numero %in% s_I,]
(n_s<-nrow(m.sic))
m.sic$N_I<-N_I

sic<-svydesign(id=~numero, data=m.sic, fpc=~N_I)
summary(sic)

svytotal(~e26, sic, deff=TRUE)

#Para calcular el estimador y su desvío estimado tengo que crear la variable
y.M<-(m.sic$e26=="Mujer")
t.M<-aggregate(y.M,by=list(m.sic$numero),sum)$x

(N_I/n_I)*sum(t.M) #El estimador pi del total
sqrt(N_I^2*(1-n_I/N_I)*var(t.M)/n_I) #El desvío del estimador pi del total

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

1+(N_I-1)/(N-N_I)*delta+Cov/(N/N_I*var(d$e26=="Mujer"))

svytotal(~d21_16, sic, deff=TRUE)

#S2yW
S2yU_i<-aggregate(d$d21_16=="Sí",by=list(d$numero),var)$x
S2yU_i[is.na(S2yU_i)]<-0
N_i<-aggregate(d$uno,by=list(d$numero),sum)$x
S2yW<-sum((N_i-1)*S2yU_i/sum(N_i-1))
#delta

delta<-1-S2yW/var(d$d21_16=="Sí")

YU_i<-S2yU_i<-aggregate(d$d21_16=="Sí",by=list(d$numero),mean)$x
Cov<-(1/(N_I-1))*sum((N_i-N/N_I)*N_i*YU_i^2)

1+(N-N_I)/(N_I-1)*delta+Cov/(N/N_I*var(d$d21_16=="Sí"))