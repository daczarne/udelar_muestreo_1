
rm(list=ls(all=TRUE))
setwd("C:/Users/Usuario/Downloads/lab")

library(foreign)

d <- read.dbf("Marco_2011_con_barrio_y_Sec_pol.dbf", as.is=TRUE)
d <- d[d$DPTO=="01",]
dim(d)
head(d)
str(d)

c(unique(d$CODLOC), unique(d$NOMLOC))

#quiero usar un diseño sistemático para estimar el total de la población
#de montevideo, tomando muestras de las ZONAs

(t<-sum(d$P_TOT))

U <- d[,c("DPTO","SECC","SEGM","ZONA","P_TOT")]
head(U)
dim(U)
plot(U$P_TOT, pch=16, col=rgb(0,0,1,0.2))
U[U$P_TOT > 3000,]

U <- U[U$P_TOT < 3000 & U$P_TOT > 0,]
plot(U$P_TOT, pch=16, col=rgb(0,0,1,0.2))
dim(U)

#################
N <- nrow(U)
a <- 40 #Quiero 2.5 cada 100 f=0.025
(n <- floor(N/a))
c <- N%%a
N; n; a; c

#################
set.seed(1234)
Azar <- sample(1:N)
#
SECCSEGMZONA <- order(U$SECC, U$SEGM, U$ZONA)
aux <- data.frame(U$SECC, U$SEGM, U$ZONA, SECCSEGMZONA)
head(aux)
tail(aux)

#
Y <- order(U$P_TOT)
head(Y, 100)
#
LOPEOR <- NULL
for(i in 1:n){LOPEOR<-c(LOPEOR,seq(i,N,n))}
head(LOPEOR, 100)


############Calculos###########

(VSI <- N^2*(1-n/N)*var(U$P_TOT)/n)*1e-6
sqrt(VSI)*1.96
sqrt(VSI)*1.96/t

#Todas las muestras
s_r <- c(rep(1:a,n),1:c)
table(s_r)

#SY y ordenamiento al azar
P_TOT <- U$P_TOT[Azar]
plot(P_TOT, pch=16, col=rgb(0,0,1,0.2))
t_r<-aggregate(P_TOT,by=list(s_r),sum)$x
plot(t_r, pch=16, col=rgb(0,0,1,1))

t/mean(t_r)

(VSY <- a*(a-1)*var(t_r))*1e-6
sqrt(VSY)*1.96
sqrt(VSY)*1.96/t

#SY y ordenamiento por y
P_TOT <- U$P_TOT[Y]
plot(P_TOT, pch=".", col="red")
t_r <- aggregate(P_TOT,by=list(s_r),sum)$x
plot(t_r)
(VSY<-a*(a-1)*var(t_r))*1e-6
sqrt(VSY)*1.96
sqrt(VSY)*1.96/t

summary(t_r)

#SY y el peor ordenamiento 
P_TOT <- U$P_TOT[Y][LOPEOR]
plot(P_TOT, pch=16, col=rgb(1,0,0,0.1))
t_r <- aggregate(P_TOT,by=list(s_r),sum)$x
plot(t_r)
(VSY<-a*(a-1)*var(t_r))*1e-6
sqrt(VSY)*1.96
sqrt(VSY)*1.96/t


#SY y ordenamiento SECC SEGM ZONA
P_TOT <- U$P_TOT[SECCSEGMZONA]
plot(P_TOT, pch=16, col=rgb(1,0,0,0.5))
t_r <- aggregate(P_TOT,by=list(s_r),sum)$x
(VSY <- a*(a-1)*var(t_r))*1e-6
sqrt(VSY)*1.96
sqrt(VSY)*1.96/t

