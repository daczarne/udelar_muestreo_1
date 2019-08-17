#$##############################
#### SIMULACIONES INDICADOR ####
#$##############################

ing = 50000 * rlnorm(n=1000)
educ = rpois(n=1000, lambda=7)
educpa = rpois(n=1000, lambda=4)

data = as.data.frame(cbind(ing,educ,educpa))

data$iing = NA
for(i in 1:dim(data)[1]){
      data$iing[i] <- (data$ing[i]-min(data$ing))/(max(data$ing)-min(data$ing))
}

data$ieduc = NA
for(i in 1:dim(data)[1]){
      data$ieduc[i] <- (data$educ[i]-min(data$educ))/(max(data$educ)-min(data$educ))
}

data$ieducpa = NA
for(i in 1:dim(data)[1]){
      data$ieducpa[i] <- (data$educpa[i]-min(data$educpa))/(max(data$educpa)-min(data$educpa))
}

data$isc = NA
for(i in 1:dim(data)[1]){
      data$isc[i] = (data$iing[i]*data$ieduc[i]*data$ieducpa[i])^(1/3)
}

round(quantile(data$isc, probs=c(0.2,0.4,0.6,0.8,1)), 4)

#$##############################
#### FIN DE LA PROGRAMACI?N ####
#$##############################