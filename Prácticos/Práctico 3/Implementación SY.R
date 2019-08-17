#$#########################
#### IMPLEMENTACI?N SY ####
#$#########################

sorteo = function(N, n){
      eps = runif(n=1, min=0, max=N)
      
      m = seq(from=0, to=(n-1)*N, by=N) + eps
      M = matrix(0, N, 2)
      a = seq(from=0, to=n*(N-1), by=n)
      a2 = c(a[2:N], N*n)
      M[,1] = a
      M[,2] = a2
      
      muestra = NULL
      i = 1
      while(length(muestra) < n){
            for (k in 1:N)
                  if (M[k,1] < m[i] & M[k,2] >= m[i]) muestra = c(muestra,k)
                  i=i+1
      }
      print(muestra)
}

sorteo(N=173, n=70)

#$##############################
#### FIN DE LA PROGRAMACI?N ####
#$##############################