#questao 1
m <- 8000         
a <- 1           
c <- 5         
s <- 67       
vetor <- numeric()  
vetor[1] <- s        
for (i in 2:10000)
{
  vetor[i] <- (a * vetor[i-1] + c) %% m
}
u = vetor / m
hist(u,main = "Histograma de 1000 numeros", col = "blue",xlab = "Vetor dividido por M",ylab = "Frequência")

#questao 2
u <- runif(1000,0,1)
x <- sqrt(vetor)
hist()
#questao3
poisson <- rpois(1000,5)
hist(poisson,main = "Histograma de de 1000 valores", col = "blue")

#questao 4

vetor = numeric()
for (i in 1:1000) {
  vetor[i] = rnorm(1000,100*5,100*1)
}
n = 100
u = 5
valores = 1000
sigma = 1
hist(vetor)
a = seq(1:1000)
x <- list(a)
print(x)
x = as.list.numeric_version(x)
b =rapply(x, rnorm(valores,n+u,n*sigma))
print(b)
