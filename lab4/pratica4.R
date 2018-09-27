# questao 1
# A
k = 10000
n = 10
s = 0
soma = 0
variancia = 0
vetor = vector()

for (i in 1:k) {
    distance = rnorm(n,500,10)
    media = mean(distance)
    soma = 0
    for (j in 1:n) {
      soma = soma + (distance[j] - media)**2
    }
    s[i] = soma/(n-1)
    variancia[i] = soma/n
}
hist(s,col = "dark red",border = "white")
hist(variancia,col = "dark blue",border = "white")

#B

k = 10000
n = 20
s = 0
soma = 0
variancia = 0
vetor = vector()

for (i in 1:k) {
  distance = rnorm(n,500,10)
  media = mean(distance)
  soma = 0
  for (j in 1:n) {
    soma = soma + (distance[j] - media)**2
  }
  s[i] = soma/(n-1)
  variancia[i] = soma/n
}
hist(s,col = "brown",border = "white")
hist(variancia,col = "red",border = "white")

#C
k = 10000
n = 30
s = 0
soma = 0
variancia = 0
vetor = vector()

for (i in 1:k) {
  distance = rnorm(n,500,10)
  media = mean(distance)
  soma = 0
  for (j in 1:n) {
    soma = soma + (distance[j] - media)**2
  }
  s[i] = soma/(n-1)
  variancia[i] = soma/n
}
hist(s,col = "black",border = "white")
hist(variancia,col = "orange",border = "white")
#D
k = 10000
n = 50
s = 0
soma = 0
variancia = 0
vetor = vector()

for (i in 1:k) {
  distance = rnorm(n,500,10)
  media = mean(distance)
  soma = 0
  for (j in 1:n) {
    soma = soma + (distance[j] - media)**2
  }
  s[i] = soma/(n-1)
  variancia[i] = soma/n
}
hist(s,col = "gray",border = "white")
hist(variancia,col = "brown",border = "white")
#E
k = 10000
n = 100
s = 0
soma = 0
variancia = 0

for (i in 1:k) {
  distance = rnorm(n,500,10)
  media = mean(distance)
  soma = 0
  for (j in 1:n) {
    soma = soma + (distance[j] - media)**2
  }
  s[i] = soma/(n-1)
  variancia[i] = soma/n
}
hist(s,col = "dark red",border = "white")
hist(variancia,col = "light blue",border = "white")


# QUESTAO 2

n = 100 
k = 10000 
erro1 = 0 
erro2 = 0 
medias = 0 
variancias = 0 
t1 = 0
t2 = 0 
for(i in 1:k){
  uniforme = runif(n, 0, 500)
  mediaUniforme = mean(uniforme)
  medias[i] = mediaUniforme
  soma = 0
  for(j in 1:n){
    soma = soma + (uniforme[j] - mediaUniforme
                    )** 2
  }
  variancias[i] <- soma/n
  t1[i] = 2*mediaUniforme
  
  t2[i] = ((n+1)/n)*max(uniforme)
  
}
erro1 = (t1-medias)** 2
var1 = erro1 - variancias
erro2 = (t2-medias)** 2
var2 = erro2 - variancias

hist(t1,main="Histograma do T1",border = "white", col="dark blue", xlab = "T1")
hist(t2, border = "white",xlab = "T2",col="red", main="Histograma do T2")

hist(erro1,border = "white", col="dark orange", main="Histograma do erro medio quadratico - T1")
hist(erro2,border = "white", col="light blue", main="Histograma do erro medio quadratico - T2")

hist(var1,border = "white", col="purple", main="Histograma do vies - T1",xlab = "Variância do T1")
hist(var2,border = "white",main="Histograma do vies - T2", col="dark green", xlab = "Variância do T2")

