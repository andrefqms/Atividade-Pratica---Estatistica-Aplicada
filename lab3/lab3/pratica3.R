
# LETRA A 

med10 = vector()
varia10 = vector()

med30 = vector()
varia30 = vector()

med50 = vector()
varia50 = vector()

for (i in 1:1000) {
  populacao10 = sample(1000, 10, TRUE)
  populacao30 = sample(1000, 30, TRUE)
  populacao50 = sample(1000, 50, TRUE)
  
  varia10[i] = var(populacao10)
  med10[i] = mean(populacao10)
  
  med30[i] = mean(populacao30)
  varia30[i] = var(populacao30)
  
  varia50[i] = var(populacao50)
  med50[i] = mean(populacao50)
  
}


hist(med10, main = "Histograma da média de amostras de tamanho 10 que tem reposição", border = "white", col = "red")
hist(varia10, main = "Histograma da variância das amostras de tamanho 10 que tem reposição", border = "white", col = "orange")

hist(med30, main = "Histograma da média de amostras de tamanho 30 que tem reposição", border = "white", col = "light blue")
hist(varia30, main = "Histograma da variância de amostras de tamanho 30 que tem reposição", border = "white", col = "dark red")

hist(med50, main = "Histograma da média das amostras de tamanho 50 com reposição", border = "white", col = "dark blue")
hist(varia50, main = "Histograma da variância de amostras de tamanho 50 que tem reposição", border = "white", col = "purple")



# LETRA B


media10 = vector()
var10 = vector()

media30 = vector()
var30 = vector()

media50 = vector()
var50 = vector()

for (i in 1:1000) {
  populacao10 = sample(1000, 10, FALSE)
  populacao30 = sample(1000, 30, FALSE)
  populacao50 = sample(1000, 50, FALSE)
  
  media30[i] = mean(populacao30)
  var30[i] = var(populacao30)
  
  media10[i] = mean(populacao10)
  var10[i] = var(populacao10)
  
  var50[i] = var(populacao50)
  media50[i] = mean(populacao50)
  
}


hist(media10, main = "Histograma da média das amostras de tamanho 10 sem repor", border = "white", col = "green")
hist(var10, main = "Histograma da variância das amostras de tamanho 10 sem reposição", border = "white", col = "blue")

hist(media30, main = "Histograma da média das amostras de tamanho 30 sem repor", border = "white", col = "gray")
hist(var30, main = "Histograma da variância das amostras de tamanho 30 sem reposição", border = "white", col = "dark green")

hist(media50, main = "Histograma da média das amostras de tamanho 50 sem repor", border = "white", col = "brown")
hist(var50, main = "Histograma da variância das amostras de tamanho 50 sem repor", border = "white", col = "pink")

# LETRA C
mean(populacao10)
mean(med10)
mean(media10)


var(populacao10)
var(var10)
var(varia10)

mean(populacao30)
mean(med30)
mean(media30)

var(populacao30)
var(var30)
var(varia30)

mean(populacao50)
mean(med50)
mean(media50)

var(populacao50)
var(var50)
var(varia50)

# QUESTAO 2

media = 0
variancias = 0
for (i in 1:1000) {
  dist = rpois(100,10)
  media[i] = mean(dist)
  variancias[i] = var(dist)
}
hist(media,main = "Histograma das medias",col = "black", border = "white")
hist(variancias,main = "Histogramas das variâncias", col = "dark gray",border = "white")


# QUESTAO 3
exp = 0
for (i in 1:1000) {
  distribuicao = rexp(100,5)
  T = 1/mean(distribuicao)
  exp[i] = T
}

hist(exp,col = "dark green", border = "white",main ="Histograma da distribuição")

