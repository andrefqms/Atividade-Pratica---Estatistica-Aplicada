dados <- read.csv("planilha.csv", header=TRUE,sep=",",dec=".")

attach(dados)

#QUESTAO 1
#A)
graf = plot(volume.de.trafego,velocidade.media,pch = 1)


#B)O gráfico de dispersão pode ser usado quando uma variável depende de outra variável 
#ou quando as duas variáveis são independentes.O gráfico de dispersão também 
#é útil para verificar como dois conjuntos de dados comparáveis 
#concordam entre si em que nesse caso mostra variáveis estao fracamente relacionadas. 


#C)
correlacao = cor(x = velocidade.media,y = volume.de.trafego,method = "pearson")
print(correlacao)

#Como r é proximo de -1, temos um relacionamento forte e como r 
#é negativo, o r tem sentido descrecente.

#D)

modelo = lm(velocidade.media~volume.de.trafego)
coef(modelo)
print(summary(modelo))

#E)
graf = plot(volume.de.trafego,velocidade.media,pch = 1)
abline(lm(velocidade.media~volume.de.trafego), col="black")


#QUESTAO 2

#A)
graf = plot(volume.de.trafego,velocidade.media,pch = 1)
abline(lm(velocidade.media~volume.de.trafego), col="black")

#B)
correlacao = cor(volume.de.trafego**volume.de.trafego,velocidade.media,method = "pearson")
print(correlacao)
plot(volume.de.trafego**volume.de.trafego,velocidade.media,pch = 1)

#C)
correlacao = cor(log(volume.de.trafego),velocidade.media,method = "pearson")
print(correlacao)
plot(log(volume.de.trafego),velocidade.media,pch = 1)
abline(lm(velocidade.media~log(volume.de.trafego)), col="dark blue")

#D)
correlacao = cor(sqrt(volume.de.trafego),velocidade.media,method = "pearson")
print(correlacao)
plot(sqrt(volume.de.trafego),velocidade.media,pch = 1)
abline(lm(velocidade.media~sqrt(volume.de.trafego)), col="dark green")

#E)
correlacao = cor(1/sqrt(volume.de.trafego),velocidade.media,method = "pearson")
print(correlacao)
plot(1/sqrt(volume.de.trafego),velocidade.media,pch = 1)
abline(lm(velocidade.media~(1/sqrt(volume.de.trafego))), col="dark red")
       