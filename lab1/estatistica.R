dados <- read.csv("tabela.csv", header=TRUE,sep=",",dec=",")

attach(dados)

total <- length(origem)
prop.table(table(origem))*100

#frequencias
freq = table(dados$origem)
freq

freq_rel = prop.table(freq)
freq_rel

porcentagem = prop.table(freq)*100
porcentagem

percentual = prop.table(table(origem))
ni <- c(40,60)
summary(dados)

comprimento <- gsub(",", "", comprimento)   # remove comma
comprimento <- as.numeric(comprimento)

print(paste0("media do motor = ",mean(motor)) )
print(paste0("desvio padrao do motor = ",sd(motor)))

print(paste0("media do comprimento = ",mean(motor)) )
print(paste0("desvio padrao do comprimento = ",sd(comprimento)))

print(paste0("media do preco = ",mean(preco)) )
print(paste0("desvio padrao do preco = ",sd(preco)))

median(preco)
comprimento <- gsub(",", "", comprimento)   # remove comma
comprimento <- as.numeric(comprimento)
median(comprimento)
median(motor)

desvio1 = sd(preco)**2
varPreco = desvio1**2
print(paste0("variancia de preco = ", varPreco))
      

desvio2 = sd(comprimento)**2
varcomprimento = desvio2**2
print(paste0("variancia de comprimento = ", varcomprimento))

desvio3 = sd(motor)**2
varmotor = desvio3**2
print(paste0("variancia de motor = ", varmotor))


      
      
#grafico de pizza
table(origem)
pie(table(origem))
pie(table(origem),main="Origem",labels=c("40%","60%"),col=c(1,2))
legend("topright",fill=c(1,2),legend=c("nacional","importado"))

#histograma

hist(preco)

hist(preco,prob=T,col=2,border = 1,main="Histograma da variávelvel preco")

hist(comprimento)
hist(comprimento,prob=T,col=2,border = 1,main="Histograma da variávelvel comprimento")
hist(motor,prob=T,col=2,border = 1,main="Histograma da variávelvel motor")

boxplot(preco~origem,col =c("light blue","red"),main="Comparacao de preços de importado e nacional")
