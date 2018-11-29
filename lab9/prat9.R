#QUESTAO 1


#A)
dados <- read.csv("tabela.csv", header=TRUE,sep=",",dec=",")
attach(dados)

graf = plot(tempo,ganho,pch = 1)
abline(lm(ganho~tempo), col="black")

graf = plot(ions,ganho,pch = 1)
abline(lm(ganho~ions), col="black")

##

#B)

correlacao = cor(x = tempo,y = ganho,method = "pearson")
print(correlacao)

correlacao = cor(x = ions,y = ganho,method = "pearson")
print(correlacao)

#C)
model2 = lm(ganho~tempo,data = dados)
anova(model1,model2)

model3 = lm(ganho~ions+tempo,data = dados)
anova(model1,model2,model3)

#D)
#Anova permite avaliar afirmações sobre as médias de populações. A análise visa, fundamentalmente,
#verificar se existe uma diferença significativa. Quanto maior for R2, que 
#representa o quão bem o modelo prediz as respostas, maior capacidade de predizer tem o modelo .
#Dos 3 modelos acima, o que tem maior valor é o 3º, tempo e dose de Íons. Logo, 
#o modelo que mais se ajusta aos dados foi o terceiro. 



#QUESTAO 2

info <- read.csv("tabela2.csv", header=TRUE,sep=",",dec=".")
attach(info)


#A)

Y = cbind(Indice)
Y
X = cbind(rep(1,8), Num.Ataques, Duracao)
X
#B)

beta = solve(t(X) %*% X) %*% (t(X) %*% Y)
print(beta)

#C)
variavel = lm(Indice ~ Num.Ataques + Duracao)
print(variavel)

#Reta pelo beta chapeu:     
  Y = 8.372602064 + 0.005095369 * Num.Ataques - 0.008576885 * Duracao 
  Y
#Reta  pelo lm():            
  Y = 8.372602 + 0.005095 * Num.Ataques - 0.008577 * Duracao
  Y
#D)
eruption.lm = lm(Num.Ataques ~ Duracao, data=faithful)
summary(eruption.lm)$r.squared 

eruption.lm2 = lm(Num.Ataques ~ Indice, data=faithful)
summary(eruption.lm2)$r.squared

eruption.lm3 = lm(Indice ~ Duracao, data=faithful)
summary(eruption.lm3)$r.squared

#R², é uma medida de ajustamento de um modelo estatístico linear generalizado, em relação aos valores observados. 
#O R² varia entre 0 e 1, indicando, em percentagem, o quanto o modelo consegue explicar os 
#valores observados. Quanto maior o R², mais explicativo é o modelo, melhor ele se ajusta à amostra.


#E)
Previsao = 8.372602 + 0.005095 * 25 - 0.008577 * 100
Previsao


#F)
# Gráficos Q-Q Plot
qqplot(Num_Ataques, residuals(ajuste), xlab="Num.Ataques",ylab="Resíduos")
qqplot(Duracao, residuals(ajuste), xlab="Duração",ylab="Resíduos")
# Gráfico de Envelope
fit.model <- ajuste
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
si <- lm.influence(fit.model)$sigma
r <- resid(fit.model)
tsi <- r/(si*sqrt(1-h))
ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
for(i in 1:100){
  epsilon[,i] <- rnorm(n,0,1)
  e[,i] <- (ident - H)%*%epsilon[,i]
  u <- diag(ident - H)
  e[,i] <- e[,i]/sqrt(u)
  e[,i] <- sort(e[,i]) }
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2 }
med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)
par(pty="s")
qqnorm(tsi,xlab="Percentil da N(0,1)",
       ylab="Residuo Studentizado", ylim=faixa, pch=16, main="")
par(new=TRUE)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="")
# Teste de Kolmogorv-Smirnov
ks.test(residuals(ajuste), "pnorm")

p-valor(0.3657) > 0.05 # logo os resíduos seguem distribuição normal.


# Homocedasticidade
plot(Num_Ataques, residuals(ajuste),xlab="Num.Ataques",ylab="Resíduos")
abline(h=0)
plot(Duracao, residuals(ajuste),xlab="Duração",ylab="Resíduos")
abline(h=0)
```

#Por causa do comportamento observado no gráfico $Num.Ataques X Resíduos$, percebemos indícios que podem nos levar à rejeição de homocedasticidade do modelo.


