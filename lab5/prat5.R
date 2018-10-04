#LETRA A

p = 522.097/(522.097+720.991)


#LETRA B

#I)

total = 1243088
dados = rep(0, total)
for (i in 1:522097) {
  dados[i] = 1
}

p1 = 522097/total
prop2 = 720991/total
n = 2200
k = 100
vetor = numeric(100)

for (i in 1:k) {
  amostragem = sample(dados, n, replace=FALSE)
  proporcao = 0
  for (j in 1:n) {
    proporcao = proporcao + amostragem[j]
  }
  vetor[i] = proporcao/n
  media = mean(amostragem)
  formula = (p1 * (1 - p1))/n
  formula = formula * 1.96
  erro = sqrt(formula)
  ic_soma = media + erro
  ic_diminui = media - erro
}

hist(vetor)





#II)

total = 1243088
dados = rep(0, total)
for (i in 1:522097) {
  dados[i] = 1
}

p1 = 522097/total
prop2 = 720991/total
n = 2200
k = 1000
vetor = numeric(100)

for (i in 1:k) {
  amostragem = sample(dados, n, replace=FALSE)
  proporcao = 0
  for (j in 1:n) {
    proporcao = proporcao + amostragem[j]
  }
  vetor[i] = proporcao/n
  media = mean(amostragem)
  formula = (p1 * (1 - p1))/n
  formula = formula * 1.96
  erro = sqrt(formula)
  ic_soma = media + erro
  ic_diminui = media - erro
}

hist(vetor)



