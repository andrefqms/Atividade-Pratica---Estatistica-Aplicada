#QUESTAO 1

total = (650135 + 592953)

#A)

p = 650135/total

print(p)

#B)

significancia = 0.05
gama = 0.95
zgama = 1.96


pro = 650135
contra = 592953

pop = 0
for (i in 1:pro) {
  pop[i] = 1
}
for (i in pro+1:total) {
  pop[i] = 0
}

atrib = sample(pop,25,replace = FALSE)
media = mean(atrib)


limiteInferior = p - (zgama *sqrt(p*(1-p)/25))
limiteSuperior = p + (zgama *sqrt(p*(1-p)/25))

#c)

#Sim,



#QUESTAO 2

# a)
amostra = c(72.31, 77.49, 77.15, 74.62, 74.17, 75.71, 77.10, 76.16, 76.96, 
             78.33, 77.86, 77.51, 79.22, 73.50, 75.16, 80.16, 73.59, 69.88, 
             72.68, 72.61, 74.71, 73.49, 81.71, 73.72)

rc = 75 + 1.714 * (var(amostra)/sqrt(24))

mean(amostra)

t.test(amostra)

#b)


