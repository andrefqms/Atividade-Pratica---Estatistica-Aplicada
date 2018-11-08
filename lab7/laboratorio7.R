#questao 1

pcara = 0.4
pcoroa = 0.6

total = 625
p0 = (pcoroa**4)
p1 = (pcoroa**3)*pcara*4
p2 = (pcoroa**2)*(pcara**2)*6
p3 = (pcoroa)*(pcara**3)*4
p4 = (pcara**4)

e = 0
e[1] = p0*total
e[2] = p1* total
e[3] = p2*total
e[4] = p3*total
e[5] = p4*total

o = 0
o[1] = 72
o[2]= 204
o[3]= 228
o[4]= 101
o[5]= 20
soma = 0
for (i in 1:5) {
  soma= soma + ((o[i]-e[i])**2/e[i])
}

print(1 - pchisq(0.470,3))


#QUESTAO 2
#a)
dados <- read.csv("DadosCiaMB.csv", header=TRUE,sep=",",dec=",")

attach(dados)

total <- length(procedencia)
proc = prop.table(table(procedencia))

total2 <- length(Instrucao)
grau_instrucao = prop.table(table(Instrucao))

total3 <- length(Instrucao)
sal = prop.table(table(salario))

conjunta = data.frame(proc,grau_instrucao)
print(conjunta)
#b)
ch = chisq.test(proc,grau_instrucao)
print(ch)
#como o p-value = 0.1991 > 0.05, não iremos rejeitar a hipótese H, isto é, ao nível de 
#significância de 5%

#c) ----

#d)
pegar<- length(Civil)
estado_civil = prop.table(table(Civil))

ch2 = chisq.test(estado_civil,grau_instrucao)
print(ch2)

#e)
 class(salario)
class(idade)
graf = plot(salario,idade,las = 1,main="Scatterplot Exemplo",cex = 1.5,col = "black")


