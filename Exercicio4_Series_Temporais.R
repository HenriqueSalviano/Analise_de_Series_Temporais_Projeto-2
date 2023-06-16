# Algoritmo usado na resolução da questão 4:
set.seed(11)
##### Simulando a serie
x <- arima.sim(n = 60, list(order = c(1,1,1), ar=c(0.5),ma=c(10)))
#### Analise descritiva
ts.plot(x, main = "S´erie Temporal Simulada - ARIMA(1,1,1)")
# Verificando Tendencia
library(randtests)
runs.test(x)

# Verificando Heterocedasticidade
med.var<-function(x,k)
{N<-length(x)
x.m<-rep(0,(N-k))
x.r<-rep(0,(N-k))
for (i in 1:(N-k)) x.m[i]<-mean(x[i:(i+k)])
for (i in 1:(N-k)) x.r[i]<-max(x[i:(i+k)])-min(x[i:(i+k)])
plot(x.m,x.r,xlab="medias",ylab="amplitude")
aa1<-lm(x.r~x.m)
abline(aa1$coef[1],aa1$coef[2],col=2)
summary(aa1)
}
med.var(x,2) #variancia


library(astsa)
acf2(x)

# Fazendo ajuste do modelo

ajust <- arima(x,order = c(1,1,1))
ajust
tsdiag(ajust)

##### Testes de raiz unitaria
# Se n~ao tem tendencia, eu n~ao tenho raiz unitaria
library(tseries)
#adf.test(x,k=p)
teste_tseries <-adf.test(x,k=1)
teste_tseries
##### estimando valores
library(astsa)
sarima.for(x,10,1,1,1)


