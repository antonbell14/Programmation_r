#Exercice 1
#1
set.seed(2000)
population<-rnorm(n = 10000000,mean = 171,sd = 9)
#2
meanpop<-mean(population)
sdpop<-sd(population)
#3
hist(population, probability = TRUE)
#4
1-pnorm(q = 190,mean = meanpop,sd = sdpop)
grandetaille<-length(population[population>190])
grandetaille/10000000
#5
pnorm(q = 144,mean = meanpop,sd = sdpop)
petitetaille<-length(population[population<144])
petitetaille/10000000

#Exercice2
#1
set.seed(2000)
echantillon<-sample(population,size=100,replace=TRUE)
meanech<-mean(echantillon)
sdech<-sd(echantillon)
print(meanech)
print(sdech)
#proche de l'echantillon mais ecart quand meme marqué
#2
u<-qnorm(p = 0.95,mean = 0,sd = 1)
print(u)
meanech-u*(sdech/sqrt(100))
meanech+u*(sdech/sqrt(100))
#3
dataframe<-replicate(1000,sample(population,size=100,replace=TRUE))
meanx<-apply(dataframe,MARGIN=2,FUN=mean)
sdx<-apply(dataframe,MARGIN=2,FUN=sd)
#4
hist(meanx, probability = TRUE)
hist(sdx, probability = TRUE)
#5
meanxbarre<-mean(meanx)
sdxbarre<-sd(meanx)
print(meanxbarre)
print(sdxbarre)
#6
length(meanx[meanx>172.8])
prop<-1-pnorm(q=172.8,mean=meanxbarre,sd=sdxbarre)
prop*1000
#7
u<-qnorm(p = 0.95,mean = 0,sd = 1)
print(u)
meanxbarre-u*(sdxbarre/sqrt(1000))
meanxbarre+u*(sdxbarre/sqrt(1000))