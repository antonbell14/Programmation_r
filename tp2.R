#a)

library(readxl)
pokemon<-read_excel("pokemon.xlsx",sheet=2)

#b)
nblc<-dim(pokemon)
cat("nb de lignes : ",nblc[1],"nb colonnes : ",nblc[2])

#c)
colonnes<-colnames(pokemon)
print(colonnes)
#d)
typecol<-sapply(pokemon,class)
print(typecol)
#e)
pokemon$is_legendary<-as.factor(pokemon$is_legendary)
pokemon$generation<-as.factor(pokemon$generation)
pokemon$type<-as.factor(pokemon$type)
class(pokemon$type)
#f)
str(pokemon$is_legendary)
str(pokemon$generation)
str(pokemon$type)
#g)
dico<-summary(pokemon)
print(dico)



#Exercice 2
#a)
moy_poids<-mean(pokemon$weight_kg,na.rm=TRUE)
print(moy_poids)
#b)
med_poids<-median(pokemon$weight_kg,na.rm = TRUE)
print(med_poids)
#c)
qua_taille<-quantile(pokemon$height_m,na.rm = TRUE )
print(qua_taille)
#d)
dec_taille<-quantile(pokemon$height_m,probs = seq(0,1,0.1),na.rm = TRUE )
print(dec_taille)

#e)
varpoids<-var(pokemon$weight_kg,na.rm = TRUE)
etpoids<-sqrt(varpoids)
cat("variance du poids",varpoids,"ecart type du poids",etpoids)

#f)
sl<-table(pokemon$is_legendary)
sort(sl, decreasing = TRUE)
sg<-table(pokemon$generation)
sort(sg, decreasing = TRUE)
st<-table(pokemon$type)
sort(st, decreasing = TRUE)

#Exercice 3

#a)
r1<-pokemon[,c("nom","is_legendary")]
dim(r1)
#b)
r2<-pokemon[1:50,1:2]
dim(r2)
#c)
r3<-pokemon[1:10,]
dim(r3)
#d)
r4<-pokemon[,1:dim(pokemon)[2]-1]
dim(r4)
#e)
r5<-pokemon[order(pokemon$nom),colnames(pokemon)]
print(r5[1,"nom"])
#f)
r6<-pokemon[order(pokemon$weight_kg,decreasing = TRUE),colnames(pokemon)]
print(r6[1,"nom"])
#g)
r6<-pokemon[order(-pokemon$attack,pokemon$speed),colnames(pokemon)]
print(r6[1:10,"nom"])