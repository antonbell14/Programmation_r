brutToNet1 <- function(salairebrut){
  if (is.numeric(salairebrut)){
  salaire_net<-salairebrut*0.78
  return (salaire_net)}
  else {return("ERROR : type not expected")}
}


brutToNet2<- function(salairebrut,statut){
  if (is.numeric(salairebrut)){
    if (statut=="non-cadre"){
      salaire_net<-salairebrut*0.78
      salaire_net<-salaire_net*0.925
      return (salaire_net)}  
    else if (statut=="cadre"){
      salaire_net<-salairebrut*0.75
      salaire_net<-salaire_net*0.925
      return (salaire_net)}
    else { "ERROR : contract unknown"}
    }
  else {return("ERROR : type not expected")}
}

brutToNet3<-function(salairebrut,statut,taux=0.075,temps=1){
  if (is.numeric(salairebrut)){
    if (0<=taux & taux<=1 & 0<=temps & temps<=1){
    if (statut=="non-cadre"){
      salaire_net<-salairebrut*0.78
      salaire_net_apimpot<-salaire_net*(1-taux)*temps
      liste<-list(salaire_net,salaire_net_apimpot)
      return (liste)}  
    else if (statut=="cadre"){
      salaire_net<-salairebrut*0.75
      salaire_net_apimpot<-salaire_net*(1-taux)*temps
      liste<-list(salaire_net,salaire_net_apimpot)
      return (liste)}
    else { "ERROR : contract unknown"}
    }
  else {return("ERROR : rate and time must be in range(0,100)")}}
  else {return("ERROR : type not expected")}
}
  
  
  
  

  
  