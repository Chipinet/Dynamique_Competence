library(deSolve)

#Fonction a, utilisée pour calculer lambda
f <- function(a,parameters) {with(parameters,{
  if (a<=0) return(0)
  return(1/(1+(a_s/a)^b))})}

#Dérivée de la fonction a, utilisée pour calculer la dérivée de lambda
df <- function(a,parameters) {with(parameters,{
  if (a<=0){
    if (b==1) return(b/a_s)
    if (b>1) return (0)
    if (b<1) return(Inf)
  }
  return((b/a_s)*(a_s/a)^(b+1)/(1+(a_s/a)^b)^2)})}

#Système d'équation différencielle
modele <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    #Créer un vecteur contenant les valeurs des c et prendre sa longueur
    n <- length(state)
    m <- n-3
    vc <- state[-c(1,n-1,n)]
    
    #valeurs de lambda et du Birth rate
    lambda <- omega*f(a, parameters)+sum(beta*vc)
    B <- 1-s-p-sum(vc)
    
    #Equations différentielles
    ds=(s+delta*p)*B-s*(xi+lambda+mus)
    
    #Calcul de ci puis des cas particuliers c1 et cn
    dc = lambda*state[1:m]+eta*c(vc[-1],0)-(lambda+eta+muc)*vc
    dc[1]=dc[1]+xi*s
    dc[m]=dc[m]+(-theta+lambda)*vc[m]
    
    dp=(1-delta)*p*B+theta*vc[m]-mup*p
    da=sum(alpha*vc)-phi*a
    
    list(c(ds, dc, dp, da))
  })
}

#Calcul de la dynamique, du point dinflexion, de la dérivé en ce point, de l'abscisse de la tangente au point d'inflexion, du max
dynamique <- function(param,dilution=NULL,t=NULL,tmax=100){
  
  #Il y a autant de classe de conpétence que de paramètres beta
  nbclass=length(param$beta)
  
  #Initiation de la valeur des densités à l'état initial. s=1-mus, tout les ci ont la même valeur
  state <- c(1-param$mus,rep(0,nbclass),0,0)
  #Nom des densité : nom de c = c + 1,2... jusqu'à n.
  names(state) <- c("s",paste0("c",1:nbclass),"p","a")
  
  #Root fonction calculant la dérivé de dc pour avoir le maximum, et la dérivée seconde de dc pour avoir le point d'inflexion
  rootfunc <- function(t, var, par){
    deriv <- unlist(modele(t, var, par))
    n <- length(var)
    
    #Dérivé = somme de toutes les dérivées dci
    dc<-sum(deriv[-c(1,n-(1:0))])
    
    #Dérivé seconde
    vc <- var[-c(1,n-(1:0))]
    with(as.list(c(var, par)),{
      lambda <- omega*f(a, par)+sum(beta*vc)
      
      dlambda=df(a, par)*omega*deriv[n]+sum(beta*deriv[-c(1,n-(1:0))])
      dc_2=dlambda*s+(xi+lambda)*deriv[1]-eta*deriv[2]-theta*deriv[n-2]-muc*dc
      return(c(dc, dc_2))
    })
  }
  
  #Pas d'eventfunc particulière, la valeur des états est renvoyées
  eventfunc <- function(t, var, par){
    var
  }
  
  #S'il n'y a pas de dillution, dillution prend 1
  if (is.null(dilution)) dilution <- 1
  
  vt <- t
  #Si t est nul ou il y a plus d'une dillution, le temps est de 0 à tmax
  if(is.null(t)| length(dilution)>1) {
    vt<-c(0,tmax)
  }
  
  #res prendra les valeurs du point d'inflexion et du max si elles existent
  res <- NULL
  
  #Intégration pour chacune des dillutions
  for (i in 1:length(dilution)){
    s=state*dilution[i]
    #Intégration
    out <- ode(y = s, times = vt, func = modele, parms = param, 
               rootfun=rootfunc, 
               events = list(func = eventfunc, root = TRUE)) #maxroot = 2))
    
    #print(attributes(out))
    #Récupération du point d'inflexion, du max
    valeurs <- c(NA,NA)
    derive <- NA
    v.res <- c(NA,NA,NA,NA,NA)
    
    #posinfl : position du point d'inflexion
    #posmax : position du maximum
    posmax <- which(attributes(out)$indroot==1)
    posmax <- posmax[1]
    posinfl <- NA
    if (!is.na(posmax)) {
      valeurs[2]<-sum(c(attributes(out)$valroot[1:nbclass+1,posmax]))
      v.res[4:5] <- c(attributes(out)$troot[posmax],valeurs[2])
      if (length(attributes(out)$indroot[posmax-1])>0){
        if(attributes(out)$indroot[posmax-1]==2) posinfl <- posmax-1
      }
    }
    if (!is.na(posinfl)){
      valeurs[1]<-sum(c(attributes(out)$valroot[1:nbclass+1,posinfl]))
      #Calcul de la dérivé au point d'inflexion
      lambda <- param$omega*f(attributes(out)$valroot[nbclass+3,posinfl], param)+sum(param$beta*c(attributes(out)$valroot[1:nbclass+1,posinfl]))
      derive <- (lambda+param$xi)*attributes(out)$valroot[1,posinfl]-param$eta*attributes(out)$valroot[2,posinfl]-param$theta*attributes(out)$valroot[nbclass+1,posinfl]-param$muc*valeurs[1]
      v.res[1:3] <-c(attributes(out)$troot[posinfl],valeurs[1],derive)
    }
  
    if (length(v.res)<5){
      warning("Inflection point not found, try increasing tmax")
    }
    else{
      names(v.res)<- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max")
      v.res["t0"] <- with(as.list(v.res),t.infl-c.infl/dc.infl)
      v.res["dilution"]<- dilution[i]
      res <- rbind(res,v.res) 
    }
  }
  #Si un temps est défini, la dynamique est renvoyée
  res <- as.data.frame(res)
  if(!is.null(t)) {
    attr(res,"dynamique") <- as.data.frame(out)
  }
  row.names(res) <- 1:nrow(res)
  return(res)
}

#Affichage de la dynamique
graph.dyn <- function(dyn, which=NULL){
  out <- attr(dyn,"dynamique")
  
  #Affiche uniquement si la dynamique est trouvée
  if (is.null(out)) {
    stop("Attribute dynamique not found")
  }
  nbclass=ncol(out)-4
  
  #1 à 4 permet de choisir quel graphique est affiché
  if (is.null(which)) which <- -1
  #Graphique (entre 0 et 1 pour toutes les variables sauf a)
  if (which<0) layout(matrix(1:4, ncol=2))
  if (which<0 | which==1) plot(out[,1],out[,2], xlab="Temps", ylab="", type="l", main="s", ylim=c(0,1))
  if (which<0 | which==2) plot(out[,1],out[,nbclass+3], xlab="Temps", ylab="", type="l", main="p", ylim=c(0,1))
  if (which<0 | which==3) plot(out[,1],out[,nbclass+4], xlab="Temps", ylab="", type="l", main="a")
  
  #Affiche de la somme des c
  if (which<0 | which==4){
    ctot <- apply(as.matrix(out[,-c(1:2, ncol(out)-(1:0))]),1,sum)
    plot(out[,1], ctot, xlab="Temps", ylab="", type="l", main="c_somme", ylim=c(0,1))
    for (i in 1:nbclass)
      lines(out[,1],out[,i+2], col=2)
    #Affichage du point d'inflexion, de sa tangente et du max
    abline(v=dyn[c("t.infl","t.max","t0")])
    abline(h=dyn[c("c.infl","c.max")])
    abline(a = with(as.list(dyn),c.infl-t.infl*dc.infl), b=dyn["dc.infl"], lty=3, col="purple")
  }
  
}