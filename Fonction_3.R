library(deSolve)

#Fonction a, utilisée pour calculer lambda
f <- function(a,parameters) {with(parameters,{
  if (a<=0 | is.na(a)) return(0)
  return(1/(1+(a_s/a)^b))})}

#Dérivée de la fonction a, utilisée pour calculer la dérivée de lambda
df <- function(a,parameters) {with(parameters,{
  if (a<=0 | is.na(a)){
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
    T <- omega*f(a, parameters)+sum(beta*vc)
    B <- 1-s-p-sum(vc)
    
    #Equations différentielles
    ds=(s+delta*p)*B+eta*vc[1]-(xi+T+mus)*s
    
    #Calcul de ci puis des cas particuliers c1 et cn
    dc = lambda*state[1:m]+eta*c(vc[-1],0)-(lambda+eta+muc)*vc
    dc[1]=dc[1]+(xi+T)*s-lambda*s
    dc[m]=dc[m]+(-theta+lambda)*vc[m]
    
    dp=(1-delta)*p*B+theta*vc[m]-mup*p
    da=alpha[1]*s+sum(alpha[2:(m+1)]*vc)-phi*a
    
    list(c(ds, dc, dp, da))
  })
}

#Calcul de la dynamique, du point dinflexion, de la dérivé en ce point, de l'abscisse de la tangente au point d'inflexion, du max
dynamique <- function(param,dilution=NULL,t=NULL,tmax=100){
  
  #Il y a autant de classe de conpétence que de paramètres beta
  nbclass=length(param$beta)
  
  #Initiation de la valeur des densités à l'état initial. s=1-mus, tout les ci ont la même valeur
  state <- c(1-param$mus,rep(0,nbclass),0,0)
  m <- length(state)
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
        T <- omega*f(a, par)+sum(beta*vc)
        
        dT=df(a, par)*omega*deriv[n]+sum(beta*deriv[-c(1,n-(1:0))])
        dc_2=dT*s+(xi+T)*deriv[1]-eta*deriv[2]-theta*deriv[n-2]-muc*dc
        return(c(dc, dc_2, sum(vc)/sum(var)-fr_seuil))
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
    s=state*1/dilution[i]
    #Intégration
    out <- ode(y = s, times = vt, func = modele, parms = param, 
               rootfun=rootfunc, 
               events = list(func = eventfunc, root = TRUE)) #maxroot = 2))
    
    #print(attributes(out))
    #Récupération du point d'inflexion, du max
    valeurs <- c(NA,NA)
    derive <- NA
    v.res <- c(NA,NA,NA,NA,NA,NA)
    
    #posinfl : position du point d'inflexion
    #posmax : position du maximum
    posmax <- which(attributes(out)$indroot==1)
    posmax <- posmax[1]
    posinfl <- NA
    #Si le maximum existe, prend sa valeur en x et y
    if (!is.na(posmax)) {
      valeurs[2]<-sum(c(attributes(out)$valroot[1:nbclass+1,posmax]))
      v.res[4:5] <- c(attributes(out)$troot[posmax],valeurs[2])
      #Prend le point d'inflection avant le max s'il existe
      if (length(attributes(out)$indroot[posmax-1])>0){
        if(attributes(out)$indroot[posmax-1]==2) posinfl <- posmax-1
      }
    }
    #Si le point d'inflection existe, prend sa valeur en x et y, calcule sa dérivé
    if (!is.na(posinfl)){
      valeurs[1]<-sum(c(attributes(out)$valroot[1:nbclass+1,posinfl]))
      #Calcul de la dérivée au point d'inflexion
      T <- param$omega*f(attributes(out)$valroot[nbclass+3,posinfl], param)+sum(param$beta*c(attributes(out)$valroot[1:nbclass+1,posinfl]))
      derive <- (T+param$xi)*attributes(out)$valroot[1,posinfl]-param$eta*attributes(out)$valroot[2,posinfl]-param$theta*attributes(out)$valroot[nbclass+1,posinfl]-param$muc*valeurs[1]
      v.res[1:3] <-c(attributes(out)$troot[posinfl],valeurs[1],derive)
    }
    #Récupère le t0
    posseuil <- which(attributes(out)$indroot==3)
    posseuil <- posseuil[1]
    if(!is.na(posseuil)) {
      v.res[6]<-attributes(out)$troot[posseuil]
    }
    if (length(v.res)<5){
      warning("Inflection point not found, try increasing tmax")
    }
    else{
      names(v.res)<- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max", "t0.seuil")
      v.res[paste("t0", param$k, sep="_")] <- with(as.list(v.res),t.infl+(c.infl/dc.infl)*(log(param$k*param$xi)-log(c.infl)))
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

#Variation de beta en cloche
beta_i <- function(beta_bar,n){
  e=n+1
  b <- factorial(n-1)*prod(e-2:n)
  b <- beta_bar/b^(1.0/(n-1))
  bi <- b*(2:n-1)*(e-2:n)
  return(bi)
} 


#Minimisation pour omega et b
find.omega.b <- function(parameters, out){
  p <- parameters
  dilution.obj <- out$dilution[1]
  slope.obj <-  out$dc.infl[1]/out$c.infl[1]
  t0.obj <-  out$t0.seuil[1]
  f <-function(x){
    p$beta <- rep(0, length(p$beta))
    p$omega <- x[1]
    p$b <- x[2]
    out2 <- dynamique(p, dilution=dilution.obj)
    slope <-  out2$dc.infl/out2$c.infl
    t0 <- out2$t0.seuil
    return(sqrt((t0-t0.obj)^2+(slope -slope.obj)^2))
  }
  res <- nlm(f, c(30,2))
  return(res)
}

#Minimisation pour omega uniquement
find.omega <- function(parameters, out){
  p <- parameters
  dilution.obj <- out$dilution[1]
  slope.obj <- out$dc.infl[1]/out$c.infl[1]
  t0.obj <- out$t0.seuil[1]
  f <-function(x){
    p$beta <- rep(0, length(p$beta))
    p$omega <- 10^(x[1])
    out2 <- dynamique(p, dilution=dilution.obj)
    slope <- out2$dc.infl/out2$c.infl
    t0 <- out2$t0.seuil
    return(sqrt((t0-t0.obj)^2+(slope -slope.obj)^2))
  }
  res <- optimize(f, lower=-6, upper=6)
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
  if (which<0 | which==1) plot(out[,1],out[,2], xlab="Time", ylab="", type="l", main="s", ylim=c(0,1))
  if (which<0 | which==2) plot(out[,1],out[,nbclass+3], xlab="Time", ylab="", type="l", main="p", ylim=c(0,1))
  if (which<0 | which==3) plot(out[,1],out[,nbclass+4], xlab="Time", ylab="", type="l", main="a")
  
  #Affiche de la somme des c
  if (which<0 | which==4){
    ctot <- apply(as.matrix(out[,-c(1:2, ncol(out)-(1:0))]),1,sum)
    plot(out[,1], ctot, xlab="Time", ylab="", type="l", main="c_sum", ylim=c(0,1))
    for (i in 1:nbclass)
      lines(out[,1],out[,i+2], col=2)
    #Affichage du point d'inflexion, de sa tangente et du max
    abline(v=dyn[c("t.infl","t.max","t0.seuil")])
    abline(h=dyn[c("c.infl","c.max")])
    lines(out[,1], with(as.list(dyn),c.infl*exp((dc.infl/c.infl)*(out[,1]-t.infl))), lty=3, col="purple")
  }
  
}

#Tout sur le même graphique
graphs <- function(dyn, which=NULL){
  out <- attr(dyn,"dynamique")
  
  #Affiche uniquement si la dynamique est trouvée
  if (is.null(out)) {
    stop("Attribute dynamique not found")
  }
  nbclass=ncol(out)-4
  
  
  #Affiche de la somme des c
  ctot <- apply(as.matrix(out[,-c(1:2, ncol(out)-(1:0))]),1,sum)
  plot(out[,1], ctot, xlab="Time", ylab="", type="l", ylim=c(0,1))
  nom=c("c1 through c5", "ctotal")
  couleur=c("red", "black")
  for (i in 1:nbclass)
    lines(out[,1],out[,i+2], col=2)
  #Affichage de s
  if (which >=1){
    lines(out[,1],out[,2], col="blue")
    nom=c(nom, "s")
    couleur=c(couleur,"blue")
  }
  #Affichage de p
  if (which >=2){
    lines(out[,1],out[,nbclass+3], col="green")
    nom=c(nom, "p")
    couleur=c(couleur,"green")
  }
  #Affichage de a
  if (which >=3){
    lines(out[,1],out[,nbclass+4], col="purple")
    nom=c(nom, "a")
    couleur=c(couleur,"purple")
  }
  legend("topright", legend=nom, col=couleur,lty=1, box.lty=0)
}



#Points d'intérêts uniquement 
pointsint <- function(dyn, which=NULL, seuil=1E-3){
  out <- attr(dyn,"dynamique")
  
  #Affiche uniquement si la dynamique est trouvée
  if (is.null(out)) {
    stop("Attribute dynamique not found")
  }
  nbclass=ncol(out)-4
  
  #1 à 4 permet de choisir quel graphique est affiché
  if (is.null(which)) which <- -1
  #Graphique (entre 0 et 1 pour toutes les variables sauf a)
  #Affiche de la somme des c
    ctot <- apply(as.matrix(out[,-c(1:2, ncol(out)-(1:0))]),1,sum)
    plot(out[,1], ctot, xlab="Time", ylab="", type="l", ylim=c(0,1))
    nom <- c("ctotal")
    couleur <- c("black")
    type <- 1
    #Affichage du point d'inflexion, de sa tangente et du max
    #max
    if (which>=1){
      abline(v=dyn[c("t.max")], col="blue")
      abline(h=dyn[c("c.max")], col="blue")
      nom <- c(nom, "maximum")
      couleur <- c(couleur, "blue")
    }
    #Inflexion
    if (which >=2){
      abline(v=dyn[c("t.infl")], col="green")
      abline(h=dyn[c("c.infl")], col="green")
      nom <- c(nom, "inflection point")
      couleur <- c(couleur, "green")
    }
    #t0 et tangente
    if (which >=3){
      abline(v=dyn[c("t0.seuil")], col="purple")
      lines(out[,1], with(as.list(dyn),c.infl*exp((dc.infl/c.infl)*(out[,1]-t.infl))), lty=3, col="red")
      points(x=dyn["t0.seuil"], y=seuil, col="purple", pch=16)
      text(x=(dyn["t0.seuil"]+1), y=seuil, labels="t0", col="purple")
      nom <- c(nom, "slope", "t0")
      couleur <- c(couleur, "red", "purple")
      type <- c(rep(1,3), 3, 1)
    }
    legend("topright", inset=0.01, legend=nom, col=couleur,lty=type, box.lty=0)
  
}