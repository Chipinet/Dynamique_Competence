rm(list=ls())
source("Fonction.R")

#Echelle de temps sur laquelle le graphique sera construit et le modèle sera intégré
temps <- seq(from=0, to=100, length = 1000)

#Nombre de classes de compétences
nbclass = 1

#Initiation des paramètres du modèle
parameters <- list(mus=0.1,
                   muc=0.1,
                   mup=0.1,
                   xi=10E-5,
                   delta=0.1,
                   eta=0.1,
                   theta=0.1,
                   phi=0.1,
                   #Répétition de la valeur de alpha, tout les alphas ont la même valeur
                   alpha=rep(0.6, nbclass),
                   omega=0.4,
                   b=10,
                   a_s=0.4,
                   #Idem que pour alpha
                   beta=rep(0, nbclass))

res <- NULL
p <- parameters
for (o in seq(0,1, length=10)){
  p$omega <- o
  out <- dynamique(p, dilution=1/c(1, 10, 50, 100, 500, 1500, 3000), tmax=1000)
  out$omega <- o
  res <- rbind (res,out)
}

#pdf(file="t0_betaomega_10.pdf",width=4,height = 4)
plot(t0~dilution, data=res, log="x", type="n")
lapply(split(res,res$omega),function(dtmp) lines(t0~dilution, data=dtmp, type="b", 
                                                pch=21, cex=2, bg=grey(omega/o)))
#dev.off()

#pdf(file="dc_betaomega_10.pdf",width=4,height = 4)
plot(dc.infl~dilution, data=res, log="x", type="n", ylab="Pente")
lapply(split(res,res$omega),function(dtmp) lines(dc.infl~dilution, data=dtmp, type="b", 
                                                 pch=21, cex=2, bg=grey(omega/o)))
#dev.off()

out <- dynamique(parameters, t=temps)
#Affiche un graphique s'il y a aucune ou 1 dillution
graph.dyn(out, which=4)


# p <- parameters
# res<-NULL
# for (nb in 1:10){
#   nbclass <- nb
#   p$alpha <- rep(p$alpha[1], nbclass)
#   p$beta <- rep(p$beta[1], nbclass)
#   out <- dynamique(p, dilution=1/c(1, 10, 50, 100, 500, 1500, 3000), tmax=1000)
#   out$nbclass <- nb
#   res <- rbind (res,out)
# }

# for (be in 2:6){
#   out <- dynamique(p, dilution=1/c(1, 10, 50, 100, 500, 1500, 3000), tmax=1000)
#   out$beta <- be-1
#   res <- rbind (res,out) 
#   if (be !=6) p$beta[be] <- out$beta[be-1]/2
# }

# e <- exp(10*0.4)
# lambda <- 0.4*(1/(1+e))
# dlambda <- 0.4*(-10*e/((1+e)^2))
# ds <- 0.9*(1-0.9)-(10^(-5)+lambda)*0.9-0.1*0.9
# dc <- (10^(-5)+lambda)*0.9
# dc2 <- dlambda*0.9+(10^(-5)+lambda)*ds-dc*0.3
# print(dc2)