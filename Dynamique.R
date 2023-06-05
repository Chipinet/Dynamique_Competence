rm(list=ls())
source("Fonction_2.R")

#Echelle de temps sur laquelle le graphique sera construit et le modèle sera intégré
temps <- seq(from=0, to=15, length = 1000)
#Nombre de classes de compétences
nbclass = 5

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
                   omega=5,
                   b=1.1,
                   a_s=2,
                   #Idem que pour alpha
                   beta=rep(0, nbclass))

res <- NULL
p <- parameters
for (i in 1:10){
  p$alpha <- i
  out <- dynamique(p, dilution=1/c(1, 10, 50, 100, 500, 1500, 3000), tmax=1000)
  out$alpha <- i
  res <- rbind (res,out)
}


#pdf(file="t0_alpha_6_diff.pdf",width=4,height = 4)
plot(t0~dilution, data=res, log="x", type="n")
lapply(split(res,res$alpha),function(dtmp) lines(t0~dilution, data=dtmp, type="b", 
                                                pch=21, cex=2, bg=grey(alpha/i)))
#dev.off()

#pdf(file="dc_alpha_6_diff.pdf",width=4,height = 4)
plot(dc.infl~dilution, data=res, log="x", type="n", ylab="Pente")
lapply(split(res,res$alpha),function(dtmp) lines(dc.infl~dilution, data=dtmp, type="b", 
                                                 pch=21, cex=2, bg=grey(alpha/i)))
#dev.off()

out <- dynamique(parameters, dilution=1/100, t=temps)
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

