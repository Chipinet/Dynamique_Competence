rm(list=ls())
source("Fonction.R")

#Echelle de temps sur laquelle le graphique sera construit et le modèle sera intégré
temps <- seq(from=0, to=1000, length = 1000)

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
                   alpha=rep(0.9, nbclass),
                   omega=0.046,
                   b=10,
                   a_s=0.1,
                   #Idem que pour alpha
                   beta=rep(0, nbclass))
res <- NULL
p <- parameters
for (o in 10^seq(0,3, length=10)){
  p$omega <- o
  out <- dynamique(p, dilution=1/c(1, 10, 50, 100, 500, 1500, 3000), tmax=1000)
  out$omega <- o
  res <- rbind (res,out)
}

#pdf(file="dc_omega.pdf",width=4,height = 4)
plot(dc.infl~dilution, data=res, log="x", type="n", ylab="Pente")
lapply(split(res,res$omega),function(dtmp) lines(dc.infl~dilution, data=dtmp, type="b", 
                                                 pch=21, cex=2,bg=grey(omega)))
#dev.off()

#pdf(file="t0_omega.pdf",width=4,height = 4)
plot(t0~dilution, data=res, log="x", type="n")
lapply(split(res,res$omega),function(dtmp) lines(t0~dilution, data=dtmp, type="b", 
                                                 pch=21, cex=2,bg=grey(omega)))
#dev.off()

#Affiche un graphique s'il y a aucune ou 1 dillution
#graph.dyn(out, which=4)

