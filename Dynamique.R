rm(list=ls())
source("Fonction_3.R")

#Echelle de temps sur laquelle le graphique sera construit
temps <- seq(from=0, to=40, length = 1000)
#Nombre de classes de compétences
nbclass = 5

#Initiation des paramètres du modèle
parameters <- list(mus=0.1,
                   muc=0.1,
                   mup=0.1,
                   xi=1E-5,
                   k= c(1,10,100,1000),
                   delta=0.1,
                   eta=0,
                   theta=0.1,
                   phi=0.1,
                   lambda=0.5,
                   #Répétition de la valeur de alpha, tout les alphas ont la même valeur
                   alpha=c(0.01, rep(0.6, nbclass)),
                   omega=0,
                   b=2,
                   a_s=1,
                   #Idem que pour alpha
                   beta=c(0, rep(0, nbclass-1)),
                   beta_bar=20,
                   fr_seuil=1E-3)


res <- NULL
p <- parameters

#Variation de omega et b définis par la minimisation
param2 <- read.table(file="parameters_omega_b_bell.csv", header=TRUE)
for (i in 1:14){
  p$omega <- param2[i,1]
  p$b <- param2[i,2]
  out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
  out$omega <- param2[i,1]
  out$b <- param2[i,2]
  res <- rbind (res,out)
}


#Variation de beta différent selon le nombre de classe
for (i in 2:15){
  nbclass <- i
  p$beta <- rep(0, nbclass)
  p$alpha <- rep(0, nbclass+1)
  p$beta[2:nbclass]<- beta_i(p$beta_bar,nbclass)
  out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
  out$nbclass <- i
  out$beta <- p$beta[2]
  res <- rbind (res,out)
}

#Variation d'un seul paramètre
for (i in 1:10){
  p$beta[2:nbclass] <- i
  out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
  out$beta <- i
  res <- rbind (res,out)
}

#Sauvegarde de la simulation dans une table
#write.table(res, file="sim_beta_1_10_seuil-6.csv",row.names = FALSE)
res <- read.table(file="sim_beta_20_30_seuil-6.csv", header=TRUE)


#pdf(file="sim_t0_beta_bell_betabar_20.pdf",width=4,height = 4)
#Plot de différents t0
plot(t0.seuil~dilution, data=res, log="x", type="n", ylab="t0")
lgd_ = rep(NA, 10)
lgd_[c(1,10)] = c(2,15)
legend("topleft", inset=0.01,legend = lgd_,
       fill = colorRampPalette(colors = c('black','grey96'))(10),
       border = NA,
       y.intersp = 0.5, cex=0.75)
lapply(split(res,res$nbclass),function(dtmp) lines(t0.seuil~dilution, data=dtmp, type="b",
                                                pch=21, cex=2, bg=grey(nbclass/i)))

#dev.off()

 
#pdf(file="sim_dc_beta_bell_betabar_20.pdf",width=4,height = 4)
#Plot de différents dc
plot(I(dc.infl/c.infl)~dilution, data=res, log="x", type="n", ylab="slope")
# lgd_ = rep(NA, 10)
# lgd_[c(1,10)] = c(2,15)
# legend("topright", inset=0.01,legend = lgd_,
#        fill = colorRampPalette(colors = c('black','grey96'))(10),
#        border = NA,
#        y.intersp = 0.5, cex=0.5)
lapply(split(res,res$nbclass),function(dtmp) lines(I(dc.infl/c.infl)~dilution, data=dtmp, type="b",
                                                 pch=21, cex=2, bg=grey(nbclass/i)))
#dev.off()

#Graph : t0 en fonction de la pente
#pdf(file="sim_t0slope_beta_bell_betabar_20.pdf",width=4,height = 4)
plot(t0.seuil~I(dc.infl/c.infl), data=res, xlab="slope", ylab="t0", type="n")
# lgd_ = rep(NA, 10)
# lgd_[c(1,10)] = c(20,30)
# legend("topright", inset=0.01,legend = lgd_,
#        fill = colorRampPalette(colors = c('black','grey96'))(10),
#        border = NA,
#        y.intersp = 0.5, cex=0.75)
lapply(split(res,res$nbclass),function(dtmp) lines(t0.seuil~I(dc.infl/c.infl), data=dtmp, type="b",
                                                 pch=21, cex=2, bg=grey(nbclass/i)))
#dev.off()

omegares <- read.table(file="sim_omega_b_samebeta_2030.csv", header=TRUE)
betares <- read.table(file="sim_beta_20_30.csv", header=TRUE)
# #Plot avec beta et omega dessus
#* pdf(file="sim_omegabeta_t0_slope_20_30.pdf",width=8,height = 6)
plot(t0.seuil~I(dc.infl/c.infl), data=omegares, xlab="slope", ylab="t0", type="n", xlim=c(1.4,1.83))
 # lgd_ = rep(NA, 5)
 # lgd_[c(1,3,5)] = c(1,"lambda",5)
# space=rep(NA, 2)
# lgd_2 = rep(NA, 10)
# lgd_2[c(1,5,10)] = c(1,"beta",10)
# legend("right" ,legend = c(lgd_),#, space, lgd_2),
#        fill = c(colorRampPalette(colors = c('black','grey96'))(5)),#, "white", "white",colorRampPalette(colors = c('black','grey96'))(10)),
#        border = NA,
#        y.intersp = 0.5, cex=0.7)
legend("bottomleft", legend=c("omega","beta"), pch=c(21,25), cex=0.6)
lapply(split(omegares,omegares$omega),function(dtmp) lines(t0.seuil~I(dc.infl/c.infl), data=dtmp, type="b",
                                                           pch=21, cex=2, bg=grey(omega/315)))
lapply(split(betares,betares$beta),function(dtmp) lines(t0.seuil~I(dc.infl/c.infl), data=dtmp, type="b",
                                                        pch=25, cex=2, bg=grey(beta/30)))
#* dev.off()


 out <- dynamique(parameters, t=temps)
# #Affiche un graphique s'il y a aucune ou 1 dillution
 graph.dyn(out, which=4)


#Trouver omega et b pour beta donné
res <- read.table(file="sim_beta_20_30_seuil-6.csv", header=TRUE)
res <- split(res,res$beta)
out <- as.data.frame(res[7])
colnames(out) <- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max", "t0.seuil", paste0("t0_", parameters$k), "dilution", "beta")
min.omega.b <- find.omega.b(parameters, out)
parameters_2 <- parameters
parameters_2$omega <- min.omega.b$estimate[1]
parameters_2$b <- min.omega.b$estimate[2]
parameters_2$beta <- rep(0, length(parameters$beta))
out2 <- dynamique(parameters_2, t=temps)


#Trouver omega et b pour une gamme de beta
res <- read.table(file="sim_beta_bell_betabar_20.csv", header=TRUE)
res <- split(res,res$nbclass)
omega.b <- NULL
for (i in 1:14){
  out <- as.data.frame(res[i])
  colnames(out) <- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max", "t0.seuil", paste0("t0_", parameters$k), "dilution", "nbclass", "beta_2")
  min.omega.b <- find.omega.b(parameters, out)
  newpara <- c(min.omega.b$estimate[1], min.omega.b$estimate[2])
  omega.b <- rbind(omega.b, newpara)
}
colnames(omega.b) <- c("omega", "b")


# #Plot beta en cloche
beta_2 <- rep(0,10)
beta_2 <- beta_i(20,10)
plot(x=c(2:10), y=beta_2,xlab="i", ylab="Beta_i", col="red", pch=16, type="b", ylim=c(0,30))
beta_2 <- beta_i(10,10)
points(x=c(2:10), y=beta_2, col="orange", pch=16, type="b")
beta_2 <- rep(0,5)
beta_2 <- beta_i(20,5)
points(x=2:5, y=beta_2, col="blue", pch=16, type="b")
beta_2 <- beta_i(10,5)
points(x=c(2:5), y=beta_2, col="cyan", pch=16, type="b")
legend("bottom", legend=c("beta_bar=20, n=10","beta_bar=10, n=10", "beta_bar=20, n=5", "beta_bar=10, n=5"), col=c("red", "orange", "blue", "cyan"), pch=16, cex=0.7)

#Affichage des différents seuils
# res <- split(res, res$seuil)
# layout(matrix(1:6, ncol=3))
# for (i in 1:6){
#   out <- as.data.frame(res[i])
#   colnames(out) <- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max", "t0.seuil", paste0("t0_", parameters$k), "dilution", "omega", "seuil")
#   plot(t0.seuil~dilution, data=out, log="x", type="n", ylab="t0")
#   legend("bottomright", legend=out$seuil[1])
#   lapply(split(out,out$omega),function(dtmp) lines(t0.seuil~dilution, data=dtmp, type="b",
#                                                    pch=(21), cex=2, bg=grey(omega/11)))
# }

