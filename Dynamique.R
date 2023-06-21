rm(list=ls())
source("Fonction_3.R")

#Echelle de temps sur laquelle le graphique sera construit et le modèle sera intégré
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
                   e=3,
                   beta_bar=20,
                   fr_seuil=1E-6)


#parameters$beta[2:nbclass] <- beta_i(parameters$beta_bar,parameters$e,nbclass-1)


res <- NULL
p <- parameters


param2 <- read.table(file="parameters_omega_b_seuil-6.csv", header=TRUE)
#Variations d'un paramètre
# for (i in 1:11){
#   p$omega <- param2[i,1]
#   p$b <- param2[i,2]
#   out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
#   out$omega <- param2[i,1]
#   out$b <- param2[i,2]
#   res <- rbind (res,out)
# }


for (i in 2:15){
  nbclass <- i
  p$e <- nbclass-1
  p$beta <- rep(0, nbclass)
  p$alpha <- rep(0, nbclass+1)
  p$beta[2:nbclass]<- beta_i(p$beta_bar,p$e,nbclass-1)
  out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
  out$nbclass <- i
  out$beta <- p$beta[2]
  res <- rbind (res,out)
}

# for (i in 1:10){
#   p$beta[2:nbclass] <- i
#   out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
#   out$beta <- i
#   res <- rbind (res,out)
# }

#write.table(res, file="sim_beta_1_10_seuil-6.csv",row.names = FALSE)
res <- read.table(file="sim_beta_20_30_seuil-6.csv", header=TRUE)


#pdf(file="sim_t0_beta_20_30.pdf",width=4,height = 4)
#plot de différents t0
plot(t0.seuil~dilution, data=res, log="x", type="n", ylab="t0")
lgd_ = rep(NA, 10)
lgd_[c(1,10)] = c(20,30)
legend("topleft", inset=0.01,legend = lgd_,
       fill = colorRampPalette(colors = c('black','grey96'))(10),
       border = NA,
       y.intersp = 0.5, cex=0.75)
lapply(split(res,res$beta),function(dtmp) lines(t0.seuil~dilution, data=dtmp, type="b",
                                                pch=21, cex=2, bg=grey(beta/i)))

#dev.off()

 
#pdf(file="sim_dc_beta_20_30.pdf",width=4,height = 4)
#plot de différents dc
plot(I(dc.infl/c.infl)~dilution, data=res, log="x", type="n", ylab="slope")
lgd_ = rep(NA, 10)
lgd_[c(1,10)] = c(20,30)
legend("topright", inset=0.01,legend = lgd_,
       fill = colorRampPalette(colors = c('black','grey96'))(10),
       border = NA,
       y.intersp = 0.5, cex=0.5)
lapply(split(res,res$beta),function(dtmp) lines(I(dc.infl/c.infl)~dilution, data=dtmp, type="b",
                                                 pch=21, cex=2, bg=grey(beta/i)))
#dev.off()

#Graph : t0 en fonction de la pente
#pdf(file="sim_t0slope_beta_20_30_seuil-6.pdf",width=8,height = 6)
plot(t0.seuil~I(dc.infl/c.infl), data=res, xlab="slope", ylab="t0", type="n")
# lgd_ = rep(NA, 10)
# lgd_[c(1,10)] = c(20,30)
# legend("topright", inset=0.01,legend = lgd_,
#        fill = colorRampPalette(colors = c('black','grey96'))(10),
#        border = NA,
#        y.intersp = 0.5, cex=0.75)
lapply(split(res,res$beta),function(dtmp) lines(t0.seuil~I(dc.infl/c.infl), data=dtmp, type="b",
                                                 pch=21, cex=2, bg=grey(beta/30)))
#dev.off()

omegares <- read.table(file="sim_samebetaomega_seuil-6.csv", header=TRUE)
betares <- read.table(file="sim_beta_20_30_seuil-6.csv", header=TRUE)
# #Plot avec beta et omega dessus
#* pdf(file="sim_omegabeta_seuil-6.pdf",width=8,height = 6)
plot(t0.seuil~I(dc.infl/c.infl), data=betares, xlab="slope", ylab="t0", type="n")
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
                                                           pch=21, cex=2, bg=grey(omega/276)))
lapply(split(betares,betares$beta),function(dtmp) lines(t0.seuil~I(dc.infl/c.infl), data=dtmp, type="b",
                                                        pch=25, cex=2, bg=grey(beta/30)))
#* dev.off()


 out <- dynamique(parameters, t=temps)
# #Affiche un graphique s'il y a aucune ou 1 dillution
 graph.dyn(out, which=4)

 
dtmp <- attr(out, "dynamique")
dtmp$c <- with(dtmp, c1+c2+c3+c4+c5)
dtmp$pr <- with(dtmp, c/(s+c+p))
plot(pr~time, data=dtmp, log="xy")


#Trouver omega pour beta donné
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

param2[7,1] <- min.omega.b$estimate[1]

res <- read.table(file="sim_beta_20_30_seuil-6.csv", header=TRUE)
res <- split(res,res$beta)
omega.b <- NULL
for (i in 1:11){
  out <- as.data.frame(res[i])
  colnames(out) <- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max", "t0.seuil", paste0("t0_", parameters$k), "dilution", "beta")
  min.omega.b <- find.omega.b(parameters, out)
  newpara <- c(min.omega.b$estimate[1], min.omega.b$estimate[2])
  omega.b <- rbind(omega.b, newpara)
}
colnames(omega.b) <- c("omega", "b")


# #Variation du nombre de classe pour beta et alpha fixes
# for (i in 1:10){
#   nbclass <- i
#   p$alpha <- rep(p$alpha[1], nbclass)
#   p$beta <- rep(p$beta[1], nbclass)
#   out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000), tmax=1000)
#   out$nbclass <- i
#   res <- rbind (res,out)
# }


 # #Variation de beta quand beta_bar fixe
 # for (i in 2:10){
 #   nbclass <- i
 #   p$beta <- rep(0, nbclass)
 #   p$alpha <- rep(0, nbclass+1)
 #   p$beta[2:nbclass] <- beta_i(p$beta_bar,p$e,i-1)
 #   out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
 #   out$nbclass <- i
 #   out$beta <- p$beta[i]
 #   res <- rbind (res,out)
 # }

# #Plot de beta_bar
# plot(beta_i(10,1.5,10), xlab="i", ylab="Beta_i", col="red", pch=16)
# points(beta_i(10,1.5,5), col="orange", pch=16)
# points(beta_i(10,0.5,10), col="blue", pch=16)
# points(beta_i(10,0.5,5), col="cyan", pch=16)
# legend("topleft", legend=c("e=1.5, n=10","e=1.5, n=5", "e=0.5, n=10", "e=0.5, n=5"), col=c("red", "orange", "blue", "cyan"), pch=16)

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
legend("bottomright", legend=c("e=8, n=10","e=10, n=10", "e=3, n=5", "e=5, n=5"), col=c("red", "orange", "blue", "cyan"), pch=16)

# #Beta pour différentes valeurs de e, beta_bar, et n, stocké dans une table
# for (e in c(2,5,8)){
#   p$e <- e
#   for(beta_bar in c(5,10,15,20)){
#     p$beta_bar <- beta_bar
#     for (i in 2:15){
#       nbclass <- i
#       p$alpha <- rep(0, nbclass+1)
#       p$beta <- rep(0,nbclass)
#       p$beta[2:nbclass] <- beta_i_bell(p$beta_bar,p$e,i-1)
#       out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000), tmax=1000)
#       out$nbclass <- i
#       out$e <- e
#       out$beta_bar <- beta_bar
#       res <- rbind (res,out)
#     }
#   }
# }
#write.table(res, file="sim_betavar_betabar_e_nbclass.csv",row.names = FALSE)

#Alpha pour différentes valeurs de e, beta_bar, et n, stocké dans une table
#write.table(res, file="sim_alphavar_betabar_e_nbclass.csv",row.names = FALSE)


##BETA
 # 
 # res <- read.table(file="sim_betavar_betabar_e_nbclass.csv", header=TRUE)
 # res$ID <- factor(with(res,paste(e,beta_bar,nbclass,sep="_")))
 # #Beta en fonction de n, e et beta_bar
 # tableau <- split(res, res$ID)
 # #Calcul des ratios entre la plus grande et la plus petite dilution de dc, t0 et max
 # difference <- t(sapply(tableau, function(dtmp) {
 #   n <- dtmp$nbclass[1]
 #   e <- dtmp$e[1]
 #   beta_bar <- dtmp$beta_bar[1]
 #   beta1 <- 1/factorial(n)^(e/n)
 #   c(n,e,beta_bar,beta1, dtmp$dc.infl[1]/dtmp$dc.infl[nrow(dtmp)], dtmp$t0_1000[1]/dtmp$t0_1000[nrow(dtmp)],dtmp$c.max[1]/dtmp$c.max[nrow(dtmp)])}))
 # difference <- as.data.frame(difference)
 # colnames(difference) <- c("nbclass", "e", "beta_bar", "beta_2", "slope_ratio", "t0_ratio", "max_ratio")
 # #Affichage du slope ratio sur un graphique
 # plot(slope_ratio~beta_2, data=subset(difference, beta_bar==10), bg=e*4, pch=21)
 # l <- difference$beta_bar==10
 # sapply(split(difference[l,],difference$e[l]), function(dtmp) lines(slope_ratio~beta_2, data=dtmp[order(dtmp$beta_2),]))
 # legend("topleft",legend=c("e=0.5","e=0.75", "e=1.25", "e=1.5"), pch=21, pt.bg=c("purple", "cyan", "green", "red"))
 # #Affichage du t0 ratio sur un graphique
 # plot(t0_ratio~beta_2, data=subset(difference, beta_bar==10), bg=e*4, pch=21)
 # l <- difference$beta_bar==10
 # sapply(split(difference[l,],difference$e[l]), function(dtmp) lines(t0_ratio~beta_2, data=dtmp[order(dtmp$beta_2),]))
 # legend("topright",legend=c("e=0.5","e=0.75", "e=1.25", "e=1.5"), pch=21, pt.bg=c("purple", "cyan", "green", "red"))
 # #Affichage du max ratio sur un graphique
 # plot(max_ratio~beta_2, data=subset(difference, beta_bar==10), bg=e*4, pch=21)
 # l <- difference$beta_bar==10
 # sapply(split(difference[l,],difference$e[l]), function(dtmp) lines(max_ratio~beta_2, data=dtmp[order(dtmp$beta_2),]))
 # legend("topleft",legend=c("e=0.5","e=0.75", "e=1.25", "e=1.5"), pch=21, pt.bg=c("purple", "cyan", "green", "red"))

##ALPHA
# res <- read.table(file="sim_alphavar_betabar_e_nbclass.csv", header=TRUE)
# res$ID <- factor(with(res,paste(e,alpha_bar,nbclass,sep="_")))
# #Beta en fonction de n, e et beta_bar
# tableau <- split(res, res$ID)
# #Calcul des ratios entre la plus grande et la plus petite dilution de dc, t0 et max
# difference <- t(sapply(tableau, function(dtmp) {
#   n <- dtmp$nbclass[1]
#   e <- dtmp$e[1]
#   alpha_bar <- dtmp$alpha_bar[1]
#   alpha1 <- 1/factorial(n)^(e/n)
#   c(n,e,alpha_bar,alpha1, dtmp$dc.infl[1]/dtmp$dc.infl[nrow(dtmp)], dtmp$t0_1000[1]/dtmp$t0_1000s[nrow(dtmp)],dtmp$c.max[1]/dtmp$c.max[nrow(dtmp)])}))
# difference <- as.data.frame(difference)
# colnames(difference) <- c("nbclass", "e", "alpha_bar", "alpha_1", "slope_ratio", "t0_ratio", "max_ratio")
# #Affichage du slope ratio sur un graphique
# plot(slope_ratio~alpha_1, data=subset(difference, alpha_bar==10), bg=e*4, pch=21)
# l <- difference$alpha_bar==10
# sapply(split(difference[l,],difference$e[l]), function(dtmp) lines(slope_ratio~alpha_1, data=dtmp[order(dtmp$alpha_1),]))
# legend("topleft",legend=c("e=0.5","e=0.75", "e=1.25", "e=1.5"), pch=21, pt.bg=c("purple", "cyan", "green", "red"))
# #Affichage du t0 ratio sur un graphique
# plot(t0_ratio~alpha_1, data=subset(difference, alpha_bar==10), bg=e*4, pch=21)
# l <- difference$alpha_bar==10
# sapply(split(difference[l,],difference$e[l]), function(dtmp) lines(t0_ratio~alpha_1, data=dtmp[order(dtmp$alpha_1),]))
# legend("topright",legend=c("e=0.5","e=0.75", "e=1.25", "e=1.5"), pch=21, pt.bg=c("purple", "cyan", "green", "red"))
# #Affichage du max ratio sur un graphique
# plot(max_ratio~alpha_1, data=subset(difference, alpha_bar==10), bg=e*4, pch=21)
# l <- difference$alpha_bar==10
# sapply(split(difference[l,],difference$e[l]), function(dtmp) lines(max_ratio~alpha_1, data=dtmp[order(dtmp$alpha_1),]))
# legend("topleft",legend=c("e=0.5","e=0.75", "e=1.25", "e=1.5"), pch=21, pt.bg=c("purple", "cyan", "green", "red"))



# #Plot avec beta et omega dessus
# omegares <- read.table(file="sim_omega_b_samebeta.csv", header=TRUE)
# betares <- read.table(file="sim_beta_5_15.csv", header=TRUE)
# # #Plot avec beta et omega dessus
# # pdf(file="sim_omegabeta_t0_slope.pdf",width=8,height = 6)
# plot(t0_1000~dc.infl, data=omegares, xlab="slope", ylab="t0", type="n")
# # lgd_ = rep(NA, 10)
# # lgd_[c(1,5,10)] = c(5,"omega",15)
# # space=rep(NA, 2)
# # lgd_2 = rep(NA, 10)
# # lgd_2[c(1,5,10)] = c(1,"beta",10)
# # legend("topright" ,legend = c(lgd_, space, lgd_2),
# #        fill = c(colorRampPalette(colors = c('black','grey96'))(10), "white", "white",colorRampPalette(colors = c('black','grey96'))(10)),
# #        border = NA,
# #        y.intersp = 0.5, cex=0.7)
# # legend("right", legend=c("omega","beta"), pch=c(21,25), cex=0.8)
# lapply(split(omegares,omegares$omega),function(dtmp) lines(t0_1000~dc.infl, data=dtmp, type="b",
#                                                            pch=21, cex=2, bg=grey(omega/param2[11,1])))
# lapply(split(betares,betares$beta),function(dtmp) lines(t0_1000~dc.infl, data=dtmp, type="b",
#                                                         pch=25, cex=2, bg=grey(beta/15)))
# # dev.off()

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


#write.table(res, file="sim_beta_10_20.csv",row.names = FALSE)
#write.table(res, file="sim_beta_5_15.csv",row.names = FALSE)
#write.table(res, file="sim_beta_20_30.csv",row.names = FALSE)
#write.table(res, file="sim_beta_1_10.csv",row.names = FALSE)
#write.table(res, file="sim_beta_0.1_1.csv",row.names = FALSE)
#write.table(res, file="sim_omega_5_15_b1.1_as1_alpha0.6.csv",row.names = FALSE)
#write.table(res, file="sim_omega_5_15_b2_as1_alpha0.6.csv",row.names = FALSE)

#write.table(omega.b, file="parameters_omega_b.csv",row.names = FALSE)
#write.table(omega.b, file="parameters_omega_as.csv",row.names = FALSE)
#write.table(omega.b, file="parameters_omega_b_bell.csv",row.names = FALSE)
#write.table(omega.b, file="parameters_omega_b_beta2030.csv",row.names = FALSE)
#write.table(omega.b, file="parameters_omega_b_lambda_1_5.csv",row.names = FALSE)
#write.table(res, file="sim_omega_b_samebeta.csv",row.names = FALSE)
#write.table(res, file="sim_omega_b_samebeta_2030.csv",row.names = FALSE)
#write.table(res, file="sim_omega_as_samebeta.csv",row.names = FALSE)
#*write.table(res, file="sim_omega_b_samebetabell.csv",row.names = FALSE)
#write.table(res, file="sim_beta_bell_betabar_20_e_3.csv",row.names = FALSE)

#write.table(res, file="sim_beta_lambda_1_5.csv",row.names = FALSE)
#write.table(res, file="sim_omega_lambda_1_5.csv",row.names = FALSE)
#write.table(res, file="sim_omega_b_samebeta_lambda.csv",row.names = FALSE)

#write.table(res, file="sim_omega_seuil_6_1.csv",row.names = FALSE)
#write.table(res, file="sim_beta_seuil_6_1.csv",row.names = FALSE)

#write.table(res, file="sim_omega_5_15_seuil-6.csv",row.names = FALSE)
#write.table(res, file="sim_beta_20_30_seuil-6.csv",row.names = FALSE)
#write.table(res, file="sim_beta_5_15_seuil-6.csv",row.names = FALSE)
#write.table(res, file="sim_samebetaomega_seuil-6.csv",row.names = FALSE)
#write.table(param2, file="parameters_omega_b_seuil-6.csv",row.names = FALSE)



