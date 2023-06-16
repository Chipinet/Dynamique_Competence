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
                   lambda=1,
                   #Répétition de la valeur de alpha, tout les alphas ont la même valeur
                   alpha=c(0.01, rep(0.6, nbclass)),
                   omega=0,
                   b=2,
                   a_s=1,
                   #Idem que pour alpha
                   beta=c(0, rep(0, nbclass-1)),
                   e=2,
                   beta_bar=20)


#parameters$beta[2:nbclass] <- beta_i(parameters$beta_bar,parameters$e,nbclass-1)
#parameters$beta[2:nbclass] <- beta_i_bell(parameters$beta_bar,parameters$e,nbclass-1)

res <- NULL
p <- parameters


param2 <- read.table(file="parameters_omega_b_lambda_1_5.csv", header=TRUE)
# #Variations d'un paramètre
for (i in 1:5){
  p$lambda <- i
  p$omega <- param2[i,1]
  p$b <- param2[i,2]
  out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
  out$omega <- param2[i,1]
  out$b <- param2[i,2]
  out$lambda <- i
  res <- rbind (res,out)
}

# p$omega <- param2[1,1]
# p$b <- param2[1,2]
# for (i in 1:5){
#   p$lambda <- i
#   out <- dynamique(p, dilution=c(1, 10, 50, 100, 500, 1500, 3000))
#   out$lambda <- i
#   res <- rbind (res,out)
# }


#write.table(res, file="sim_omegavar_lambda_1_5.csv",row.names = FALSE)
#res <- read.table(file="sim_omega_5_15_b2_as1_alpha0.6.csv", header=TRUE)

#pdf(file="sim_t0_omega_lambda_1_5.pdf",width=4,height = 4)
#plot de différents t0
plot(t0_1000~dilution, data=res, log="x", type="n")
lgd_ = rep(NA, 5)
lgd_[c(1,5)] = c(1,5)
legend("topleft", inset=0.01,legend = lgd_,
       fill = colorRampPalette(colors = c('black','grey96'))(5),
       border = NA,
       y.intersp = 0.5, cex=0.75)
lapply(split(res,res$lambda),function(dtmp) lines(t0_1000~dilution, data=dtmp, type="b",
                                                pch=21, cex=2, bg=grey(lambda/i)))

#dev.off()
 
#pdf(file="sim_dc_beta_omega_1_5.pdf",width=4,height = 4)
#plot de différents dc
plot(dc.infl~dilution, data=res, log="x", type="n", ylab="slope")
lgd_ = rep(NA, 5)
lgd_[c(1,5)] = c(1,5)
legend("topright" ,legend = lgd_,
       fill = colorRampPalette(colors = c('black','grey96'))(5),
       border = NA,
       y.intersp = 0.5, cex=0.75)
lapply(split(res,res$lambda),function(dtmp) lines(dc.infl~dilution, data=dtmp, type="b",
                                                 pch=21, cex=2, bg=grey(lambda/i)))
#dev.off()

#Graph : t0 en fonction de la pente
#pdf(file="sim_t0slope_beta_omega_1_5.pdf",width=4,height = 4)
plot(t0_1000~dc.infl, data=res, xlab="slope", ylab="t0", type="n")
lgd_ = rep(NA, 5)
lgd_[c(1,5)] = c(1,5)
legend("topright" ,legend = lgd_,
       fill = colorRampPalette(colors = c('black','grey96'))(5),
       border = NA,
       y.intersp = 0.5, cex=0.75)
lapply(split(res,res$lambda),function(dtmp) lines(t0_1000~dc.infl, data=dtmp, type="b",
                                                 pch=21, cex=2, bg=grey(lambda/i)))
#dev.off()

omegares <- read.table(file="sim_omegavar_lambda_1_5.csv", header=TRUE)
betares <- read.table(file="sim_beta_lambda_1_5.csv", header=TRUE)
# #Plot avec beta et omega dessus
# pdf(file="sim_omegabeta_t0_slope_lambda1_5_omegavar.pdf",width=8,height = 6)
plot(t0_1000~dc.infl, data=betares, xlab="slope", ylab="t0", type="n", xlim=c(0.59,1.16), ylim=c(1.32,11.3))
 lgd_ = rep(NA, 5)
 lgd_[c(1,3,5)] = c(1,"lambda",5)
# space=rep(NA, 2)
# lgd_2 = rep(NA, 10)
# lgd_2[c(1,5,10)] = c(1,"beta",10)
legend("right" ,legend = c(lgd_),#, space, lgd_2),
       fill = c(colorRampPalette(colors = c('black','grey96'))(5)),#, "white", "white",colorRampPalette(colors = c('black','grey96'))(10)),
       border = NA,
       y.intersp = 0.5, cex=0.7)
legend("topright", legend=c("omega","beta"), pch=c(21,25), cex=0.8)
lapply(split(omegares,omegares$lambda),function(dtmp) lines(t0_1000~dc.infl, data=dtmp, type="b",
                                                           pch=21, cex=2, bg=grey(lambda/5)))
lapply(split(betares,betares$lambda),function(dtmp) lines(t0_1000~dc.infl, data=dtmp, type="b",
                                                        pch=25, cex=2, bg=grey(lambda/5)))
# dev.off()


 out <- dynamique(parameters, dilution=3000, t=temps)
# #Affiche un graphique s'il y a aucune ou 1 dillution
 graph.dyn(out, which=4)


#Trouver omega pour beta donné
res <- read.table(file="sim_beta_lambda_1_5.csv", header=TRUE)
res <- split(res,res$lambda)
out <- as.data.frame(res[5])
colnames(out) <- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max", paste0("t0_", parameters$k), "dilution", "lambda")

min.omega.b <- find.omega.b(parameters, out)
parameters_2 <- parameters
parameters_2$omega <- min.omega.b$estimate[1]
parameters_2$b <- min.omega.b$estimate[2]
parameters_2$beta <- rep(0, length(parameters$beta))
out2 <- dynamique(parameters_2, t=temps)


res <- read.table(file="sim_beta_lambda_1_5.csv", header=TRUE)
res <- split(res,res$lambda)
omega.b <- NULL
for (i in 1:5){
  parameters$lambda <- i
  out <- as.data.frame(res[i])
  colnames(out) <- c("t.infl", "c.infl", "dc.infl", "t.max", "c.max", paste0("t0_", parameters$k), "dilution", "lambda")
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
# beta_2 <- rep(0,10)
# beta_2[2:10] <- beta_i_bell(20,2,9)
# plot(x=c(1:10), y=beta_2, xlab="i", ylab="Beta_i", col="red", pch=16)
# beta_2[2:10] <- beta_i_bell(20,5,9)
# points(x=c(1:10), y=beta_2, col="orange", pch=16)
# beta_2 <- rep(0,5)
# beta_2[2:5] <- beta_i_bell(20,2,4)
# points(x=c(1:5), y=beta_2, col="blue", pch=16)
# beta_2[2:5] <- beta_i_bell(20,5,4)
# points(x=c(1:5), y=beta_2, col="cyan", pch=16)
# legend("bottom", legend=c("e=2, n=10","e=5, n=10", "e=2, n=5", "e=5, n=5"), col=c("red", "orange", "blue", "cyan"), pch=16)

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


#write.table(res, file="sim_beta_10_20.csv",row.names = FALSE)
#write.table(res, file="sim_beta_5_15.csv",row.names = FALSE)
#write.table(res, file="sim_beta_20_30.csv",row.names = FALSE)
#write.table(res, file="sim_beta_1_10.csv",row.names = FALSE)
#write.table(res, file="sim_beta_0.1_1.csv",row.names = FALSE)
#write.table(res, file="sim_omega_5_15_b1.1_as1_alpha0.6.csv",row.names = FALSE)
#write.table(res, file="sim_omega_5_15_b2_as1_alpha0.6.csv",row.names = FALSE)

#write.table(omega.b, file="parameters_omega_b",row.names = FALSE)
#write.table(omega.b, file="parameters_omega_as.csv",row.names = FALSE)
#write.table(param2, file="parameters_omega_b_bell.csv",row.names = FALSE)
#write.table(omega.b, file="parameters_omega_b_beta2030.csv",row.names = FALSE)
#write.table(omega.b, file="parameters_omega_b_lambda_1_5.csv",row.names = FALSE)
#write.table(res, file="sim_omega_b_samebeta.csv",row.names = FALSE)
#write.table(res, file="sim_omega_as_samebeta.csv",row.names = FALSE)
#write.table(res, file="sim_omega_b_samebetabell.csv",row.names = FALSE)
#write.table(res, file="sim_beta_bell_betabar_20_e_2.csv",row.names = FALSE)

#write.table(res, file="sim_beta_lambda_1_5.csv",row.names = FALSE)
#write.table(res, file="sim_omega_lambda_1_5.csv",row.names = FALSE)
#write.table(res, file="sim_omega_b_samebeta_lambda.csv",row.names = FALSE)





