setwd("C:/Users/Jonathan/Desktop/STAT/Regression/Class work")
psa = read.table("psa.txt", header = T)

#changes the names of everything and takes the log of variables as needed
psa$cancer_volume = log(psa$cancer_volume + 1); names(psa)[3] = "lcavol";
psa$prostate_weight = log(psa$prostate_weight + 1); names(psa)[4] = "lweight";
names(psa)[5] = "age";
psa$benign_prostatic_hyperplasia = log(psa$benign_prostatic_hyperplasia + 1); names(psa)[6] = "lbph";
names(psa)[7] = "svi";
psa$capsular_penetration = log(psa$capsular_penetration + 1); names(psa)[8] = "lcp";
names(psa)[9] = "gleason";

#standardize psa
#psa = scale(psa)

#principle components analysis
psa_log.pca = princomp(x = cbind(psa$lcavol, psa$lweight, psa$age, psa$lbph, psa$svi, psa$lcp, psa$gleason), cor = T)
#summary(psa_log.pca)
cor(cbind(psa$lcavol, psa$lweight, psa$age, psa$lbph, psa$svi, psa$lcp, psa$gleason))

#principle components regression
psa.pca.reg = lm(log(PSA)~psa_log.pca$scores, data = psa)
summary(psa.pca.reg)

#standard regression
psa.lm = lm(PSA~lcavol+lweight+age+lbph+svi+lcp+gleason, data = psa)
summary(psa.lm)

#stepwise regression
#step(psa.lm, direction = c("forward"))
#same as full

step(psa.lm, direction = c("backward"))
psa.lm.backward = lm(PSA~lcavol+svi+gleason, data = psa)
summary(psa.lm.backward)
#step(psa.lm, direction = c("both"))
#same as backwards




#which is best
random10 = sample(1:97, 10, replace = F)

predict(psa.lm, psa[random10,])
predict(psa.lm.backward, psa[random10,])

forPredict = psa; names(forPredict)[3] = "psa_log.pca$scoresComp.1"; names(forPredict)[4] = "psa_log.pca$scoresComp.2"; names(forPredict)[5] = "psa_log.pca$scoresComp.3"; names(forPredict)[6] = "psa_log.pca$scoresComp.4"; names(forPredict)[7] = "psa_log.pca$scoresComp.5"; names(forPredict)[8] = "psa_log.pca$scoresComp.6"; names(forPredict)[9] = "psa_log.pca$scoresComp.7";
predict(psa.pca.reg, forPredict[random10,])
forPredict[random10,]
