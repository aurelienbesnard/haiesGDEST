#setwd('C:/Users/besnard/Desktop/A TRAITER/_Acorriger/___IMMEDIAT/___SEMAINE/___A CORRIGER URGENT/donnees HAIES gd Est')

setwd('C:/RECHERCHE/COLLABORATIONS/OISEAUX/BOCAGE/PROTOCOLE GD EST/donnees HAIES gd Est')
data<-read.csv2('donnees_haies_habitats FINAL_prep.csv')
data2<-read.csv2('donnees_haies_habitats FINAL_prep_queprotegSTRATE.csv')
strate_haie<-data2$classe_haie

library(ade4)


df<-data.frame(agri=data$agricole, bati=data$bati, foret=data$foret,lhaie=data$length,
               longueur=data$longueur,hauteur=data$hauteur,largeur=data$largeur)


df2<-data.frame(agri=data$agricole, bati=data$bati, foret=data$foret,lhaie=data$length,
               longueur=data$longueur,hauteur=data$hauteur,largeur=data$largeur,
               bande=as.factor(data$bande), vieux=as.factor(data$vieux),tetards=as.factor(data$tetards), emondes=as.factor(data$emondes),
               cavite=as.factor(data$cavite),morts=as.factor(data$morts),plessee=as.factor(data$plessee))
library(corrplot)
library(RColorBrewer)
M <-cor(df)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

acp<-dudi.pca(df, scannf=TRUE, nf=2)
scatter(acp)
s.label(acp$li)
s.label(acp$co)
cor(df)
sunflowerplot(data$largeur~data$hauteur)

mix<-dudi.mix(df2,scannf=F,nf=2)
scatter(mix)
s.label(mix$co)


s.label(mix$li,clabel=0,boxes=FALSE,cpoint=1,pch=1)
s.class(mix$li, as.factor(strate_haie), add.plot = TRUE)


write.csv2(mix$co,'correlation_mix.csv')
distances<-dist(mix$li)				# récupérer et calculer les distances entre les relévés (les sites)
rp<-hclust(distances,'ward')				# réaliser un dendrogramme (hierarchical clustering) avec la méthode de ward
plot(rp)	

plot(table(as.factor(data$hauteur),as.factor(data$vieux)), main='',xlab='Classe hauteur',ylab='Présence vieux arbres')
plot(table(as.factor(data$hauteur),as.factor(data$tetards)))
plot(table(as.factor(data$hauteur),as.factor(data$cavite)))


chisq.test(as.factor(data$longueur),as.factor(data$vieux))

coord1<-mix$li[,1]
coord2<-mix$li[,2]

oiseaux<-data[,41:115]

oiseauxred<-oiseaux[,colSums(oiseaux)>6]
afc<-dudi.coa(oiseauxred,scannf=F,nf=2)
scatter(afc)
s.label(afc$li,clabel=0,boxes=FALSE,cpoint=1,pch=1)
s.class(afc$li, as.factor(strate_haie), add.plot = TRUE)


R<-rowSums(oiseaux)
hist(R)
mean(R)
median(R)
table(R)/length(R)
occ<-R
occ[occ>1]<-1

# modèle

res<-glm(R~1,family=poisson) # un peu de sur-dispersion initiale

res<-glm(R~Ripisylve,family=poisson,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm(R~Longueur,family=poisson,data=data) # un peu de sur-dispersion initiale
summary(res)

### tout en NB
library(MASS)

res<-glm.nb(R~as.factor(Dept),data=data) # un peu de sur-dispersion initiale
summary(res)
boxplot(R~as.factor(data$Dept))
newdata<-data.frame(Dept=levels(as.factor(data$Dept)))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
require(plotrix)
plotCI(1:10, estim, ui=upIC, li=lowIC,xaxt="n",xlab="Département",ylab='Nombre espèce moyen')
axis(1, at=1:10, labels=newdata$Dept)


prediction<-data.frame(Dept=newdata$Dept,Abondance=estim,lowIC=lowIC,upIC=upIC)

res<-glm.nb(R~ripisylve,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~ornementale,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~intervention,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~taille,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~continuite,data=data) # un peu de sur-dispersion initiale
summary(res)


res<-glm.nb(R~hauteur,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~largeur,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~bande,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~fosse,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~vieux,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~tetards,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~emondes,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~cavites,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~morts,data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~scale(agricole),data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~scale(bati),data=data) # un peu de sur-dispersion initiale
summary(res)

res<-glm.nb(R~scale(foret),data=data) # un peu de sur-dispersion initiale
summary(res)
 
res<-glm.nb(R~scale(length),data=data) # un peu de sur-dispersion initiale
summary(res)


res<-glm.nb(R~coord1,data=data) # un peu de sur-dispersion initiale
summary(res)
newdata<-data.frame(coord1=seq(min(coord1),max(coord1),by=0.01))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
plot(estim~newdata$coord1,xlab="coordonnees sur axe 1",ylab="abondance espèces",ylim=c(0,max(upIC+1)),type='l')
lines(lowIC~newdata$coord1,lty=2)
lines(upIC~newdata$coord1,lty=2)

library(boot)

res<-glm(occ~coord1,family=binomial, data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(coord1=seq(min(coord1),max(coord1),by=0.01))
pred<-predict(res,newdata=newdata,se=T)
estim<-inv.logit(pred$fit)
lowIC<-inv.logit(pred$fit-1.96*pred$se.fit)
upIC<-inv.logit(pred$fit+1.96*pred$se.fit)
plot(estim~newdata$coord1,xlab="coordonnees sur axe 1",ylab="abondance espèces",ylim=c(0,1),type='l')
lines(lowIC~newdata$coord1,lty=2)
lines(upIC~newdata$coord1,lty=2)


res<-glm.nb(R~coord2,data=data) # un peu de sur-dispersion initiale
summary(res)

newdata<-data.frame(coord1=seq(min(coord),max(coord1),by=0.01))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
plot(estim~newdata$coord1,xlab="coordonnees sur axe 1",ylab="abondance espèces",ylim=c(0,max(upIC+1)),type='l')
lines(lowIC~newdata$coord1,lty=2)
lines(upIC~newdata$coord1,lty=2)


res<-glm.nb(R~longueur,data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(longueur=seq(min(data$longueur),max(data$longueur),by=0.01))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
plot(estim~newdata$longueur,xlab="Longueur de la haire",ylab="abondance espèces",ylim=c(0,max(upIC+1)),type='l')
lines(lowIC~newdata$longueur,lty=2)
lines(upIC~newdata$longueur,lty=2)

res<-glm(occ~as.factor(longueur),family=binomial, data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(longueur=c(1,2,3))
pred<-predict(res,newdata=newdata,se=T)
estim<-inv.logit(pred$fit)
lowIC<-inv.logit(pred$fit-1.96*pred$se.fit)
upIC<-inv.logit(pred$fit+1.96*pred$se.fit)
plot(estim~newdata$longueur,xlab="Longueur de la haire",ylab="abondance espèces",ylim=c(0,1),type='l')
lines(lowIC~newdata$longueur,lty=2)
lines(upIC~newdata$longueur,lty=2)


res<-glm.nb(R~vieux,data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(vieux=c('non','oui'))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
plotCI(1:2, estim, ui=upIC, li=lowIC,xaxt="n",xlab="Présence vieux arbres",ylab='Nombre espèce moyen',ylim=c(3,max(upIC)),xlim=c(0.7,2.2))
axis(1, at=1:2, labels=newdata$vieux)

res<-glm(occ~vieux,family=binomial,data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(vieux=c('non','oui'))
pred<-predict(res,newdata=newdata,se=T)
estim<-inv.logit(pred$fit)
lowIC<-inv.logit(pred$fit-1.96*pred$se.fit)
upIC<-inv.logit(pred$fit+1.96*pred$se.fit)
plotCI(1:2, estim, ui=upIC, li=lowIC,xaxt="n",xlab="Présence vieux arbres",ylab='Nombre espèce moyen',ylim=c(0.95,1),xlim=c(0.7,2.2))
axis(1, at=1:2, labels=newdata$vieux)


res<-glm.nb(R~as.factor(hauteur), data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(hauteur=c(1,2,3,4))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
plotCI(1:4, estim, ui=upIC, li=lowIC,xaxt="n",xlab="Classes de hauteurs",ylab='Nombre espèce moyen',ylim=c(0,max(upIC+1)),xlim=c(0.7,4.2))
axis(1, at=1:4, labels=c('<1m','1 à 2m','2 à 3m','>3m'))

res<-glm(occ~hauteur,family=binomial, data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(hauteur=c(1,2,3,4))
pred<-predict(res,newdata=newdata,se=T)
estim<-inv.logit(pred$fit)
lowIC<-inv.logit(pred$fit-1.96*pred$se.fit)
upIC<-inv.logit(pred$fit+1.96*pred$se.fit)
plot(estim~newdata$hauteur,xlab="Longueur de la haire",ylab="abondance espèces",ylim=c(0,1),type='l')
lines(lowIC~newdata$hauteur,lty=2)
lines(upIC~newdata$hauteur,lty=2)


res<-glm.nb(R~continuite, data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(continuite=levels(as.factor(data$continuite)))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
plotCI(c(2,3,1), estim, ui=upIC, li=lowIC,xaxt="n",xlab="Continuité",ylab='Nombre espèce moyen',ylim=c(0,max(upIC+1)),xlim=c(0.7,3.2))
axis(1, at=1:3, labels=c('bcq trouées','avec qq trouées','haie continue'))

res<-glm(occ~continuite, family=binomial,data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(continuite=levels(as.factor(data$continuite)))
pred<-predict(res,newdata=newdata,se=T)
estim<-inv.logit(pred$fit)
lowIC<-inv.logit(pred$fit-1.96*pred$se.fit)
upIC<-inv.logit(pred$fit+1.96*pred$se.fit)
plotCI(c(2,3,1), estim, ui=upIC, li=lowIC,xaxt="n",xlab="Continuité",ylab='Nombre espèce moyen',ylim=c(0,1),xlim=c(0.7,3.2))
axis(1, at=1:3, labels=c('bcq trouées','avec qq trouées','haie continue'))

setwd('C:/Users/besnard/Desktop/A TRAITER/_Acorriger/___IMMEDIAT/___SEMAINE/___A CORRIGER URGENT/donnees HAIES gd Est')

data2<-read.csv2('donnees_haies_habitats FINAL_prep_queprotegSTRATE.csv')
strate_haie<-data2$classe_haie
res<-glm.nb(R~as.factor(strate_haie), data=data) # un peu de sur-dispersion initiale
newdata<-data.frame(strate_haie=c(1,2,3,4))
pred<-predict(res,newdata=newdata,se=T)
estim<-exp(pred$fit)
lowIC<-exp(pred$fit-1.96*pred$se.fit)
upIC<-exp(pred$fit+1.96*pred$se.fit)
plotCI(1:4, estim, ui=upIC, li=lowIC,xaxt="n",xlab="Classes de hauteurs",ylab='Nombre espèce moyen',ylim=c(0,max(upIC+1)),xlim=c(0.7,4.2))
axis(1, at=1:4, labels=c('cl.1','cl.2','cl.3','cl.4'))



