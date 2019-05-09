plydata<-read.csv("C:/Users/liuha/Desktop/2017-18_NBA_salary.csv", header = TRUE)
library("VIM")
library("ggplot2")
library("reshape2")
library("faraway")
library("MASS")
library("leaps")
library("car")

#####################################################################################

##visualize if there is any NA data
a<-aggr(plydata,prop=FALSE,numbers=TRUE)
summary(a)
matrixplot(plydata)
newdata<-plydata[complete.cases (plydata),]
#####################################################################################

##show the density of the salary in season 2017-2018
ggplot(newdata, aes(x=Salary/1000, colour=newdata$Tm))+geom_density(position = "identity")+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#####################################################################################

##see the correaltion between each predictor via graph
newdata2<-newdata[,-c(1,3,6)]
cor(newdata2)
cormat<-cor(newdata2)
melt_cormat<-melt(cormat)
head(melt_cormat)
ggplot(data = melt_cormat, aes(Var1, Var2, fill=value))+geom_tile()
#####################################################################################

##Select the most suitable predictors via leaps
leapmod<-regsubsets(Salary~., data=newdata2, nbest=6)
plot(leapmod, scale='adjr2')
##draft_number, G, MP, DRB,USG, DWS,OWS,VORP

##Select predictor via "bic" method
out0=regsubsets(Salary~., data=newdata2, method="forwardward", nvmax=20)
out00=summary(out0)
plot(out00$bic, main="forward search: BIC")

##select model by adding both predictors from bic and leaps
mod<-lm(Salary~NBA_DraftNumber+Age+G+MP+PER+newdata$DRB.+newdata2$USG.+newdata2$OWS+newdata2$OWS+newdata2$VORP,data = newdata2)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
##eliminate predictor which p-value greeater than 0.05
mod2<-lm(Salary~NBA_DraftNumber+Age+G+MP+newdata$DRB.+newdata2$OWS+newdata2$VORP,data=newdata2)
##use power transformation
par(mfrow=c(1,1))
bc=boxcox(mod2,lambda = seq(-2, 4, 1/10), plotit = TRUE)
best.lam=bc$x[which(bc$y==max(bc$y))]
best.lam
##After power transformation, the new model
mod3<-lm(Salary^0.24~NBA_DraftNumber+Age+G+MP+newdata2$DRB.+newdata2$OWS+newdata2$VORP,data = newdata2)
summary(mod3)
##eliminate predictor again
mod4<-lm(Salary^0.24~NBA_DraftNumber+Age+G+MP+newdata2$DRB., data=newdata2)
summary(mod4)
plot(mod4)
#####################################################################################

##find the influencial points
cook<-cooks.distance(mod4)
halfnorm(cook,5,labs=newdata$Player,ylab="Cook's distances")
#row 355, 274, 329,370, 377are leverage point
##use jacknife to find the outlier
stud <- rstudent(mod4)
jackres <- stud*((486-24-2)/(486-24-1-stud^2))^0.5
head(jackres[order(abs(stud),decreasing=T)])
#row 354, 328, 273, 428, 114, 376 are outlier
##exclude the outliers points
newdata3<-newdata[-c(354, 328, 273, 428, 114, 376),-c(1,3,6)]
#####################################################################################

mod5<-lm(Salary^0.24~NBA_DraftNumber+MP+Age+I(Age^2)+I(Age^3)+G+I(G^2)+MP+DRB.,data=newdata3)
summary(mod5)
par(mfrow=c(2,2))
plot(mod5)
shapiro.test(mod5$residuals)
ncvTest(mod5)
vif(mod5)
#P-value is 0.7008, which is greater than 0.05, so H0 is not rejected and data are normally distributed
#P-value is 0.90882, which is not significant and satisfy the constant variance assumption
##use partial regression
avPlots(mod5,ask=FALSE, id.method="identify")
#####################################################################################

#prediction
predata<-plydata[, c(4,5,7,8,14)]
x<-c()
y<-c()
z<-c()
for (i in 1:485){
  x<-c(1,as.numeric(predata[i,]))
  y[i]<-sum(x*coef(mod5))
  z[i]<-y[i]^(1/0.24)
  print(z[i])
}
transz<-as.data.frame(z)
names(z)<-(plydata$Player)
name<-strsplit((names(z))," ")
lastname<-sapply(name, "[", 2)
predata2<-cbind(transz, lastname, plydata$Tm, plydata$NBA_DraftNumber)
ggplot(predata2, aes(predata2$`plydata$Tm`,predata2$z))+geom_point(colour=predata2$`plydata$NBA_DraftNumber`,size=3)+
  geom_text(aes(y=predata2$z,label=predata2$lastname))

