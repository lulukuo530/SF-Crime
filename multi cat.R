#multinom
#---- in newCategory ----#
newtrain$fourtime[which(newtraina$Hour=="0"|newtrain$Hour=="1"|newtrain$Hour=="2"|newtrain$Hour=="3"|newtrain$Hour=="4"|newtrain$Hour=="5")]<-"twilight"
newtrain$fourtime[which(newtrain$Hour=="6"|newtrain$Hour=="7"|newtrain$Hour=="8"|newtrain$Hour=="9"|newtrain$Hour=="10"|newtrain$Hour=="11")]<-"morning"
newtrain$fourtime[which(newtrain$Hour=="12"|newtrain$Hour=="13"|newtrain$Hour=="14"|newtrain$Hour=="15"|newtrain$Hour=="16"|newtrain$Hour=="17")]<-"afternoon"
newtrain$fourtime[which(newtrain$Hour=="18"|newtrain$Hour=="19"|newtrain$Hour=="20"|newtrain$Hour=="21"|newtrain$Hour=="22"|newtrain$Hour=="23")]<-"night"
library(nnet)
options(contrast=c("contr.treatment","contr.poly"))
fit=multinom(newCategory~DayOfWeek+PdDistrict,data=newtrain)
fitxy=multinom(newCategory~DayOfWeek+X+Y, data=newtrain)
fityear=multinom(newCategory~year+PdDistrict,data=newtrain)
fithour=multinom(newCategory~Hour+PdDistrict,data=newtrain)
fitfourtime=multinom(newCategory~fourtime+PdDistrict,data=newtrain)
#model comparison
library(lmtest)
lrtest(fit,fitxy)
#df bigger is Ha, p-value reject H0, so model1(DayOfWeek+PdDistrict) is better
#too complicated so we use fit predictions
#fit predictions
newtrain$newCategory<-as.factor(newtrain$newCategory)
newpred <- predict(fit,type = "c")
newpred<-function(m,r){
  t2014.1<-predict(m,type="c")
  t2014.2<-0
  t2014.3<-as.numeric(t2014.1)-as.numeric(r)
  t2014.4<-cbind(t2014.3,t2014.2)
  t2014.4[which(t2014.4[,1]==0),2]<-1
  return(sum(t2014.4[,2])/nrow(t2014.4))}
newpred(fit, newtrain$newCategory)
newpred(fitxy, sample_data$newCategory)  ###lon&lat worse, PdDistrict is better
newpred(fityear, sample_data$newCategory)
newpred(fithour, sample_data$newCategory)
newpred(fitfourtime,sample_data$newCategory)
###newCategory~fourtime+PdDistrict is the best

###Do cross validation in fitfourtime
##Data split / only one time
library(caret)
Train <- createDataPartition(sample_data$newCategory, p=0.9, list=FALSE)
training <- sample_data[ Train, ]
testing <- sample_data[ -Train, ]
pred = predict(fitfourtime, newdata=testing)
accuracy <- table(pred, testing[,"newCategory"])
sum(diag(accuracy))/sum(accuracy) 
## Ten-fold Cross Validation
train_control=trainControl(method="cv", number=10)
model=train(newCategory~fourtime+PdDistrict, data=sample_data,
            trControl=train_control, method="multinom")
predictions <- predict(model, sample_data)
confusionMatrix(predictions, sample_data$newCategory)  #Accuracy : 0.3705         
###FINAL MODEL : newCategory~fourtime+PdDistrict 
table(predictions)

summary(fitfourtime)
z=summary(fitfourtime)$coefficients/summary(fitfourtime)$standard.errors
z
z.test=(1 - pnorm(abs(z), 0, 1))*2
z.test   #p-value
RelvRisk=exp(coef(fitfourtime))   #relative risk coefficients
ff=fitted(fitfourtime)   #predicted probabilities

###visualization
sample_data$fourtime=as.factor(sample_data$fourtime)
fitfourtime=multinom(newCategory~fourtime+PdDistrict,data=sample_data)
library(effects)
fourtime=factor(fourtime, levels=c("twilight","morning","afternoon","night"))
plot(effect("fourtime*PdDistrict",fitfourtime))
plot(allEffects(fitfourtime),ask=FALSE)

DDframe=data.frame(fourtime=rep(c("twilight","morning","afternoon","night"),each=25000),
                   PdDistrict=rep(c("BAYVIEW","INGLESIDE","CENTRAL","NORTHERN","MISSION"
                                    ,"RICHMOND","SOUTHERN","PARK","TENDERLOIN","TARAVAL"),10000))
PredDD=cbind(DDframe,predict(fitfourtime,newdata=DDframe,type="probs",se=TRUE))
library(reshape2)
library(ggplot2)
MeltDD=melt(PredDD,id.vars=c("fourtime","PdDistrict"),value.name="Probability")
MeltDD$DayOfWeek=factor(MeltDD$fourtime,levels=c("twilight","morning","afternoon","night"))
dw_fp=ggplot(MeltDD,aes(x=fourtime,y=Probability,color=PdDistrict,group=PdDistrict)) +
      geom_line() +
      geom_point() +
      facet_grid(variable~., scales="free") +
      labs(title="fourtime vs PdDistrict in each newCategory")
dw_fp
#-------------------------------#


#---- in Category top 10 ----#
#fourtime+PdDistrict vs top 10 crime
od<-summary(newtrain$Category)[order(summary(newtrain$Category),decreasing=T)]
x<-vector("list",10)
for(j in 1:10){
  x[[j]]<-subset(newtrain,newtrain$Category==names(od[j]))
}
newdata=rbind(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]],x[[8]],x[[9]],x[[10]])
sm_top10=newdata[sample(nrow(newdata),100000,replace=FALSE),]
top10_fourtime=multinom(Category~fourtime+PdDistrict,data=sm_top10)
top10_year=multinom(Category~year+PdDistrict,data=sm_top10)
top10_hour=multinom(Category~Hour+PdDistrict,data=sm_top10)
top10_dow=multinom(Category~DayOfWeek+PdDistrict,data=sm_top10)
#fit predictions
sm_top10$Category=as.factor(paste0(sm_top10$Category))
newpred(top10_fourtime, sm_top10$Category)
newpred(top10_year, sm_top10$Category)
newpred(top10_hour, sm_top10$Category)
newpred(top10_dow, sm_top10$Category)
###top10_fourtime is the best
###ten-fold cv
#top10_hour
train_control=trainControl(method="cv", number=10)
model=train(Category~Hour+PdDistrict, data=sm_top10,
            trControl=train_control, method="multinom")
predictions <- predict(model, sm_top10)
confusionMatrix(predictions, sm_top10$Category)   #Accuracy : 0.2654 

#top10_fourtime
model=train(Category~fourtime+PdDistrict, data=sm_top10,
            trControl=train_control, method="multinom")
predictions <- predict(model, sm_top10)
confusionMatrix(predictions, sm_top10$Category)   #Accuracy : 0.2672 

summary(top10_fourtime)
z=summary(top10_fourtime)$coefficients/summary(top10_fourtime)$standard.errors
z.test=(1 - pnorm(abs(z), 0, 1))*2
z.test   #p-value
RelvRisk=exp(coef(top10_fourtime))   #relative risk coefficients
ff=fitted(top10_fourtime)   #predicted probabilities

###visualization
DDframe=data.frame(fourtime=rep(c("twilight","morning","afternoon","night"),each=25000),
                   PdDistrict=rep(c("BAYVIEW","INGLESIDE","CENTRAL","NORTHERN","MISSION","RICHMOND","SOUTHERN","PARK","TENDERLOIN","TARAVAL"),10000))
PredDD=cbind(DDframe,predict(top10_fourtime,newdata=DDframe,type="probs",se=TRUE))
MeltDD=melt(PredDD,id.vars=c("fourtime","PdDistrict"),value.name="Probability")
MeltDD$fourtime=factor(MeltDD$fourtime,levels=c("twilight","morning","afternoon","night"))
tpf=ggplot(MeltDD,aes(x=fourtime,y=Probability,color=PdDistrict,group=PdDistrict)) +
  geom_line() +
  geom_point() +
  facet_grid(variable~., scales="free") +
  labs(title="fourtime vs PdDistrict in each Category")
tpf

#Put more variables in the model
top10_ydp=multinom(Category~year+DayOfWeek+PdDistrict,data=sm_top10)
top10_dyfp=multinom(Category~DayOfWeek+year+fourtime+PdDistrict,data=sm_top10)
newpred(top10_ydp, sm_top10$Category)
newpred(top10_dyfp, sm_top10$Category)
lrtest(top10_dyfp,top10_hour)   ###top10_dyfp is better
###FINAL MODEL : Category ~ DayOfWeek + year + fourtime + PdDistrict
## Ten-fold CV in top10_dyfp
train_control=trainControl(method="cv", number=10)
model=train(Category~DayOfWeek+year+fourtime+PdDistrict,data=sm_top10,
            trControl=train_control, method="multinom")
predictions=predict(model, sm_top10)   #Accuracy : 0.2668
confusionMatrix(predictions, sm_top10$Category)

#Category~DayOfWeek+year+fourtime+PdDistrict
#analysis
summary(top10_dyfp)   # too compliecated no information
z=summary(top10_dyfp)$coefficients/summary(top10_dyfp)$standard.errors
z
z.test=(1 - pnorm(abs(z), 0, 1))*2
z.test   #p-value
RelvRisk=exp(coef(top10_dyfp))   #relative risk coefficients
ff=fitted(top10_dyfp)   #predicted probabilities

#visualization
# parallel set in R #

#Only three variables but almost the same accuracy as the above model
#Category~Hour+year+PdDistrict
top10_hyp=multinom(Category~Hour+year+PdDistrict, data=sm_top10)
newpred(top10_hyp, sm_top10$Category)
train_control=trainControl(method="cv", number=10)
model=train(Category~Hour+year+PdDistrict,data=sm_top10,
            trControl=train_control, method="multinom")
predictions=predict(model, sm_top10)   #Accuracy : 0.2663
confusionMatrix(predictions, sm_top10$Category)

#_______#----------#
#analysis
summary(top10_hyp)   # too compliecated no information
z=summary(top10_hyp)$coefficients/summary(top10_dyfp)$standard.errors
z.test=(1 - pnorm(abs(z), 0, 1))*2
z.test   #p-value
RelvRisk=exp(coef(top10_hyp))   #relative risk coefficients
ff=fitted(top10_hyp)   #predicted probabilities






