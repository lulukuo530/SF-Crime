###summarize data
library(data.table)
train=read.csv("~/Desktop/NCKU/Consulting/kaggle_crime/train.csv", header=TRUE)
train = fread("~/Desktop/NCKU/Consulting/kaggle_crime/train.csv", sep=",", stringsAsFactors = F)
train$d<-as.Date(train$Dates)
newtrain=subset(train, train$Y!=90)
newdates=strptime(newtrain$Dates,format="%Y-%m-%d")
new_year=newdates$year+1900
new_month=newdates$mon+1
newtrain$year=new_year
newtrain$month=new_month
library(lubridate)
newtrain$Hour=hour(newtrain$Dates)
newtrain$fourtime[which(newtrain$Hour=="0"|newtrain$Hour=="1"|newtrain$Hour=="2"|newtrain$Hour=="3"|newtrain$Hour=="4"|newtrain$Hour=="5")]<-"twilight"
newtrain$fourtime[which(newtrain$Hour=="6"|newtrain$Hour=="7"|newtrain$Hour=="8"|newtrain$Hour=="9"|newtrain$Hour=="10"|newtrain$Hour=="11")]<-"morning"
newtrain$fourtime[which(newtrain$Hour=="12"|newtrain$Hour=="13"|newtrain$Hour=="14"|newtrain$Hour=="15"|newtrain$Hour=="16"|newtrain$Hour=="17")]<-"afternoon"
newtrain$fourtime[which(newtrain$Hour=="18"|newtrain$Hour=="19"|newtrain$Hour=="20"|newtrain$Hour=="21"|newtrain$Hour=="22"|newtrain$Hour=="23")]<-"night"

newtrain$newCategory<-newtrain$Category
newtrain$newCategory<-paste0(newtrain$newCategory)
newtrain$newCategory[which(
  newtrain$newCategory=="BAD CHECKS"|
    newtrain$newCategory=="BRIBERY"|
    newtrain$newCategory=="EMBEZZLEMENT"|
    newtrain$newCategory=="EXTORTION"|
    newtrain$newCategory=="FORGERY/COUNTERFEITING"|
    newtrain$newCategory=="FRAUD"|
    newtrain$newCategory=="GAMBLING"|
    newtrain$newCategory=="SECONDARY CODES"|
    newtrain$newCategory=="SUSPICIOUS OCC")]<-"FINANCIAL"

newtrain$newCategory[which(
  newtrain$newCategory=="ARSON"|
    newtrain$newCategory=="ASSAULT"|
    newtrain$newCategory=="KIDNAPPING"|
    newtrain$newCategory=="WEAPON LAWS"|
    newtrain$newCategory=="FAMILY OFFENSES")]<-"VIOLENCE"

newtrain$newCategory[which(
  newtrain$newCategory=="DRIVING UNDER THE INFLUENCE"|
    newtrain$newCategory=="DRUG/NARCOTIC"|
    newtrain$newCategory=="DRUNKENNESS"|
    newtrain$newCategory=="LIQUOR LAWS"|
    newtrain$newCategory=="VANDALISM"|
    newtrain$newCategory=="DISORDERLY CONDUCT"|
    newtrain$newCategory=="TREA"|
    newtrain$newCategory=="TRESPASS")]<-"SOCIAL SECURE"

newtrain$newCategory[which(
  newtrain$newCategory=="PORNOGRAPHY/OBSCENE MAT"|
    newtrain$newCategory=="PROSTITUTION"|
    newtrain$newCategory=="SEX OFFENSES FORCIBLE"|
    newtrain$newCategory=="SEX OFFENSES NON FORCIBLE")]<-"SEX CRIME"

newtrain$newCategory[which(
  newtrain$newCategory=="BURGLARY"|
    newtrain$newCategory=="LARCENY/THEFT"|
    newtrain$newCategory=="RECOVERED VEHICLE"|
    newtrain$newCategory=="ROBBERY"|
    newtrain$newCategory=="STOLEN PROPERTY"|
    newtrain$newCategory=="VEHICLE THEFT")]<-"THEFT"

newtrain$newCategory[which(
  newtrain$newCategory=="NON-CRIMINAL"|
    newtrain$newCategory=="OTHER OFFENSES"|
    newtrain$newCategory=="SUICIDE"|
    newtrain$newCategory=="WARRANTS")]<-"OTHER"

newtrain$newCategory[which(
  newtrain$newCategory=="MISSING PERSON"|
    newtrain$newCategory=="RUNAWAY"|
    newtrain$newCategory=="LOITERING")]<-"PERSON"

###fit models
#-seven categories-#
library(nnet)
options(contrast=c("contr.treatment","contr.poly"))
fit=multinom(newCategory~DayOfWeek+PdDistrict,data=newtrain)
fitxy=multinom(newCategory~DayOfWeek+X+Y, data=newtrain)
fityear=multinom(newCategory~year+PdDistrict,data=newtrain)
fithour=multinom(newCategory~Hour+PdDistrict,data=newtrain)
fitfourtime=multinom(newCategory~fourtime+PdDistrict,data=newtrain)
#-top 10-#
#-data-#
od<-summary(newtrain$Category)[order(summary(newtrain$Category),decreasing=T)]
x<-vector("list",10)
for(j in 1:10){
  x[[j]]<-subset(newtrain,newtrain$Category==names(od[j]))
}
newdata=rbind(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]],x[[8]],x[[9]],x[[10]])
newdata$Category=paste0(newdata$Category)
top10_fourtime=multinom(Category~fourtime+PdDistrict,data=newdata)
top10_year=multinom(Category~year+PdDistrict,data=newdata)
top10_hour=multinom(Category~Hour+PdDistrict,data=newdata)
top10_dow=multinom(Category~DayOfWeek+PdDistrict,data=newdata)
#-top 10-#
top10_ydp=multinom(Category~year+DayOfWeek+PdDistrict,data=newdata)
top10_dyfp=multinom(Category~DayOfWeek+year+fourtime+PdDistrict,data=newdata)
top10_hyp=multinom(Category~Hour+year+PdDistrict, data=newdata)
top10_yfp=multinom(Category~year+fourtime+PdDistrict,data=newdata)

###model predictions
newtrain$newCategory<-as.factor(newtrain$newCategory)
newpred<-function(m,r){
  t2014.1<-predict(m,type="c")
  t2014.2<-0
  t2014.3<-as.numeric(t2014.1)-as.numeric(r)
  t2014.4<-cbind(t2014.3,t2014.2)
  t2014.4[which(t2014.4[,1]==0),2]<-1
  return(sum(t2014.4[,2])/nrow(t2014.4))}
#-seven categories-#
newpred(fit, newtrain$newCategory)
newpred(fitxy, newtrain$newCategory)  
newpred(fityear, newtrain$newCategory)
newpred(fithour, newtrain$newCategory)
newpred(fitfourtime,newtrain$newCategory)
#-top 10-#
newdata$Category=as.factor(paste0(newdata$Category))
newpred(top10_fourtime, newdata$Category)
newpred(top10_year, newdata$Category)
newpred(top10_hour, newdata$Category)
newpred(top10_dow, newdata$Category)
#-top 10-#
newpred(top10_ydp, newdata$Category)
newpred(top10_dyfp, newdata$Category)
newpred(top10_hyp, newdata$Category)
newpred(top10_yfp, newdata$Category)

###Cross Validation
#fitfourtime=multinom(newCategory~fourtime+PdDistrict,data=newtrain)
train_control=trainControl(method="cv", number=10)
model1=train(newCategory~fourtime+PdDistrict, data=newtrain,
            trControl=train_control, method="multinom")
prmodel1=predict(model1,newtrain)
c1=confusionMatrix(prmodel1, newtrain$newCategory) 
#choose the final model
best=tolerance(model1$results, metric = "Accuracy",tol = 1.5, maximize = TRUE)
model1$results[best,]

###visualization
newtrain$fourtime=as.factor(newtrain$fourtime)
library(effects)
fourtime=factor(fourtime, levels=c("twilight","morning","afternoon","night"))
plot(effect("fourtime*PdDistrict",fitfourtime))
plot(allEffects(fitfourtime),ask=FALSE)

DDframe=data.frame(fourtime=rep(c("twilight","morning","afternoon","night"),each=219495),
                   PdDistrict=rep(c("BAYVIEW","INGLESIDE","CENTRAL","NORTHERN","MISSION",
                                    "RICHMOND","SOUTHERN","PARK","TENDERLOIN","TARAVAL"),87798))
PredDD=cbind(DDframe,predict(fitfourtime,newdata=DDframe,type="probs",se=TRUE))
library(reshape2)
library(ggplot2)
MeltDD=melt(PredDD,id.vars=c("fourtime","PdDistrict"),value.name="Probability")
MeltDD$fourtime=factor(MeltDD$fourtime,levels=c("twilight","morning","afternoon","night"))
dw_fp=ggplot(MeltDD,aes(x=fourtime,y=Probability,color=PdDistrict,group=PdDistrict)) +
  geom_line() +
  geom_point() +
  facet_grid(variable~., scales="free") +
  labs(title="fourtime vs PdDistrict in each newCategory") +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        axis.text = element_text(size=14, color="navy"),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12))
dw_fp
ggsave("fourtime vs PdDistrict in each newCategory.png",dw_fp,path="/Users/macintosh/Desktop")

#--------------#
#top10_fourtime=multinom(Category~fourtime+PdDistrict,data=newdata)
train_control=trainControl(method="cv", number=10)
model2=train(Category~fourtime+PdDistrict, data=newdata,
            trControl=train_control, method="multinom")
prmodel2=predict(model2,newdata)
c2=confusionMatrix(prmodel2, newdata$Category) 
#choose the final model
best=tolerance(model2$results, metric = "Accuracy",tol = 1.5, maximize = TRUE)
model2$results[best,]

###
train_control=trainControl(method="cv", number=10)
model.hour=train(Category~Hour+PdDistrict, data=newdata,
             trControl=train_control, method="multinom")
prmodel.hour=predict(model.hour,newdata)
c.hour=confusionMatrix(prmodel.hour, newdata$Category) 
###
train_control=trainControl(method="cv", number=10)
model.year=train(Category~year+PdDistrict, data=newdata,
                 trControl=train_control, method="multinom")
prmodel.year=predict(model.year,newdata)
c.year=confusionMatrix(prmodel.hour, newdata$Category) 
###

###visualization
DDframe=data.frame(fourtime=rep(c("twilight","morning","afternoon","night"),each=219495),
                   PdDistrict=rep(c("BAYVIEW","INGLESIDE","CENTRAL","NORTHERN","MISSION","RICHMOND","SOUTHERN","PARK","TENDERLOIN","TARAVAL"),87798))
PredDD=cbind(DDframe,predict(top10_fourtime,newdata=DDframe,type="probs",se=TRUE))
MeltDD=melt(PredDD,id.vars=c("fourtime","PdDistrict"),value.name="Probability")
MeltDD$fourtime=factor(MeltDD$fourtime,levels=c("twilight","morning","afternoon","night"))
tpf=ggplot(MeltDD,aes(x=fourtime,y=Probability,color=PdDistrict,group=PdDistrict)) +
  geom_line() +
  geom_point() +
  facet_grid(variable~., scales="free") +
  labs(title="fourtime vs PdDistrict in each Category") +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        axis.text = element_text(size=14, color="navy"),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12))
tpf
ggsave("fourtime vs PdDistrict in each Category.png",tpf,path="/Users/macintosh/Desktop")

#top10_yfp=multinom(Category~year+fourtime+PdDistrict,data=newdata)
train_control=trainControl(method="cv", number=10)
model4=train(Category~year+fourtime+PdDistrict, data=newdata,
            trControl=train_control, method="multinom")
prmodel4=predict(model4,newdata)
c4=confusionMatrix(prmodel4, newdata$Category) 
#choose the final model?
best=tolerance(model4$results, metric = "Accuracy",tol = 1.5, maximize = TRUE)
model4$results[best,]

###parallel set
parallelset <- function(..., freq, col="gray", border=0, layer, 
                        alpha=0.5, gap.width=0.05) {
  p <- data.frame(..., freq, col, border, alpha, stringsAsFactors=FALSE)
  n <- nrow(p)
  if(missing(layer)) { layer <- 1:n }
  p$layer <- layer
  np <- ncol(p) - 5
  d <- p[ , 1:np, drop=FALSE]
  p <- p[ , -c(1:np), drop=FALSE]
  p$freq <- with(p, freq/sum(freq))
  col <- col2rgb(p$col, alpha=TRUE)
  if(!identical(alpha, FALSE)) { col["alpha", ] <- p$alpha*256 }
  p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x), maxColorValue = 256)))
  getp <- function(i, d, f, w=gap.width) {
    a <- c(i, (1:ncol(d))[-i])
    o <- do.call(order, d[a])
    x <- c(0, cumsum(f[o])) * (1-w)
    x <- cbind(x[-length(x)], x[-1])
    gap <- cumsum( c(0L, diff(as.numeric(d[o,i])) != 0) )
    gap <- gap / max(gap) * w
    (x + gap)[order(o),]
  }
  dd <- lapply(seq_along(d), getp, d=d, f=p$freq)
  par(mar = c(0, 0, 2, 0) + 0.1, xpd=TRUE )
  plot(NULL, type="n",xlim=c(0, 1), ylim=c(np, 1),
       xaxt="n", yaxt="n", xaxs="i", yaxs="i", xlab='', ylab='', frame=FALSE)
  for(i in rev(order(p$layer)) ) {
    for(j in 1:(np-1) )
      polygon(c(dd[[j]][i,], rev(dd[[j+1]][i,])), c(j, j, j+1, j+1),
              col=p$col[i], border=p$border[i])
  }
  text(0, seq_along(dd), labels=names(d), adj=c(0,-2), font=2)
  for(j in seq_along(dd)) {
    ax <- lapply(split(dd[[j]], d[,j]), range)
    for(k in seq_along(ax)) {
      lines(ax[[k]], c(j, j))
      text(ax[[k]][1], j, labels=names(ax)[k], adj=c(0, -0.25))
    }
  }           
}

###top 2 crime vs fourtime vs "SOUTHERN","MISSION","CENTRAL"
pstrain=newtrain
pstrain$ps_Category=pstrain$Category
pstrain$ps_Category=paste0(pstrain$ps_Category)
pstrain$ps_Category[which(pstrain$ps_Category!="LARCENY/THEFT"&
                            pstrain$ps_Category!="OTHER OFFENSES")] <- "MINOR"
pstrain$ps_PdDistrict=pstrain$PdDistrict
pstrain$ps_PdDistrict=paste0(pstrain$ps_PdDistrict)
pstrain$ps_PdDistrict[which(pstrain$ps_PdDistrict!="SOUTHERN"&
                              pstrain$ps_PdDistrict!="MISSION"&
                              pstrain$ps_PdDistrict!="CENTRAL")] <- "OTHER"
dta=pstrain[which(pstrain$ps_Category=="LARCENY/THEFT"|
                    pstrain$ps_Category=="OTHER OFFENSES"),]
pdta=dta[which(dta$ps_PdDistrict=="SOUTHERN"|dta$ps_PdDistrict=="MISSION"|
                 dta$ps_PdDistrict=="CENTRAL"),]				  
#pdta is a dataset which contains only ps_Category="LARCENY/THEFT", "OTHER OFFENSES" ,
#ps_PdDistrict="SOUTHERN", "MISSION", "CENTRAL"
pdta$fourtime=factor(pdta$fourtime, levels=c("twilight","morning","afternoon","night"))
ndta=subset(pdta, select=c("ps_Category","fourtime","ps_PdDistrict"))
ndta_table=ftable(ndta$ps_Category,ndta$fourtime,ndta$ps_PdDistrict)
parallel_data=as.data.frame(ndta_table)
myt <- within(parallel_data, {
  Top_two_Categories <- factor(parallel_data$Var1, levels=c("LARCENY/THEFT","OTHER OFFENSES"))
  Fourtime <- factor(parallel_data$Var2, levels=c("twilight","morning","afternoon","night"))
  Top_three_PdDistricts <- factor(parallel_data$Var3, levels=c("SOUTHERN","MISSION","CENTRAL"))
  color=ifelse(parallel_data$Var1=="LARCENY/THEFT","#008888","#330066")
})

with(myt, parallelset(Top_two_Categories, Fourtime, Top_three_PdDistricts, freq=Freq, col=color, alpha=.3))

###Category~DayOfWeek+year+fourtime+PdDistrict
npdta=pdta[which(pdta$year==2012|pdta$year==2013|pdta$year==2014),]
ndta=npdta[sample(nrow(npdta),10000,replace=FALSE),]
ndta=subset(ndta, select=c("ps_Category","fourtime","ps_PdDistrict","year"))
ndta_table=ftable(ndta$ps_Category,ndta$fourtime,ndta$ps_PdDistrict,ndta$year)
parallel_data=as.data.frame(ndta_table)
myt <- within(parallel_data, {
  Top_two_Categories <- factor(parallel_data$Var1, levels=c("LARCENY/THEFT","OTHER OFFENSES"))
  Year <- factor(parallel_data$Var4,  levels=c(2012, 2013, 2014))
  Fourtime <- factor(parallel_data$Var2, levels=c("twilight","morning","afternoon","night"))
  Top_three_PdDistricts <- factor(parallel_data$Var3, levels=c("SOUTHERN","MISSION","CENTRAL"))
  color=ifelse(parallel_data$Var1=="LARCENY/THEFT","#008888","#330066")
})

with(myt, parallelset(Top_two_Categories, Year, Fourtime, Top_three_PdDistricts, freq=Freq, col=color, alpha=.3))


