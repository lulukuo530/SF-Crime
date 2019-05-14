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

#parallel set
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

#top 2 crime vs fourtime vs "SOUTHERN","MISSION","CENTRAL"
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


#Category~DayOfWeek+year+fourtime+PdDistrict
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
