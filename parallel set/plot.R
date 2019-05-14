setwd("~/")
getwd()
png("/Users/macintosh/Desktop/fourtime vs PdDistrict in each newCategory.png",width=1200,height=900)
# draw plot #
dev.off()

#-----#
setwd("~/")
getwd()
dw_fp=ggplot(MeltDD,aes(x=fourtime,y=Probability,color=PdDistrict,group=PdDistrict)) +
  geom_line() +
  geom_point() +
  facet_grid(variable~., scales="free") +
  labs(title="fourtime vs PdDistrict in each newCategory") +
  theme(plot.title = element_text(size=18, face="bold.italic"),
        axis.text = element_text(size=12, color="navy"),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12))
dw_fp
png("fourtime vs PdDistrict in each newCategory.png")
dev.off()

#----#
DDframe=data.frame(fourtime=rep(c("twilight","morning","afternoon","night"),each=219495),
                   PdDistrict=rep(c("BAYVIEW","INGLESIDE","CENTRAL","NORTHERN","MISSION",
                                    "RICHMOND","SOUTHERN","PARK","TENDERLOIN","TARAVAL"),87798))
PredDD=cbind(DDframe,predict(fitfourtime,newdata=DDframe,type="probs",se=TRUE))
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
ggsave("fourtime vs PdDistrict in each newCategory.png",dw_fp,path="/Users/macintosh/Desktop")

###
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
        axis.text = element_text(size=10, color="navy"),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12))
ggsave("fourtime vs PdDistrict in each Category.png",tpf,path="/Users/macintosh/Desktop")



