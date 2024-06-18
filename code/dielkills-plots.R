library(gridExtra)
library(ggplot2)
library(grid)
library(data.table)
library(lme4)
library(tidyr)
library(corrplot)
library(scales)
library(viridis)



###my data 
data <- fread("data/2021-09-19-Bloc-DielModel.csv")

summary(as.factor(data$PreySpecie))

###all data average
alldata <- subset(data, PreySpecie=="AllWEM" & Period=="ALLMON")

###subset by prey
 elkdata <- subset(data, PreySpecie=="Elk" & Period=="ALLMON")
 moosedata <- subset(data, PreySpecie=="Moose" & Period=="ALLMON")
 wtddata <- subset(data, PreySpecie=="WTD" & Period=="ALLMON")
# 

# ###subset by seasons, all prey
snowdata <- subset(data, PreySpecie=="AllWEM" & Period=="ALLMON")
freedata <- subset(data, PreySpecie=="AllWEM" & Period=="ALLMON")

yeardata <- subset(data, PreySpecie=="Moose"& Period=="ALLMON" | PreySpecie=="Elk"& Period=="ALLMON" | PreySpecie=="WTD" & Period=="ALLMON")
yeardata$Species = as.factor(yeardata$PreySpecie)
yeardata$Species = relevel(as.factor(yeardata$Species), ref = "WTD")
 
###subset by seasons

summary(as.factor(data$Period))

snowdata <- subset(data, PreySpecie=="Moose"& Period=="SNOW_11_04" | PreySpecie=="Elk" & Period=="SNOW_11_04"| PreySpecie=="WTD" & Period=="SNOW_11_04")
snowdata$Species = as.factor(snowdata$PreySpecie)
snowdata$Species = relevel(as.factor(snowdata$Species), ref = "WTD")

freedata <- subset(data, PreySpecie=="Moose" & Period=="FREE_05_10"| PreySpecie=="Elk" & Period=="FREE_05_10"| PreySpecie=="WTD" & Period=="FREE_05_10")
freedata$Species = as.factor(freedata$PreySpecie)
freedata$Species  = relevel(as.factor(freedata$Species), ref = "WTD")

###### season, prey
snowmoose <- subset(snowdata, PreySpecie=="Moose")
snowdeer <- subset(snowdata, PreySpecie=="WTD")
snowelk <- subset(snowdata, PreySpecie=="Elk")

summary(snowmoose$PreySpeed)
summary(snowdeer$PreySpeed)
summary(snowelk$PreySpeed)

freemoose <- subset(freedata, PreySpecie=="Moose")
freedeer <- subset(freedata, PreySpecie=="WTD")
freeelk <- subset(freedata, PreySpecie=="Elk")

summary(freemoose$PreySpeed)
summary(freedeer$PreySpeed)
summary(freeelk$PreySpeed)

###Figure 1  main text SNOW MODEL 

#coef2=max(snowdata$CrepLight)/max(snowdata$TotalLight)

snowlight = ggplot() + #ggtitle("c.") +
 # geom_line(data = snowmoose,aes(x=Hour, y=TotalLight, linetype = "Sunlight")) +
  #geom_line(data = snowmoose,aes(x=Hour, y=CrepLight, linetype = "Crepuscular")) +
  geom_area(data = snowmoose,aes(x=Hour, y=TotalLight), fill = "#FDE725FF", alpha = .3) +
  geom_area(data = snowmoose,aes(x=Hour, y=CrepLight), fill = "#440154FF", alpha = .5) +
#  scale_y_continuous(name = expression("Crepuscular Light"),sec.axis = sec_axis(~./coef2, name = "Light Availability")) +
  theme_bw() + theme(text = element_text(size=20)) +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(2,20,10,15),units="points"),
        axis.title.y = element_text(vjust =1),
        plot.title = element_text(vjust = - 10, hjust = .03)) +
  scale_linetype_manual("", values = c("Sunlight" = "dotted","Crepuscular"="dashed" )) + 
  labs(x = "Time of Day (h)", y = "Light Availability") +
  theme(legend.position="none")+ 
  scale_y_continuous(labels = label_number(accuracy = 0.1))
snowlight

snoweffspeed = ggplot() + #ggtitle("b.") +
  geom_line(data = snowmoose, aes(Hour,EffSpeed))+
  labs(x = "Time of Day (h)", y = " Effective Speed (kmph)")+
  theme_bw()+ theme(text = element_text(size=20)) +theme(legend.position="none")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(2,20,-25,15),units="points"),
        axis.title.y = element_text(vjust =1),
        plot.title = element_text(vjust = - 10, hjust = .03)) 
snoweffspeed 

snowkill = ggplot() + #ggtitle("a.") +
  geom_bar(data= snowdata, aes(Hour,Kills, fill=as.factor(Species)),stat="identity") + 
  theme_bw()+ theme(text = element_text(size=20)) + theme(text = element_text(size=20)) + theme(legend.title=element_blank())+ theme(legend.position="none")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(plot.margin = unit(c(2,20,-25,19),units="points"),
        axis.title.y = element_text(vjust =1),
        plot.title = element_text(vjust = - 10, hjust = .03)) +
  scale_fill_manual(values=c( "#440154","#21918c","#3b528b")) +
  scale_colour_manual(values=c(  "#440154","#21918c","#3b528b")) +
  #scale_fill_manual("legend", values = c( "WTD" = "lightgray","Elk" = "darkgray","Moose" = "black")) + 
  theme(legend.position=c(0.9,0.7)) 
snowkill


png('output/figures/SnowMod.png', width = 8000, height = 10000, res=1000, units="px")

snowplot = grid.arrange(snowkill,snoweffspeed,snowlight)

dev.off()


#####Figure 2 Snow speed plot

snowspeed = ggplot()+
  geom_point(data = snowmoose, aes(Hour,EffSpeed, colour="Moose")) +
  geom_point(data = snowdeer, aes(Hour,EffSpeed, colour="WTD")) +
  geom_point(data = snowelk, aes(Hour,EffSpeed, colour="Elk")) +
  geom_line(data = snowelk, aes(Hour,WolfSpeed,linetype="Wolf")) +
  geom_line(data= snowdeer,aes(Hour,PreySpeed, linetype="WTD")) +
  geom_line(data= snowmoose,aes(Hour,PreySpeed, linetype="Moose")) +
  geom_line(data= snowelk,aes(Hour,PreySpeed, linetype="Elk")) +
  labs(x = "Time of Day (h)", y = "Speed (kmph)")+
  scale_linetype_manual("Species", values = c( "Wolf" = "dotdash","WTD" = "dashed","Elk" = "dotted","Moose" = "solid")) + 
  scale_colour_manual("Effective Speed", values = c("WTD" = "#440154","Elk" = "#21918c","Moose" = "#3b528b")) + 
  theme_bw()+theme(legend.position="none",) + theme(text = element_text(size=20)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(10,10,10,10),units="points"))
snowspeed

png('output/figures/Speeds_snow.png', width = 8000, height = 7000, res=1000, units="px")

snowspeed

dev.off()

####snowfree of Figure 1 for appendix

freelight = ggplot()  + ggtitle("c.") +
  geom_line(data = freemoose,aes(x=Hour, y=CrepLight, linetype = "Crepuscular")) +
  geom_line(data = freemoose,aes(x=Hour, y=TotalLight, linetype = "Sunlight")) +
  #  scale_y_continuous(name = expression("Crepuscular Light"),sec.axis = sec_axis(~./coef2, name = "Light Availability")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(2,20,10,15),units="points"),
        axis.title.y = element_text(vjust =1),
        plot.title = element_text(vjust = - 10, hjust = .03)) +
  scale_linetype_manual("", values = c("Sunlight" = "dotted","Crepuscular"="dashed" )) + 
  labs(x = "Hour", y = "Light Availability") +
  theme(legend.position=c(0.9, 0.7))+ 
  scale_y_continuous(labels = label_number(accuracy = 0.1))
freelight

freeeffspeed = ggplot() + ggtitle("b.") +
  geom_line(data = freemoose, aes(Hour,EffSpeed))+
  labs(x = "Hour", y = " Effective Speed (kmph)")+
  theme_bw()+theme(legend.position="none")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(2,20,-25,15),units="points"),
        axis.title.y = element_text(vjust =1),
        plot.title = element_text(vjust = - 10, hjust = .03)) 
freeeffspeed 

freekill = ggplot() + ggtitle("a.") +
  geom_bar(data= freedata, aes(Hour,Kills, fill=as.factor(PreySpecie)),stat="identity") + 
  theme_bw()+theme(legend.title=element_blank())+ theme(legend.position="none")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(plot.margin = unit(c(2,20,-25,22),units="points"), axis.title.y = element_text(vjust =1),
        plot.title = element_text(vjust = - 10, hjust = .03)) +
  scale_fill_manual("legend", values = c( "WTD" = "#440154","Elk" = "#21918c","Moose" = "#3b528b")) + 
  theme(legend.position=c(0.1,0.8)) 
freekill

png('output/figures/FreeMod.png', width = 8000, height = 10000, res=1000, units="px")

freeplot = grid.arrange(freekill,freeeffspeed,freelight)

dev.off()

#####speed plot

snowspeed = ggplot()+
  geom_point(data = snowmoose, aes(Hour,EffSpeed, colour="Moose")) +
  geom_point(data = snowdeer, aes(Hour,EffSpeed, colour="WTD")) +
  geom_point(data = snowelk, aes(Hour,EffSpeed, colour="Elk")) +
  geom_line(data = snowelk, aes(Hour,WolfSpeed,linetype="Wolf")) +
  geom_line(data= snowdeer,aes(Hour,PreySpeed, linetype="WTD")) +
  geom_line(data= snowmoose,aes(Hour,PreySpeed, linetype="Moose")) +
  geom_line(data= snowelk,aes(Hour,PreySpeed, linetype="Elk")) +
  labs(x = "Hour", y = "Speed (kmph)")+
  scale_linetype_manual("Species", values = c( "Wolf" = "dotdash","WTD" = "dashed","Elk" = "dotted","Moose" = "solid")) + 
  scale_colour_manual("Effective Speed", values = c("WTD" = "#440154","Elk" = "#21918c","Moose" = "#3b528b")) + 
  theme_bw()+theme(legend.position="right",) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(10,10,10,10),units="points"))
snowspeed

png('output/figures/Speeds_snow.png', width = 8000, height = 7000, res=1000, units="px")

snowspeed

dev.off()

#####Year Models
