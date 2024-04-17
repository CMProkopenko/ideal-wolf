library(gridExtra)
library(ggplot2)
library(grid)
library(data.table)
library(lme4)
library(tidyr)
library(corrplot)

######after running dielkills-models

mod1<- light_snow 
dat = setDT(snowdata)


summary(snowdata)

summary(log(snowdata$TotalLight+1))
summary(log(snowdata$EffSpeed+1))

#define new observation
moose_data = data.frame(
  EffSpeed =seq(0.2,.5, by = .01), 
  TotalLight = mean(snowdata$TotalLight, na.rm = T),
  Species = "Moose"
  )

deer_data = data.frame(
  EffSpeed =seq(0.2,.5, by = .01), 
  TotalLight = mean(snowdata$TotalLight, na.rm = T),
  Species = "WTD"
)

elk_data = data.frame(
  EffSpeed =seq(0.2,.5, by = .01), 
  TotalLight = mean(snowdata$TotalLight, na.rm = T),
  Species = "Elk"
)


#use model to predict value of am
predmooseKills = predict(mod1, moose_data, type="response")
preddeerKills= predict(mod1, deer_data, type="response")
predelkKills= predict(mod1, elk_data, type="response")

speed_moosekill <- cbind(moose_data, predmooseKills)
speed_deerkill <- cbind(deer_data, preddeerKills)
speed_elkkill <- cbind(elk_data, predelkKills)

snowdata$Species

pred1 = ggplot() +
  geom_line(data = speed_moosekill, aes(EffSpeed, predmooseKills,linetype="Moose"), size = 1) +
  geom_line(data = speed_deerkill, aes(EffSpeed, preddeerKills,linetype="WTD"), size = 1) +
  geom_line(data = speed_elkkill, aes(EffSpeed, predelkKills,linetype="Elk"), size = 1) +
  scale_linetype_manual("Predicted", values = c("WTD" = "dashed","Elk" = "dotted","Moose" = "solid")) + 
  scale_colour_manual("Observed", values = c( "WTD" = "#440154","Elk" = "#21918c","Moose" = "#3b528b")) + 
  geom_point(data = snowdata,aes(log(EffSpeed+1),log(Kills+1),colour =factor(Species)), size = 3) +
  theme_bw() + theme(legend.position = c("none"), #legend.position = c(.14,.79), 
                     legend.key = element_blank(),
                     text = element_text(size = 20),
                     plot.title = element_text(vjust = - 10, hjust = .03)) +
  labs(y = "Kills", x = "Effective Speed (kmph)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #+ ggtitle("a.") 
pred1

png('output/figures/EffSpeedKills.png',width = 8000, height = 7000, res=1000, units="px")

pred1

dev.off()

#define new observation
moo_data = data.frame(
  EffSpeed =mean (snowdata$EffSpeed), 
  TotalLight = seq(0,.7,by = .05),
  Species = "Moose"
)

dee_data = data.frame(
  EffSpeed =mean (snowdata$EffSpeed), 
  TotalLight = seq(0,.7,by = .05),
  Species = "WTD"
)

el_data = data.frame(
  EffSpeed =mean (snowdata$EffSpeed), 
  TotalLight = seq(0,.7,by = .05),
  Species = "Elk"
)

predmooseKills2 = predict(mod1, moo_data, type="response")
preddeerKills2= predict(mod1, dee_data, type="response")
predelkKills2= predict(mod1, el_data, type="response")

light_moosekill <- cbind(moo_data, predmooseKills2)
light_deerkill <- cbind(dee_data, preddeerKills2)
light_elkkill <- cbind(el_data, predelkKills2)



pred2= ggplot() +
  geom_line(data = light_moosekill, aes(TotalLight, predmooseKills2,linetype="Moose"), size = 1) +
  geom_line(data = light_deerkill, aes(TotalLight, preddeerKills2,linetype="WTD"), size = 1) +
  geom_line(data = light_elkkill, aes(TotalLight, predelkKills2,linetype="Elk"), size = 1) +
  scale_linetype_manual("Predicted", values = c( "Wolf" = "dotdash","WTD" = "dashed","Elk" = "dotted","Moose" = "solid")) + 
  scale_colour_manual("Observed", values = c( "WTD" = "#440154","Elk" = "#21918c","Moose" = "#3b528b")) + 
  geom_point(data = snowdata,aes(log(TotalLight+1),log(Kills+1),colour =factor(Species)), size = 3) +
  theme_bw() + theme(legend.position = "none",
                     text = element_text(size = 20),
                     plot.title = element_text(vjust = - 10, hjust = .03)) + 
  labs(y = "Kills", x = "Daylight") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #+ ggtitle("b.") 
pred2

png('output/figures/DaylightKills.png', width = 8000, height = 7000, res=1000, units="px")

pred2

dev.off()


png('output/figures/Kills_Predict.png', width = 16000, height = 7000, res=1100, units="px")

predictions = grid.arrange(pred1,pred2, nrow = 1)

dev.off()

