library(gridExtra)
library(ggplot2)
library(grid)
library(data.table)
library(lme4)
library(tidyr)
library(corrplot)

###my data 
data <- fread("data/2021-07-25-Bloc-DielModel.csv")

summary(as.factor(data$PreySpecie))

###three species added
# wemdata =  subset(data, PreySpecie=="AllWEM" & Period=="ALLMON")
# wemsnodata =  subset(data, PreySpecie=="AllWEM" & Period=="SNOW_11_04")
# wemfredata =  subset(data, PreySpecie=="AllWEM" & Period=="FREE_05_10")


#### all data is species seperated

yeardata <- subset(data, PreySpecie=="Moose" | PreySpecie=="Elk" | PreySpecie=="WTD" & Period=="ALLMON")
yeardata$Species = as.factor(yeardata$PreySpecie)
yeardata$Species = relevel(as.factor(yeardata$Species), ref = "WTD")


myvars <- c("EffSpeed", "WolfSpeed", "PreySpeed", "TotalLight", "CrepLight")
newdata <- yeardata[myvars]

corrplot(cor(newdata),
         method = "number",
         type = "upper")


###subset by seasons

summary(as.factor(data$Period))

snowdata <- subset(data, PreySpecie=="Moose" | PreySpecie=="Elk" | PreySpecie=="WTD" & Period=="SNOW_11_04")
snowdata$Species = as.factor(snowdata$PreySpecie)
snowdata$Species = relevel(as.factor(snowdata$Species), ref = "WTD")

freedata <- subset(data, PreySpecie=="Moose" | PreySpecie=="Elk" | PreySpecie=="WTD" & Period=="FREE_05_10")
freedata$Species = as.factor(freedata$PreySpecie)
freedata$Species  = relevel(as.factor(freedata$Species), ref = "WTD")

######## TI models ######## holling TI

effv_year = glm(log(Kills+1)~log(EffSpeed+1):Species+ log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=yeardata)
effv_snow = glm(log(Kills+1)~log(EffSpeed+1):Species + log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=snowdata)
effv_free = glm(log(Kills+1)~log(EffSpeed+1):Species + log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=freedata)

# prey_year = glm(log(Kills+1)~log(PreySpeed+1):Species+ log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=yeardata)
# prey_snow = glm(log(Kills+1)~log(PreySpeed+1):Species+ log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=snowdata)
# prey_free = glm(log(Kills+1)~log(PreySpeed+1):Species+ log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=freedata)
# 
# wolf_year = glm(log(Kills+1)~log(WolfSpeed+1):Species+ log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=yeardata)
# wolf_snow = glm(log(Kills+1)~log(WolfSpeed+1):Species+ log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=snowdata)
# wolf_free = glm(log(Kills+1)~log(WolfSpeed+1):Species+ log(TotalLight+1):Species + log(CrepLight+1):Species + Species, data=freedata)
# 
# 
light_year = glm(log(Kills+1)~log(EffSpeed+1):Species+ log(TotalLight+1):Species + Species, data=yeardata)
light_snow = glm(log(Kills+1)~log(EffSpeed+1):Species+ log(TotalLight+1):Species + Species, data=snowdata)
light_free = glm(log(Kills+1)~log(EffSpeed+1):Species+ log(TotalLight+1):Species + Species, data=freedata)
# 
# crep_year = glm(log(Kills+1)~log(EffSpeed+1):Species+ log(CrepLight+1):Species + Species, data=yeardata)
# crep_snow = glm(log(Kills+1)~log(EffSpeed+1):Species+ log(CrepLight+1):Species + Species, data=snowdata)
# crep_free = glm(log(Kills+1)~log(EffSpeed+1):Species+ log(CrepLight+1):Species + Species, data=freedata)
# 

######
summary(effv_year)
summary(effv_snow)
summary(effv_free)

# summary(prey_year)
# summary(prey_snow)
# summary(prey_free)
# 
# summary(wolf_year)
# summary(wolf_snow)
# summary(wolf_free)
# 
summary(light_year)
summary(light_snow)
summary(light_free)
# 
# summary(crep_year)
# summary(crep_snow)
# summary(crep_free)


####exporting outputs as tables
# mod_wem <- broom::tidy(mTI_wem, conf.int=TRUE)
# mod_wem
# 
# mod_wemsno <- broom::tidy(mTI_wemsno, conf.int=TRUE)
# mod_wemsno
# 
# mod_wemfre <- broom::tidy(mTI_wemfre, conf.int=TRUE)
# mod_wemfre

mod_year <- broom::tidy(effv_year, conf.int=TRUE)
mod_year

# save predictions of the model in the new data frame 
# together with variable you want to plot against
summary(mod_year)
predicted_yearmod <- data.frame(mod_pred = predict(mod_year, yeardata), Kills=yeardata$Kills)


mod_snow <- broom::tidy(effv_snow, conf.int=TRUE)
mod_snow

mod_free <- broom::tidy(effv_free, conf.int=TRUE)
mod_free


write.csv(mod_year,'output/tables/year_results.csv')
write.csv(mod_snow,'output/tables/snow_results.csv')
write.csv(mod_free,'output/tables/free_results.csv')
