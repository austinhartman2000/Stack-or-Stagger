library (readxl)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

my_data <- read_excel("CS 497 Basketball Sim/basketball-reference-data/StackOrStaggerData.xlsx")

View(my_data)

#Clean and Setup Data
my_data$Stack = my_data$`Stack/Stagger` > 0 #Boolean Stacking Variable
my_data$Arch1 = factor(my_data$Arch1)
my_data$Arch2 = factor(my_data$Arch2)

#Eliminate rows with not enough minutes to qualify
cutoff = 168 #Min number of minutes for each of the four lineups for a team to qualify
subset = my_data[my_data$ActM0 >= cutoff & my_data$ActM1 >= cutoff & my_data$ActM2 >= cutoff & my_data$ActM3 >= cutoff,]
nrow(subset)

#Estimate the minutes per game based on total minutes and minutes per game
subset$GamesPlayed = (subset$ActM0 + subset$ActM1 + subset$ActM2 + subset$ActM3)/48
subset$ActM0_PerGame = subset$ActM0 / subset$GamesPlayed
subset$ActM1_PerGame = subset$ActM1 / subset$GamesPlayed
subset$ActM2_PerGame = subset$ActM2 / subset$GamesPlayed
subset$ActM3_PerGame = subset$ActM3 / subset$GamesPlayed
subset$ActSum = subset$ActM0_PerGame + subset$ActM1_PerGame + subset$ActM2_PerGame + subset$ActM3_PerGame
subset$MPG1 = (subset$ActM1_PerGame + subset$ActM3_PerGame)
subset$MPG2 = (subset$ActM2_PerGame + subset$ActM3_PerGame)
hist(subset$ActM0_PerGame, prob = TRUE)


# #Old density code that looked at raw M0 values instead of proportions
# dx = density(subset$ActM0_PerGame, from = 0, to = 20)
# dx2 = density(subset$ActM0_PerGame, adjust=2, from = 0, to = 20)
# lines(dx)
# lines(dx2, lty='dotted')

#Function to find the max m0 possible based on minutes per game of both players
get_max_m0 = function(row){
  my_min = min(48-as.numeric(row['MPG1']), 48-as.numeric(row['MPG2']))
  return(my_min)
}

#Find proportion of M0 values compared to theoretical max value computed with above function
subset$max_m0 = apply(subset, 1, get_max_m0)
subset$ActM0_Prop = subset$ActM0_PerGame / subset$max_m0

#Find the density of m0 proportions to use for prior later
hist(subset$ActM0_Prop, prob = TRUE)
dx = density(subset$ActM0_Prop, from = 0, to = 20)
dx2 = density(subset$ActM0_Prop, adjust=2, from = 0, to = 20)
lines(dx)
lines(dx2, lty="dotted")

#Potentially explore less diverse clusters for larger sample sizes
newArchs = function(row, num){
  if(num==1){
    arch = row['Arch1']
  }else{
    arch = row['Arch2']
  }
  if (arch %in% c("Post Scorer", "Roll + Cut Big", "Versatile Big", "Stretch Big")){
    return("Big")
  }
  else if (arch %in% c("Off Screen Shooter", "Movement Shooter", "Stationary Shooter", "Athletic Finisher")){
    return("Off Ball")
  }
  else{
    return("On Ball")
  }
}

subset$NewArch1 = apply(subset, 1, newArchs, num = 1)
subset$NewArch2 = apply(subset, 1, newArchs, num = 2)

get_double_Arch = function(row, type){
  if (type == 'Old'){
    arch_sort = sort(c(row['Arch1'], row['Arch2']))
    return(paste(arch_sort[1], arch_sort[2], sep = " "))
  }
  else{
    arch_sort = sort(c(row['NewArch1'], row['NewArch2']))
    return(paste(arch_sort[1], arch_sort[2], sep = " "))
  }
}

subset$Double_Arch = apply(subset, 1, get_double_Arch, type = 'Old')
subset$NewDouble_Arch = apply(subset, 1, get_double_Arch, type = 'New')

#Prior function for the density of M0 proportion
my_prior = function(dx, M0){
  if(is.na(approx(dx$x, dx$y, M0)$y)){
    return(-10)
  }
  else{
    return(log10(approx(dx$x, dx$y, M0)$y))
  }
}

#Function to maximize with net rating and prior
my_function = function(par, row, dx){
  return(with(row, -(par[1]*PM0 + (48 - MPG2 - par[1])*PM1 + (48 - MPG1 - par[1])*PM2 + (MPG1 + MPG2 - 48 + par[1])*PM3)/48 - my_prior(dx, par[1]/max_m0)))
}

#Optimizes the above function
get_optimal = function(row){
  my_min = min(48-as.numeric(row['MPG1']), 48-as.numeric(row['MPG2']))
  optim_output = optim(par = c(my_min/2), fn = my_function, dx = dx2, row = row, lower = c(0), upper = c(my_min), method="L-BFGS-B")
  m0 = optim_output$par
  m1 = 48 - as.numeric(row['MPG2']) - m0
  m2 = 48 - as.numeric(row['MPG1']) - m0
  m3 = as.numeric(row['MPG2']) + as.numeric(row['MPG1']) - 48 + m0
  return(c(m0,m1,m2,m3))
}

#Get all the optimized suggestions for every row
subset[c('NewSugg0', 'NewSugg1', 'NewSugg2', 'NewSugg3')] = 0
for(i in 1:nrow(subset)){
  out = get_optimal(subset[i,])
  subset[i,]$NewSugg0 = out[1]
  subset[i,]$NewSugg1 = out[2]
  subset[i,]$NewSugg2 = out[3]
  subset[i,]$NewSugg3 = out[4]
}

#For comparisons, look at proportions for suggested m0s
subset$suggM0_Prop = subset$NewSugg0/subset$max_m0

subset$SuggProp = subset$NewSugg3/(subset$NewSugg3 + subset$NewSugg2 + subset$NewSugg1)
subset$DiffProp = subset$SuggProp - subset$ActualProp
subset$FinalStack = subset$suggM0_Prop > 0.5
subset$FinalCompStack = subset$DiffProp > 0

#Testing - Nees to be updated for new values
t.test(subset$`Stack/Stagger`)

t.test(subset$DiffProp)

prop.test(nrow(subset[subset$Stack == TRUE,]), nrow(subset), p = 0.5, alternative = "two.sided")

prop.test(nrow(subset[subset$FinalCompStack == TRUE,]), nrow(subset), p = 0.5, alternative = "two.sided")

subset$Diff_M0_PerGame = -(subset$ActM0_PerGame - subset$NewSugg0)
subset$Diff_M1_PerGame = -(subset$ActM1_PerGame - subset$NewSugg1)
subset$Diff_M2_PerGame = -(subset$ActM2_PerGame - subset$NewSugg2)
subset$Diff_M3_PerGame = -(subset$ActM3_PerGame - subset$NewSugg3)
anova_df = stack(subset[c('Diff_M0_PerGame','Diff_M1_PerGame','Diff_M2_PerGame','Diff_M3_PerGame')])
one.way = aov(values ~ ind, data = anova_df)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

guard_subset = subset[subset$Arch1 == subset$Arch2,]
guard_df = stack(guard_subset[c('Diff_M0_PerGame','Diff_M1_PerGame','Diff_M2_PerGame','Diff_M3_PerGame')])
one.way = aov(values ~ ind, data = guard_df)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way


lm0 = lm(subset$`Stack/Stagger` ~ subset$PM0)
summary(lm0)
plot(subset$PM0, subset$`Stack/Stagger`)
abline(lm0)

lm1 = lm(subset$`Stack/Stagger` ~ subset$PM1)
summary(lm1)
plot(subset$PM1, subset$`Stack/Stagger`)
abline(lm1)

lm2 = lm(subset$`Stack/Stagger` ~ subset$PM2)
summary(lm2)
plot(subset$PM2, subset$`Stack/Stagger`)
abline(lm2)

lm3 = lm(subset$`Stack/Stagger` ~ subset$PM3)
summary(lm3)
plot(subset$PM3, subset$`Stack/Stagger`)
abline(lm3)

lm0 = lm(subset$SuggProp ~ subset$PM0)
summary(lm0)
plot(subset$PM0, subset$SuggProp)
abline(lm0)

lm1 = lm(subset$SuggProp ~ subset$PM1)
summary(lm1)
plot(subset$PM1, subset$SuggProp)
abline(lm1)

lm2 = lm(subset$SuggProp ~ subset$PM2)
summary(lm2)
plot(subset$PM2, subset$SuggProp)
abline(lm2)

lm3 = lm(subset$SuggProp ~ subset$PM3)
summary(lm3)
plot(subset$PM3, subset$SuggProp)
abline(lm3)

lm4 = lm(SuggProp ~ PM1 +PM2 + PM3 + PM0, data=subset)
summary(lm4)
plot(subset$SuggProp ~ predict(lm4,subset))
abline(lm4)

mean.PM.data <- subset %>%
  group_by(Stack) %>%
  summarise(
    PM0 = mean(PM0),
    PM1 = mean(PM1),
    PM2 = mean(PM2),
    PM3 = mean(PM3)
  )

arch1PMMeans <- subset %>%
  group_by(Arch1) %>%
  summarise(
    PM1 = mean(PM1),
    SuggProp = mean(SuggProp)
  )

arch2PMMeans <- subset %>%
  group_by(Arch2) %>%
  summarise(
    PM2 = mean(PM2)
  )

my_plot = ggplot(subset, aes(x = Stack, y = PM0)) + geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
my_plot = my_plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.PM.data, aes(x=Stack, y=PM0))
my_plot


my_plot = ggplot(subset, aes(x = Stack, y = PM1)) + geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
my_plot = my_plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.PM.data, aes(x=Stack, y=PM1))
my_plot


my_plot = ggplot(subset, aes(x = Stack, y = PM2)) + geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
my_plot = my_plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.PM.data, aes(x=Stack, y=PM2))
my_plot


my_plot = ggplot(subset, aes(x = Stack, y = PM3)) + geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
my_plot = my_plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.PM.data, aes(x=Stack, y=PM3))
my_plot

my_plot = ggplot(subset, aes(x = Arch1, y = PM1)) + geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
my_plot = my_plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=arch1PMMeans, aes(x=Arch1, y=PM1))
my_plot

anova_arch1 = aov(PM1 ~ NewArch1, data = subset)
summary(anova_arch1)
tukey.arch1<-TukeyHSD(anova_arch1)
tukey.arch1

my_plot = ggplot(subset, aes(x = Arch2, y = PM2)) + geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
my_plot = my_plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=arch2PMMeans, aes(x=Arch2, y=PM2))
my_plot

















anova_arch2 = aov(`Stack/Stagger` ~ Double_Arch, data = subset)
summary(anova_arch2)
tukey.arch2<-TukeyHSD(anova_arch2)
tukey.arch2

anova_arch2 = aov(`Stack/Stagger` ~ NewDouble_Arch, data = subset)
summary(anova_arch2)
tukey.arch2<-TukeyHSD(anova_arch2)
tukey.arch2

anova_mix = aov()

anova_arch2 = aov(PM2 ~ NewArch2, data = subset)
summary(anova_arch2)
tukey.arch2<-TukeyHSD(anova_arch2)
tukey.arch2

anova_mix = aov(PM3 ~ Arch1 * Arch2, data = subset)
summary(anova_mix)
tukey.mix<-TukeyHSD(anova_mix)
tukey.mix

anova_mix = aov(ActualProp ~ Arch1 * Arch2, data = subset)
summary(anova_mix)

anova_mix = aov(SuggProp ~ Arch1 * Arch2, data = subset)
summary(anova_mix)

anova_mix = aov(DiffProp ~ Arch1 * Arch2, data = subset)
summary(anova_mix)

plot(PM1 ~ Lebron1, subset)
plot(PM2 ~ Lebron2, subset)

my_plot = ggplot(subset, aes(x = Arch1, y = SuggProp)) + geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
my_plot = my_plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=arch1PMMeans, aes(x=Arch1, y=SuggProp))
my_plot

library(aod)


View(subset[c('Arch1', 'Lebron1', 'Arch2', 'Lebron2', 'Stack')][with(subset, order(Arch1, Arch2)),])

mylogit = glm(Stack ~ Lebron1 + Lebron2 + Lebron1:Arch1 + Lebron2:Arch2, data = subset, family = "binomial")
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))