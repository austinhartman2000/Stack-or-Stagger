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
cutoff = 192 #Min number of minutes for each of the four lineups for a team to qualify
subset = my_data[my_data$ActM0 >= cutoff & my_data$ActM1 >= cutoff & my_data$ActM2 >= cutoff & my_data$ActM3 >= cutoff,]
nrow(subset)/nrow(my_data)

#Estimate the minutes per game based on total minutes and minutes per game
subset$GamesPlayed = (subset$ActM0 + subset$ActM1 + subset$ActM2 + subset$ActM3)/48
subset$ActM0_PerGame = subset$ActM0 / subset$GamesPlayed
subset$ActM1_PerGame = subset$ActM1 / subset$GamesPlayed
subset$ActM2_PerGame = subset$ActM2 / subset$GamesPlayed
subset$ActM3_PerGame = subset$ActM3 / subset$GamesPlayed
#Compute the minutes per game for each player and view a histogram of M0
subset$ActSum = subset$ActM0_PerGame + subset$ActM1_PerGame + subset$ActM2_PerGame + subset$ActM3_PerGame
subset$MPG1 = (subset$ActM1_PerGame + subset$ActM3_PerGame)
subset$MPG2 = (subset$ActM2_PerGame + subset$ActM3_PerGame)
hist(subset$ActM0_PerGame, prob = TRUE)


#Function to find the max m0 possible based on minutes per game of both players
get_max_m0 = function(row){
  my_min = min(48-as.numeric(row['MPG1']), 48-as.numeric(row['MPG2']))
  return(round(my_min,2))
}

#Find proportion of M0 values compared to theoretical max value computed with above function
subset$max_m0 = apply(subset, 1, get_max_m0)
subset$ActM0_Prop = subset$ActM0_PerGame / subset$max_m0

#Find the density of m0 proportions to use for prior later
hist(subset$ActM0_Prop, prob = TRUE, main = "Lambda = Density Distribution of M1*", xlab = "M1*")
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
  if (arch %in% c("Post Scorer", "Versatile Big")){
    return("On_Ball_Big")
  }
  else if (arch %in% c("Roll + Cut Big", "Stretch Big")){
    return("Off_Ball_Big")
  }
  else if (arch %in% c("Off Screen Shooter", "Movement Shooter", "Stationary Shooter", "Athletic Finisher")){
    return("Off Ball")
  }
  else{
    return("On Ball") #This was inititally on ball
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
  my_max = min(48-as.numeric(row['MPG1']), 48-as.numeric(row['MPG2']))
  my_min = max(48 - as.numeric(row['MPG2']) - as.numeric(row['MPG1']), 0)
  optim_output = optim(par = c(my_min/2), fn = my_function, dx = dx2, row = row, lower = c(my_min), upper = c(my_max), method="L-BFGS-B")
  m0 = round(optim_output$par,2)
  m1 = round(48 - as.numeric(row['MPG2']) - m0, 2)
  m2 = round(48 - as.numeric(row['MPG1']) - m0,2)
  m3 = round(as.numeric(row['MPG2']) + as.numeric(row['MPG1']) - 48 + m0,2)
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

#Optimizes the above function
get_stack = function(row){
  my_max = min(48-as.numeric(row['MPG1']), 48-as.numeric(row['MPG2']))
  my_min = max(48 - as.numeric(row['MPG2']) - as.numeric(row['MPG1']), 0)
  m0 = round(my_max,2)
  m1 = round(48 - as.numeric(row['MPG2']) - m0, 2)
  m2 = round(48 - as.numeric(row['MPG1']) - m0,2)
  m3 = round(as.numeric(row['MPG2']) + as.numeric(row['MPG1']) - 48 + m0,2)
  return(c(m0,m1,m2,m3))
}

#Get all the optimized suggestions for every row
subset[c('StackSugg0', 'StackSugg1', 'StackSugg2', 'StackSugg3')] = 0
for(i in 1:nrow(subset)){
  out = get_stack(subset[i,])
  subset[i,]$StackSugg0 = out[1]
  subset[i,]$StackSugg1 = out[2]
  subset[i,]$StackSugg2 = out[3]
  subset[i,]$StackSugg3 = out[4]
}

#Optimizes the above function
get_Stagger = function(row){
  my_max = min(48-as.numeric(row['MPG1']), 48-as.numeric(row['MPG2']))
  my_min = max(48 - as.numeric(row['MPG2']) - as.numeric(row['MPG1']), 0)
  m0 = round(my_min,2)
  m1 = round(48 - as.numeric(row['MPG2']) - m0, 2)
  m2 = round(48 - as.numeric(row['MPG1']) - m0,2)
  m3 = round(as.numeric(row['MPG2']) + as.numeric(row['MPG1']) - 48 + m0,2)
  return(c(m0,m1,m2,m3))
}

#Get all the optimized suggestions for every row
subset[c('StaggerSugg0', 'StaggerSugg1', 'StaggerSugg2', 'StaggerSugg3')] = 0
for(i in 1:nrow(subset)){
  out = get_Stagger(subset[i,])
  subset[i,]$StaggerSugg0 = out[1]
  subset[i,]$StaggerSugg1 = out[2]
  subset[i,]$StaggerSugg2 = out[3]
  subset[i,]$StaggerSugg3 = out[4]
}

#For comparisons, look at proportions for suggested m0s
subset$suggM0_Prop = subset$NewSugg0/subset$max_m0

subset$SuggProp = subset$NewSugg3/(subset$NewSugg3 + subset$NewSugg2 + subset$NewSugg1)
subset$DiffProp = subset$SuggProp - subset$ActualProp
subset$FinalStack = subset$suggM0_Prop > 0.5
subset$FinalCompStack = subset$DiffProp > 0

subset$RealGameScore = (subset$PM0*subset$ActM0_PerGame + subset$PM1*subset$ActM1_PerGame + subset$PM2*subset$ActM2_PerGame + subset$PM3*subset$ActM3_PerGame)/48

subset$SuggGameScore = (subset$PM0*subset$SuggM0 + subset$PM1*subset$suggM1 + subset$PM2*subset$SuggM2 + subset$PM3*subset$SuggM3)/48
subset$GameImprovement = subset$SuggGameScore - subset$RealGameScore
t.test(subset$GameImprovement)

subset$StackGameScore = (subset$PM0*subset$StackSugg0 + subset$PM1*subset$StackSugg1 + subset$PM2*subset$StackSugg2 + subset$PM3*subset$StackSugg3)/48
subset$StackImprovement = subset$StackGameScore - subset$RealGameScore
t.test(subset$StackImprovement)

subset$StaggerGameScore = (subset$PM0*subset$StaggerSugg0 + subset$PM1*subset$StaggerSugg1 + subset$PM2*subset$StaggerSugg2 + subset$PM3*subset$StaggerSugg3)/48
subset$StaggerImprovement = subset$StaggerGameScore - subset$RealGameScore
t.test(subset$StaggerImprovement)







#Testing
t.test(subset$`Stack/Stagger`)

t.test(subset$DiffProp)

prop.test(nrow(subset[subset$Stack == TRUE,]), nrow(subset), p = 0.5, alternative = "two.sided")

lm0 = lm(subset$`Stack/Stagger` ~ subset$PM0)
summary(lm0)
plot(subset$PM0, subset$`Stack/Stagger`, main = "", xlab = "N1", ylab = "alpha", cex.lab = 2)
abline(lm0)

lm1 = lm(subset$`Stack/Stagger` ~ subset$PM1)
summary(lm1)
plot(subset$PM1, subset$`Stack/Stagger`, main = "", xlab = "N2", ylab = "alpha", cex.lab = 2)
abline(lm1)

lm2 = lm(subset$`Stack/Stagger` ~ subset$PM2)
summary(lm2)
plot(subset$PM2, subset$`Stack/Stagger`, main = "", xlab = "N3", ylab = "alpha", cex.lab = 2)
abline(lm2)

lm3 = lm(subset$`Stack/Stagger` ~ subset$PM3)
summary(lm3)
plot(subset$PM3, subset$`Stack/Stagger`, main = "", xlab = "N4", ylab = "alpha", cex.lab = 2)
abline(lm3)












write.csv(subset, "C:/Users/Austin Hartman/Documents/CS 497 Basketball Sim/basketball-reference-data/StackorStaggerAnovaData.csv")

anova_arch1 = aov(PM2 ~ Arch2, data = subset)
summary(anova_arch1)
tukey.arch1<-TukeyHSD(anova_arch1)
tukey.arch1

anova_arch1 = aov(PM1 ~ Arch1, data = subset)
summary(anova_arch1)
tukey.arch1<-TukeyHSD(anova_arch1)
tukey.arch1

anova_arch2 = aov(`Stack/Stagger` ~ Double_Arch, data = subset)
summary(anova_arch2)
tukey.arch2<-TukeyHSD(anova_arch2)
tukey.arch2

anova_arch2 = aov(SuggProp ~ Double_Arch, data = subset)
summary(anova_arch2)
tukey.arch2<-TukeyHSD(anova_arch2)
tukey.arch2

table = tukey.arch2$Double_Arch
table[order(table[, 'p adj']), ][1:20,]

anova_arch2 = aov(ActualProp ~ Double_Arch, data = subset)
summary(anova_arch2)
tukey.arch2<-TukeyHSD(anova_arch2)
tukey.arch2

table = tukey.arch2$Double_Arch
table[order(table[, 'p adj']), ][1:20,]



anova_arch1 = aov(PM2 ~ NewArch2, data = subset)
summary(anova_arch1)
tukey.arch1<-TukeyHSD(anova_arch1)
tukey.arch1

anova_arch1 = aov(PM1 ~ NewArch1, data = subset)
summary(anova_arch1)
tukey.arch1<-TukeyHSD(anova_arch1)
tukey.arch1

anova_arch3 = aov(`Stack/Stagger` ~ NewDouble_Arch, data = subset)
summary(anova_arch3)
tukey.arch3<-TukeyHSD(anova_arch3)
tukey.arch3

anova_arch3 = aov(SuggProp ~ NewDouble_Arch, data = subset)
summary(anova_arch3)
tukey.arch3<-TukeyHSD(anova_arch3)
tukey.arch3

table = tukey.arch3$NewDouble_Arch
table[order(table[, 'p adj']), ]

pairwise.t.test(subset$SuggProp, subset$NewDouble_Arch, p.adj = "bonf")

anova_arch4 = aov(ActualProp ~ NewDouble_Arch, data = subset)
summary(anova_arch4)
tukey.arch4<-TukeyHSD(anova_arch4)
tukey.arch4

table = tukey.arch4$Double_Arch
table[order(table[, 'p adj']), ][1:20,]