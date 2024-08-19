rm(list=ls())

library(MASS)
library(dplyr)
library(AER) #for overdispersion test
library(lme4)
library(emmeans)
library(ggplot2)
library(performance)
library(lemon)

#Testing Stuff
#####
#Below is  where I build/test functionality for the variety of scripts used to analyse data

#power analysis practice
full.dataset = read.csv(file.choose()) #"full data with GPS_Rounded.csv", don't use dataset with simulated values or NAs, also I pre-rounded eDNA values as negative binomials only take integers
str(full.dataset)
full.dataset$Year = as.factor(full.dataset$Year)

#This is practice to set up the larger function below
#Set up dataframes to fill from functions
lake.df = data.frame(Lake=character(), #dataframe with lake names
                     variable=character(),
                     stringsAsFactors=FALSE)
final.sim.dataset = setNames(data.frame(matrix(ncol = 199, nrow = 0)),paste0("n.", c(2:200))) #dataframe with sample size columns
final.sim.dataset = cbind(lake.df,final.sim.dataset) #combine dataframe



lake.d = data.frame(Lake=character(), #dataframe with lake names
                    variable=character(),
                    stringsAsFactors=FALSE)
d = setNames(data.frame(matrix(ncol = 199, nrow = 0)),paste0("n.", c(2:200))) #dataframe with sample size columns
d = cbind(lake.d,d)


summary.sim.dataset = data.frame(Lake=character(), #dataframe to store summary data for each lake
                                 sample.size.upper90 = integer(),
                                 sample.size.lower90 = integer(),
                                 sample.size.upper80 = integer(),
                                 sample.size.lpper80 = integer(),
                                 average.sample.90 = integer(),
                                 average.sample.80 = integer(),
                                 Lake.CoV = integer(),
                                 stringsAsFactors=FALSE)

summary.d = data.frame(Lake=character(), #Build empty data frame to populate for each specific lake, same structure as dataframe above for easy 'rbind'ing
                          sample.size.upper90 =integer(),
                          sample.size.lower90 = integer(),
                          sample.size.upper80 = integer(),
                          sample.size.lower80 = integer(),
                          average.sample.90 = integer(),
                          average.sample.80 = integer(),
                          Lake.CoV = integer(),
                          stringsAsFactors=FALSE)


practice1 = filter(full.dataset, Lake == "5A") #run the function on just a single lake for now
#practice = filter(practice1, Year == "2021" ) #isolate the year (can change to month if needed)
practice = filter(practice1, Month == "May")

glm.nb1.identity = glm.nb(eDNA~1, data = practice, link = "identity") #run a glm just to check it out
summary(glm.nb1.identity)

resids = residuals(glm.nb1.identity)
hist(resids)
qqnorm(resids)
qqline(resids)

glm.nb1.log = glm.nb(eDNA~1, data = practice, link = "log") #run a glm just to check it out
summary(glm.nb1.log)

resids = residuals(glm.nb1.log)
hist(resids)
qqnorm(resids)
qqline(resids)


hist(practice$eDNA)

#Does not like original code ###(10000/length(practice$eDNA))### - replacing with 5000 until i find fix
x = simulate(glm.nb1.log,nsim = 5000)

hist(x$sim_1)
hist(x$sim_2)
hist(x$sim_3)
hist(x$sim_4)
hist(x$sim_5)
hist(x$sim_6)

y = as.vector(t(x))

hist(y)


#let's try simulating

for(j in unique(practice$Lake)){
  sub1 = filter(full.dataset, Lake =="5A")    #subset lake in function
  #sub = filter(sub1, Year == "2021")         #filter out by year or month
  sub = filter(sub1, Month == "May")
  
  for(i in 2:80){
    
    #Fill data frame with relevant info for practice setted lake
    d[1,1] = practice$Lake[1] 
    d[2,1] = practice$Lake[1] 
    d[3,1] = practice$Lake[1]
    d[4,1] = practice$Lake[1]
    d[5,1] = practice$Lake[1]
    d[6,1] = practice$Lake[1] 
    d[7,1] = practice$Lake[1] 
    d[8,1] = practice$Lake[1]
    d[9,1] = practice$Lake[1]
    d[10,1] = practice$Lake[1]
    d[11,1] = practice$Lake[1]
    
    d[1,2] = "lake.mean"
    d[2,2] = "lake.meanplus10"
    d[3,2] = "lake.meanminus10"
    d[4,2] = "lake.meanplus20"
    d[5,2] = "lake.meanminus20"
    d[6,2] = "Upper95CI"
    d[7,2] = "Lower95CI"
    d[8,2] = "lake.meanplus10.minus.upper95"
    d[9,2] = "lower95.minus.lake.meanminus10."
    d[10,2] = "lake.meanplus20.minus.upper95"
    d[11,2] = "lower95.minus.lake.meanminus10."
    
    d[1,i+1] = mean(practice$eDNA)
    d[2,i+1] = mean(practice$eDNA) + mean(practice$eDNA)*.1
    d[3,i+1] = mean(practice$eDNA) - mean(practice$eDNA)*.1
    d[4,i+1] = mean(practice$eDNA) + mean(practice$eDNA)*.2
    d[5,i+1] = mean(practice$eDNA) - mean(practice$eDNA)*.2
    d[6,i+1] = mean(practice$eDNA) + ((1.644854*sd(practice$eDNA))/sqrt(i))
    d[7,i+1] = mean(practice$eDNA) - ((1.644854*sd(practice$eDNA))/sqrt(i))
    d[8,i+1] = d[2,i+1] - d[6,i+1] #written so that positive values imply passing of sample size threshold
    d[9,i+1] = d[7,i+1] - d[3,i+1] #written so that positive values imply passing of sample size threshold
    d[10,i+1] = d[4,i+1]-d[6,i+1] #written so that positive values imply passing of sample size threshold
    d[11,i+1] = d[7,i+1]-d[5,i+1] #written so that positive values imply passing of sample size threshold
    
    }
  d
  summary.d[1,1] = sub$Lake[1]
  summary.d[1,2] = (which(d[8,3:41] >=0)[1])+1
  summary.d[1,3] = (which(d[9,3:41] >=0)[1])+1
  summary.d[1,4] = (which(d[10,3:41] >=0)[1])+1
  summary.d[1,5] = (which(d[11,3:41] >=0)[1])+1
  summary.d[1,6] = mean(summary.d[1,2],summary.d[1,3])
  summary.d[1,7] = mean(summary.d[1,4],summary.d[1,5])
  
  power.dataset = rbind(power.dataset, mean(practice$eDNA),)
  
  summary.power.dataset = rbind(summary.power.dataset,summary.d)
  
}




#Real Analysis
#####
#Real analyses begin below

#Load full dataset and switch year to factor 
full.dataset = read.csv(file.choose()) #"full data with GPS_Rounded.csv", don't use dataset with simulated values or NAs, also I pre-rounded eDNA values as negative binomials only take integers
str(full.dataset)
full.dataset$Year = as.factor(full.dataset$Year)
full.dataset$eDNA = round(full.dataset$eDNA, digits = 0)

##############################################################################
#Testing if lakes exhibited violations of normality
##############################################################################

shapiro_test_results = data.frame(Lake=character(), #dataframe with lake names
                                        p_value=integer(),
                                        Year = character(),
                                        Mean.eDNA = integer(),
                                        Mean.Temperature = integer(),
                                  stringsAsFactors=FALSE)

#now make 'disposable' data frame to rewrite each iteration
#we will rbind to the main 'results' dataframe before we reiterate the loop to save the data
d = data.frame(Lake=character(), #dataframe with lake names
               p_value=integer(),
               Year = character(),
               Mean.eDNA = integer(),
               Mean.Temperature = integer(),
               stringsAsFactors=FALSE)

for(j in unique(full.dataset$Lake)){
  sub = filter(full.dataset, Lake == j)    #subset lake in function
  shap.test = shapiro.test(sub$eDNA)
  d[1,1] = sub$Lake[1] #fill disposable data frame with lake name
  d[1,2] = shap.test$p.value
  sub$year.char = as.character(sub$Year)
  d[1,3] = sub$year.char[1]
  d[1,4] = mean(sub$eDNA)
  d[1,5] = mean(sub$Temperature)
  shapiro_test_results = rbind(shapiro_test_results, d)
}


shapiro_test_results$p_value = round(shapiro_test_results$p_value, digits = 4)
shapiro_test_results

shapiro_test_results$log.Mean.eDNA.scaled = scale(log(shapiro_test_results$Mean.eDNA), center = TRUE, scale = TRUE)
shapiro_test_results$Mean.Temperature.scaled = scale(shapiro_test_results$Mean.Temperature, center = TRUE, scale = TRUE)

#Don't remember what the below is referring to?
#5/13 in 2019
#12/15 in 2020
#interesting pattern

#Use binomial test to examine effect of year, mean eDNA concentration on normality
shapiro_test_results$binary = ifelse(shapiro_test_results$p_value <= 0.05, 1, 0)
str(shapiro_test_results)

m0.shapiro <- glm(binary ~ Year + Mean.Temperature.scaled + log.Mean.eDNA.scaled, family = binomial, data = shapiro_test_results)
check_collinearity(m0.shapiro)

#Low Correlation

#Term                        VIF     VIF 95% CI   Increased SE  Tolerance
#Year                       1.06  [1.00, 18.22]           1.03      0.95
#Mean.Temperature.scaled    1.07  [1.00,  9.37]          1.03       0.93
#log.Mean.eDNA.scaled       1.08  [1.00,  7.90]          1.04       0.93
 
#Pretty bad multicollinearity

#Gonna need to run variables separately
m0.shaprio = glm(binary ~ 1, family = binomial, data = shapiro_test_results)
m1.shapiro = glm(binary ~ log(Mean.eDNA), family = binomial, data = shapiro_test_results)
m2.shapiro = glm(binary ~ Year, family = binomial, data = shapiro_test_results)
m3.shapiro = glm(binary ~ Mean.Temperature, family = binomial, data = shapiro_test_results)

log(shapiro_test_results$Mean.eDNA)

summary(m1.shapiro)
summary(m2.shapiro)
summary(m3.shapiro)
summary(m0.shapiro)

anova(m0.shaprio,m1.shapiro, test = "LRT")
anova(m0.shaprio,m2.shapiro, test = "LRT")
anova(m0.shaprio,m3.shapiro, test = "LRT")


#Testing whether nb fits better than poisson
#####

#Make final results dataframe
nb_vs_poisson_test_results = data.frame(Lake=character(), #dataframe with lake names
                                        dispersion.test.p_value=integer(),
                                        nb.vs.poisson.test.stat = integer(),
                                        nb.vs.poisson.test.p_value = integer(),
                                        stringsAsFactors=FALSE)

#now make 'disposable' data frame to rewrite each iteration
#we will rbind to the main 'results' dataframe before we reiterate the loop to save the data
d = data.frame(Lake=character(), #dataframe with lake names
               dispersion.test.p_value=integer(),
               nb.vs.poisson.test.stat = integer(),
               nb.vs.poisson.test.p_value = integer(),
               stringsAsFactors=FALSE)




for(j in unique(full.dataset$Lake)){
  sub = filter(full.dataset, Lake ==j)    #subset lake in function
  glm.nb = glm.nb(eDNA~1, data = sub) #run neg binomial glm
  glm.poisson = glm(eDNA~1, data = sub, family = poisson)
  d[1,1] = sub$Lake[1] #fill disposable data frame with lake name
  disp.test = dispersiontest(glm.poisson, trafo = 2, alternative = c("greater"))
  d[1,2] = round(disp.test$p.value, digits = 4)
  d[1,3] = (2 * (logLik(glm.nb) - logLik(glm.poisson)))
  d[1,4] = pchisq(2 * (logLik(glm.nb) - logLik(glm.poisson)), df = 1, lower.tail = FALSE)
  nb_vs_poisson_test_results = rbind(nb_vs_poisson_test_results, d)
}

#I get a lot of 0s for nb.vs.poisson.test.p_value, is this correct?
nb_vs_poisson_test_results

#Making a density plot/histogram of all lake eDNA concentrations
######

#Separate out years for density plots because values are too drastically different for graphs to make sense

lakes.2022 = filter(full.dataset, Year == "2022") 
lakes.2021 = filter(full.dataset,Year == "2021")

#Let's look at histograms vs density plots, see which looks better
#Histograms look way, way better just use those
#Makes sense, per-lake sample size is a bit small for density plots in retrospect

#ggplot(lakes.2020, aes(x=ADNe, fill = Year))+ #group=SamplingActivity,
#  geom_density(alpha=.7) +
#  facet_rep_wrap(~Lake, repeat.tick.labels = 'all') + #keeps labels on axes
#  labs(title= paste(""), y="Density", x="eDNA (Copies/L)") + 
#  theme_classic() +
#  theme(legend.position = "none")


ggplot(lakes.2022) +
  geom_histogram(aes(x = eDNA),
                 binwidth = 100000, fill = "grey", color = "black") +
  facet_rep_wrap(~Lake, repeat.tick.labels = 'all') + #keeps labels on axes
  labs(title= paste(""), y="Frequency", x="eDNA (Copies/L)") + 
  theme_classic() +
  theme(legend.position = "none")


ggplot(lakes.2021) +
  geom_histogram(aes(x = eDNA),
                 binwidth = 100000, fill = "grey", color = "black") +
  facet_rep_wrap(~Lake, repeat.tick.labels = 'all') + #keeps labels on axes
  labs(title= paste(""), y="Frequency", x="eDNA (Copies/L)") + 
  theme_classic() +
  theme(legend.position = "none")

#Simulating from a negative binomial distribution parameterized from model run on each lake
#Example on one pond
#####

#Run function on just example lake, for Figure 1

#Run function on just example lake
practice = filter(full.dataset, Lake == "127") #run the function on just a single lake for now

glm.nb1.log = glm.nb(eDNA~1, data = practice, link = "log") #run a glm just to check it out
summary(glm.nb1.log)

hist(practice$eDNA)

x = simulate(glm.nb1.log,nsim = 5000)

sim.data.example = as.vector(t(x)) 
#puts it all in a single vector
#the simulate function just simulates new data for as many datapoints originally supplied to model (in this case 20)


n = 10000
mean.eDNA = numeric(n)
mean.lower.confint = numeric(n)
mean.upper.confint = numeric(n)

boot.lake.eDNA = function(x,y){
  for(i in 1:n){
    lake.resample = sample(x, y, replace = FALSE)
    mean.resample = mean(lake.resample)
    mean.eDNA[i] = mean.resample
  }
  return(mean.eDNA)
}



Boisvert.thresholds = c(1066,1598)
thresholds = Boisvert.thresholds
library(WVPlots)
library(cowplot)

par(mfrow = c(2,3))

x.2 = boot.lake.eDNA(sim.data.example,2)
x.2 = as.data.frame(x.2)
p.2 <- WVPlots::ShadedDensityCenter(frame = x.2, 
                                    xvar = "x.2",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=2)")
plot2 = p.2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

#hist(x.2, main = paste("Histogram of Boisvert means (n = 2)"), xlab = "ADNe Concentration", ylab = "Frequency", breaks = Breaks.1) 
#abline(v = Boisvert.thresholds, col = 'red')


x.5 = boot.lake.eDNA(sim.data.example,5)
x.5 = as.data.frame(x.5)
p.5 <- WVPlots::ShadedDensityCenter(frame = x.5, 
                                    xvar = "x.5",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=5)")
plot5 = p.5 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.10 = boot.lake.eDNA(sim.data.example,10)
x.10 = as.data.frame(x.10)
p.10 <- WVPlots::ShadedDensityCenter(frame = x.10, 
                                     xvar = "x.10",
                                     boundaries = thresholds,
                                     linecolor = "black",
                                     shading = "grey",
                                     title = "Boisvert eDNA means (n=10)")
plot10 = p.10 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.12 = boot.lake.eDNA(sim.data.example,12)
x.12 = as.data.frame(x.12)
p.12 <- WVPlots::ShadedDensityCenter(frame = x.12, 
                                     xvar = "x.12",
                                     boundaries = thresholds,
                                     linecolor = "black",
                                     shading = "grey",
                                     title = "Boisvert eDNA means (n=12)")
plot12 = p.12 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.14 = boot.lake.eDNA(sim.data.example,14)
x.14 = as.data.frame(x.14)
p.14 <- WVPlots::ShadedDensityCenter(frame = x.14, 
                                     xvar = "x.14",
                                     boundaries = thresholds,
                                     linecolor = "black",
                                     shading = "grey",
                                     title = "Boisvert eDNA means (n=14)")
plot14 = p.14 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.16 = boot.lake.eDNA(sim.data.example,16)
x.16 = as.data.frame(x.16)
p.16 <- WVPlots::ShadedDensityCenter(frame = x.16, 
                                     xvar = "x.16",
                                     boundaries = thresholds,
                                     linecolor = "black",
                                     shading = "grey",
                                     title = "Boisvert eDNA means (n=16)")
plot16 = p.16 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.20 = boot.lake.eDNA(sim.data.example,20)
x.20 = as.data.frame(x.20)
p.20 <- WVPlots::ShadedDensityCenter(frame = x.20, 
                                     xvar = "x.20",
                                     boundaries = thresholds,
                                     linecolor = "black",
                                     shading = "grey",
                                     title = "Boisvert eDNA means (n=20)")
plot20 = p.20 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000) 


plot_grid(plot5,plot10,plot12,plot14,plot16,plot20, nrow=2, ncol=3)

#Export as 7*10 landscape plot


#Perform main analysis on all lakes
#All ponds
#####
library(MASS)
library(dplyr)

n = 1000
mean.eDNA = numeric(n)
mean.lower.confint = numeric(n)
mean.upper.confint = numeric(n)
boot.lake.eDNA = function(x,y){
  for(i in 1:n){
    lake.resample = sample(x, y, replace = FALSE)
    mean.resample = mean(lake.resample)
    mean.eDNA[i] = mean.resample
  }
  return(mean.eDNA)
}

#Coefficient of Variation Function
coef.var = function(x){
  sd(x)/mean(x)*100
}

lake.df = data.frame(Lake=character(), #dataframe with lake names
                                 variable=character(),
                                 stringsAsFactors=FALSE)
final.sim.nb.dataset = setNames(data.frame(matrix(ncol = 299, nrow = 0)),paste0("n.", c(2:300))) #dataframe with sample size columns
final.sim.nb.dataset = cbind(lake.df,final.sim.nb.dataset) #combine dataframe


lake.d = data.frame(Lake=character(), #dataframe with lake names
                     variable=character(),
                     stringsAsFactors=FALSE)
d = setNames(data.frame(matrix(ncol = 299, nrow = 0)),paste0("n.", c(2:300))) #dataframe with sample size columns
d = cbind(lake.d,d)


summary.sim.nb.dataset = data.frame(Lake=character(), #dataframe to store summary data for each lake
                                    sample.size.90.within.10perc =integer(),
                                    sample.size.90.within.20perc = integer(),
                                    Lake.CoV = integer(),
                                    Lake.Theta = integer(),
                                    stringsAsFactors=FALSE)

summary.sim.d= data.frame(Lake=character(), #Build empty data frame to populate for each specific lake, same structure as dataframe above for easy 'rbind'ing
                      sample.size.90.within.10perc =integer(),
                      sample.size.90.within.20perc = integer(),
                      Lake.CoV = integer(),
                      Lake.Theta = integer(),
                      stringsAsFactors=FALSE)


for(j in unique(full.dataset$Lake)){
  sub = filter(full.dataset, Lake ==j)    #subset lake in function
  glm.nb.sim = glm.nb(eDNA~1, data = sub) #run a glm just to check it out
  x = simulate(glm.nb.sim,nsim = round(10000/length(sub$eDNA), digits = 0) ) #puts it into a matrix 
  sim.data = as.vector(t(x)) #transforms matrix into a vector so function can sample effectively
  
  for(i in 2:300){
    #Fill data frame with relevant info for subsetted lake, makes searching later easier to do
    d[1,1] = sub$Lake[1]                    
    d[2,1] = sub$Lake[1]
    d[3,1] = sub$Lake[1]
    d[4,1] = sub$Lake[1]
    d[5,1] = sub$Lake[1]
    d[6,1] = sub$Lake[1]
    d[7,1] = sub$Lake[1]
    d[8,1] = sub$Lake[1]
    d[9,1] = sub$Lake[1]
    d[10,1] = sub$Lake[1]
    d[11,1] = sub$Lake[1]
    d[12,1] = sub$Lake[1]
    d[13,1] = sub$Lake[1]
    d[14,1] = sub$Lake[1]
    d[15,1] = sub$Lake[1]

  d[1,2] = "mean.bootstrap"
  d[2,2] = "st.dev.bootstrap"
  d[3,2] = "lower.90.int"
  d[4,2] = "upper.90.int"
  d[5,2] = "CoV"
  d[6,2] = "lake.mean"
  d[7,2] = "lake.meanplus10"
  d[8,2] = "lake.meanminus10"
  d[9,2] = "lake.meanplus20"
  d[10,2] = "lake.meanminus20"
  d[11,2] = "lake.stdev"
  d[12,2] = "lake.CoV"
  d[13,2] = "proportion.values.within.10perc"
  d[14,2] = "proportion.values.within.20perc"
  d[15,2] = "lake.theta"
  
  x = boot.lake.eDNA(sim.data,i)
  d[1,i+1] = mean(x)
  d[2,i+1] = sd(x)
  d[3,i+1] = quantile(x,0.05)
  d[4,i+1] = quantile(x,0.95)
  d[5,i+1] = coef.var(x)
  d[6,i+1] = mean(sim.data)
  d[7,i+1] = mean(sim.data) + mean(sim.data)*.1
  d[8,i+1] = mean(sim.data) - mean(sim.data)*.1
  d[9,i+1] = mean(sim.data) + mean(sim.data)*.2
  d[10,i+1] = mean(sim.data) - mean(sim.data)*.2
  d[11,i+1] = sd(sim.data)
  d[12,i+1] = coef.var(sim.data)
  d[13,i+1] = sum(x > d[8,i+1] & x < d[7,i+1])/length(x) #proportion of values falling within +/- 10%
  d[14,i+1] = sum(x > d[10,i+1] & x < d[9,i+1])/length(x) #proportion of values falling within +/- 20%
  d[15,i+1] = glm.nb.sim$theta
}

summary.sim.d[1,1] = sub$Lake[1]
summary.sim.d[1,2] = (which(d[13,3:301] >=0.90)[1])+1
summary.sim.d[1,3] = (which(d[14,3:301] >=0.90)[1])+1
summary.sim.d[1,4] = d[12,3] #extract lake CoV
summary.sim.d[1,5] = d[15,3] #extract fitted theta parameter from nb glm fit to each lake

final.sim.nb.dataset = rbind(final.sim.nb.dataset, d)

summary.sim.nb.dataset = rbind(summary.sim.nb.dataset,summary.sim.d)

}

write.csv(final.sim.nb.dataset, file = "C:/Users/User/Desktop/Power Analysis/final.sim.nb.dataset.csv")
write.csv(summary.sim.nb.dataset, file = "C:/Users/User/Desktop/Power Analysis/summary.sim.nb.dataset.csv")

final.sim.nb.dataset[1:20,1:20]

summary.sim.nb.dataset

#summary.sim.nb.dataset = read.csv(file.choose()) #To reload older version from .csv file
str(summary.sim.nb.dataset)


#Make a histogram of sampling effort needed for each lake for lowest accuracy threshold from simulated data
######
Breaks = seq(0,100,10)
par(cex.axis = 2.25, cex.lab = 2.25, mar = c(4, 6, 3, 5) + 0.4)
hist(summary.sim.nb.dataset$sample.size.90.within.20perc,main = c(paste("Sampling effort required to obtain "),paste("90% of estimates within +/- 20% of mean")), xlab = "Sampling Effort", ylab = "Frequency", breaks = Breaks)

Breaks2 = seq(0,300,25)
par(cex.axis = 2.25, cex.lab = 2.25, mar = c(4, 6, 3, 5) + 0.4)
hist(summary.sim.nb.dataset$sample.size.90.within.10perc,main = c(paste("Sampling effort required to obtain "),paste("90% of estimates within +/- 10% of mean")), xlab = "Sampling Effort", ylab = "Frequency", breaks = Breaks2)

#Calculate median
median(summary.sim.nb.dataset$sample.size.90.within.20perc)
#28
median(summary.sim.nb.dataset$sample.size.90.within.10perc)
#NA

#Make a nice plot of lake theta vs sampling effort
#Probably don't present this, don't fully understand relationship yet, likely obvious from statistical perspective if you just do the algebra

#Exploratory
plot(log(summary.sim.nb.dataset$Lake.Theta),log(summary.sim.nb.dataset$sample.size.90.within.20perc))
plot((summary.sim.nb.dataset$Lake.Theta),(summary.sim.nb.dataset$sample.size.90.within.20perc))

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,1), c(0,4), xaxt = "n", yaxt = "n", type = "n", xlab = "ln (Lake Theta)",
     ylab = c(paste("Ln (Sampling effort)")), las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,4), ylim = c(0,4) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0, col = "black", cex.axis = 1.5)
points(log(summary.sim.nb.dataset$Lake.Theta),log(summary.sim.nb.dataset$sample.size.90.within.20perc), col = "black", pch = 16, cex = 1.25)  


median(summary.sim.nb.dataset$sample.size.90.within.10perc)
median(summary.sim.nb.dataset$sample.size.90.within.20perc)


#Below is no man's land, run code at own risk
#Feel free to use for ideas though

#####
#Inferring 'clumpiness' of eDNA and it's relationship to density

plot(log(enviro.theta.dataset$mean.eDNA),enviro.theta.dataset$Lake.CoV)
enviro.theta.dataset$log.mean.eDNA = log(enviro.theta.dataset$mean.eDNA)


CoV.eDNAstate = lm(Lake.CoV~log.mean.eDNA + I((log.mean.eDNA)^2), data = enviro.theta.dataset)
summary(CoV.eDNAstate)

CoV.eDNAstate.1 = lm(Lake.CoV~log.mean.eDNA, data = enviro.theta.dataset)
summary(CoV.eDNAstate.1)

anova(CoV.eDNAstate.1,CoV.eDNAstate)
#  Res.Df    RSS Df Sum of Sq      F  Pr(>F)   
#1     26 5753.0                               
#2     25 3886.9  1    1866.2 12.003 0.00193 **


CoV.mean.eDNA.vector = seq(5,12,0.5)

CoV.eDNA.pred.vector = vector(length = length(CoV.mean.eDNA.vector))
CoV.eDNA.pred.vector = as.numeric(CoV.eDNA.pred.vector)
R = length(CoV.mean.eDNA.vector)

for (i in 1:R){
  CoV.pred = emmeans(CoV.eDNAstate, specs = ~log.mean.eDNA, at = list(log.mean.eDNA = CoV.mean.eDNA.vector[i]), type = 'response')
  CoV.pred = summary(CoV.pred)
  CoV.eDNA.pred.vector[i] = CoV.pred$emmean
}

#Check to see if predictions make sense
CoV.eDNA.pred.vector


#Make graph
par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,50), c(5,25), xaxt = "n", yaxt = "n", type = "n", xlab = "log(mean eDNA (Copies/L))",
     ylab = "eDNA Coefficient of Variation", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(5,12), ylim = c(0,90) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 5, col = "black", cex.axis = 1.5)
lines(log.mean.eDNA.vector, CoV.eDNA.pred.vector, col = "black", lty = 1, lwd = 2)
points(enviro.theta.dataset$log.mean.eDNA, enviro.theta.dataset$Lake.CoV, col = "black", pch = 16, cex = 1.4 )  



#Need to repeat analysis on geometric CoV
summary.CoV = data.frame(Lake=character(), #dataframe to store summary data for each lake
                                   Geom.CoV = integer(),
                                    stringsAsFactors=FALSE)

summary.CoV.d = data.frame(Lake=character(), #dataframe to store summary data for each lake
                         Geom.CoV = integer(),
                         stringsAsFactors=FALSE)

for(j in unique(full.dataset$Lake)){
  sub = filter(full.dataset, Lake ==j)    #subset lake in function
  geom.CoV = (sqrt(((exp(1))^((sd(log(sub$ADNe)))^2))-1))*100 #formula to calculate 'geometric CoV'
  summary.CoV.d[1,1] = sub$Lake[1]
  summary.CoV.d[1,2] = geom.CoV
  summary.CoV = rbind(summary.CoV,summary.CoV.d)
  }
  
enviro.theta.dataset = left_join(enviro.theta.dataset,summary.CoV, by = "Lake")

plot(enviro.theta.dataset$Geom.CoV,enviro.theta.dataset$Lake.CoV) #Shouldn't change much, pretty strong relationship
plot(enviro.theta.dataset$log.mean.eDNA,enviro.theta.dataset$Lake.CoV) #Shouldn't change much, pretty strong relationship



#Now we have to investigate extent to which collinearity is a problem
#Step 1: Full model
geom.CoV.eDNAstate.m0 = lm(Geom.CoV~Surface.Area.scaled + Year + Temperature.scaled + log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2) + I(Temperature.scaled^2), data = enviro.theta.dataset)
summary(geom.CoV.eDNAstate.m0)
check_collinearity(geom.CoV.eDNAstate.m0)

#Low Correlation

#Term  VIF Increased SE Tolerance
#Surface.Area.scaled 1.10         1.05      0.91
#log.mean.eDNA.scaled 4.17         2.04      0.24
#I((log.mean.eDNA.scaled)^2) 1.24         1.11      0.81

#Moderate Correlation

#Term  VIF Increased SE Tolerance
#Year 7.27         2.70      0.14
#Temperature.scaled 8.20         2.86      0.12


#Okay, we cannot include Year and temp in the model due to hugh VIFs
#We will drop Year


#Step 2:
geom.CoV.eDNAstate.m1 = lm(Geom.CoV~Surface.Area.scaled + Temperature.scaled + log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2) + I(Temperature.scaled^2), data = enviro.theta.dataset)
summary(geom.CoV.eDNAstate.m1)
check_collinearity(geom.CoV.eDNAstate.m1)

#Low Correlation

#Term  VIF Increased SE Tolerance
#Surface.Area.scaled 1.12         1.06      0.89
#Temperature.scaled 4.15         2.04      0.24
#log.mean.eDNA.scaled 3.83         1.96      0.26
#I((log.mean.eDNA.scaled)^2) 1.36         1.16      0.74
#I(Temperature.scaled^2) 1.64         1.28      0.61

#These are bad, but sort-of within tolerances? I think it's definitely going to affect estimates of the SE around these terms
#let's try retaining them and see what happens


#Let's proceed to model selection:
#Base model:
geom.CoV.eDNAstate.m1 = lm(Geom.CoV~Surface.Area.scaled + Temperature.scaled + log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2) + I(Temperature.scaled^2), data = enviro.theta.dataset)

#Step 1: Test quadratic interaction terms first
geom.CoV.eDNAstate.m2 = lm(Geom.CoV~Surface.Area.scaled + Temperature.scaled + log.mean.eDNA.scaled + I(Temperature.scaled^2), data = enviro.theta.dataset)
geom.CoV.eDNAstate.m3 = lm(Geom.CoV~Surface.Area.scaled + Temperature.scaled + log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2), data = enviro.theta.dataset)

summary(geom.CoV.eDNAstate.m1)

anova(geom.CoV.eDNAstate.m2, geom.CoV.eDNAstate.m1)
#  Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
#1     23 9576.1                              
#2     22 7060.8  1    2515.3 7.8372 0.01045 *
anova(geom.CoV.eDNAstate.m3, geom.CoV.eDNAstate.m1)
#  Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1     23 7066.0                           
#2     22 7060.8  1    5.2096 0.0162 0.8998

#Step 2: drop temp quadratic term
geom.CoV.eDNAstate.m4 = lm(Geom.CoV~Surface.Area.scaled + Temperature.scaled + log.mean.eDNA.scaled, data = enviro.theta.dataset)
anova(geom.CoV.eDNAstate.m4, geom.CoV.eDNAstate.m3)
#  Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
#1     24 9896.7                                
#2     23 7066.0  1    2830.7 9.2141 0.005879 **


#eDNA quadratic Interaction term is highly significant, so we retain it and its corresponding linear term 



#Step 3: Test other base effects
geom.CoV.eDNAstate.m5 = lm(Geom.CoV~Temperature.scaled + log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2), data = enviro.theta.dataset)
geom.CoV.eDNAstate.m6 = lm(Geom.CoV~Surface.Area.scaled + log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2), data = enviro.theta.dataset)
summary(geom.CoV.eDNAstate.m5)

anova(geom.CoV.eDNAstate.m5, geom.CoV.eDNAstate.m3)
#  Res.Df    RSS Df Sum of Sq    F Pr(>F)
#1     24 7069.1                         
#2     23 7066.0  1    3.0651 0.01 0.9213
anova(geom.CoV.eDNAstate.m6, geom.CoV.eDNAstate.m3)
#  Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1     24 7145.3                           
#2     23 7066.0  1    79.295 0.2581 0.6163



#Step 4: drop Surface area term
geom.CoV.eDNAstate.m7 = lm(Geom.CoV ~ log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2), data = enviro.theta.dataset)

anova(geom.CoV.eDNAstate.m7, geom.CoV.eDNAstate.m5)
#  Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1     25 7165.8                           
#2     24 7069.1  1    96.706 0.3283  0.572

#Drop temperature  term



#Final model:
geom.CoV.eDNAstate.m7 = lm(Geom.CoV ~ log.mean.eDNA.scaled + I((log.mean.eDNA.scaled)^2), data = enviro.theta.dataset)
summary(geom.CoV.eDNAstate.m7)

str(enviro.theta.dataset$log.mean.eDNA.scaled)
#Make graph
geom.CoV.mean.eDNA.vector = seq(5,12,0.25)
geom.CoV.mean.eDNA.vector.scaled = (geom.CoV.mean.eDNA.vector-8.15)/1.94


geom.CoV.eDNA.pred.vector = vector(length = length(geom.CoV.mean.eDNA.vector))
geom.CoV.eDNA.pred.vector = as.numeric(geom.CoV.eDNA.pred.vector)
R = length(geom.CoV.mean.eDNA.vector)

for (i in 1:R){
  geom.CoV.pred = emmeans(geom.CoV.eDNAstate.m7, specs = ~log.mean.eDNA.scaled, at = list(log.mean.eDNA.scaled = geom.CoV.mean.eDNA.vector.scaled[i]), type = 'response')
  geom.CoV.pred = summary(geom.CoV.pred)
  geom.CoV.eDNA.pred.vector[i] = geom.CoV.pred$emmean
}

#Check to see if predictions make sense
geom.CoV.eDNA.pred.vector


#Make graph
par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,50), c(5,25), xaxt = "n", yaxt = "n", type = "n", xlab = "log(mean eDNA (Copies/L))",
     ylab = "eDNA Geometric Coefficient of Variation", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(5,12), ylim = c(0,110) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 5, col = "black", cex.axis = 1.5)
lines(geom.CoV.mean.eDNA.vector, geom.CoV.eDNA.pred.vector, col = "black", lty = 1, lwd = 2)
points(enviro.theta.dataset$log.mean.eDNA, enviro.theta.dataset$Geom.CoV, col = "black", pch = 16, cex = 1.4 )  







#Exploring relationships between environmental variables and sample size to accurately quantify mean

summary.dataset = read.csv(file.choose()) #summary dataset file provided by Maxime, contains environmental data for each lake
enviro.theta.dataset = full_join(summary.sim.nb.dataset,summary.dataset, by = "Lake")
str(enviro.theta.dataset)
enviro.theta.dataset$Year = as.factor(enviro.theta.dataset$Year)


#Do some preliminary graphs to visually gauge if data need transformations
#Variables to include: Temp, mean eDNA, Surface area, lake perimeter
#Depth and pH were dropped due to lack of data for all lakes

#Definitely needs a log transformation
plot(log(enviro.theta.dataset$mean.eDNA),(enviro.theta.dataset$sample.size.90.within.20perc))


#These variables are probably fine
plot((enviro.theta.dataset$Temperature),(enviro.theta.dataset$sample.size.90.within.20perc))
plot((enviro.theta.dataset$Surface.Area),(enviro.theta.dataset$sample.size.90.within.20perc))
plot((enviro.theta.dataset$Perimeter),(enviro.theta.dataset$sample.size.90.within.20perc))


#Let's do the analysis, with log-trandsformed mean eDNA
#Six parameters is about the max we can fit in here, and even then it's an overfit model
#We could consider cutting one of the more 'redundent' ones
#Let's see how correlated the variables are with each other
#The real question is if we want to run a linear model or a poisson, since the dependent variable is a count
#Gonna start with nb to test VIFs, because I suspect that we will end up on nb vs a Poisson


#Let's go with a nb for now to test multi-collinearity
#Want to examine variables when they are scaled and centered
#Won't affect interpretation of final output, generally best practice for running regressions
enviro.theta.dataset$Temperature.scaled = scale(enviro.theta.dataset$Temperature, center = TRUE, scale = TRUE)
enviro.theta.dataset$Surface.Area.scaled = scale(enviro.theta.dataset$Surface.Area, center = TRUE, scale = TRUE)
enviro.theta.dataset$log.mean.eDNA.scaled = scale(log(enviro.theta.dataset$mean.eDNA), center = TRUE, scale = TRUE)


m.scaled = glm.nb(sample.size.90.within.20perc~ Temperature.scaled + Surface.Area.scaled + log.mean.eDNA.scaled + Year + I(log.mean.eDNA.scaled^2) + I(Temperature.scaled^2), data = enviro.theta.dataset)
summary(m.scaled)

check_collinearity(m.scaled)
#Low Correlation

#                     Term  VIF Increased SE Tolerance
# Surface.Area.scaled 1.11         1.05      0.90
# log.mean.eDNA.scaled 3.75         1.94      0.27

#Moderate Correlation

#             Term  VIF Increased SE Tolerance
#Temperature.scaled 7.17         2.68      0.14
#               Year 6.87         2.62      0.15


#Well that right there is pretty crappy, but not unexpected
#Temperature is pretty damn correlated with Year of collection, not much we can do about it except drop Year I think


m1.scaled = glm.nb(sample.size.90.within.20perc~ Temperature.scaled + Surface.Area.scaled, data = enviro.theta.dataset)
summary(m1.scaled)
str(enviro.theta.dataset)
#Need to check for multicollinearity using VIFs
check_collinearity(m1.scaled)
#Check for Multicollinearity

#Low Correlation

#                 Term  VIF Increased SE Tolerance
#   Temperature.scaled 3.77         1.94      0.27
#   Surface.Area.scaled 1.09         1.04      0.92
#   log.mean.eDNA.scaled 3.66         1.91      0.27
    
#This is better, but still not great
#Honestly Temperature and log.mea.eDNA.scaled are still to correlated
#You can tell this by examining these models:


m3 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled + log.mean.eDNA.scaled, 
            data = enviro.theta.dataset)
m5 = glm.nb(sample.size.90.within.20perc~ log.mean.eDNA.scaled, 
            data = enviro.theta.dataset)
m6 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled, 
            data = enviro.theta.dataset)

anova(m3,m5)
#p = 0.2632947
anova(m3,m6)
#p = 0.4827258

#Both of these terms are not significant when together



#Step 3
m6 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled, 
            data = enviro.theta.dataset)
m5 = glm.nb(sample.size.90.within.20perc~ log.mean.eDNA.scaled, 
            data = enviro.theta.dataset)
m7 = glm.nb(sample.size.90.within.20perc~ 1, 
            data = enviro.theta.dataset)

anova(m6,m7)
#p = 0.009585737
anova(m5,m7)
#p = 0.01470349

#But both are highly significant when alone - this is due to inflated standard errors due to multicollinearity
#Note also that the strength of significance is very similar


#Makes sense, variables are highly correlated. See model:
temp.eDNA = lm(log(mean.eDNA)~Temperature, data = enviro.theta.dataset)
summary(temp.eDNA)
plot((enviro.theta.dataset$Temperature),log(enviro.theta.dataset$mean.eDNA))
#this is pretty strong, R^2 of .7, we clearly need to drop one variable
#This is actually a very challenging decision, as they are both strong candidates for effect on necessary sample sizes due to functional link







#I was originally going to conduct model selection, but honestly the multi-collinearity among the variables is just too much
#I think we need to just examine the variables separately - we will instead conduct an 'exploratory analysis' 
#We can even throw Year in there too






#Okay we have our candidate variables, now let's check which distribution works best, poisson or negative binomial
#Use likelihood-ratio test, as above
#Include interactions in this version
m1.scaled = glm.nb(sample.size.90.within.20perc~ Temperature.scaled, data = enviro.theta.dataset)
m1.scaled.poisson = glm(sample.size.90.within.20perc~ Temperature.scaled, family = "poisson", data = enviro.theta.dataset)
nb.vs.poisson.test = pchisq(2 * (logLik(m1.scaled)-logLik(m1.scaled.poisson)), df = 1, lower.tail = FALSE)
nb.vs.poisson.test
#p = 2.102696e-15
#So again, negative binomial distribution works a lot better than Poisson
dispersiontest(m1.scaled.poisson)
#p-value = 0.0009398


m1.scaled = glm.nb(sample.size.90.within.20perc~ Surface.Area.scaled, data = enviro.theta.dataset)
m1.scaled.poisson = glm(sample.size.90.within.20perc~ Surface.Area.scaled, family = "poisson", data = enviro.theta.dataset)
nb.vs.poisson.test = pchisq(2 * (logLik(m1.scaled)-logLik(m1.scaled.poisson)), df = 1, lower.tail = FALSE)
nb.vs.poisson.test
#p = 2.477342e-21
#So again, negative binomial distribution works a lot better than Poisson
dispersiontest(m1.scaled.poisson)
#p = 0.001458

m1.scaled = glm.nb(sample.size.90.within.20perc~ log.mean.eDNA.scaled, data = enviro.theta.dataset)
m1.scaled.poisson = glm(sample.size.90.within.20perc~ log.mean.eDNA.scaled, family = "poisson", data = enviro.theta.dataset)
nb.vs.poisson.test = pchisq(2 * (logLik(m1.scaled)-logLik(m1.scaled.poisson)), df = 1, lower.tail = FALSE)
nb.vs.poisson.test
#p = 2.445905e-17
#So again, negative binomial distribution works a lot better than Poisson
dispersiontest(m1.scaled.poisson)
#p = 0.005893







#Graphing results + significance tests for terms
str(enviro.theta.dataset)
#look at effect of year on sampling effort
m.0 = glm.nb(sample.size.90.within.20perc~ 1, data = enviro.theta.dataset)

m.year = glm.nb(sample.size.90.within.20perc~ Year, data = enviro.theta.dataset)

year.model = emmeans(m.year, pairwise ~ Year)
year.model

year.model.response = emmeans(m.year, pairwise ~ Year, type = 'response')
year.model.response

anova(m.0, m.year)
#  Model    theta Resid. df    2 x log-lik.   Test    df LR stat.   Pr(Chi)
#1     1 2.897932        27       -200.7146                                
#2  Year 3.179987        26       -198.5207 1 vs 2     1 2.193948 0.1385537





#Effect of surface area on sampling effort
m.surface.area = glm.nb(sample.size.90.within.20perc~ Surface.Area.scaled, data = enviro.theta.dataset)
anova(m.surface.area,m.0)



#Effect of temperature on samplign effort
m.temp = glm.nb(sample.size.90.within.20perc~ Temperature, data = enviro.theta.dataset)
anova(m.temp,m.0)
#        Model    theta Resid. df    2 x log-lik.   Test    df LR stat.     Pr(Chi)
#1           1 2.897932        27       -200.7146                                  
#2 Temperature 3.849592        26       -194.0043 1 vs 2     1 6.710304 0.009585737


#Temperature graph 
temp.vector = seq(5,25,1)

temp.pred.vector = vector(length = length(temp.vector))
temp.pred.vector = as.numeric(temp.pred.vector)
R = length(temp.vector)

for (i in 1:R){
  temp.pred = emmeans(m.temp, specs = ~Temperature, at = list(Temperature = temp.vector[i]), type = 'response')
  temp.pred = summary(temp.pred)
  temp.pred.vector[i] = temp.pred$response
}

#check to see if function worked/made reasonable results
temp.pred.vector


par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,50), c(5,25), xaxt = "n", yaxt = "n", type = "n", xlab = "Temperature (C)",
     ylab = c(paste("Sampling effort for 90% of estimate"),paste("within +/- 20% of mean")), las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(5,25), ylim = c(0,50) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 5, col = "black", cex.axis = 1.5)
lines(temp.vector, temp.pred.vector, col = "black", lty = 1, lwd = 2)
points(enviro.theta.dataset$Temperature, enviro.theta.dataset$sample.size.90.within.20perc, col = "black", pch = 16, cex = 1.5 )  



####repeating graph on scaled terms
#should be identical to above if I didn't fuck this up
m.temp.scaled = glm.nb(sample.size.90.within.20perc~ Temperature.scaled, data = enviro.theta.dataset)
summary(m.temp.scaled)

anova(m.0,m.temp.scaled)

max(enviro.theta.dataset$Temperature.scaled)


temp.vector = seq(5,25,1)
temp.vector.scaled = (temp.vector-17.16679)/3.97408


temp.pred.vector.scaled = vector(length = length(temp.vector.scaled))
temp.pred.vector.scaled = as.numeric(temp.pred.vector.scaled)
R = length(temp.vector.scaled)

for (i in 1:R){
  temp.pred = emmeans(m.temp.scaled, specs = ~Temperature.scaled, at = list(Temperature.scaled = temp.vector.scaled[i]), type = 'response')
  temp.pred = summary(temp.pred)
  temp.pred.vector.scaled[i] = temp.pred$response
}

#check to see if function worked/made reasonable results
temp.pred.vector.scaled

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,50), c(5,25), xaxt = "n", yaxt = "n", type = "n", xlab = "Temperature (C)",
     ylab = c(paste("Sampling effort for 90% of estimate"),paste("within +/- 20% of mean")), las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(5,25), ylim = c(0,50) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 5, col = "black", cex.axis = 1.5)
lines(temp.vector, temp.pred.vector.scaled, col = "black", lty = 1, lwd = 2)
points(enviro.theta.dataset$Temperature, enviro.theta.dataset$sample.size.90.within.20perc, col = "black", pch = 16, cex = 1.5 )  




#Now we do log(mean eDNA) vs. sampling effort
#Now I want to run an analysis examining the effect of the 'mean concentration' of eDNA on sampling effort
m.log.eDNA = glm.nb(sample.size.90.within.20perc~ log.mean.eDNA.scaled, data = enviro.theta.dataset)
m.log.eDNA.1 = glm.nb(sample.size.90.within.20perc~ log.mean.eDNA.scaled + I(log.mean.eDNA.scaled^2), data = enviro.theta.dataset)


summary(m.log.eDNA.1)
summary(m.log.eDNA)


anova(m.log.eDNA,m.log.eDNA.1)
#                                            Model    theta Resid. df    2 x log-lik.   Test    df LR stat.      Pr(Chi)
#1                             log.mean.eDNA.scaled 3.659719        26       -194.7630                                   
#2 log.mean.eDNA.scaled + I(log.mean.eDNA.scaled^2) 7.052450        25       -181.1956 1 vs 2     1 13.56738 0.0002301503


#Make a graph

#log(mean.eDNA) graph 
str(enviro.theta.dataset$log.mean.eDNA.scaled)

log.mean.eDNA.vector = seq(5,12,0.25)
log.mean.eDNA.vector.scaled = (log.mean.eDNA.vector-8.15)/1.94
log.eDNA.pred.vector.scaled = vector(length = length(log.mean.eDNA.vector.scaled))
log.eDNA.pred.vector.scaled = as.numeric(log.eDNA.pred.vector.scaled)
R = length(log.mean.eDNA.vector)

for (i in 1:R){
  eDNA.pred = emmeans(m.log.eDNA.1, specs = ~log.mean.eDNA.scaled, at = list(log.mean.eDNA.scaled = log.mean.eDNA.vector.scaled[i]), type = 'response')
  eDNA.pred = summary(eDNA.pred)
  log.eDNA.pred.vector[i] = eDNA.pred$response
}

#check to see if function worked/made reasonable results
log.eDNA.pred.vector


par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,50), c(5,25), xaxt = "n", yaxt = "n", type = "n", xlab = "log(mean eDNA copies/L)",
     ylab = c(paste("Sampling effort for 90% of estimate"),paste("within +/- 20% of mean")), las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(5,12), ylim = c(0,50) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 5, col = "black", cex.axis = 1.5)
lines(log.mean.eDNA.vector, log.eDNA.pred.vector, col = "black", lty = 1, lwd = 2)
points(enviro.theta.dataset$log.mean.eDNA, enviro.theta.dataset$sample.size.90.within.20perc, col = "black", pch = 16, cex = 1.5 )  



#Experimenting to see if multicollinearity still a problem
m.log.eDNA.2 = glm.nb(sample.size.90.within.20perc~ log.mean.eDNA.scaled + I(log.mean.eDNA.scaled^2) + Temperature.scaled, data = enviro.theta.dataset)
check_collinearity(m.log.eDNA.2)

summary(m.log.eDNA.2)










#Looking at Spatial autocorrelation using Moran's I

library(ape)
spatial.auto = full.dataset[,c(2:6,8)]


#example to make function work
practice = filter(spatial.auto, Lake == 'Boisvert') 
station.dists <- as.matrix(dist(cbind(practice$Longitude , practice$Latitude), method = "euclidean"))
station.dists.inv <- 1/station.dists
diag(station.dists.inv) <- 0

station.dists.inv[1:5, 1:5]

practice.moran = Moran.I(practice$ADNe, station.dists.inv)
practice.moran$observed
lake.moran.temp[1,1] = practice$Lake[1]
lake.moran.temp[1,2] = practice.moran$observed
lake.moran.temp[1,3] = practice.moran$expected
lake.moran.temp[1,4] = practice.moran$p.value



lake.moran = data.frame(Lake=character(), #dataframe with lake names to populate with function
                     Moran.Observed=integer(),
                     Moran.Expected = integer(),
                     Moran.p.value = integer(),
                     stringsAsFactors=FALSE)

lake.moran.temp = data.frame(Lake=character(), #dataframe with lake names to population at each stage of the function
                             Moran.Observed=integer(),
                             Moran.Expected = integer(),
                             Moran.p.value = integer(),
                        stringsAsFactors=FALSE)

for(j in unique(spatial.auto$Lake)){
  sub = filter(spatial.auto, Lake ==j)    #subset lake in function
  station.dists <- as.matrix(dist(cbind(sub$Longitude , sub$Latitude), method = "euclidean"))
  station.dists.inv <- 1/station.dists
  diag(station.dists.inv) <- 0
  Moran.stats = Moran.I(sub$ADNe, station.dists.inv)
  lake.moran.temp[1,1] = sub$Lake[1]
  lake.moran.temp[1,2] = Moran.stats$observed
  lake.moran.temp[1,3] = Moran.stats$expected
  lake.moran.temp[1,4] = Moran.stats$p.value
  lake.moran = rbind(lake.moran, lake.moran.temp)
}

lake.moran$Moran.p.value = round(lake.moran$Moran.p.value, digits = 5)





final.analysis.dataset = left_join(summary.sim.nb.dataset,lake.moran, by = "Lake")

final.analysis.dataset



#Testing statistical difference between Anais' and this dataset
anais.vs.maxime.data = data.frame(study=c("Anais", "Maxime"), 
                        Success=c(1,4),
                        Failure = c(11,24),
                        stringsAsFactors=FALSE)

anais.vs.maxime.test.m0 = glm(cbind(Success,Failure)~study, data =anais.vs.maxime.data, family = "binomial")
anais.vs.maxime.test.m1 = glm(cbind(Success,Failure)~1, data =anais.vs.maxime.data, family = "binomial")

summary(anais.vs.maxime.test.m1)
summary(anais.vs.maxime.test)

anova(anais.vs.maxime.test.m0,anais.vs.maxime.test.m1, test = "LRT")
#Model 1: cbind(Success, Failure) ~ study
#Model 2: cbind(Success, Failure) ~ 1
#Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#1         0    0.00000                     
#2         1    0.29103 -1 -0.29103   0.5896




#Next, need to evaluate effect of surface area on spatial autocorrelation
spatial.auto.envirodata = left_join(summary.dataset,final.analysis.dataset, by = "Lake")
spatial.auto.envirodata$AutoCorr.detected = ifelse(spatial.auto.envirodata$Moran.p.value < 0.05, 1, 0)
spatial.auto.envirodata$z.score =  spatial.auto.envirodata$Moran.Observed - spatial.auto.envirodata$Moran.Expected
str(spatial.auto.envirodata)
spatial.auto.envirodata$Year = as.factor(spatial.auto.envirodata$Year)


plot(spatial.auto.envirodata$Surface.Area,spatial.auto.envirodata$z.score)
plot(spatial.auto.envirodata$Temperature,spatial.auto.envirodata$z.score)
plot(spatial.auto.envirodata$sample.size.90.within.20perc,spatial.auto.envirodata$z.score)


m0.spatial.auto = glm(AutoCorr.detected ~ Surface.Area, data = spatial.auto.envirodata, family="binomial")

summary(m0.spatial.auto) #this sucks, has no statistical power

#regression on z-scores:
m0.spatial.auto = lm(z.score ~ Surface.Area + Year + Temperature + sample.size.90.within.20perc, data = spatial.auto.envirodata)
check_collinearity(m0.spatial.auto)
#Year is very colinear with Temperature, gotta drop Year

m0.spatial.auto = lm(z.score ~ Surface.Area + Temperature, data = spatial.auto.envirodata)
check_collinearity(m0.spatial.auto)
summary(m0.spatial.auto)
#No predictor of spatial autocorrelation based on data we have

anova(m0.spatial.auto)
#             Df   Sum Sq   Mean Sq F value Pr(>F)
#Surface.Area  1 0.000017 0.0000171  0.0018 0.9663
#Temperature   1 0.016800 0.0167998  1.7867 0.1934
#Residuals    25 0.235069 0.0094027


m2.spatial.auto = lm(z.score ~ log(mean.eDNA), data = spatial.auto.envirodata)
anova(m2.spatial.auto)





#Looking at distribution of theta is relation to concentration of eDNA

plot(log(enviro.theta.dataset$Lake.Theta), enviro.theta.dataset$log.mean.eDNA)

summary(lm(log.mean.eDNA ~log(Lake.Theta), data = enviro.theta.dataset))



















#Playing around with secondary analysis regarding systematic vs. random sampling)

#Example lake to get code up and running





n = 10000
mean.eDNA = numeric(n)
mean.lower.confint = numeric(n)
mean.upper.confint = numeric(n)
boot.lake.eDNA = function(x,y){
  for(i in 1:n){
    lake.resample = sample(x, y, replace = FALSE)
    mean.resample = mean(lake.resample)
    mean.eDNA[i] = mean.resample
  }
  return(mean.eDNA)
}



Boisvert.thresholds = c(1066,1598)
library(WVPlots)

par(mfrow = c(2,3))

x.2 = boot.lake.eDNA(sim.data.example,2)












































######
#All of this is old, disregard
######
#Now we start model selection process, let's use likelihood ratio tests since it's a glm
#F-tests are unsuitable

#Step 1 - We are NOT testing interactions, for the record; we have three continuous variables, and an 'n' of 28
#That is very insufficient to test continuous interactions
#Could probably get away with interactions for year because categorical, but we dropped year earlier
#We are sticking to main effects for this analysis


m1b.scaled = glm.nb(sample.size.90.within.20perc~ Temperature.scaled + Surface.Area.scaled + log.mean.eDNA.scaled, 
                    data = enviro.theta.dataset)


#Step 2
m2 = glm.nb(sample.size.90.within.20perc~ Surface.Area.scaled + log.mean.eDNA.scaled, 
            data = enviro.theta.dataset)
m3 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled + log.mean.eDNA.scaled, 
            data = enviro.theta.dataset)
m4 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled + Surface.Area.scaled, 
            data = enviro.theta.dataset)

anova(m1b.scaled,m2)
#p = 0.2715579
anova(m1b.scaled,m3)
#                                                            Model    theta Resid. df    2 x log-lik.   Test    df   LR stat.   Pr(Chi)
#1                       Temperature.scaled + log.mean.eDNA.scaled 3.890231        25       -193.5116                                  
#2 Temperature.scaled + Surface.Area.scaled + log.mean.eDNA.scaled 3.897882        24       -193.4739 1 vs 2     1 0.03773714 0.8459719
anova(m1b.scaled,m4)
#p = 0.5489473


#Step 3
m3 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled + log.mean.eDNA.scaled, 
            data = enviro.theta.dataset)
m5 = glm.nb(sample.size.90.within.20perc~ log.mean.eDNA.scaled, 
            data = enviro.theta.dataset)
m6 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled, 
            data = enviro.theta.dataset)

anova(m3,m5)
#p = 0.2632947
anova(m3,m6)
#                                      Model    theta Resid. df    2 x log-lik.   Test    df  LR stat.   Pr(Chi)
#1                        Temperature.scaled 3.849592        26       -194.0043                                 
#2 Temperature.scaled + log.mean.eDNA.scaled 3.890231        25       -193.5116 1 vs 2     1 0.4926991 0.4827258



#Step 3
m6 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled, 
            data = enviro.theta.dataset)
m7 = glm.nb(sample.size.90.within.20perc~ 1, 
            data = enviro.theta.dataset)

anova(m6,m7)
#               Model    theta Resid. df    2 x log-lik.   Test    df LR stat.     Pr(Chi)
#1                  1 2.897932        27       -200.7146                                  
#2 Temperature.scaled 3.849592        26       -194.0043 1 vs 2     1 6.710304 0.009585737



#Final model:
m6 = glm.nb(sample.size.90.within.20perc~ Temperature.scaled, 
            data = enviro.theta.dataset)
summary(m6)
#Check model residuals to see if they are reasonable
resids = residuals(m5)
hist(resids)
qqnorm(resids)
qqline(resids)
#Honestly not bad, but I don't think normality of residuals matters as much anyways for Poisson or negative binomial models



#Example stuff for Louis
#Subset example lake
example = filter(full.dataset,Lake == 'Boisvert')
hist(example$ADNe)
Breaks.1 = seq(0,5000,250)
hist(example$ADNe, main = paste("Histogram of Boisvert eDNA Concentrations"), xlab = "ADNe Concentration", ylab = "Frequency", breaks = Breaks.1) 
dens = density(example$ADNe)
plot(dens)


glm.nb1 = glm.nb(ADNe~1, data = example) #run a glm  
summary(glm.nb1)
resids = residuals(glm.nb1)
hist(resids)
qqnorm(resids)
qqline(resids)

glm.nb1$theta

#make a nie density plot
x = simulate(glm.nb1,nsim = 5000) #puts it into a matrix 
dens.example = density(as.vector(t(x)))
plot(dens.example, ann=FALSE)

x = simulate(glm.nb1,nsim = 500) #puts it into a matrix 
sim.data.example = as.vector(t(x)) #transforms matrix into a vector so function can sample effectively

hist(example$ADNe)
Breaks = seq(0,5000,250)
hist(sim.data.example, main = paste("Histogram of Simulated Boisvert eDNA Concentrations"), xlab = "Simulated ADNe Concentration", ylab = "Frequency",breaks = Breaks, yaxt = 'n')



max(sim.data.example)


#Prelim analysis on environmental variables
































#Run function on just example lake


n = 10000
mean.eDNA = numeric(n)
mean.lower.confint = numeric(n)
mean.upper.confint = numeric(n)
boot.lake.eDNA = function(x,y){
  for(i in 1:n){
    lake.resample = sample(x, y, replace = FALSE)
    mean.resample = mean(lake.resample)
    mean.eDNA[i] = mean.resample
  }
  return(mean.eDNA)
}



Boisvert.thresholds = c(1066,1598)

x.2 = boot.lake.eDNA(sim.data.example,2)
x.2 = as.data.frame(x.2)
p.2 <- WVPlots::ShadedDensityCenter(frame = x.2, 
                                  xvar = "x.2",
                                  boundaries = thresholds,
                                  linecolor = "black",
                                  shading = "grey",
                                  title = "Boisvert eDNA means (n=2)")
p.2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)



#hist(x.2, main = paste("Histogram of Boisvert means (n = 2)"), xlab = "ADNe Concentration", ylab = "Frequency", breaks = Breaks.1) 
#abline(v = Boisvert.thresholds, col = 'red')


x.5 = boot.lake.eDNA(sim.data.example,5)
x.5 = as.data.frame(x.5)
p.5 <- WVPlots::ShadedDensityCenter(frame = x.5, 
                                    xvar = "x.5",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=5)")
p.5 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.10 = boot.lake.eDNA(sim.data.example,10)
x.10 = as.data.frame(x.10)
p.10 <- WVPlots::ShadedDensityCenter(frame = x.10, 
                                    xvar = "x.10",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=10)")
p.10 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.12 = boot.lake.eDNA(sim.data.example,12)
x.12 = as.data.frame(x.12)
p.12 <- WVPlots::ShadedDensityCenter(frame = x.12, 
                                    xvar = "x.12",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=12)")
p.12 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.14 = boot.lake.eDNA(sim.data.example,14)
x.14 = as.data.frame(x.14)
p.14 <- WVPlots::ShadedDensityCenter(frame = x.14, 
                                    xvar = "x.14",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=14)")
p.14 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.16 = boot.lake.eDNA(sim.data.example,16)
x.16 = as.data.frame(x.16)
p.16 <- WVPlots::ShadedDensityCenter(frame = x.16, 
                                    xvar = "x.16",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=16)")
p.16 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000)

x.20 = boot.lake.eDNA(sim.data.example,20)
x.20 = as.data.frame(x.20)
p.20 <- WVPlots::ShadedDensityCenter(frame = x.20, 
                                    xvar = "x.20",
                                    boundaries = thresholds,
                                    linecolor = "black",
                                    shading = "grey",
                                    title = "Boisvert eDNA means (n=20)")
p.20 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(0,4000) 


#Export as 4*7 landscape plot














for(i in 2:300){
  #Fill data frame with relevant info for subsetted lake, makes searching later easier to do
  d[1,1] = sub$Lake[1] 
  d[2,1] = sub$Lake[1]
  d[3,1] = sub$Lake[1]
  d[4,1] = sub$Lake[1]
  d[5,1] = sub$Lake[1]
  d[6,1] = sub$Lake[1]
  d[7,1] = sub$Lake[1]
  d[8,1] = sub$Lake[1]
  d[9,1] = sub$Lake[1]
  d[10,1] = sub$Lake[1]
  d[11,1] = sub$Lake[1]
  d[12,1] = sub$Lake[1]
  d[13,1] = sub$Lake[1]
  d[14,1] = sub$Lake[1]
  d[15,1] = sub$Lake[1]
  d[16,1] = sub$Lake[1]
  
  d[1,2] = "mean.bootstrap"
  d[2,2] = "st.dev.bootstrap"
  d[3,2] = "lower.90.int"
  d[4,2] = "upper.90.int"
  d[5,2] = "CoV"
  d[6,2] = "lake.mean"
  d[7,2] = "lake.meanplus10"
  d[8,2] = "lake.meanminus10"
  d[9,2] = "lake.meanplus20"
  d[10,2] = "lake.meanminus20"
  d[11,2] = "lake.stdev"
  d[12,2] = "lake.CoV"
  d[13,2] = "lake.meanplus10.minus.upper90"
  d[14,2] = "lower90.minus.lake.meanminus10."
  d[15,2] = "lake.meanplus20.minus.upper90"
  d[16,2] = "lower90.minus.lake.meanminus20."
  
  
  x = boot.lake.eDNA(sim.data,i)
  d[1,i+1] = mean(x)
  d[2,i+1] = sd(x)
  d[3,i+1] = quantile(x,0.05)
  d[4,i+1] = quantile(x,0.95)
  d[5,i+1] = coef.var(x)
  d[6,i+1] = mean(sim.data)
  d[7,i+1] = mean(sim.data) + mean(sim.data)*.1
  d[8,i+1] = mean(sim.data) - mean(sim.data)*.1
  d[9,i+1] = mean(sim.data) + mean(sim.data)*.2
  d[10,i+1] = mean(sim.data) - mean(sim.data)*.2
  d[11,i+1] = sd(sim.data)
  d[12,i+1] = coef.var(sim.data)
  d[13,i+1] = d[7,i+1]-d[4,i+1] #written so that positive values imply passing of sample size threshold
  d[14,i+1] = d[3,i+1]-d[8,i+1] #written so that positive values imply passing of sample size threshold
  d[15,i+1] = d[9,i+1]-d[4,i+1] #written so that positive values imply passing of sample size threshold
  d[16,i+1] = d[3,i+1]-d[10,i+1] #written so that positive values imply passing of sample size threshold
  
}

summary.sim.d[1,1] = sub$Lake[1]
summary.sim.d[1,2] = (which(d[13,3:301] >=0)[1])+1
summary.sim.d[1,3] = (which(d[14,3:301] >=0)[1])+1
summary.sim.d[1,4] = (which(d[15,3:301] >=0)[1])+1
summary.sim.d[1,5] = (which(d[16,3:301] >=0)[1])+1
summary.sim.d[1,6] = (((which(d[13,3:301] >=0)[1])+1)+((which(d[14,3:301] >=0)[1])+1))/2  #mean(summary.sim.d[1,2],summary.sim.d[1,3])
summary.sim.d[1,7] = (((which(d[15,3:301] >=0)[1])+1)+((which(d[16,3:301] >=0)[1])+1))/2  #mean(summary.sim.d[1,4],summary.sim.d[1,5])
summary.sim.d[1,8] = d[12,3]






















