library(dplyr)
library(lme4)
library(tidyr)


#Master Metabarcoding file "Metabarcode master file high only"
CPUE.metabar = read.csv(file.choose())
str(CPUE.metabar)

#CPUE file "CPUE"
Pure.CPUE = read.csv(file.choose())
#Pure.CPUE = subset(Pure.CPUE, CPUE != "???")
str(Pure.CPUE)

#Organizing of the Data
#
#
#
#Split the dataset into two - separate by marker first

MMito.CPUE = filter(CPUE.metabar, Marker == "Mito")

#
num.samples = MMito.CPUE %>% group_by(Pond,Cell) %>% summarise(length(unique(Site)))
#

example.MMito = MMito.CPUE %>% group_by(Pond,Cell,Species) %>% summarise(sum(Standard.Meta.Hit))
example.MMito$Aggregate.hits = example.MMito$`sum(Standard.Meta.Hit)`
example.MMito$`sum(Standard.Meta.Hit)` = NULL

sum.pond.reads.MMito = MMito.CPUE %>% group_by(Pond,Cell) %>% summarise(sum(Standard.Meta.Hit))
sum.pond.reads.MMito$Total.hits.all.spec = sum.pond.reads.MMito$`sum(Standard.Meta.Hit)`
sum.pond.reads.MMito$`sum(Standard.Meta.Hit)` = NULL

#this is for easy merging later
temp.reads.df = example.MMito %>%  unite(Pond_Cell, c("Pond", "Cell"))
example.MMito = cbind(example.MMito,temp.reads.df$Pond_Cell)
example.MMito$Pond_Cell = example.MMito$...5
example.MMito$...5 = NULL

temp.sumreads.df = sum.pond.reads.MMito %>%  unite(Pond_Cell, c("Pond", "Cell"))
sum.pond.reads.MMito = cbind(sum.pond.reads.MMito,temp.sumreads.df$Pond_Cell)
sum.pond.reads.MMito$Pond_Cell = sum.pond.reads.MMito$...4
sum.pond.reads.MMito$...4 = NULL


#example.MMito$prop.reads = example.MMito$Aggregate.hits

all.data.MMito = left_join(sum.pond.reads.MMito, example.MMito, by='Pond_Cell')
all.data.MMito$Pond = all.data.MMito$Pond.y
all.data.MMito$Cell = all.data.MMito$Cell.y
all.data.MMito$Pond.x = NULL
all.data.MMito$Cell.x = NULL
all.data.MMito$Pond.y = NULL
all.data.MMito$Cell.y = NULL


all.data.MMito$relative.hits = all.data.MMito$Aggregate.hits/all.data.MMito$Total.hits.all.spec
correct.column.order = c("Pond","Cell", "Pond_Cell", "Species","Aggregate.hits","Total.hits.all.spec","relative.hits")
all.data.MMito = all.data.MMito %>% select(correct.column.order)

#
all.data.MMito = merge.data.frame(all.data.MMito,num.samples)
all.data.MMito$num.sites = all.data.MMito$`length(unique(Site))` 
all.data.MMito$`length(unique(Site))` = NULL

#Calculate a standardized read metric
all.data.MMito$stand.reads = all.data.MMito$Aggregate.hits/all.data.MMito$num.sites
#
options(scipen = 999)
all.data.MMito


#Now do PS1

PS1.CPUE = filter(CPUE.metabar, Marker == "PS1")

num.samples = PS1.CPUE %>% group_by(Pond,Cell) %>% summarise(length(unique(Site)))

example.PS1 = PS1.CPUE %>% group_by(Pond,Cell,Species) %>% summarise(sum(Standard.Meta.Hit))
example.PS1$Aggregate.hits = example.PS1$`sum(Standard.Meta.Hit)`
example.PS1$`sum(Standard.Meta.Hit)` = NULL

sum.pond.reads.PS1 = PS1.CPUE %>% group_by(Pond,Cell) %>% summarise(sum(Standard.Meta.Hit))
sum.pond.reads.PS1$Total.hits.all.spec = sum.pond.reads.PS1$`sum(Standard.Meta.Hit)`
sum.pond.reads.PS1$`sum(Standard.Meta.Hit)` = NULL

#this is for easy merging later
temp.reads.df = example.PS1 %>%  unite(Pond_Cell, c("Pond", "Cell"))
example.PS1 = cbind(example.PS1,temp.reads.df$Pond_Cell)
example.PS1$Pond_Cell = example.PS1$...5
example.PS1$...5 = NULL

temp.sumreads.df = sum.pond.reads.PS1 %>%  unite(Pond_Cell, c("Pond", "Cell"))
sum.pond.reads.PS1 = cbind(sum.pond.reads.PS1,temp.sumreads.df$Pond_Cell)
sum.pond.reads.PS1$Pond_Cell = sum.pond.reads.PS1$...4
sum.pond.reads.PS1$...4 = NULL


#example.PS1$prop.reads = example.PS1$Aggregate.hits

all.data.PS1 = left_join(sum.pond.reads.PS1, example.PS1, by='Pond_Cell')
all.data.PS1$Pond = all.data.PS1$Pond.y
all.data.PS1$Cell = all.data.PS1$Cell.y
all.data.PS1$Pond.x = NULL
all.data.PS1$Cell.x = NULL
all.data.PS1$Pond.y = NULL
all.data.PS1$Cell.y = NULL


all.data.PS1$relative.hits = all.data.PS1$Aggregate.hits/all.data.PS1$Total.hits.all.spec
correct.column.order = c("Pond","Cell", "Pond_Cell", "Species","Aggregate.hits","Total.hits.all.spec","relative.hits")
all.data.PS1 = all.data.PS1 %>% select(correct.column.order)

#
all.data.PS1 = merge.data.frame(all.data.PS1,num.samples)
all.data.PS1$num.sites = all.data.PS1$`length(unique(Site))` 
all.data.PS1$`length(unique(Site))` = NULL

#Calculate a standardized read metric
all.data.PS1$stand.reads = all.data.PS1$Aggregate.hits/all.data.PS1$num.sites
#
options(scipen = 999)
all.data.PS1


#Now we need to bind the CPUE info into the relevant rows in the metabarcoding data sets
#Also need to convert from % to numeric

#We need to create a 'Pond_Cell' column in the Pure.CPUE file for later merging w/ eDNA file
temp.CPUE.df = Pure.CPUE %>%  unite(Pond_Cell, c("Pond", "Cell"))
Pure.CPUE = cbind(Pure.CPUE,temp.CPUE.df$Pond_Cell)
Pure.CPUE$Pond_Cell = Pure.CPUE[,8]
Pure.CPUE[,8] = NULL

merged.MMito = merge.data.frame(all.data.MMito, Pure.CPUE)
str(merged.MMito)

merged.PS1 = merge.data.frame(all.data.PS1, Pure.CPUE)
str(merged.PS1)

# Analysis
#
#
#

#MMito Analysis - Average Hits
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

#MITO ANALYSIS

#This first analysis looks at standardized read number against raw CPUE
#It's an OK analysis, but not the best potential approach

TestModel.MMitoA <- lmer(stand.reads ~ CPUE + Capture.Method + CPUE:Capture.Method + (1|Pond) + (Capture.Method|Species), data = merged.MMito)
vif.mer(TestModel.MMitoA)
#super inflated, but don't worry about it because it's the interaction term
#high vifs dissapear when interaction is removed from model

summary(TestModel.MMitoA)

TestModel.MMitoA.1 <- lmer(stand.reads ~ CPUE + Capture.Method + CPUE:Capture.Method + (1|Pond) + (1|Species), data = merged.MMito)
anova(TestModel.MMitoA,TestModel.MMitoA.1)
#p = 0.9741, we cut it

TestModel.MMitoA.2 <- lmer(stand.reads ~ CPUE + Capture.Method + (1|Pond) + (1|Species), data = merged.MMito)
anova(TestModel.MMitoA.2,TestModel.MMitoA.1)
#Highly significant interaction term
#this is pretty dope
summary(TestModel.MMitoA.1)


library(emmeans)

Mmito.results = emtrends(TestModel.MMitoA.1, specs = pairwise ~ Capture.Method, var = "CPUE")
#Capture.Method CPUE.trend   SE   df lower.CL upper.CL
#Efish               10316 2493 63.4     5336    15296
#Seine                 613  118 62.0      376      850

#PS1 ANALYSIS

TestModel.PS1A <- lmer(stand.reads ~ CPUE + Capture.Method + CPUE:Capture.Method + (1|Pond) + (Capture.Method|Species), data = merged.PS1)
vif.mer(TestModel.PS1A)
#super inflated, but don't worry about it because it's the interaction term
#high vifs dissapear when interaction is removed from model

summary(TestModel.PS1A)

TestModel.PS1A.1 <- lmer(stand.reads ~ CPUE + Capture.Method + CPUE:Capture.Method + (1|Pond) + (1|Species), data = merged.PS1)
anova(TestModel.PS1A,TestModel.PS1A.1)
#p = 0.1, we cut it

TestModel.PS1A.2 <- lmer(stand.reads ~ CPUE + Capture.Method + (1|Pond) + (1|Species), data = merged.PS1)
anova(TestModel.PS1A.2,TestModel.PS1A.1)
#Non significant interaction term
#this is NOT pretty dope

#Step 2, because interaction was not significant
TestModel.PS1A.3 <- lmer(stand.reads ~ Capture.Method + (1|Pond) + (1|Species), data = merged.PS1)

TestModel.PS1A.4 <- lmer(stand.reads ~ CPUE + (1|Pond) + (1|Species), data = merged.PS1)

anova(TestModel.PS1A.2,TestModel.PS1A.3)
#0.009576
anova(TestModel.PS1A.2,TestModel.PS1A.4)
#0.8027

#step 3, test PS1A.4 against intercept only model

TestModel.PS1A.5 <- lmer(stand.reads ~ 1 + (1|Pond) + (1|Species), data = merged.PS1)

anova(TestModel.PS1A.4,TestModel.PS1A.5)
#0.008838

#Final model = PS1A.4


summary(TestModel.PS1A.4)

PS1.results = emtrends(TestModel.PS1A.4, specs = ~1, var = "CPUE")
# 1       CPUE.trend SE   df lower.CL upper.CL
# overall        127 49 63.6     29.5      225

#Degrees-of-freedom method: kenward-roger 

# Visualization
#
#
#

library(ggplot2)

merged.MMito.efish <- filter(merged.MMito, Capture.Method == "Efish")
merged.MMito.seine <- filter(merged.MMito, Capture.Method == "Seine")

# Now let's create the plot
##### Mito

#First thing we need to do, is calculate out line values for each model
Mmito.results
summary(TestModel.MMitoA.1)

#We can extract all the information we need to make the 'lines' from the model summary in combination with the emmeans object
#First, let's identify the intercepts
summary(TestModel.MMitoA.1)
#Since efishing is the 'default' method in the model summary, the listed intercept corresponds to the efishing intercept
#(w/outlier)
efishing.intercept = 5821

#w/out outlier
#efishing.intercept = 5964

#We can derive the seine intercept but adding the value of the 'Capture.MethodSeine' intercept modifier in the summary
#(w/outlier)
Seine.intercept = 5821 + 13511 

#w/out outlier
#Seine.intercept = 5964 + 7392

#now, we use the emmeans to derive the slopes
#note that we can actually use the same method above to derive slope values as well
#emmeans is easier to do this with, but also kind of necessary for big models
Mmito.results

#(w outlier)
Efishing.slope = 10316 
Seine.slope = 613 

#w/o outlier
#Efishing.slope = 10259 
#Seine.slope = 1562  

##### PS1

#Since there was no sig difference between capture methods for the PS1 primer, we do not have to separate into 2 graphs
#We can extract the slope and intercept directly from the summary of the model
summary(TestModel.PS1A.4)

#w outlier
PS1.slope = 3489.86
PS1.intercept = 127.48

#w/o outlier
#PS1.slope = 2624.2
#PS1.intercept = 402.8 

#Correlation Calcs
library(MuMIn)
r.squaredGLMM(TestModel.MMitoA.1)
r.squaredGLMM(TestModel.PS1A.4)

#Let's do efishing for M-Mito first

PlotMMito.efish = ggplot(merged.MMito.efish, aes(x = CPUE, y = stand.reads)) +
  geom_point(aes(color = Capture.Method), size = 1.75, colour = "red") +  
  geom_abline(intercept = efishing.intercept, slope = Efishing.slope, linetype = "dashed", color = "black", linewidth = 1) + 
  labs(title = "Relationship between CPUE and stand reads - MMito.efish",
       x = "CPUE", y = "Standardized reads") +
  theme_classic() +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15))
  

PlotMMito.efish

#Now seine for MMito

PlotMMito.seine = ggplot(merged.MMito.seine, aes(x = CPUE, y = stand.reads)) +
  geom_point(aes(color = Capture.Method), size = 1.75, colour = "green") +  
  geom_abline(intercept = Seine.intercept, slope = Seine.slope, linetype = "dashed", color = "black", linewidth = 1) + 
  labs(title = "Relationship between CPUE and stand reads - MMito.seine",
  x = "CPUE", y = "Standardized reads") +
  theme_classic() +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15)) 

PlotMMito.seine

##### PS1
PlotPS1 = ggplot(merged.PS1, aes(x = CPUE, y = stand.reads)) +
  geom_point(aes(), colour = "blue", size = 1.75) +  
  geom_abline(intercept = PS1.intercept, slope = PS1.slope, linetype = "dashed", color = "black", linewidth = 1) + 
    labs(title = "Relationship between CPUE and stand reads - PS1",
       x = "CPUE", y = "Standardized reads") +
  theme_classic() +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15)) 

PlotPS1

#####
PlotMMito.efish
PlotMMito.seine
PlotPS1

#####
#Second analysis: now we will look at relative read count and relative CPUE, which should be a stronger fit
#first thing we do is some data exploration

#plot
plot(merged.MMito$Relative.CPUE,merged.MMito$relative.hits)


#Let's explore using a beta-binomial model 
install.packages('TMB', type = 'source')

library(glmmTMB)

BetaMito.binom.m1 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                  data = merged.MMito, family = betabinomial(link = "logit"), weights = Total.hits.all.spec)
summary(BetaMito.binom.m1)

resids.Mito = residuals(BetaMito.binom.m1)
hist(resids.Mito, las = 1)
qqnorm(resids.Mito, las = 1)
qqline(resids.Mito)
#This is not the prettiest qqplot

#Let's try changing the 1.0 to 0.99, so we can use the beta distribution
merged.MMito[1,7] = 0.999

BetaMito.m2 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                         data = merged.MMito, family = beta_family(link = "logit"))
summary(BetaMito.m2)
resids.betaMito = residuals(BetaMito.m2)
hist(resids.betaMito, las = 1)
qqnorm(resids.betaMito, las = 1)
qqline(resids.betaMito)


#Now let's try integrating overdispersion into the model
BetaMito.binom.od = glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                                         data = merged.MMito, family = betabinomial(link = "logit"), weights = Total.hits.all.spec,
                                         dispformula = ~ Relative.CPUE)
summary(BetaMito.binom.od)

anova(BetaMito.binom.od,BetaMito.binom.m1)
#No evidence for overdispersion in betabinomial
#Not sure if betabinomial is the best model to use though?

#Beta might make more sense in the context of our data



BetaMito.od <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                   data = merged.MMito, family = beta_family(link = "logit"),
                   dispformula = ~ Relative.CPUE)
summary(BetaMito.od)
resids.betaMito.od = residuals(BetaMito.od)
hist(resids.betaMito.od, las = 1)
qqnorm(resids.betaMito.od, las = 1)
qqline(resids.betaMito.od)

anova(BetaMito.od,BetaMito.m2)
#No evidence to support overdispersion


#Let's add method as a covariate
BetaMito.od.m0 <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                   data = merged.MMito, family = beta_family(link = "logit"),
                   dispformula = ~ Relative.CPUE*Capture.Method)

summary(BetaMito.od.m0)

#Let's test dispersion parameter again
BetaMito.od.m0a <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                      data = merged.MMito, family = beta_family(link = "logit"),
                      dispformula = ~ Relative.CPUE + Capture.Method)
anova(BetaMito.od.m0,BetaMito.od.m0a)

#Now we test the two base dispersion parameters
BetaMito.od.m0b <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                       data = merged.MMito, family = beta_family(link = "logit"),
                       dispformula = ~ Capture.Method)
BetaMito.od.m0c <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                       data = merged.MMito, family = beta_family(link = "logit"),
                       dispformula = ~ Relative.CPUE)
anova(BetaMito.od.m0a,BetaMito.od.m0b)

anova(BetaMito.od.m0a,BetaMito.od.m0c)

#final overdispersion test
BetaMito.od.m0d <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                       data = merged.MMito, family = beta_family(link = "logit"))
anova(BetaMito.od.m0c,BetaMito.od.m0d)
#0.7544

#No evidence of overdispersion

#Now we test teh fixed effects
#don't test random effects because they are inherently needed due to experimental design
BetaMito.m0d <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                       data = merged.MMito, family = beta_family(link = "logit"))
BetaMito.m1 <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + (1|Pond) + (1|Species), 
                       data = merged.MMito, family = beta_family(link = "logit"))
anova(BetaMito.m0d,BetaMito.m1)
#0.05196, marginally significant
#we will probably cut? 

summary(BetaMito.m0d)



BetaMito.m2 <- glmmTMB(relative.hits ~ Capture.Method + (1|Pond) + (1|Species), 
                   data = merged.MMito, family = beta_family(link = "logit"))
BetaMito.m3 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                   data = merged.MMito, family = beta_family(link = "logit"))
anova(BetaMito.m1,BetaMito.m2)
anova(BetaMito.m1,BetaMito.m3)

#
BetaMito.m3 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                   data = merged.MMito, family = beta_family(link = "logit"))
BetaMito.m4 <- glmmTMB(relative.hits ~ 1 + (1|Pond) + (1|Species), 
                   data = merged.MMito, family = beta_family(link = "logit"))
anova(BetaMito.m3,BetaMito.m4)
#Highly significant
#p = <0.001

#Now you have to plot!
summary(BetaMito.m3)

#Need to back-transform logit values, which are what is presented in model summary
library(boot)
inv.logit(-2.4812)

inv.logit(-2.4812 + 1*4.5033)
inv.logit(-2.4812 + 0.5*4.5033)


predict.values.Mito = seq(0,1,0.01)
predicts.Mito = c()

for(i in 1:length(predict.values.Mito)){
  predicts.Mito[i] = inv.logit(-2.4812 + (i)/100*4.5033)
  
  }

library(ggplot2)

plot(predicts.Mito,predict.values.Mito)
final.predict.Mito = data.frame()
final.predict.Mito = cbind(predict.values.Mito,predicts.Mito)

Mito.beta.plot = ggplot(data = merged.MMito, aes(x = Relative.CPUE, y = relative.hits)) +
  geom_point(aes(), colour = "black", size = 4.5) +
  geom_line(data = final.predict.Mito, aes(x = predict.values.Mito, y = predicts.Mito)) +
  labs(title = "Relationship between Relative CPUE and Relative reads - Mito",
     x = "Relative CPUE", y = "Relative Reads") +
  theme_classic() + 
  theme(axis.title = element_text(size = 35), axis.text = element_text(size = 30), plot.title = element_text(size = 32))

Mito.beta.plot
#Now for PS1 - pray its nice!
#####


#plot
plot(merged.PS1$Relative.CPUE,merged.PS1$relative.hits)


BetaPS1.binom.m1 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                             data = merged.PS1, family = betabinomial(link = "logit"), weights = Total.hits.all.spec)
summary(BetaPS1.binom.m1)

resids.PS1 = residuals(BetaPS1.binom.m1)
hist(resids.PS1)
qqnorm(resids.PS1)
qqline(resids.PS1)
#This is not the prettiest qqplot

#Let's try changing the 1.0 to 0.99, so we can use the beta distribution
merged.PS1[1,7] = 0.999

BetaPS1.m2 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                       data = merged.PS1, family = beta_family(link = "logit"))
summary(BetaPS1.m2)
resids.betaPS1 = residuals(BetaPS1.m2)
hist(resids.betaPS1)
qqnorm(resids.betaPS1)
qqline(resids.betaPS1)


#Now let's try integrating overdispersion into the model
BetaPS1.binom.od = glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                            data = merged.PS1, family = betabinomial(link = "logit"), weights = Total.hits.all.spec,
                            dispformula = ~ Relative.CPUE)
summary(BetaPS1.binom.od)

anova(BetaPS1.binom.od,BetaPS1.binom.m1)
#There is evidence for overdispersion in betabinomial!


#Beta might make more sense in the context of our data



BetaPS1.od <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                       data = merged.PS1, family = beta_family(link = "logit"),
                       dispformula = ~ Relative.CPUE)
summary(BetaPS1.od)
resids.betaPS1.od = residuals(BetaPS1.od)
hist(resids.betaPS1.od, las = 1)
qqnorm(resids.betaPS1.od, las = 1)
qqline(resids.betaPS1.od)

anova(BetaPS1.od,BetaPS1.m2)
#Evidence to support overdispersion


#Let's add method as a covariate
BetaPS1.od.m0 <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                          data = merged.PS1, family = beta_family(link = "logit"),
                          dispformula = ~ Relative.CPUE*Capture.Method)

summary(BetaPS1.od.m0)

#Let's test dispersion parameter again
BetaPS1.od.m0a <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                           data = merged.PS1, family = beta_family(link = "logit"),
                           dispformula = ~ Relative.CPUE + Capture.Method)
anova(BetaPS1.od.m0,BetaPS1.od.m0a)

#Now we test the two base dispersion parameters
BetaPS1.od.m0b <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                           data = merged.PS1, family = beta_family(link = "logit"),
                           dispformula = ~ Capture.Method)
BetaPS1.od.m0c <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                           data = merged.PS1, family = beta_family(link = "logit"),
                           dispformula = ~ Relative.CPUE)
anova(BetaPS1.od.m0a,BetaPS1.od.m0b)

anova(BetaPS1.od.m0a,BetaPS1.od.m0c)

#final overdispersion test
BetaPS1.od.m0d <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                           data = merged.PS1, family = beta_family(link = "logit"))
anova(BetaPS1.od.m0c,BetaPS1.od.m0d)
#0.2501

#No evidence of overdispersion?

#Now we test the fixed effects
#don't test random effects because they are inherently needed due to experimental design
BetaPS1.m0d <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + Relative.CPUE:Capture.Method + (1|Pond) + (1|Species), 
                        data = merged.PS1, family = beta_family(link = "logit"))
BetaPS1.m1 <- glmmTMB(relative.hits ~ Relative.CPUE + Capture.Method + (1|Pond) + (1|Species), 
                       data = merged.PS1, family = beta_family(link = "logit"))
anova(BetaPS1.m0d,BetaPS1.m1)
#0.06755, marginally significant
#we will probably cut? 

summary(BetaPS1.m0d)



BetaPS1.m2 <- glmmTMB(relative.hits ~ Capture.Method + (1|Pond) + (1|Species), 
                       data = merged.PS1, family = beta_family(link = "logit"))
BetaPS1.m3 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                       data = merged.PS1, family = beta_family(link = "logit"))
anova(BetaPS1.m1,BetaPS1.m2)
anova(BetaPS1.m1,BetaPS1.m3)

#
BetaPS1.m3 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                       data = merged.PS1, family = beta_family(link = "logit"))
BetaPS1.m4 <- glmmTMB(relative.hits ~ 1 + (1|Pond) + (1|Species), 
                       data = merged.PS1, family = beta_family(link = "logit"))
anova(BetaPS1.m3,BetaPS1.m4)
#Highly significant



#Now you have to plot!
summary(BetaPS1.m3)

#Need to back-transform logit values, which are what is presented in model summary
library(boot)

inv.logit(-1.8853)
inv.logit(-1.8853 + 1*3.1587)
inv.logit(-1.8853 + 0.5*3.1587)

predict.values.PS1 = seq(0,1,0.01)
predicts.PS1 = c()

for(i in 1:length(predict.values.PS1)){
  predicts.PS1[i] = inv.logit(-1.8853 + (i)/100*3.1587)
  
}

plot(predicts.PS1,predict.values.PS1)
final.predict.PS1 = data.frame()
final.predict.PS1 = cbind(predict.values.PS1,predicts.PS1)

PS1.beta.plot = ggplot(data = merged.PS1, aes(x = Relative.CPUE, y = relative.hits)) +
  geom_point(aes(), colour = "black", size = 4.5) +
  geom_line(data = final.predict.PS1, aes(x = predict.values.PS1, y = predicts.PS1)) +
  labs(title = "Relationship between Relative CPUE and Relative reads - PS1",
       x = "Relative CPUE", y = "Relative Reads", size = 15) +
  theme_classic() + 
  theme(axis.title = element_text(size = 35), axis.text = element_text(size = 30), plot.title = element_text(size = 32))

PS1.beta.plot

#####
Mito.beta.plot 
PS1.beta.plot

#####
#Determine Fit of models
merged.MMito$Relative.CPUE[merged.MMito$Relative.CPUE == 0] <- 0.001
merged.MMito$Relative.CPUE[merged.MMito$Relative.CPUE == 1] <- 0.999

merged.PS1$Relative.CPUE[merged.PS1$Relative.CPUE == 0] <- 0.001
merged.PS1$Relative.CPUE[merged.PS1$Relative.CPUE == 1] <- 0.999

#Mito
BetaMito.m3 <- glmmTMB(relative.hits ~ Relative.CPUE  + (1|Pond) + (1|Species), 
                       data = merged.MMito, family = beta_family(link = "logit"))
summary(BetaMito.m3)


predict.Mito1 = predict(BetaMito.m3, type = 'response')
Actual.Mito = merged.MMito$relative.hits
Efrons.pseudo.rsquared.MMito = 1 - sum((Actual.Mito - predict.Mito1)^2)/sum((Actual.Mito - mean(Actual.Mito))^2)
Efrons.pseudo.rsquared.MMito


plot(predict.Mito1, Actual.Mito)


model.mmito = lm(Actual.Mito ~ predict.Mito1)
summary(model.mmito)

x = data.frame()
x$predict = cbind(predict.Mito1)

#PS1
BetaPS1.m3 <- glmmTMB(relative.hits ~ Relative.CPUE + (1|Pond) + (1|Species), 
                      data = merged.PS1, family = beta_family(link = "logit"))
summary(BetaPS1.m3)


predict.PS11 = predict(BetaPS1.m3, type = 'response')
Actual.PS1 = merged.PS1$relative.hits
Efrons.pseudo.rsquared.PS1 = 1 - sum((Actual.PS1 - predict.PS11)^2)/sum((Actual.PS1 - mean(Actual.PS1))^2)
Efrons.pseudo.rsquared.PS1


plot(predict.PS11, Actual.PS1)


model.mPS1 = lm(Actual.PS1 ~ predict.PS11)
summary(model.mPS1)

