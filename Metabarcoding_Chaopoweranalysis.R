metabar.chao = read.csv(file.choose())


str(metabar.chao)

library(lme4)

model1 = lmer(Sample.Threshold ~ Stringency.Criteria + Total.Num.Species.DFO + Pond.Size.ha + Temp +
                (1|Pond), 
              data = metabar.chao)

summary(model1)

model1b = lmer(Sample.Threshold ~ 1  + (1|Pond), 
               data = metabar.chao)

summary(model1b)


model1 = glmer(Sample.Threshold ~ Stringency.Criteria + Total.Num.Species.DFO + Pond.Size.ha + Temp +
                      (1|Pond), control=glmerControl(optCtrl=list(maxfun=1000000)), 
                    data = metabar.chao)
summary(model1)


model1.Pois= glmer(Sample.Threshold ~ Stringency.Criteria + Total.Num.Species.DFO + Pond.Size.ha + Temp +
                         (1|Pond), family = poisson, 
                       data = metabar.chao)
summary(model1.Pois)

anova(model1,model1.Pois)



#Negative binomial does not provide a better fit relative to the Poisson
#We will use Poisson, therefore
#Now we will do backwards model selection 

model1 = glmer(Sample.Threshold ~ Stringency.Criteria + Total.Num.Species.DFO + Pond.Size.ha + Temp +
                             (1|Pond), family = poisson, 
                           data = metabar.chao)

model2 =  glmer(Sample.Threshold ~ Total.Num.Species.DFO + Pond.Size.ha + Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)

model3 =  glmer(Sample.Threshold ~ Stringency.Criteria + Pond.Size.ha + Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)

model4 =  glmer(Sample.Threshold ~ Stringency.Criteria + Total.Num.Species.DFO + Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)

model5 =  glmer(Sample.Threshold ~ Stringency.Criteria + Total.Num.Species.DFO + Pond.Size.ha + 
                  (1|Pond), family = poisson, 
                data = metabar.chao)

anova(model1,model2)
#0.269
anova(model1,model3)
#0.93
anova(model1,model4)
#0.275
anova(model1,model5)
#0.3111


#Step 2
model2 =  glmer(Sample.Threshold ~ Total.Num.Species.DFO + Pond.Size.ha + Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)
model6 =  glmer(Sample.Threshold ~ Pond.Size.ha + Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)
model7 =  glmer(Sample.Threshold ~ Total.Num.Species.DFO + Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)
model8 =  glmer(Sample.Threshold ~ Total.Num.Species.DFO + Pond.Size.ha +
                  (1|Pond), family = poisson, 
                data = metabar.chao)

anova(model2,model6)
#0.93
anova(model2,model7)
#0.275
anova(model2,model8)
#0.3111


#Step 3
model7 =  glmer(Sample.Threshold ~ Total.Num.Species.DFO + Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)
model9 =  glmer(Sample.Threshold ~ Temp +
                  (1|Pond), family = poisson, 
                data = metabar.chao)
model10 =  glmer(Sample.Threshold ~ Total.Num.Species.DFO +
                   (1|Pond), family = poisson, 
                data = metabar.chao)

anova(model7,model9)
#0.8927
anova(model7,model10)
#0.3873

#Step 4
model10 =  glmer(Sample.Threshold ~ Total.Num.Species.DFO +
                   (1|Pond), family = poisson, 
                 data = metabar.chao)
model11 =  glmer(Sample.Threshold ~ (1|Pond), family = poisson, 
                 data = metabar.chao)

anova(model9,model11)
#0.1897



summary(model11)

library(emmeans)

#This gives you an estimate of the MEAN across all your ponds
emmeans.metabarcode.chao = emmeans(model11, ~ 1, type = "response")
emmeans.metabarcode.chao

#However, the confidence interval corresponds only to the mean estimate
#NOT the 'extreme cases'



#For that, let's simulate based on the fitted parameters!

Simulated_Poisson = rpois(10000, lambda = 6.34)
par(mar = c(5, 6, 4, 2) + 0.1)
hist(Simulated_Poisson, cex.axis = 2.3, cex.lab = 2.3)

perc.threshold.95 = quantile(Simulated_Poisson, probs = c(.5, .75, .85, .95))
perc.threshold.95






