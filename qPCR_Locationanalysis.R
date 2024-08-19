qPCR.near.off = read.csv(file.choose())

#Note: We removed ponds with A) no goldfish; or B) Zero amplification (105, in this case)

str(qPCR.near.off)
#Need to round to integers for neg binomial model
qPCR.near.off$Number.Hits = round(qPCR.near.off$Number.Hits, digits = 0)

#Note: Negative binomial model did not converge


#test concentrations of samples in nearshore vs offshore sampling

#We will do a linear mixed effects model
#Must have a pond random effect, to account for non-independence of samples taken from same pond

library(lme4)

Model1 = glmer.nb(Number.Hits ~ Sample.Type + (1|Pond), data = qPCR.near.off)


summary(Model1)

#Testing fitting a pond-specific nearshore vs. offshore random effect
Model1a = glmer.nb(Number.Hits ~ Sample.Type + (Sample.Type|Pond), data = qPCR.near.off)
summary(Model1a)


anova(Model1,Model1a)



#We need to include the 'sample type' random pond intercept, it improves the model a lot
#We are done testing random effect - always keep the random effect from Model 1a 

#Now we test, across ponds, whether there is a CONSISTENT difference between near vs off

Model2a = glmer.nb(Number.Hits ~ 1 + (1|Pond), data = qPCR.near.off)
summary(Model2a)

anova(Model1,Model2a)

#Pond has a way stronger effect on eDNA concentration
#No effect of Sample Type on concentration of eDNA in goldfish ponds



















