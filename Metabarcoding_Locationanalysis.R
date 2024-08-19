Meta.near.off = read.csv(file.choose())
str(Meta.near.off)

library(lme4)

ModelA = glmer(cbind(Species.Detected,Detection.Failures) ~ Sample.Type + Number.Of.Samples + 
                 (1|Pond), data = Meta.near.off, family = binomial)
summary(ModelA)

ModelB = glmer(cbind(Species.Detected,Detection.Failures) ~ Number.Of.Samples + 
                 (1|Pond), data = Meta.near.off, family = binomial)

ModelC = glmer(cbind(Species.Detected,Detection.Failures) ~ Sample.Type + 
                 (1|Pond), data = Meta.near.off, family = binomial)

#Significance of Sample Type variable
anova(ModelA, ModelB)
#0.1148

#Significance of Number of Samples variable
anova(ModelA, ModelC)
#<0.001

#check for variance inflation
#This tests for collinearity among predictors


#Just to check if this might be an issue
ModelC = glmer(cbind(Species.Detected,Detection.Failures) ~ Sample.Type + 
                 (1|Pond), data = Meta.near.off, family = binomial)

ModelD = glmer(cbind(Species.Detected,Detection.Failures) ~ 1 + 
                  (1|Pond), data = Meta.near.off, family = binomial)

anova(ModelC, ModelD)

#Variance inflation factor code
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

vif.mer(ModelA)







################################################################################
#Repeat this analysis on all stringency criteria
################################################################################






