certainty.dat = read.csv(file.choose())
certainty.dat$Pond = as.character(certainty.dat$Pond)
str(certainty.dat)

library(lme4)
library(emmeans)
library(ggplot2)

#Drained Removed
ModelA = glmer(Num.Species.Detect ~ Method + (1|Pond), data = certainty.dat, family = "poisson")
summary(ModelA)

ModelB = glmer(Num.Species.Detect ~ 1 + (1|Pond), data = certainty.dat, family = "poisson")
summary(ModelB)

anova(ModelA, ModelB)

ggplot(certainty.dat, aes(x = Pond, y = Num.Species.Detect, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("lightgray", "darkgray", "black")) +  
  scale_y_continuous(expand = c(0, 0), limits = c(0, 13)) +  # Set max y-axis value to 13
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        axis.title = element_text(size = 14)) +  # Adjust axis label size
  ylab("Number of Species Detected")  # Change y-axis label


#Not on reponse scale
ModelA.emmeans = emmeans(ModelA, ~ Method, adjust = "bonferroni")
ModelA.emmeans

#HIGH RESULTS
#Method emmean    SE  df asymp.LCL asymp.UCL
#eDNA    1.433 0.175 Inf     1.015      1.85
#Efish   1.311 0.215 Inf     0.796      1.83
#Seine   0.976 0.208 Inf     0.480      1.47

#LOW RESULTS

#Method emmean    SE  df asymp.LCL asymp.UCL
#eDNA    1.736 0.149 Inf     1.378      2.09
#Efish   1.378 0.199 Inf     0.902      1.85
#Seine   0.986 0.196 Inf     0.517      1.46

ModelA.emmeans.resp = emmeans(ModelA, ~ Method, type = "response", adjust = "bonferroni")
ModelA.emmeans.resp

#HIGH RESULTS
#Method rate    SE  df asymp.LCL asymp.UCL
#eDNA   4.19 0.732 Inf      2.76      6.37
#Efish  3.71 0.797 Inf      2.22      6.20
#Seine  2.65 0.551 Inf      1.62      4.36

#LOW RESULTS

#Method rate    SE  df asymp.LCL asymp.UCL
#eDNA   5.67 0.847 Inf      3.97      8.11
#Efish  3.97 0.789 Inf      2.46      6.39
#Seine  2.68 0.526 Inf      1.68      4.29

#Extract pairwise comparisons and their significance using the pairs function of emmeans
pairs.ModelA = pairs(ModelA.emmeans, adjust = "bonferroni")
pairs.ModelA

#HIGH RESULTS
#contrast      estimate    SE  df z.ratio p.value
#eDNA - Efish     0.123 0.203 Inf   0.604  1.0000
#eDNA - Seine     0.457 0.204 Inf   2.241  0.0751
#Efish - Seine    0.334 0.238 Inf   1.406  0.4796

#LOW RESULTS

#contrast      estimate    SE  df z.ratio p.value
#eDNA - Efish     0.357 0.193 Inf   1.848  0.1940
#eDNA - Seine     0.749 0.195 Inf   3.843  0.0004 **
#Efish - Seine    0.392 0.235 Inf   1.667  0.2864



