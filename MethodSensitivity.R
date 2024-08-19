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

data.High <- data.frame(
  Category = c("eDNA", "Efish", "Seine"),
  Value = c(4.19, 3.71, 2.65),
  Upper_Error = c(2.18, 2.49, 1.71),
  Lower_Error = c(1.43, 1.49, 1.03)
)

ggplot(data.High, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", fill = "lightgray", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Value - Lower_Error, ymax = Value + Upper_Error), width = 0.2, color = "red") +
  labs(title = "", x = "Categories", y = "Values") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) + 
  theme(axis.text.x = element_text(size = 30),  # Increase font size for x-axis labels
        axis.text.y = element_text(size = 30),  # Increase font size for y-axis labels
        axis.title = element_text(size = 30)) +
  ylab("Number of Species Detected") +
  xlab("")


#LOW RESULTS

#Method rate    SE  df asymp.LCL asymp.UCL
#eDNA   5.67 0.847 Inf      3.97      8.11
#Efish  3.97 0.789 Inf      2.46      6.39
#Seine  2.68 0.526 Inf      1.68      4.29

data.Low <- data.frame(
  Category = c("eDNA", "Efish", "Seine"),
  Value = c(5.67, 3.97, 2.68),
  Upper_Error = c(2.44, 2.42, 1.61),
  Lower_Error = c(1.7, 1.33, 1)
)

ggplot(data.Low, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", fill = "lightgray", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Value - Lower_Error, ymax = Value + Upper_Error), width = 0.2, color = "red") +
  labs(title = "", x = "Categories", y = "Values") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) + 
  theme(axis.text.x = element_text(size = 30),  # Increase font size for x-axis labels
        axis.text.y = element_text(size = 30),  # Increase font size for y-axis labels
        axis.title = element_text(size = 30)) +
  ylab("Number of Species Detected") +
  xlab("")

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



