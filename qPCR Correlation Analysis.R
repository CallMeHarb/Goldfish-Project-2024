library(tidyr)
library(dplyr)

#Data cleaning

#Data Cleaning Notes for pond 89
#CRUCIAL NOTE: FISH WITH MISSING TL_mm ESTIMATES ACTUALLY ARE MISSING FL_mm ESTIMATES
#THIS WAS LIKELY A RECORDING ERROR
#The FL being an actual TL measurement only occurs for fish above 70mm
#Below 70mm, if they are missing a TL measurement, the FL is likely accurate - this is mostly just YoY
#FL estimates in original file for fish > 70 mm were moved over to the 'FL' Column prior to loading in R

#Note: Fish from 104 were likely originally measured using standard length, but forgotten to recod
#this applies ONLY to the Mandrak preserved specimens
#'True' fork length values for these specimens were imputed from fish measured by DFO and from pictures taken by the Mandrak lab
#Weights for the pictures and for SOME of the Mandrak preserved samples were also imputed

#Load file - "test"
Fish.dat = read.csv(file.choose())
str(Fish.dat)

#89 Cleaning
#####
#Let's clean pond 89 first

#Filter out pond 89 from dataset:
Pond.89.dat = filter(Fish.dat, Location == "89")
str(Pond.89.dat)

#Now we want to remove the old Pond 89 data from the original file
#We will r-bind the new, manipulated/cleaned data back into the dataframe at the end
Fish.dat = filter(Fish.dat, Location != "89")
unique(Fish.dat$Location)


#We now have to convert all TL measurements into FL measurements
conversion.model = lm(FL_mm ~ TL_mm, data = Pond.89.dat)
summary(conversion.model)

#Predict new FL based off of TL -> FL model for ALL fish
predict.FL.pond89 = predict(conversion.model, Pond.89.dat)

#cbind predicted FLs to the Pond 89 dataframe
Pond.89.dat = cbind(Pond.89.dat, predict.FL.pond89)

#For all fish missing a FL, use the predicted one instead
#Note that this code KEEPS FL lengths if originally found in data
Pond.89.dat$FL_mm[is.na(Pond.89.dat$FL_mm)] <- Pond.89.dat$predict.FL.pond89[is.na(Pond.89.dat$FL_mm)] #if FL is NA use the predicted FL
Pond.89.dat$FL_mm

#Now we have to erase the 'predicted' FL column so we can re-bind to original dataframe
Pond.89.dat$predict.FL.pond89 = NULL #delete character vector predict.FL.pond89

#Now we re-bind back into the original total datafile
Fish.dat = rbind(Fish.dat, Pond.89.dat)
unique(Fish.dat$Location)

#Note: We had already binned size classes for 'imputed' FL fish in excel
#Otherwise, we would need to re-classify all of the 'size classes' for fish with imputed FL estimates


#104.21 Cleaning
#####
#Data cleaning for pond 104
#Lengths were taken in total length (TL), but binned afterwards by DFO based on fork length (FL)
#Need to come up with a TL -> FL conversion equation
#Can then use Original datafile to generate LW curve
#Then, can use LW curve to estimate mass values for fish from picture file!

#We need to filter out Pond 104 from 2021 
#Filter out pond 104 from dataset:
Pond.104.2021.dat = filter(Fish.dat, Location == "104.2021")
str(Pond.104.2021.dat)

#Now we want to remove the old Pond 104 data from the original file
#We will r-bind the new, manipulated/cleaned data back into the dataframe at the end
Fish.dat = filter(Fish.dat, Location != "104.2021")
unique(Fish.dat$Location)

#Now, we make the same TL to FL conversion model using the fish Ian measured on ImageJ
conversion.model.104 = lm(FL_mm ~ TL_mm, data = Pond.104.2021.dat)
summary(conversion.model.104)

plot(Pond.104.2021.dat$FL_mm,Pond.104.2021.dat$TL_mm)

#Use coefficients from this relationship to predict fork length for fish from original sampling
Pond.104.2021.FLpredict = predict(conversion.model.104, Pond.104.2021.dat)

#cbind predicted FLs to the Pond 104 dataframe
Pond.104.2021.dat = cbind(Pond.104.2021.dat, Pond.104.2021.FLpredict)

#For all fish missing a FL, use the predicted one instead
#Note that this code KEEPS FL lengths if originally found in data
Pond.104.2021.dat$FL_mm[is.na(Pond.104.2021.dat$FL_mm)] <- Pond.104.2021.dat$Pond.104.2021.FLpredict[is.na(Pond.104.2021.dat$FL_mm)] #if FL is NA use the predicted FL
Pond.104.2021.dat$FL_mm

#Check using a graph, should approximately follow a 1:1 line
plot(Pond.104.2021.FLpredict, Pond.104.2021.dat$FL_mm)
x = seq(20,160,1)
y = seq(20,160,1)
lines(x,y)


#Now we have to erase the 'predicted' FL column so we can re-bind to original dataframe
Pond.104.2021.dat$Pond.104.2021.FLpredict = NULL #delete character vector predict.FL.pond89

#Bind back onto dataframe
Fish.dat = rbind(Fish.dat, Pond.104.2021.dat)
unique(Fish.dat$Location)


#LW modeling
#####

#Now we make the LW model

LWMod.104 <- lm(log(Pond.104.2021.dat$Weight_g)~log(Pond.104.2021.dat$FL_mm))
summary(LWMod.104)

plogW <- predict(LWMod.104, Pond.104.2021.dat) #prediction log mass EVERYTHING - even ones without mass
predMass <- exp(plogW) #name the guessed masses


Pond.104.2021.dat$PredMass <- predMass #bind it to the dataframe
Pond.104.2021.dat$Mass = Pond.104.2021.dat$Weight_g
Pond.104.2021.dat$Weight_g[is.na(Pond.104.2021.dat$Weight_g)] <- Pond.104.2021.dat$PredMass[is.na(Pond.104.2021.dat$Weight_g)] #if mass is NA use the predicted mass


#Check predictions with graph, looks like model is overestimating weight of larger size classes
FL.predict = seq(20,160,1)
Weight_g.predict = -12.60536 + 3.42137*log(FL.predict)

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,3), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = "Mass (g)",
     ylab = "FL (mm)", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,600), ylim = c(0,160))
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
lines(exp(Weight_g.predict),FL.predict, col = "black", lty = 1, lwd = 2)
points(Pond.104.2021.dat$Weight_g, Pond.104.2021.dat$FL_mm, pch = 16, col = 'red')
points(Pond.104.2021.dat$Mass, Pond.104.2021.dat$FL_mm, pch = 16, col = 'black')

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,3), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = "Mass (g)",
     ylab = "FL (mm)", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,150), ylim = c(0,200))
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
lines(exp(Weight_g.predict),FL.predict, col = "black", lty = 1, lwd = 2)
points(Pond.104.2021.dat$Weight_g, Pond.104.2021.dat$FL_mm, pch = 16, col = 'red')
points(Pond.104.2021.dat$Mass, Pond.104.2021.dat$FL_mm, pch = 16, col = 'black')

#Main Analysis
##### 
#Analysis

#Let's examine how much L/W curves differ for the different populations
#Just make sure dataset is complete
unique(Fish.dat$Location)

#Also filter pond 105 because only 1 fish captured there, can't make a 'curve'
library(dplyr)
FishMass.dat = filter(Fish.dat, Location != "105")

#ABOVE CODE IS CAUSING PROBLEMS SINCE WE CHANGE THE MAIN FILE NAME - TEMP FIX BELOW
#FishMass.dat = Fish.dat

LW.model = lm(log(Weight_g) ~ Location + log(FL_mm) + Location:log(FL_mm), data = FishMass.dat)
summary(LW.model)

LW.model.nopond = lm(log(Weight_g) ~ Location + log(FL_mm), data = FishMass.dat)

AIC(LW.model.nopond) #1396.313
AIC(LW.model) #122.2578

anova(LW.model.nopond,LW.model)
#  Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
#1   1374 220.88                                 
#2   1368 161.69  6    59.193 83.47 < 2.2e-16 ***


#Now, we will want to creat L/W lines for all other ponds to superimpose onto the graph above for 104
#Need this for a frame of comparison
#We will use 'for loops' and the package emmeans to do this quickly
library(emmeans)

#Now we just need this for reference to get the different pond location names
unique(FishMass.dat$Location)

#Set up vector to predict mass from different fork length values
FL.predict = seq(20,200,1)

#Set up iterative loops to predict Mass from given FL values for each pond
Weight.88.predict = c()
for(i in 1:length(FL.predict)){
  
  x = emmeans(LW.model, ~ 1, var = "FL_mm", at = list(Location = c("88"), FL_mm = c(i+19)))
  x = summary(x)
  y = exp(x$emmean)
  Weight.88.predict[i] = y
}

Weight.127.predict = c()
for(i in 1:length(FL.predict)){
  
  x = emmeans(LW.model, ~ 1, var = "FL_mm", at = list(Location = c("127"), FL_mm = c(i+19)))
  x = summary(x)
  y = exp(x$emmean)
  Weight.127.predict[i] = y
}

Weight.5A.predict = c()
for(i in 1:length(FL.predict)){
  
  x = emmeans(LW.model, ~ 1, var = "FL_mm", at = list(Location = c("5A"), FL_mm = c(i+19)))
  x = summary(x)
  y = exp(x$emmean)
  Weight.5A.predict[i] = y
}

Weight.104.2022.predict = c()
for(i in 1:length(FL.predict)){
  
  x = emmeans(LW.model, ~ 1, var = "FL_mm", at = list(Location = c("104.2022"), FL_mm = c(i+19)))
  x = summary(x)
  y = exp(x$emmean)
  Weight.104.2022.predict[i] = y
}

Weight.124.predict = c()
for(i in 1:length(FL.predict)){
  
  x = emmeans(LW.model, ~ 1, var = "FL_mm", at = list(Location = c("124"), FL_mm = c(i+19)))
  x = summary(x)
  y = exp(x$emmean)
  Weight.124.predict[i] = y
}

Weight.89.predict = c()
for(i in 1:length(FL.predict)){
  
  x = emmeans(LW.model, ~ 1, var = "FL_mm", at = list(Location = c("89"), FL_mm = c(i+19)))
  x = summary(x)
  y = exp(x$emmean)
  Weight.89.predict[i] = y
}

Weight.104.2021.predict = c()
for(i in 1:length(FL.predict)){
  
  x = emmeans(LW.model, ~ 1, var = "FL_mm", at = list(Location = c("104.2021"), FL_mm = c(i+19)))
  x = summary(x)
  y = exp(x$emmean)
  Weight.104.2021.predict[i] = y
}

#Add the curves to the graphs above from pond 104

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,3), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = "Mass (g)",
     ylab = "FL (mm)", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,200), ylim = c(0,200))
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
lines((Weight.124.predict),FL.predict, col = "gold", lty = 2, lwd = 2)
lines((Weight.88.predict),FL.predict, col = "green", lty = 2, lwd = 2)
lines((Weight.127.predict),FL.predict, col = "blue", lty = 2, lwd = 2)
lines((Weight.104.2022.predict),FL.predict, col = "brown", lty = 2, lwd = 2)
lines((Weight.5A.predict),FL.predict, col = "orange", lty = 2, lwd = 2)
lines((Weight.89.predict),FL.predict, col = "purple", lty = 2, lwd = 2)
lines((Weight.104.2021.predict),FL.predict, col = "black", lty = 2, lwd = 2)



###########################################################################################
#Allometric Scaling and eDNA concentrations
#####

#Now we will try to calculate allometrically scaled mass values for a pond

#Load data for fish counts from DFO - 'Depletion_totalabund'
fishcounts.dat = read.csv(file.choose())
str(fishcounts.dat)

#Integrating Allometric scaling into relationships between eDNA and goldfish abundance

goldfishcounts.dat = filter(fishcounts.dat, Species == "Goldfish ")
goldfishcounts.dat
#No Size class 1 for any goldfish
goldfishcounts.dat$Adult1 = NULL

#Now we need to transpose the goldfish count dataframe
#Do this using the gather + spread function in tidyr
#First, we subset only the columns that we want to retain/organize
library(tidyr)
subset.goldfishcounts = goldfishcounts.dat[,c(1,4:11)]
trans.goldfishcounts = subset.goldfishcounts %>% gather(Size.class,N,-Pond_ID)
unique(trans.goldfishcounts$Pond_ID)


#Load the length-weight data for each pond population - Unsure what file this is refering to?
#PondMass.dat = read.csv(file.choose())
str(PondMass.dat)
#Remove NAs, because R doesn't play nice with them
PondMass.dat = Fish.dat
PondMass.dat = PondMass.dat[complete.cases(PondMass.dat[,11]), ] 
unique(PondMass.dat$Location)


#Create dataframe external to function to store results
#Create dataframe with repeating values of b from 0 to 200
#Need an extra column to hold the Pond ID values
allo.dataframe <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = 0,
                          ncol = 202))
allo.dataframe[1,]=NA
allo.column.names = c(paste0("b", -100:100), "Pond_ID")
colnames(allo.dataframe) = allo.column.names

#Now we generate allometrically scaled biomass values for each pond

for(k in unique(PondMass.dat$Location)){
sub1 = filter(PondMass.dat, Location == k)

#Create an empty dataframe within the function with same structure as allo.dataframe
df <- data.frame(matrix(NA,    # Create empty data frame
                        nrow = 0,
                        ncol = 202))
df[1,]=NA
df.column.names = c(paste0("b", -100:100), "Pond_ID")
colnames(df) = df.column.names

  for(i in 1:201){
  #Create dataframe internal to the function to store summary statistics calculated for each size class
  #We can go back and save this in a 'permanent' repository if needed later
  summary.d = data.frame(Size.class = character(), size.class.mean = integer(), stringsAsFactors=FALSE)

    for(j in unique(sub1$Size.class)){
      sub2 = filter(sub1, Size.class == j)    #subset size class in function
      size.class.mean = mean(sub2$Weight_g^((i-101)/100)) #Calculate mean ASM for each size class for a given value of 'b'
  
      #Now, create a dataframe internal to the function with the same structure as our external 'summary' dataframe
      sim.d = data.frame(Size.class = character(), size.class.mean = integer(), stringsAsFactors=FALSE)
      #Copy values of size class, and mean ASM into external summary dataframe 
      sim.d[1,1] = sub2[1,14]
      sim.d$size.class.mean = size.class.mean
  
      summary.d = rbind(summary.d,sim.d)
    }
 
  #Just for internal checking 
  #summary.d  
  #str(summary.d)


  #Now, for our example, let's filter out only pond 89
  Pond.Counts = filter(trans.goldfishcounts, Pond_ID == k)

  #Remove all rows with NAs from the dataframe
  Pond.Counts = Pond.Counts[complete.cases(Pond.Counts), ]

  #Now we combine counts for each size class with the allometric scaling data we processed above
  Pond.Allo.Biomass = left_join(summary.d,Pond.Counts, by = 'Size.class')

  #Calculate allometrically scaled mass values by multiplying mean ASM by size class N 
  Pond.Allo.Biomass$ASM.size.category = Pond.Allo.Biomass$size.class.mean*Pond.Allo.Biomass$N
  Pond.Allo.Biomass = Pond.Allo.Biomass[complete.cases(Pond.Allo.Biomass),]
  
  #Calculate total pond ASM by summing ASM values across size classes
  ASM = sum(Pond.Allo.Biomass$ASM.size.category)
 
  #This is going to fill the first row of df with the allometrically scaled mass values
  df[1,i] = ASM

  #Put the pond ID in that last column
  df[1,202] = Pond.Allo.Biomass$Pond_ID[1]


}
allo.dataframe = rbind(allo.dataframe,df)


}  
  
allo.dataframe[,c(1:10,190:202)] 
unique(allo.dataframe$Pond_ID)
allo.dataframe




#Now we just need to delete the first row of NAs
allo.dataframe = allo.dataframe[c(2:(length(allo.dataframe[,1]))),] 
unique(allo.dataframe$Pond_ID)

#eDNA to allometric scaling
#####

#Load eDNA data 
eDNA.dat = read.csv(file.choose())
str(eDNA.dat)

#Join eDNA data to ASM dataframe
eDNA.dat$Pond_ID = eDNA.dat$Pond
unique(eDNA.dat$Pond_ID)

#Looking at relationship visually
final.dat = left_join(eDNA.dat,allo.dataframe, by = 'Pond_ID')
final.dat = distinct(final.dat)
plot(final.dat$'b-32'/final.dat$Surface.Area..hectare.,final.dat$Mean.eDNA..copy.L.)
plot(final.dat$b0/final.dat$Surface.Area..hectare.,final.dat$Mean.eDNA..copy.L.)
plot(final.dat$b100/final.dat$Surface.Area..hectare.,final.dat$Mean.eDNA..copy.L.)


final.dat$example = final.dat$b100/final.dat$Surface.Area..hectare.
library(glmmTMB)
final.dat$Mean.eDNA..copy.L. = round(final.dat$Mean.eDNA..copy.L., digits = 0)

library(MASS)

m1 <- glm.nb(Mean.eDNA..copy.L. ~ example, data= final.dat)
m2 <- lm(Mean.eDNA..copy.L. ~ example, data= final.dat)
m3 = lm(Mean.eDNA..copy.L. ~ I(final.dat$b0/final.dat$Surface.Area..hectare.), data = final.dat)

summary(m1)
#Looks like we don't have enough data to support a negative binomial
#Probably need to use a truncated gaussian then?

summary(m3)



ASM_AIC.Golfish <- c()
for(i in 1:201){
  ASM_AIC.Golfish[i] <- AIC(lm(final.dat$Mean.eDNA..copy.L. ~ (final.dat[,i + 9]/final.dat$Surface.Area..hectare.))) 
}   

ASM_AIC.Golfish

ASM_AIC.Golfish.noSA <- c()
for(i in 1:201){
  ASM_AIC.Golfish.noSA[i] <- AIC(lm(final.dat$Mean.eDNA..copy.L. ~ final.dat[,i + 9])) 
}   

ASM_AIC.Golfish.noSA



b.value = seq(-1,1,0.01)

Final.b = data.frame(ASM_AIC.Golfish,b.value)
str(Final.b)

library(ggplot2)

optimal.curve = ggplot(data = Final.b, aes(b.value, ASM_AIC.Golfish)) +
  geom_line(linewidth = 1.2) +
  theme_classic() +
  xlab("b Value") +
  ylab("Model AIC") + 
  scale_x_continuous(limits = c(-1,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(203,220), expand = c(0, 0)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text = element_text(size = 12.5), plot.margin = margin(30, 30, 30, 30)) +
  geom_vline(xintercept = -0.32, colour = 'red', linewidth = 0.5, linetype = "dashed") + 
  geom_segment(x = -0.57, xend = -0.51, y = 206.1676, yend = 206.1676, linewidth = 0.6) +
  geom_segment(x = -0.16, xend = -0.10, y = 206.1170, yend = 206.1170, linewidth = 0.6) +
  geom_segment(x = -0.54, xend = -0.54, y = 206.1676, yend = 0, linewidth = 0.6, linetype = "dashed") +
  geom_segment(x = -0.13, xend = -0.13, y = 206.1170, yend = 0, linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = -0.14, y = 215, label = "Optimal b = -0.32", color = "black", size = 4)
optimal.curve

optimal.curve.noSA = ggplot(data= Final.b, aes(b.value, ASM_AIC.Golfish.noSA)) +
  geom_line(linewidth = 1.2) +
  theme_classic() +
  xlab("b Value") +
  ylab("Model AIC") + 
  scale_x_continuous(limits = c(-1,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(203,225), expand = c(0, 0)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text = element_text(size = 12.5), plot.margin = margin(30, 30, 30, 30)) +
  geom_vline(xintercept = -0.33, colour = 'red', linewidth = 0.5, linetype = "dashed") + 
  geom_segment(x = -0.57, xend = -0.51, y = 205.4072, yend = 205.4072, linewidth = 0.6) +
  geom_segment(x = -0.17, xend = -0.11, y = 205.5584, yend = 205.5584, linewidth = 0.6) +
  geom_segment(x = -0.54, xend = -0.54, y = 205.4072, yend = 0, linewidth = 0.6, linetype = "dashed") +
  geom_segment(x = -0.14, xend = -0.14, y = 205.5584, yend = 0, linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = -0.14, y = 215, label = "Optimal b = -0.33", color = "black", size = 4)
optimal.curve.noSA

density.per.ha = final.dat$b0/final.dat$Surface.Area..hectare.
mean.body.size = final.dat$b100/final.dat$b0
pondID = final.dat$Pond
PondSA = final.dat$Surface.Area..hectare.

Explanation.dataframe = data.frame(density.per.ha,mean.body.size,pondID,PondSA)

#Strong relationship between mean size and density

Densityvsbodysize = ggplot(data = Explanation.dataframe, aes(density.per.ha, log(mean.body.size))) +
  geom_point() +
  theme_classic() +
  xlab("Density per Hectare") +
  ylab("Log of Mean Body Size") + 
  scale_x_continuous(limits = c(0,100000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,8), expand = c(0, 0)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text = element_text(size = 12), plot.margin = margin(30, 30, 30, 30)) + 
  geom_smooth(method = "lm")
Densityvsbodysize

dvb = lm(density.per.ha ~ log(mean.body.size), data = Explanation.dataframe)
summary(dvb)

#No pattern for SA and body size
SurfaceAreavsbodysize = ggplot(data = Explanation.dataframe, aes(PondSA, log(mean.body.size))) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Pond Surface Area") +
  ylab("Log of Mean Body Size") + 
  scale_x_continuous(limits = c(0,1.3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,8), expand = c(0, 0)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), axis.text = element_text(size = 13), plot.margin = margin(30, 30, 30, 30)) 
SurfaceAreavsbodysize

svb = lm(PondSA ~ mean.body.size, data = Explanation.dataframe)
summary(svb)

mean.size.vs.density = lm(log(mean.body.size)~density.per.ha, data = Explanation.dataframe)
summary(mean.size.vs.density)



optimal.b.position = which.min(Final.b$ASM_AIC.Golfish)
optimal.b = Final.b$b.value[optimal.b.position]


 #Funny testing
#####
test = data.frame(matrix(ncol = 200, nrow = 10))
test1 = data.frame(matrix(ncol = 1, nrow = 1))

for (i in 1:10){
  test1 = final.dat[i,]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test1 = test1[-c(1)]
  test[i,] = test1
  test1 = data.frame(matrix(ncol = 1, nrow = 1))
}

test = na.omit(test)

test[7,] = rbind(1:200)
testa = test[1,]
testa = as.numeric(testa)
testb = test[2,]
testa = as.numeric(testb)
testc = test[3,]
testa = as.numeric(testc)
testd = test[4,]
testa = as.numeric(testd)
teste = test[5,]
testa = as.numeric(teste)
testf = test[6,]
testa = as.numeric(testf)
testz = test[7,]
testz = as.numeric(testz)

par(plt = c(0.1, 0.9, 0.1, 0.9)) # Set plot region size

# Plotting
plot(testa, testz, col = "red")
points(testb, testz, col = "blue")
points(testc, testz, col = "green")
points(testd, testz, col = "yellow")
points(teste, testz, col = "purple")
points(testf, testz, col = "brown")


#####

goldfish.ASM = data.frame(final.dat$`b-32`/final.dat$Surface.Area..hectare.,final.dat$Mean.eDNA..copy.L.)
goldfish.ASM$ASM = goldfish.ASM$final.dat..b.32..final.dat.Surface.Area..hectare.
goldfish.ASM$final.dat..b.32..final.dat.Surface.Area..hectare. = NULL
goldfish.ASM$eDNA = goldfish.ASM$final.dat.Mean.eDNA..copy.L.
goldfish.ASM$final.dat.Mean.eDNA..copy.L. = NULL

goldfish.ASM.abund = data.frame(final.dat$`b0`/final.dat$Surface.Area..hectare.,final.dat$Mean.eDNA..copy.L.)
goldfish.ASM.abund$ASM = goldfish.ASM.abund$final.dat.b0.final.dat.Surface.Area..hectare.
goldfish.ASM.abund$final.dat.b0.final.dat.Surface.Area..hectare. = NULL
goldfish.ASM.abund$eDNA = goldfish.ASM.abund$final.dat.Mean.eDNA..copy.L.
goldfish.ASM.abund$final.dat.Mean.eDNA..copy.L. = NULL

goldfish.ASM.biomass = data.frame(final.dat$`b100`/final.dat$Surface.Area..hectare.,final.dat$Mean.eDNA..copy.L.)
goldfish.ASM.biomass$ASM = goldfish.ASM.biomass$final.dat.b100.final.dat.Surface.Area..hectare.
goldfish.ASM.biomass$final.dat.b100.final.dat.Surface.Area..hectare. = NULL
goldfish.ASM.biomass$eDNA = goldfish.ASM.biomass$final.dat.Mean.eDNA..copy.L.
goldfish.ASM.biomass$final.dat.Mean.eDNA..copy.L. = NULL

#Generate predictions for line
optimal.model = lm(eDNA ~ ASM, data = goldfish.ASM)
summary(optimal.model)

optimal.model.abundance = lm(eDNA ~ ASM, data = goldfish.ASM.abund)
summary(optimal.model.abundance)

optimal.model.biomass = lm(eDNA ~ ASM, data = goldfish.ASM.biomass)
summary(optimal.model.biomass)



ASM.sim = seq(3267.485,80000,1000)
ASM.opt = seq(5523.167,100000,1000)
ASM.bio = seq(0,800000,50000)
eDNA.estimates.optimal = optimal.model$coefficients[1] + ASM.sim*(optimal.model$coefficients[2])
eDNA.estimates.abundance = abundance.model$coefficients[1] + ASM.opt*(abundance.model$coefficients[2])
eDNA.estimates.biomass = biomass.model$coefficients[1] + ASM.bio*(biomass.model$coefficients[2])

options(scipen = 99999999)


par(mfrow = c(1,3))

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,1), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = expression(paste("ASM"^"-0.32")),
     ylab = "eDNA (Thousands Copies/L)", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,80000), ylim = c(0,600) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
points(goldfish.ASM$ASM, goldfish.ASM$eDNA/1000, col = "black", pch = 16, cex = 1.25)
lines(ASM.sim, eDNA.estimates.optimal/1000, col = "black", lty = 1, lwd = 2)

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,1), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = expression(paste(Abundance)),
     ylab = "", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,100000), ylim = c(0,600) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
points(final.dat$b0/final.dat$Surface.Area..hectare., goldfish.ASM$eDNA/1000, col = "black", pch = 16, cex = 1.25)
lines(ASM.opt, eDNA.estimates.abundance/1000, col = "black", lty = 1, lwd = 2)

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,1), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = expression(paste(Biomass (kg))),
     ylab = "", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,800000), ylim = c(0,600) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
points(final.dat$b100/final.dat$Surface.Area..hectare., goldfish.ASM$eDNA/1000, col = "black", pch = 16, cex = 1.25)
lines(ASM.bio, eDNA.estimates.biomass/1000, col = "black", lty = 1, lwd = 2)






ASM_AIC.Golfish.nosurfacecorrection <- c()
for(i in 1:201){
  ASM_AIC.Golfish.nosurfacecorrection[i] <- AIC(lm(final.dat$Mean.eDNA..copy.L. ~ (final.dat[,i + 9]))) 
}  

b.value = seq(-100,100,1)
no.surface.correction = data.frame(cbind(ASM_AIC.Golfish.nosurfacecorrection,b.value))


optimal.model.no.SACorrection = lm(final.dat$Mean.eDNA..copy.L. ~ final.dat$'b-33')
summary(optimal.model.no.SACorrection)


#Does correcting for Surface Area make it significantly worse?
#AIC values within '2' are statistically equivalent in terms of explanatory power
AIC(optimal.model.no.SACorrection)
AIC(optimal.model)

#Significant improvement by NOT correcting for pond Surface Area
#When writing, show plot as well (in appendices) demonstrating no effect between mean body size and pond surface area



goldfish.ASM.noSACorr = data.frame(final.dat$`b-32`,final.dat$Mean.eDNA..copy.L.)
goldfish.ASM.noSACorr$ASM = goldfish.ASM.noSACorr$final.dat..b.32.
goldfish.ASM.noSACorr$final.dat..b.32. = NULL
goldfish.ASM.noSACorr$eDNA = goldfish.ASM.noSACorr$final.dat.Mean.eDNA..copy.L.
goldfish.ASM.noSACorr$final.dat.Mean.eDNA..copy.L. = NULL

goldfish.ASM.noSACorr.abund = data.frame(final.dat$`b0`,final.dat$Mean.eDNA..copy.L.)
goldfish.ASM.noSACorr.abund$ASM = goldfish.ASM.noSACorr.abund$final.dat.b0
goldfish.ASM.noSACorr.abund$final.dat.b0 = NULL
goldfish.ASM.noSACorr.abund$eDNA = goldfish.ASM.noSACorr.abund$final.dat.Mean.eDNA..copy.L.
goldfish.ASM.noSACorr.abund$final.dat.Mean.eDNA..copy.L. = NULL

goldfish.ASM.noSACorr.biomass = data.frame(final.dat$`b100`,final.dat$Mean.eDNA..copy.L.)
goldfish.ASM.noSACorr.biomass$ASM = goldfish.ASM.noSACorr.biomass$final.dat.b100
goldfish.ASM.noSACorr.biomass$final.dat.b100 = NULL
goldfish.ASM.noSACorr.biomass$eDNA = goldfish.ASM.noSACorr.biomass$final.dat.Mean.eDNA..copy.L.
goldfish.ASM.noSACorr.biomass$final.dat.Mean.eDNA..copy.L. = NULL


options(scipen = 99999999)

#Generate predictions for line
optimal.model.noPondSA = lm(eDNA ~ ASM, data = goldfish.ASM.noSACorr)
summary(optimal.model.noPondSA)

abundance.model.noPondSA = lm(eDNA ~ ASM, data = goldfish.ASM.noSACorr.abund)
summary(abundance.model.noPondSA)

biomass.model.noPondSA = lm(eDNA ~ ASM, data = goldfish.ASM.noSACorr.biomass)
summary(biomass.model.noPondSA)


ASM.sim.noPondSA = seq(0,15000,100)
ASM.sim.noPondSA.abundance = seq(721.84,20000,100)
ASM.sim.noPondSA.biomass = seq(0,150000,1000)
#ASM.sim = ASM.sim[5:length(ASM.sim)] #This trims it to only produce positive eDNA values for the line
eDNA.estimates.noPondSA = optimal.model.noPondSA$coefficients[1] + ASM.sim.noPondSA*(optimal.model.noPondSA$coefficients[2])
eDNA.estimates.noPondSA.abundance = abundance.model.noPondSA$coefficients[1] + ASM.sim.noPondSA.abundance*(abundance.model.noPondSA$coefficients[2])
eDNA.estimates.noPondSA.biomass = biomass.model.noPondSA$coefficients[1] + ASM.sim.noPondSA.biomass*(biomass.model.noPondSA$coefficients[2])


par(mfrow = c(1,3))

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,1), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = expression(paste("ASM"^"-0.32")),
     ylab = "eDNA (Thousands Copies/L)", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,15000), ylim = c(0,600) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
points(goldfish.ASM.noSACorr$ASM, goldfish.ASM.noSACorr$eDNA/1000, col = "black", pch = 16, cex = 1.35)
lines(ASM.sim.noPondSA, eDNA.estimates.noPondSA/1000, col = "black", lty = 1, lwd = 2)

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,1), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = expression(paste(Abundance)),
     ylab = "", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,20000), ylim = c(0,600) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
points(final.dat$b0, goldfish.ASM.noSACorr$eDNA/1000, col = "black", pch = 16, cex = 1.35)
lines(ASM.sim.noPondSA.abundance, eDNA.estimates.noPondSA.abundance/1000, col = "black", lty = 1, lwd = 2)

par(mar = c(5,5.6,2.1,1.1))
par(mgp = c(3,.8,0))
par(las = 1)
par(bty = "n")
plot(c(0,1), c(5,20), xaxt = "n", yaxt = "n", type = "n", xlab = expression(paste(Biomass (kg))),
     ylab = "", las = 1, cex.lab = 1.5, cex.main = 2, xlim=c(0,150), ylim = c(0,600) )
axis(1, pos = 0, col = "black", cex.axis = 1.5)
axis(2, pos = 0.0, col = "black", cex.axis = 1.5)
points(final.dat$b100/1000, goldfish.ASM.noSACorr$eDNA/1000, col = "black", pch = 16, cex = 1.35)
lines(ASM.sim.noPondSA.biomass/1000, eDNA.estimates.noPondSA.biomass/1000, col = "black", lty = 1, lwd = 2)







