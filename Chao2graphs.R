#Load data file (Page 1, 2 ect.)
Chao2.dat = read.csv(file.choose())
str(Chao2.dat)

library(dplyr)
library(ggplot2)

unique(Chao2.dat$PondID)

#Filter out by pond name - fill in "" with correct pond names
Pond1.dat = Chao2.dat %>% filter(PondID == "104.21")
Pond2.dat = Chao2.dat %>% filter(PondID == "127")
Pond3.dat = Chao2.dat %>% filter(PondID == "155")
Pond4.dat = Chao2.dat %>% filter(PondID == "27")

#Filter out by stringency type - no need to change names on anything
Pond1Low = Pond1.dat %>% filter(Strin == "Low")
Pond1Mid = Pond1.dat %>% filter(Strin == "Mid")
Pond1High = Pond1.dat %>% filter(Strin == "High")

Pond2Low = Pond2.dat %>% filter(Strin == "Low")
Pond2Mid = Pond2.dat %>% filter(Strin == "Mid")
Pond2High = Pond2.dat %>% filter(Strin == "High")

Pond3Low = Pond3.dat %>% filter(Strin == "Low")
Pond3Mid = Pond3.dat %>% filter(Strin == "Mid")
Pond3High = Pond3.dat %>% filter(Strin == "High")

Pond4Low = Pond4.dat %>% filter(Strin == "Low")
Pond4Mid = Pond4.dat %>% filter(Strin == "Mid")
Pond4High = Pond4.dat %>% filter(Strin == "High")

#create graph for each of the ponds/stringencies
Pond1LowGraph = ggplot(Pond1Low, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "Samples", y = "Number of Species") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond1Low$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond1Low$Cuttoff[1], linetype = "dashed", colour =  "black")

Pond1MidGraph = ggplot(Pond1Mid, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "Number of Species") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond1Mid$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond1Mid$Cuttoff[1], linetype = "dashed", colour =  "black")        

Pond1HighGraph = ggplot(Pond1High, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "Number of Species") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond1High$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond1High$Cuttoff[1], linetype = "dashed", colour =  "black")

#Second Pond
Pond2LowGraph = ggplot(Pond2Low, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "Samples", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond2Low$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond2Low$Cuttoff[1], linetype = "dashed", colour =  "black")

Pond2MidGraph = ggplot(Pond2Mid, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond2Mid$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond2Mid$Cuttoff[1], linetype = "dashed", colour =  "black")        

Pond2HighGraph = ggplot(Pond2High, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond2High$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond2High$Cuttoff[1], linetype = "dashed", colour =  "black")

#Third Pond
Pond3LowGraph = ggplot(Pond3Low, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "Samples", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond3Low$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond3Low$Cuttoff[1], linetype = "dashed", colour =  "black")

Pond3MidGraph = ggplot(Pond3Mid, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond3Mid$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond3Mid$Cuttoff[1], linetype = "dashed", colour =  "black")        

Pond3HighGraph = ggplot(Pond3High, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond3High$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond3High$Cuttoff[1], linetype = "dashed", colour =  "black")

#Last pond yay!
Pond4LowGraph = ggplot(Pond4Low, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "Samples", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond4Low$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond4Low$Cuttoff[1], linetype = "dashed", colour =  "black")

Pond4MidGraph = ggplot(Pond4Mid, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond4Mid$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond4Mid$Cuttoff[1], linetype = "dashed", colour =  "black")        

Pond4HighGraph = ggplot(Pond4High, aes(Samples,Chao2mean)) + 
  geom_point(colour = "black", size = 2) +
  geom_errorbar(aes(ymin = Chao2mean - Chao2SD, ymax = Chao2mean + Chao2SD, width = 0.2)) +
  labs(x = "", y = "") + 
  theme_classic() +
  theme(legend.position = "top",legend.justification = "right") +
  theme(panel.grid = element_blank()) +
  ylim(c(0,20)) +
  guides(color = FALSE) +
  geom_point(aes(Samples,Smean), shape = 1, fill = "transparent", colour ="red", size = 2) +
  geom_hline(yintercept = Pond4High$Detectedspecies[1], linetype = "dashed", colour = "green") +
  geom_vline(xintercept = Pond4High$Cuttoff[1], linetype = "dashed", colour =  "black")

#arrange graphs into one MEGA GRAPH
library(gridExtra)
library(grid)
library(cowplot)

#Arrange grids into correct pattern 4x3
Tgrid = grid.arrange(Pond1HighGraph,Pond2HighGraph,Pond3HighGraph,Pond4HighGraph,
                     Pond1MidGraph,Pond2MidGraph,Pond3MidGraph,Pond4MidGraph,
                     Pond1LowGraph,Pond2LowGraph,Pond3LowGraph,Pond4LowGraph,
             ncol = 4, nrow = 3
             )









