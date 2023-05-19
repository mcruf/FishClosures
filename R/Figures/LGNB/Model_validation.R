#####################################################
#                                                   #
#        Analyze the residuals of the model         #
#                                                   #
#####################################################




#~~~~~~~~~~~~~~~~~~~~~~~~
#
#       Spawners
#
#~~~~~~~~~~~~~~~~~~~~~~~~



#setwd("D:/PhD/Project III/Results/Spawning_HotSpots/YearQuarter/CheckConsistency/") #Spawner data
setwd("D:/PhD/Project III/Results/Recruits_HotSpot/YearQuarter/CheckConsistency/") #Spawner data


load("residuals_A0_survey.RData")
load("residuals_A1_survey.RData")
load("residuals_A2_both.RData")


# Check significance of the FIXED-effect parameters
fixed <- 1 - pchisq( 2 * (f2-f1), df=df )


# Check significance of the RANDOM-effect parameters
random <- 1 - pchisq( 2 * (f2.all-f1.all), df=df.all )
i <- (par1 != 0) ## consider non missing parameters



## Residuals
kst <- ks.test(residual, "pnorm"); kstp <- kst$p.value






#~~~~~~~~~~~~~~~~~~~~~~~~
#
#  Plotting with ggplot
#
#~~~~~~~~~~~~~~~~~~~~~~~~

dfpl <- data.frame(pl1 = as.vector(pl1$eta_density), pl2 = as.vector(pl2$eta_density)) #Random effects
dfixed <- data.frame(par1 = par1[i], par2 = par2[i]) #Fixed effects


# Plot random effects
#~~~~~~~~~~~~~~~~~~~~~~
ggplot(dfpl, aes(x=pl1,y=pl2)) +
  geom_point(alpha = 0.1, size=2.5) +
  geom_abline(col="#0073C2FF",size=1.4, linetype = "dashed") +
  theme_bw() +
  ggtitle("Spawners - A5") +
  ylab("Random Effects of survey data") +
  xlab("Random Effects of integrated data") +
  #geom_label(x = 2.8, y = -2.5, label = paste("p-val = ", round(random,3),sep=""), fontface = "bold", size=3.5) + #ks.test(residual,"pnorm")
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=11),
        axis.text.y = element_text(size=11,face="bold"),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=12,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=12,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        
        plot.margin = unit(c(1,1,1,1),"cm"))


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Model_validation")
ggsave("A5_RandomEff.png",dpi=350, width = 10, height = 10, units = "cm")



# Plot fixed effects
#~~~~~~~~~~~~~~~~~~~~~~
ggplot(dfixed, aes(x=par1,y=par2)) +
  geom_point(alpha = 0.3, size=2.5) +
  geom_abline(col="#0073C2FF",size=1.4, linetype = "dashed") +
  theme_bw() +
  ggtitle("Spawners - A5") +
  ylab("Fixed Effects of survey data") +
  xlab("Fixed Effects of integrated data") +
  #geom_label(x = 0.5, y = -5, label = paste("p-val = ", round(fixed,3),sep=""), fontface = "bold", size=3.5) + #ks.test(residual,"pnorm")
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=11),
        axis.text.y = element_text(size=11,face="bold"),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=12,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=12,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        
        plot.margin = unit(c(1,1,1,1),"cm"))





setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Model_validation")
ggsave("Sapwner_A5_FixedEff.png",dpi=350, width = 10, height = 10, units = "cm")



# Plot residuals
#~~~~~~~~~~~~~~~~

dfres <- data.frame(y = residual)

ggplot(dfres, aes(sample = y)) + 
  stat_qq(size = 1.6, alpha = 0.2) + 
  stat_qq_line(col="#0073C2FF",size=1, linetype = "dashed") +
  theme_bw() +
  ggtitle("Spawners - A5") +
  #geom_label(x = 1.7, y = -2.5, label = paste("p-val = ", round(kstp,3),sep=""), fontface = "bold", size=3.5) + #ks.test(residual,"pnorm")
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=11),
        axis.text.y = element_text(size=11,face="bold"),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=12,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=12,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        
        plot.margin = unit(c(1,1,1,1),"cm"))


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Model_validation")
ggsave("Spawner_A5_residuals.png",dpi=350, width = 10, height = 10, units = "cm")



#####################################################################################################



#~~~~~~~~~~~~~~~~~~~~~~~~
#
#       Recruits
#
#~~~~~~~~~~~~~~~~~~~~~~~~



setwd("D:/PhD/Project III/Results/Recruits_HotSpot/YearQuarter/CheckConsistency/") #Recruits data


#load("residuals_A0_survey.RData")
#load("residuals_A0_survey.RData")
load("residuals_A2_both.RData")


# Check significance of the FIXED-effect parameters
fixed <- 1 - pchisq( 2 * (f2-f1), df=df )


# Check significance of the RANDOM-effect parameters
random <- 1 - pchisq( 2 * (f2.all-f1.all), df=df.all )
i <- (par1 != 0) ## consider non missing parameters



## Residuals
kst <- ks.test(residual, "pnorm"); kstp <- kst$p.value






#~~~~~~~~~~~~~~~~~~~~~~~~
#
#  Plotting with ggplot
#
#~~~~~~~~~~~~~~~~~~~~~~~~

dfpl <- data.frame(pl1 = as.vector(pl1$eta_density), pl2 = as.vector(pl2$eta_density)) #Random effects
dfixed <- data.frame(par1 = par1[i], par2 = par2[i]) #Fixed effects


# Plot random effects
#~~~~~~~~~~~~~~~~~~~~~~
ggplot(dfpl, aes(x=pl1,y=pl2)) +
  geom_point(alpha = 0.1, size=2.5) +
  geom_abline(col="#0073C2FF",size=1.4, linetype = "dashed") +
  theme_bw() +
  ggtitle("Juveniles - A2") +
  ylab("Random Effects of survey data") +
  xlab("Random Effects of integrated data") +
  #geom_label(x = 2.8, y = -2.5, label = paste("p-val = ", round(random,3),sep=""), fontface = "bold", size=3.5) + #ks.test(residual,"pnorm")
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=11),
        axis.text.y = element_text(size=11,face="bold"),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=12,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=12,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        
        plot.margin = unit(c(1,1,1,1),"cm"))


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Model_validation")
ggsave("A2_RandomEff.png",dpi=350, width = 10, height = 10, units = "cm")



# Plot fixed effects
#~~~~~~~~~~~~~~~~~~~~~~
ggplot(dfixed, aes(x=par1,y=par2)) +
  geom_point(alpha = 0.3, size=2.5) +
  geom_abline(col="#0073C2FF",size=1.4, linetype = "dashed") +
  theme_bw() +
  ggtitle("Juveniles - A2") +
  ylab("Fixed Effects of survey data") +
  xlab("Fixed Effects of integrated data") +
  #geom_label(x = 0.5, y = -5, label = paste("p-val = ", round(fixed,3),sep=""), fontface = "bold", size=3.5) + #ks.test(residual,"pnorm")
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=11),
        axis.text.y = element_text(size=11,face="bold"),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=12,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=12,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        
        plot.margin = unit(c(1,1,1,1),"cm"))





setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Model_validation")
ggsave("Sapwner_A2_FixedEff.png",dpi=350, width = 10, height = 10, units = "cm")



# Plot residuals
#~~~~~~~~~~~~~~~~

dfres <- data.frame(y = residual)

ggplot(dfres, aes(sample = y)) + 
  stat_qq(size = 1.6, alpha = 0.2) + 
  stat_qq_line(col="#0073C2FF",size=1, linetype = "dashed") +
  theme_bw() +
  ggtitle("Juveniles - A2") +
  #geom_label(x = 1.7, y = -2.5, label = paste("p-val = ", round(kstp,3),sep=""), fontface = "bold", size=3.5) + #ks.test(residual,"pnorm")
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=11),
        axis.text.y = element_text(size=11,face="bold"),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=12,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=12,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        
        plot.margin = unit(c(1,1,1,1),"cm"))


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Model_validation")
ggsave("Juveniles_A2_residuals.png",dpi=350, width = 10, height = 10, units = "cm")

