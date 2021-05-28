library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rstatix)
library(agricolae)
library(lsmeans)
library(gridExtra)
require(cowplot)
library(lme4)
library(ggplot2)


df <- read_xlsx ("Regrow.xlsx", sheet = "Sheet3")
df$OM <- as.factor(df$OM)
df$Year <- as.factor(df$Year)



# filter with your condition
# I chose to work with IL and PA silt loam soil with 2% soil organic matter

IL <- filter(df, Site == "IL", Texture == "Silt Loam", OM == 2)

PA <- filter(df, Site == "PA", Texture == "Silt Loam", OM == 2)


#fitting anova
fit_IL <- lm(Total_GWP ~Crop_rotation* Cover_crop*Tillage , data=IL)
anova(fit_IL)
(HSD.test(fit_IL, "Tillage")) #Tukey test for multiple means comparison

fit_PA <- lm(Total_GWP ~Crop_rotation* Cover_crop*Tillage , data=PA)
anova(fit_PA)
(HSD.test(fit_PA, "Tillage"))



#Normality test
hist(residuals(fit_IL),
     col="darkgray")
plot(fitted(fit_IL),
     residuals(fit_IL))



#IL ggplot

Fig_IL <- ggplot(IL, aes(x = Crop_rotation, y = Total_GWP, group = Cover_crop, col = Cover_crop)) + 
  stat_summary(fun.y = "mean", geom = "line")+
  stat_summary(fun.y = "mean", geom = "point")+
  facet_grid( .~ Tillage)+
  labs(
    y = "Total GWP\n(tonne CO2e/ac/y)") + 
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),
       axis.title.x = element_blank(),
        legend.position = "right",
        panel.grid = element_blank(), #to remove the background grid
        panel.border = element_rect(colour = "black", fill=NA, size=1)) #to add frame to the graph))


#PA ggplot

Fig_PA <- ggplot(PA, aes(x = Crop_rotation, y = Total_GWP, group = Cover_crop, col = Cover_crop)) + 
  stat_summary(fun.y = "mean", geom = "line")+
  stat_summary(fun.y = "mean", geom = "point")+
  facet_grid( .~ Tillage)+
  labs(x = "Crop Rotation",
       y = "Total GWP\n(tonne CO2e/ac/y)")+
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),
        legend.position = "right",
        panel.grid = element_blank(), #to remove the background grid
        panel.border = element_rect(colour = "black", fill=NA, size=1)) #to add frame to the graph))
       
       
#making figure grid
fig_grd <- plot_grid( Fig_IL,  Fig_PA, 
                      labels = c('A', 'B'),
                      ncol = 1, nrow = 2)
       
#saving the plot
ggsave("Total_GWP.tiff",fig_grd, units="in", width=7.3, height=9, dpi = 800)

       
       
       