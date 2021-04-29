####################
## Model Performance

rm(list = ls())

# Define working directory path
setwd("O://50_PhDResearchProject/40_PartX_ZaniEtAl/") # ethz ; #setwd(".../AutumnPhenology/AutumnPhenology_Data&Metadata/Data/")

myData   <- "30_LaurasEulerData/"

# Load libraries
library(tidyverse)
library(lme4) #lmer
library(effects) #plot effects

##----------------------------------------
## Load data
df <- data.table::fread(paste0(myData, "DataMeta_3_Drivers_20_11_10.csv")) %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off_zani = autumn_anomaly, anom_on_zani = spring_anomaly, 
         species = Species, id_site = PEP_ID, id_species_site = timeseries)

##----------------------------------------
df_anl <- df

#
Fit_LT_cA_tot <- lmer(off ~ scale(cA_tot) + scale(year) + (1|id_site) + (1|species), data = df_anl, na.action = "na.exclude")

out_plot_lt   <- allEffects(Fit_LT_cA_tot)

#
mod_anet_year <- lmer(cA_tot ~ scale(year) + (1|id_site) + (1|species), data = df_anl, na.action = "na.exclude")

out_plot_anet <- allEffects(mod_anet_year)

##----------------------------------------
ggplot_year_anet <- function(x, y){
  
  df1 <- tibble(upper = x$`scale(year)`$upper[,1],
                lower = x$`scale(year)`$lower[,1],
                off = x$`scale(year)`$fit[,1],
                year = x$`scale(year)`$x[,1])
  df2 <- tibble(upper = y$`scale(year)`$upper[,1],
                lower = y$`scale(year)`$lower[,1],
                anet = y$`scale(year)`$fit[,1],
                year = y$`scale(year)`$x[,1])
  
  coef <- 0.7 * mean(df1$off) / mean(df2$anet)
  
  gg <- ggplot() + 
    geom_ribbon(data = df1, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df1, aes(x = year, y = off), col = "royalblue") +
    
    geom_ribbon(data = df2, aes(x = year, ymin = lower * coef + 100, ymax = upper * coef + 100), alpha = 0.2) +
    geom_line(data = df2, aes(x = year, y = anet * coef + 100), col = "tomato") +
    theme_classic() +
    labs(x = "Year") +
    scale_y_continuous(name = "EOS (DOY)",
                       sec.axis = sec_axis( ~(. - 100) / coef, name = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-2, ")")))) +
    theme(axis.text.y.right = element_text(colour="tomato"),
          axis.ticks.y.right = element_line(colour="tomato"),
          axis.title.y.right = element_text(colour="tomato"),
          axis.text.y = element_text(colour="royalblue"),
          axis.ticks.y = element_line(colour="royalblue"),
          axis.title.y = element_text(colour="royalblue"),
          axis.line.y = element_line(colour = "royalblue"),
          axis.line.y.right = element_line(colour = "red"))
  
  return(gg)
}

gg_dual <- ggplot_year_anet(out_plot_lt, out_plot_anet)

##----------------------------------------
library(gtable)
library(ggpubr)
library(grid)

gg_dual_L <- gg_dual +
  labs(title = expression(paste("EOS ~ ", italic("A")[net], " + ", bold("Year"))), 
       subtitle = "Zani et al.") +
  theme(plot.title = element_text(size = 12, colour = "royalblue"),
        plot.subtitle = element_text(face = "italic", colour = "royalblue"))

g <- ggplotGrob(gg_dual_L)

myPos <- c(t = g$layout$t[which(g$layout$name == "title")],
           l = g$layout$l[which(g$layout$name == "title")],
           b = g$layout$b[which(g$layout$name == "title")],
           r = g$layout$r[which(g$layout$name == "title")])

gg_dual_LR <- gtable_add_grob(g, textGrob(expression(paste(italic("A")[net], " + Year")),
                                          gp = gpar(col = "tomato", fontsize = 12),
                                          x = 1, hjust = 1),
                              t = myPos["t"], l = myPos["l"], b = myPos["b"], r = myPos["r"], 
                              name = "right-title")


gg_dual_LR <- as_ggplot(gg_dual_LR)



# SOURCES:
# https://stackoverflow.com/questions/21997715/add-ggplot-annotation-outside-the-panel-or-two-titles
# https://stackoverflow.com/questions/49735290/ggplot2-color-individual-words-in-title-to-match-colors-of-groups
