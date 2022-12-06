#Script name: penguin_assignment.r

#Purpose of script:
  #Load penguin data, clean data, run statistical test on bill
  #dimensions, plot bill dimensions and save the plots to a file

#Date created: 12/11/2022

# Loading Packages --------------------------------------------------------

library(palmerpenguins)
library(ggplot2)
library(janitor)
library(dplyr)
library(ragg)
library(svglite)


# Defining functions ---------------------------------------------------------


# Cleaning data -----------------------------------------------------------

#Clean column names, remove empty rows, remove columns called comment and delta
cleaning <- function(data_raw){
  data_raw %>% 
    clean_names() %>% 
    remove_empty(c("rows","cols")) %>% 
    select(-starts_with("delta")) %>% 
    select(-comments)
}

#Sub-setting data and removing NAs for bill length and depth
remove_empty_bills<- function(data_clean){
  data_clean %>% 
    filter(!is.na(culmen_length_mm)) %>% 
    filter(!is.na(culmen_depth_mm)) %>% 
  select(species, culmen_length_mm, culmen_depth_mm)
}




# Plotting ----------------------------------------------------------------

#Plotting a scatter plot showing the relationship between bill length and depth
#separated for the different species with the linear models overlaid
plot_bill_figure <- function(penguins_bills){
  penguins_bills %>% 
    ggplot(aes(x=culmen_length_mm, y=culmen_depth_mm, color=species, shape= species))+
    geom_point(size=3, alpha=0.8)+
    geom_smooth(method="lm")+
    scale_colour_manual(values = c("pink2","orchid4","skyblue4")) +
    theme_bw()+
    labs(title= "Penguin bill dimensions for Adelie, Chinstrap and Gentoo Penguins",
         x="Bill length (mm)",
         y="Bill depth (mm)",
         colour="Species",
         shape="Species")
  
}


# Saving ------------------------------------------------------------------
##Saving the plot as a PNG, defining size, resolution and scaling
  save_bill_plot_png <- function(penguins_bills, filename, size, size1, res, scaling){
    agg_png(filename, width = size, 
            height = size1, 
            units = "cm", 
            res = res, 
            scaling = scaling)
    bill_plot <- plot_bill_figure(penguins_bills)
    print(bill_plot)
    dev.off()
  }

##Saving the plot as a SVG defining size and scaling
save_bill_plot_svg <- function(penguins_bills, filename, size, size1, scaling){
  size_inches = size/2.54
  size1_inches = size1/2.54
  svglite(filename, width = size_inches, height = size1_inches, scaling = scaling)
  bill_plot <- plot_bill_figure(penguins_bills)
  print(bill_plot)
  dev.off()
}


# Load data ---------------------------------------------------------------

write.csv(penguins_raw, "data_raw/penguins_raw.csv")
penguins_raw <- read.csv("data_raw/penguins_raw.csv")


# Running functions -------------------------------------------------------


# Cleaning ----------------------------------------------------------------

penguins_clean <- cleaning(penguins_raw)

write.csv(penguins_clean, "data_clean/penguins_clean.csv")
penguins_bills <-remove_empty_bills(penguins_clean)


# Plotting ----------------------------------------------------------------

bill_plot <-plot_bill_figure(penguins_bills)
bill_plot


# Saving figures ----------------------------------------------------------


save_bill_plot_png(penguins_bills, "figures/fig01.png", size=35, size1= 25, res=600, scaling=1)


save_bill_plot_svg(penguins_clean, "figures/fig01_vector.svg", 
                      size = 30, size1= 20, scaling = 1)


# Statistical Analysis ---------------------------------------------------------
#ANCOVA looking at the relationship between bill length and bill depth and the interaction with species
penguins_model1 <-lm(culmen_depth_mm ~ culmen_length_mm* species, penguins_bills)

#Checking assumptions of linear regression
#qqplot
plot(penguins_model1, which=2)
#residuals vs fitted values
plot(penguins_model1, which=1)

#Results
anova(penguins_model1)
summary(penguins_model1)


