library(tidyverse)
library(ggthemes)
library(ggtext)
library(patchwork)


#################################################################################
# Importing the data
#################################################################################

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

#################################################################################
# Cleaning & reformatting
#################################################################################


# Check if all values exist for all countries

energy_types %>%
  group_by(country) %>%
  summarise(count = n_distinct(type)) # All countries have 8 values


# Add new variable that groups clean energy types into one
energy_types$type_group <- recode(energy_types$type,
                                  Geothermal = "Clean energy",
                                  Hydro = "Clean energy",
                                  Nuclear = "Clean energy",
                                  Other = "Clean energy",
                                  `Pumped hydro power` = "Clean energy",
                                  Solar = "Clean energy",
                                  Wind = "Clean energy")




energy_2018_raw <- energy_types %>%
  filter(level == "Level 1") %>%
  select(country, country_name, `2018`, type_group) %>%
  rename(value = `2018`) %>%
  group_by(country, country_name, type_group) %>% 
  summarise_at(c("value"), sum)
energy_2018_raw


# Mean of Clean energy and Conventional thermal produced in Europe
europe_mean <- energy_2018_raw %>%
  mutate(value = value/sum(value)*100) %>%
  group_by(type_group) %>% 
  summarise_at(c("value"), mean)  
europe_mean



#############################################################################
# Plots
#############################################################################


# Radial plot: clean energy by country


## Data formatting

energy_2018 <- energy_types %>%
  filter(level == "Level 1") %>%
  select(country, country_name, `2018`, type_group) %>%
  rename(value = `2018`) %>%
  group_by(country, country_name, type_group) %>% 
  summarise_at(c("value"), sum) %>%
  mutate(value = value/sum(value)*100)
energy_2018

energy_2018_wide <- energy_2018 %>% 
  spread(type_group, value)
energy_2018_wide

energy_2018_wide_simmetric <- mutate(energy_2018_wide, `Conventional thermal` = -`Conventional thermal`)

energy_2018_wide_simmetric$leg1 <- "Clean energy"
energy_2018_wide_simmetric$leg2 <- "Conventional thermal"



#############################################################################

## Plot

radial <- ggplot(energy_2018_wide_simmetric, aes(x = reorder(country, `Clean energy`))) +
  geom_col(aes(y = `Clean energy`, fill = leg1)) +
  geom_text(aes(y = ifelse(`Clean energy` >= 20, 12, (`Clean energy` + 10)),
                color = ifelse(`Clean energy` >= 20, "#FFFFFF", "#5d8402"),
                label = round(`Clean energy`, 1)), size = 3) +
  geom_col(aes(y = `Conventional thermal`, fill = leg2)) +
  geom_text(aes(y = ifelse(`Conventional thermal` <= -20, -10, (`Conventional thermal` - 10)),
                color = ifelse(`Conventional thermal` <= -20, "#FFFFFF", "#817d79"),
                label = round(-`Conventional thermal`, 1)), size = 3)+
  geom_text(aes(y = 120, label = country)) +
  annotate("segment", x = 1, xend = 37, y = 52, yend = 52, color = "#7fa614", size=.6, alpha=0.6) +
  annotate("segment", x = 1, xend = 37, y = -48, yend = -48, color = "#706c68", size=.6, alpha=0.6) +
  annotate("segment", x = 18.45, xend = 37, y = 52, yend = 52, color = "#FFFFFF", size=.6, alpha=0.6) +
  annotate("segment", x = 1, xend = 18.5, y = -48, yend = -48, color = "#FFFFFF", size=.6, alpha=0.6) +
  annotate("text", x = c(1.6, 35.2), y = c(71,-71), 
           label = c("Europe's\naverage", "Europe's\naverage") , color=c("#7fa614", "#706c68"), 
           size=3.2, fontface="bold") +
  coord_polar() +
  scale_fill_manual(values = c("#5d8402", "#817d79")) +
  scale_y_continuous(limits = c(-150, 120)) +
  scale_color_identity() +
  labs(fill = "Energy type") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.position = c(0.05, 0),
        legend.spacing.y = unit(0.5, 'cm'))
radial




#############################################################################

# Stacked barplot: percentage of energy sources for all Europe


## Data formatting


energy_types_2018 <- energy_types %>%
  filter(level == "Level 1") %>%
  select(country, country_name, `2018`, type) %>%
  rename(value = `2018`) %>%
  group_by(type) %>% 
  summarise_at(c("value"), sum) %>%
  mutate(value = value/sum(value)*100)
energy_types_2018


energy_types_2018_clean <- energy_types_2018 # %>%

energy_types_2018_clean$year = "2018"
energy_types_2018_clean$type = as.factor(energy_types_2018_clean$type)
energy_types_2018_clean$type = factor(energy_types_2018_clean$type,
                                      levels = c("Conventional thermal", "Nuclear", "Hydro", "Wind", "Solar", "Geothermal", "Other"))


#############################################################################


## Plot


clean_division_stacked <- energy_types_2018_clean %>%
  ggplot(aes(x=year, y = value, fill = type)) +
  geom_col(width = 0.1) +
  
  # Annotations on the left side
  
  annotate("segment", x = 0.6, xend = 0.62, y = 100, yend = 100, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("segment", x = 0.6, xend = 0.62, y = 54.8, yend = 54.8, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 0.75, y = 78, 
           label = "Conventional thermal", color="black", 
           size=3.2, fontface="bold") +
  annotate("text", x = 0.66, y = 74, 
           label = "46%", color="black", 
           size=3.2) +
  
  annotate("segment", x = 0.6, xend = 0.62, y = 54, yend = 54, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("segment", x = 0.6, xend = 0.62, y = 0, yend = 0, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 0.716, y = 44, 
           label = "Clean eletricity", color="black", 
           size=3.2, fontface="bold") +
  annotate("text", x = 0.66, y = 40,
           label = "54%", color="black", 
           size=3.2) +
  
  annotate("segment", x = 0.76, xend = 0.78, y = 31.2, yend = 31.2, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("segment", x = 0.76, xend = 0.78, y = 0, yend = 0, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 0.853, y = 18, 
           label = "Renewable", color="black", 
           size=3.2, fontface="bold") +
  annotate("text", x = 0.82, y = 14, 
           label = "31%", color="black", 
           size=3.2) +
  
  annotate("segment", x = 0.6, xend = 0.6, y = 100, yend = 54.8, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("segment", x = 0.6, xend = 0.6, y = 54, yend = 0, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("segment", x = 0.76, xend = 0.76, y = 31.2, yend = 0, color = "#BBBBBB", size=.3, alpha=1) +
  
  # Annotations on the right side
  
  annotate("segment", x = 1.05, xend = 1.07, y = 42.8, yend = 42.8, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 1.17, y = 42.8,
           label = "Nuclear 23%", color="black",
           size=3.2) +
  
  annotate("segment", x = 1.05, xend = 1.07, y = 23, yend = 23, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 1.165, y = 23,
           label = "Hydro 16%", color="black",
           size=3.2) +
  
  annotate("segment", x = 1.05, xend = 1.07, y = 9.4, yend = 9.4, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 1.16, y = 9.4,
           label = "Wind 11%", color="black",
           size=3.2) +
  
  annotate("segment", x = 1.05, xend = 1.07, y = 2, yend = 2, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 1.155, y = 2.2,
           label = "Solar 4%", color="black",
           size=3.2) +
  
  annotate("segment", x = 1.05, xend = 1.07, y = 0.15, yend = 0.15, color = "#BBBBBB", size=.3, alpha=1) +
  annotate("text", x = 1.195, y = 0.15,
           label = "Geothermal <1%", color="black",
           size=3.2) +
  
  geom_curve(aes(x = 1.1, y = 60, xend = 1.35, yend = 74 ),
             size = 0.17,
             curvature = 0.3) +
  geom_richtext(aes(x = 1.35, y = 80,
                    label = "In 2018, <br> more than 50% of all energy <br>  produced in Europe was <br><span style='color:#4c7301;'>**clean**</span>"),
                color = "black",
                size = 3.8,
                fill = NA,
                label.color = NA) +
  
  scale_fill_manual(values=c("#817d79", "#4c7301", "#5d8402", "#6e9503", "#7fa614", "#8fb725", "#9fc836")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
clean_division_stacked


#############################################################################

# Putting the plots together using patchwork

design <- "BBBBCCC
           BBBBCCC"

p <- wrap_plots(radial, clean_division_stacked, design = design) +
  plot_annotation(
    title = "Clean energy production in Europe in 2018\n",
    subtitle = "Percentage of clean energy produced by European countries, versus energy generated from conventional thermal sources. Clean energy encompasses nuclear and renewable sources.",
    caption = "Visualization by Nádia Soares | Twitter: @Naf_Soares
Code at: github.com/nadias
Data source: Eurostat Energy"
  ) &
  theme(plot.background = element_rect(fill = "#f1f1f1"),
        plot.margin = unit(c(1,0.7,0.7,0.7), "cm"),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12, 
                                     lineheight = 1.2))

p
