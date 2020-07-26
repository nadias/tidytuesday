library(tidyverse)
library(ggthemes)
library(cowplot)
library(magick)
library(grid)


#################################################################################
# Importing the data
#################################################################################

tuesdata <- tidytuesdayR::tt_load("2020-07-21")
tuesdata <- tidytuesdayR::tt_load(2020, week = 30)

animal_outcomes <- tuesdata$animal_outcomes
animal_complaints <- tuesdata$animal_complaints
#brisbane_complaints <- tuesdata$brisbane_complaints # NOT IN USE



#################################################################################
# Complaints Plot
#################################################################################


# Changing the strings "cat" to "Cats" and "dog" to "Dogs"
animal_complaints$`Animal Type` <- recode(animal_complaints$`Animal Type`, cat = "Cats", dog = "Dogs")


# Inserting lines for missing Animal Type - Complaint Type combinations
complaints_counts <- animal_complaints %>%
  count(`Animal Type`, `Date Received`, `Complaint Type`) %>%
  complete(`Animal Type`, nesting(`Date Received`, `Complaint Type`), fill = list(n = 0))


# Converting "Date Received" string to a date in the format yyyy-mm-01
complaints_counts$`Date Received` <- parse_datetime(complaints_counts$`Date Received`, "%B %Y")


# Remove 2013 and 2020 since those years are not complete
complaints_counts <- complaints_counts %>%
                        filter(`Date Received` >= parse_datetime("January 2014", "%B %Y") &
                                 `Date Received` <= parse_datetime("December 2019", "%B %Y"))

# Exploration

## Check seasonal pattern of noise complaints for dogs
complaints_counts %>% filter(`Complaint Type` == "Noise" & `Animal Type` == "Dogs")


## Get percentage of complaints by type of animal.
## Dogs - 34564 complaints (90.2%)
## Cats - 3745 complaints (9.78%)
complaints_counts %>%
  group_by(`Animal Type`) %>% 
  summarise(n = sum(n)) %>%
  mutate(Percentage=n/sum(n)*100)


## Get percentage of complaints related to dogs by type of complaint.
complaints_counts %>%
  filter(`Animal Type` == 'Dogs') %>% 
  group_by(`Complaint Type`) %>% 
  summarise(n = sum(n)) %>%
  mutate(Percentage=n/sum(n)*100)


## Get percentage of complaints related to cats by type of complaint.
complaints_counts %>%
  filter(`Animal Type` == 'Cats') %>% 
  group_by(`Complaint Type`) %>% 
  summarise(n = sum(n)) %>%
  mutate(Percentage=n/sum(n)*100)


# Creating the plot
p <- complaints_counts %>%
     ggplot(aes(x = `Date Received`, y = n, color = `Animal Type`)) +
       geom_line() +
       geom_smooth(method = "loess", formula = "y ~ x") +
       facet_grid(`Animal Type` ~ `Complaint Type`, scales = "free_y") +
       theme_fivethirtyeight() +
       theme(axis.title = element_text(),
             axis.title.x = element_blank(),
             axis.text.x = element_text(angle = 90),
             plot.margin = unit(c(1,3.5,1,1), "cm"),
             strip.text.x = element_text(size = 12),
             strip.text.y = element_blank()) +
       labs(title = "Evolution of the number of complaints by animal type from 2014 to 2019\n",
            subtitle = "The Royal Society for the Prevention of Cruelty to Animals is an independent charity that strives to provide care and protection to all creatures, and to prevent animal cruelty.\n
Their database shows a downward trend for several types of complaint. More than 90% of the reports were related to dogs, with private impound being the most common problem (25%). Noise complaints\nfollowed closely and showed a seasonal pattern, being less prevalent in warmer months, especially from November to February.\nFor cats, private impound was also the most worrying issue, corresponding to nearly 75% of the reports in this period.\n",
            caption = "Visualization by Nádia Soares for TidyTuesday | Data source: RSPCA Australia",
            y = "Number of complaints")

# Adding cat and dog icons
ggdraw() +
  draw_plot(p) +
  draw_image(file.path(getwd(), "img/cat_logo.png"),  x = 0.46, y = 0.12, scale = .1) +
  draw_image(file.path(getwd(), "img/dog_logo.png"),  x = 0.45, y = -0.18, scale = .1)



#################################################################################
# Outcomes Plot
#################################################################################


# Keep only Dogs and Cats and variables of interest
animal_outcomes <- animal_outcomes %>%
                      filter(animal_type == 'Dogs' | animal_type == 'Cats') %>%
                      select(year, animal_type, outcome, Total)

# Convert "Currently In Care" and "In Stock" outcomes to "Other"
animal_outcomes$outcome <- recode(animal_outcomes$outcome, `Currently In Care` = "Other", `In Stock` = "Other")

# Reordering the values of "outcome" factor
animal_outcomes$outcome = factor(animal_outcomes$outcome, levels=c("Reclaimed", "Rehomed", "Euthanized", "Transferred", "Other"))

# Inserting lines for missing Animal Type - Complaint Type combinations
animal_outcomes <- animal_outcomes %>%
  complete(outcome, nesting(year, animal_type), fill = list(Total = 0))

# Add percentage value
animal_outcomes <- animal_outcomes %>%
  group_by(animal_type, year, outcome) %>% 
  summarise(Total = sum(Total)) %>%
  mutate(Percentage=Total/sum(Total)*100)


# Exploration

## Keep only outcomes related to cats and dogs
animal_outcomes_pets <- animal_outcomes %>%
  filter(animal_type == 'Dogs' | animal_type == 'Cats')


## Get percentage of different animal types
## Dogs - 1148313 complaints (50.7%)
## Cats - 1117527 complaints (49.3%)
animal_outcomes_pets%>%
  group_by(animal_type) %>% 
  summarise(n = sum(Total)) %>%
  mutate(Percentage=n/sum(n)*100)


## Get percentage of outcomes
animal_outcomes_pets %>%
  group_by(outcome) %>% 
  summarise(n = sum(Total)) %>%
  mutate(Percentage=n/sum(n)*100)


## Keep only the year 2018
animal_outcomes_pets_2018 <- animal_outcomes_pets %>%
  filter(year == "2018")


## Get percentage of outcomes (2018 only)
animal_outcomes_pets_2018 %>%
  group_by(outcome) %>% 
  summarise(n = sum(Total)) %>%
  mutate(Percentage=n/sum(n)*100)



# Creating the plot
p <- animal_outcomes %>%
  ggplot(aes(x = year, y = Percentage, color = animal_type)) +
  geom_line() +
  facet_grid(animal_type ~ outcome, scales = "free_y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90),
        plot.margin = unit(c(1,3.5,1,1), "cm"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank()) +
  labs(title = "Evolution of outcomes by animal type from 1999 to 2018\n",
       subtitle = "From 1999 to 2018, RSPCA Australia has taken in more than 2 million cats and dogs. The rate of euthanized animals has fallen, and more pets have been reclaimed or rehomed. Cats tend to be rehomed
much more often than reclaimed.
Still, in 2018 alone, 16,048 pets were euthanized and 9,236 were transferred or had another outcome.\n
Please consider adopting your next pet and donating to the RSPCA or helping your local associations.\n",
       caption = "Visualization by Nádia Soares for TidyTuesday | Data source: RSPCA Australia",
       col = "Animal Type")


# Adding cat and dog icons
ggdraw() +
  draw_plot(p) +
  draw_image(file.path(getwd(), "img/cat_logo.png"),  x = 0.46, y = 0.12, scale = .1) +
  draw_image(file.path(getwd(), "img/dog_logo.png"),  x = 0.45, y = -0.19, scale = .1)
  
