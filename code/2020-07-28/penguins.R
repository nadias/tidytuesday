#devtools::install_github("thomasp85/patchwork")
library(tidyverse)
library(ggthemes)
library(GGally)
library(ggfortify)
library(patchwork)
library(cowplot)
library(magick)

#################################################################################
# Importing the data
#################################################################################

penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

#################################################################################
# Pre-processing
#################################################################################

# Removing 11 rows with missing values
penguins.csv <- penguins.csv[complete.cases(penguins.csv), ]


# Changing the strings to add "Island" to the island's name
penguins.csv$island <- recode(penguins.csv$island, Biscoe = "Biscoe Island", Dream = "Dream Island", Torgersen = "Torgersen Island")


# Changing the strings "female" to "Female" and "male" to "Male"
penguins.csv$sex <- recode(penguins.csv$sex, female = "Female", male = "Male")

# Renaming variables
penguins.csv <- penguins.csv %>% rename('Bill length' = bill_length_mm,
                                        'Bill depth' = bill_depth_mm,
                                        'Flipper length' = flipper_length_mm,
                                        'Body mass' = body_mass_g,
                                        'Island' = island)


#################################################################################
# Exploration
#################################################################################

# How many penguins of each species? 
penguins.csv %>%
  ggplot(aes(x = species, fill = species)) +
  geom_bar() +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum()


# How many penguins in each island? 
penguins.csv %>%
  ggplot(aes(x = island, fill = island)) +
  geom_bar() +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum()


# Where are the Adelie? 
penguins.csv %>%
  filter (species == 'Adelie') %>%
  ggplot(aes(x = island, fill = island)) +
  geom_bar() +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum()


# How are species distributed by islands
penguins.csv %>%
  ggplot(aes(x = island, y=species, color = species)) +
  geom_point() +
  geom_jitter() +   # Add jitter so we can see all points Not overlapped
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()


# Check sex distribution by species
penguins.csv %>%
  ggplot(aes(x = species, fill = sex)) + 
  geom_bar(position=position_dodge()) +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum()


# Check sex distribution by island
penguins.csv %>%
  ggplot(aes(x = island, fill = sex)) + 
  geom_bar(position=position_dodge()) +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum()


# Average Bill length -> Adelie     38.8
penguins.csv %>%
  group_by(species) %>%
  dplyr::summarize(Mean = mean(`Bill length`, na.rm=TRUE))


# Average Male Bill length -> Adelie     40.4
penguins.csv %>%
  filter(sex == "Male") %>%
  group_by(species) %>%
  dplyr::summarize(Mean = mean(`Bill length`, na.rm=TRUE))


# Average Female Bill length -> Adelie     37.3
penguins.csv %>%
  filter(sex == "Female") %>%
  group_by(species) %>%
  dplyr::summarize(Mean = mean(`Bill length`, na.rm=TRUE))


#################################################################################
# Plots
#################################################################################


# Plotting species distribution by island (barplot)
island_bar <- penguins.csv %>%
  filter (species == 'Adelie') %>%
  ggplot(aes(x = island, fill = island)) +
  geom_bar() +
  scale_fill_manual(values=c('#E69F00','#999999', '#000000')) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        strip.text.x = element_text(size = 12),
        legend.position = "none")
island_bar


# Plotting bill and flipper lengths segmented by island and color coded by species
points <- penguins.csv %>%
  ggplot(aes(x = `Bill length`, y = `Bill depth`, color = species)) + #(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~ island, scales = "free_x") +
  scale_color_manual(values=c('#E69F00','#999999', '#000000')) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        strip.text.x = element_text(size = 12),
        legend.position = "top",
        panel.spacing = unit(3, "lines")) +
  labs(col = "Species",
       x = "Bill length (mm)",
       y = "Flipper length (mm)")
points


# Violin plots showing the distribution of bill length by different genders and color coded by species
violins <- penguins.csv %>%
  ggplot(aes(x = species, y = `Bill length`, fill = species)) + 
  geom_violin() +
  facet_wrap(~ sex, scales = "free_x") +
  scale_fill_manual(values=c('#E69F00','#999999', '#000000')) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  labs(y = "Bill length (mm)")
violins


# Biplot
# Select only numerical variables
penguins <- penguins.csv %>% select(`Bill length`, `Bill depth`, `Flipper length`, `Body mass`, species)
# Perform PCA
pca_res <- prcomp(penguins[1:4], scale. = TRUE)
# PCA biplot = PCA score plot + loading plot
pca <- autoplot(pca_res, data = penguins, colour = 'species',
         loadings = TRUE, loadings.colour = '#FF9900') +
  geom_label(aes(x = -0.05, y = -0.15, label = "Bill depth"), fill = "#E69F00") +
  geom_label(aes(x = 0.09, y = -0.11, label = "Bill length"), fill = "#E69F00") +
  geom_label(aes(x = 0.11, y = 0.03, label = "Flipper length"), fill = "#E69F00") +
  geom_label(aes(x = 0.11, y = -0.03, label = "Body mass"), fill = "#E69F00") +
  scale_color_manual(values=c('#E69F00','#999999', '#000000')) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    axis.title = element_text()
)
pca


# Parallel coordinate plot of the numerical variables
parallel_coord <- penguins %>%
  arrange(desc(species)) %>%
  ggparcoord(columns = 1:4, groupColumn = 5,
             scale="uniminmax",
             showPoints = TRUE, 
             alphaLines = 0.3
  ) + 
  scale_color_manual(values=c('#E69F00','#999999', '#000000')) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none"
  )
parallel_coord


# Putting the plots together using patchwork package
design <- "#BBB#
           #BBB#
           CCCDD
           CCCDD
           CCCDD"
# The design parameter defines the layout of the plots
p <- wrap_plots(points, pca, violins, design = design) +
  plot_annotation(
    title = 'How to spot an Adelie penguin?\n',
    subtitle = 'Head over to Biscoe, Dream or Torgersen island in the Palmer archipelago in Antarctica At Biscoe Island, you can look at the flipper length to distinguish
them from Gentoo penguins. Chintraps are trickier to separate from Adelies, but you can look out for a shorter bill length. On average, Adelie males have
a 40.4 mm long bill, and females only 37.3 mm.',
    caption = 'Visualization by Nádia Soares for TidyTuesday | Data source: Palmer Penguins'
  ) & theme(plot.background = element_rect(fill = "#f1f1f1"),
            plot.margin = unit(c(1,0.7,0.7,0.7), "cm"),
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 12, 
                                         lineheight = 1.2))
p


# Printing to png
png("penguins.png", width = 2366, height = 1872)
print(p) 
dev.off()