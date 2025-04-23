# To install pacman if needed
if (!require("pacman")) install.packages("pacman")

# Load requiered libraries
pacman::p_load(dplyr, ggplot2, synergyfinder)

# Set the working directotoy
#setwd("/path/to/your/directory")

setwd("/Users/jhamy/Desktop/Kadai/Drug testing")

# Load the data
df <- read.csv("df_plot.csv") #to generate bar plot
sf_df <- read.csv("synergyfinder_df.csv") #to use synergy finder

#####plot####

# to set the bars order
label_order <- c(
  "NC", 
  "Dip8", 
  "Dip16", 
  "Dip32",
  "T-180",
  "T-360",
  "T-720", 
  "Dip8/T-180",
  "Dip8/T-360",
  "Dip8/T-720",
  "Dip16/T-180",
  "Dip16/T-360", 
  "Dip16/T-720", 
  "Dip32/T-180",
  "Dip32/T-360",
  "Dip32/T-720"
)

df$treatment <- factor(df$treatment, levels = label_order)

# Bar plot
ggplot(df, aes(x = treatment, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - SD, ymax = mean + SD),  
    width = 0.2,
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Treatment", 
    y = "Cell viability (%)", 
    title = "Cell viability by Dose (Mean ± SD)"
  ) +
  theme_bw()  

#### Synergy Scores for Drug Combinations ####

res <- ReshapeData(
  data = sf_df,
  data_type = "viability",
  impute = TRUE,
  impute_method = NULL,
  noise = TRUE,
  seed = 1)

res <- CalculateSynergy(
  data = res,
  method = c("ZIP", "HSA", "Bliss", "Loewe"),
  Emin = NA,
  Emax = NA,
  correct_baseline = "non")


