library(dplyr)
library(ggplot2)
library(synergyfinder)

#load the data
df <- read_csv("Drug screening/df_plot.csv")
sf_df <- read_csv("Drug screening/synergyfinder_input.csv")

####for raw data####

#calculate NC mean
nc_mean <- treatment %>%
  filter(treatment == "NC") %>%
  select(starts_with("Rep")) %>%
  unlist() %>%
  mean()

#adjust data by NC mean

df <- df %>%
  mutate(across(starts_with("Rep"), ~ . / nc_mean))


#add mean and DS
df <- df %>%
  mutate(
    mean = rowMeans(select(., starts_with("Rep"))),
    SD = apply(select(., starts_with("Rep")), 1, sd)
    )

#####plot####

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

#### Bliss ####
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


