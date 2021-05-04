library(tidyverse)
library(haven)
library(dplyr)
setwd("~/Documents/JHU/Probability and Statistics/Datasets")
ANES_2020 <- read_dta("ANES_2020_timeseries.dta")

# Part 1 - Background Information

df <- ANES_2020 %>% 
  select(V201510, V202468x, V202158, V202143, V202144, V202156, V202167, V202173)
colnames(df)[1] <- "education_level"
colnames(df)[2] <- "household_income"
colnames(df)[3] <- "fauci_therm"
colnames(df)[4] <- "biden_therm"
colnames(df)[5] <- "trump_therm"
colnames(df)[6] <- "harris_therm"
colnames(df)[7] <- "congress_therm"
colnames(df)[8] <- "scientists_therm"

df[df == -9 | df == -8 |df == -7 | df == -6 | df == -5 | df == 95 | 
     df == 998 | df == 999] <- NA

head(df)

# Part 2 - Descriptive Information

prop.table(table(df$education_level))
prop.table(table(df$household_income))

summary_stats <- function(x) {
  min <- min(x, na.rm = T)
  max <- max(x, na.rm = T)
  mean <- mean(x, na.rm = T)
  sd <- sd(x, na.rm = T)
  N <- length(x[!is.na(x)])
  return(c(min, max, mean, sd, N))
}

summary_stats(df$education_level)
summary_stats(df$household_income)
summary_stats(df$fauci_therm)
summary_stats(df$biden_therm)
summary_stats(df$trump_therm)
summary_stats(df$harris_therm)
summary_stats(df$congress_therm)
summary_stats(df$scientists_therm)

hist(df$fauci_therm, 
     freq = F, 
     ylim = c(0, 0.03),
     main = "Feeling Thermometer for Dr. Anthony Fauci", 
     xlab = "Thermometer Ratings")

hist(df$trump_therm, 
     freq = F, 
     ylim = c(0, 0.05), 
     main = "Feeling Thermometer for Donald Trump", 
     xlab = "Thermometer Ratings")

MyGray <- rgb(t(col2rgb("black")), alpha=50, maxColorValue=255)

plot(df$trump_therm, df$fauci_therm, 
     main = "Fauci and Trump Thermometer Ratings",
     xlab = "Trump Thermometer Ratings", 
     ylab = "Fauci Thermometer Ratings",
     type = "p", 
     pch = 20, 
     col = MyGray)

jittertrump <- jitter(df$trump_therm, factor = 20)
jitterfauci <- jitter(df$fauci_therm, factor = 20)
plot(jittertrump, jitterfauci, 
     main = "Fauci and Trump Thermometer Ratings",
     xlab = "Trump Thermometer Ratings", 
     ylab = "Fauci Thermometer Ratings",
     type = "p", 
     pch = 20, 
     col = MyGray)
abline(lm(df$fauci_therm ~ df$trump_therm), col = "#185a37")

# What proportion of people who gave Fauci 100 gave Trump 0?
trump_scores <- df$trump_therm[!is.na(df$trump_therm) & !is.na(df$fauci_therm)]
fauci_scores <- df$fauci_therm[!is.na(df$fauci_therm) & !is.na(df$trump_therm)]

scores <- data.frame(fauci_scores = fauci_scores, 
                     trump_scores = trump_scores)
scores <- as_tibble(scores)

scores %>%
  filter(scores$fauci_scores == 100 & scores$trump_scores == 0) %>%
  nrow() / nrow(scores)

scores %>%
  filter(scores$fauci_scores == 0 & scores$trump_scores == 100) %>%
  nrow() / nrow(scores)

# Part 3 - Regression Analysis

btrump_on_fauci <- lm(df$fauci_therm ~ df$trump_therm)
summary(btrump_on_fauci)

trump_on_fauci <- lm(df$fauci_therm ~ df$trump_therm
                   + df$scientists_therm
                   + df$congress_therm
                   + df$harris_therm
                   + df$biden_therm
                   + df$household_income
                   + df$education_level)
# categorical variables are entered linearly, as they are ordered
summary(trump_on_fauci)

bfauci_on_trump <- lm(df$trump_therm ~ df$fauci_therm)
summary(bfauci_on_trump)

fauci_on_trump <- lm(df$trump_therm ~ df$fauci_therm
                     + df$scientists_therm
                     + df$congress_therm
                     + df$harris_therm
                     + df$biden_therm
                     + df$household_income
                     + df$education_level)
summary(fauci_on_trump)

























