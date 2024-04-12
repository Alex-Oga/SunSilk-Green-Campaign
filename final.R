setwd("D:/Mahidol/ICMK 352 Marketing Intelligence/Final Assignment")
pacman::p_load(tidyverse, ggthemes, CGPfunctions, gmodels, broom, factoextra)
survey_dataframe <- read_csv("Sunsilk Green Survey (Responses).csv")

## Margin of Error Function

moe <- function(x, y) {
  a <- ((1 - y) / 2)
  error <- qnorm(a) * (sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
  up <- mean(x, na.rm = TRUE) + error
  lo <- mean(x, na.rm = TRUE) - error
  return(paste0("Mean = ", format(round(mean(x, na.rm = TRUE), 2), nsmall = 2), ", ", format(round(lo, 2), nsmall = 2), ") at ", y))
}

## Summary, Standard Deviation and Margin of Error of all uncategorized data
## MOTIV
summary(survey_dataframe$MOTIV_1, na.rm = TRUE)
sd(survey_dataframe$MOTIV_1, na.rm = TRUE)
moe(survey_dataframe$MOTIV_1, 0.95)
table(survey_dataframe$MOTIV_1)

summary(survey_dataframe$MOTIV_2, na.rm = TRUE)
sd(survey_dataframe$MOTIV_2, na.rm = TRUE)
moe(survey_dataframe$MOTIV_2, 0.95)
table(survey_dataframe$MOTIV_2)

summary(survey_dataframe$MOTIV_3, na.rm = TRUE)
sd(survey_dataframe$MOTIV_3, na.rm = TRUE)
moe(survey_dataframe$MOTIV_3, 0.95)
table(survey_dataframe$MOTIV_3)

summary(survey_dataframe$MOTIV_4, na.rm = TRUE)
sd(survey_dataframe$MOTIV_4, na.rm = TRUE)
moe(survey_dataframe$MOTIV_4, 0.95)
table(survey_dataframe$MOTIV_4)

summary(survey_dataframe$MOTIV_5, na.rm = TRUE)
sd(survey_dataframe$MOTIV_5, na.rm = TRUE)
moe(survey_dataframe$MOTIV_5, 0.95)
table(survey_dataframe$MOTIV_5)

## OCCAS
summary(survey_dataframe$OCCAS_1, na.rm = TRUE)
sd(survey_dataframe$OCCAS_1, na.rm = TRUE)
moe(survey_dataframe$OCCAS_1, 0.95)
table(survey_dataframe$OCCAS_1)

summary(survey_dataframe$OCCAS_2, na.rm = TRUE)
sd(survey_dataframe$OCCAS_2, na.rm = TRUE)
moe(survey_dataframe$OCCAS_2, 0.95)
table(survey_dataframe$OCCAS_2)

summary(survey_dataframe$OCCAS_3, na.rm = TRUE)
sd(survey_dataframe$OCCAS_3, na.rm = TRUE)
moe(survey_dataframe$OCCAS_3, 0.95)

## DEMOTIV
summary(survey_dataframe$DEMOTIV_1, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_1, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_1, 0.95)
table(survey_dataframe$DEMOTIV_1)

summary(survey_dataframe$DEMOTIV_2, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_2, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_2, 0.95)
table(survey_dataframe$DEMOTIV_2)

summary(survey_dataframe$DEMOTIV_3, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_3, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_3, 0.95)
table(survey_dataframe$DEMOTIV_3)

summary(survey_dataframe$DEMOTIV_4, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_4, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_4, 0.95)
table(survey_dataframe$DEMOTIV_4)

summary(survey_dataframe$DEMOTIV_5, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_5, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_5, 0.95)
table(survey_dataframe$DEMOTIV_5)

summary(survey_dataframe$DEMOTIV_6, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_6, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_6, 0.95)
table(survey_dataframe$DEMOTIV_6)

summary(survey_dataframe$DEMOTIV_7, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_7, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_7, 0.95)
table(survey_dataframe$DEMOTIV_7)

summary(survey_dataframe$DEMOTIV_8, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_8, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_8, 0.95)
table(survey_dataframe$DEMOTIV_8)

summary(survey_dataframe$DEMOTIV_9, na.rm = TRUE)
sd(survey_dataframe$DEMOTIV_9, na.rm = TRUE)
moe(survey_dataframe$DEMOTIV_9, 0.95)
table(survey_dataframe$DEMOTIV_9)

## PREFER
summary(survey_dataframe$PREFER_1, na.rm = TRUE)
sd(survey_dataframe$PREFER_1, na.rm = TRUE)
moe(survey_dataframe$PREFER_1, 0.95)
table(survey_dataframe$PREFER_1)

summary(survey_dataframe$PREFER_2, na.rm = TRUE)
sd(survey_dataframe$PREFER_2, na.rm = TRUE)
moe(survey_dataframe$PREFER_2, 0.95)
table(survey_dataframe$PREFER_2)

summary(survey_dataframe$PREFER_3, na.rm = TRUE)
sd(survey_dataframe$PREFER_3, na.rm = TRUE)
moe(survey_dataframe$PREFER_3, 0.95)
table(survey_dataframe$PREFER_3)

summary(survey_dataframe$PREFER_4, na.rm = TRUE)
sd(survey_dataframe$PREFER_4, na.rm = TRUE)
moe(survey_dataframe$PREFER_4, 0.95)
table(survey_dataframe$PREFER_4)

## SATIS
summary(survey_dataframe$SATIS_1.1, na.rm = TRUE)
sd(survey_dataframe$SATIS_1.1, na.rm = TRUE)
moe(survey_dataframe$SATIS_1.1, 0.95)
table(survey_dataframe$SATIS_1.1)

summary(survey_dataframe$SATIS_1.2, na.rm = TRUE)
sd(survey_dataframe$SATIS_1.2, na.rm = TRUE)
moe(survey_dataframe$SATIS_1.2, 0.95)
table(survey_dataframe$SATIS_1.2)

summary(survey_dataframe$SATIS_1.3, na.rm = TRUE)
sd(survey_dataframe$SATIS_1.3, na.rm = TRUE)
moe(survey_dataframe$SATIS_1.3, 0.95)
table(survey_dataframe$SATIS_1.3)

summary(survey_dataframe$SATIS_2.1, na.rm = TRUE)
sd(survey_dataframe$SATIS_2.1, na.rm = TRUE)
moe(survey_dataframe$SATIS_2.1, 0.95)
table(survey_dataframe$SATIS_2.1)

summary(survey_dataframe$SATIS_2.2, na.rm = TRUE)
sd(survey_dataframe$SATIS_2.2, na.rm = TRUE)
moe(survey_dataframe$SATIS_2.2, 0.95)
table(survey_dataframe$SATIS_2.2)

summary(survey_dataframe$SATIS_2.3, na.rm = TRUE)
sd(survey_dataframe$SATIS_2.3, na.rm = TRUE)
moe(survey_dataframe$SATIS_2.3, 0.95)
table(survey_dataframe$SATIS_2.3)

summary(survey_dataframe$SATIS_3.1, na.rm = TRUE)
sd(survey_dataframe$SATIS_3.1, na.rm = TRUE)
moe(survey_dataframe$SATIS_3.1, 0.95)
table(survey_dataframe$SATIS_3.1)

summary(survey_dataframe$SATIS_3.2, na.rm = TRUE)
sd(survey_dataframe$SATIS_3.2, na.rm = TRUE)
moe(survey_dataframe$SATIS_3.2, 0.95)
table(survey_dataframe$SATIS_3.2)

summary(survey_dataframe$SATIS_3.3, na.rm = TRUE)
sd(survey_dataframe$SATIS_3.3, na.rm = TRUE)
moe(survey_dataframe$SATIS_3.3, 0.95)
table(survey_dataframe$SATIS_3.3)

summary(survey_dataframe$SATIS_4.1, na.rm = TRUE)
sd(survey_dataframe$SATIS_4.1, na.rm = TRUE)
moe(survey_dataframe$SATIS_4.1, 0.95)
table(survey_dataframe$SATIS_4.1)

summary(survey_dataframe$SATIS_4.2, na.rm = TRUE)
sd(survey_dataframe$SATIS_4.2, na.rm = TRUE)
moe(survey_dataframe$SATIS_4.2, 0.95)
table(survey_dataframe$SATIS_4.2)

summary(survey_dataframe$SATIS_4.3, na.rm = TRUE)
sd(survey_dataframe$SATIS_4.3, na.rm = TRUE)
moe(survey_dataframe$SATIS_4.3, 0.95)
table(survey_dataframe$SATIS_4.3)

summary(survey_dataframe$SATIS_5.1, na.rm = TRUE)
sd(survey_dataframe$SATIS_5.1, na.rm = TRUE)
moe(survey_dataframe$SATIS_5.1, 0.95)
table(survey_dataframe$SATIS_5.1)

summary(survey_dataframe$SATIS_5.2, na.rm = TRUE)
sd(survey_dataframe$SATIS_5.2, na.rm = TRUE)
moe(survey_dataframe$SATIS_5.2, 0.95)
table(survey_dataframe$SATIS_5.2)

summary(survey_dataframe$SATIS_5.3, na.rm = TRUE)
sd(survey_dataframe$SATIS_5.3, na.rm = TRUE)
moe(survey_dataframe$SATIS_5.3, 0.95)
table(survey_dataframe$SATIS_5.3)

summary(survey_dataframe$SATIS_6.1, na.rm = TRUE)
sd(survey_dataframe$SATIS_6.1, na.rm = TRUE)
moe(survey_dataframe$SATIS_6.1, 0.95)
table(survey_dataframe$SATIS_6.1)

summary(survey_dataframe$SATIS_6.2, na.rm = TRUE)
sd(survey_dataframe$SATIS_6.2, na.rm = TRUE)
moe(survey_dataframe$SATIS_6.2, 0.95)
table(survey_dataframe$SATIS_6.2)

summary(survey_dataframe$SATIS_6.3, na.rm = TRUE)
sd(survey_dataframe$SATIS_6.3, na.rm = TRUE)
moe(survey_dataframe$SATIS_6.3, 0.95)
table(survey_dataframe$SATIS_6.3)

## Summary, Standard Deviation and Margin of Error of all categorized data
## Motivation
summary(survey_dataframe$Motivation, na.rm = TRUE)
sd(survey_dataframe$Motivation, na.rm = TRUE)
moe(survey_dataframe$Motivation, 0.95)
table(survey_dataframe$Motivation)
## Occasion
summary(survey_dataframe$Occasion, na.rm = TRUE)
sd(survey_dataframe$Occasion, na.rm = TRUE)
moe(survey_dataframe$Occasion, 0.95)
table(survey_dataframe$Occasion)
## Demotivation
summary(survey_dataframe$Demotivation, na.rm = TRUE)
sd(survey_dataframe$Demotivation, na.rm = TRUE)
moe(survey_dataframe$Demotivation, 0.95)
table(survey_dataframe$Demotivation)
## Preference
summary(survey_dataframe$Preference, na.rm = TRUE)
sd(survey_dataframe$Preference, na.rm = TRUE)
moe(survey_dataframe$Preference, 0.95)
table(survey_dataframe$Preference)
## Satisfaction
summary(survey_dataframe$Satisfaction, na.rm = TRUE)
sd(survey_dataframe$Satisfaction, na.rm = TRUE)
moe(survey_dataframe$Satisfaction, 0.95)
table(survey_dataframe$Satisfaction)

## Histogram
## Motivation 
ggplot(data = survey_dataframe, mapping = aes(x =
                                                Motivation)) + 
  geom_histogram(bins = 10)
## Occasion
ggplot(data = survey_dataframe, mapping = aes(x =
                                                Occasion)) + 
  geom_histogram(bins = 10)
## Demotivation
ggplot(data = survey_dataframe, mapping = aes(x =
                                                Demotivation)) + 
  geom_histogram(bins = 10)
## Preference
ggplot(data = survey_dataframe, mapping = aes(x =
                                                Preference)) + 
  geom_histogram(bins = 10)
## Satisfaction 
ggplot(data = survey_dataframe, mapping = aes(x =
                                                Satisfaction)) + 
  geom_histogram(bins = 10)

## Bar Chart
## USE_SUNSLIK_GREEN
ggplot(data = survey_dataframe, mapping = aes(x =
                                                USE_SUNSLIK_GREEN)) + 
  geom_bar()

## WANT_LONG_STRONG
ggplot(data = survey_dataframe, mapping = aes(x =
                                                WANT_LONG_STRONG)) + 
  geom_bar()

## CONTENT
ggplot(data = survey_dataframe, mapping = aes(x =
                                                CONTENT)) + 
  geom_bar()

## PERSONALITY
ggplot(data = survey_dataframe, mapping = aes(x =
                                                PERSONALITY)) + 
  geom_bar()

## PLATFORM
ggplot(data = survey_dataframe, mapping = aes(x =
                                                PLATFORM)) + 
  geom_bar()

## GENDER
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER)) + 
  geom_bar()

## AGE
ggplot(data = survey_dataframe, mapping = aes(x =
                                                AGE)) + 
  geom_bar()

## STATUS
ggplot(data = survey_dataframe, mapping = aes(x =
                                                STATUS)) + 
  geom_bar()

## EDUCATION
ggplot(data = survey_dataframe, mapping = aes(x =
                                                EDUCATION)) + 
  geom_bar()

## INCOME
ggplot(data = survey_dataframe, mapping = aes(x =
                                                INCOME)) + 
  geom_bar()

## Colored Bar Chart
## Gender USE_SUNSLIK_GREEN
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = USE_SUNSLIK_GREEN)) + 
  geom_bar()
## Gender WANT_LONG_STRONG
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = WANT_LONG_STRONG)) + 
  geom_bar()
## Gender CONTENT
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = CONTENT)) + 
  geom_bar()
## Gender PERSONALITY
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = PERSONALITY)) + 
  geom_bar()
## Gender PLATFORM
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = PLATFORM)) + 
  geom_bar()
## Gender AGE
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = AGE)) + 
  geom_bar()
## Gender STATUS
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = STATUS)) + 
  geom_bar()
## Gender EDUCATION
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = EDUCATION)) + 
  geom_bar()
## Gender INCOME
ggplot(data = survey_dataframe, mapping = aes(x =
                                                GENDER, fill = INCOME)) + 
  geom_bar()
## WANT_LONG_STRONG INCOME
ggplot(data = survey_dataframe, mapping = aes(x =
                                                WANT_LONG_STRONG, fill = INCOME)) + 
  geom_bar()
## WANT_LONG_STRONG AGE
ggplot(data = survey_dataframe, mapping = aes(x =
                                                WANT_LONG_STRONG, fill = INCOME)) + 
  geom_bar()
## WANT_LONG_STRONG EDUCATION
ggplot(data = survey_dataframe, mapping = aes(x =
                                                WANT_LONG_STRONG, fill = EDUCATION)) + 
  geom_bar()
## WANT_LONG_STRONG STATUS
ggplot(data = survey_dataframe, mapping = aes(x =
                                                WANT_LONG_STRONG, fill = STATUS)) + 
  geom_bar()

## USE_SUNSLIK_GREEN INCOME
ggplot(data = survey_dataframe, mapping = aes(x =
                                                USE_SUNSLIK_GREEN, fill = INCOME)) + 
  geom_bar()
## USE_SUNSLIK_GREEN AGE
ggplot(data = survey_dataframe, mapping = aes(x =
                                                USE_SUNSLIK_GREEN, fill = AGE)) + 
  geom_bar()
## USE_SUNSLIK_GREEN EDUCATION
ggplot(data = survey_dataframe, mapping = aes(x =
                                                USE_SUNSLIK_GREEN, fill = EDUCATION)) + 
  geom_bar()
## USE_SUNSLIK_GREEN STATUS
ggplot(data = survey_dataframe, mapping = aes(x =
                                                USE_SUNSLIK_GREEN, fill = STATUS)) + 
  geom_bar()
## Scatter Plot 
ggplot(data = survey_dataframe, mapping = aes(x = Motivation, y = Occasion)) + 
  geom_point()

## Scatter Plot Colored
ggplot(data = survey_dataframe, mapping = aes(x = Motivation, y = Occasion, color = GENDER)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Motivation, y = Preference, color = GENDER)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Motivation, y = Satisfaction, color = GENDER)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Satisfaction, y = Preference, color = WANT_LONG_STRONG)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Motivation, y = Occasion, color = USE_SUNSLIK_GREEN)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Motivation, y = Preference, color = USE_SUNSLIK_GREEN)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Motivation, y = Satisfaction, color = USE_SUNSLIK_GREEN)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Preference, y = Satisfaction, color = USE_SUNSLIK_GREEN)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Occasion, y = Preference, color = USE_SUNSLIK_GREEN)) + 
  geom_jitter()

ggplot(data = survey_dataframe, mapping = aes(x = Occasion, y = Satisfaction, color = USE_SUNSLIK_GREEN)) + 
  geom_jitter()

## Line Chart
## MOTIVATION
columns_to_plot <- c("MOTIV_1", "MOTIV_2", "MOTIV_3", "MOTIV_4", "MOTIV_5") 

long_motiv <- pivot_longer(survey_dataframe, cols = all_of(columns_to_plot), names_to = "column", values_to = "value")

ggplot(long_motiv, aes(x = column, y = value, group = GENDER, color = GENDER)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point") + 
  theme_minimal() + 
  labs(title = "Mean of Each Column", x = "", y = "Mean")

## OCCASION
columns_to_plot <- c("OCCAS_1", "OCCAS_2", "OCCAS_3") 

long_motiv <- pivot_longer(survey_dataframe, cols = all_of(columns_to_plot), names_to = "column", values_to = "value")

ggplot(long_motiv, aes(x = column, y = value, group = GENDER, color = GENDER)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point") + 
  theme_minimal() + 
  labs(title = "Mean of Each Column", x = "", y = "Mean")

## DEMOTIVATION
columns_to_plot <- c("DEMOTIV_1", "DEMOTIV_2", "DEMOTIV_3", "DEMOTIV_4", "DEMOTIV_5", "DEMOTIV_6", "DEMOTIV_7", "DEMOTIV_8", "DEMOTIV_9") 

long_motiv <- pivot_longer(survey_dataframe, cols = all_of(columns_to_plot), names_to = "column", values_to = "value")

ggplot(long_motiv, aes(x = column, y = value, group = GENDER, color = GENDER)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point") + 
  theme_minimal() + 
  labs(title = "Mean of Each Column", x = "", y = "Mean")

## PREFERENCE
columns_to_plot <- c("PREFER_1", "PREFER_2", "PREFER_3", "PREFER_4") 

long_motiv <- pivot_longer(survey_dataframe, cols = all_of(columns_to_plot), names_to = "column", values_to = "value")

ggplot(long_motiv, aes(x = column, y = value, group = GENDER, color = GENDER)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point") + 
  theme_minimal() + 
  labs(title = "Mean of Each Column", x = "", y = "Mean")

## SATISFACTION
columns_to_plot <- c("SATIS_1.1", "SATIS_1.2",	"SATIS_1.3",	"SATIS_2.1",	
                     "SATIS_2.2",	"SATIS_2.3",	"SATIS_3.1",	"SATIS_3.2",	"SATIS_3.3",	
                     "SATIS_4.1",	"SATIS_4.2",	"SATIS_4.3",	"SATIS_5.1",
                     "SATIS_5.2",	"SATIS_5.3",	"SATIS_6.1",	"SATIS_6.2",	"SATIS_6.3") 

long_motiv <- pivot_longer(survey_dataframe, cols = all_of(columns_to_plot), names_to = "column", values_to = "value")

ggplot(long_motiv, aes(x = column, y = value, group = GENDER, color = GENDER)) +
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point") + 
  theme_minimal() + 
  labs(title = "Mean of Each Column", x = "", y = "Mean")

## Visualize Difference
## Motivation
ggplot(data = survey_dataframe, mapping = aes(x = GENDER, y = Motivation, fill = GENDER)) + 
  geom_boxplot()

## Occasion
ggplot(data = survey_dataframe, mapping = aes(x = GENDER, y = Occasion, fill = GENDER)) + 
  geom_boxplot()

## Demotivation
ggplot(data = survey_dataframe, mapping = aes(x = GENDER, y = Demotivation, fill = GENDER)) + 
  geom_boxplot()

## Preference
ggplot(data = survey_dataframe, mapping = aes(x = GENDER, y = Preference, fill = GENDER)) + 
  geom_boxplot()

## Satisfaction
ggplot(data = survey_dataframe, mapping = aes(x = GENDER, y = Satisfaction, fill = GENDER)) + 
  geom_boxplot()

## USE_SUNSLIK
ggplot(data = survey_dataframe, mapping = aes(x = USE_SUNSLIK_GREEN, y = Satisfaction, fill = USE_SUNSLIK_GREEN)) + 
  geom_boxplot()
ggplot(data = survey_dataframe, mapping = aes(x = USE_SUNSLIK_GREEN, y = Preference, fill = USE_SUNSLIK_GREEN)) + 
  geom_boxplot()
ggplot(data = survey_dataframe, mapping = aes(x = USE_SUNSLIK_GREEN, y = Demotivation, fill = USE_SUNSLIK_GREEN)) + 
  geom_boxplot()
ggplot(data = survey_dataframe, mapping = aes(x = USE_SUNSLIK_GREEN, y = Occasion, fill = USE_SUNSLIK_GREEN)) + 
  geom_boxplot()
ggplot(data = survey_dataframe, mapping = aes(x = USE_SUNSLIK_GREEN, y = Motivation, fill = USE_SUNSLIK_GREEN)) + 
  geom_boxplot()

## WANT_LONG_STRONG
ggplot(data = survey_dataframe, mapping = aes(x = WANT_LONG_STRONG, y = Satisfaction, fill = WANT_LONG_STRONG)) + 
  geom_boxplot()
ggplot(data = survey_dataframe, mapping = aes(x = WANT_LONG_STRONG, y = Preference, fill = WANT_LONG_STRONG)) + 
  geom_boxplot()

## T-Test
t.test(formula = Motivation ~ USE_SUNSLIK_GREEN, data = survey_dataframe)
t.test(formula = Occasion ~ USE_SUNSLIK_GREEN, data = survey_dataframe)
t.test(formula = Demotivation ~ USE_SUNSLIK_GREEN, data = survey_dataframe)
t.test(formula = Preference ~ USE_SUNSLIK_GREEN, data = survey_dataframe)
t.test(formula = Satisfaction ~ USE_SUNSLIK_GREEN, data = survey_dataframe)
t.test(formula = Preference ~ WANT_LONG_STRONG, data = survey_dataframe)
t.test(formula = Satisfaction ~ WANT_LONG_STRONG, data = survey_dataframe)

## ANOVA

aov_income <- aov(formula = Motivation ~ INCOME, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

aov_income <- aov(formula = Motivation ~ STATUS, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

aov_income <- aov(formula = Motivation ~ GENDER, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

aov_income <- aov(formula = Motivation ~ EDUCATION, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

aov_income <- aov(formula = Motivation ~ GENDER, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

aov_income <- aov(formula = Motivation ~ AGE, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

aov_income <- aov(formula = Motivation ~ EDUCATION, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

aov_income <- aov(formula = Motivation ~ STATUS, data = survey_dataframe)
summary(aov_income)
TukeyHSD(aov_income)

## K-means 
set.seed(2023)

## MOTIV
survey_behaviour <- na.omit(survey_dataframe |>
  dplyr::select(starts_with(c("MOTIV_"))) |>
  scale())
fviz_nbclust(survey_behaviour, kmeans, method = "wss", k.max = 12)
k <- 5
km_output1 <- kmeans(survey_behaviour, centers = k, iter.max = 50, nstart = 25)
km_output1$centers
km_output1$size
km_centers <- as.data.frame(km_output1$centers)  |>
  mutate(cluster = as.factor(1:k)) |> 
  pivot_longer(cols = 1:ncol(km_output1[["centers"]]), values_to = "score", names_to = "variable") |> 
  mutate(variable = factor(variable, levels = colnames(km_output1[["centers"]])))
ggplot(km_centers, aes(x = cluster, y = score, fill = variable)) +
  geom_col(position = "dodge") +
  theme_light() +
  scale_fill_viridis_d(option = "magma")
ggplot(km_centers, aes(x = score, y = variable, fill = variable)) +
  geom_col() +
  facet_wrap(vars(cluster)) +
  theme_light() +
  scale_fill_viridis_d(option = "turbo") +
  theme(legend.position = "none")
km_output1$cluster 

## OCCAS
survey_behaviour <- na.omit(survey_dataframe |>
                              dplyr::select(starts_with(c("OCCAS_"))) |>
                              scale())
fviz_nbclust(survey_behaviour, kmeans, method = "wss", k.max = 12)
k <- 4
km_output2 <- kmeans(survey_behaviour, centers = k, iter.max = 50, nstart = 25)
km_output2$centers
km_output2$size
km_centers <- as.data.frame(km_output2$centers)  |>
  mutate(cluster = as.factor(1:k)) |> 
  pivot_longer(cols = 1:ncol(km_output2[["centers"]]), values_to = "score", names_to = "variable") |> 
  mutate(variable = factor(variable, levels = colnames(km_output2[["centers"]])))
ggplot(km_centers, aes(x = cluster, y = score, fill = variable)) +
  geom_col(position = "dodge") +
  theme_light() +
  scale_fill_viridis_d(option = "magma")
ggplot(km_centers, aes(x = score, y = variable, fill = variable)) +
  geom_col() +
  facet_wrap(vars(cluster)) +
  theme_light() +
  scale_fill_viridis_d(option = "turbo") +
  theme(legend.position = "none")
km_output2$cluster 

## DEMOTIV
survey_behaviour <- na.omit(survey_dataframe |>
                              dplyr::select(starts_with(c("DEMOTIV_"))) |>
                              scale())
fviz_nbclust(survey_behaviour, kmeans, method = "wss", k.max = 5)
k <- 3
km_output3 <- kmeans(survey_behaviour, centers = k, iter.max = 50, nstart = 25)
km_output3$centers
km_output3$size
km_centers <- as.data.frame(km_output3$centers)  |>
  mutate(cluster = as.factor(1:k)) |> 
  pivot_longer(cols = 1:ncol(km_output3[["centers"]]), values_to = "score", names_to = "variable") |> 
  mutate(variable = factor(variable, levels = colnames(km_output3[["centers"]])))
ggplot(km_centers, aes(x = cluster, y = score, fill = variable)) +
  geom_col(position = "dodge") +
  theme_light() +
  scale_fill_viridis_d(option = "magma")
ggplot(km_centers, aes(x = score, y = variable, fill = variable)) +
  geom_col() +
  facet_wrap(vars(cluster)) +
  theme_light() +
  scale_fill_viridis_d(option = "turbo") +
  theme(legend.position = "none")
km_output3$cluster 

## PREFER
survey_behaviour <- na.omit(survey_dataframe |>
                              dplyr::select(starts_with(c("PREFER_"))) |>
                              scale())
fviz_nbclust(survey_behaviour, kmeans, method = "wss", k.max = 15)
k <- 5
km_output4 <- kmeans(survey_behaviour, centers = k, iter.max = 50, nstart = 25)
km_output4$centers
km_output4$size
km_centers <- as.data.frame(km_output4$centers)  |>
  mutate(cluster = as.factor(1:k)) |> 
  pivot_longer(cols = 1:ncol(km_output4[["centers"]]), values_to = "score", names_to = "variable") |> 
  mutate(variable = factor(variable, levels = colnames(km_output4[["centers"]])))
ggplot(km_centers, aes(x = cluster, y = score, fill = variable)) +
  geom_col(position = "dodge") +
  theme_light() +
  scale_fill_viridis_d(option = "magma")
ggplot(km_centers, aes(x = score, y = variable, fill = variable)) +
  geom_col() +
  facet_wrap(vars(cluster)) +
  theme_light() +
  scale_fill_viridis_d(option = "turbo") +
  theme(legend.position = "none")
km_output4$cluster 

## SATIS
survey_behaviour <- na.omit(survey_dataframe |>
                              dplyr::select(starts_with(c("SATIS_"))) |>
                              scale())
fviz_nbclust(survey_behaviour, kmeans, method = "wss", k.max = 15)
k <- 5
km_output5 <- kmeans(survey_behaviour, centers = k, iter.max = 50, nstart = 25)
km_output5$centers
km_output5$size
km_centers <- as.data.frame(km_output5$centers)  |>
  mutate(cluster = as.factor(1:k)) |> 
  pivot_longer(cols = 1:ncol(km_output5[["centers"]]), values_to = "score", names_to = "variable") |> 
  mutate(variable = factor(variable, levels = colnames(km_output5[["centers"]])))
ggplot(km_centers, aes(x = cluster, y = score, fill = variable)) +
  geom_col(position = "dodge") +
  theme_light() +
  scale_fill_viridis_d(option = "magma")
ggplot(km_centers, aes(x = score, y = variable, fill = variable)) +
  geom_col() +
  facet_wrap(vars(cluster)) +
  theme_light() +
  scale_fill_viridis_d(option = "turbo") +
  theme(legend.position = "none")
km_output5$cluster

## Segments
## PREFERENCE
survey_PREFERENCE <- survey_dataframe |> 
  bind_cols(segment = as.factor(km_output4$cluster)) |>
  mutate(preference = case_when(segment == 1 ~ "Mostly Moderate Satisfaction", 
                                  segment == 2 ~ "Mostly Moderate Dissatisfaction",
                                  segment == 3 ~ "Mostly Major Satisfaction",
                                  segment == 4 ~ "Mostly Minor Satisfaction",
                                  segment == 5 ~ "Mostly Major Dissatisfaction"))

PlotXTabs2(survey_PREFERENCE, x = preference, y = INCOME)

## SATISFACTION
survey_SATISFACTION <- survey_dataframe |> 
  bind_cols(segment = as.factor(km_output5$cluster)) |>
  mutate(satisfaction = case_when(segment == 1 ~ "Moderate Dissatisfaction", 
                                  segment == 2 ~ "Major Satisfaction",
                                  segment == 3 ~ "Mixed Moderate",
                                  segment == 4 ~ "Minor Satisfaction",
                                  segment == 5 ~ "Mostly Major Dissatisfaction"))

PlotXTabs2(survey_SATISFACTION, x = satisfaction, y = INCOME)

## Cross Tab
table(survey_PREFERENCE$INCOME)
table(survey_PREFERENCE$preference)
table(survey_PREFERENCE$INCOME,survey_PREFERENCE$preference)
CrossTable(survey_PREFERENCE$preference, survey_PREFERENCE$INCOME, expected = TRUE)

## Visual CrossTab
PlotXTabs(survey_PREFERENCE, INCOME, preference)
PlotXTabs2(survey_PREFERENCE, preference, INCOME)

PlotXTabs2(survey_PREFERENCE, preference, INCOME) + scale_fill_viridis_d(option = "mako")

## Multiple Regression
## MOTIV
lm_survey_MOTIVOccasion <- lm(data = survey_dataframe, formula = Occasion ~ MOTIV_1 + MOTIV_2 + MOTIV_3 + MOTIV_4 + MOTIV_5)
summary(lm_survey_MOTIVOccasion)
lm_survey_MOTIVPreference <- lm(data = survey_dataframe, formula = Preference ~ MOTIV_1 + MOTIV_2 + MOTIV_3 + MOTIV_4 + MOTIV_5)
summary(lm_survey_MOTIVPreference)
lm_survey_MOTIVSatisfaction <- lm(data = survey_dataframe, formula = Satisfaction ~ MOTIV_1 + MOTIV_2 + MOTIV_3 + MOTIV_4 + MOTIV_5)
summary(lm_survey_MOTIVSatisfaction)

## OCCAS
lm_survey_OCCASMotivation <- lm(data = survey_dataframe, formula = Motivation ~ OCCAS_1 + OCCAS_2 + OCCAS_3)
summary(lm_survey_OCCASMotivation)
lm_survey_OCCASPreference <- lm(data = survey_dataframe, formula = Preference ~ OCCAS_1 + OCCAS_2 + OCCAS_3)
summary(lm_survey_OCCASPreference)
lm_survey_OCCASSatisfaction <- lm(data = survey_dataframe, formula = Satisfaction ~ OCCAS_1 + OCCAS_2 + OCCAS_3)
summary(lm_survey_OCCASSatisfaction)

## DEMOTIV
lm_survey_DEMOTIVPreference <- lm(data = survey_dataframe, formula = Preference ~ DEMOTIV_1 + DEMOTIV_2 + DEMOTIV_3 + DEMOTIV_4 + DEMOTIV_5 + DEMOTIV_6 + DEMOTIV_7 + DEMOTIV_8 + DEMOTIV_9)
summary(lm_survey_DEMOTIVPreference)
lm_survey_DEMOTIVSatisfaction <- lm(data = survey_dataframe, formula = Satisfaction ~ DEMOTIV_1 + DEMOTIV_2 + DEMOTIV_3 + DEMOTIV_4 + DEMOTIV_5 + DEMOTIV_6 + DEMOTIV_7 + DEMOTIV_8 + DEMOTIV_9)
summary(lm_survey_DEMOTIVSatisfaction)

