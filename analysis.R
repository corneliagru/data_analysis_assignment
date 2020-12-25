# Task:
# https://free.openeclass.org/modules/document/
# file.php/SC517/Assignments/Assignment%201/INDEX_DIRECTIONS_eng.html
#


# AGE: Patients’ age
# SBP: Systolic Blood Pressure
# DBP: Diastolic Blood Pressure
# CHOL: Total Cholesterol level
# CIG: Number of cigarettes smoked per day
# MALE: Gender, 0 for Females, 1 for Males



# import ------------------------------------------------------------------
library(GGally)
library(tidyr)
library(ggplot2)
library(haven)


data <- read_sav("12_framdata.sav")


# You are asked to answer the following questions by analyzing the data


# a -----------------------------------------------------------------------

# a. Present suitable descriptive measures for all the variables individually,
# as well as for their (pairwise) associations

# remove NA in CIG for one observation
data <- na.omit(data)
data <- data[, c("AGE", "SBP", "DBP", "CHOL", "CIG", "MALE", "cig_cat")]


data$cig_cat <- factor(data$cig_cat)
data$MALE <- factor(data$MALE)


summary(data)

table(data$CIG, data$cig_cat)

# cat
# 0 = no cigarettes
# 1 = 1-20 per day
# 2 = 25-40
# 3 = 45-60


data_long <- data %>%
  pivot_longer(
    cols = c(AGE, SBP, DBP, CHOL, CIG),
    names_to = "var"
  )

data_long$var <- factor(data_long$var)




# plots individual --------------------------------------------------------

# density matrix
ggplot(data_long, aes(value)) +
  geom_density() +
  facet_wrap(~var, scales = "free") +
  labs(x = "", y = "Density", title = "Density of continuous variables")

ggsave("plots/density.png", width = 13, units = "cm")



# boxplot each var
ggplot(data_long, aes(x = var, y = value, fill = var)) +
  geom_boxplot()



# barplot smoking cat
ggplot(data, aes(cig_cat)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_text(
    stat = "count",
    aes(label = scales::percent((..count..) / sum(..count..))),
    vjust = +1.3,
    col = c(rep("white", 3), "black")
  ) +
  ylim(-30, 830) +
  labs(x = "Cigarette Category", y = "Count", title = "Smoking Behaviour")




# barplot male
ggplot(data, aes(MALE)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_text(
    stat = "count",
    aes(label = scales::percent((..count..) / sum(..count..))),
    vjust = +1.3,
    col = "white"
  ) +
  ylim(0, 790) +
  labs(x = "Gender", y = "Count", title = "Distribution of Gender") +
  scale_x_discrete(labels = c("male", "female"))
ggsave("plots/gender.png", width = 13, units = "cm")



# barplot age
ggplot(data, aes(AGE)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  ylim(0, 115) +
  labs(x = "Age", y = "Count", title = "Age Distribution")
ggsave("plots/age.png", width = 13, units = "cm")



add_loss <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}


gpairs_lower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow -1
  
  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1
  
  g
}

# Default loess curve    
ggpairs(data[1:4], lower = list(continuous = add_loss))
gp_plot <- ggpairs(data[1:4],
        lower = list(continuous = "smooth_loess"),
        upper  = list(continuous = "blank"),
        diag  = list(continuous = "blankDiag"))

gpairs_lower(gp_plot)





# continuous vars
cont <- c("AGE", "SBP", "DBP", "CHOL", "CIG")

for (i in 1:4) {
  for (j in 2:5) {
    if (i != j) {
      p <- ggplot(data, aes_string(cont[i], cont[j])) +
        geom_point() +
        geom_smooth()

      print(p)
    }
  }
}


# per gender


# b -----------------------------------------------------------------------

# b. Is there a correlation between systolic and diastolic blood pressure?


cor(data$SBP, data$DBP)
cor(data$SBP, data$DBP, method = "spearman")

ggplot(data, aes(SBP, DBP)) +
  geom_point() +
  geom_smooth()

mod0 <- lm(SBP ~ DBP, data)
summary(mod0)


# c -----------------------------------------------------------------------

# c. Is the number of cigarettes associated with the patients’ systolic
# pressure?


ggplot(data, aes(CIG, SBP)) +
  geom_point() +
  geom_smooth()


ggplot(data, aes(SBP, col = cig_cat)) +
  geom_density()


mod1 <- lm(SBP ~ cig_cat, data)
summary(mod1)

mod2 <- lm(SBP ~ CIG, data)
summary(mod2)

# d -----------------------------------------------------------------------

# d. Estimate a linear model that examines the association of cholesterol levels
# with the rest of the variables in the dataset


mod3 <- lm(log(CHOL) ~ AGE + SBP + DBP + cig_cat + MALE, data)
summary(mod3)
plot(mod3)

car::vif(mod3)



library(mgcv)

mod4 <- gam(CHOL ~ s(AGE, bs = "ps") + s(SBP, bs = "ps") + s(DBP, bs = "ps")
  + cig_cat + MALE, family = gaussian, data = data)
plot(mod4)
