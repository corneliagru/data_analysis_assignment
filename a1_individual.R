# Task:
# https://free.openeclass.org/modules/document/file.php/SC517/Assignments/Assignment%201/INDEX_DIRECTIONS_eng.html



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
library(gridExtra)
library(haven)
library(dplyr)


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


#density only one
ggplot(data, aes(x = SBP))+
  geom_density()+
  #geom_vline(xintercept = mean(data$DBP))+
  #geom_vline(xintercept = median(data$DBP))+
  geom_vline(xintercept = 120)+
  geom_vline(xintercept = 130)+
  geom_vline(xintercept = 140)+
  geom_vline(xintercept = 160)+
  geom_vline(xintercept = 180)



dia_dens <- with(density(data$DBP), data.frame(x, y))

p_dbp <- ggplot(data = dia_dens, mapping = aes(x = x, y = y)) +
  geom_area(aes(x = ifelse(x<80 , x, 0)), fill = "#1a9850", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>80 & x<=85 , x, 0)), fill = "#91cf60", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>85 & x<=90 , x, 0)), fill = "#d9ef8b", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>90 & x<=100 , x, 0)), fill = "#fee08b", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>100 & x<=110 , x, 0)), fill = "#fc8d59", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>110, x, 0)), fill = "#d73027", alpha = 0.9) +
  geom_line() +
  geom_vline(linetype="dashed", xintercept = 80)+
  geom_vline(linetype="dashed", xintercept = 85)+
  geom_vline(linetype="dashed", xintercept = 90)+
  geom_vline(linetype="dashed", xintercept = 100)+
  geom_vline(linetype="dashed", xintercept = 110)+
  scale_y_continuous(name = "Density", limits = c(0, max(dia_dens$y)))+
  scale_x_continuous(name = "DBP", limits = c(min(dia_dens[dia_dens$y > 0,"x"]),
                                              max(dia_dens[dia_dens$y > 0,"x"]))) 



sys_dens <- with(density(data$SBP), data.frame(x, y))

p_sbp <- ggplot(data = sys_dens, mapping = aes(x = x, y = y)) +
  geom_area(aes(x = ifelse(x<120 , x, 0)), fill = "#1a9850", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>=120 & x<130 , x, 0)), fill = "#91cf60", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>=130 & x<140 , x, 0)), fill = "#d9ef8b", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>=140 & x<160 , x, 0)), fill = "#fee08b", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>=160 & x<180 , x, 0)), fill = "#fc8d59", alpha = 0.9) +
  geom_area(aes(x = ifelse(x>=180, x, 0)), fill = "#d73027", alpha = 0.9) +
  geom_line() +
  geom_vline(linetype="dashed", xintercept = 120)+
  geom_vline(linetype="dashed", xintercept = 130)+
  geom_vline(linetype="dashed", xintercept = 140)+
  geom_vline(linetype="dashed", xintercept = 160)+
  geom_vline(linetype="dashed", xintercept = 180)+
  scale_y_continuous(name = "Density", limits = c(0, max(sys_dens$y)))+
  scale_x_continuous(name = "SBP", limits = c(min(sys_dens[sys_dens$y > 0,"x"]),
                                              max(sys_dens[sys_dens$y > 0,"x"]))) 



arrangeGrob(p_sbp, p_dbp, nrow = 1)



ggsave("plots/bp_density.png", arrangeGrob(p_sbp, p_dbp, nrow = 1),
       width = 18, height = 7, units = "cm")



# check how many in ok levels

#optimal normal
mean(data$SBP<140)
mean(data$DBP<90)

#bad
mean(data$SBP>180)
mean(data$DBP>110)




ggplot(data, aes(CHOL))+
  geom_density() +
  ylab("Density")
ggsave("plots/chol_density.png", width = 13, height = 7, units = "cm")



#cig
table(data$CIG)

#cig
ggplot(data, aes(CIG))+
  geom_bar()+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_text(
    stat = "count",
    aes(label = scales::percent((..count..) / sum(..count..))),
    vjust = +1.3,
    col = "red"  )





# boxplot each var
ggplot(data_long, aes(x = var, y = value, fill = var)) +
  geom_boxplot()




# barplot smoking cat
p_cig <- ggplot(data, aes(cig_cat)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_text(
    stat = "count",
    aes(label = scales::percent((..count..) / sum(..count..))),
    vjust = 1.4,
    col = c(rep("white", 3), "black")
  ) +
  ylim(-30, 830) +
  labs(x = "Cigarette Category", y = "") #title = "Smoking Behaviour"
ggsave("plots/smoking.png", width = 13, height = 7, units = "cm")




# barplot male
p_male <- ggplot(data, aes(MALE)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_text(
    stat = "count",
    aes(label = scales::percent((..count..) / sum(..count..))),
    vjust = +1.3,
    col = "white"
  ) +
  ylim(0, 790) +
  labs(x = "Gender", y = "") + #, title = "Distribution of Gender"
  scale_x_discrete(labels = c("male", "female"))
ggsave("plots/gender.png", width = 13, height = 7, units = "cm")



# barplot age
p_age <- ggplot(data, aes(AGE)) +
  geom_bar() +
 # geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  #ylim(0, 115) +
  labs(x = "Age", y = "") #, title = "Age Distribution"
ggsave("plots/age.png", width = 13, units = "cm")


ggsave("plots/age_male_cig.png", arrangeGrob(p_age, p_male, p_cig, nrow = 1),
       width = 21, height = 7, units = "cm")

