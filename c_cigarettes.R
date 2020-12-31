# c -----------------------------------------------------------------------

# c. Is the number of cigarettes associated with the patients’ systolic
# pressure?


ggplot(data, aes(CIG, SBP, col = MALE)) +
  facet_wrap(~MALE)+
  geom_point() +
  geom_smooth()

ggplot(data, aes(SBP, y = cig_cat,  col = MALE)) +
  geom_boxplot()

library(dplyr)
data %>% group_by(MALE,cig_cat) %>%
  summarise(mean(SBP), n())

ggplot(data, aes(SBP, col = cig_cat)) +
  geom_density()


mod1 <- lm(SBP ~ cig_cat, data)
summary(mod1)
plot(mod1)


mod2 <- lm(SBP ~ CIG, data)
summary(mod2)
plot(mod2)


mod3 <- lm(SBP ~ cig_cat + AGE + CHOL + MALE+ DBP, data)
summary(mod3)



mod4 <- lm(SBP ~ CIG + AGE + CHOL + MALE+ DBP, data)
summary(mod4)

stargazer(mod1, mod3,mod4, no.space=TRUE, omit.stat=c("LL","ser","f"), single.row=TRUE)

