
# b -----------------------------------------------------------------------

# b. Is there a correlation between systolic and diastolic blood pressure?


cor(data$SBP, data$DBP)
cor(data$SBP, data$DBP, method = "spearman")

ggplot(data, aes(SBP, DBP)) +
  geom_point() +
  geom_smooth()

mod0 <- lm(SBP ~ DBP, data)
summary(mod0)

predict(mod0, data.frame(DBP  =85))


