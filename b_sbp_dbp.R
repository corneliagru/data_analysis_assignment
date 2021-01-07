
# b -----------------------------------------------------------------------

# b. Is there a correlation between systolic and diastolic blood pressure?


cor(data$SBP, data$DBP)
cor(data$SBP, data$DBP, method = "spearman")

ggplot(data, aes(SBP, DBP)) +
  geom_point() +
  geom_smooth()

mod0 <- lm(log(SBP) ~ DBP, data)
summary(mod0)
plot(mod0)



png(filename = "plots/fit_resp_sbp.png", width = 13, height =9, units = "cm", res = 100)
plot(mod0$fitted.values, data$SBP, xlab = "Fitted Values", ylab = "Response",
     main = expression("SBP, R"^2~"= 63%"))
dev.off()
predict(mod0, data.frame(DBP  =85))





p_reg_sbp <- ggplot(mod0, aes(mod0$fitted.values, data$SBP)) +
  geom_point(shape = 1, alpha = 0.7)+
  labs(x = "Fitted Values", y = "Response", title = expression("SBP, R"^2~"= 63%"))
ggsave("plots/fit_resp_sbp.png", width = 13, height = 9, units = "cm")








