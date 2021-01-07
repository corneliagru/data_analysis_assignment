# d -----------------------------------------------------------------------

# d. Estimate a linear model that examines the association of cholesterol levels
# with the rest of the variables in the dataset

mod3a <- lm(CHOL ~ AGE + SBP + DBP + cig_cat + MALE, data)
summary(mod3a)
plot(mod3a)


mod3 <- lm(log(CHOL) ~ AGE*MALE + SBP + DBP + CIG, data)
summary(mod3)
plot(mod3)


extractAIC(mod3)

car::vif(mod3)

plot(data$AGE, data$CHOL)


#default is both
step3 <- step(mod3, k = log(nrow(data)))



#both
f_both  <-  log(CHOL) ~ AGE + MALE + DBP + AGE:MALE




# mod intercept only
mod_int <- lm(log(CHOL) ~ 1, data)
step4 <- step(mod_int, scope = list(lower = mod_int, upper = mod3), 
              direction = "both")
summary(step4)
plot(step4)

plot(step4$fitted.values, log(data$CHOL), xlab = "Fitted Values",
     ylab = "Response", main = expression("log(CHOL), R"^2~"= 5%"))

p_reg_chol <- ggplot(step4, aes(step4$fitted.values, log(data$CHOL))) +
  geom_point(shape = 1, alpha = 0.7)+
  labs(x = "Fitted Values", y = "Response", title = expression("log(CHOL), R"^2~"= 5%"))
ggsave("plots/fit_resp_chol.png", width = 13, height = 9, units = "cm")


ggsave("plots/fit_resp_both.png", arrangeGrob(p_reg_chol, p_reg_sbp, nrow = 1),
       width = 14, height = 7, units = "cm")

stargazer::stargazer(step4, label = "tab:reg_chol", no.space=TRUE,
                     omit.stat=c("LL","ser","f"), single.row=TRUE)







# splines -----------------------------------------------------------------





mod4 <- gam(CHOL ~ s(AGE, bs = "ps", by = MALE) + DBP
            + MALE, family = gaussian, data = data)
summary(mod4)

png(filename = "plots/splines.png", width = 14, height =7, units = "cm", res = 300)
par(mfrow = c(1,2), mar = c(4.5, 5, 2, 1))
plot(mod4, select = 1, ylab = "s(AGE_female)")
abline(h = 0, col = "red3")
abline(v = 49, lty = "dotted", lwd = 0.5)
abline(v = 52, lty = "dotted", lwd = 0.5)
abline(v = 59, lty = "dotted", lwd = 0.5)
plot(mod4, select = 2, ylab = "s(AGE_male)")
abline(h = 0, col = "red3")
dev.off()


gam.check(mod4)
mod4$de


stargazer::stargazer(mod4, label = "tab:reg_chol", no.space=TRUE,
                     omit.stat=c("LL","ser","f"), single.row=TRUE)
