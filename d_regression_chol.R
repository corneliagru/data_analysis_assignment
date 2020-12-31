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
