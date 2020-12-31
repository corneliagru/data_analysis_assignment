
add_loss <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(...)
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
ggsave("plots/pairs.png", width = 13, height = 10, units = "cm")

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

#Differences between male and female patients
ggplot(data_long, aes(value,y = var,  fill = MALE))+
  geom_boxplot()+
  labs(title = "", y = "", x = "")
ggsave("plots/var_per_gender.png", width = 13, height = 7, units = "cm")



p <- ggplot(data, aes(DBP, fill = MALE))+
  geom_boxplot(alpha = 0.5)
ggplot_build(p)$data





ggplot(data_long[data_long$var != "CIG",], aes(value,y = var,  fill = cig_cat))+
  geom_boxplot()+
  labs(title = "", y = "", x = "")
ggsave("plots/var_per_cig_cat.png", width = 13, height = 7, units = "cm")



data %>% 
  group_by(cig_cat) %>% 
  summarise(quantile(DBP, 0.25),median(DBP), quantile(DBP, 0.75))



