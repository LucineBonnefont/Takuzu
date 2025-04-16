library(ggplot2)
library(reshape2)


grid <- matrix(sample(c(0, 1), 225, replace = TRUE), nrow = 15, ncol = 15)

grid_long <- melt(grid)

p <- ggplot(grid_long, aes(Var1, Var2, fill = factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("white", "white")) +  
  geom_text(aes(label = value), size = 6, color = "blue") + 
  theme_void() +
  theme(legend.position = "none")

ggsave("background.png", plot = p, width = 10, height = 10)
