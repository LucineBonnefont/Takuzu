library(ggplot2)
library(reshape2)  # Assurez-vous que reshape2 est chargé

# Créer une grille de Takuzu (avec 0 et 1)
grid <- matrix(sample(c(0, 1), 225, replace = TRUE), nrow = 15, ncol = 15)

# Convertir la matrice en format long pour ggplot2
grid_long <- melt(grid)

# Créer l'objet ggplot
p <- ggplot(grid_long, aes(Var1, Var2, fill = factor(value))) +
  geom_tile() +
  scale_fill_manual(values = c("white", "white")) +  # Définir les couleurs pour les 0 et 1
  geom_text(aes(label = value), size = 6, color = "blue") +  # Ajouter les valeurs 0 et 1
  theme_void() +
  theme(legend.position = "none")

# Sauvegarder l'image avec ggsave
ggsave("background.png", plot = p, width = 10, height = 10)
