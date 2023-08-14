# Load required libraries
library(ggplot2)

# Generate example data (random normal distribution)
set.seed(123)
data <- rnorm(1000, mean = 0, sd = 1) %>% as.data.frame()
colnames(data) <- "data"
# Create a ggplot to visualize the distribution
ggplot(data.frame(values = data)) +
  # geom_histogram(binwidth = 0.1, color = "black", fill = "lightblue", alpha = 0.7) +
  # geom_density(color = "red") +
  labs(title = "Distribution Plot",
       x = "Values",
       y = "Density") +
  theme_minimal()

ggplot(data, aes(x = data)) +
  geom_histogram(aes(y = ..density..), fill = "grey") +
  geom_density() +
  labs(title = "Distribution Plot",
       x = "Values",
       y = "Density") +
  theme_minimal()