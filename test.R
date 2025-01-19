library(visualizeR)
library(ggplot2)

# Example usagea
# Sample data
data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(3, 7, 9, 5)
)

library(visualizeR)
library(ggplot2)
# Regular bar plot
p1 <-
  bar(
    df = data,
    x = "category",
    y = "value",
    flip = F
  ) +
  theme_bar(flip = F)
p1
