library(dplyr)
library(plotly)
library(learnbayes)
install.packages("LearnBayes")
packageVersion('plotly')

f <- file.choose()
test1 <- read.csv(f, sep = ",", header = T)
colnames(test1)
x <- test1$Ash
y <- test1$TotalPhenols

plot(x,y)


pc1 <- ggplot(subset(test1), aes(x = x, y = y, color = Flavanoids))
pc1
pc2 <- pc1 + geom_point() + geom_smooth(method = "lm", se = TRUE)
pc2

summary(pc2)

