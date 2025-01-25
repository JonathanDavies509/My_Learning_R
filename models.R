data <- mtcars

library(tidyverse)

plot <- ggplot(data, aes(x=disp, y=hp)) +
  geom_point()
plot


model <- lm(hp~disp, data=data)
summary(model)

plot(hp~disp, data)

max(data$disp)

seq <- data.frame(seq(71.1, 472, length = 100))
colnames(seq) <- "disp"


## y = 0.4375(x) + 45.7345

fit <- data.frame(seq, predict(model, seq, se.fit = T))

plot(hp~disp, data)

plot + geom_point(data=fit, aes(x=disp, y=fit))

  