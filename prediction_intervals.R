mtcars


m1<-lm(cyl~hp, data= mtcars)

predictions <- predict(m1, newdata=mtcars, interval="prediction", level=0.95)
predictions

mtcars2 <- cbind(mtcars,predictions)

confidence <- predict(m1, newdata=mtcars, interval="confidence", level=0.95)
confidence

confidence <- as.data.frame(cbind(mtcars$hp,confidence))


dplyr::rename(confidence, hp = V1) -> confidence

library(ggplot2)

plot<-ggplot(data=mtcars2, aes(x=hp, y=cyl)) +
  geom_point(col = "white") +
  geom_line(aes(y=fit), col = "purple") +
  geom_line(aes(y=lwr), col="pink", linetype="dotted") +
  geom_line(aes(y=upr), col="pink", linetype="dotted") +
  geom_line(data=confidence, aes(y=lwr), col="hotpink", linetype="dashed") +
  geom_line(data=confidence, aes(y=upr), col="hotpink", linetype="dashed") +
  theme_minimal() +
  theme_dark()

plot
