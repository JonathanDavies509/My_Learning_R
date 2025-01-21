##----Gapminder----------------------------------------------------------------------
str(gapminder)

data <- gapminder

data %>%
  filter(year==1952) -> data_1952

##----starwars----------------------------------------------------------------------
data_2 <- starwars

str(data_2)

luke <- filter(starwars, name=="Luke Skywalker")

luke$films

##----plot--------------------------------------------------------------------------

## all not necessary after all
pop_cuts = c(10000, 100000, 1000000, 10000000, 100000000, 1000000000)

data_1952 %>%
  group_by(pop_cat = cut(pop, breaks = pop_cuts)) -> data_1952

data_1952$pop_cat <- as.integer(data_1952$pop_cat)
##

plot <- ggplot(data, aes(gdpPercap, lifeExp, colour=continent, size=pop)) +
  geom_point() +
  scale_x_log10() +
  ylab("Life Expectancy") +
  xlab("GDP per Capita")

plot

##----animating plot----------------------------------------------------------------
library(gganimate)
library(gifski)

anim <- plot + transition_time(year) +
  labs(title = "Year: {frame_time}")

gif <- animate(anim, renderer = gifski_renderer())

gif

last_animation()



