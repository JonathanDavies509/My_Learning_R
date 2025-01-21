data <- readxl::read_xlsx("C:/Users/jd291/OneDrive - University of Edinburgh/Documents/My_Learning_R/gather_test.xlsx")

library(tidyverse)

data %>%
  gather('DrugA','DrugB', key="Drug", value="Survival") -> new_data

plot <- ggplot(new_data, aes(x=Time_point, y=Survival, col=Drug)) +
  geom_line() +
  theme_classic() +
  theme_bw()
plot

my_comparisons <- list(c("DrugA","DrugB"))

new_data %>%
  group_by(Drug) %>%
  summarise(sd = sd(Survival), value = mean(Survival)) -> stats

plot <- ggplot(new_data, aes(x=Time_point, y=Survival, col=Drug)) +
  geom_line() +
  stat_summary(fun="mean", geom="line") +
  theme_classic() +
  theme_bw()
plot