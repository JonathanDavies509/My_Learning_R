##---Read_packages_required------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggpubr)


##---Import_data-----------------------------------------------------------------
sheet_names<-excel_sheets("D:/Mine/201 Stats for biochemists/My_Learning_R/data/41586_2019_1263_MOESM9_ESM.xlsx")
data<-read_excel("D:/Mine/201 Stats for biochemists/My_Learning_R/data/41586_2019_1263_MOESM9_ESM.xlsx",sheet_names[3])


##---Tidy_data-------------------------------------------------------------------
data %>%
  pivot_longer(-tissue) %>%
  separate(name, into=c(NA, "dtr_status")) %>%
  mutate(tissue = factor(tissue, c("% destained cartilage","% pannus tissue",
                                   "% bone erosion","% bone formation",
                                   "Micro CT bone erosion", "Micro CT bone formation")))-> tidy_data_2


##---Stats_for_plot--------------------------------------------------------------
my_comparisons <- list(c("minus","plus"))

tidy_data_2 %>%
  group_by(dtr_status,tissue) %>%
  summarise(sd = sd(value), value = mean(value)) -> tidy_data_stats


##---Create_plot-----------------------------------------------------------------
plot <- ggplot(tidy_data_2, aes(x=dtr_status, y=value, fill = dtr_status)) +
  stat_summary(fun="mean", geom="bar") +
  geom_errorbar(data = tidy_data_stats, aes(ymin= value-sd, ymax= value+sd), 
                width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("blue", "red")) + 
  facet_wrap(~tissue, scales ="free_y", strip.position = "left") +
  scale_x_discrete(labels = c(bquote(bold(DTR^'-')),bquote(bold(DTR^'+')))) +
  geom_jitter(width = 0.1, shape = 1)+
  stat_compare_means(comparisons = my_comparisons, method = "t.test", paired = TRUE, symnum.args = list(cutpoints = c(0,0.0001,0.005,0.001, 1), 
                                                                                                        symbols = c("****","***", "**", "ns")))+
  theme_classic()+
  theme(strip.background = element_rect(fill = "white", colour = "white"))+
  theme(legend.position = "none", strip.placement = "outside")+
  labs(x="DTR Status", y=NULL, title= "Figure 2d", subtitle= "From Croft et al, Nature, 2019")
plot





