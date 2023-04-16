library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)


#'Duomenų failo importavimas

data = read_csv("../data/lab_sodra.csv")


#'Duomenų atrinkimas pagal ekonominės veiklos kodą

filtered = data %>%
  filter(ecoActCode == 682000)
summary(filtered)

#'Pirma užduotis
#'Vidutinių atlyginimų histograma

filtered %>% 
  ggplot(aes(avgWage)) +
  scale_y_sqrt()+
  geom_histogram(bins = 100, fill='navy', alpha = 0.6, col = 'white') +
  labs(title = 'Vidutinių atlyginimų histograma', x = 'Average wage', y = 'Count') +
  theme(plot.title = element_text(hjust = 0.5, size = rel(2)),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24, face = "bold"),
        panel.background = element_rect(fill = "grey90"),
        plot.margin = margin(2,2,2,2, "cm"),
        plot.background = element_rect(fill = "grey95", colour = "navy",  linewidth = 2)
  )
ggsave(filename = "plot1.png", path = "../img/",  width = 20, height = 10)
dev.off()

#'Antra užduotis
#'5 įmonių vidutinio atlyginimo kitimo dinamika metų eigoje

top5 <- filtered %>% 
  group_by(name) %>% 
  summarise(avgWage = mean(avgWage)) %>% 
  arrange(desc(avgWage)) %>% 
  head(5)

top5_d <- data %>% 
  filter(name %in% top5$name) %>% 
  mutate("month" = as.numeric(substr(month, 5, 6)))

euro <- label_dollar(
  prefix = "",
  suffix = "\u20ac",
  big.mark = ".",
  decimal.mark = ","
)

top5_d %>% 
  ggplot(aes(month, avgWage)) +
  geom_line(aes(col = name), linewidth = 1) +
  geom_point(color = "white", size = 1)+
  scale_y_continuous(labels = euro)+
  scale_x_continuous(breaks = 1:12) +
  labs(title = 'Vidutinio atlyginimo kitimo dinamika metų eigoje', x = 'Month', y = 'Average wage', col = 'Companies') +
  theme(plot.title = element_text(hjust = 0.5, size = rel(2)),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 14), legend.title = element_text(size = 16),
        panel.background = element_rect(fill = "grey90"),
        plot.margin = margin(2,2,2,2, "cm"),
        plot.background = element_rect(fill = "grey95", colour = "navy", linewidth = 2),
        legend.background = element_rect(fill = "grey95"),
        legend.position = "bottom")+
  scale_color_manual(values=c('#3D9DD9', '#F2B705', '#F28705', '#F24405', '#730202'))
ggsave(filename = "plot2.png", path = "../img/",  width = 20, height = 10)
dev.off()

#'Trečia užduotis
#'Maksimalus apdraustųjų darbuotojų skaičius per metus

maxIns <- top5_d %>% 
  group_by(name) %>% 
  summarise(maxInsured = max(numInsured))

maxIns %>% 
  ggplot(aes(reorder(name, desc(maxInsured)), maxInsured)) +
  scale_y_log10()+
  geom_bar(aes(fill = name), stat = "identity", position = "dodge")+
  geom_label(aes(name, label = maxInsured), fill = 'grey50',size = 10, color = 'white')+
  labs(title = 'Maksimalus apdraustųjų darbuotojų skaičius per metus', x = 'Company', y = 'Max insured')+
  theme(plot.title = element_text(hjust = 0.5, size = rel(2)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 24, face = "bold"),
        panel.background = element_rect(fill = "grey90"),
        plot.margin = margin(2,2,2,2, "cm"),
        plot.background = element_rect(fill = "grey95", colour = "navy", linewidth = 2),
        legend.position = "none") +
  scale_fill_manual(values=c('#3D9DD9', '#F2B705', '#F28705', '#F24405', '#730202'))
ggsave(filename = "plot3.png", path = "../img/",  width = 20, height = 10)
dev.off()
