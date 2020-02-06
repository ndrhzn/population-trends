library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)
library(geofacet)
source('custom_grid.R')

df <- read.csv('data/population_trends.csv', stringsAsFactors = FALSE)

df <- df %>% 
  gather(key = type, value = ratio, -region, -year) %>% 
  mutate(region = str_replace_all(region, ' область', ''),
         region = str_replace_all(region, '\\(міськрада\\)', ''),
         region = str_replace_all(region, 'Автономна Республіка', 'АР'),
         region = trimws(region))

png(filename = 'geofacet.png', width = 1000, height = 800)

ggplot(df)+
  geom_hline(yintercept = 0, size = 0.3, color = '#3A3F4A')+
  geom_ribbon(aes(x = year, ymin = 0, ymax = ratio, fill = type), alpha = 0.075)+
  geom_line(aes(x = year, y = ratio, color = type), size = 1)+
  facet_geo(~region, grid = ukraine)+
  scale_x_continuous(breaks = seq(1990, 2015, 5), 
                     labels = c("90", "95", "00", "05", "10", "15"))+
  scale_color_manual(values = c('#276419', '#c51b7d'), 
                     aesthetics = c("color", "fill"))+
  labs(title = 'Лише в Києві народжуваність переважає смертність',
       subtitle = 'Природний приріст/скорочення населення у <span style="color:#c51b7d"><b>міських поселеннях</b></span> та <span style="color:#276419"><b>сільській місцевості</b></span>, на 1000 населення',
       caption = 'Дані: Державна служба статистики України, 1990-2018 | Візуалізація: Textura.in.ua')+
  theme_minimal(base_family = 'Ubuntu Condensed')+
  theme(legend.position = 'none',
        text = element_text(family = 'Ubuntu Mono', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(color = '#3A3F4A', size = 12),
        panel.spacing.x = unit(1.1, 'lines'),
        panel.spacing.y = unit(1.1, 'lines'),
        panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold', size = 36, margin = margin(b = 5, t = 20)),
        plot.title.position = 'plot',
        plot.subtitle = element_markdown(size = 18, face = 'plain', margin = margin(b = 10)),
        plot.caption = element_text(size = 12, color = '#5D646F', margin = margin(t = 20)),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(1, 1, 1, 1), 'cm'))

dev.off()
