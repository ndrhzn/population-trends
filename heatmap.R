library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)

df <- read.csv('population.csv', stringsAsFactors = F)

# --------------------------------------------------------------------------------------------

df <- df %>% 
  rename(region = X) %>% 
  gather(key = 'year', value = 'rate', -region) %>% 
  mutate(year = str_replace_all(year, 'X', ''),
         region = str_replace_all(region, ' область', ''),
         region = str_replace_all(region, '\\(міськрада\\)', ''),
         region = str_replace_all(region, 'м. ', ''),
         region = str_replace_all(region, 'Автономна Республіка', 'АР'),
         year = as.numeric(year),
         rate = as.numeric(rate))

# --------------------------------------------------------------------------------------------

png(filename = 'rate.png', width = 1000, height = 700)
  
ggplot(df)+
  geom_tile(aes(x = year, 
                y = reorder(region, rate, order = T, FUN = mean, na.rm = T), 
                fill = rate), color = '#F3F7F7', size = 0)+
  scale_fill_gradient2(low = '#67001f', mid = '#f7f7f7', high = '#053061', 
                       na.value = '#F0F0F0',
                       limits = c(-15, 8),
                       breaks = c(-15, -8, 0, 8))+
  scale_x_continuous(position = 'top', 
                     breaks = c(1990, 1995, 2000, 2005, 2010, 2015),
                     #labels = c("'90", "'95", "'00", "'05", "'10", "'15"),
                     expand = c(0, 0))+
  scale_y_discrete(position = 'right')+
  guides(fill = guide_colorbar(title = 'природний приріст/скорочення населення, на 1000 осіб', 
                               title.position = 'top',
                               raster = F,
                               nbin = 24,
                               ticks = F))+
  labs(title = 'У більшості регіонів смертність\nпереважає народжуваність',
       caption = 'Дані: Державна служба статистики | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Mono', size = 12, color = '#3A3F4A'),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 12, color = '#5D646F'),
        legend.position = 'top',
        legend.justification = 'left',
        legend.key.width = unit(156, 'pt'),
        legend.key.height = unit(7.5, 'pt'),
        legend.title = element_text(size = 12, color = '#5D646F'),
        legend.text = element_text(family = 'Ubuntu Mono', size = 12, color = '#5D646F'),
        legend.margin = margin(b = -5, t = 10),
        plot.title = element_text(face = 'bold', size = 36, lineheight = 0.7),
        plot.caption = element_text(size = 12, color = '#5D646F', margin = margin(t = 20)),
        plot.background = element_rect(fill = '#F3F7F7'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()
