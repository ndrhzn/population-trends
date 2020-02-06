library(ggplot2)
library(tidyr)
library(geofacet)

df <- read.csv('data/population_trends.csv', stringsAsFactors = FALSE)

df <- df %>% gather(key = type, value = ratio, -name, -year)
df$ratio <- as.numeric(df$ratio)
df$name <- trimws(df$name)

png(filename = 'geofacet.png', width = 1200, height = 1000)

ggplot(df)+
  geom_hline(yintercept = 0, size = 0.3, color = '#3A3F4A')+
  geom_ribbon(aes(x = year, ymin = 0, ymax = ratio, fill = type), alpha = 0.075)+
  geom_line(aes(x = year, y = ratio, color = type), size = 1)+
  facet_geo(~name, grid = ukraine)+
  scale_x_continuous(breaks = seq(1990, 2015, 5), labels = c("'90", "'95", "'00", "'05", "'10", "'15"))+
  scale_color_manual(values = c('#276419', '#c51b7d'), labels = c('сільська місцевість ', 'міські поселення'))+
  scale_fill_manual(values = c('#276419', '#c51b7d'), labels = c('сільська місцевість ', 'міські поселення'))+
  labs(title = 'Природний приріст/скорочення населення у 1990-2015 роках',
       subtitle = 'На 1000 населення, обидві статі',
       caption = 'Дані: Державна служба статистики України | Візуалізація: Textura.in.ua')+
  theme_minimal(base_family = 'Ubuntu Condensed')+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 16),
        panel.spacing.x = unit(1.25, 'lines'),
        panel.spacing.y = unit(1.50, 'lines'),
        legend.position = 'top',
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold', size = 40, margin = margin(b = 10, t = 20)),
        plot.subtitle = element_text(size = 20, face = 'plain', margin = margin(b = 30)),
        plot.caption = element_text(size = 16, margin = margin(b = 10, t = 50), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()
