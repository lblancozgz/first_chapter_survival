library(ggplot2)
library(ggridges)
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(RColorBrewer)
seguimientopal <- read_excel("Escritorio/probatinas/seguimientopal.xlsx", 
                             col_types = c("numeric", "date", "date", 
                                           "text", "numeric"))
seguimientopal <- seguimientopal[-427, ]
seguimientopal = seguimientopal %>% #vamos a crear dos columnas que separen start_date en año y mes (lo que nos interesa)
  mutate(start_date = ymd(start_date)) %>%
  mutate_at(vars(start_date), funs(year, month))
seguimientopal$month<- as.factor(seguimientopal$month)


Months <- c(          #para cambiar titulo de cada facet
  `6` = "June",
  `7` = "July",
  `8` = "August",
  `9` = "September",
  `10` = "October")
data_text <- data.frame(label = c("n=17", "n=81", "n=115", "n=119", "n=94"),  # Create data for text
                        total_lived = c(17,82, 115, 119, 94),
                        month = c(6,7,8,9,10))
data_text$label_f = factor(data_text$label, levels=c("n=17", "n=81", "n=115", "n=119", "n=94"))
data_text$month_f = factor(data_text$month, levels=c("6", "7", "8", "9", "10"))
seguimientopal$month_f = factor(seguimientopal$month, levels=c("6", "7", "8", "9", "10"))


plotpalafolls <- ggplot(seguimientopal, aes(x = total_lived, y = 1)) +
  geom_density_ridges(aes(fill = month_f), scale = 0.96) +
  scale_fill_brewer(palette = "RdYlBu",  name = "Months", labels = c("June", "July", "August", "September", "October")) +
  theme_bw() +  coord_flip() + scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
  facet_grid(~ month_f, scales = "free", drop = TRUE, labeller = as_labeller(Months)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_text(
    data    = data_text,
    mapping = aes(x = -Inf, y = -Inf, label = label_f),
    hjust   = -0.75,
    vjust   = -1)
