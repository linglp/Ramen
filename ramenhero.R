library(tidyverse)
library(dplyr)
library(ggplot2)
library (plyr)
library(emojifont)

setwd("~/Downloads")

ramen <- read_csv("ramen-ratings.csv", col_types= cols(Stars = col_number()
                                                       ))



#filter out data
udon<- ramen %>%
  filter(Country == "Japan", str_detect(Variety, "Udon")) %>%
  select(Brand, Stars,Variety)


#other ways to sort a dataframe
#udon_sorted <- udon[order(-udon$Stars),] #don't forget ","

udon_top <- head(arrange(udon,desc(Stars)), n = 25)

#create unique ID (based on sort order)
udon_top <- tibble::rowid_to_column(udon_top, "ID")

#create a new column
udon_top$Brand_variety <- paste(udon_top$Brand, udon_top$Variety, sep="-")



##new
ggplot() + 
geom_polygon(aes(x = c(-30, 32, 32, -30), y = c(0.35, 0.12, 0.2, 0.63)),
               fill = "#404040") +
  geom_col(data = udon_top, aes(ID,Stars), fill = "#FFFEEB") + 
  geom_text(data = udon_top, aes(x=ID, y=Stars, label=Brand_variety), angle = 90, size=1.2,
            position = position_stack(vjust=0.5), color="black", fontface="bold") +
  geom_polygon(aes(x = c(-30, 32, 32, -30), y = c(0.02,0.7, 0.8, 0.3)),
                   fill = "#404040") +
  scale_y_reverse(
    breaks = c(1,2,3,4,5),
    limits = c(9, -9), 
  ) + theme_void()+
  theme(
    panel.background = element_rect(fill = "steel blue", color = "steel blue"),
  )+
  geom_text(aes(x = 32, y = 8, label = "Top 25 Japanese Udon Ramen"),
            family = "Comic Sans MS", hjust = 1,
            color = "white", size = 4.5) +
  geom_text(aes(x = 32, y = 9, label = "Source:Kaggle | Author: Lingling"),
            family = "Comic Sans MS", hjust = 1,
            color = "white", size = 3.5)+
  annotate("text", x = -20, y = 5, size=8,  label="*****", color="firebrick4") +
  annotate("text", x = -20, y = 4, size=8, label="****", color="firebrick4") +
  annotate("text", x = -20, y = 3, size=8, label="***", color="firebrick4")

ggsave("ramen.png", height = 7, width = 5, dpi = 300)
