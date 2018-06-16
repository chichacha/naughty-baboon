
## 445 Calendar Visually

## 2018 Calendar

library(tidyverse)
library(hrbrthemes)
library(scales)
library(lubridate)
library(ggthemes)

## Set Global Option for Lubridate 
options("lubridate.week.start"=1)  ## 1 sets to Monday
Sys.setenv(TZ = "America/Vancouver")

cur.date <- today()

calendar.tbl <- tibble(
  date = seq.Date(from=floor_date(cur.date,"year"), to=ceiling_date(cur.date,"year")-1, by="day"),
  yr = year(date),
  mo = month(date, label=T),
  wk = week(date),
  day = day(date),
  wday = wday(date, label=T),
  
  wk.start.date = floor_date(date,"week"),
  wk.end.date = ceiling_date(date,"week")-1,
  wk.num.445  = dense_rank(wk.start.date),
  
  mo.445 = dense_rank(cut(wk.num.445,c(0,4,8,13,17,21,26,30,34,39,43,47,52,56))), ## how can i do this more nicely
  qt.445 = paste0("Q",quarter(mo.445)),
  
  yqm.445  = paste0(yr, " ", qt.445, " Period " ,mo.445, "(",month.abb[mo.445],")")
)


## So what's today?
calendar.tbl %>% filter(date==today())


ggplot(calendar.tbl %>% filter(mo.445<=12), aes(x=wday, y=wk.num.445)) + 
  geom_tile(color="white", aes(fill=factor(mo))) +
  geom_text(aes(label=format(date,"%e")), family="Roboto Condensed", color="white") +
  scale_y_reverse(breaks=c(1:53)) + 
  facet_wrap(~yqm.445, scales="free", ncol=3) + 
  theme_ipsum_rc() +
  labs(title="2018 Calendar", subtitle="With Week Number Listed") +
  scale_fill_tableau("cyclic", name="Gregorian Month") +
  guides(fill = guide_legend(nrow =1 , position="left")) +
  theme(legend.position="")

ggsave(filename="Images/2018_445_Cakebdar.png", device = "png", width=16, height=9)


