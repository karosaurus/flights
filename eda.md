```R


lapply(list("tidyverse","lubridate","magrittr"
,"ggplot2","alr3","gridExtra"), require, c=1)

mymonths <- c("Jan","Feb","Mar", "Apr","May","Jun",
              "Jul","Aug","Sep", "Oct","Nov","Dec")





df <-
  read.csv("./655176550_12017_3428_airline_delay_causes.csv") %>% ## import data
  filter(grepl("AA|DL|WN|UA", carrier)) %>%                   ## subset
  select(year, arr_del15, arr_flights, X.month, carrier       ## choose columns
       , X.arr_delay, X.carrier_delay, carrier_name, -X) %>%  ##
  na.omit %>%                                                 ## only complete cases
  set_colnames(c("year", "delayed", "arrivals", "month"       ## rename columns
               , "carrier", "delay", "delay_car", "carrier_name")) %>%
  mutate(
    date = ymd(paste(year, month, 1, sep="-")),               ## date column
    carrier = as.factor(as.character(carrier)),               ## factorize
    month = as.integer(month) %>%                             ## factorize
      plyr::mapvalues(1:12, mymonths) %>%
      factor(levels = mymonths)
  )





df1 <-
  summarise(
    group_by(df, date, carrier_name)
  , delayed_flight_share = sum(delayed) / sum(arrivals)
  , carrier_share_min = sum(delay_car) / sum(delay)) %>%
  ungroup() %>%
  arrange(date)




## Create Plots
p1 <- ggplot(data = df1,
             aes(x = date, y = delayed_flight_share)) +
  geom_line(aes(color = carrier_name)) +
  scale_x_date(date_breaks = "1 month", date_labels = mymonths) +
  ggtitle ("share of flights delayed")

p2 <- ggplot(data = df1,
       aes(x = date, y = carrier_share_min)) +
  geom_line(aes(color = carrier_name)) +
  scale_x_date(date_breaks = "1 month", date_labels = mymonths) +
  ggtitle ("share of minutes delayed due to the carrier")

grid.arrange(p1, p2, ncol=1)
write.csv(df1, file="data.csv", row.names=FALSE)
```


