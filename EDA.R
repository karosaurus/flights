
lapply(list("dplyr", "rvest", "stringr", "xlsx", "readxl","seleniumPipes",
            "regexPipes", "gtools","readxl",'lubridate','ggplot2','alr3','gridExtra'), require, c = 1)
setwd('/Users/Alex/Desktop/datavis')
df <- read.csv('./655176550_12017_3428_airline_delay_causes.csv', header = TRUE)
str(df)


##Shaping
df <- subset(df, select = c('year','arr_del15','arr_flights','X.month','carrier','X.arr_delay',
                            'X.carrier_delay'))
colnames(df)[4] <- 'month'
colnames(df)[6] <- 'arr_delay'
colnames(df)[7] <- 'carrier_delay'

df <- subset(df, df$carrier %in% c('AA','DL', 'WN', 'UA'))
df <- df[complete.cases(df),]
df$date <- as.Date(paste(df$year, df$month, 1, sep='-'), format="%Y-%m-%d")
df$carrier <- as.factor(as.character(df$carrier))

table(df$month)
str(df)
table(df$carrier)
df$month <- as.integer(df$month)
str(df)


groups <- group_by(df, month, carrier)
df1 <- summarise(groups,  delayed_flight_share = sum(arr_del15) / sum(arr_flights),
                 carrier_share_min = sum(carrier_delay) / sum(arr_delay))

df1 <- df1 %>% arrange(carrier)
df1$date <- as.Date(paste(2016, df1$month, 1, sep='-'), format="%Y-%m-%d")

ggplot(data = df1,
       aes(x = month, y = delayed_flight_share)) +
  geom_line(aes(color = carrier)) + scale_x_continuous(limits=c(1,12), 
                                                       breaks=seq(1,12,1))+
  ggtitle ('share of flights delayed')

ggplot(data = df1,
       aes(x = month, y = carrier_share_min)) +
  geom_line(aes(color = carrier)) + scale_x_continuous(limits=c(1,12), 
                                                       breaks=seq(1,12,1))+
  ggtitle ('share of minutes delayed due to the carrier')

##we see that July sucks for delays, but its not carriers' fault

