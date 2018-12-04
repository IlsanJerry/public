#### .###################. ####
#### | 1. 시계열 데이터 | ####
#### .###################. ####

#### __ [01] 시계열 데이터 ####

#### __ [02] 시간 데이터 생성 ####
# as.POSIXct() 함수 사용
as.POSIXct("2016-03-10")
as.POSIXct("2016-03-10 13:12")
as.POSIXct("2016-03-10 13:12:45")
as.POSIXct("2016-03-10", tz="UTC")
as.POSIXct("2016-03-10") + 1
as.POSIXct("2016-03-10") + 100

# 시스템 시간 호출
Sys.Date()
Sys.time()

# seq() 함수 활용
seq(from = as.POSIXct("2016-03-10"), 
    to   = as.POSIXct("2018-05-12"),
    by   = "year")

seq(from = as.POSIXct("2016-03-10"), 
    to   = as.POSIXct("2018-05-12"),
    by   = "month")

seq(from = as.POSIXct("2016-03-10"), 
    to   = as.POSIXct("2018-05-12"),
    by   = "day")

seq(from = as.POSIXct("2016-03-10"), 
    to   = as.POSIXct("2018-05-12"),
    by   = "hour")

seq(from = as.POSIXct("2016-03-10"), 
    to   = as.POSIXct("2018-05-12"),
    by   = "min")

seq(from = as.POSIXct("2016-03-10"), 
    to   = as.POSIXct("2018-05-12"),
    by   = "sec")


# Q. 격일로 출력하려면?
seq(from = as.POSIXct("2018-03-10"), 
    to   = as.POSIXct("2018-03-20"),
    by   = "2 day")

seq(from = as.POSIXct("2018-03-10"), 
    to   = as.POSIXct("2018-03-20"),
    by   = 60 * 60 * 24 * 2)


#### __ [03] 시간 데이터 추출 ####
time_1 = as.POSIXct("2016-03-10 13:12:45")

# 내장함수 활용
months(time_1, abbreviate = TRUE) 
weekdays(time_1, abbreviate = TRUE)
quarters(time_1, abbreviate = TRUE)
as.Date(time_1)

# 패키지(lubridate) 활용
library("lubridate")

year(time_1)
month(time_1)
day(time_1)
hour(time_1)
second(time_1)

year(time_1) = 2015
time_1





#### __ [04] 달력 만들기 ####
library("lubridate")
timeline = seq(as.POSIXct("2018-01-01"), 
               as.POSIXct("2018-12-31"), by = "30 sec")

df = data.frame(obs   = 1:length(timeline),
                time  = timeline,
                year  = year(timeline),
                month = month(timeline),
                day   = day(timeline),
                hour  = hour(timeline),
                minute  = minute(timeline),
                second  = second(timeline),
                quarter = quarter(timeline),
                week    = week(timeline),
                weekday = weekdays(timeline))
head(df)
tail(df)

# quiz
# 방법 1
df[, "value"] = rnorm(n = nrow(df), mean = 3)
head(df)

df_agg = aggregate(data = df, 
                   value ~ year + month + day, 
                   FUN = "mean")
head(df_agg)

# 방법 2
df[, "date"] = date(df$time)
head(df)

df_agg = aggregate(data = df, value ~ date, FUN = "mean")
head(df_agg)

df_agg[, "weekday"] = weekdays(df_agg$date, abbreviate = TRUE)
head(df_agg)

df_agg[, "weight"] = ifelse(df_agg$weekday == "월", 1, 0)
head(df_agg)

as.Date(12345, origin = "1970-01-01")

strptime("!@2018--2&&03", 
         format = "!@%Y--%m&&%d")
?strptime

### .-============================-. ####

#### .###################. ####
#### | 2. R과 금융 데이터 | ####
#### .###################. ####

## 패키지: 관련 데이터 주소
# Quantmod:	http://www.quantmod.com/
# Quandl:	http://www.quandl.com/help/packages/r
# TFX: http://rpubs.com/gsee/TFX
# Rbbg: http://findata.org/rbloomberg/
# IBrokers:	https://www.interactivebrokers.com/en/main.php
# rdatastream: https://github.com/fcocquemas/rdatastream
# pwt: https://pwt.sas.upenn.edu/
# fImport: http://www.rmetrics.org/
# Thinknum: http://thinknum.com/

### .-============================-. ####

#### .###################. ####
#### | 3. 금융 데이터 시각화 | ####
#### .###################. ####
library("ggplot2")

#### __ [01] 목표치 설정 ####
#### ____ ● 막대 그래프 ####
# 데이터 준비
set.seed(1228)
df = data.frame(Company = LETTERS[1:5],
                Performance = sample(30:120, 5),
                Predict = sample(130:160, 5))

# 기본 그래프
gg_bar = ggplot(data = df,
       aes(x = Company)) + 
  geom_col(aes(y = Predict,
               fill  = Company,
               color = Company),
           size = 1,
           alpha = 0.1,
           width = 0.7) +
  geom_col(aes(y = Performance,
               fill = Company),
           width = 0.7) 
gg_bar 

class(gg_bar)
str(gg_bar)

# 01
gg_bar +
  geom_hline(yintercept = 0, color = "#666666", size = 1.2) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 160)) + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.ticks = element_line(size = 1),
        panel.border = element_blank(),
        legend.position = "none")

# 02 - 올바른 y축 기준
gg_bar +
  geom_hline(yintercept = 0, color = "#666666", size = 1.2) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 160),
                     breaks = (0:6) * 25,
                     labels = (0:6) * 25) + 
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 1),
        panel.border = element_blank(),
        legend.position = "none")

# 03
gg_bar +
  geom_hline(yintercept = 0, color = "#666666", size = 1.2) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 160),
                     breaks = (0:6) * 25,
                     labels = (0:6) * 25) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 1, color = "#000000"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 1),
        panel.border = element_blank(),
        legend.position = "none")

# 04
gg_bar +
  geom_hline(yintercept = 0, color = "#666666", size = 1.2) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 160),
                     breaks = (0:6) * 25,
                     labels = (0:6) * 25) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 1.5),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 1),
        panel.border = element_blank(),
        legend.position = "none")

# 05
ggplot(data = df,
       aes(x = Company)) + 
  geom_col(aes(y = Predict),
           fill = "#EEEEEE",
           width = 0.7) +
  geom_col(aes(y = Predict,
               fill  = Company,
               color = Company),
           size = 1,
           alpha = 0.1,
           width = 0.7) +
  geom_col(aes(y = Performance,
               fill = Company),
           width = 0.7) +
  geom_hline(yintercept = 0, color = "#666666", size = 1.2) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 160),
                     breaks = (0:6) * 25,
                     labels = (0:6) * 25) + 
  theme(axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(size = 1.5),
        panel.border = element_blank(),
        legend.position = "none")

#### ____ ● 선 그래프 ####
# 데이터 준비
set.seed(1228)
df = data.frame(date = seq(as.POSIXct("2018-01-01"), as.POSIXct("2018-07-01"), by = "month"),
                value = cumsum(sample(1:4, 7, replace = TRUE)))
df

# 01
ggplot(data = df,
       aes(x = date,
           y = value)) + 
  geom_line()

# 02
ggplot(data = df,
       aes(x = date,
           y = value)) + 
  geom_line(size = 2) + 
  scale_x_datetime(breaks = df$date,
                   labels = paste0(lubridate::month(df$date), "M"))


# 03
ggplot(data = df,
       aes(x = date,
           y = value)) + 
  geom_line(size = 2) + 
  geom_point(size = 5) + 
  geom_point(size = 3, color = "#FFFFFF") + 
  scale_x_datetime(breaks = df$date,
                   labels = paste0(lubridate::month(df$date), "M"))

# 04
ggplot(data = df,
       aes(x = date,
           y = value)) + 
  geom_line(data = df[-nrow(df), ],
            size = 2) + 
  geom_line(data = df[(nrow(df) - 1):nrow(df), ],
            size = 2,
            color = "#888888") + 
  geom_point(size = 5) + 
  geom_point(size = 3, color = "#FFFFFF") + 
  scale_x_datetime(breaks = df$date,
                   labels = paste0(lubridate::month(df$date), "M"))

# 05
ggplot(data = df,
       aes(x = date,
           y = value)) + 
  geom_line(data = df[-nrow(df), ],
            size = 2) + 
  geom_line(data = df[(nrow(df) - 1):nrow(df), ],
            size = 1.2,
            color = "#888888",
            linetype = 3) + 
  geom_point(size = 5) + 
  geom_point(size = 3, color = "#FFFFFF") + 
  scale_x_datetime(breaks = df$date,
                   labels = paste0(lubridate::month(df$date), "M"))

#### __ [02] 지역 비교 ####
# 데이터 준비
library("maptools")
library("maps")
library("ggsn")
library("scatterpie")
map = readShapePoly("2013_si_do.shp", verbose = FALSE)
map_for = fortify(map)
head(map_for)

# map_for = map_for[(map_for$long >= 125.6) & (map_for$long <= 130), ]
df_cl = read.csv("clusters.csv")
head(df_cl)

#### ____ ● geom_polygon 활용 ####
# 01
ggplot(data = df_cl) + 
  geom_polygon(data = map_for, 
               aes(x = long, 
                   y = lat,
                   group = group),
               color = "#000000")

# 02
library("dplyr")
df_index = data.frame(id = as.character(0:16),
                      index = 0:16,
                      stringsAsFactors = FALSE)
map_join = left_join(map_for, df_index)
head(map_join)

ggplot(data = df_cl) + 
  geom_polygon(data = map_join, 
               aes(x = long, 
                   y = lat, 
                   fill = index,
                   group = group),
               color = "#000000")

#### ____ ● 파이차트 활용 ####
# 데이터 준비
df_cl = read.csv("clusters.csv")
head(df_cl)

# 그래프
ggplot(data = df_cl) + 
  geom_polygon(data = map_for, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "#FFFFFF", 
               color = "#000000") + 
  geom_scatterpie(data = df_cl,
                  aes(x = long, 
                      y = lat,
                      group = obs, 
                      r = 0.13),
                  cols = c("cl_1", "cl_2"), 
                  color = NA,
                  alpha = 1) +
  scale_fill_manual(values = c("cl_1" = "#3171FF", "cl_2" = "#E86815")) + 
  facet_wrap(~ year, ncol = 2) +
  scalebar(data = df_cl,
           dist = 100, dd2km = TRUE, model = "WGS84", location = "bottomright",
           st.size = 3.6,
           anchor = c(x = 131.5,
                      y = 33.25),
           facet.var = "df_cl$year",
           facet.lev = "year") +
  labs(x = "Longitude", y = "Latitude") + 
  theme_bw() + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20)) 


#### __ [03] 금융거래 ####
#### ____ ● Heatmap ####
df = read.csv("calendar_heatmap.csv", stringsAsFactors = FALSE)
head(df)

gg_heat = ggplot(data = df, 
       aes(x = monthweek, 
           y = weekdayf, 
           fill = VIX.Close)) + 
  geom_tile(color = "white") + 
  facet_grid(year ~ monthf) + 
  scale_fill_gradient(low = "#FF0000", high = "#00FF00") +
  labs(x = "Week of Month", y = NULL,
       title = "Time-Series Calendar Heatmap", 
       subtitle = "Yahoo Closing Price", 
       fill = "Close")
gg_heat

library("plotly")
ggplotly(gg_heat)

#### ____ ● Bar-plot ####
# install.packages("ggpubr")
library("ggpubr")
df = read.csv("fund_bar_lolipop.csv", stringsAsFactors = FALSE)
head(df)

# 01
ggbarplot(df, 
          x = "Fund",
          y = "Interest",
          fill = "group",         
          color = "#FFFFFF",
          palette = "jco",          
          sort.val = "asc",         
          sort.by.groups = FALSE,   
          x.text.angle = 90) + 
  geom_hline(yintercept = 0)
 
# 02
ggbarplot(df, 
          x = "Fund",
          y = "Interest",
          fill = "group",         
          color = "#FFFFFF",
          palette = "jco",
          rotate = TRUE,
          sort.val = "asc",         
          sort.by.groups = FALSE) + 
  geom_hline(yintercept = 0)


#### ____ ● Bubble chart ####
library("ggpubr")
df = read.csv("fund_bar_lolipop.csv", stringsAsFactors = FALSE)
head(df)

ggdotchart(df, 
           x = "Fund", 
           y = "Invest",
           color = "Type",  
           sorting = "asc", 
           add = "segments")

ggdotchart(df, 
           x = "Fund", 
           y = "Invest",
           color = "Type",
           rotate = TRUE,
           dot.size = 7,
           label = round(df$Invest),
           font.label = list(size = 10, 
                             vjust = 0.5), 
           sorting = "asc", 
           add = "segments")


### .-============================-. ####

#### .###################. ####
#### | 4. R과 JS의 결합 | ####
#### .###################. ####

# https://www.highcharts.com/
# d3js.org

#### __ [01] highchart #####
# install.packages("highcharter")
library("highcharter")

# 데이터 준비
data(diamonds, mpg, package = "ggplot2")
head(mpg)
# 산점도
hchart(object  = mpg, 
       type    = "scatter", 
       mapping = hcaes(x = displ, 
                       y = hwy, 
                       group = class))


# 막대 그래프
hchart(object = diamonds$cut, 
       colorByPoint = TRUE)

table(diamonds$cut)

# 선 그래프 + 시계열
# install.packages("forecast")
library("forecast")
airforecast = forecast(auto.arima(AirPassengers), level = 95)
hchart(airforecast)

airforecast
head(airforecast)
str(airforecast)

# 봉차트
# install.packages("quantmod")
library("quantmod")
highchart(type = "stock") %>% 
  hc_add_series(getSymbols("GOOG", auto.assign = FALSE)) %>% 
  hc_add_series(getSymbols("AMZN", auto.assign = FALSE),
                type = "ohlc")

#### __ [02] D3.js ####
library("networkD3")
# library("d3Network")

src = c(rep("A",4), rep("B", 2), rep("C", 2), "D")
target = c("B", "C", "D", "J", "E", "F", "G", "H", "I")
networkData = data.frame(src, target)

simpleNetwork(networkData)

#### __ [03] radarchart ####
# install.packages("radarchart")
library("radarchart")

labs = c("리스크", "매출", "자본", "규모", "시장", "부채")
scores = list("values" = c(6, 3, 8, 6, 9, 1))

chartJSRadar(scores = scores,
             labs = labs,
             width  = 20, 
             height = 20,
             addDots = FALSE, 
             maxScale = 10,
             lineAlpha = 0,
             showLegend = FALSE)


### .-============================-. ####

#### .###################. ####
#### | 5. Shiny를 활용한 시각화 | ####
#### .###################. ####

### .-============================-. ####