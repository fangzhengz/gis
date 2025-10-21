---
  title:"homeworkone1"
  output:html_document
---
```{r}
install.packages("plotly")
library(sf)
library(here)
st_layers(here("gadm41_CHN.gpkg"))

chinaoutline <- st_read(here("gadm41_CHN.gpkg"), 
                      layer='ADM_ADM_0')
print(chinaoutline)

st_crs(chinaoutline)$proj4string
library (raster)
library(terra)
#case1<-terra::rast(here("raster/wc2.1_2.5m_tmax_ACCESS-CM2_ssp585_2081-2100.tif"))
# have a look at the raster layer jan
#case2<-terra::rast(here("raster/wc2.1_2.5m_tmax_ACCESS-CM2_ssp126_2081-2100.tif"))

#case2
#case1

#plot(case1)
#plot(case2)
#查看栅格文件具体信息
library(fs)
dir_info("raster/") 

library(tidyverse)
listfiles<-dir_info("raster/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles

worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp
#访问一月的层#发现实际有24层分别为两个场景下的12个月。
worldclimtemp[[1]]
#重新命名每一层#想问下是怎么找出这一层的
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#前12层定为case_1
names(worldclimtemp)[1:12] <- paste0("case1_",month)
#后12层定为case_2
names(worldclimtemp)[13:24] <- paste0("case2_",month)
#分别查看一下
worldclimtemp$case1_Jan
worldclimtemp$case2_Jan
#制作栅格堆栈
site <- c("BeiJing", "XiAn", "ShangHai", "ZhengZhou", "ShenYang", "LiuZhou", "Ürümqi", 
          "LanZhou")
lon <- c(115.7, 108.9, 121.2, 113.6, 123.2, 108.5, 87.3, 103.4)
lat <- c(39.4, 34.35, 31.1, 34.7, 41.4, 23.5, 43.4, 36.3)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
chinacitytemp<- terra::extract(worldclimtemp, samples)
#将温度和城市放在一起
chinacitytemp2 <- chinacitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "case1_Jan")
#对数据进行子集化
BeiJingtemp <- chinacitytemp2 %>%
  filter(site=="BeiJing")
#分别取出case1_和case2_的数据
Beijing_case1 <-BeiJingtemp%>%
  select(starts_with("case1_"))%>%
  unlist()%>%
  as.numeric()

Beijing_case2 <-BeiJingtemp%>%
  select(starts_with("case2_"))%>%
  unlist()%>%
  as.numeric()
#做直方图
hist(Beijing_case1,
     main = "Beijing Case1 Temperature Distribution",
     xlab = "Temperature")

hist(Beijing_case2,
     main = "Beijing Case2 Temperature Distribution",
     xlab = "Temperature")
#美化直方图
library(tidyverse)
#define where you want the breaks in the historgram

#userbreak<-c(8,10,12,14,16,18,20,22,24,26)

# remove the ID and site columns
Beijingtemp_1 <- chinacitytemp2 %>%
  filter(site=="BeiJing")

t<-Beijingtemp_1 %>%
  select(starts_with("case1_"))
  #dplyr::select(case1_Jan:case1_Dec)

hist((as.numeric(t)), 
     breaks=10, 
     col="red", 
     main="Histogram of Beijing Temperature", 
     xlab="Temperature", 
     ylab="Frequency")
#查看直方图信息
histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)
histinfo
#加载中国轮廓图
plot(chinaoutline$geom)
#简化边界
chinaoutSIMPLE <- chinaoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()

print(chinaoutline)

#this works nicely for rasters
crs(worldclimtemp)
#继续地图
chinamp <- chinaoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(chinamp)
#裁剪到合适范围
exactchina<-terra::mask(chinamp, chinaoutline)

#subset using the known location of the raster
hist(exactchina[[3]], col="red", main ="case_1March temperature")
#转换直方图格式，顺便把之前打错的名字改了
exactchina_df <- exactchina %>%
  as.data.frame()

#绘制新的直方图
library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactchina_df, 
                 aes(x=case1_Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of china March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(case1_Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
#选出case1_加入
squishdata <- chinacitytemp2 %>%
  pivot_longer(
    cols = starts_with("case"),
    names_to = c("Case", "Month"),
    names_sep = "_",
    values_to = "Temp")

#子集化
twocase <- squishdata %>%
  # | = OR
  filter(., Case== "case1" | Case=="case2")

#获取每种的平均值
meantwocase <- twocase %>%
  group_by(Case) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

#制作图表
ggplot(twocase, aes(x=Temp, color=Case, fill=Case)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwocase, 
             aes(xintercept=mean, 
                 color=Case),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of China case1 and case2 temp",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#制作交互直方图
library(plotly)
# split the data for plotly based on month

case1_data <- squishdata %>%
  drop_na() %>%
  filter(., Case=="case1")

case2_data <- squishdata %>%
  drop_na() %>%
  filter(., Case=="case2")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")
..
# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = case1_data$Temp,
                xbins=xbinsno, name="case1") %>%
  add_histogram(x = case2_data$Temp,
                xbins=xbinsno, name="case2") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)
ihist
<!--begin.rcode

end.rcode-->

```

