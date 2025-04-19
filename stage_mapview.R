library(move)
library(ggmap)
library(raster)
library(lubridate)
library(viridis)
library(ggplot2)
library(moveVis)
library(maptools)
library(plyr)
library(sf)
library(tidyverse)

#-----前期准备-----
#读取数据
gps_data <- read.csv(file = "C:/Users/19618/Desktop/黑鸢05-20250201.csv", fileEncoding = "UTF-8", stringsAsFactors = F)
colnames(gps_data)<-c("ID", "IMEID", "timestamp",
                      "east", "longitude", "north", "latitude",
                      "speed", "direction", "altitude", "temperture",
                      "battery", "acctivity", "satellite",
                      "HDOP", "VDOP", "accuracy", "valiade")
View(gps_data)

#重命名、增加物种名
gps_data$ID <- "BK5"
gps_data$species <- "black kite"

#筛选精度正确的数据
gps_data <- filter(gps_data, accuracy %in% c("A","B","C","D"))

#转换时间数据
gps_data$timestamp <- as.POSIXct(gps_data$timestamp, 
                                 formats=c("%Y/%m/%d %H:%M:%OS"),
                                 tz="Asia/Taipei")
#按时间升序来排列数据
gps_data <- arrange(gps_data, timestamp)

#划定迁徙时间
range(gps_data$timestamp)
gps_data$stage<-"pre-migration"
gps_data$stage[which(gps_data$timestamp>=as.POSIXct("2023-5-13") & gps_data$timestamp<=as.POSIXct("2023-7-8"))]<-"spring_migration_1st"
gps_data$stage[which(gps_data$timestamp>as.POSIXct("2023-7-8") & gps_data$timestamp<as.POSIXct("2023-10-5"))]<-"summer_1st"
gps_data$stage[which(gps_data$timestamp>=as.POSIXct("2023-10-5")& gps_data$timestamp<=as.POSIXct("2023-10-17"))]<-"autumn_migration_1st"
gps_data$stage[which(gps_data$timestamp>as.POSIXct("2023-10-17")& gps_data$timestamp<as.POSIXct("2024-4-8"))]<-"winter_1st"
gps_data$stage[which(gps_data$timestamp>=as.POSIXct("2024-4-5") & gps_data$timestamp<=as.POSIXct("2024-4-21"))]<-"spring_migration_2st"
gps_data$stage[which(gps_data$timestamp>as.POSIXct("2024-4-21") & gps_data$timestamp<as.POSIXct("2024-9-22"))]<-"summer_2st"
gps_data$stage[which(gps_data$timestamp>=as.POSIXct("2024-9-22")& gps_data$timestamp<=as.POSIXct("2024-10-11"))]<-"autumn_migration_2st"
gps_data$stage[which(gps_data$timestamp>as.POSIXct("2024-10-11")& gps_data$timestamp<as.POSIXct("2025-3-19"))]<-"winter_2st"
gps_data$stage[which(gps_data$timestamp>=as.POSIXct("2025-3-19") & gps_data$timestamp<=as.POSIXct("2025-4-5"))]<-"spring_migration_3st"
gps_data$stage[which(gps_data$timestamp>as.POSIXct("2025-4-6"))]<-"summer_3st"

table(gps_data$stage)

gps_data$trackID<-paste0(gps_data$ID,"_",gps_data$stage)

#-----转换数据-----
#将GPS数据框转换为move对象
gps_data.move<-df2move(gps_data,removeDuplicatedTimestamps=T,
                       proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", #移除具有重复时间戳的数据记录
                       x = "longitude", y = "latitude", time = "timestamp", track_id = "trackID")



move::plot(gps_data.move,type="l") #指定绘图类型为线条图（l代表lines ）
timeLag(gps_data.move, unit = "hours") #计算轨迹数据中相邻观测点之间的时间间隔
gps_data.move@idData$ID <- unique(gps_data$ID) #将GPS数据里唯一值重新赋值给move对象中idData槽里的ID变量


gps_data.move$stage<-gps_data$stage #赋值进迁徙阶段
table(gps_data.move$stage) #显示不同迁徙阶段出现的频次


#-----使用mapview制图-----
library(sf)
library(mapview)
library(viridis)

# all bird ：将数据框转换为简单要素数据框
bird.sf<-sf::st_as_sf(gps_data.move, coords = c("location_long","location_lat"),crs = 4326)

# sf point：将经度值转换到 - 180 到 180 的标准范围内
bird.sf<-st_shift_longitude(bird.sf)

# sp line
bird.spl<-as(gps_data.move,'SpatialLinesDataFrame') #转换为SpatialLinesDataFrame 类型
bird.sf.l<-st_as_sf(bird.spl) #将SpatialLinesDataFrame 类型转换为简单要素数据框
bird.sf.l<-st_shift_longitude(bird.sf.l)
bird.sf.l$stage<-rownames(bird.sf.l)

#将stage转换为因子型
bird.sf.l$stage <- as.factor(bird.sf.l$stage)
class(bird.sf.l$stage)
levels(bird.sf.l$stage)
#手动设置因子水平顺序
new_order <- c("BK5_pre.migration", "BK5_spring_migration_1st", "BK5_summer_1st","BK5_autumn_migration_1st",
               "BK5_winter_1st","BK5_spring_migration_2st","BK5_summer_2st","BK5_autumn_migration_2st",
               "BK5_winter_2st" )
bird.sf.l$stage <- factor(bird.sf.l$stage, levels = new_order)
unique_values <- unique(bird.sf.l$stage)
levels(bird.sf.l$stage)


#改色方法1：创建调色板
print(unique_values) #查看stage取值顺序
my_colors <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026")
print(my_colors) #查看是否与stage顺序一一对应
names(my_colors) <- new_order

library(mapview)
mapview(bird.sf.l,
        zcol = "stage",
        color = my_colors,
        #col.regions = my_colors,
        layer.name = paste0("Black kite"),
        burst = FALSE,
        legend = TRUE)


#改色方法2：使用已有的调色板
library(RColorBrewer)
display.brewer.all()
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
mapview(bird.sf.l,
        layer.name = paste0("Black kite"),
        zcol = "stage",
        color = pal,
        #col.regions = pal,
        burst = F,
        legend = T)

save.image("D:/BK/R数据科学/黑鸢迁徙图/stage_mapview.Rdata")
