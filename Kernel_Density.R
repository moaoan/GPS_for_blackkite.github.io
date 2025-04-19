library(geosphere)
library(amt)
library(recurse)
library(ranger)
library(modEvA)
library(adehabitatLT)
library(devtools)
library(usethis)
library(tidyverse)
library(sf)
library(spatstat)
library(terra)
library(raster)


######前期准备######
#读取数据
gps_data <- read.csv(file = "C:/Users/19618/Desktop/黑鸢05-20250201.csv", fileEncoding = "UTF-8", stringsAsFactors = F)
colnames(gps_data)<-c("ID", "IMEID", "timestamp",
                      "east", "longitude", "north", "latitude",
                      "speed", "direction", "altitude", "temperture",
                      "battery", "acctivity", "satellite",
                      "HDOP", "VDOP", "accuracy", "valiade")
View(gps_data)

#重命名、增加物种名
gps_data$ID <- "BK"
gps_data$species <- "black kite"

#筛选精度正确的数据
gps_data <- filter(gps_data, accuracy %in% c("A","B","C","D"))

#转换时间数据
gps_data$timestamp <- as.POSIXct(gps_data$timestamp, 
                                 formats=c("%Y/%m/%d %H:%M:%OS"),
                                 tz="Asia/Taipei")
#按时间升序来排列数据
gps_data <- arrange(gps_data, timestamp)



######计算运动指标######
#计算步长
n <- nrow(gps_data)
gps_data$step_length <- numeric(n)
for (i in 2:n) {
  gps_data$step_length[i] <- distHaversine(c(gps_data$longitude[i - 1], gps_data$latitude[i - 1]), c(gps_data$longitude[i], gps_data$latitude[i]))
}

#计算每时段的平均速度
time_diff <- as.numeric(diff(gps_data$timestamp), units = "secs")
time_diff <- c(0, time_diff)
gps_data$average_speed <- gps_data$step_length / time_diff
gps_data$average_speed[is.na(gps_data$average_speed)] <- 0 #把第一个数据设为0

#计算转向角
n <- nrow(gps_data)
##方位角
bearings <- numeric(n)
for (i in 1:(n - 1)) {
  bearings[i] <- bearing(c(gps_data$longitude[i], gps_data$latitude[i]), c(gps_data$longitude[i + 1], gps_data$latitude[i + 1]))
}
##转向角
turning_angles <- numeric(n)
for (i in 2:(n - 1)) {
  turning_angles[i] <- bearings[i] - bearings[i - 1] #转向角在-180到180度之间
  turning_angles[i] <- (turning_angles[i] + 180) %% 360 - 180
}
gps_data$turning_angle <- turning_angles


######计算访问次数和停留时间######
#自定义函数
calculate_recursions_single_id <- function(data, radius = 50, timeunits = "hours") {
  n <- nrow(data)
  #创建两个空向量，用于存储每个点的重访次数和停留时间
  revisits <- numeric(n)
  residence_time <- numeric(n)
  
  for (j in 1:n) {
    # 获取当前点的经度和纬度
    current_longitude <- data$longitude[j]
    current_latitude <- data$latitude[j]
    # 计算当前点与所有点（包括自身）之间的球面距离
    distances = distHaversine(c(current_longitude, current_latitude), cbind(data$longitude, data$latitude))
    # 统计距离当前点小于等于指定半径的点的数量,即重访次数
    revisits[j] <- sum(distances <= radius)
    # 筛选出距离当前点小于等于半径的点，即附近点
    nearby_points <- data[distances <= radius, ]
    
    # 计算停留时间（有附近点的情况下）
    has_nearby_points <- nrow(nearby_points) > 0
    if (has_nearby_points) {
      max_time = max(nearby_points$timestamp)
      min_time = min(nearby_points$timestamp)
      time_diff <- difftime(max_time, min_time, units = timeunits)
      residence_time[j] <- as.numeric(time_diff)
    }
  }
  
  # 将重访次数和停留时间添加到数据框
  data$revisits <- revisits
  data$residence_time<- residence_time
  
  return(data)
}

# 调用函数计算重访次数和停留时间
gps_data <- calculate_recursions_single_id(gps_data, radius = 50, timeunits = "hours")

# 查看结果
print(gps_data)


# 核密度计算
wgs84 <- st_crs(4326)
coor_data <- gps_data[,c(5,7)]
#utm_zone <- floor((coor_data$longitude + 180)/6) + 1
clean_sf <- st_as_sf(coor_data, coords = c("longitude", "latitude"), crs = wgs84)
clean_utm <- st_transform(clean_sf, crs = 32651)


coords <- st_coordinates(clean_utm)
window <- owin(range(coords[,1]), range(coords[,2]))
ppp_obj <- ppp(coords[,1], coords[,2], window = window)
kde <- raster::density(ppp_obj, sigma = 10000,dimyx = 1000) # sigma调整平滑度，dimyx调节精度
kde_raster <- raster(kde)
crs(kde_raster) <- 32651
#plot(kde_raster, main = "Black Kite KDE (Kernel Density Estimation)")

nj <- st_read("~/Documents/juju/2024报告/南京市/南京市.shp")
nj <- st_transform(nj, crs = 32651)

kde_crop <- crop(rast(kde_raster), vect(nj))
kde_masked <- mask(kde_crop, vect(nj))


# KDE 转为 data.frame
kde_df <- as.data.frame(kde_masked, xy = TRUE)
colnames(kde_df) <- c("x", "y", "density")

# ggplot 绘图
ggplot() +
  geom_raster(data = kde_df, aes(x = x, y = y, fill = density)) +
  scale_fill_viridis_c(option = "viridis", name = "Density") +
  geom_sf(data = nj, fill = NA, color = "red", size = 0.8) +  # 南京边界
  coord_sf() +
  theme_minimal() +
  labs(title = "Black Kite Kernel Density in Nanjing",
       x = "Easting (m)", y = "Northing (m)")

save.image("~/Documents/juju/student/gps.Rdata")



