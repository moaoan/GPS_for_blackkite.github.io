library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(move)
library(adehabitatHR)  # 家域分析
library(mapview)
library(dplyr)
library(prettymapr)


#-----导入数据-----
kite01 <- read_csv("D:/BK/黑鸢gps数据/20250201/黑鸢01-20250201.csv")
kite05 <- read_csv("D:/BK/黑鸢gps数据/20250201/黑鸢05-20250201.csv")
kite01$设备 <- "BK1"
kite05$设备 <- "BK5"
#合并数据
kites <- bind_rows(kite01, kite05)
colnames(kites)<-c("ID", "IMEID", "timestamp",
                      "east", "longitude", "north", "latitude",
                      "speed", "direction", "altitude", "temperture",
                      "battery", "acctivity", "satellite",
                      "HDOP", "VDOP", "accuracy", "valiade")

#-----数据整理与清洗------
#筛选精度正确的数据
kites <- filter(kites, accuracy %in% c("A","B","C","D"))
#转换时间数据
kites$timestamp <- as.POSIXct(kites$timestamp, 
                                 formats=c("%Y/%m/%d %H:%M:%OS"),
                                 tz="Asia/Taipei")
#提取时间信息
kites <- mutate(kites,
    date = as.Date(timestamp),
    hour = hour(timestamp),
    yday = yday(timestamp),
    month = month(timestamp, label = TRUE), 
    speed_kmh = speed * 3.6) #速度转换为km/h

#排列数据
kites <- arrange(kites, ID, timestamp)



#-----家域分析------
# 转换为空间数据
library(sp)
kites_sf <- st_as_sf(kites, coords = c("longitude", "latitude"), crs = 4326)
kites_sp <- as(kites_sf, "Spatial")
mcp_results <- mcp(kites_sp[,"ID"], percent = 95)


# 可视化家域（整体）
mcp_plot <- ggplot() +
  geom_sf(data = st_as_sf(mcp_results), aes(fill = id), alpha = 0.3) +
  geom_sf(data = kites_sf, aes(color = ID), size = 0.5) +
  labs(title = "Home Range Estimation (95% MCP)") +
  theme_minimal()
mcp_plot


# 可视化筛选家域（筛选01）
#MCP结果转换为sf对象
mcp_sf <- st_as_sf(mcp_results)
#筛选
mcp_01 <- mcp_sf %>% filter(id == "BK1")
points_01 <- kites_sf %>% filter(ID == "BK1")
#制图
mcp_plot_01 <- ggplot() +
  geom_sf(data = mcp_01, aes(fill = id), alpha = 0.3) +
  geom_sf(data = points_01, color = "red", size = 0.5) +
  labs(title = "Home Range of ID 01 (95% MCP)") +
  theme_minimal()
mcp_plot_01


# 增加地图底板
library(mapview)
library(sf)
library(dplyr)

mapview(mcp_01,
        col.regions = "darkblue",
        alpha.regions = 0.4,
        color = "darkblue", #注意多边形配色
        lwd = 2)+  #边界线宽度
  mapview(points_01, color = "darkblue", cex = 2) #点的大小

save.image("D:/BK/R数据科学/黑鸢迁徙图/home_range.Rdata")
