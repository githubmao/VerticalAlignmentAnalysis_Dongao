#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, date: 20180523, by MaoY
#
# Description:
# 纵坡12%，不同道路摩擦系数，爬坡分析。
#------------------------------------------------------------------------------#

# 加载相关package----
library(data.table)
library(ggplot2)

# 数据导入----
setwd("E:/R/DongAo/Slope/FrictionData/")
get.filename <- list.files(path = "E:/R/DongAo/Slope/FrictionData/",
                           pattern = ".txt")
get.dfname <- gsub(pattern = ".txt", replacement = "", x = get.filename)

for(i in 1:length(get.filename))
{
  get.tmpdata <- fread(input = get.filename[i],
                       header = TRUE,
                       sep = "auto",
                       stringsAsFactors = FALSE,
                       data.table = FALSE,
                       skip = "Station",
                       col.names = c("Station", "Speed"))
  
  get.tmpdata$frictionValue <- strsplit(x = get.dfname[i], split = "_")[[1]][3]
  
  assign(get.dfname[i], get.tmpdata)
}


# 合并数据----
df.friction <- rbind(Grade12_Speed20_0.34,
                     Grade12_Speed20_0.35,
                     Grade12_Speed20_0.45,
                     Grade12_Speed20_0.55,
                     Grade12_Speed20_0.65,
                     Grade12_Speed20_0.75,
                     Grade12_Speed20_0.85)

# 作图分析----
plot.friction <- ggplot(df.friction,
                        aes(x = Station, y = Speed)) +
  geom_path(aes(colour = factor(frictionValue)),size = 1.2) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1.0) +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(0, 500, 100)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 10)) +
  labs(x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.friction


