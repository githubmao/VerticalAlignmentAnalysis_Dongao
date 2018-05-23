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


# Vertical, Mu3.5 & Mu4.5----
df.verticalMu3.5 <- fread(input = "E:/R/DongAo/Slope/VerticalFriction/coasterSpeed20_3.5.txt",
                          header = TRUE,
                          sep = "auto",
                          stringsAsFactors = FALSE,
                          data.table = FALSE,
                          skip = "Station",
                          col.names = c("Station", "Speed"))


df.verticalMu4.5 <- fread(input = "E:/R/DongAo/Slope/VerticalFriction/coasterSpeed20_4.5.txt",
                          header = TRUE,
                          sep = "auto",
                          stringsAsFactors = FALSE,
                          data.table = FALSE,
                          skip = "Station",
                          col.names = c("Station", "Speed"))

# 增加数据项 Mu----
df.verticalMu3.5$Mu <- "3.5"
df.verticalMu4.5$Mu <- "4.5"


# 合并数据----
df.verticalMu <- rbind(df.verticalMu3.5,
                       df.verticalMu4.5)


# Plot, Vertical, Mu 3.5----
plot.coasterMu3.5 <- ggplot(df.verticalMu3.5,
                            aes(x = Station, y = Speed)) +
  geom_path(size = 1.2) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1.0) +
  scale_x_continuous(limits = c(100, 7523),
                     breaks = c(seq(100, 7100, 500), 7523),
                     labels = c("K0+000", "K0+500", "K1+000", "K1+500",
                                "K2+000", "K2+500", "K3+000", "K3+500",
                                "K4+000", "K4+500", "K5+000", "K5+500",
                                "K6+000", "K6+500", "K7+000", "K7+423")) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(x = "位置", y = "速度，km/h") +
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coasterMu3.5


# Plot, Vertical, Mu 4.5----
plot.coasterMu4.5 <- ggplot(df.verticalMu4.5,
                            aes(x = Station, y = Speed)) +
  geom_path(size = 1.2) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1.0) +
  scale_x_continuous(limits = c(100, 7523),
                     breaks = c(seq(100, 7100, 500), 7523),
                     labels = c("K0+000", "K0+500", "K1+000", "K1+500",
                                "K2+000", "K2+500", "K3+000", "K3+500",
                                "K4+000", "K4+500", "K5+000", "K5+500",
                                "K6+000", "K6+500", "K7+000", "K7+423")) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(x = "位置", y = "速度，km/h") +
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coasterMu4.5


# Plot, Vertical, Mu----
plot.coasterMu <- ggplot(df.verticalMu,
                         aes(x = Station, y = Speed)) +
  geom_path(aes(colour = factor(Mu)), size = 1.2) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1.0) +
  scale_x_continuous(limits = c(100, 7523),
                     breaks = c(seq(100, 7100, 500), 7523),
                     labels = c("K0+000", "K0+500", "K1+000", "K1+500",
                                "K2+000", "K2+500", "K3+000", "K3+500",
                                "K4+000", "K4+500", "K5+000", "K5+500",
                                "K6+000", "K6+500", "K7+000", "K7+423")) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coasterMu


