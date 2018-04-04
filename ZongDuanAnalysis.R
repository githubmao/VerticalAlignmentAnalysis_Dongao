#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, date: 20180327, by MaoY
#
# Description:
# 冬奥会2号路，柯斯达（20座） & 50座客车，纵断面行车速度分析。
#------------------------------------------------------------------------------#


# 加载相关package---------------------------------------------------------------
library(data.table)
library(ggplot2)


# 导入Carsim和Trucksim数据------------------------------------------------------
df.coaster40 <- fread(input = "E:/R/DongAo/Slope/Data/coasterSpeed.txt",
                      header = TRUE,
                      sep = "auto",
                      stringsAsFactors = FALSE,
                      data.table = FALSE,
                      skip = "Station",
                      col.names = c("Station", "Speed"))  # 柯斯达数据40km/h

df.bus40 <- fread(input = "E:/R/DongAo/Slope/Data/busSpeed.txt",
                  header = TRUE,
                  sep = "auto",
                  stringsAsFactors = FALSE,
                  data.table = FALSE,
                  skip = "Station",
                  col.names = c("Station", "Speed"))  # 50座bus数据40km/h

df.coaster20 <- fread(input = "E:/R/DongAo/Slope/Data/coasterSpeed20.txt",
                      header = TRUE,
                      sep = "auto",
                      stringsAsFactors = FALSE,
                      data.table = FALSE,
                      skip = "Station",
                      col.names = c("Station", "Speed"))  # 柯斯达数据20km/h

df.bus20 <- fread(input = "E:/R/DongAo/Slope/Data/busSpeed20.txt",
                  header = TRUE,
                  sep = "auto",
                  stringsAsFactors = FALSE,
                  data.table = FALSE,
                  skip = "Station",
                  col.names = c("Station", "Speed"))  # 50座bus数据20km/h

df.coasterGrade12 <- fread(input = "E:/R/DongAo/Slope/Data/coasterSpeedGrade12.txt",
                           header = TRUE,
                           sep = "auto",
                           stringsAsFactors = FALSE,
                           data.table = FALSE,
                           skip = "Station",
                           col.names = c("Station", "Speed"))  # 柯斯达数据20km/h,Grade12

df.busGrade12 <- fread(input = "E:/R/DongAo/Slope/Data/busSpeedGrade12.txt",
                       header = TRUE,
                       sep = "auto",
                       stringsAsFactors = FALSE,
                       data.table = FALSE,
                       skip = "Station",
                       col.names = c("Station", "Speed"))  # 50座bus数据20km/h,Grade12


# 作图，速度vs桩号--------------------------------------------------------------
# 柯斯达行驶速度40km/h
plot.coasterspeed40 <- ggplot(df.coaster40,
                              aes(x = Station, y = Speed))+
  geom_line(size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(100, 7523),
                     breaks = c(seq(100, 7100, 500), 7523),
                     labels = c("K0+000", "K0+500", "K1+000", "K1+500",
                                "K2+000", "K2+500", "K3+000", "K3+500",
                                "K4+000", "K4+500", "K5+000", "K5+500",
                                "K6+000", "K6+500", "K7+000", "K7+423"))+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10))+
  labs(x = "位置", y = "速度，km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coasterspeed40


# 柯斯达行驶速度40km/h
plot.busspeed40 <- ggplot(df.bus40,
                          aes(x = Station, y = Speed))+
  geom_line(size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(100, 7523),
                     breaks = c(seq(100, 7100, 500), 7523),
                     labels = c("K0+000", "K0+500", "K1+000", "K1+500",
                                "K2+000", "K2+500", "K3+000", "K3+500",
                                "K4+000", "K4+500", "K5+000", "K5+500",
                                "K6+000", "K6+500", "K7+000", "K7+423"))+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10))+
  labs(x = "位置", y = "速度，km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.busspeed40


# 柯斯达行驶速度20km/h
plot.coasterspeed20 <- ggplot(df.coaster20,
                              aes(x = Station, y = Speed))+
  geom_line(size = 1.2)+
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(100, 7523),
                     breaks = c(seq(100, 7100, 500), 7523),
                     labels = c("K0+000", "K0+500", "K1+000", "K1+500",
                                "K2+000", "K2+500", "K3+000", "K3+500",
                                "K4+000", "K4+500", "K5+000", "K5+500",
                                "K6+000", "K6+500", "K7+000", "K7+423"))+
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))+
  labs(x = "位置", y = "速度，km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coasterspeed20


# 柯斯达行驶速度20km/h
plot.busspeed20 <- ggplot(df.bus20,
                          aes(x = Station, y = Speed))+
  geom_line(size = 1.2)+
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(100, 7523),
                     breaks = c(seq(100, 7100, 500), 7523),
                     labels = c("K0+000", "K0+500", "K1+000", "K1+500",
                                "K2+000", "K2+500", "K3+000", "K3+500",
                                "K4+000", "K4+500", "K5+000", "K5+500",
                                "K6+000", "K6+500", "K7+000", "K7+423"))+
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5))+
  labs(x = "位置", y = "速度，km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.busspeed20


# 柯斯达行驶速度20km/h,Grade12


# 柯斯达行驶速度20km/h,Grade12

