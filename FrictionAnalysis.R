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


# Vertical, Friction3.5 to Friction 4.5----
setwd("E:/R/DongAo/Slope/VerticalFriction/")
get.filename <- list.files(path = "E:/R/DongAo/Slope/VerticalFriction/",
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
  
  get.tmpdata$frictionValue <- strsplit(x = get.dfname[i], split = "_")[[1]][2]
  
  assign(get.dfname[i], get.tmpdata)
}


# 合并数据----
df.verticalMu <- rbind(coasterSpeed20_0.35,
                       coasterSpeed20_0.36,
                       coasterSpeed20_0.37,
                       coasterSpeed20_0.38,
                       coasterSpeed20_0.39,
                       coasterSpeed20_0.40,
                       coasterSpeed20_0.41,
                       coasterSpeed20_0.42,
                       coasterSpeed20_0.43,
                       coasterSpeed20_0.44,
                       coasterSpeed20_0.45)


# Plot, Vertical, Mu----
plot.coasterMu <- ggplot(df.verticalMu,
                         aes(x = Station, y = Speed)) +
  geom_path(aes(colour = factor(frictionValue)), size = 1.2) +
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


# Plot, Vertical, Mu0.35----
plot.coaster35 <- ggplot(coasterSpeed20_0.35,
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
  labs(title = "Friction Mu 0.35", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster35


# Plot, Vertical, Mu0.36----
plot.coaster36 <- ggplot(coasterSpeed20_0.36,
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
  labs(title = "Friction Mu 0.36", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster36


# Plot, Vertical, Mu0.37----
plot.coaster37 <- ggplot(coasterSpeed20_0.37,
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
  labs(title = "Friction Mu 0.37", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster37


# Plot, Vertical, Mu0.38----
plot.coaster38 <- ggplot(coasterSpeed20_0.38,
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
  labstitle = "Friction Mu 0.38", (x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster38


# Plot, Vertical, Mu0.39----
plot.coaster39 <- ggplot(coasterSpeed20_0.39,
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
  labs(title = "Friction Mu 0.39", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster39


# Plot, Vertical, Mu0.40----
plot.coaster40 <- ggplot(coasterSpeed20_0.40,
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
  labs(title = "Friction Mu 0.40", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster40


# Plot, Vertical, Mu0.41----
plot.coaster41 <- ggplot(coasterSpeed20_0.41,
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
  labs(title = "Friction Mu 0.41", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster41


# Plot, Vertical, Mu0.42----
plot.coaster42 <- ggplot(coasterSpeed20_0.42,
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
  labs(title = "Friction Mu 0.42", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster42


# Plot, Vertical, Mu0.43----
plot.coaster43 <- ggplot(coasterSpeed20_0.43,
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
  labs(title = "Friction Mu 0.43", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster43


# Plot, Vertical, Mu0.44----
plot.coaster44 <- ggplot(coasterSpeed20_0.44,
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
  labs(title = "Friction Mu 0.44", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster44


# Plot, Vertical, Mu0.45----
plot.coaster45 <- ggplot(coasterSpeed20_0.45,
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
  labs(title = "Friction Mu 0.45", x = "位置", y = "速度，km/h") +
  scale_colour_hue("Friction Mu")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.coaster45



