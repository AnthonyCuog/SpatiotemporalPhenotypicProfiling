library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(plot3D)
library(plot3Drgl)

##LUMINAO 1
(files <- fs::dir_ls("ListModeInSitu_March23/List_1-21_2021-07-16_at_02-46-24pm/", 
                     glob="*.CSV"))
Lum1 <- read_csv(files, id="path")
head(Lum1)

names(Lum1) <- gsub("-", ".", names(Lum1), fixed=TRUE)

Lum1$RED.B.HLog <- as.numeric(Lum1$RED.B.HLog)

#ggdensity(Lum1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Lum1$RED.B.HLog)$y
DensityX <- density(Lum1$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.004362113
which(DensityY == MinYDensity)
#354
DensityX[354]
#3.441757

#Visualize your threshold here
#ggdensity(Lum1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Lum1$RED.B.HLog)$x[354])

#fluorescent signatures for each separately counted cell 

Lum1sym <- subset(Lum1, RED.B.HLog>=3.441757)
head(Lum1sym)
nrow(Lum1sym)
#ggdensity(Lum1sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
#  scale_x_continuous(limits = c(1.5, 5))

#ggplot(Lum1sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

##WEST AGANA 1
scatter3D(Lum1sym$FSC.HLog, Lum1sym$SSC.HLog, 
                Lum1sym$RED.B.HLog, colvar = Lum1sym$RED.R.HLog, 
          pch = 16, alpha = 0.3)

(files <- fs::dir_ls("ListModeInSitu_March23/List_25-45_2021-07-17_at_03-25-47pm/", 
                     glob="*.CSV"))
WA1 <- read_csv(files, id="path")
head(WA1)

names(WA1) <- gsub("-", ".", names(WA1), fixed=TRUE)

WA1$RED.B.HLog <- as.numeric(WA1$RED.B.HLog)

#ggdensity(WA1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(WA1$RED.B.HLog)$y
DensityX <- density(WA1$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.01163516
which(DensityY == MinYDensity)
#359
DensityX[359]
#3.535182

#Visualize your threshold here
#ggdensity(WA1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(WA1$RED.B.HLog)$x[359])

#fluorescent signatures for each separately counted cell 

WA1sym <- subset(WA1, RED.B.HLog>=3.535182)
head(WA1sym)
nrow(WA1sym)
#ggdensity(WA1sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
#  scale_x_continuous(limits = c(1.5, 5))

#ggplot(WA1sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

#scatter3D(WA1sym$RED.B.HLog, WA1sym$FSC.HLog,  WA1sym$SSC.HLog, colvar = WA1sym$RED.R.HLog, 
#          pch = 16, alpha = 0.2)

###TOGCHA 1
(files <- fs::dir_ls("ListModeInSitu_March23/List_Tog_T1_Redone/", 
                     glob="*.CSV"))
Tog1 <- read_csv(files, id="path")
head(Tog1)

names(Tog1) <- gsub("-", ".", names(Tog1), fixed=TRUE)

Tog1$RED.B.HLog <- as.numeric(Tog1$RED.B.HLog)

#ggdensity(Tog1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Tog1$RED.B.HLog)$y
DensityX <- density(Tog1$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.002776713
which(DensityY == MinYDensity)
#297
DensityX[297]
#2.907253

#Visualize your threshold here
#ggdensity(Tog1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Tog1$RED.B.HLog)$x[297])

#fluorescent signatures for each separately counted cell 
Tog1sym <- subset(Tog1, RED.B.HLog>=2.907253)
head(Tog1sym)
nrow(Tog1sym)
#ggdensity(Tog1sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
#  scale_x_continuous(limits = c(1.5, 5))

#ggplot(Tog1sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

Tog1sym <- subset(Tog1sym, SSC.HLog > 0.5)

#ggplot(Tog1sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

#scatter3D(Tog1sym$RED.B.HLog, Tog1sym$FSC.HLog,  Tog1sym$SSC.HLog, 
#          colvar = Tog1sym$RED.R.HLog, 
#          pch = 16, alpha = 0.2)


#plotrgl()

###Urunao 1
(files <- fs::dir_ls("ListModeInSitu_March23/List_73-93/", 
                     glob="*.CSV"))
Uru1 <- read_csv(files, id="path")
head(Uru1)

names(Uru1) <- gsub("-", ".", names(Uru1), fixed=TRUE)

Uru1$RED.B.HLog <- as.numeric(Uru1$RED.B.HLog)

#ggdensity(Uru1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Uru1$RED.B.HLog)$y
DensityX <- density(Uru1$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.0114136
which(DensityY == MinYDensity)
#353
DensityX[353]
#3.485939

#Visualize your threshold here
#ggdensity(Uru1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
 # scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Uru1$RED.B.HLog)$x[353])

#fluorescent signatures for each separately counted cell 
Uru1sym <- subset(Uru1, RED.B.HLog>=3.485939)
head(Uru1sym)
nrow(Uru1sym)
#ggdensity(Uru1sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
 # scale_x_continuous(limits = c(1.5, 5))

#ggplot(Uru1sym, aes(RED.B.HLog, SSC.HLog))+
 # geom_point()

Uru1sym <- subset(Uru1sym, SSC.HLog > 0.5)

#ggplot(Uru1sym, aes(RED.B.HLog, SSC.HLog))+
 # geom_point()

#scatter3D(Uru1sym$RED.B.HLog, Uru1sym$FSC.HLog,  Uru1sym$SSC.HLog, 
 #         col = "#648FFF", 
#          pch = 16, alpha = 0.2)
#plotrgl()

###COCOS 1
(files <- fs::dir_ls("ListModeInSitu_March23/List_97-117_2021-07-17_at_03-25-47pm/", 
                     glob="*.CSV"))
Coco1 <- read_csv(files, id="path")
head(Coco1)

names(Coco1) <- gsub("-", ".", names(Coco1), fixed=TRUE)

Coco1$RED.B.HLog <- as.numeric(Coco1$RED.B.HLog)

#ggdensity(Coco1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Coco1$RED.B.HLog)$y
DensityX <- density(Coco1$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.004816085
which(DensityY == MinYDensity)
#379
DensityX[379]
#3.558183

#Visualize your threshold here
#ggdensity(Coco1, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
 # scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Coco1$RED.B.HLog)$x[353])

#fluorescent signatures for each separately counted cell 
Coco1sym <- subset(Coco1, RED.B.HLog>=3.558183)
head(Coco1sym)
nrow(Coco1sym)
#ggdensity(Coco1sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
 # scale_x_continuous(limits = c(1.5, 5))

#ggplot(Coco1sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

Coco1sym <- subset(Coco1sym, SSC.HLog > 0.5)

#ggplot(Coco1sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

#scatter3D(Coco1sym$RED.B.HLog, Coco1sym$FSC.HLog,  Coco1sym$SSC.HLog, 
#          col = "#FFB000", 
#          pch = 16, alpha = 0.2)
#plotrgl()

##LUMINAO 2
(files <- fs::dir_ls("ListModeInSitu_March23/List_Lum_T2_2021-10-17_at_01-18-48pm/", 
                     glob="*.CSV"))
Lum2 <- read_csv(files, id="path")
head(Lum2)

names(Lum2) <- gsub("-", ".", names(Lum2), fixed=TRUE)

Lum2$RED.B.HLog <- as.numeric(Lum2$RED.B.HLog)

#ggdensity(Lum2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Lum2$RED.B.HLog)$y
DensityX <- density(Lum2$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.01073804
which(DensityY == MinYDensity)
#294
DensityX[294]
#2.848861

#Visualize your threshold here
#ggdensity(Lum2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Lum2$RED.B.HLog)$x[354])

#fluorescent signatures for each separately counted cell 

Lum2sym <- subset(Lum2, RED.B.HLog>=2.848861)
head(Lum2sym)
nrow(Lum2sym)
#ggdensity(Lum2sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
 # scale_x_continuous(limits = c(1.5, 5))

#ggplot(Lum2sym, aes(RED.B.HLog, SSC.HLog))+
 # geom_point()

Lum2sym <- subset(Lum2sym, SSC.HLog>0.5)

#scatter3D(Lum2sym$FSC.HLog, Lum2sym$SSC.HLog, 
#          Lum2sym$RED.B.HLog, colvar = Lum2sym$RED.R.HLog, 
#          pch = 16, alpha = 0.3)

(files <- fs::dir_ls("ListModeInSitu_March23/List_WA_T2_2021-12-07_at_03-48-14pm/", 
                     glob="*.CSV"))
WA2 <- read_csv(files, id="path")
head(WA2)

names(WA2) <- gsub("-", ".", names(WA2), fixed=TRUE)

WA2$RED.B.HLog <- as.numeric(WA2$RED.B.HLog)

#ggdensity(WA2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(WA2$RED.B.HLog)$y
DensityX <- density(WA2$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.006553191
which(DensityY == MinYDensity)
#328
DensityX[328]
#3.222205

#Visualize your threshold here
#ggdensity(WA2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(WA2$RED.B.HLog)$x[328])

#fluorescent signatures for each separately counted cell 

WA2sym <- subset(WA2, RED.B.HLog>=3.222205)
head(WA2sym)
nrow(WA2sym)
#ggdensity(WA2sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
#  scale_x_continuous(limits = c(1.5, 5))

#ggplot(WA2sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

#scatter3D(WA2sym$RED.B.HLog, WA2sym$FSC.HLog,  WA2sym$SSC.HLog, 
#          col = "#785EF0", 
#          pch = 16, alpha = 0.2)

###TOGCHA 1
(files <- fs::dir_ls("ListModeInSitu_March23/List_Tog_T2_2021-10-07_at_02-58-29pm/", 
                     glob="*.CSV"))
Tog2 <- read_csv(files, id="path")
head(Tog2)

names(Tog2) <- gsub("-", ".", names(Tog2), fixed=TRUE)

Tog2$RED.B.HLog <- as.numeric(Tog2$RED.B.HLog)

#ggdensity(Tog2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Tog2$RED.B.HLog)$y
DensityX <- density(Tog2$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.009392434
which(DensityY == MinYDensity)
#294
DensityX[297]
#2.82773

#Visualize your threshold here
#ggdensity(Tog2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Tog2$RED.B.HLog)$x[297])

#fluorescent signatures for each separately counted cell 
Tog2sym <- subset(Tog2, RED.B.HLog>=2.82773)
head(Tog2sym)
nrow(Tog2sym)
#ggdensity(Tog2sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
#  scale_x_continuous(limits = c(1.5, 5))

#ggplot(Tog2sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

Tog2sym <- subset(Tog2sym, SSC.HLog > 0.5)

#ggplot(Tog2sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

#scatter3D(Tog2sym$RED.B.HLog, Tog2sym$FSC.HLog,  Tog2sym$SSC.HLog, 
#          col = "#FE6100", 
#          pch = 16, alpha = 0.2)

#plotrgl()

###Urunao 2
(files <- fs::dir_ls("ListModeInSitu_March23/List_Uru_T2_2021-10-17_at_01-18-48pm/", 
                     glob="*.CSV"))
Uru2 <- read_csv(files, id="path")
head(Uru2)

names(Uru2) <- gsub("-", ".", names(Uru2), fixed=TRUE)

Uru2$RED.B.HLog <- as.numeric(Uru2$RED.B.HLog)

#ggdensity(Uru2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Uru2$RED.B.HLog)$y
DensityX <- density(Uru2$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.006889302
which(DensityY == MinYDensity)
#301
DensityX[301]
#2.878173

#Visualize your threshold here
#ggdensity(Uru2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Uru2$RED.B.HLog)$x[301])

#fluorescent signatures for each separately counted cell 
Uru2sym <- subset(Uru2, RED.B.HLog>=2.878173)
head(Uru2sym)
nrow(Uru2sym)
#ggdensity(Uru2sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
 # scale_x_continuous(limits = c(1.5, 5))

#ggplot(Uru2sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

Uru2sym <- subset(Uru2sym, SSC.HLog > 0.5)

#ggplot(Uru2sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

#scatter3D(Uru2sym$RED.B.HLog, Uru2sym$FSC.HLog,  Uru2sym$SSC.HLog, 
#          col = "#648FFF", 
#          pch = 16, alpha = 0.2)
#plotrgl()

###COCOS 2
(files <- fs::dir_ls("ListModeInSitu_March23/List_Coco_T2_2021-12-08_at_02-28-02pm/", 
                     glob="*.CSV"))
Coco2 <- read_csv(files, id="path")
head(Coco2)

names(Coco2) <- gsub("-", ".", names(Coco2), fixed=TRUE)

Coco2$RED.B.HLog <- as.numeric(Coco2$RED.B.HLog)

#ggdensity(Coco2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))

DensityY <- density(Coco2$RED.B.HLog)$y
DensityX <- density(Coco2$RED.B.HLog)$x

DensityX < 4 & DensityX > 2
MinYDensity<- min(DensityY[DensityX < 4 & DensityX > 2])
MinYDensity
#0.008210107
which(DensityY == MinYDensity)
#333
DensityX[333]
#3.272636

#Visualize your threshold here
#ggdensity(Coco2, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
#  scale_x_continuous(limits = c(1.5, 5))+
#  geom_vline(xintercept = density(Coco2$RED.B.HLog)$x[333])

#fluorescent signatures for each separately counted cell 
Coco2sym <- subset(Coco2, RED.B.HLog>=3.272636)
head(Coco2sym)
nrow(Coco2sym)
#ggdensity(Coco2sym, x = "RED.B.HLog", fill = "lightgray", rug = TRUE)+
  #scale_y_continuous(limits = c(0,0.3))+
#  scale_x_continuous(limits = c(1.5, 5))

#ggplot(Coco2sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

Coco2sym <- subset(Coco2sym, SSC.HLog > 0.5)

#ggplot(Coco2sym, aes(RED.B.HLog, SSC.HLog))+
#  geom_point()

#scatter3D(Coco2sym$RED.B.HLog, Coco2sym$FSC.HLog,  Coco2sym$SSC.HLog, 
 #         col = "#FFB000", 
  #        pch = 16, alpha = 0.2)
#plotrgl()

Lum1sym$Site <- "West"
Lum1sym$Time <- "May"
WA1sym$Site <- "Northwest"
WA1sym$Time <- "May"
Tog1sym$Site <- "East"
Tog1sym$Time <- "May"
Uru1sym$Site <- "North"
Uru1sym$Time <- "May"
Coco1sym$Site <- "South"
Coco1sym$Time <- "May"

Lum2sym$Site <- "West"
Lum2sym$Time <- "August"
WA2sym$Site <- "Northwest"
WA2sym$Time <- "August"
Tog2sym$Site <- "East"
Tog2sym$Time <- "August"
Uru2sym$Site <- "North"
Uru2sym$Time <- "August"
Coco2sym$Site <- "South"
Coco2sym$Time <- "August"

Lum1Sub <- Lum1sym[sample(nrow(Lum1sym), size = 1000), ]
WA1Sub <- WA1sym[sample(nrow(WA1sym), size = 1000), ]
Tog1Sub <- Tog1sym[sample(nrow(Tog1sym), size = 1000), ]
Uru1Sub <-Uru1sym[sample(nrow(Uru1sym), size = 1000), ]
Coco1Sub <-Coco1sym[sample(nrow(Coco1sym), size = 1000), ]

Lum2Sub <- Lum2sym[sample(nrow(Lum2sym), size = 1000), ]
WA2Sub <- WA2sym[sample(nrow(WA2sym), size = 1000), ]
Tog2Sub <- Tog2sym[sample(nrow(Tog2sym), size = 1000), ]
Uru2Sub <-Uru2sym[sample(nrow(Uru2sym), size = 1000), ]
Coco2Sub <-Coco2sym[sample(nrow(Coco2sym), size = 1000), ]

LumFullSub<-rbind(Lum1Sub, Lum2Sub)
WAFullSub<-rbind(WA1Sub, WA2Sub)

names(Tog1Sub)[names(Tog1Sub) == 'Subs'] <- 'P01.R1'
TogFullSub<-rbind(Tog1Sub, Tog2Sub)
UruFullSub<-rbind(Uru1Sub, Uru2Sub)
CocoFullSub<-rbind(Coco1Sub, Coco2Sub)

FullInSitu <- rbind(LumFullSub, WAFullSub)
FullInSitu <- rbind(FullInSitu, TogFullSub)
FullInSitu <- rbind(FullInSitu, UruFullSub)
FullInSitu <- rbind(FullInSitu, CocoFullSub)
head(FullInSitu)

FullInSitu$Site<-as.factor(FullInSitu$Site)
FullInSitu$Time<-as.factor(FullInSitu$Time)

levels(FullInSitu$Site)
levels(FullInSitu$Time)
scatter3D(FullInSitu$RED.B.HLog, FullInSitu$FSC.HLog, FullInSitu$SSC.HLog, 
          colvar = as.integer(FullInSitu$Site),
          phi = 6, bty ="g", ticktype = "detailed",
         col = c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000"), 
         pch = 16, alpha = 0.1)

scatter3D(FullInSitu$RED.B.HLog, FullInSitu$FSC.HLog, FullInSitu$SSC.HLog, 
          colvar = as.integer(FullInSitu$Time), phi = 6, ticktype = "detailed",
          col = c("#648FFF", "#FFB000"), 
          pch = 16, alpha = 0.1)

#Individual
LumFullSub$Time <- as.factor(LumFullSub$Time)
scatter3D(LumFullSub$RED.B.HLog, LumFullSub$FSC.HLog, LumFullSub$SSC.HLog, 
          colvar = as.integer(LumFullSub$Time), phi = 6, ticktype = "detailed",
          col = c("#648FFF", "#FFB000"), 
          pch = 16, alpha = 0.1)

WAFullSub$Time <- as.factor(WAFullSub$Time)
scatter3D(WAFullSub$RED.B.HLog, WAFullSub$FSC.HLog, WAFullSub$SSC.HLog, 
          colvar = as.integer(WAFullSub$Time), phi = 6, ticktype = "detailed",
          col = c("#648FFF", "#FFB000"), 
          pch = 16, alpha = 0.1)
TogFullSub$Time <- as.factor(TogFullSub$Time)
scatter3D(TogFullSub$RED.B.HLog, TogFullSub$FSC.HLog, TogFullSub$SSC.HLog, 
          colvar = as.integer(TogFullSub$Time), phi = 6, ticktype = "detailed",
          col = c("#648FFF", "#FFB000"), 
          pch = 16, alpha = 0.1)
UruFullSub$Time <- as.factor(UruFullSub$Time)
scatter3D(UruFullSub$RED.B.HLog, UruFullSub$FSC.HLog, UruFullSub$SSC.HLog, 
          colvar = as.integer(UruFullSub$Time), phi = 6, ticktype = "detailed",
          col = c("#648FFF", "#FFB000"), 
          pch = 16, alpha = 0.1)

CocoFullSub$Time <- as.factor(CocoFullSub$Time)
scatter3D(CocoFullSub$RED.B.HLog, CocoFullSub$FSC.HLog, CocoFullSub$SSC.HLog, 
          colvar = as.integer(CocoFullSub$Time), phi = 6, ticktype = "detailed",
          col = c("#648FFF", "#FFB000"), 
          pch = 16, alpha = 0.1)

# install.packages("ggridges")
library(ggridges)
# install.packages("ggplot2")
library(ggplot2)

ggplot(FullInSitu, aes(x = RED.B.HLog, y = Site, fill = Time)) +
  geom_density_ridges(aes(fill = Time, color = Time), size = 1.5, alpha = 0.5)+
  #geom_boxplot(aes(fill = Time), alpha = 0.5, size = 1.5)+
  scale_fill_manual(values = c("#648FFF", "#FFB000"))+
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  theme_classic2()
ggplot(FullInSitu, aes(x = SSC.HLog, y = Site, fill = Time)) +
  geom_density_ridges(aes(fill = Time, color = Time), size = 1.5, alpha = 0.5)+
  #geom_boxplot(aes(fill = Time), alpha = 0.5, size = 1.5)+
  scale_fill_manual(values = c("#648FFF", "#FFB000"))+
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  theme_classic2()
ggplot(FullInSitu, aes(x = FSC.HLog, y = Site)) +
  geom_density_ridges(aes(fill = Time, color = Time), size = 1.5, alpha = 0.5)+
  #geom_boxplot(aes(fill = Time), alpha = 0.5, size = 1.5)+
  scale_fill_manual(values = c("#648FFF", "#FFB000"))+
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  theme_classic2()
head(FullInSitu)

library(dplyr)
library(tidyr)
#install.packages("readr")
library(readr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gridExtra)
install.packages("ggalt")
library(ggalt)
library(ggthemes)
library(viridis)

###ASSIGN COLONY LEVEL INFO
View(Lum1Sub)

L1S2 <- Lum1sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(L1S2)

ggplot(L1S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

L1S2$colony<-replace(L1S2$well, L1S2$well == "A01", "W1")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A02", "W1")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A03", "W1")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A04", "W1")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A05", "W1")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A06", "W1")

L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A07", "W2")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A08", "W2")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A09", "W2")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A10", "W2")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A11", "W2")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "A12", "W2")

L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B01", "W3")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B02", "W3")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B03", "W3")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B04", "W3")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B05", "W3")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B06", "W3")

L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B07", "W4")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B08", "W4")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B09", "W4")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B10", "W4")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B11", "W4")
L1S2$colony<-replace(L1S2$colony, L1S2$colony == "B12", "W4")
View(L1S2)

L2S2 <- Lum2sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(L2S2)

ggplot(L2S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

L2S2$colony<-replace(L2S2$well, L2S2$well == "A01", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A02", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A03", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A04", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A05", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A06", "W1")

L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A07", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A08", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A09", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A10", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A11", "W1")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "A12", "W1")

L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B01", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B02", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B03", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B04", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B05", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B06", "W2")

L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B07", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B08", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B09", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B10", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B11", "W2")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "B12", "W2")

L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C01", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C02", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C03", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C04", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C05", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C06", "W3")

L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C07", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C08", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C09", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C10", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C11", "W3")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "C12", "W3")

L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D01", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D02", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D03", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D04", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D05", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D06", "W4")

L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D07", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D08", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D09", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D10", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D11", "W4")
L2S2$colony<-replace(L2S2$colony, L2S2$colony == "D12", "W4")
View(L2S2)

NW1S2 <- WA1sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(NW1S2)

ggplot(NW1S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

NW1S2$colony<-replace(NW1S2$well, NW1S2$well == "C01", "NW1")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C02", "NW1")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C03", "NW1")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C04", "NW1")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C05", "NW1")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C06", "NW1")

NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C07", "NW2")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C08", "NW2")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C09", "NW2")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C10", "NW2")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C11", "NW2")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "C12", "NW2")

NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D01", "NW3")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D02", "NW3")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D03", "NW3")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D04", "NW3")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D05", "NW3")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D06", "NW3")

NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D07", "NW4")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D08", "NW4")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D09", "NW4")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D10", "NW4")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D11", "NW4")
NW1S2$colony<-replace(NW1S2$colony, NW1S2$colony == "D12", "NW4")
View(NW1S2)

NW2S2 <- WA2sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(NW2S2)

ggplot(NW2S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

NW2S2$colony<-replace(NW2S2$well, NW2S2$well == "A01", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A02", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A03", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A04", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A05", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A06", "NW1")

NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A07", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A08", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A09", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A10", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A11", "NW1")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "A12", "NW1")

NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B01", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B02", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B03", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B04", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B05", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B06", "NW2")

NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B07", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B08", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B09", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B10", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B11", "NW2")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "B12", "NW2")

NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C01", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C02", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C03", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C04", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C05", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C06", "NW3")

NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C07", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C08", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C09", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C10", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C11", "NW3")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "C12", "NW3")

NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D01", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D02", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D03", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D04", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D05", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D06", "NW4")

NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D07", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D08", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D09", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D10", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D11", "NW4")
NW2S2$colony<-replace(NW2S2$colony, NW2S2$colony == "D12", "NW4")
View(NW2S2)

View(Uru1sym)
N1S2 <- Uru1sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(N1S2)

ggplot(N1S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

N1S2$colony<-replace(N1S2$well, N1S2$well == "C01", "N1")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C02", "N1")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C03", "N1")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C04", "N1")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C05", "N1")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C06", "N1")

N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C07", "N2")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C08", "N2")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C09", "N2")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C10", "N2")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C11", "N2")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "C12", "N2")

N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D01", "N3")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D02", "N3")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D03", "N3")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D04", "N3")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D05", "N3")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D06", "N3")

N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D07", "N4")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D08", "N4")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D09", "N4")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D10", "N4")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D11", "N4")
N1S2$colony<-replace(N1S2$colony, N1S2$colony == "D12", "N4")

N2S2 <- Uru2sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(N2S2)

ggplot(N2S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

N2S2$colony<-replace(N2S2$well, N2S2$well == "A01", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A02", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A03", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A04", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A05", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A06", "N1")

N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A07", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A08", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A09", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A10", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A11", "N1")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "A12", "N1")

N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B01", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B02", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B03", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B04", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B05", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B06", "N2")

N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B07", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B08", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B09", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B10", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B11", "N2")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "B12", "N2")

N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C01", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C02", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C03", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C04", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C05", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C06", "N3")

N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C07", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C08", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C09", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C10", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C11", "N3")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "C12", "N3")

N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D01", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D02", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D03", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D04", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D05", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D06", "N4")

N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D07", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D08", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D09", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D10", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D11", "N4")
N2S2$colony<-replace(N2S2$colony, N2S2$colony == "D12", "N4")
View(N2S2)

S1S2 <- Coco1sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(S1S2)

ggplot(S1S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

S1S2$colony<-replace(S1S2$well, S1S2$well == "A01", "S1")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A02", "S1")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A03", "S1")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A04", "S1")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A05", "S1")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A06", "S1")

S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A07", "S2")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A08", "S2")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A09", "S2")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A10", "S2")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A11", "S2")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "A12", "S2")

S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B01", "S3")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B02", "S3")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B03", "S3")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B04", "S3")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B05", "S3")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B06", "S3")

S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B07", "S4")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B08", "S4")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B09", "S4")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B10", "S4")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B11", "S4")
S1S2$colony<-replace(S1S2$colony, S1S2$colony == "B12", "S4")

S2S2 <- Coco2sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(S2S2)

ggplot(S2S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

S2S2$colony<-replace(S2S2$well, S2S2$well == "E01", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E02", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E03", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E04", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E05", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E06", "S1")

S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E07", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E08", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E09", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E10", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E11", "S1")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "E12", "S1")

S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F01", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F02", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F03", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F04", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F05", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F06", "S2")

S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F07", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F08", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F09", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F10", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F11", "S2")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "F12", "S2")

S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G01", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G02", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G03", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G04", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G05", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G06", "S3")

S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G07", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G08", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G09", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G10", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G11", "S3")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "G12", "S3")

S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H01", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H02", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H03", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H04", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H05", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H06", "S4")

S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H07", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H08", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H09", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H10", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H11", "S4")
S2S2$colony<-replace(S2S2$colony, S2S2$colony == "H12", "S4")

E1S2 <- Tog1sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
View(E1S2)

ggplot(E1S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()

E1S2$colony<-replace(E1S2$well, E1S2$well == "A01", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A02", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A03", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A04", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A05", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A06", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A07", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A08", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A09", "E1")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A10", "E1")

E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A11", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "A12", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B01", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B02", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B03", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B04", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B05", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B06", "E2")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B07", "E2")

E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B08", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B09", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B10", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B11", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "B12", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C01", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C02", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C03", "E3")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C04", "E3")

E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C05", "E4")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C06", "E4")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C07", "E4")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C08", "E4")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C09", "E4")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C10", "E4")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C11", "E4")
E1S2$colony<-replace(E1S2$colony, E1S2$colony == "C12", "E4")

E2S2 <- Tog2sym %>% 
  separate(path, c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"well",NA))
head(E2S2)

ggplot(E2S2, aes(Site, RED.B.HLog))+
  #geom_smooth(aes(color = dilution), se = FALSE)+
  #geom_boxplot(aes(group = interaction(Time, Site)))+
  geom_tufteboxplot(aes(color = well), outlier.alpha = 0, alpha = 0.8)+
  #scale_y_continuous(limits = c(2.8,4.7))+
  xlab("")+
  theme_classic()
View(E2S2)
E2S2$colony<-replace(E2S2$well, E2S2$well == "A01", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A02", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A03", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A04", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A05", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A06", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A07", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A08", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A09", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A10", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A11", "E1")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "A12", "E1")

E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B01", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B02", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B03", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B04", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B05", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B06", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B07", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B08", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B09", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B10", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B11", "E2")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "B12", "E2")

E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C01", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C02", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C03", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C04", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C05", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C06", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C07", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C08", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C09", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C10", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C11", "E3")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "C12", "E3")

E2S2$colony<-replace(E2S2$colony, E2S2$colony == "D01", "E4")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "D02", "E4")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "D03", "E4")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "D04", "E4")
E2S2$colony<-replace(E2S2$colony, E2S2$colony == "D05", "E4")

WFull<-rbind(L1S2, L2S2)
NWFull<-rbind(NW1S2, NW2S2)
NFull<- rbind(N1S2,N2S2)
SFull <- rbind(S1S2, S2S2)
EFull <- rbind(E1S2, E2S2)

FullInSitu2 <- rbind(WFull, NWFull)
FullInSitu2 <- rbind(FullInSitu2, NFull)
FullInSitu2 <- rbind(FullInSitu2, SFull)
FullInSitu2 <- rbind(FullInSitu2, EFull)

Subset<-FullInSitu2 %>% group_by(interaction(colony,Site,Time)) %>% sample_n(size = 1000)

ggplot(Subset, aes(Site, RED.B.HLog))+
  geom_tufteboxplot(aes(group = interaction(colony,Time), color = Time), size = 1.3, alpha = 0.8)+
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  xlab("")+
  theme_classic()

ggplot(Subset, aes(Site, FSC.HLog))+
  geom_tufteboxplot(aes(group = interaction(colony,Time), color = Time), size = 1.3, alpha = 0.8)+
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")

ggplot(Subset, aes(Site, SSC.HLog))+
  geom_tufteboxplot(aes(group = interaction(colony,Time), color = Time), size = 1.3, alpha = 0.8)+
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
colnames(Subset)

names(Subset)[names(Subset) == 'interaction(colony, Site, Time)'] <- 'interactions'


#install.packages("FSA")
library(FSA)
#install.packages("rcompanion")
library(rcompanion)

kruskal.test(RED.B.HLog ~ interactions, data = Subset)
#Post-hoc comparison of red fluorescence
REDstats <- dunnTest(RED.B.HLog ~ interactions, data = Subset, method = "bh")
REDstats
REDstats = REDstats$res
cldoutput <- cldList(comparison = REDstats$Comparison, p.value = REDstats$P.adj, threshold = 0.001)
head(cldoutput)
cldRED <- as.data.frame(cldoutput)
head(cldRED)
names(cldRED)[names(cldRED) == 'Group'] <- 'interactions'
names(cldRED)[names(cldRED) == 'Letter'] <- 'RED.Letter'
names(cldRED)[names(cldRED) == 'MonoLetter'] <- 'RED.MonoLetter'
Wombo<-merge(Subset, cldRED, by=c("interactions"))
head(Wombo)

Wombo$Site <- as.factor(Wombo$Site)
Wombo$interactions <- as.factor(Wombo$interactions)
Wombo$Time <- as.factor(Wombo$Time)
ggplot()+
  geom_tufteboxplot(data = Wombo, aes(x = Site, y = RED.B.HLog, group = interactions, color = Time), 
                    size = 1.3, alpha = 0.8)+
  geom_text(data = Wombo, aes(x = Site, y = mean(RED.B.HLog), group = interactions, label = RED.MonoLetter), 
            angle = 90, size = 5, position = position_dodge(0.9)) + 
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  xlab("")+
  theme_classic()

kruskal.test(FSC.HLog ~ interactions, data = Subset)
#Post-hoc comparison of genera
FSCstats <- dunnTest(FSC.HLog ~ interactions, data = Subset, method = "bh")
FSCstats
FSCstats = FSCstats$res
cldoutput <- cldList(comparison = FSCstats$Comparison, p.value = FSCstats$P.adj, threshold = 0.001)
head(cldoutput)
cldFSC <- as.data.frame(cldoutput)
head(cldFSC)
names(cldFSC)[names(cldFSC) == 'Group'] <- 'interactions'
names(cldFSC)[names(cldFSC) == 'Letter'] <- 'FSC.Letter'
names(cldFSC)[names(cldFSC) == 'MonoLetter'] <- 'FSC.MonoLetter'
Wombo<-merge(Wombo, cldFSC, by=c("interactions"))
head(Wombo)

ggplot()+
  geom_tufteboxplot(data = Wombo, aes(x = Site, y = FSC.HLog, group = interactions, color = Time), 
                    size = 1.3, alpha = 0.8)+
  geom_text(data = Wombo, aes(x = Site, y = mean(FSC.HLog), group = interactions, label = FSC.MonoLetter), 
            angle = 90, size = 5, position = position_dodge(0.95)) + 
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  xlab("")+
  theme_classic()

kruskal.test(SSC.HLog ~ interactions, data = Subset)
#Post-hoc comparison of genera
SSCstats <- dunnTest(SSC.HLog ~ interactions, data = Subset, method = "bh")
SSCstats
SSCstats = SSCstats$res
cldoutput <- cldList(comparison = SSCstats$Comparison, p.value = SSCstats$P.adj, threshold = 0.001)
head(cldoutput)
cldSSC <- as.data.frame(cldoutput)
head(cldSSC)
names(cldSSC)[names(cldSSC) == 'Group'] <- 'interactions'
names(cldSSC)[names(cldSSC) == 'Letter'] <- 'SSC.Letter'
names(cldSSC)[names(cldSSC) == 'MonoLetter'] <- 'SSC.MonoLetter'
Wombo<-merge(Wombo, cldSSC, by=c("interactions"))
head(Wombo)

ggplot()+
  geom_tufteboxplot(data = Wombo, aes(x = Site, y = SSC.HLog, group = interactions, color = Time), 
                    size = 1.3, alpha = 0.8)+
  geom_text(data = Wombo, aes(x = Site, y = mean(SSC.HLog), group = interactions, label = SSC.MonoLetter), 
            angle = 90, size = 5, position = position_dodge(0.95)) + 
  scale_color_manual(values = c("#648FFF", "#FFB000"))+
  xlab("")+
  theme_classic()

##REDESIGN CELL DENSITY FIGURE
