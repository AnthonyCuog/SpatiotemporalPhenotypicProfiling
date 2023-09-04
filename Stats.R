library(MANOVA.RM)
library(readxl)
library(SimComp)
library(dplyr)

options(max.print=1000000)

#Load data
WildMANOVA <- read_excel("/ColinAnthony/Thesis/Data/FinalFluorData.xlsx", 
                             sheet = "MANOVAWild", col_types = c("text", 
                                                                 "text", "text", "text", "text", "text", "text", "numeric", 
                                                                 "numeric", "numeric", "numeric"))
View(WildMANOVA)

set.seed(123)

##MATS Multivariate
mult1 <- multRM(cbind(Cells, Chl, Car, Flavo) ~ Time * Site * Plot2,
                data = WildMANOVA, iter = 1000, subject = "Replicate",
                seed = 123, CPU = 1, within = c("Site", "Plot2"))
summary(mult1)
View(WildMANOVA)
mult1Tuk <- simCI(mult1, contrast = "pairwise", type = "Tukey")
mult1Tuk
summary(mult1Tuk)
plot(mult1Tuk)
mult1GM <- simCI(mult1, contrast = "pairwise", type = "GrandMean")
summary(mult1GM)
plot(mult1GM)
TukeyHSD(mult1, "Site")

mult2 <- multRM(cbind(Chl, Car, Flavo) ~ Time * Site * Plot2,
                data = WildMANOVA, iter = 1000, subject = "Replicate",
                seed = 123, CPU = 1, within = c("Site", "Plot2"))
summary(mult2)

mult2Tuk <- simCI(mult2, contrast = "pairwise", type = "Tukey")
mult2Tuk
summary(mult2Tuk)
plot(mult2Tuk)
mult2GM <- simCI(mult2, contrast = "pairwise", type = "AVE")
mult2GM
summary(mult2GM)
plot(mult2GM)



#WTS Univariate
WildCells <- MANOVA.wide(cbind(Cells) ~ Time * Site * Plot2,
                       data = WildMANOVA, iter = 1000, Subject = Replicate, nsubj = 2,
                       nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildCells)

CellsTuk <- simCI(WildCells, contrast = "pairwise", type = "Tukey")
CellsTuk
summary(CellsTuk)
plot(CellsTuk)
CellsGM <- simCI(WildCells, contrast = "pairwise", type = "GrandMean")
CellsGM
summary(CellsGM)
plot(CellsGM)

WildCar <- MANOVA.wide(cbind(Car) ~ Time * Site * Plot2,
                    data = WildMANOVA, iter = 1000, Subject = Replicate, nsubj = 2,
                    nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildCar)

CarTuk <- simCI(WildCar, contrast = "pairwise", type = "Tukey")
CarTuk
summary(CarTuk)
plot(CarTuk)
CarGM <- simCI(WildCar, contrast = "pairwise", type = "GrandMean")
CarGM
summary(CarGM)
plot(CarGM)

WildFlavo <- MANOVA.wide(cbind(Flavo) ~ Time * Site * Plot2,
                        data = WildMANOVA, iter = 1000, Subject = Replicate, nsubj = 2,
                        nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildFlavo)

FlavTuk <- simCI(WildFlavo, contrast = "pairwise", type = "Tukey")
summary(FlavTuk)
plot(FlavTuk)
FlavGM <- simCI(WildFlavo, contrast = "pairwise", type = "GrandMean")
FlavGM
summary(FlavGM)
plot(FlavGM)

WildChl <- MANOVA.wide(cbind(Chl) ~ Time * Site * Plot2,
                        data = WildMANOVA, iter = 1000, Subject = Replicate, nsubj = 2,
                        nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildChl)

ChlTuk <- simCI(WildChl, contrast = "pairwise", type = "Tukey")
ChlTuk
summary(ChlTuk)
plot(ChlTuk)
ChlGM <- simCI(WildChl, contrast = "pairwise", type = "GrandMean")
ChlGM
summary(ChlGM)
plot(ChlGM)

#Part2
##Togcha Removed

WildMANOVA2 <- filter(WildMANOVA, Site != "TOG")
head(WildMANOVA2)
##Multivariate MATS
multTSP1 <- multRM(cbind(Cells, Chl, Car, Flavo) ~ Time * Site * Plot2,
                   data = WildMANOVA2, iter = 1000, subject = "Replicate",
                   seed = 123, CPU = 1, within = c("Site", "Plot2"))
summary(multTSP1)
library(MANOVA.RM)
TSP1Tuk <- simCI(multTSP1, contrast = "pairwise", type = "Tukey")
summary(TSP1Tuk)

plot(TSP1Tuk)
TSP1GM <- simCI(multTSP1, contrast = "pairwise", type = "GrandMean")
plot(TSP1GM)

TukeyHSD(multTSP1, ordered=FALSE)

multTSP2 <- multRM(cbind(Chl, Car, Flavo) ~ Time * Site * Plot2,
                   data = WildMANOVA2, iter = 1000, subject = "Replicate",
                   seed = 123, CPU = 1, within = c("Site", "Plot2"))
summary(multTSP)

TSP2Tuk <- simCI(multTSP2, contrast = "pairwise", type = "Tukey")
plot(TSP2Tuk)
TSP2GM <- simCI(multTSP2, contrast = "pairwise", type = "GrandMean")
plot(TSP2GM)

#WTS
WildCells2 <- MANOVA.wide(cbind(Cells) ~ Time * Site * Plot2,
                         data = WildMANOVA2, iter = 1000, Subject = Replicate, nsubj = 2,
                         nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildCells2)

WildCar2 <- MANOVA.wide(cbind(Car) ~ Time * Site * Plot2,
                       data = WildMANOVA2, iter = 1000, Subject = Replicate, nsubj = 2,
                       nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildCar2)

WildFlavo2 <- MANOVA.wide(cbind(Flavo) ~ Time * Site * Plot2,
                         data = WildMANOVA2, iter = 1000, Subject = Replicate, nsubj = 2,
                         nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildFlavo2)

WildChl2 <- MANOVA.wide(cbind(Chl) ~ Time * Site * Plot2,
                       data = WildMANOVA2, iter = 1000, Subject = Replicate, nsubj = 2,
                       nested.levels.unique = FALSE, seed = 123, CPU = 1, interaction = TRUE)
summary(WildChl2)


##Urunao only

WildMANOVA3 <- subset(WildMANOVA, Site == "URU")
head(WildMANOVA3)

multUru <- multRM(cbind(Cells, Chl, Car, Flavo) ~ Time * Plot2,
                  data = WildMANOVA3, iter = 1000, subject = "Replicate",
                  seed = 123, CPU = 1, within = c("Plot2"))
summary(multUru)

multUru2 <- multRM(cbind(Chl, Car, Flavo) ~ Time * Plot2,
                  data = WildMANOVA3, iter = 1000, subject = "Replicate",
                  seed = 123, CPU = 1, within = c("Plot2"))
summary(multUru2)

UruTuk <- simCI(multUru, contrast = "pairwise", type = "Tukey")
plot(UruTuk)
UruGM <- simCI(multUru, contrast = "pairwise", type = "GrandMean")
plot(UruGM)

Uru2Tuk <- simCI(multUru2, contrast = "pairwise", type = "Tukey")
plot(Uru2Tuk)
Uru2GM <- simCI(multUru2, contrast = "pairwise", type = "GrandMean")
plot(Uru2GM)
View(WildMANOVA3)

UruCell <- multRM(cbind(Cells) ~ Time * Plot2,
                  data = WildMANOVA3, iter = 1000, subject = "Replicate",
                  seed = 123, CPU = 1, within = c("Plot2"))
summary(multUru)

UruFlav <- multRM(cbind(Flavo) ~ Time * Plot2,
                   data = WildMANOVA3, iter = 1000, subject = "Replicate",
                   seed = 123, CPU = 1, within = c("Plot2"))
summary(multUru2)

multUru <- multRM(cbind(Chl) ~ Time * Plot2,
                  data = WildMANOVA3, iter = 1000, subject = "Replicate",
                  seed = 123, CPU = 1, within = c("Plot2"))
summary(multUru)

multUru2 <- multRM(cbind(Car) ~ Time * Plot2,
                   data = WildMANOVA3, iter = 1000, subject = "Replicate",
                   seed = 123, CPU = 1, within = c("Plot2"))
summary(multUru2)

WildMANOVA4 <- subset(WildMANOVA3, Plot2 == c(1,4))
head(WildMANOVA4)
WildMANOVA4 <- subset(WildMANOVA4, Time == c(1))
head(WildMANOVA4)
View(WildMANOVA4)
kruskal.test(Chl ~ Plot2, data = WildMANOVA4)
kruskal.test(Car ~ Plot2, data = WildMANOVA4)
kruskal.test(Flavo ~ Plot2, data = WildMANOVA4)
kruskal.test(Cells ~ Plot2, data = WildMANOVA4)
