#Math Quest Baseball

library(gcookbook)
library(ggplot2)
library(dplyr)
library(readxl)

hitting2005 <- read_excel("Work/Baseball MathQuest/Hitting2005.xls.xlsx")
hitting2006 <- read_excel("Work/Baseball MathQuest/Hitting2006.xls.xlsx")
hitting2007 <- read_excel("Work/Baseball MathQuest/Hitting2007.xls.xlsx")
hitting2008 <- read_excel("Work/Baseball MathQuest/Hitting2008.xls.xlsx")
hitting2009 <- read_excel("Work/Baseball MathQuest/Hitting2009.xls.xlsx")
hitting2010 <- read_excel("Work/Baseball MathQuest/Hitting2010.xls.xlsx")
hitting2011 <- read_excel("Work/Baseball MathQuest/Hitting2011.xls.xlsx")
hitting2012 <- read_excel("Work/Baseball MathQuest/Hitting2012.xls.xlsx")
hitting2013 <- read_excel("Work/Baseball MathQuest/Hitting2013.xls.xlsx")
hitting2014 <- read_excel("Work/Baseball MathQuest/Hitting2014.xls.xlsx")
hitting2015 <- read_excel("Work/Baseball MathQuest/Hitting2015.xls.xlsx")
hitting2016 <- read_excel("Work/Baseball MathQuest/Hitting2016.xls.xlsx")
hitting2017 <- read_excel("Work/Baseball MathQuest/Hitting2017.xls.xlsx")
hitting2018 <- read_excel("Work/Baseball MathQuest/Hitting2018.xls.xlsx")
hitting2019 <- read_excel("Work/Baseball MathQuest/Hitting2019.xls.xlsx")
hitting2020 <- read_excel("Work/Baseball MathQuest/Hitting2020.xls.xlsx")
hitting2021 <- read_excel("Work/Baseball MathQuest/Hitting2021.xls.xlsx")
hitting2022 <- read_excel("Work/Baseball MathQuest/Hitting2022.xls.xlsx")
hitting2023 <- read_excel("Work/Baseball MathQuest/Hitting2023.xls.xlsx")
hitting2024 <- read_excel("Work/Baseball MathQuest/Hitting2024.xls.xlsx")


hitting <- merge(hitting2005[-c(31,32), ], hitting2006[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2007[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2008[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2009[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2010[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2011[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2012[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2013[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2014[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2015[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2016[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2017[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2018[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2019[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2021[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2022[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2023[-c(31,32), ], all.x = TRUE, all.y = TRUE)
hitting <- merge(hitting, hitting2024[-c(31,32), ], all.x = TRUE, all.y = TRUE)

for (i in 1:nrow(hitting)) {
  if (hitting$Tm[i] == "Cleveland Indians") {
    hitting$Tm[i] <- "Cleveland Guardians"
  }
  if(hitting$Tm[i] == "Los Angeles Angels of Anaheim"){
    hitting$Tm[i] <- "Los Angeles Angels"
  }
}

hitting$Tm <- factor(hitting$Tm)

ggplot(hitting, mapping = aes(y = R, x = H))+
  geom_point(aes(color = Tm), show.legend = FALSE) +
  labs(title = "Runs Vs Hits",
       xlab = "Hits",
       ylab = "Total Runs") + 
  annotate("text", x=0.9*(max(hitting$H)), y = 500, label = round(cor(hitting$R, hitting$H), 5))

ggplot(hitting, mapping = aes(y = R, x = HR))+
  geom_point(aes(color = Tm), show.legend = FALSE) +
  labs(title = "Runs Vs Home Runs",
       xlab = "Home Runs",
       ylab = "Total Runs") + 
  annotate("text", x=0.9*(max(hitting$HR)), y = 500, label = round(cor(hitting$R, hitting$HR), 5))

ggplot(hitting, mapping = aes(y = R, x = SB))+
  geom_point(aes(color = Tm), show.legend = FALSE) +
  labs(title = "Runs Vs Stolen Bases",
       xlab = "Hits",
       ylab = "Stolen Bases") + 
  annotate("text", x=0.9*(max(hitting$SB)), y = 500, label = round(cor(hitting$R, hitting$SB), 5))

ggplot(hitting, mapping = aes(y = R, x = BA))+
  geom_point(aes(color = Tm), show.legend = FALSE) +
  labs(title = "Runs Vs Batting Average",
       xlab = "Hits",
       ylab = "Batting Average") + 
  annotate("text", x=0.9*(max(hitting$BA)), y = 500, label = round(cor(hitting$R, hitting$BA), 5))

ggplot(hitting, mapping = aes(y = R, x = SO))+
  geom_point(aes(color = Tm), show.legend = FALSE) +
  labs(title = "Runs Vs Strikeouts",
       xlab = "Hits",
       ylab = "Strikeouts") + 
  annotate("text", x=0.9*(max(hitting$SO)), y = 500, label = round(cor(hitting$R, hitting$SO), 5))

ggplot(hitting, mapping = aes(y = R, x = OBP))+
  geom_point(aes(color = Tm), show.legend = FALSE) +
  labs(title = "Runs Vs On Base Percentage",
       xlab = "Hits",
       ylab = "On Base Percentage") + 
  annotate("text", x=0.95*(max(hitting$OBP)), y = 500, label = round(cor(hitting$R, hitting$OBP), 5))



ggplot(hitting, mapping = aes(y = R, x = SLG))+
  geom_point(aes(color = Tm), show.legend = FALSE) +
  labs(title = "Runs Vs On Base Percentage",
       xlab = "Hits",
       ylab = "On Base Percentage") + 
  annotate("text", x=0.95*(max(hitting$SLG)), y = 500, label = round(cor(hitting$R, hitting$SLG), 5))
