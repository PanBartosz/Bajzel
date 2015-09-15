library(lattice)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(aod)
data <- read.csv("Sat Sep 12 16:17:23 2015/cleanData28553.csv")
adverbs <-c("Specjalnie", "Świadomie", "Celowo", "Umyślnie", "Brak przysłówka",
            "Chciał", "Z rozmysłem", "Zamierzał")
data <- data[which(data$ADVERB %in% adverbs),]
data.specjalnie <- filter(data, ADVERB=="Specjalnie")
data.swiadomie <- filter(data, ADVERB=="Świadomie")
data.celowo <- filter(data, ADVERB=="Celowo")
data.umyslnie <- filter(data, ADVERB=="Umyślnie")
data.brak <- filter(data, ADVERB=="Brak przysłówka")
data.zrozmyslem <- filter(data, ADVERB=="Z rozmysłem")

data2 <- read.csv(file.path(date, "cleanKasiaBartosz2169464.csv"))
adverbs2 <-c("Umyślnie", "Z rozmysłem")
data.umyslnie2 <- filter(data, ADVERB=="Umyślnie")
data.zrozmyslem2 <- filter(data, ADVERB=="Z rozmysłem")

data<- data[which(data$RESPONSE !=0),]

attach(data)
xtable <-xtabs(~ RESPONSE + GROUP + ADVERB, drop.unused.levels = TRUE)
xtable[1:2,1:2,1] <- prop.table(xtable[1:2,1:2,1],2)
xtable[1:2,1:2,2] <- prop.table(xtable[1:2,1:2,2],2)
xtable[1:2,1:2,3] <- prop.table(xtable[1:2,1:2,3],2)
xtable[1:2,1:2,4] <- prop.table(xtable[1:2,1:2,4],2)
xtable[1:2,1:2,5] <- prop.table(xtable[1:2,1:2,5],2)
xtable[1:2,1:2,6] <- prop.table(xtable[1:2,1:2,6],2)
xtable[1:2,1:2,7] <- prop.table(xtable[1:2,1:2,7],2)
xtable[1:2,1:2,8] <- prop.table(xtable[1:2,1:2,8],2)
barchart(xtable, groups = ADVERB, horizontal = 0)
detach(data)

DATA <- as.data.frame(xtable)
DATA$RESPONSE <- factor(DATA$RESPONSE, levels = levels(DATA$RESPONSE)[c(1,2)])
DATA <- DATA[order(DATA$RESPONSE), ]
DATA <- DATA[c(17,18,19,20,23,24,25,26,27,28,29,30,21,22,31,32,1,2,3,4,7,8,9,10,11,12,13,14,5,6,15,16), ]

DATA$FreqPercen <- paste(as.character(round(DATA$Freq * 100)), "%", sep = "")
colnames(DATA)[1] <- "Intencjonalność"
DATA$Intencjonalność <- factor(DATA$Intencjonalność,levels(DATA$Intencjonalność)[c(2,1)])
DATA$ADVERB <- factor(DATA$ADVERB,levels(DATA$ADVERB)[c(1,2,4,5,6,7,3,8)])

plot <- ggplot(data = DATA)

png("p_chart1.png", width = 1366, height = 786)

plot + aes(y = Freq, x = GROUP, fill=Intencjonalność) + geom_bar(stat = "identity") + facet_grid(~ADVERB) + scale_fill_brewer(palette = "Pastel1")

dev.off()


png("bwp_chart1.png", width = 1366, height = 786)
plot + aes(y = Freq, x = ADVERB, fill=Intencjonalność) + geom_bar(stat = "identity") + 
facet_grid(GROUP~.) + scale_fill_grey(end = 0.8, start = 0.5) + theme_bw() +
theme(axis.text=element_text(size=24, family = "Times"), axis.title=element_text(size=0), legend.text=element_text(size=30, family="Times"), 
      legend.title=element_text(size=30, family="Times"), strip.text=element_text(size=32, family = "Times"), legend.key.height=unit(3,"line"), legend.key.width=unit(3,"line"))
dev.off()

png("p_chart2.png", width = 1366, height = 786)
colnames(DATA)[3] <- "Modyfikator"
DATA[which(DATA$Intencjonalność == "Nie"), ]$FreqPercen <- ""
plot <- ggplot(data = DATA)
plot + aes(y = Freq, x = Modyfikator, fill=Intencjonalność) + geom_bar(stat = "identity") + facet_grid(GROUP~.) + scale_fill_brewer(palette = "Pastel1") +
  theme(legend.key.height=unit(3,"line"), legend.key.width=unit(3,"line"), legend.text=element_text(size=16), 
        legend.title=element_text(size=16), strip.text=element_text(size=18)) + 
    geom_text(aes(label = FreqPercen), size = 5, hjust = 0.5, vjust = -1, position = "stack") 
dev.off()



# Tutaj kombinacje z logitem
# Płeć 
dataG<- data[which(data$Gender == "Kobieta" | data$Gender == "Mężczyzna"),]
dataG <- dataG[which(dataG$Birth > 1940 & dataG$Birth < 2010),]
dataG$Age <- 2015 - dataG$Birth
dataG <- dataG[which(dataG$Education %in% levels(dataG$Education)[c(2,3,5,6,7)]) ,]

logitplec <- glm(RESPONSE ~ Gender, data = dataG, family = "binomial")
summary(logitplec)

# Wiek
logitwiek <- glm(RESPONSE ~ Age, data = dataG, family = "binomial")
summary(logitwiek)

#Zbiorcze

logit <- glm(RESPONSE ~ GROUP + Gender + Age + Education + Philosophical_Education, data = dataG, family = "binomial")
summary(logit)
logit2 <- glm(RESPONSE ~ GROUP + ADVERB + Gender + Age + Education + Philosophical_Education, data = dataG, family = "binomial")
summary(logit2)
logit3 <- glm(RESPONSE ~ GROUP + RESPONSIBILITY + Gender + Age + Education + Philosophical_Education, data = dataG, family = "binomial")
summary(logit3)
logit4 <- glm(RESPONSIBILITY ~ GROUP + RESPONSE + ADVERB + Gender + Age + Education + Philosophical_Education, data = dataG)
summary(logit4)