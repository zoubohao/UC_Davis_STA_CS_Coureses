cData = survival::colon
deathCData = cData[which(cData$etype == 2),]
recurCData = cData[which(cData$etype == 1),]

deathNewData = subset(deathCData, select = -c(id, study, node4, etype))
deathNewData = na.omit(deathNewData)
#write.csv(deathNewData,"d:\\colon.csv", row.names = F)
recurNewData = subset(recurCData, select = -c(id, study, node4, etype))
recurNewData = na.omit(recurNewData)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

## density of different rx for death
type_time = data.frame(rx = deathNewData$rx[which(deathNewData$status == 1)], 
                       time = deathNewData$time[which(deathNewData$status == 1)])

p = ggplot(data = type_time, aes(x = time, group = rx, fill = rx)) + 
  geom_histogram( alpha = .5,bins = 50) + theme_ipsum()
p

p = ggplot(data = type_time, aes(x = rx, y = time, fill = rx)) + 
  geom_boxplot()  +  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
p

##
type_time = data.frame(rx = recurNewData$rx[which(recurNewData$status == 1)], 
                       time = recurNewData$time[which(recurNewData$status == 1)])
p = ggplot(data = type_time, aes(x = time, group = rx, fill = rx)) + 
  geom_histogram( alpha = .5,bins = 50) + theme_ipsum()
p

p = ggplot(data = type_time, aes(x = rx, y = time, fill = rx)) + 
  geom_boxplot()  +  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
p


type_time = data.frame(rx = deathNewData$rx[which(deathNewData$status == 1)], 
                       time = deathNewData$time[which(deathNewData$status == 1)],
                       differ = as.factor(deathNewData$differ[which(deathNewData$status == 1)]))
p = ggplot(data = type_time, aes(x = rx, y = time, fill=differ)) + 
  geom_boxplot() + theme_ipsum()
p












