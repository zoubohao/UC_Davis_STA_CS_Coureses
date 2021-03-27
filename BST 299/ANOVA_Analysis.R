library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

data_3dpf = read.csv("d:\\3dpfCompletedScore.csv")
data_5dpf = read.csv("d:\\5dpfCompletedScore.csv")

### analysis for 3dpf data
wt_3dpf = data_3dpf[which(data_3dpf$Label == "wt"),]
homo_3dpf = data_3dpf[which(data_3dpf$Label == "homo"),]
het_3dpf = data_3dpf[which(data_3dpf$Label == "het"),]


anovaSingleDataFrameGenerate = function(dataSet) {
  n = dim(dataSet)[1]
  head = rep("head", n)
  body = rep("body", n)
  tail = rep("tail", n)
  whole = rep("whole", n)
  Group = c(head, body, tail, whole)
  Scores = c(dataSet$head, dataSet$body, dataSet$tail, dataSet$whole)
  anovaDataFrame = data.frame(Scores = Scores, Group = Group)
  return(anovaDataFrame)
}

anovaDoubleDataFrameGenerate = function(dataSet) {
  n = dim(dataSet)[1]
  head_body = rep("head_body", n)
  head_tail = rep("head_tail", n)
  body_tail = rep("body_tail", n)
  whole = rep("whole", n)
  Group = c(head_body, head_tail, body_tail, whole)
  Scores = c(dataSet$head_body, dataSet$head_tail, dataSet$body_tail, dataSet$whole)
  anovaDataFrame = data.frame(Scores = Scores, Group = Group)
  return(anovaDataFrame)
}

### wt 3dpf

## head vs. whole
ks.test(wt_3dpf$head, wt_3dpf$whole)
## body vs. whole
ks.test(wt_3dpf$body, wt_3dpf$whole)
## tail vs. whole
ks.test(wt_3dpf$tail, wt_3dpf$whole)

## head_body vs. whole
ks.test(wt_3dpf$head_body, wt_3dpf$whole)
## head_tail vs. whole
ks.test(wt_3dpf$head_tail, wt_3dpf$whole)
## body_tail vs.whole
ks.test(wt_3dpf$body_tail, wt_3dpf$whole)

wt_anovaFrame3Single = anovaSingleDataFrameGenerate(wt_3dpf)
pdensity <- ggplot(wt_anovaFrame3Single, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

wt_anovaFrame3Double = anovaDoubleDataFrameGenerate(wt_3dpf)
pdensity <- ggplot(wt_anovaFrame3Double, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

### het 3dpf
## head vs. whole
ks.test(het_3dpf$head, het_3dpf$whole)
## body vs. whole
ks.test(het_3dpf$body, het_3dpf$whole)
## tail vs. whole
ks.test(het_3dpf$tail, het_3dpf$whole)

## head_body vs. whole
ks.test(het_3dpf$head_body, het_3dpf$whole)
## head_tail vs. whole
ks.test(het_3dpf$head_tail, het_3dpf$whole)
## body_tail vs.whole
ks.test(het_3dpf$body_tail, het_3dpf$whole)

wt_anovaFrame3Single = anovaSingleDataFrameGenerate(het_3dpf)
pdensity <- ggplot(wt_anovaFrame3Single, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

wt_anovaFrame3Double = anovaDoubleDataFrameGenerate(het_3dpf)
pdensity <- ggplot(wt_anovaFrame3Double, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

### homo 3dpf
## head vs. whole
ks.test(homo_3dpf$head, homo_3dpf$whole)
## body vs. whole
ks.test(homo_3dpf$body, homo_3dpf$whole)
## tail vs. whole
ks.test(homo_3dpf$tail, homo_3dpf$whole)

## head_body vs. whole
ks.test(homo_3dpf$head_body, homo_3dpf$whole)
## head_tail vs. whole
ks.test(homo_3dpf$head_tail, homo_3dpf$whole)
## body_tail vs.whole
ks.test(homo_3dpf$body_tail, homo_3dpf$whole)

wt_anovaFrame3Single = anovaSingleDataFrameGenerate(homo_3dpf)
pdensity <- ggplot(wt_anovaFrame3Single, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

wt_anovaFrame3Double = anovaDoubleDataFrameGenerate(homo_3dpf)
pdensity <- ggplot(wt_anovaFrame3Double, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

### analysis for 5dpf data
wt_5dpf = data_5dpf[which(data_5dpf$Label == "wt"),]
homo_5dpf = data_5dpf[which(data_5dpf$Label == "homo"),]
het_5dpf = data_5dpf[which(data_5dpf$Label == "het"),]
### wt 5dpf

## head vs. whole
ks.test(wt_5dpf$head, wt_5dpf$whole)
## body vs. whole
ks.test(wt_5dpf$body, wt_5dpf$whole)
## tail vs. whole
ks.test(wt_5dpf$tail, wt_5dpf$whole)

## head_body vs. whole
ks.test(wt_5dpf$head_body, wt_5dpf$whole)
## head_tail vs. whole
ks.test(wt_5dpf$head_tail, wt_5dpf$whole)
## body_tail vs.whole
ks.test(wt_5dpf$body_tail, wt_5dpf$whole)

wt_anovaFrame3Single = anovaSingleDataFrameGenerate(wt_5dpf)
pdensity <- ggplot(wt_anovaFrame3Single, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

wt_anovaFrame3Double = anovaDoubleDataFrameGenerate(wt_5dpf)
pdensity <- ggplot(wt_anovaFrame3Double, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

### het 5dpf

ks.test(het_5dpf$head, het_5dpf$whole)
## body vs. whole
ks.test(het_5dpf$body, het_5dpf$whole)
## tail vs. whole
ks.test(het_5dpf$tail, het_5dpf$whole)

## head_body vs. whole
ks.test(het_5dpf$head_body, het_5dpf$whole)
## head_tail vs. whole
ks.test(het_5dpf$head_tail, het_5dpf$whole)
## body_tail vs.whole
ks.test(het_5dpf$body_tail, het_5dpf$whole)

wt_anovaFrame3Single = anovaSingleDataFrameGenerate(het_5dpf)
pdensity <- ggplot(wt_anovaFrame3Single, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

wt_anovaFrame3Double = anovaDoubleDataFrameGenerate(het_5dpf)
pdensity <- ggplot(wt_anovaFrame3Double, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity


### homo

ks.test(homo_5dpf$head, homo_5dpf$whole)
## body vs. whole
ks.test(homo_5dpf$body, homo_5dpf$whole)
## tail vs. whole
ks.test(homo_5dpf$tail, homo_5dpf$whole)

## head_body vs. whole
ks.test(homo_5dpf$head_body,homo_5dpf$whole)
## head_tail vs. whole
ks.test(homo_5dpf$head_tail, homo_5dpf$whole)
## body_tail vs.whole
ks.test(homo_5dpf$body_tail, homo_5dpf$whole)

wt_anovaFrame3Single = anovaSingleDataFrameGenerate(homo_5dpf)
pdensity <- ggplot(wt_anovaFrame3Single, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity

wt_anovaFrame3Double = anovaDoubleDataFrameGenerate(homo_5dpf)
pdensity <- ggplot(wt_anovaFrame3Double, aes(x=Scores, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.8) +
  theme_ipsum() + ggtitle("Density plot of WT for 3dpf") + xlab("Scores of Increase of Confidence")
pdensity



