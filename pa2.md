Reproducible Research - Peer Assignment 2
Economic and Health Impacts of Severe Weather Events
========================================================
# Synopsis

Describe and summarize analysis in <=10 sentences.



# Tool Used (Requirements)

This assignment was completed by leveraging the following applications:

1. RStudio: Created, edited and wrote the analysis for publication to RPubs
2. knitr: Compiled the R Markdown file and converted to HTML



# Data Processing

## Set Working Directory


```r
setwd("~/Rprac/reproducible_research/peer_assessment_2")
```

## Read Data File
```
data <- read.csv("repdata-data-StormData.csv.bz2")
```

## Subset Data

Subset the data to focus on population health (evtype (8), fatalities (23), injuries (24), state (7))


```r
phdata <- data[,c(8,23,24,7)]
```

```
## Error in data[, c(8, 23, 24, 7)]: object of type 'closure' is not subsettable
```

Subset the data to focus on economic consequences (evtype (8), propdmg (25), propdmgexp (26), cropdmg (27), cropdmgexp (28), state (7))


```r
ecdata <- data[,c(8, 25:28, 7)]
```

```
## Error in data[, c(8, 25:28, 7)]: object of type 'closure' is not subsettable
```

## Determining which event types to focus on

Determine the most common types of weather by analysing all the words in the event types and finding the most common


```r
phdata.evtype <- (unique(phdata[,1]))
```

```
## Error in unique(phdata[, 1]): object 'phdata' not found
```

```r
phdata.evtype <- as.character(phdata.evtype)
```

```
## Error in eval(expr, envir, enclos): object 'phdata.evtype' not found
```

```r
phdata.evtype.split <- unlist(strsplit(phdata.evtype, c(" ")), recursive = FALSE)
```

```
## Error in strsplit(phdata.evtype, c(" ")): object 'phdata.evtype' not found
```

```r
phdata.evtype.split <- unlist(strsplit(phdata.evtype.split, c("/")), recursive = FALSE)
```

```
## Error in strsplit(phdata.evtype.split, c("/")): object 'phdata.evtype.split' not found
```

```r
df <- as.data.frame(phdata.evtype.split, stringAsFactors = default.stringsAsFactors())
```

```
## Error in as.data.frame(phdata.evtype.split, stringAsFactors = default.stringsAsFactors()): object 'phdata.evtype.split' not found
```

```r
summary(df, 10)
```

```
## Error in object[[i]]: object of type 'closure' is not subsettable
```

Based on the frequency of words, it looks like WIND/WINDS, SNOW, THUNDERSTORM, FLOOD, RAIN are the most common.

## Subset and apply factor Event Types WIND/WINDS, SNOW, THUNDERSTORM, FLOOD, RAIN

### For Population Health Data


```r
subevtype <- c("WIND", "SNOW", "THUNDERSTORM", "FLOOD", "RAIN")

phdata.subevtype <- subset(phdata, grepl(paste(subevtype, collapse = "|"), phdata$EVTYPE))
```

```
## Error in subset(phdata, grepl(paste(subevtype, collapse = "|"), phdata$EVTYPE)): object 'phdata' not found
```

```r
phdata.subevtype$SUBEVTYPE <- as.factor(gsub(".*(wind|snow|thunderstorm|flood|rain).*$", "\\1", phdata.subevtype$EVTYPE, ignore.case = TRUE))
```

```
## Error in gsub(".*(wind|snow|thunderstorm|flood|rain).*$", "\\1", phdata.subevtype$EVTYPE, : object 'phdata.subevtype' not found
```

```r
phdata.subevtype <- phdata.subevtype[,c(5,4,2,3)]
```

```
## Error in eval(expr, envir, enclos): object 'phdata.subevtype' not found
```

### For Economic Consequences Data


```r
ecdata.subevtype <- subset(ecdata, grepl(paste(subevtype, collapse = "|"), ecdata$EVTYPE))
```

```
## Error in subset(ecdata, grepl(paste(subevtype, collapse = "|"), ecdata$EVTYPE)): object 'ecdata' not found
```

```r
ecdata.subevtype$SUBEVTYPE <- as.factor(gsub(".*(wind|snow|thunderstorm|flood|rain).*$", "\\1", ecdata.subevtype$EVTYPE, ignore.case = TRUE))
```

```
## Error in gsub(".*(wind|snow|thunderstorm|flood|rain).*$", "\\1", ecdata.subevtype$EVTYPE, : object 'ecdata.subevtype' not found
```

```r
ecdata.subevtype <- ecdata.subevtype[,c(7,6,2,4)]
```

```
## Error in eval(expr, envir, enclos): object 'ecdata.subevtype' not found
```

Note: As the raw data was the same for both the economic and personal subsets, the focus will remain on the weather that occurs most frequently in the data.

## Summarizing data

### For Population Health Data

Summarize the # of injuries and # of fatalities by subset of the weather event types by state.


```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.1.1
```

```r
phdata.subevtype_melt <- melt(phdata.subevtype, id = c("SUBEVTYPE", "STATE"))
```

```
## Error in melt(phdata.subevtype, id = c("SUBEVTYPE", "STATE")): object 'phdata.subevtype' not found
```

```r
phdata.subevtype_cast_total <- dcast(phdata.subevtype_melt, SUBEVTYPE ~ variable, sum)
```

```
## Error in match(x, table, nomatch = 0L): object 'phdata.subevtype_melt' not found
```

```r
phdata.subevtype_cast_state <- dcast(phdata.subevtype_melt, SUBEVTYPE + STATE ~ variable, sum)
```

```
## Error in match(x, table, nomatch = 0L): object 'phdata.subevtype_melt' not found
```

### For Economic Consequences Data

Summarize the 


```r
ecdata.subevtype_melt <- melt(ecdata.subevtype, id = c("SUBEVTYPE", "STATE"))
```

```
## Error in melt(ecdata.subevtype, id = c("SUBEVTYPE", "STATE")): object 'ecdata.subevtype' not found
```

```r
ecdata.subevtype_cast_total <- dcast(ecdata.subevtype_melt, SUBEVTYPE ~ variable, sum)
```

```
## Error in match(x, table, nomatch = 0L): object 'ecdata.subevtype_melt' not found
```

```r
ecdata.subevtype_cast_state <- dcast(ecdata.subevtype_melt, SUBEVTYPE + STATE ~ variable, sum)
```

```
## Error in match(x, table, nomatch = 0L): object 'ecdata.subevtype_melt' not found
```

# Results

Your data analysis must address the following questions:

Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

Across the United States, which types of events have the greatest economic consequences?

Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations in your report.

**** must have at least one plot ****
**** max 3 figures, panel plots count as 1 figure ****



# Publishing Your Analysis

For this assignment you will need to publish your analysis on RPubs.com. If you do not already have an account, then you will have to create a new account. After you have completed writing your analysis in RStudio, you can publish it to RPubs by doing the following:

In RStudio, make sure your R Markdown document (.Rmd) document is loaded in the editor

Click the Knit HTML button in the doc toolbar to preview your document.

In the preview window, click the Publish button.

Once your document is published to RPubs, you should get a unique URL to that document. Make a note of this URL as you will need it to submit your assignment.

NOTE: If you are having trouble connecting with RPubs due to proxy-related or other issues, you can upload your final analysis document file as a PDF to Coursera instead.
