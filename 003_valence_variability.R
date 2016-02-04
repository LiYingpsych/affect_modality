## Bodo Winter
## Part 2: Analysis of the AbsValence of the words (adjectives and nouns) themselves
## May 30, 2015; Changed July 19, 2015
## Edited January 21, 2016 to incorporate new data and reviews

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load in libraries:

library(dplyr)
library(lme4)
library(MuMIn)
library(effsize)

## Set options:

options(stringsAsFactors = F)

## Load in norms (processed,  including AbsValence information from Warriner):

setwd('/Users/teeniematlock/Desktop/research/senses_sensory_modalities/affect_modality/new_analysis/data/')
ln <- read.csv('lynott_connell_2009_adj_norms.csv')

## How much data?

nrow(ln)		# 423

## Load in valence norms:

warr <- read.csv('warriner_2013_affective_norms.csv')
hash <- read.csv('NRC_hashtag_unigrams-pmilexicon.txt', sep = '\t', header = F)

## Change colnames of Hashtag norms:

hash <- rename(hash,
	Word = V1, Sent = V2, NumPos = V3, NumNeg = V4)

## Load in adjective context data (from COCA):

adj <- read.csv('adjective_noun_combinations.csv')

## Merge Warriner and Twitter valence into adjective-noun COCA data:

adj$Val <- warr[match(adj$Noun, warr$Word), ]$Val
adj$Sent <- hash[match(adj$Noun, hash$Word), ]$Sent

## How many data points per POS?:

nrow(adj)	# 149,387
sum(!is.na(adj$Val))	# 119,771
sum(!is.na(adj$Sent))	# 117,783

## Take averages:

xdata <- summarise(group_by(adj, Word),
	Val = sd(Val, na.rm = T),
	Sent = sd(Sent, na.rm = T))

## How many NA's?

sum(!is.na(xdata$Val))		# 2
sum(!is.na(xdata$Sent))		# 2
nrow(xdata)	# 405

## Add dominant modality from Lynott & Connell (2009):

xdata$DominantModality <- ln[match(xagr$Word, ln$Word), ]$DominantModality



##------------------------------------------------------------------
## Analysis of context valence:
##------------------------------------------------------------------

## LM analysis for plotting, raw valence:

anova(Val.mdl <- lm(Val ~ DominantModality, xdata))
anova(Sent.mdl <- lm(Sent ~ DominantModality, xdata))

## Post-hoc tests of absolute valence, chem vs. no chem senses:

chem_senses <- c('Gustatory', 'Olfactory')
t.test(xdata[xdata$DominantModality %in% chem_senses, ]$Val,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$Val, paired = F, var.equal = T)
t.test(xdata[xdata$DominantModality %in% chem_senses, ]$Sent,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$Sent, paired = F, var.equal = T)

## Calculate effect sizes for absolute valence:

cohen.d(na.omit(xdata[xdata$DominantModality %in% chem_senses, ]$Val),
	na.omit(xdata[!(xdata$DominantModality %in% chem_senses), ]$Val), paired = F)
cohen.d(na.omit(xdata[xdata$DominantModality %in% chem_senses, ]$Sent),
	na.omit(xdata[!(xdata$DominantModality %in% chem_senses), ]$Sent), paired = F)

## New dataframe for predictions:

newdata <- data.frame(DominantModality = c('Visual', 'Haptic', 'Auditory',
	'Gustatory', 'Olfactory'))

## Get predictions, raw valence:

newdata$Val <- predict(Val.mdl, newdata)
newdata$ValSE <- predict(Val.mdl, newdata, se.fit = T)[[2]]

## Get predictions, absolute valence:

newdata$Sent <- predict(Sent.mdl, newdata)
newdata$SentSE <- predict(Sent.mdl, newdata, se.fit = T)[[2]]

## Make a plot of valence and absolute valence:

attach(newdata)
quartz('', 11, 5)
par(mfrow = c(1, 2), omi = c(1.1, 1.1, 0.85, 0.25), mai = c(0, 0.25, 0, 0))
# Plot 1:
plot(1, 1, typ = 'n',
	xlim = c(0.5, 5.5), ylim = c(0.8, 1.4),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
box(lwd = 2)
axis(side = 1, at = 1:5, labels = c('Vis', 'Hap', 'Aud', 'Gus', 'Olf'),
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
mtext(text = 'Sensory Modality', side = 1, line = 3.5, cex = 2, font = 2)
axis(side = 2, at = seq(0.8, 1.4, 0.2), las = 2,
	font = 2, lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Context variability (SD)', side = 2, line = 3.5, cex = 1.65, font = 2)
mtext(text = 'Warriner et al. (2013) norms', side = 3, line = 1, cex = 1.5, font = 2)
# Inside plot:
text(x = 0.625, y = 1.37, labels = '(a)', font = 2, cex = 1.45)
arrows(x0 = 1:5,
	y0 = Val - 1.96 * ValSE, y1 = Val + 1.96 * ValSE,
	length = 0.08, angle = 90, code = 3, lwd = 2)
points(x = 1:5, y = Val, pch = 15, cex = 1.25)
# Plot 2:
plot(1, 1, typ = 'n',
	xlim = c(0.5, 5.5), ylim = c(0.8, 1.4),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
box(lwd = 2)
axis(side = 1, at = 1:5, labels = c('Vis', 'Hap', 'Aud', 'Gus', 'Olf'),
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
mtext(text = 'Sensory Modality', side = 1, line = 3.5, cex = 2, font = 2)
mtext(text = 'Mohammad (2012) norms', side = 3, line = 1, cex = 1.5, font = 2)
# Inside plot:
text(x = 0.625, y = 1.37, labels = '(b)', font = 2, cex = 1.45)
arrows(x0 = 1:5,
	y0 = AbsVal - 1.96 * AbsValSE, y1 = AbsVal + 1.96 * AbsValSE,
	length = 0.08, angle = 90, code = 3, lwd = 2)
points(x = 1:5, y = AbsVal, pch = 15, cex = 1.25)
detach(newdata)



