## Bodo Winter
## Part 2: Analysis of the AbsValence of the words (adjectives and nouns) themselves
## May 30, 2015; Changed July 19, 2015
## Edited January 21, 2016 to incorporate new data and reviews

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load in libraries:

library(dplyr)
library(effsize)

## Set options:

options(stringsAsFactors = F)

## Load in norms (processed,  including AbsValence information from Warriner):

setwd('/Users/teeniematlock/Desktop/research/senses_sensory_modalities/affect_modality/new_analysis/data/')
ln <- read.csv('lynott_connell_2009_adj_norms.csv')
nn <- read.csv('lynott_connell_2013_noun_norms.csv')
vb <- read.csv('winter_2015_verb_norms.csv')

## How much data?

nrow(ln)		# 423
nrow(nn)		# 400
nrow(vb)		# 300

## Merge norming datasets:

xdata <- rbind(select(ln, Word:OlfactoryStrengthMean),
	select(nn, Word:OlfactoryStrengthMean),
	select(vb, Word:OlfactoryStrengthMean))

## Add POS column:

xdata$POS <- c(rep('adj', nrow(ln)),
	rep('noun', nrow(nn)),
	rep('verb', nrow(vb)))

## Load in valence norms:

warr <- read.csv('warriner_2013_affective_norms.csv')
hash <- read.csv('NRC_hashtag_unigrams-pmilexicon.txt', sep = '\t', header = F)

## Change colnames of Hashtag norms:

hash <- rename(hash,
	Word = V1, Sent = V2, NumPos = V3, NumNeg = V4)

## Create absolute valence scores:

warr <- mutate(warr,
	AbsVal = abs(Val - mean(Val, na.rm = T)))
hash <- mutate(hash,
	AbsSent = abs(Sent - mean(Sent, na.rm = T)))

## Merge Warriner valence and modality norms:

xdata$Val <- warr[match(xdata$Word, warr$Word), ]$Val
xdata$AbsVal <- warr[match(xdata$Word, warr$Word), ]$AbsVal

## Merge Twitter valence and modality norms:

xdata$Sent <- hash[match(xdata$Word, hash$Word), ]$Sent
xdata$AbsSent <- hash[match(xdata$Word, hash$Word), ]$AbsSent

## How many data points per POS?:

table(xdata[!is.na(xdata$Val), ]$POS)
table(xdata[!is.na(xdata$Sent), ]$POS)



##------------------------------------------------------------------
## Analysis:
##------------------------------------------------------------------

## Model with POS interaction, raw valence:

anova(lm(Val ~ DominantModality * POS, xdata))
anova(lm(Sent ~ DominantModality * POS, xdata))

## Model with POS interaction, absolute valence:

anova(lm(AbsVal ~ DominantModality * POS, xdata))
anova(lm(AbsSent ~ DominantModality * POS, xdata))

## LM analysis for plotting, raw valence:

anova(Val.mdl <- lm(Val ~ DominantModality, xdata))
anova(Sent.mdl <- lm(Sent ~ DominantModality, xdata))

## LM analysis for plotting, absolute valence:

anova(AbsVal.mdl <- lm(AbsVal ~ DominantModality, xdata))
anova(AbsSent.mdl <- lm(AbsSent ~ DominantModality, xdata))

## Post-hoc tests of valence for gustatory versus olfactory words:

t.test(xdata[xdata$DominantModality == 'Gustatory', ]$Val,
	xdata[xdata$DominantModality == 'Olfactory', ]$Val, paired = F, var.equal = T)
t.test(xdata[xdata$DominantModality == 'Gustatory', ]$Sent,
	xdata[xdata$DominantModality == 'Olfactory', ]$Sent, paired = F, var.equal = T)

## Effect size of valence for gustatory versus olfactory words:

cohen.d(na.omit(xdata[xdata$DominantModality == 'Gustatory', ]$Val),
	na.omit(xdata[xdata$DominantModality == 'Olfactory', ]$Val), paired = F)
cohen.d(na.omit(xdata[xdata$DominantModality == 'Gustatory', ]$Sent),
	na.omit(xdata[xdata$DominantModality == 'Olfactory', ]$Sent), paired = F)

## Post-hoc tests:

chem_senses <- c('Gustatory', 'Olfactory')
t.test(xdata[xdata$DominantModality %in% chem_senses, ]$AbsVal,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsVal, paired = F, var.equal = T)
t.test(xdata[xdata$DominantModality %in% chem_senses, ]$AbsSent,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsSent, paired = F, var.equal = T)

## Effect size of absolute valence:

cohen.d(na.omit(xdata[xdata$DominantModality %in% chem_senses, ]$AbsVal),
	na.omit(xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsVal), paired = F)
cohen.d(na.omit(xdata[xdata$DominantModality %in% chem_senses, ]$AbsSent),
	na.omit(xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsSent), paired = F)

## New dataframe for predictions:

newdata <- data.frame(DominantModality = c('Visual', 'Haptic', 'Auditory',
	'Gustatory', 'Olfactory'))

## Get predictions, raw valence:

newdata$Val <- predict(Val.mdl, newdata)
newdata$ValSE <- predict(Val.mdl, newdata, se.fit = T)[[2]]

## Get predictions, absolute valence:

newdata$AbsVal <- predict(AbsVal.mdl, newdata)
newdata$AbsValSE <- predict(AbsVal.mdl, newdata, se.fit = T)[[2]]

## Make a plot of valence and absolute valence:

attach(newdata)
quartz('', 11, 5)
par(mfrow = c(1, 2), omi = c(1.1, 1.1, 0.85, 1.25), mai = c(0, 0.25, 0, 0))
# Plot 1:
plot(1, 1, typ = 'n',
	xlim = c(0.5, 5.5), ylim = c(4, 6),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
box(lwd = 2)
axis(side = 1, at = 1:5, labels = c('Vis', 'Hap', 'Aud', 'Gus', 'Olf'),
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
mtext(text = 'Sensory Modality', side = 1, line = 3.5, cex = 2, font = 2)
axis(side = 2, at = seq(4, 6, 0.5), las = 2,
	font = 2, lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Valence rating', side = 2, line = 3.5, cex = 1.65, font = 2)
mtext(text = 'Valence', side = 3, line = 1, cex = 2, font = 2)
# Inside plot:
text(x = 0.625, y = 5.9, labels = '(a)', font = 2, cex = 1.45)
arrows(x0 = 1:5,
	y0 = Val - 1.96 * ValSE, y1 = Val + 1.96 * ValSE,
	length = 0.08, angle = 90, code = 3, lwd = 2)
points(x = 1:5, y = Val, pch = 15, cex = 1.25)
# Plot 2:
plot(1, 1, typ = 'n',
	xlim = c(0.5, 5.5), ylim = c(0.8, 1.8),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
box(lwd = 2)
axis(side = 1, at = 1:5, labels = c('Vis', 'Hap', 'Aud', 'Gus', 'Olf'),
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
mtext(text = 'Sensory Modality', side = 1, line = 3.5, cex = 2, font = 2)
mtext(text = 'Absolute Valence', side = 3, line = 1, cex = 2, font = 2)
axis(side = 4, at = seq(0.8, 1.8, 0.2), las = 2,
	font = 2, lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Absolute valence rating', side = 4, line = 3.5, cex = 1.65, font = 2)
# Inside plot:
text(x = 0.625, y = 1.75, labels = '(b)', font = 2, cex = 1.45)
arrows(x0 = 1:5,
	y0 = AbsVal - 1.96 * AbsValSE, y1 = AbsVal + 1.96 * AbsValSE,
	length = 0.08, angle = 90, code = 3, lwd = 2)
points(x = 1:5, y = AbsVal, pch = 15, cex = 1.25)
detach(newdata)



