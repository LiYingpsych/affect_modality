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
vb <- read.csv('winter_2015_verb_norms.csv')

## How much data?

nrow(ln)		# 423
nrow(vb)		# 300

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

## Load in adjective context data (from COCA):

adj <- read.csv('adjective_noun_combinations.csv')

## Merge Warriner valence into adjective-noun COCA data:

adj$Val <- warr[match(adj$Noun, warr$Word), ]$Val
adj$AbsVal <- warr[match(adj$Noun, warr$Word), ]$AbsVal

## Merge Twitter valence and modality norms:

adj$Sent <- hash[match(adj$Noun, hash$Word), ]$Sent
adj$AbsSent <- hash[match(adj$Noun, hash$Word), ]$AbsSent

## How many data points per POS?:

nrow(adj)	# 149,387
sum(!is.na(adj$Val))	# 135,322
sum(!is.na(adj$Sent))	# 141,482

## To compute frequency-weighted context valence, first create weights:

adj_freqs <- aggregate(Freq ~ Word, adj, sum)
adj$AdjFreq <- adj_freqs[match(adj$Word, adj_freqs$Word), ]$Freq
adj <- mutate(adj, Weight = Freq / AdjFreq)

## Take averages:

xdata <- summarise(group_by(adj, Word),
	Val = mean(Val, na.rm = T, w = Weight),
	AbsVal = mean(AbsVal, na.rm = T, w = Weight),
	Sent = mean(Sent, na.rm = T, w = Weight),
	AbsSent = mean(AbsSent, na.rm = T, w = Weight))

## How many NA's?

!is.na(xdata$Val)		# none
!is.na(xdata$Sent)	# none
nrow(xdata)	# 405

## Add dominant modality from Lynott & Connell (2009):

xdata$DominantModality <- ln[match(xdata$Word, ln$Word), ]$DominantModality



##------------------------------------------------------------------
## Analysis of context valence:
##------------------------------------------------------------------

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

## Calculate effect sizes for raw context valence:

cohen.d(xdata[xdata$DominantModality == 'Gustatory', ]$Val,
	xdata[xdata$DominantModality == 'Olfactory', ]$Val, paired = F)
cohen.d(xdata[xdata$DominantModality == 'Gustatory', ]$Sent,
	xdata[xdata$DominantModality == 'Olfactory', ]$Sent, paired = F)

## Post-hoc tests of absolute valence, chem vs. no chem senses:

chem_senses <- c('Gustatory', 'Olfactory')
t.test(xdata[xdata$DominantModality %in% chem_senses, ]$AbsVal,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsVal, paired = F, var.equal = T)
t.test(xdata[xdata$DominantModality %in% chem_senses, ]$AbsSent,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsSent, paired = F, var.equal = T)

## Calculate effect sizes for absolute valence:

cohen.d(xdata[xdata$DominantModality %in% chem_senses, ]$AbsVal,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsVal, paired = F)
cohen.d(xdata[xdata$DominantModality %in% chem_senses, ]$AbsSent,
	xdata[!(xdata$DominantModality %in% chem_senses), ]$AbsSent, paired = F)

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
	xlim = c(0.5, 5.5), ylim = c(5, 6),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
box(lwd = 2)
axis(side = 1, at = 1:5, labels = c('Vis', 'Hap', 'Aud', 'Gus', 'Olf'),
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
mtext(text = 'Sensory Modality', side = 1, line = 3.5, cex = 2, font = 2)
axis(side = 2, at = seq(5, 6, 0.25), las = 2,
	font = 2, lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Average context valence', side = 2, line = 3.5, cex = 1.65, font = 2)
mtext(text = 'Context Valence', side = 3, line = 1, cex = 2, font = 2)
# Inside plot:
text(x = 0.625, y = 5.95, labels = '(a)', font = 2, cex = 1.45)
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
mtext(text = 'Context Absolute Valence', side = 3, line = 1, cex = 2, font = 2)
axis(side = 4, at = seq(0.8, 1.4, 0.2), las = 2,
	font = 2, lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Average context valence', side = 4, line = 3.5, cex = 1.65, font = 2)
# Inside plot:
text(x = 0.625, y = 1.37, labels = '(b)', font = 2, cex = 1.45)
arrows(x0 = 1:5,
	y0 = AbsVal - 1.96 * AbsValSE, y1 = AbsVal + 1.96 * AbsValSE,
	length = 0.08, angle = 90, code = 3, lwd = 2)
points(x = 1:5, y = AbsVal, pch = 15, cex = 1.25)
detach(newdata)



##------------------------------------------------------------------
## Analysis of valence by frequency:
##------------------------------------------------------------------

## Add dominant modality to adjective dataframe:

adj$DominantModality <- ln[match(adj$Word, ln$Word), ]$DominantModality

## Log-transform frequency and center predictor:

adj <- mutate(adj,
	LogFreq = log10(Freq),
	AbsVal_c = AbsVal - mean(AbsVal, na.rm = T),
	AbsSent_c = AbsSent - mean(AbsSent, na.rm = T),
	AbsVal_z = AbsVal_c / sd(AbsVal, na.rm = T),
	AbsSent_z = AbsSent_c / sd(AbsSent, na.rm = T))

## Take subset of those that have complete cases:

adj_warrcomplete <- filter(adj, !is.na(AbsVal_c))
adj_hashcomplete <- filter(adj, !is.na(AbsSent_c))

## Lmer-analysis with absolute valence / context frequency interaction, Warriner:

summary(xmdl <- lmer(LogFreq ~ AbsVal_z * DominantModality +
	(1 + AbsVal_z|Word) + (1|Noun), adj_warrcomplete, REML = F))
summary(xmdl.noint <- lmer(LogFreq ~ AbsVal_z + DominantModality +
	(1 + AbsVal_z|Word) + (1|Noun), adj_warrcomplete, REML = F))
summary(xmdl.noabsv <- lmer(LogFreq ~ 1 + DominantModality +
	(1 + AbsVal_z|Word) + (1|Noun), adj_warrcomplete, REML = F))
summary(xmdl.nodom <- lmer(LogFreq ~ AbsVal_z + 1 +
	(1 + AbsVal_z|Word) + (1|Noun), adj_warrcomplete, REML = F))

## Likelihood ratio tests of these comparisons:

anova(xmdl.noint, xmdl, test = 'Chisq')
anova(xmdl.nofreq, xmdl.noint, test = 'Chisq')
anova(xmdl.noabsv, xmdl.noint, test = 'Chisq')
anova(xmdl.noint, xmdl, test = 'Chisq')

## Lmer-analysis with absolute valence / context frequency interaction, Twitter:

summary(twit.mdl <- lmer(LogFreq ~ AbsSent_z * DominantModality +
	(1 + AbsSent_z|Word) + (1|Noun), adj_hashcomplete, REML = F))
summary(twit.mdl.noint <- lmer(LogFreq ~ AbsSent_z + DominantModality +
	(1 + AbsSent_z|Word) + (1|Noun), adj_hashcomplete, REML = F))
summary(twit.mdl.noabsv <- lmer(LogFreq ~ 1 + DominantModality +
	(1 + AbsSent_z|Word) + (1|Noun), adj_hashcomplete, REML = F))
summary(twit.mdl.nodom <- lmer(LogFreq ~ AbsSent_z + 1 +
	(1 + AbsSent_z|Word) + (1|Noun), adj_hashcomplete, REML = F))

## Likelihood ratio tests of these comparisons:

anova(twit.mdl.noint, twit.mdl, test = 'Chisq')
anova(twit.mdl.nofreq, twit.mdl.noint, test = 'Chisq')
anova(twit.mdl.noabsv, twit.mdl.noint, test = 'Chisq')
anova(twit.mdl.noint, twit.mdl, test = 'Chisq')

## Get R-squared:

r.squaredGLMM(xmdl)
r.squaredGLMM(twit.mdl)

## Get predictions for Warriner model:

AbsVal_z <- seq(-1, 5, 0.01)
newdata <- data.frame(AbsVal_z = rep(AbsVal_z, 5),
	DominantModality = as.factor(rep(unique(xdata$DominantModality), each = length(AbsVal_z))))
newdata$LogFreq <- predict(xmdl, newdata, re.form = NA)

## Get predictions for Twitter model:

AbsSent_z <- seq(-1, 5, 0.01)
newdata.twit <- data.frame(AbsSent_z = rep(AbsSent_z, 5),
	DominantModality = as.factor(rep(unique(xdata$DominantModality), each = length(AbsSent_z))))
newdata.twit$LogFreq <- predict(twit.mdl, newdata.twit, re.form = NA)

## Make a plot of valence and absolute valence:

quartz('', 11, 5)
par(mfrow = c(1, 2), omi = c(1.1, 1.1, 0.85, 0.25), mai = c(0, 0.25, 0, 0))
# Plot 1:
plot(1, 1, typ = 'n',
	xlim = c(-1, 6), ylim = c(0, 0.21),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
box(lwd = 2)
axis(side = 1, at = seq(-1, 5, 1), 
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
mtext(text = 'Absolute Valence', side = 1, line = 3.5, cex = 2, font = 2)
axis(side = 2, at = seq(0, 0.2, 0.04), las = 2,
	font = 2, lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Word Frequency (log10)', side = 2, line = 3.5, cex = 1.65, font = 2)
mtext(text = 'Warriner et al. (2013) norms', side = 3, line = 1, cex = 1.5, font = 2)
# Inside plot:
text(x = -0.8, y = 0.1995, labels = '(a)', font = 2, cex = 1.45)
for (i in 1:5) {
	this_modality <- c('Auditory', 'Visual', 'Haptic', 'Gustatory', 'Olfactory')[i]
	if (this_modality %in% c('Gustatory', 'Olfactory')) {
		line_type = 1
		line_width = 3
		this_cex = 1.25
		this_typeface = 2
		this_color = 'black'
		} else { line_type = 3
			line_width = 1
			this_cex = 0.9
			this_typeface = 1
			this_color = rgb(0.5, 0.5, 0.5, 1)
			}
	xtemp <- newdata[newdata$DominantModality == this_modality, ]
	points(xtemp$AbsVal_z, xtemp$LogFreq, type = 'l', lwd = 2,
		lty = line_type, col = this_color)
	this_label <- substr(xtemp[1, ]$DominantModality, 1, 3)
	text(x = 5.5, y = xtemp[nrow(xtemp), ]$LogFreq,
		labels = this_label,
		font = this_typeface, cex = this_cex, col = this_color)
	}
# Plot 2:
plot(1, 1, typ = 'n',
	xlim = c(-1, 6), ylim = c(0, 0.21),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
box(lwd = 2)
axis(side = 1, at = seq(-1, 5, 1), 
	font = 2, cex.axis = 1.5, lwd.ticks = 2)
mtext(text = 'Absolute Valence', side = 1, line = 3.5, cex = 2, font = 2)
mtext(text = 'Mohammad (2012) norms', side = 3, line = 1, cex = 1.5, font = 2)
# Inside plot:
text(x = -0.8, y = 0.1995, labels = '(b)', font = 2, cex = 1.45)
for (i in 1:5) {
	this_modality <- c('Auditory', 'Visual', 'Haptic', 'Gustatory', 'Olfactory')[i]
	if (this_modality %in% c('Gustatory', 'Olfactory')) {
		line_type = 1
		line_width = 3
		this_cex = 1.25
		this_typeface = 2
		this_color = 'black'
		if (this_modality == 'Gustatory') yfac = -0.006
		if (this_modality == 'Olfactory') yfac = 0.006
		} else { line_type = 3
			line_width = 1
			this_cex = 0.9
			this_typeface = 1
			this_color = rgb(0.5, 0.5, 0.5, 1)
			yfac = 0
			}
	xtemp <- newdata.twit[newdata.twit$DominantModality == this_modality, ]
	points(xtemp$AbsSent_z, xtemp$LogFreq, type = 'l', lwd = 2,
		lty = line_type, col = this_color)
	this_label <- substr(xtemp[1, ]$DominantModality, 1, 3)
	text(x = 5.5, y = xtemp[nrow(xtemp), ]$LogFreq + yfac,
		labels = this_label,
		font = this_typeface, cex = this_cex, col = this_color)
	}



