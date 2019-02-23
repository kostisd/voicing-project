# Kostis Dimos
# 2016

## Load Data and Packages ####
source('./scripts/voicing_data.R')

# Remove NA cases from the consonant factor
data.stats = data.exp

voic_descr <- describeBy(data.ctrl[,9], data.ctrl$sibilant, mat = T)
xtable(as.data.frame(voic_descr)[,-c(1,3,8,9,15)])

voic_descr_acoustics <- describeBy(data.ctrl[,c(10,11,12)], data.ctrl$sibilant, mat = T)
xtable(as.data.frame(voic_descr_acoustics[,-c(1,3,8,9,15)]))

# Repeated measures ANOVA after van Heuven pointed out the wrong number of DF.
data.aov <- ddply(data.ctrl, .(speaker, sibilant), summarize,  int_sp=mean(int), dur_sp=mean(dur), int_sp=mean(int), cog_sp=mean(cog))
int_anova <- aov(int_sp ~ sibilant + Error(speaker), data=data.aov)
cog_anova <- aov(dur_sp ~ sibilant + Error(speaker), data=data.aov)
dur_anova <- aov(dur_sp ~ sibilant + Error(speaker), data=data.aov)
xtable(dur_anova)
xtable(int_anova)
xtable(cog_anova)

#########################################
# Acoustic correlates - linear regression
#########################################

cog_int_lm <- lm(data = data.stats, cog_z~int_z)
cor.test(data.stats$cog_z, data.stats$int_z, method = "pearson")

cog_dur_lm <- lm(data = data.stats, cog_z~dur_z)
cor.test(data.stats$cog_z, data.stats$dur_z, method = "pearson")

int_dur_lm <- lm(data = data.stats, int_z~dur_z)
cor.test(data.stats$int_z, data.stats$dur_z, method = "pearson")

acoustics_lm_table <- stargazer(cog_int_lm, cog_dur_lm, int_dur_lm)

############################################
# Multiple regression: voicing ~ dur,int,cog
############################################

voicing_multi_lm = lm(voic_ratio ~ cog_z + int_z + dur_z, data=data.stats)
voicing_multi_table <- stargazer(voicing_multi_lm)

# Linear regressions
voicing_cog_lm <- lm(voic_ratio ~ cog_z, data=data.stats)
cor.test(data.stats$voic_ratio, data.stats$cog_z, method = "pearson")
voicing_int_lm <- lm(voic_ratio ~ int_z, data=data.stats)
cor.test(data.stats$voic_ratio, data.stats$int_z, method = "pearson")
voicing_dur_lm <- lm(voic_ratio ~ dur_z, data=data.stats)
cor.test(data.stats$voic_ratio, data.stats$dur_z, method = "pearson")
voicing_lm_table <- stargazer(voicing_cog_lm, voicing_int_lm, voicing_dur_lm)

##############################################
# Beta regression mixed effects model_beta ###
##############################################
# Fixed effects: consonant, boundary(stem, morpheme,word)
# Random effects: speaker, precending vowel (1=present, 0= missing)
data.stats$voic_ratio <- (data.stats$voic_ratio * (nrow(data.stats)-1) + 0.5)/nrow(data.stats)
# Check for interaction: consonant~level
data.inter <- filter(data.stats, boundary != 'word')
inter.null_beta <- glmmadmb(voic_ratio~consonant+boundary + (1|speaker) + (1|vowel_bin), family="beta", data=data.inter)
inter.model_beta = glmmadmb(voic_ratio~consonant*boundary + (1|speaker) + (1|vowel_bin), family="beta", data=data.inter)
anova.interaction_beta = anova(inter.null_beta,inter.model_beta)
aov(inter.null_beta,inter.model_beta)
interaction_table <- xtable(anova.interaction_beta)
# No interaction found

# Beta regression on boundary
boundary.model_beta = glmmadmb(voic_ratio~consonant + boundary + (1|speaker) + (1|vowel_bin), family="beta", data=data.stats)
boundary.null_beta = glmmadmb(voic_ratio~consonant + (1|speaker) + (1|vowel_bin), family="beta", data=data.stats)
anova.boundary_beta = anova(boundary.null_beta, boundary.model_beta)

# Beta regression on consonant
consonant.model_beta = glmmadmb(voic_ratio~ consonant + boundary+ (1|speaker) + (1|vowel_bin), family="beta", data=data.stats)
consonant.null_beta = glmmadmb(voic_ratio~ boundary + (1|speaker) + (1|vowel_bin), family="beta", data=data.stats)
anova.consonant_beta = anova(consonant.null_beta, consonant.model_beta)
xtable(anova.boundary_beta)
xtable(anova.consonant_beta)

# Post-hoc Analysis
library(lsmeans)
post_hoc_cons <- emmeans(consonant.model_beta, list(pairwise ~ "consonant"), adjust = "tukey")
post_hoc_bound <- emmeans(boundary.model_beta, list(pairwise ~ "boundary"), adjust = "tukey")

library(multcomp)
summary(glht(consonant.model_beta, linfct = mcp(consonant = "Tukey"), test = adjusted("bonferroni")))
summary(glht(boundary.model_beta, linfct = mcp(boundary = "Tukey"), test = adjusted("bonferroni")))

emmeans(consonant.model_beta, list(pairwise ~ consonant), adjust = "bonferroni")
emmeans(boundary.model_beta, list(pairwise ~ boundary), adjust = "bonferroni")

##########################
# Acoustic Correlates ####
##########################

# Ratio ~ Acoustic correlates
# Ratio ~ cog
# na.action = na.exclude causes "unused argument" error, removed
dur.model_beta <- glmmadmb(voic_ratio~dur_z + (1|speaker) + (1|vowel_bin), data=data.stats, family="beta")
dur.null_beta <- glmmadmb(voic_ratio~ (1|speaker) + (1|vowel_bin), data=data.stats, family="beta")
anova_dur <- anova(dur.null_beta, dur.model_beta)

int.model_beta = glmmadmb(voic_ratio~int_z + (1|speaker) + (1|vowel_bin), family="beta", data=data.stats)
int.null_beta = glmmadmb(voic_ratio~ (1|speaker) + (1|vowel_bin), family="beta", data=data.stats)
anova(int.null_beta,int.model_beta)

cog.model_beta = glmmadmb(voic_ratio~cog_z + (1|speaker) + (1|vowel_bin),  family="beta", data=data.stats)
cog.null_beta = glmmadmb(voic_ratio ~ (1|speaker) + (1|vowel_bin),  family="beta", data=data.stats)
anova(cog.null_beta,cog.model_beta)
