# Kostis Dimos
# 2016

## Load Data and Packages ####
source('scripts/voicing_data.R')
# data graphs ####
data.graph = data.exp

data.exp_spkr <- mutate(data.exp, speaker = paste(speaker, gender, sep = ""))
View(data.exp_spkr)

###############
# Control Data
###############
# -> IN
ctrl_int_box <- ggplot(data.ctrl, aes(sibilant,int)) + geom_boxplot(fill = 'darkblue') + labs(y = "Intensity", x = "Sibilant") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE)
ctrl_int_box
ctrl_cog_box <- ggplot(data.ctrl, aes(sibilant,cog)) + geom_boxplot(fill = 'darkblue') + labs(y = "CoG", x = "Sibilant") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE)

ctrl_dur_box <- ggplot(data.ctrl, aes(sibilant,dur)) + geom_boxplot(fill = 'darkblue') + labs(y = "Duration", x = "Sibilant") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE)

ctrl_acoustics_grid <- grid.arrange(ctrl_dur_box,ctrl_int_box,ctrl_cog_box, ncol = 3)

voicing_sz <- ggplot(data.ctrl, aes(voic_ratio)) + geom_histogram(fill = 'darkblue') + facet_grid(sibilant~.) + labs(x = "Voicing Ratio", y = "Frequency") +
  geom_vline(data=filter(data.ctrl, sibilant=="/s/"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl, sibilant=="/s/"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=30), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl, sibilant=="/z/"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl, sibilant=="/z/"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=30), colour="red", angle=0) +
  theme(legend.position="none")
voicing_sz
head(data) 

############################
# Histograms Density Plots
############################

# dur
durexp_hist <- ggplot(data.exp, aes(dur)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Duration") + geom_density()
durs_hist <- ggplot(data.ctrl_s, aes(dur)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Duration") + geom_density()
durz_hist <- ggplot(data.ctrl_z, aes(dur)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Duration") + geom_density()
histogrid.dur <- grid.arrange(durexp_hist,durs_hist,durz_hist, nrow=3)
histogrid.dur

# cog
cogexp_hist <- ggplot(data.exp, aes(cog)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Center of Gravity") + geom_density()
cogs_hist <- ggplot(data.ctrl_s, aes(cog)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Center of Gravity") + geom_density()
cogz_hist <- ggplot(data.ctrl_z, aes(cog)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Center of Gravity") + geom_density()
histgrid.cog <- grid.arrange(cogexp_hist,cogs_hist,cogz_hist, nrow=3)

# intensity
intexp_hist <- ggplot(data.exp, aes(int)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Mean Intensity") + geom_density()
ints_hist <- ggplot(data.ctrl_s, aes(int)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Mean Intensity") + geom_density()
intz_hist <- ggplot(data.ctrl_z, aes(int)) +
  geom_histogram(aes(y = ..density..), fill="deepskyblue1") + ggtitle("Mean Intensity") + geom_density()
histgrid.int <- grid.arrange(intexp_hist,ints_hist,intz_hist, nrow=3)
histogrid <- grid.arrange(histogrid.dur, histgrid.cog, histgrid.int, ncol=3)

################
# Density Plots
###############

ratio_cons_dns <- ggplot(data.exp, aes(voic_ratio)) + facet_grid(~consonant, scales="free") + geom_histogram(fill="darkblue") +
  ggtitle("following consonant") + 
  geom_vline(data=filter(data.exp, consonant=="fricative"), aes(xintercept = mean(voic_ratio), colour="red")) + 
  geom_label(data=filter(data.exp, consonant=="fricative"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=40), colour="red", angle=0) +
  geom_vline(data=filter(data.exp, consonant=="nasal"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp, consonant=="nasal"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=40), colour="red", angle=0) +
  geom_vline(data=filter(data.exp, consonant=="plosive"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp, consonant=="plosive"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=40), colour="red", angle=0) +
  geom_vline(data=filter(data.exp, consonant=="sonorant"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp, consonant=="sonorant"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=40), colour="red", angle=0) +
  theme(legend.position="none")

ratio_bound_dns <- ggplot(data.exp, aes(voic_ratio)) + facet_grid(~boundary, scales="free") + geom_histogram(fill="darkblue") +
  ggtitle("boundary depth") + 
  geom_vline(data=filter(data.exp, boundary=="stem"), aes(xintercept = mean(voic_ratio), colour="red")) + 
  geom_label(data=filter(data.exp, boundary=="stem"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=30), colour="red", angle=0) +
  geom_vline(data=filter(data.exp, boundary=="morpheme"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp, boundary=="morpheme"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=30), colour="red", angle=0) +
  geom_vline(data=filter(data.exp, boundary=="word"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp, boundary=="word"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=30), colour="red", angle=0) +
  theme(legend.position="none")
ratio_grid <- grid.arrange(ratio_cons_dns, ratio_bound_dns, nrow=2)

ratio_cons_hist <- ggplot(data.exp, aes(voic_ratio)) + labs( x = "Voicing Ratio", y = "Density") +
  geom_vline(aes(xintercept = mean(voic_ratio)),col='red',size=1) +
  geom_density(fill="deepskyblue1") + ggtitle("following consonant") + geom_density() + facet_grid(.~consonant)
ratio_bound_hist <- ggplot(data.exp, aes(voic_ratio)) + labs( x = "Voicing Ratio", y = "Density") +
  geom_density(fill="deepskyblue1") + ggtitle("boundary depth") + geom_density() + facet_grid(.~boundary) 

################
# Density Plots
###############
# data.graph
voicing_hist <- ggplot(data.exp, aes(voic_ratio)) +
  geom_histogram(fill="deepskyblue1") + ggtitle('Voicing Ratio')

voicing_den <- ggplot(data.exp, aes(voic_ratio)) +
  geom_density(fill="deepskyblue1") + ggtitle('Voicing Ratio')

voicing_facet <- ggplot(data.exp, aes(voic_ratio)) + geom_density(fill="deepskyblue1") + facet_grid(boundary~consonant)

ratioboundarysm_den <- ggplot(data.exp, aes(x=voic_ratio)) + facet_grid(boundary~ .) +
  geom_density(fill='deepskyblue1') +
  ggtitle("Voicing Ratio ~ boundary of Boundary")

ratioconsm_den <- ggplot(na.omit(data.graph), aes(x=voic_ratio)) + facet_grid(consonant~ .) +
  geom_density(colour='blue', fill='blue3') +
  ggtitle('Voicing Ratio ~ Following Consonant')

# ratio by boundary, overlapping
boundarysm_den <- ggplot(data.graph, aes(voic_ratio, fill=boundary, colour=boundary)) + geom_density(alpha=0.4)
boundarysm_den
# ratio by consonant, overlapping
conssm_den <- ggplot(data.graph, aes(voic_ratio, fill=consonant, colour=consonant)) + geom_density(alpha=0.5)
conssm_den

################################
# Scatter plots for collinearity
################################

# z_sp_data = z-scores by speaker for cog, int, dur
## Scatter plots for collinearity
dur_int <- qplot(dur_sp_z, int_sp_z, data=z_sp_data) + ggtitle("Duration - Intensity") + geom_point(colour='darkblue')
dur_cog <- qplot(dur_sp_z, cog_sp_z, data=z_sp_data) + ggtitle("Duration - CoG") + geom_point(colour='darkblue')
int_cog <- qplot(int_sp_z, cog_sp_z, data=z_sp_data) + ggtitle("Intensity - CoG") + geom_point(colour='darkblue')
acoustics_grid <- grid.arrange(dur_cog,dur_int,int_cog, nrow=3)

## Scatter plots for voic_ratio and acoustic correlates (z-scores)
ratio_int <- qplot(voic_ratio, int_sp_z, data=z_sp_data)
ratio_cog <- qplot(voic_ratio, cog_sp_z, data=z_sp_data)
ratio_dur <- qplot(voic_ratio, dur_sp_z, data=z_sp_data)

# Scatter plots for voicing
voic_cog_point <- ggplot(data.exp, aes(voic_ratio, cog_z)) + geom_point(colour='deepskyblue1') + ggtitle("voicing ratio ~ CoG")
voic_int_point <- ggplot(data.exp, aes(voic_ratio, int_z)) + geom_point(colour='deepskyblue1') + ggtitle("voicing ratio ~ intensity")
voic_dur_point <- ggplot(data.exp, aes(voic_ratio, dur_z)) + geom_point(colour='deepskyblue1') + ggtitle("voicing ratio - duration")
voic_point_grid <- grid.arrange(voic_int_point,voic_cog_point,voic_dur_point, nrow=3)

######################
# Speaker variability
######################
speaker_voicing_dns <- ggplot(data.exp_spkr, aes(voic_ratio)) +  geom_histogram(fill="darkblue") + facet_grid(.~speaker) +
  scale_x_continuous(breaks=seq(0,1)) + labs(x = "Voicing Ratio", y = "Density") +
  geom_vline(data=filter(data.exp_spkr, speaker=="1f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="1f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="2f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="2f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="3f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="3f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="4f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="4f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="5f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="5f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="6f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="6f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="7m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="7m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="8m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="8m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
  geom_vline(data=filter(data.exp_spkr, speaker=="9m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.exp_spkr, speaker=="9m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=20), colour="red", angle=0) +
    theme(legend.position="none")
speaker_voicing_dns

# Create ctrl_z subset
data.ctrl_z <- mutate(data.ctrl_z, speaker = paste(speaker, gender, sep = ""))
# Control_z Histogram
speaker_voicing_z_dns <- ggplot(data.ctrl_z, aes(voic_ratio)) +  geom_histogram(fill="darkblue") + facet_grid(.~speaker) +
  scale_x_continuous(breaks=seq(0,0.5,1)) + ggtitle("/z/") + labs(x = "Voicing Ratio", y = "Frequency") +
  geom_vline(data=filter(data.ctrl_z, speaker=="1f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="1f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=2), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="2f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="2f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="3f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="3f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="4f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="4f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="5f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="5f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="6f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="6f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="7m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="7m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="8m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="8m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_z, speaker=="9m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_z, speaker=="9m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=4), colour="red", angle=0) +
  theme(legend.position="none")
speaker_voicing_z_dns

data.ctrl_s <- mutate(data.ctrl_s, speaker = paste(speaker, gender, sep = ""))

speaker_voicing_s_dns <- ggplot(data.ctrl_s, aes(voic_ratio)) + geom_histogram(fill="darkblue") + facet_grid(.~speaker) +
  scale_x_continuous(breaks=seq(0,0.5,1)) + ggtitle("/s/") + labs(x = "Voicing Ratio", y = "Frequency") +
  geom_vline(data=filter(data.ctrl_s, speaker=="1f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="1f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="2f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="2f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="3f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="3f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="4f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="4f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="5f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="5f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="6f"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="6f"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="7m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="7m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="8m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  geom_label(data=filter(data.ctrl_s, speaker=="8m"), aes(x=mean(voic_ratio), label=round(mean(voic_ratio), 3), y=5.0), colour="red", angle=0) +
  geom_vline(data=filter(data.ctrl_s, speaker=="9m"), aes(xintercept = mean(voic_ratio), colour="red")) +
  theme(legend.position="none")

speaker_voicing_s_dns

grid_speaker <- grid.arrange(speaker_voicing_s_dns, speaker_voicing_z_dns, nrow = 2)

###########
# data_ctrl
###########
# Histograms
# ratio
ratiophons_hist <- ggplot(data_ctrl_s, aes(voic_ratio)) +
  geom_histogram(colour="navy", fill="navy") + ggtitle("/s/ Voicing Ratio")
ratiophons_hist
ratiophonz_hist <- ggplot(data_ctrl_z, aes(voic_ratio)) +
  geom_histogram(colour="navy", fill="navy") + ggtitle("/z/ Voicing Ratio")
ratiophonz_hist
# dur
durphons_hist <- ggplot(data_ctrl_s, aes(dur)) +
  geom_histogram(colour="navy", fill="navy") + ggtitle("/s/ Duration")
durphons_hist
histgridphon <- grid.arrange(ratiophonz_hist,ratiophons_hist, ncol=2)
# cog
cogphons_hist <- ggplot(data_ctrl_s, aes(cog)) +
  geom_histogram(colour="navy", fill="navy") + ggtitle("Center of Gravity")
cogphons_hist
# intensity
intphons_hist <- ggplot(data_ctrl_s, aes(int)) +
  geom_histogram(colour="navy", fill="navy") + ggtitle("Mean Intensity")
intphons_hist

#######################
# More data.exp graphs
######################
# Density Plots
ratiophon_den <- ggplot(data.ctrl, aes(x=voic_ratio)) + facet_grid(sibilant~ .) + geom_density(colour='blue', fill='blue3')
# voicing ratio by boundary, overlapping
sratio_den <- ggplot(data.ctrl, aes(voic_ratio, fill=sibilant, colour=sibilant)) + geom_density(alpha=0.4)

# Jitter plots
# speaker vs voicing ratio
spkr_jtr <- ggplot(data =data.graph, aes(x=speaker, y=voic_ratio)) +
  geom_jitter(data= data.graph, aes(colour=speaker, size=2)) + 
  ggtitle("Voicing Ratio by Speaker") +
  guides(size=FALSE) + coord_flip() + facet_grid(speaker ~ .)

# boundary vs voicing ratio
boundary_jtr <- ggplot(data=data.graph, aes(x=boundary, y=voic_ratio)) +
  geom_jitter(aes(colour=boundary, size=2)) +
  ggtitle("Voicing Ratio by Boundary Depth") +
  guides(size=FALSE) + coord_flip() + facet_grid(boundary ~ .)

# consonant vs voicing_ratio
con_jtr <- ggplot(data=data.graph, aes(x=consonant, y=voic_ratio)) +
  geom_jitter(data=data.graph, aes(colour=consonant, size=2)) +
  ggtitle("Voicing Ratio by Following Consonant") +
  guides(size=FALSE) + coord_flip() + facet_grid(consonant ~ .)
