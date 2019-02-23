# Kostis Dimos
# 2016

## LOAD PACKAGES ####
source('scripts/packages.R')

## Load data ####
data_original = read.table('data/voicing.Table', header = T, sep='\t')
data = data_original

# Get z-scores for cog, int and dur
 data$dur_z <- as.vector(scale(data$dur))
 data$int_z <- as.vector(scale(data$int))
 data$cog_z <- as.vector(scale(data$cog))

# Organise Data
# 1. Make a factor and Assign labels to levels: stem, morpheme, word, phoneme
# 2. Make a factor and assign labels to speaker numbers 1:9
# 3. Create a column with speaker and gender combined for graphs
data = data %>%
  mutate(boundary = factor(boundary, levels=c("1","2","3","4"), labels=c("stem","morpheme","word","phoneme"))) %>%
  mutate(speaker = factor(speaker, levels=c("1","2","3","4","5","6","7","8","9"), labels=c("1","2","3","4","5","6","7","8","9")))
# Preceding Vowel
# Set missing vowels from ? to NA
data$vowel[data$vowel=="?"]<- NA
data$vowel <- factor(data$vowel)
# Create binary variable: 1 for existing precding vowel, 0 for misisng preceding vowel
data <- data %>%
  mutate(vowel_bin = factor(ifelse(is.na(vowel),0, 1)))

## Subsetting data ####
# Filter: Experimental tokens
data.exp <- data %>%
  filter(boundary != 'phoneme') %>%
# remove level "phoneme" from factor levels
  mutate(boundary = factor(boundary))

# Filter: Control group
data.ctrl = data %>%
  filter(boundary == 'phoneme') %>%
  mutate(boundary = factor(boundary)) %>%
  mutate(sibilant =  factor(sibilant, levels = c("s","z"), labels = c("/s/", "/z/")))

# Filter: Sibilant
data.ctrl_s = data.ctrl %>%
  filter(sibilant == '/s/') %>%
  mutate(sibilant = factor(sibilant))
data.ctrl_z = data.ctrl %>%
  filter(sibilant == '/z/') %>%
  mutate(sibilant = factor(sibilant))

# Z-normalisation
z_sp_data <- data.exp
z_sp_data <- ddply(z_sp_data, .(speaker), mutate, int_sp_z = scale(int))
z_sp_data <- ddply(z_sp_data, .(speaker), mutate, cog_sp_z = scale(cog))
z_sp_data <- ddply(z_sp_data, .(speaker), mutate, dur_sp_z = scale(dur))
