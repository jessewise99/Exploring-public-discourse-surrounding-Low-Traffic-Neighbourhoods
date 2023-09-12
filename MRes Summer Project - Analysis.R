
#################################################
#                 Jesse Wise                    #
#                                               #
#           MRes Thesis July 2023               #
#                                               #
#    Scraping twitter for sentiment analysis    #
# on public discourse surrounding LTNs & 15MCs  #
#################################################

# I keep getting the following error every so often...
# Error in exists(cacheKey, where = .rs.WorkingDataEnv, inherits = FALSE) : 
# invalid first argument
#
# You need to reset the R session using the keyboard short cut ctrl + shift + F10

################## LOADING PACKAGES ###################
# Be wary of downloading the right version of RTools (i.e 4.2)
#remotes::install_github("quanteda/quanteda.sentiment")

# This function comes from https://gist.github.com/stevenworthington/3178163
# It checks whether you have installed a packages, installs it if you haven't and otherwise loads it
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# CHOOSE YOUR LIBRARIES HERE
packages <- c("reshape2", "tidytext", "topicmodels", "ggplot2", "dplyr", "usethis", "devtools", "rjson", "jsonlite", "effects", "nlme",
              "lexicon", "rtweet", "SnowballC", "gtExtras", "readr", "sentimentr", "textdata", "tm", "textstem", "stringr", "dplyr", 
              "SnowballC", "wordcloud", "RColorBrewer", "ggpubr", "vader", "car", "utils", "ggpubr", "nortest", "performance", "sjPlot", "stm", "ggplot2", "gridExtra", "seededlda",
              "tidyverse", "utf8", "quanteda", "summarytools", "qdapDictionaries", "quanteda.textstats", "quanteda.textplots")
ipak(packages) #loading the libraries


################## LOADING DATA ###################
##### Tweets have already been downloaded and cleaned see Python file ####
setwd("C:/Users/jw3873/Documents/PostGrad/MRes/ME50376/Code")

#Reading in my data
myDataCSV<- read.csv('data_with_sentiment.csv', encoding = "UTF-8")

# Correctly coding variables
myDataCSV$created_at <- as.POSIXct(myDataCSV$created_at, format = "%Y-%m-%d %H:%M:%S")

myDataCSV$binary_indiv <- factor(myDataCSV$binary_indiv, 
                                 levels = c(0,1,2), 
                                 labels = c('Non-Moral', 'Binding morals', 'Individualsing morals'))
myDataCSV$binding_cat <- factor(myDataCSV$binding_cat, 
                                 levels = c(0,1), 
                                 labels = c('No binding morals', 'Binding morals'))
myDataCSV$individualising_cat <- factor(myDataCSV$individualising_cat, 
                                 levels = c(0,1), 
                                 labels = c('No individualising morals', 'Individualsing morals'))
myDataCSV$moral_present <- factor(myDataCSV$moral_present, 
                                        levels = c(0,1), 
                                        labels = c('No morals language', 'Moral language present'))
myDataCSV$VADER_cat <- factor(myDataCSV$VADER_cat, 
                                  levels = c(0,1,2), 
                                  labels = c('Negative', 'Neutral', 'Positive'))
myDataCSV$annotated_stance  <- factor(myDataCSV$annotated_stance, 
                              levels = c(0,1,2), 
                              labels = c('Against', 'Neutral', 'For'),
                              exclude = NA)
myDataCSV$annotated_binary  <- factor(myDataCSV$annotated_binary, 
                                      levels = c(0,1,2), 
                                      labels = c('Non-moral', 'Conservative', 'Liberal'),
                                      exclude = NA)


# simple regression can't handle missing values so I must replace all NaN with 0s in a new dataset
dataNoNaN<-myDataCSV
columns_to_process <- c("care", "fairness", "authority","loyalty","purity")
for (col in columns_to_process) {
  dataNoNaN[, col][is.na(dataNoNaN[, col])] <- 0
}

#Making binary morals
for (var in columns_to_process) {
  dataNoNaN[[paste0('binary_', var)]] <- ifelse(dataNoNaN[[var]] == 0, 0, 1)
  dataNoNaN[[paste0('binary_', var)]] <- factor(dataNoNaN[[paste0('binary_', var)]], 
                                    levels = c(0,1), 
                                    labels = c('No moral language', 'Moral language present'))
}

################## Checking a sub-sample ##########
sample1<-dataNoNaN[sample(nrow(dataNoNaN), size=225),]
sample1
sample1text<-dataNoNaN[sample(nrow(dataNoNaN), size=225), 'text']
sample1text
sample2<-myDataCSV[sample(nrow(myDataCSV), size=50), 'text']
sample2
sample3<-myDataCSV[sample(nrow(myDataCSV), size=50), 'text']
sample3

#Somehow this has come through "RT :". So need to remove it
myDataCSV$tweet <- gsub('RT :', '', myDataCSV$tweet)

#Check the encoding
#myDataCSV$text <- enc2utf8(myDataCSV$text)
head(myDataCSV$tweet[4]) # Tweet is the original raw tweet
head(myDataCSV$text[4]) # Text has been cleaned, with emojis, weird strings, utls, RTs and usernames removed
head(myDataCSV$stemmed_text[4]) # This has been stemmed and made all lowercase

######################### Descriptive ##############
summary(myDataCSV)

#gt_plt_summary(myDataCSV) #This is a really cool function from gtExtras
dfSummary(myDataCSV) # Another cool summary function from the summarytools package

# Summary of dataNoNaN
summary(dataNoNaN)
#gt_plt_summary(dataNoNaN) #This is a really cool function from gtExtras
dfSummary(dataNoNaN)
summary(dataNoNaN$created_at)

####### Graphics
hist(dataNoNaN$VADER_cont, xlab='Continous sentiment score(VADER)', main='Distribution of Sentiment')

#Distributions of moral
par(mfrow=c(2,3))
hist(dataNoNaN$care, xlab='Care score (MoralStrength)', main='Distribution of Care')
hist(dataNoNaN$fairness, xlab='Fairness score (MoralStrength)', main='Distribution of Fairness')
hist(dataNoNaN$loyalty, xlab='Loyalty score (MoralStrength)', main='Distribution of Loyalty')
hist(dataNoNaN$authority, xlab='Authority score (MoralStrength)', main='Distribution of Authority')
hist(dataNoNaN$purity, xlab='Purity score (MoralStrength)', main='Distribution of Purity')

dev.off() #clears the plot
par(mfrow=c(2,3))
plot(myDataCSV$fairness, myDataCSV$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Fairness score (MoralStrength)')
plot(myDataCSV$care, myDataCSV$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Care score (MoralStrength)')
plot(myDataCSV$loyalty, myDataCSV$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Loyalty score (MoralStrength)')
plot(myDataCSV$authority, myDataCSV$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Authority score (MoralStrength)')
plot(myDataCSV$purity, myDataCSV$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Purity score (MoralStrength)')
# I wonder what is causing the 8 scores in purity? 


#When NA are recoded to 0
par(mfrow=c(2,3))
plot(dataNoNaN$fairness, dataNoNaN$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Fairness score (MoralStrength)')
plot(dataNoNaN$care, dataNoNaN$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Care score (MoralStrength)')
plot(dataNoNaN$loyalty, dataNoNaN$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Loyalty score (MoralStrength)')
plot(dataNoNaN$authority, dataNoNaN$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Authority score (MoralStrength)')
plot(dataNoNaN$purity, dataNoNaN$VADER_cont, ylab='Continuous sentiment score (VADER)', xlab='Purity score (MoralStrength)')


# How does the distribution of morals look like under different sentiments?
binary_vars <- c("binary_loyalty", "binary_authority", "binary_purity", "binary_fairness", "binary_care")
data_long_bin <- pivot_longer(dataNoNaN, cols = all_of(binary_vars), names_to = "moral_trait") # Pivot data
data_long <- pivot_longer(dataNoNaN, cols = c(loyalty, authority, purity, fairness, care), names_to = "moral_trait")
trait_colors <- c(
  "binary_loyalty" = "red",
  "binary_authority" = "blue",
  "binary_purity" = "green",
  "binary_fairness" = "purple",
  "binary_care" = "orange"
)
ggplot(data_long_bin, aes(x = moral_trait, y = VADER_cont, fill = moral_trait)) +
  geom_boxplot() +
  labs(x = "Moral Trait", y = "Continuous sentiment score (VADER)") +
  facet_wrap(~ value, scales = "free_x") +
  scale_fill_manual(values = trait_colors) + 
  labs(fill = "Moral Traits") + 
  scale_fill_manual(values = trait_colors, 
                    labels = c("Authority", "Care", "Fairness", "Loyalty", "Purity")) +  # Change legend labels
    scale_x_discrete(labels = c("Authority", "Care", "Fairness", "Loyalty", "Purity")) +  # Change x-axis labels
  
  theme_minimal()


# Create the density plot using ggplot2
total_grid <- ggplot(data_long, aes(x = value, fill = moral_trait)) +
  geom_density(alpha = 0.7) +
  labs(x = "Score", y = "Density", title = "Density of Moral Trait Scores") +
  scale_fill_discrete(name = "Moral Trait")
total_grid

#How does this distribution change by positive and negative sentiment?
dataNoNanPos <- subset(dataNoNaN, VADER_cat == 'Positive')
data_long_pos <- pivot_longer(dataNoNanPos, cols = c(loyalty, authority, purity, fairness, care), names_to = "moral_trait")
dataNoNanNeg <- subset(dataNoNaN, VADER_cat == 'Negative')
data_long_neg <- pivot_longer(dataNoNanNeg, cols = c(loyalty, authority, purity, fairness, care), names_to = "moral_trait")
dataNoNanNeu <- subset(dataNoNaN, VADER_cat == 'Neutral')
data_long_neu <- pivot_longer(dataNoNanNeu, cols = c(loyalty, authority, purity, fairness, care), names_to = "moral_trait")

#By liberals and conservatives?
dataNoNanLib <- subset(dataNoNaN, binary_indiv == 'Individualsing morals')
data_long_Lib <- pivot_longer(dataNoNanLib, cols = c(loyalty, authority, purity, fairness, care), names_to = "moral_trait")
dataNoNanCon <- subset(dataNoNaN, binary_indiv == 'Binding morals')
data_long_Con <- pivot_longer(dataNoNanCon, cols = c(loyalty, authority, purity, fairness, care), names_to = "moral_trait")


dev.off()
graph_dist_pos <- ggplot(data_long_pos, aes(x = value, fill = moral_trait)) +
  geom_density(alpha = 0.7) +
  labs(x = "Valence", y = "Density", title = "Positive sentiment: Density of Moral Valence") +
  scale_fill_discrete(name = "Moral Trait")

 graph_dist_neg <- ggplot(data_long_neg, aes(x = value, fill = moral_trait)) +
  geom_density(alpha = 0.7) +
  labs(x = "Valence", y = "Density", title = "Negative sentiment: Density of Moral Valence") +
  scale_fill_discrete(name = "Moral Trait")

graph_dist_neu <- ggplot(data_long_neu, aes(x = value, fill = moral_trait)) +
  geom_density(alpha = 0.7) +
  labs(x = "Valence", y = "Density", title = "Neutral sentiment: Density of Moral Trait Valence") +
  scale_fill_discrete(name = "Moral Trait")

graph_dist_Lib <- ggplot(data_long_Lib, aes(x = value, fill = moral_trait)) +
  geom_density(alpha = 0.7) +
  labs(x = "Valence", y = "Density", title = "Liberals: Density of Moral Valence") +
  scale_fill_discrete(name = "Moral Trait")

graph_dist_Cons <- ggplot(data_long_Con, aes(x = value, fill = moral_trait)) +
  geom_density(alpha = 0.7) +
  labs(x = "Valence", y = "Density", title = "Conservatives: Density of Moral Valence") +
  scale_fill_discrete(name = "Moral Trait")

# Create density plot with superimposed lines for each level of binary_indiv
graph_dist <- ggplot(dataNoNaN, aes(x = VADER_cont, color = factor(binary_indiv))) +
  geom_density(alpha = 0.5) +
  labs(x = "Sentiment Score", y = "Density", title = "Density of semtiment by political ideology") +
  scale_color_discrete(name = "Inferred political identity") 

#Display the density plot
print(graph_dist)

graph_dist <- ggplot(dataNoNaN, aes(x = VADER_cont, color = factor(binary_indiv))) +
  stat_density(geom = "line", position = "identity", alpha = 0.5) +
  labs(x = "Sentiment Score", y = "Density", title = "Density of sentiment by political ideology") +
  scale_color_discrete(name = "Inferred political identity")

# Display the density plot
print(graph_dist)

graph_dist <- ggplot(data_long, aes(x = VADER_cont, color = moral_trait)) +
  geom_density(alpha = 0.7) +
  labs(x = "Score", y = "Density", title = "Density of Moral Trait Scores") +
  scale_color_discrete(name = "Moral Trait") +
  facet_wrap(~ binary_indiv, ncol = 3)  # Facet by 'group'

# Display the density plots
print(graph_dist)

grid.arrange(total_grid, graph_dist_neu, graph_dist_pos, graph_dist_neg, ncol = 4)
grid.arrange( graph_dist_pos, graph_dist_neg, ncol = 2)
grid.arrange( total_grid, graph_dist_Lib, graph_dist_Cons, ncol = 3)
# Assuming you have a variable named 'sentiment' and another named 'group' with three levels each in your data frame

########## Exploring annotations vs ML ########## 
test_anno<- myDataCSV[!is.na(dataNoNaN[, "annotated_stance"]), ]
print_accuracy<- function(dataset, target, truth){
  i <- 0
  for (n in 1:nrow(dataset)) {
    if (test_anno[n, target] == test_anno[n, truth]) {
      i <- i + 1
    }
  }
  message(sprintf('%f tweets were matched correctly out of %f', i, nrow(dataset)))
}


print_accuracy(test_anno, "VADER_cat", "annotated_stance")
print_accuracy(test_anno, "binary_indiv", "annotated_binary")



######################### Inferential #######################
########## Taken from pre-registration - https://osf.io/ad4g5
##### RQ1: What is the extent of opposition?
#H1: Tweet sentiment (VADER compound scores) is not uniformly distributed.
VADER_cat<-dataNoNaN$VADER_cat
VADER_cat.freq<- table(dataNoNaN$VADER_cat)
barplot(VADER_cat.freq, xlab='Categorical sentiment score(VADER)', main='Distribution of Sentiment')
hist(dataNoNaN$VADER_cont, xlab='Continous sentiment score(VADER)', main='Distribution of Sentiment')
ggdensity(dataNoNaN$VADER_cont, 
          main = "Density plot of sentiment score(VADER)",
          xlab = "Tweet sentiment")
dev.off()
png("qqplot_sentiment_cont.png", width = 2000, height = 2000, res = 400)
ggqqplot(dataNoNaN$VADER_cont, title='QQ plot of tweet sentiment (VADER)') # draws the correlation between a sample and nomral distribution
dev.off()
shapiro.test(dataNoNaN$VADER_cont) # Shapiro wilk test of normality
ad.test(dataNoNaN$VADER_cont) # Anderson-Darling test. Requires library(nortest)

#RQ2: Has political signalling contributed to discourse on LTNs and 15MCS?
# H2a: Support for LTNs will be predicted by the presence of individualising morals. Opposition for LTNs will be predicted by presence of binding morals.
#H2b: 15MCs provide a greater opportunity for political polarisation and will therefore show a stronger relationship. Beta coefficient will be greater for tweets referencing 15MCs than LTNs.

md1<-lm(VADER_cont~ purity+loyalty+authority+care+fairness, data=dataNoNaN)
summary(md1)

md1b<-lm(VADER_cont~ purity*loyalty*authority*care*fairness, data=dataNoNaN)
summary(md1b)

md1c<-lm(VADER_cont~binary_care + binary_fairness+ binary_authority + binary_loyalty + binary_purity, data=dataNoNaN)
summary(md1c)
sjPlot::tab_model(md1c,
                  pred.labels = c("Intercept", "Presence of care", "Presence of fairness", "Presence of loyalty", "Presence of authority" ,"Presence of purity"),
                  dv.labels="Tweet sentiment (VADER score)")

#Assumption check
plot(md1c, 1) 
durbinWatsonTest(md1c) 
plot(md1c, which = 2) 
vif(md1c)


md2<-lm(VADER_cont~ individualising_cat + binding_cat, data=dataNoNaN)
md2b<-lm(VADER_cont~ individualising_cat * binding_cat, data=dataNoNaN)
summary(md2b)
sjPlot::tab_model(md2b)
summary(md2)
x<-sjPlot::tab_model(md2,
                  pred.labels = c("Intercept", "Presence of individualising morals (Care and Fairness)", "Presence of binding morals (Loyalty, Authority and Purity)"),
                  dv.labels="Tweet sentiment (VADER score)"
                  )
#Assumption check
compare_performance(md2,md2b) 
vif(md2b)
plot(md2b, 1) 
durbinWatsonTest(md2b) 
plot(md2b, which = 2) 
plot(allEffects(md2), xlab= c("Effect of presence of individualising morals", "Effect of presence of binding morals"), ylab="Tweet sentiment (VADER)")


md3<-lm(VADER_cont~ binary_indiv, data=dataNoNaN)
summary(md3)
sjPlot::tab_model(md3)


######## COMPARING MODEL FIT 
compare_performance(md1, md2, md3,md2b)

#RQ3: How has political signalling contributed to the discourse on LTNs and 15MCs?
#LDA topic modelling of tweets, with MFD (2.0) as seeds to identify moral themes in the discourse

######################### Exploring the most extreme views  ##############
###### Writing the functions
# A function that will return a varying number of tweets that score higher than a certain percentile
get_percentile_data <- function(data, topic, percentile) {
  threshold_value <- quantile(data[[topic]], probs=percentile) # calculate a threshold value
  
  extreme_sample <- data[data[[topic]] > threshold_value, ] #save a subset according to this and return it
  
  return(extreme_sample)
}

get_percentile_data_desc <- function(data, topic, percentile) {
  threshold_value <- quantile(data[[topic]], probs=percentile) # calculate a threshold value
  
  extreme_sample <- data[data[[topic]] < threshold_value, ] #save a subset according to this and return it
  
  return(extreme_sample)
}

# A function that returns all a number of tweets that are highest scoring in a column of yuor choice
return_extreme_absolute_number <- function(data, topic, num) {
  
  # reorder dataset in ascending order according to the variable of interest
  temp_data <- arrange(.data = data, desc(!!sym(topic))) 
  # You have to dynamically call the column names --> !! and sym() tells R to take the variables name as a symbol rather than a string
  
  # save top x rows into a new data set
  sampled_extreme <- head(temp_data, num)
  
  return(sampled_extreme)  # return this subset of data
}

###### Exploring the data
# Most extreme sentiments
extreme_sample_pos <- get_percentile_data(data=dataNoNaN, topic="VADER_cont", percentile=0.99)
extreme_sample_neg <- get_percentile_data_desc(data=dataNoNaN, topic="VADER_cont", percentile=0.01)
print(extreme_sample_pos$tweet)
print(extreme_sample_neg$text)

# Most extreme individual morals
extreme_sample_loy <- return_extreme_absolute_number(data=dataNoNaN, topic="loyalty", num=10)
print(extreme_sample_loy$text)
extreme_sample_pur <- return_extreme_absolute_number(data=dataNoNaN, topic="purity", num=10)
print(extreme_sample_pur$text)
extreme_sample_fair <- return_extreme_absolute_number(data=dataNoNaN, topic="fairness", num=10)
print(extreme_sample_fair$text)
extreme_sample_car <- return_extreme_absolute_number(data=dataNoNaN, topic="care", num=10)
print(extreme_sample_car$text)
extreme_sample_auth <- return_extreme_absolute_number(data=dataNoNaN, topic="authority", num=10)
print(extreme_sample_auth$text)

# Most extreme liberals/conservatives
extreme_sample_lib <- return_extreme_absolute_number(data=dataNoNaN, topic="individualising_cont", num=10)
print(extreme_sample_lib$text)
extreme_sample_cons <- return_extreme_absolute_number(data=dataNoNaN, topic="binding_cont", num=10)
print(extreme_sample_cons$text)

######################### LDA Topic modelling  ##############
#RQ4: How much has political signalling contributed to the discourse on LTNs and 15MCs?
#H4: Presence of a topic identified in RQ4 will have a nonzero beta coefficient.

#Import a quanteda dictionary that will be the seeds. 
mdfd_list <- list(
  "CareVirtue" = c("safe*", "peace*", "compassion*", "empath*", "sympath*", "care", "caring", "protect*", "shield", "shelter", "amity", "secur*", "benefit*", "defen*", "guard*", "preserve"),
  "CareVice" = c("harm*", "suffer*", "war", "wars", "warl*", "warring", "fight*", "violen*", "hurt*", "kill", "kills", "killer*", "killed", "killing", "endanger*", "cruel*", "brutal*", "abuse*", "damag*", "ruin*", "ravage", "detriment*", "crush*", "attack*", "annihilate*", "destroy", "stomp", "abandon*", "spurn", "impair", "exploit", "exploits", "exploited", "exploiting", "wound*"),
  "FairnessVirtue" = c("fair", "fairly", "fairness", "fair-*", "fairmind*", "fairplay", "equal*", "justice", "justness", "justifi*", "reciproc*", "impartial*", "egalitar*", "rights", "equity", "evenness", "equivalent", "unbias*", "tolerant", "equable", "balance*", "homologous", "unprejudice*", "reasonable", "constant", "honest*"),
  "FairnessVice" = c("unfair*", "unequal*", "bias*", "unjust*", "injust*", "bigot*", "discriminat*", "disproportion*", "inequitable", "prejud*", "dishonest", "unscrupulous", "dissociate", "preference", "favoritism", "segregat*", "exclusion", "exclud*"),
  "LoyaltyVirtue" = c("together", "nation*", "homeland*", "family", "families", "familial", "group", "loyal*", "patriot*", "communal", "commune*", "communit*", "communis*", "comrad*", "cadre", "collectiv*", "joint", "unison", "unite*", "fellow*", "guild", "solidarity", "devot*", "member", "cliqu*", "cohort", "ally", "insider"),
  "LoyaltyVice" = c("foreign*", "enem*", "betray*", "treason*", "traitor*", "treacher*", "disloyal*", "individual*", "apostasy", "apostate", "deserted", "deserter*", "deserting", "deceiv*", "jilt*", "imposter", "miscreant", "spy", "sequester", "renegade", "terroris*", "immigra*"),
  "AuthorityVirtue" = c("obey*", "obedien*", "duty", "law", "lawful*", "legal*", "duti*", "honor*", "respect", "respectful*", "respected", "respects", "order*", "father*", "mother", "motherl*", "mothering", "mothers", "tradition*", "hierarch*", "authorit*", "permit", "permission", "status*", "rank*", "leader*", "class", "bourgeoisie", "caste*", "position", "complian*", "command", "supremacy", "control", "submi*", "allegian*", "serve", "abide", "defere*", "defer", "revere*", "venerat*", "comply"),
  "AuthorityVice" = c("defian*", "rebel*", "dissent*", "subver*", "disrespect*", "disobe*", "sediti*", "agitat*", "insubordinat*", "illegal*", "lawless*", "insurgent", "mutinous", "defy*", "dissident", "unfaithful", "alienate", "defector", "heretic*", "nonconformist", "oppose", "protest", "refuse", "denounce", "remonstrate", "riot*", "obstruct"),
  "PurityVirtue" = c("piety", "pious", "purity", "pure*", "clean*", "steril*", "sacred*", "chast*", "holy", "holiness", "saint*", "wholesome*", "celiba*", "abstention", "virgin", "virgins", "virginity", "virginal", "austerity", "integrity", "modesty", "abstinen*", "abstemiousness", "upright", "limpid", "unadulterated", "maiden", "virtuous", "refined", "decen*", "immaculate", "innocent", "pristine", "church*"),
  "PurityVice" = c("disgust*", "deprav*", "disease*", "unclean*", "contagio*", "indecen*", "sin", "sinful*", "sinner*", "sins", "sinned", "sinning", "slut*", "whore", "dirt*", "impiety", "impious", "profan*", "gross", "repuls*", "sick*", "promiscu*", "lewd*", "adulter*", "debauche*", "defile*", "tramp", "prostitut*", "unchaste", "intemperate", "wanton", "profligate", "filth*", "trashy", "obscen*", "lax", "taint*", "stain*", "tarnish*", "debase*", "desecrat*", "wicked*", "blemish", "exploitat*", "pervert", "wretched*")
) # This is from This is from https://moralfoundations.org/wp-content/uploads/files/downloads/moral%20foundations%20dictionary.dic

moral_named_list <- list(
  "CareSeed" = c("CareVirtue", "CareVice"),
  "FairSeed" = c("FairnessVirtue", "FairnessVice"),
  "LoyaltySeed" = c("LoyaltyVirtue", "LoyaltyVice"),
  "AuthoritySeed" = c("AuthorityVirtue", "AuthorityVice"),
  "PuritySeed" = c("PurityVirtue", "PurityVice")
)

dict <- dictionary(moral_named_list) # Make it a dictionary

####

corp <- head(dataNoNaN$text, 2252)
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)

dfmt <- dfm(toks) %>%
  dfm_remove(stopwords("en"), min_nchar = 2) %>%
  dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

#Semi-supervised LDA (ie seeded)
lda_seed <- textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 10,
                                max_iter = 500)

#### Results
size<-sizes(lda_seed) # Computes the size of topics
trait_colors <- c(
  "LoyaltySeed" = "red",
  "AuthoritySeed" = "blue",
  "PuritySeed" = "green",
  "FairSeed" = "purple",
  "CareSeed" = "orange",
  "other" = "grey"
)

barplot(size, main="Frequency of topics", xlab="Seeded topics", ylab="Frequency of tweets containing seeded topics")

terms_result<-terms(lda_seed, n=20) #Extract most likely terms
topic_list<-topics(lda_seed) #returns the most likely topics for documents based on the theta parameter.
summary(topics(lda_seed))
dataNoNaN$text[971]

######### Visualizing the topic model results
# Need to set up the dataframe for the word cloud
word_probs_df <- as.data.frame(t(lda_seed$phi))

# Set column names to be more informative
colnames(word_probs_df) <- paste("Topic", 1:ncol(word_probs_df))

# Add the words as a column
word_probs_df$Words <- rownames(word_probs_df)
rownames(word_probs_df) <- NULL  # Remove row names

# Make the word cloud
word_probs_df <- word_probs_df[complete.cases(word_probs_df), ]


# Iterate through each topic
for (topic in 1:(ncol(word_probs_df) - 1)) {  # Exclude the "Words" column
  word_freqs <- as.numeric(word_probs_df[, topic])
  
  # Handle missing values (replace with 0 or another appropriate value)
  word_freqs[is.na(word_freqs)] <- 0
  
  # Normalize to 0-1 range
  word_freqs <- word_freqs / max(word_freqs)
  
  # Scale up for wordcloud
  word_freqs <- word_freqs * 100
  
  # Create wordcloud
  wordcloud(word_probs_df$Words, word_freqs, scale = c(6, 0.5), colors = brewer.pal(8, "Dark2"), min.freq = 1, max.words =50)
  
  title(paste("Topic", topic))
}
png(paste("wordcloud_topic", topic, ".png", sep = ""))

##### Exploratory
###### one way Anova

ggplot(dataNoNaN, aes(x = binary_indiv, y = VADER_cont, fill = binary_indiv)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("#E7B800","#00AFBB", "#FC4E07")) +
  labs(
    y = "Sentiment",
    x = "Moral language present"
  )

table(dataNoNaN$binary_indiv)
md4<-aov(VADER_cont~binary_indiv, data=dataNoNaN)
summary(md4)

TukeyHSD(md4)

data<-subset(dataNoNaN, select = c("binary_care", "binary_fairness", "binary_authority","binary_loyalty","binary_purity"))
data$binary_care <- as.numeric(data$binary_care)
data$binary_fairness <- as.numeric(data$binary_fairness)
data$binary_authority <- as.numeric(data$binary_authority)
data$binary_loyalty <- as.numeric(data$binary_loyalty)
data$binary_purity <- as.numeric(data$binary_purity)
# Calculate frequency counts
counts <- colSums(data)

# Create a data frame from the counts
counts_df <- data.frame(Moral = names(counts), Count = counts)

selected_columns <- c("binary_care", "binary_fairness", "binary_authority", "binary_loyalty", "binary_purity")

# Count the number of 2s in each selected column
counts_2 <- colSums(data[selected_columns] == 2)

# Create a summary data frame
summary_df <- data.frame(Moral = names(counts_2), Count = counts_2)

# Create a boxplot of the counts
barplot(Count ~ Moral, data = summary_df, 
        xlab = "Moral Category", ylab = "Frequency",
        main = "Frquency of References to Morals", names.arg= c("Authority", "Care", "Fairness", "Loyalty", "Purity"))
#Factorial Mixed ANOVA
#How many of my tweets contain both individualising and binding morals?
count <- sum(dataNoNaN$individualising_cat == "Individualsing morals" & dataNoNaN$binding_cat == "Binding morals")
sum(dataNoNaN$individualising_cat == "Individualsing morals")
sum(dataNoNaN$binding_cat == "Binding morals")
count