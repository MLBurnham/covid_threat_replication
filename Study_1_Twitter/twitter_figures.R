# R version 4.1.2
library(tidyr) # version 1.1.4
library(dplyr) # version 1.0.7
library(ggplot2) # version 3.3.5
library(ggrepel) # version 0.9.1
library(quanteda) # version 3.1.0

########################
# Import and Clean Data
########################
setwd("C:/Users/mikeb/OneDrive - The Pennsylvania State University/covid_threat/replication")
tweets <- read.csv("data/classified_tweets.csv")
users <- read.csv('data/users.csv')
# create the 10 digit user id for joining tweets to users
tweets$id10 <- as.numeric(substr(tweets$user, 1, 10))

# keep only users with robust estimates of ideology
users <- users[complete.cases(users),]
users <- users[users['tweets'] >= 1 & users['Rhat'] <= 1.1,]

# keep only tweets from these users
tweets <- tweets[tweets$id10 %in% users$id10,]

# define label names for easier plotting later
tweets$non_comp <- ifelse(tweets$non_comp == 1, "Non-Compliant", "Compliant")

# join tweets and users
tweets <- left_join(tweets, users[,c('id10', 'ideology')], by = 'id10')

# define ideological poles as the local max of the modes
tweets$pole <- ifelse(tweets$ideology >= 0.966, 
                      'Conservative', 
                      ifelse(tweets$ideology <= -1.982, 
                             'Liberal', 
                             'Centrist'))

poles <- tweets[tweets$pole == 'Conservative' | tweets$pole == 'Liberal',]

###########################
## Fightin Words Functions
###########################
fwgroups <- function(dtm, groups, pair = NULL, weights = rep(1,nrow(dtm)), k.prior = .1) {
  
  weights[is.na(weights)] <- 0
  
  weights <- weights/mean(weights)
  
  zero.doc <- rowSums(dtm)==0 | weights==0
  zero.term <- colSums(dtm[!zero.doc,])==0
  
  dtm.nz <- apply(dtm[!zero.doc,!zero.term],2,"*", weights[!zero.doc])
  
  g.prior <- tcrossprod(rowSums(dtm.nz),colSums(dtm.nz))/sum(dtm.nz)
  
  # 
  
  g.posterior <- as.matrix(dtm.nz + k.prior*g.prior)
  
  groups <- groups[!zero.doc]
  groups <- droplevels(groups)
  
  g.adtm <- as.matrix(aggregate(x=g.posterior,by=list(groups=groups),FUN=sum)[,-1])
  rownames(g.adtm) <- levels(groups)
  
  g.ladtm <- log(g.adtm)
  
  g.delta <- t(scale( t(scale(g.ladtm, center=T, scale=F)), center=T, scale=F))
  
  g.adtm_w <- -sweep(g.adtm,1,rowSums(g.adtm)) # terms not w spoken by k
  g.adtm_k <- -sweep(g.adtm,2,colSums(g.adtm)) # w spoken by groups other than k
  g.adtm_kw <- sum(g.adtm) - g.adtm_w - g.adtm_k - g.adtm # total terms not w or k 
  
  g.se <- sqrt(1/g.adtm + 1/g.adtm_w + 1/g.adtm_k + 1/g.adtm_kw)
  
  g.zeta <- g.delta/g.se
  
  g.counts <- as.matrix(aggregate(x=dtm.nz, by = list(groups=groups), FUN=sum)[,-1])
  
  if (!is.null(pair)) {
    pr.delta <- t(scale( t(scale(g.ladtm[pair,], center = T, scale =F)), center=T, scale=F))
    pr.adtm_w <- -sweep(g.adtm[pair,],1,rowSums(g.adtm[pair,]))
    pr.adtm_k <- -sweep(g.adtm[pair,],2,colSums(g.adtm[pair,])) # w spoken by groups other than k
    pr.adtm_kw <- sum(g.adtm[pair,]) - pr.adtm_w - pr.adtm_k - g.adtm[pair,] # total terms not w or k
    pr.se <- sqrt(1/g.adtm[pair,] + 1/pr.adtm_w + 1/pr.adtm_k + 1/pr.adtm_kw)
    pr.zeta <- pr.delta/pr.se
    
    return(list(zeta=pr.zeta[1,], delta=pr.delta[1,],se=pr.se[1,], counts = colSums(dtm.nz), acounts = colSums(g.adtm)))
  } else {
    return(list(zeta=g.zeta,delta=g.delta,se=g.se,counts=g.counts,acounts=g.adtm))
  }
}
############## FIGHTIN' WORDS PLOTTING FUNCTION
# helper function
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
fw.ggplot.groups <- function(fw.ch, groups.use = as.factor(rownames(fw.ch$zeta)), max.words = 50, max.countrank = 400, colorpalette=rep("black",length(groups.use)), sizescale=2, title="Comparison of Terms by Groups", subtitle = "", caption = "Group-specific terms are ordered by Fightin' Words statistic (Monroe, et al. 2008)") {
  if (is.null(dim(fw.ch$zeta))) {## two-group fw object consists of vectors, not matrices
    zetarankmat <- cbind(rank(-fw.ch$zeta),rank(fw.ch$zeta))
    colnames(zetarankmat) <- groups.use
    countrank <- rank(-(fw.ch$counts))
  } else {
    zetarankmat <- apply(-fw.ch$zeta[groups.use,],1,rank)
    countrank <- rank(-colSums(fw.ch$counts))
  }
  wideplotmat <- as_tibble(cbind(zetarankmat,countrank=countrank))
  wideplotmat$term=names(countrank)
  #rankplot <- gather(wideplotmat, party, zetarank, 1:ncol(zetarankmat))
  rankplot <- gather(wideplotmat, groups.use, zetarank, 1:ncol(zetarankmat))
  rankplot$plotsize <- sizescale*(50/(rankplot$zetarank))^(1/4)
  rankplot <- rankplot[rankplot$zetarank < max.words + 1 & rankplot$countrank<max.countrank+1,]
  rankplot$groups.use <- factor(rankplot$groups.use,levels=groups.use)
  
  p <- ggplot(rankplot, aes((nrow(rankplot)-countrank)^1, -(zetarank^1), colour=groups.use)) + 
    geom_point(show.legend=F,size=sizescale/2) + 
    theme_classic() +
    theme(axis.ticks=element_blank(), axis.text=element_blank() ) +
    ylim(-max.words,40) +
    facet_grid(groups.use ~ .) +
    geom_text_repel(aes(label = term), size = rankplot$plotsize, point.padding=.05,
                    box.padding = unit(0.20, "lines"), show.legend=F) +
    scale_colour_manual(values = alpha(colorpalette, .7)) + 
    #    labs(x="Terms used more frequently overall ???", y="Terms used more frequently by group ???",  title=title, subtitle=subtitle , caption = caption) 
    labs(x=paste("Terms used more frequently overall -->"), y=paste("Terms used more frequently by label -->"),  title=title, subtitle=subtitle , caption = element_blank()) 
  
}
fw.keys <- function(fw.ch,n.keys=10) {
  n.groups <- nrow(fw.ch$zeta)
  keys <- matrix("",n.keys,n.groups)
  colnames(keys) <- rownames(fw.ch$zeta)
  
  for (g in 1:n.groups) {
    keys[,g] <- names(sort(fw.ch$zeta[g,],dec=T)[1:n.keys])
  }
  keys
}

##########################
## FW By Compliance Label
##########################
# create a quanteda corpus
txt <- corpus(tweets$text)
# drop punctuation, symbols, etc.
txt.tokens <- tokens(txt, 
                      remove_punct = T, 
                      remove_symbols = T, 
                      remove_numbers = T,
                      remove_url = T)

txt.tokens <- tokens_keep(txt.tokens, pattern = "^[a-zA-Z0-9\\W]+$", valuetype = c('regex'))
# keep only tokens longer than 1 character
txt.tokens <- tokens_select(txt.tokens, min_nchar = 2)
# create a document frequency matrix
txt.dfm <- dfm(txt.tokens, stem = T)
# trim matrix to manageable size. This won't affect the plot because it is 
# plotting frequency and we are dropping the least frequent words
txt.dfm <- dfm_trim(txt.dfm, min_docfreq = 10000, min_termfreq = 20000)
# convert to data frame for fightnin words function
txt.dfm <- convert(txt.dfm, to = 'data.frame')
# drop the document number introduced by quanteda
txt.dfm <- txt.dfm[-1] 

# FW by compliance label
# Calculate fightin words statistic
fw.txt <- fwgroups(txt.dfm, groups = as.factor(tweets$non_comp))
# Top keys for each category
fw.keys(fw.txt, n.keys = 20)
# plot
p <- fw.ggplot.groups(fw.txt, 
                      sizescale = 3, 
                      max.words = 50, 
                      max.countrank = 400, 
                      colorpalette = c('blue3', 'red3'),
                      title = element_blank())
p
# Save
ggsave('../images/fightinwords.png', plot = p)

#########################
# FW By Ideological Pole
#########################
# create a quanteda corpus
txt <- corpus(poles$text)
# drop punctuation, symbols, etc.
txt.tokens <- tokens(txt, 
                     remove_punct = T, 
                     remove_symbols = T, 
                     remove_numbers = T,
                     remove_url = T)

txt.tokens <- tokens_keep(txt.tokens, pattern = "^[a-zA-Z0-9\\W]+$", valuetype = c('regex'))
# keep only tokens longer than 1 character
txt.tokens <- tokens_select(txt.tokens, min_nchar = 2)
# create a document frequency matrix
txt.dfm <- dfm(txt.tokens, stem = T)
# trim matrix to manageable size. This won't affect the plot because it is 
# plotting frequency and we are dropping the least frequent words
txt.dfm <- dfm_trim(txt.dfm, min_docfreq = 5000, min_termfreq = 10000)
# convert to data frame for fightnin words function
txt.dfm <- convert(txt.dfm, to = 'data.frame')
# drop the document number introduced by quanteda
txt.dfm <- txt.dfm[-1] 

# FW by compliance label
# Calculate fightin words statistic
fw.txt <- fwgroups(txt.dfm, groups = as.factor(poles$pole))
# Top keys for each category
fw.keys(fw.txt, n.keys = 20)
# plot
p2 <- fw.ggplot.groups(fw.txt, 
                      sizescale = 3, 
                      max.words = 50, 
                      max.countrank = 400, 
                      colorpalette = c('red3', 'blue3'),
                      title = "Comparison of Terms by Ideological Pole")
p2
ggsave('images/fightinwords2.png', plot = p2)

#########################
## Ideology Density Plot
#########################
# density of ideology
d <- density(users$ideology)
# find the local minimum between the peaks
lmin <- optimize(approxfun(d$x, d$y), interval = c(-2, 1))$minimum
# calculate the max at the peaks
libmax <- optimize(approxfun(d$x, d$y), interval = c(-3, -1), maximum = T)$maximum
conmax <- optimize(approxfun(d$x, d$y), interval = c(0, 2), maximum = T)$maximum

# plot the distribution with a reference line at the local minimum
tsize = 3.5 # text size for labels
dplot <- ggplot(users, aes(x = ideology)) +
  geom_density(alpha = .2, fill = 'gray') +
  geom_vline(aes(xintercept = lmin), linetype = 'dashed') +
  geom_vline(aes(xintercept = libmax), linetype = 'dashed', color = 'blue') +
  geom_vline(aes(xintercept = conmax), linetype = 'dashed', color = 'red') +
  geom_text(aes(x=lmin + 0.35, label=round(lmin, digits = 3), y=0.3), colour='black', size = tsize) +
  geom_text(aes(x=libmax + 0.35, label=round(libmax, digits = 3), y=0.48), colour='black', size=tsize) +
  geom_text(aes(x=conmax + 0.35, label=round(conmax, digits = 3), y=0.46), colour='black', size=tsize) +
  labs(title = element_blank(), x = 'Ideology', y = 'Density') +
  theme_classic()
dplot

ggsave('../images/ideology_dist.png', plot = dplot)

###################################
# Non-compliant tweets by ideology
###################################

# get median values of ideology for plots
meds <- tweets %>% group_by(non_comp) %>% summarise(Median=median(ideology))

# Box plot
b <- ggplot(tweets, aes(x= non_comp, y = ideology, fill = non_comp)) +
  geom_boxplot(alpha = 0.2) +
  scale_fill_manual(values = c('blue', 'red')) +
  theme_classic() +
  theme(legend.position = 'none') +
  labs(title = 'Distribution of Tweet Author Ideology by Label', x = 'Tweet Label', y = 'Ideology') +
  geom_text(data = meds, aes(x = non_comp, y = Median, label = Median), size = 3, vjust = 1)
b
ggsave('images/ideal_box.png', plot = b)

# Violin plot
v <- ggplot(tweets, aes(x= non_comp, y = ideology, fill = non_comp)) +
  geom_violin(alpha = 0.2) +
  scale_fill_manual(values = c('blue', 'red')) +
  theme_classic() +
  theme(legend.position = 'none') +
  labs(title = element_blank(), x = 'Tweet Label', y = 'Ideology') +
  geom_text(data = meds, aes(x = non_comp, y = Median, label = Median), size = 3, vjust = 1)
v
ggsave('../images/ideal_violin.png', plot = v)

# correlation with ideology
#tweets$non_comp_num <- ifelse(tweets$non_comp == 'Non-Compliant', 1, 0)
#cor(tweets$non_comp_num, tweets$ideology)

#poles$non_comp_num <- ifelse(poles$non_comp == 'Non-Compliant', 1, 0)
#poles$con <- ifelse(poles$pole == 'Conservative', 1, 0)
#cor(poles$non_comp_num, poles$con)

# need to drop tweets from people that weren't in the final analysis
