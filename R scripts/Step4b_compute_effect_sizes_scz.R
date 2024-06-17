setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir2save<-'csvs/'
library(esc) #in my experience esc is vastly superior to effectsize because:
#a. it actually uses the right formula for computing t to d and
#b. it allows for conversions with unbalanced designs
#c. it directly computes hedges' g which is nice
library(dplyr)

#this function is from esc, but I wasn't able to call it from the package
#so just putting it here for easy use
esc.vd <- function(d, grp1n, grp2n) {
  (grp1n + grp2n) / (grp1n * grp2n) + (d * d) / (2 * (grp1n + grp2n))
}

es_results = data.frame()


########## SIZE domain ############
domain_type <- "Size" #really this variable should be changed to domain_type, but that's a problem for another day
illusion_name <- "Size Constancy"
paper <- 'Harway & Salzman (1964)'
total_n = NA
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)


paper <- "MacDorman et al. (1964)"
illusion_name <- "Size Constancy"
t = -5.43
grp1n = 10
grp2n = 10
total_n = grp1n + grp2n
g<-esc_t(-5.43, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
single_row<-cbind(paper,total_n, domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)


paper <-"Turkozer et al. (2019)"
illusion_name <- "Ball in the Hall"
grp1n <- 28
grp2n <- 31
total_n = grp1n + grp2n
f = 1.96
g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <- "King et al. (2008)"
illusion_name <- "Delbeouf (square frame variant)"
total_n = 42 + 42
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- 'Asarnow & Mann (1978)'
illusion_name <- "Ebbinghaus"
total_n = 30 + 15
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- 'Uhlhaas et al. (2006) - 1'
illusion_name <- "Ebbinghaus"
total_n = 35 + 35
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- 'Uhlhaas et al. (2006) - 2'
illusion_name <- "Ebbinghaus"
total_n = 48 + 26
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- 'Silverstein et al. (2013)'
illusion_name <- "Ebbinghaus"
total_n = 21 + 27
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- "Cromwell (1968)"
illusion_name <- "Mueller-Lyer"
total_n = 30 + 15
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- "Rund et al. (1994)"
illusion_name <- "Mueller-Lyer"
grp2m  <- 28.5
grp2sd <- 9
grp2n  <- 35
grp1m  <- 23.9
grp1sd <- 8.2
grp1n  <- 20

g<-esc_mean_sd(grp1m=grp1m, grp1sd=grp1sd, grp1n = grp1n,
               grp2m = grp2m, grp2sd = grp2sd, grp2n = grp2n,
               es.type = 'g')
total_n = 35 + 20
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <- "Tam et al. (1998)"
illusion_name <- "Mueller-Lyer"
grp2m  <- 70.74
grp2sd <- 8.54
grp2n  <- 26
grp1m  <- 58.27
grp1sd <- 6.29
grp1n  <- 10
g<-esc_mean_sd(grp1m, grp1sd, grp1n,grp2m, grp2sd, grp2n, es.type = 'g')
total_n = grp1n + grp2n
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <- "Parnas et al. (2001)"
illusion_name <- "Mueller-Lyer"
total_n = 19 + 14
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)




########## ORIENTATION domainS ############

domain_type <- "Orientation"
paper <-"Seymour & Kaliuzhna (2022)"
illusion_name <- "Tilt Illusion"
#the following values are approximate values visually decoded from Figure 3
grp2m  <- 3.8 #SCZ mean
grp2sd <- 6-grp2m #SCZ sd
grp2n  <- 19
grp1m  <- 4.2 #CON
grp1sd <- 6.5-grp1m
grp1n  <- 19
g<-esc_mean_sd(grp1m, grp1sd, grp1n,grp2m, grp2sd, grp2n, es.type = 'g')
total_n = grp2n + grp1n
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <-"Razzak et al. (2022)"
illusion_name <- "Rod and Frame"
#authors report signed and unsigned error with little justification for which should be
#preferred. For now we will choose
grp1n = 15
grp2n = 17
f1 = 6.37
f2 =.67
g1 = esc_f(f1, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
g2 = esc_f(f2, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
total_n = grp2n + grp1n
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(-g1$es,3),round(g1$se,3))
es_results <- rbind(es_results,single_row)
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(-g2$es,3),round(g2$se,3))
es_results <- rbind(es_results,single_row)

########## Contrast domains: Orientation INDEPENDENT ############
domain_type = 'Contrast: Orientation Independent'
illusion_name <- "Unoriented Contrast-Contrast"
paper <- "Dakin et al. (2005)"
#paper only reports SCZ vs. psychiatric + non-ps
t = 6.12
grp1n = 15
grp2n = 33
total_n = grp2n + grp1n
g = esc_t(t, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <- 'Barch et al. (2012)'
illusion_name <- "Unoriented Contrast-Contrast"
#not sure if this works with 3 way anova... update: my simulations suggest that this
#DOES in fact work
f = 7.27
grp1n = 130
grp2n = 132
total_n = grp2n + grp1n
g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <- 'Strauss et al. (2014)'
illusion_name <- "Unoriented Contrast-Contrast"
grp1n = 99
grp2n = 131
total_n = grp2n + grp1n
d <- .13
g$es<-esc::hedges_g(d,grp1n + grp2n)
g$se<-sqrt(esc.vd(g$es,grp1n, grp2n))
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

########## Contrast Domain: Orientation DEPENDENT + INDEPENDENT ############
#Orientation-dependent contrast domains generally test two hypotheses:
#1. SCZ show less suppression by surrounding context irrespective of orientation
#(usually reported as a main effect of group)
#2. SCZ show less change in suppression as a function of change in orientation
#(usually reported as an interaction between group and orientation conditions)
#For this convergent analysis it makes the most sense to focus on #1 because that is
#the primary hypothesis tested by most of the domains (and allows for better pooling
#across orientation dependent and orientation-independent domains).
#DON"T WORRY THOUGH, we will conduct a follow up meta-analysis of the orientation
#dependent contrast suppression effects in another script
domain_types = c('Contrast: Orientation Independent', 'Contrast: Orientation Dependent')

paper <-"Must et al. (2004)"
illusion_name <- "Collinear Flankers"
Fs = c(NA, 13.95)
grp1n = 20
grp2n = 15
total_n = grp2n + grp1n
for (j in seq(domain_types)){
  f = Fs[j]
  domain_type = domain_types[j]
  if (is.na(f)){
    g$es <- NA
    g$se <- NA
  } else {
    g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')}
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}


paper <-"Keri et al. (2005) - 1"
illusion_name <- "Collinear Flankers"
domain_type <- "Contrast: Orientation Dependent"
means = c(-.07, -.31)
sds = c(.18, .17)
ns = c(35, 20)
g = esc_mean_sd(grp1n = ns[1],    grp2n = ns[2],
                grp1m = means[1], grp2m = means[2],
                grp1sd = sds[1],  grp2sd = sds[2])
total_n <- ns[1] + ns[2]
single_row <- cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

domain_type <- "Contrast: Orientation Independent"
single_row <- cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- "Keri et al. (2009)"
illusion_name <- "Collinear Flankers"
#ran simulations to check whether sqrt(F) trick works for three way anova (2x2x2)
#it does assuming folks are using type II or III SS method (which I think is generally
#fair to assume especially for designs with unequal group sizes)
Fs = c(NA,47.49)
grp1n = 43
grp2n = 25
total_n = grp2n + grp1n
for (j in seq(domain_types)){
  f = Fs[j]
  domain_type = domain_types[j]
  if (is.na(f)){
    g$es <- NA
    g$se <- NA
  } else {
    g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')}
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}

paper <- 'Keane et al. (2014)'
illusion_name <- "Collinear Flankers"
grp1n = 24
grp2n = 24
total_n = grp2n + grp1n
Fs = c(NA,.253)
for (j in seq(domain_types)){
  f = Fs[j]
  domain_type = domain_types[j]
  if (is.na(f)) {g$es = NA 
  g$se = NA}
  else{
    g = esc_f(f, grp1n = grp1n, grp2n = grp2n,es.type = 'g')
  }
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}


paper <- 'Keane et al. (2019)' 
illusion_name <- 'Collinear Flankers'
grp1n = 38
grp2n = 50
total_n = grp2n + grp1n
fs = c(4.68, 2.71) #high SF and low SF
sfs = c('high','low')
for (j in seq(fs)) {
  f = fs[j]
  sf = sfs[j]
  g = esc_f(j, grp1n = grp1n, grp2n = grp2n,es.type = 'g')
  if (sf == 'high'){ #SCZ modulated more for low spatial frequency condition, but less for high 
    g$es = -g$es
  }
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}

paper <- 'Yoon et al. (2009)'
illusion_name <- "Oriented Contrast-Contrast"
Fs = c(.37, 8.93)
grp1n = 17
grp2n = 20
total_n = grp2n + grp1n
for (j in seq(domain_types)){
  f = Fs[j]
  domain_type = domain_types[j]
  g = esc_f(f, grp1n = grp1n, grp2n = grp2n,es.type = 'g')
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)}

paper <- 'Seymour et al. (2013)'
illusion_name <- "Oriented Contrast-Contrast"
grp1n = 18
grp2n = 18
total_n = grp2n + grp1n
set.seed(1)
#V1, V2, V3
Fs_list = list(runif(3, min = -1, max = 1),#paper only reports that f-values were < 1 so let's just randomly select three values between 0 and 1
               c(4.87, 4.7, 6.11))
for (j in seq(domain_types)){
  domain_type = domain_types[j]
  Fs = Fs_list[[j]]
  for (jj in seq(Fs)){
    f = Fs[[jj]]
    if (f<0){
      f = abs(f)
      g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
      single_row <- cbind(paper,total_n,domain_type,illusion_name,-round(g$es,3),round(g$se,3))
      es_results <- rbind(es_results,single_row)
    }else {
      g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
      single_row <- cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
      es_results <- rbind(es_results,single_row)}
  }
}


paper <- 'Serrano-Pedraza et al. (2014)'
illusion_name <- 'Oriented Contrast-Contrast'
grp1n = 21
grp2n = 24
total_n = grp2n + grp1n
Fs = c(22.69,13.25)
for (j in seq(domain_types)){
  f = Fs[j]
  domain_type = domain_types[j]
  g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)}


paper <- 'Schallmo et al. (2015)'
illusion_name <- 'Oriented Contrast-Contrast'
grp1n = 23
grp2n = 38
total_n = grp2n + grp1n
nSubj = grp1n + grp2n
ds = c(.42, NA)
for (j in seq(domain_types)){
  domain_type = domain_types[j]
  d = ds[j]
  g$es<-esc::hedges_g(d,nSubj)
  g$se<-sqrt(esc.vd(g$es,grp1n, grp2n))
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
  }


paper <- 'Pokorny et al. (2023)'
illusion_name <- 'Oriented Contrast-Contrast'
#reading in the raw data from the paper to compute the effect sizes
raw_data<-as.data.frame(data.table::fread('csvs/paper_data.csv'))
raw_data_goodfit<-raw_data[raw_data$goodfit==1,]
raw_data_goodfit_sczcon<-raw_data_goodfit[raw_data_goodfit$grp %in% c('sz','ctrl'),]
d = lsr::cohensD(offset ~ grp, raw_data_goodfit_sczcon)
grp1n = 31
grp2n = 29
total_n = grp2n + grp1n
g$es<-esc::hedges_g(d, grp1n + grp2n)
g$se<-sqrt(esc.vd(g$es,grp1n, grp2n))
domain_type = domain_types[1]
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

f = 4.2
g <- esc_f(f, grp1n = grp1n, grp2n = grp2n,es.type = 'g')
domain_type = domain_types[2]
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

domain_type = "Contrast: Distance Dependent"
#this study provides cohen's d values for different lambda distances but no value
#across all distances so let's just average cohen's d across the distances to get main effect.
# Not including 0 and 1 lambda condition because that is not visual context...
paper <- "Keri et al. (2005) - 2"
illusion_name <- 'Collinear Flankers'
grp1n = 24
grp2n = 20
total_n = grp2n + grp1n
lambda_ds = c(1.29,1.41,1.42,1.25,.78,.45)
for (j in lambda_ds){
  g$es<-esc::hedges_g(j,totaln = grp1n + grp2n)
  g$se<-sqrt(esc.vd(g$es, grp1n = grp1n, grp2n = grp2n))
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}

########## Motion domain ############
domain_type = "Motion"
paper <- 'Tadin et al. (2006)'
illusion_name <- 'Size-Dependent Drifting Grating'
#cannot convert F statistic to Hedges G
#tried to compute ES based on figure for only min and max conditions and assuming
#within subject correlation, but it's a little too convoluted and requires
# assuming two levels worth of correlations (within-subject and within-study)
grp1n = 16
grp2n = 14
total_n = grp1n + grp2n
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- 'Chen et al. (2008)'
illusion_name <- 'Center-Surround Random Dot Patterns'
grp1n = 24
grp1n = 33
total_n = grp2n + grp1n
f = 28.67
g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(-g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

########## Contour domain ############
domain_type = "Contour"
paper <- 'Roinishvili et al. (2015)'
illusion_name <- 'Vernier Crowding'
grp1n = 15
grp2n = 16
total_n = grp2n + grp1n
f = .02
g = esc_f(f, grp1n = grp1n, grp2n = grp2n, es.type = 'g')
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <- 'Choung et al. (2022)'
illusion_name <- 'Vernier Crowding'
grp1n = 15
grp2n = 15
total_n = grp1n + grp2n
single_row<-cbind(paper,total_n,domain_type,illusion_name,NA,NA)
es_results <- rbind(es_results,single_row)

paper <- 'Schallmo et al. (2013)'
illusion_name <- 'Collinear Gabor Contour'
grp1n = 25
grp2n = 28
total_n = grp1n + grp2n
#para, orth
ds = c(.68,-.47)
for (j in ds){
  g$es <- esc::hedges_g(j,grp1n+grp2n)
  g$se <- sqrt(esc.vd(g$es, grp1n = grp1n, grp2n = grp2n))
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results, single_row)
}

paper <- 'Pokorny et al. (2021)'
illusion_name <- 'Collinear Gabor Contour'
grp1n = 27
grp2n = 37
total_n = grp1n + grp2n
#read in raw data
raw_data        <- data.table::fread('csvs/Gabor Linegraph data.csv')
raw_data$id     <- seq(nrow(raw_data))
raw_data <- raw_data[raw_data$groupIDs %in% c(1,5),] #1 = SCZ, 5 = CON
raw_data$conmod_para_P2 <- raw_data$para.p2.meanAmp.contra - 
  raw_data$rand.p2.meanAmp.contra
raw_data$conmod_orth_P2 <- raw_data$orth.p2.meanAmp.contra - 
  raw_data$rand.p2.meanAmp.contra
raw_data$conmod_para_P3 <- raw_data$para.p3.meanAmp.CPz - 
  raw_data$rand.p3.meanAmp.CPz
raw_data$conmod_orth_P3 <- raw_data$orth.p3.meanAmp.CPz - 
  raw_data$rand.p3.meanAmp.CPz
indices <- list(c('conmod_para','conmod_orth'),
                c('conmod_para_P2','conmod_orth_P2'),
                c('conmod_para_P3','conmod_orth_P3'))
for (j in seq(indices)){
  #let's start with behavioral data and look at the interaction between group and orientation
  raw_data_melt<-tidyr::pivot_longer(raw_data, cols = indices[[j]],
                                     names_to = c('condition'))
  result<-afex::aov_ez(data = raw_data_melt,
                       dv = 'value',
                       id = 'id',
                       within = 'condition',
                       between = 'groupIDs',
                       type = 3,
                       anova_table=list(correction = "HF"))
  f = result$anova_table[3,4]
  g<- esc_f(f, grp1n=grp1n, grp2n=grp2n, es.type = 'g')
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}



########## Lightness domains ############
domain_type = "Lightness"
paper <- 'Kaliuzhna et al. (2019)'
illusion_name <- 'Lightness Grid'
grp1n = 19
grp2n = 21
total_n = grp1n + grp2n
d = .22 #experiment 1a
g$es <- esc::hedges_g(d,grp1n+grp2n)
g$se <- esc.vd(g1$es,grp1n,grp2n)
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

Fs = c(.13,.04) #experiment 1b and 1c
for (j in seq(domain_types)){
  f = Fs[j]
  g = esc_f(f, grp1n = grp1n, grp2n = grp2n,es.type = 'g')
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)}



########## Multi-domain Studies ############
domain_type <- "Size + Contour"
paper<- 'Bolte et al. (2007)'
grp1m  <- 11.7
grp1sd <- 2.8
grp1n  <- 15
grp2m  <- 8.4
grp2sd <- 2.2
grp2n  <- 15
total_n = grp1n + grp2n
g<-esc_mean_sd(grp1m, grp1sd, grp1n, grp2m, grp2sd, grp2n, es.type = 'g')
single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

paper <-"Kantrowitz et al. (2009)"
#Achieved sample consisted of 18 patients and 12 control subjects for the Sander’s paralellogram and
#Ponzo illusion

#ML, Pogg, Sand, Ponzo
grp1ns         <- c(38,   38,    18,  18)
grp2ns         <- c(28,   28,    12,  12)
total_n = grp1ns[1] + grp2ns[1]
domain_types   <- c('Size','Contour','Size','Size')
illusion_names <- c('Mueller-Lyer','Poggendorf','Sanders','Ponzo')
fs = c(10.9, .21, .35, 5.2) 
directions =c('-','+','-','+')
for (j in seq(fs)){
  domain_type   <- domain_types[j]
  illusion_name <- illusion_names[j]
  g             <- esc_f(fs[j], grp1n = grp1ns[j], 
                         grp2n = grp2ns[j], es.type = 'g')
  if (directions[j] == '+'){
    single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  }else{
    single_row<-cbind(paper,total_n,domain_type,illusion_name,round(-g$es,3),round(g$se,3))
  }
  es_results <- rbind(es_results,single_row)
}

paper <- 'Yang et al. (2013)'
#d-values taken from companion paper: Visual Context Processing in Schizophrenia
domain_types   <- c('Size',       'Motion',        'Orientation',   'Contrast: Orientation Independent', 'Lightness')
illusion_names <- c('Ebbinghaus', 'Motion-Motion', 'Tilt Illusion', 'Contrast-Contrast', 'Simultaneous Contrast')
scz_ns         <- c(29,26,27,27,28)
con_ns         <-c(23,21,23,23,23)
total_n = scz_ns[1] + con_ns[1]
ds = c(-.11,-.11,-.43,.64,.31)
for (j in seq(length(ds))){
  d = ds[j]
  domain_type = domain_types[j]
  illusion_name = illusion_names[j]
  grp1n = scz_ns[j]
  grp2n = con_ns[j]
  g$es <- esc::hedges_g(d,grp1n+grp2n)
  g$se <- sqrt(esc.vd(g$es, grp1n = grp1n, grp2n = grp2n))
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results, single_row)
}


paper <- 'Tibber et al. (2013)'
domain_types   <- c('Lightness', 'Contrast: Orientation Independent', 'Orientation', 'Size')
illusion_names <- c('Mean Difference Contrast-Contrast','Contrast-Contrast', 'Tilt Illusion','Ebbinghaus')
ds    <- c(-.2,.68,.39,.52)
grp1n <- c(18, 23, 20, 23) # these ns were extracted by counting points in figure 3
grp2n <- c(24, 23, 22, 24) # (and checking against df reported in Table 3)
total_n = grp1n[4] + grp2n[4]
for (j in seq(length(ds))){
  d = ds[j]
  domain_type = domain_types[j]
  illusion_name = illusion_names[j] 
  grp1n = scz_ns[j]
  grp2n = con_ns[j]
  g$es <- esc::hedges_g(d,grp1n+grp2n)
  g$se <- sqrt(esc.vd(g$es, grp1n = grp1n, grp2n = grp2n))
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results, single_row)
}

paper  <- 'Robol et al. (2013)'
domain_types   <- c('Contour','Orientation')
illusion_names <- c('Collinear Gabor Contour', 'Orientation Flankers')
grp1ns <- c(18, 13) #ns for contour, orientation crowding experiments
grp2ns <- c(18, 13)
total_n = grp1ns[1] + grp2ns[1]
Fs     <- c(1.08, 9.16) #interaction between group and context modulation index
for (j in seq(length(Fs))){
  f = Fs[j]
  domain_type = domain_types[j]
  illusion_name = illusion_names[j]
  grp1n = grp1ns[j]
  grp2n = grp2ns[j]
  g <- esc_f(f, grp1n= grp1n, grp2n = grp2n)
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results, single_row)
}

paper <- 'Grzeczkowski et al. (2018) - Sample 1'
#.                     EB,    ML,    PZ,      SC,      PZh,      WH,         TT
illusion_names <- c('Ebbinghaus','Mueller-Lyer','Ponzo','Simultaneous Contrast','Ball in the Hall','White','Tilt Illusion')
domain_types   <- c('Size','Size','Size','Lightness','Size','Lightness','Orientation')
ds             <- c( .02,  -.27,  .16,     .32,      .11,     -.4,        .19)
#experiment 1
grp1n<-19
grp2n<-19
total_n = grp1n + grp2n
for (j in seq(length(ds))){
  d = ds[j]
  domain_type = domain_types[j]
  illusion_name = illusion_names[j]
  g$es <- esc::hedges_g(d,grp1n+grp2n)
  g$se <- sqrt(esc.vd(g$es, grp1n = grp1n, grp2n = grp2n))
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results, single_row)
}

#experiment 2
paper <- 'Grzeczkowski et al. (2018) - Sample 2'
grp1n = rep(54,10) #authors do not report ns per group but we know total n
grp2n = rep(59,10) #so we will make a reasonable assumption of roughly equal data loss per group
dfs<-   c(111,98, 105, 110, 97,  108, 105,103,111,109)
floor_n <- floor((111-dfs)/2)
ceiling_n <- ceiling((111-dfs)/2)
set.seed(1) #for reproducibility
coin_flip<-(sample.int(2,10,replace = TRUE)-1)
coin_flip_comp <- (coin_flip-1) *-1
grp1_subtraction <- ifelse(coin_flip==1,floor_n,ceiling_n) #split data loss evenly
grp2_subtraction <- ifelse(coin_flip_comp==1,floor_n,ceiling_n) #split data loss evenly
grp1ns = grp1n-grp1_subtraction
grp2ns = grp2n-grp2_subtraction
ns = grp1n + grp2n
total_n = ns[1]
#                     EB,           EBs,      EBb,          PZ,   PZw,      PZg,         ML,           SC,                      WH,          TT
domain_types   <- c('Size',       'Size',    'Size',       'Size','Size',  'Size',      'Size',        'Lightness',            'Lightness','Orientation' )
illusion_names <- c('Ebbinghaus','Ebbinghaus','Ebbinghaus','Ponzo','Ponzo','Ponzo Grid','Mueller-Lyer','Simultaneous Contrast','White',    'Tilt Illusion')
ds             <- c(-.13,         -.06,            .16,      .18,    .12,  .00,            -.21,        .43,                    .07,           -.08)
for (j in seq(length(ds))){
  grp1n = grp1ns[j]
  grp2n = grp1ns[j]
  illusion_name = illusion_names[j]
  domain_type = domain_types[j]
  d = ds[j]
  g$es <- esc::hedges_g(d,grp1n+grp2n)
  g$se <- sqrt(esc.vd(g$es, grp1n = grp1n, grp2n = grp2n))
  single_row<-cbind(paper,total_n,domain_type,illusion_name,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results, single_row)
}

colnames(es_results)<-c('Study','Total N','Domain','Illusion',"Effect Size","Standard Error")
write.csv(es_results,paste0(dir2save,'effect_sizes_scz_revised.csv'))


#### need to get aux info as well ####
library(data.table)
#read in acuity and stimuli location info from Table 1. 
#THIS MEANS THAT IF YOU MAKE CHANGES TO TABLE 1 YOU NEED TO PROPAGATE THOSE CHANGES
#TO THIS TABLE
aux_info<-fread(paste0(dir2save,'/Table 1_revised.csv'))
colnames(aux_info)<-as.character(aux_info[2,])
aux_info <- aux_info[,c('Authors (Year)','Minimum Acuity Threshold',
                        'Target Stimulus Location (Foveal or Peripheral)',
                        "Effect Size (Hedges' G)")]
#remove blank rows- yes I know this is overkill but the sleeker solutions weren't working
idx_all = NULL
for (j in seq(nrow(aux_info))) {
  if (any(is.na(aux_info[j]))){ #this is why the other solutions weren't working
    next
  }
  if (all(aux_info[j] == "")){
    idx_all <- rbind(idx_all,j)
  }
}
aux_info = aux_info[-idx_all,]
#make aux info less verbose
aux_info$`Minimum Acuity Threshold`[aux_info$`Minimum Acuity Threshold` == "Not specified"] = 'None'
aux_info$`Minimum Acuity Threshold`[aux_info$`Minimum Acuity Threshold` == "Matched groups"] = 'Match'

aux_info$`Target Stimulus Location (Foveal or Peripheral)`[
  grepl('eripheral',aux_info$`Target Stimulus Location (Foveal or Peripheral)`)] =
  'Peri.'

#just filling in empty rows which will be useful later on
for (j in (seq(aux_info$`Authors (Year)`))){
  study = aux_info$`Authors (Year)`[j]
  acuity = aux_info$`Minimum Acuity Threshold`[j]
  location = aux_info$`Target Stimulus Location (Foveal or Peripheral)`[j]
  if (study == ""){
    aux_info$`Authors (Year)`[j] = aux_info$`Authors (Year)`[j-1]
  }
  if (acuity == ""){
    aux_info$`Minimum Acuity Threshold`[j] = aux_info$`Minimum Acuity Threshold`[j-1]
  }
  if (location== ""){
    aux_info$`Target Stimulus Location (Foveal or Peripheral)`[j] = aux_info$`Target Stimulus Location (Foveal or Peripheral)`[j-1]
  }
}

#remove extra header row
aux_info <- aux_info[2:nrow(aux_info),]

#finally 
write.csv(aux_info,paste0(dir2save,'aux_info_scz_revised.csv'))




