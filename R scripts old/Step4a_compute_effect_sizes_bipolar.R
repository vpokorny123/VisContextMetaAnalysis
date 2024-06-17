setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir2save<-'csvs/'
library(esc)
#in my experience esc is vastly superior to effectsize
#a. because it actually uses the right formula for computing t to d and
#b. because it allows for conversions with unbalanced designs
#c. it directly computes hedges' g which is nice

#this function is from esc, but I wasn't able to call it from the package
#so just putting it here for easy use
esc.vd <- function(d, grp1n, grp2n) {
  (grp1n + grp2n) / (grp1n * grp2n) + (d * d) / (2 * (grp1n + grp2n))
}

es_results = data.frame()


########## SIZE PARADIGMS ############
paradigm_type <- "Size"

paper <- "Tam et al. 1998"
grp2m  <- 60.72
grp2sd <- 11.08
grp2n  <- 23
grp1m  <- 58.27
grp1sd <- 6.29
grp1n  <- 10
g<-esc_mean_sd(grp1m, grp1sd, grp1n,grp2m, grp2sd, grp2n, es.type = 'g')
single_row<-cbind(paper,paradigm_type,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)


########## Contrast PARADIGMS: Orientation DEPENDENT + INDEPENDENT ############
paradigm_types = c('Contrast: Orientation Independent', 'Contrast: Orientation Dependent')

paper <- 'Schallmo et al. 2015'
grp1n = 19
grp2n = 38
nSubj = grp1n + grp2n
ds = c(.26, NA)
for (j in seq(paradigm_types)){
  paradigm_type = paradigm_types[j]
  d = ds[j]
  g$es<-esc::hedges_g(d,nSubj)
  g$se<-sqrt(esc.vd(g$es,grp1n, grp2n))
  single_row<-cbind(paper,paradigm_type,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)}

paper <- 'Pokorny et al. 2023'
#reading in the raw data from the paper to compute the effect sizes
raw_data<-as.data.frame(data.table::fread(paste0('csvs/paper_data.csv')))
raw_data_goodfit<-raw_data[raw_data$goodfit==1,]
raw_data_goodfit_bpcon<-raw_data_goodfit[raw_data_goodfit$grp %in% c('bp','ctrl'),]
d = lsr::cohensD(offset ~ grp, raw_data_goodfit_bpcon)
grp1n = 29
grp2n = 29
g$es<-esc::hedges_g(d, grp1n + grp2n)
g$se<-sqrt(esc.vd(g$es,grp1n, grp2n))
paradigm_type = paradigm_types[1]
single_row<-cbind(paper,paradigm_type,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)

f = 4.20
g <- esc_f(f, grp1n = grp1n, grp2n = grp2n,es.type = 'g')
paradigm_type = paradigm_types[2]
single_row<-cbind(paper,paradigm_type,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results,single_row)


###### Contrast Paradigms: DISTANCE dependent############
# this is kind of a special case in which degree of suppression by flanking context
#is measured as a function of distance...
paradigm_type = "Contrast: Distance Independent"
#this study provides cohen's d values for scz vs con and scz vs. bp , but not for bp vs. con
#however we can get a rough estimate of the cohen's D by eye-balling the SEMs to determine the 
# means and SDs
#across all distances so let's just average cohen's d across the distances to get main effect.
# Not including 0 or 1 lambda condition because that is not visual context...
paper <- "Keri et al. 2005"
grp1n = 22
grp2n = 20
lambda_means_grp1        <- c( -.110, -.150, -.110, -.06, -.015, -.005)
lambda_means_grp2        <- c( -.130, -.200, -.145, -.075, -.04, .005)
lambda_upper_error_grp1  <- c( -.085, -.135, -.088, -.037,  .01, .013)
lambda_upper_error_grp2  <- c( -.110, -.185, -.13, -.062, -.013, .023)
lambda_ses_grp1         <- lambda_upper_error_grp1 - lambda_means_grp1  
lambda_ses_grp2         <- lambda_upper_error_grp2 - lambda_means_grp2 
lambda_sds_grp1         <- lambda_ses_grp1*sqrt(grp1n)
lambda_sds_grp2         <- lambda_ses_grp2*sqrt(grp2n)


for (j in seq(lambda_means_grp1)){
  grp1m  <- lambda_means_grp1[j]
  grp2m  <- lambda_means_grp2[j]
  grp1sd <- lambda_sds_grp1[j]
  grp2sd <- lambda_sds_grp2[j]
  g<-esc_mean_sd(grp1m, grp1sd, grp1n,grp2m, grp2sd, grp2n, es.type = 'g')
  single_row<-cbind(paper,paradigm_type,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}


########## Contour PARADIGMS ############
paradigm_type = 'Contour'
paper <- 'Pokorny et al. 2021'
# taken from SPSS output documented in '3/28/19 Updated Gabor Results' Google Slides
grp1n = 23
grp2n = 37
# read in raw data from paper
raw_data        <- data.table::fread('csvs/Gabor Linegraph data.csv')
raw_data$id     <- seq(nrow(raw_data))
raw_data <- raw_data[raw_data$groupIDs %in% c(3,5),] #3 = BP, 5 = CON
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
  single_row<-cbind(paper,paradigm_type,round(g$es,3),round(g$se,3))
  es_results <- rbind(es_results,single_row)
}

########## Multi-paradigm Studies ############
paper <- 'Yang et al. 2013'
# this paper is kind of a pain
#they report a cohen's D for contrast only so let's factor that in first
paradigm_type = 'Contrast'
d = .29
grp1n = 13 # 1 lost outright, 2 excluded for >3 sd. Thus 16-3 = 13
grp2n = 23 # I'm only counting 22 in the plot, but paper does not report exclusions
g$es <- esc::hedges_g(d,grp1n+grp2n)
g$se <- sqrt(esc.vd(g$es, grp1n = grp1n, grp2n = grp2n))
single_row<-cbind(paper,paradigm_type,round(g$es,3),round(g$se,3))
es_results <- rbind(es_results, single_row)


# now time for the grunt work
# the. only way I can get a reasonable estimate of Cohen's d is to eyeball the 
#individual values for each group based on figure 3

paradigm_types = c('Size', 'Motion', 'Orientation', 'Luminance')
bp_vals = list(c(16, 13.5, 11,    10.5, 9.25, 7.75, 7.5, 7.25, 6.5,  5.75, 4.9, 4.9,  3.5, 3.4, 2.3, 1.9),
               c(16, 15.5, 15.25, 13.5, 12.5, 11,    10, 10,   9.25, 8.5, 8.5,  5.75, 5,   2,    1),
               c(4.4, 4,    3.5,  3.4,   2.9, 2.5,   2.3, 2,   1.7,   1.65, 1.4,  1.4,   1.4,   1.38, 1.1,   1),
               c(13.9,12.9, 12.3, 11.6, 10.6, 10.5, 10.1, 9.9, 8,    7.8, 7.3, 6.8, 6.75,  6,   5.1))
con_vals = list(c(16.4, 15.8, 15, 14.2, 13.8, 12.5, 12.2, 11, 9.1, 9, 8.7, 8.2, 7.2, 7, 6, 6, 5.5, 4.6, 3.5, 3.1, 1.7, 1, -.8),
               c(26, 22, 20, 16, 15.5, 14.5, 11.5, 9.7, 9.5, 9.2, 7.1, 7.1, 7.1, 7, 6, 6, 4.8, 4.2, 3.9, 3.9, .9),
               c(4.4, 3.85, 3.5, 3.4, 3.2 , 3.15, 2.85, 2.7, 2.5, 2.45, 2.3, 2.2, 2.1, 2.1, 2.05, 1.8, 1.8, 1.7, 1.65,  1.6, 1.25, 1.15, .8 ),
               c(13.8, 13.5, 13.1, 12.2, 11.65, 11.6, 11.6, 11.1, 11, 10.9, 10.6, 10.6, 10.5, 10.2, 9.8, 9.2, 8, 8, 6.5, 6.3, 6, 5.2, 2.5))

for (j in seq(paradigm_types)){
  paradigm_type = paradigm_types[j]
  grp1n = length(bp_vals[[j]])
  grp2n = length(con_vals[[j]])
  grp1m  <- mean(bp_vals[[j]])
  grp2m  <- mean(con_vals[[j]])
  grp1sd <- sd(bp_vals[[j]])
  grp2sd <- sd(con_vals[[j]])
  g<-esc_mean_sd(grp1m, grp1sd, grp1n, grp2m, grp2sd, grp2n, es.type = 'g')
  single_row<-cbind(paper,paradigm_type,round(-g$es,3),round(g$se,3))
  es_results <- rbind(es_results, single_row)
}

paper <- 'Salmela et al. 2021'
paradigm_types = c('Luminance', 'Contrast')
grp1n = 38
grp2n = 29
                    #brightness, contrast
bp_m            <- c(.3,          .035)     
con_m           <- c(.42,         .19)
bp_upper_error  <- c(.365,        .065)
con_upper_error <- c(.51,         .245)
bp_se           <- bp_upper_error- bp_m
con_se          <- con_upper_error- con_m
bp_sd           <- bp_se * sqrt(grp1n)
con_sd          <- con_se * sqrt(grp2n)
for (j in seq(bp_m)){
  paradigm_type = paradigm_types[j]
  grp1m  <- bp_m[j]
  grp2m  <- con_m[j]
  grp1sd <- bp_sd[j]
  grp2sd <- con_sd[j]
  g<-esc_mean_sd(grp1m, grp1sd, grp1n, grp2m, grp2sd, grp2n, es.type = 'g')
single_row<-cbind(paper,paradigm_type,round(-g$es,3),round(g$se,3))
es_results <- rbind(es_results, single_row)
}

colnames(es_results)<-c('Study','Paradigm',"Effect Size","Standard Error")
write.csv(es_results,paste0(dir2save,'effect_sizes_bp.csv'))



#### need to get aux info as well ####
library(data.table)
#read in acuity and stimuli location info
aux_info<-fread(paste0(dir2save,'/Table 2.csv'))
colnames(aux_info)<-as.character(aux_info[2,])
aux_info <- aux_info[,c(2,7,8,11)]
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
aux_info$`Minimum Acuity Threshold`[aux_info$`Minimum Acuity Threshold` == "not specified"] = 'None'
aux_info$`Minimum Acuity Threshold`[aux_info$`Minimum Acuity Threshold` == "matched groups"] = 'Matched'

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
write.csv(aux_info,paste0(dir2save,'aux_info_bp.csv'))


