library(metafor)
library(data.table)
plot_file_ext <- 'png' #png or SVG
comparison_type <- 'bp' # choose between scz or bp
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir2load<-'csvs/'
df <- fread(paste0(dir2load,'effect_sizes_',comparison_type,'_revised.csv'),
            header = TRUE)
df_aux <- fread(paste0(dir2load,'aux_info_',comparison_type,'_revised.csv'),
                header = TRUE )
df = df[!is.na(df$`Effect Size`),]
df_aux = df_aux[!is.na(df_aux$`Effect Size`),]
df = cbind(df,df_aux)

illusions = list()
if (comparison_type == 'bp'){
  group_name <-'BP'
  domains <- 'all'
  for (j in domains){
    illusions[[j]] = 'all'
  }
} else {
  group_name <-'SCZ'
  domains <- c('all','Size','Contrast','Contrast: Orientation Independent',
               'Contrast: Orientation Dependent',
               'Lightness','Orientation', 'Contour')
  for (j in domains){
    illusions[[j]] = c('all',unique(df$Illusion[grepl(j,df$Domain)]))
  }
}

for (j in domains){
  if (j == 'all'){
    domain_df <- df 
  } else if ( j == "Contrast") { 
    domain_df <- df[grepl(j,df$Domain),]
  } else { 
    domain_df <- df[df$Domain == j,]
  }
  illusion <- illusions[[j]]
  for (jj in illusion){
    if (jj == 'all'){
      illusion_df = domain_df
      }else{
      #subset to specific illusion type
      illusion_df = domain_df[domain_df$Illusion == jj,]
      #now let's say if there's less than four studies we'll skip to next Illusion
      #this is mainly to save computation time and my own sanity 
      #browser()
      if (length(unique(illusion_df$Study))<5){ #<5 is 5 or more
        next
      }
    }
    meta_df <- escalc(measure = 'SMD', yi = `Effect Size`, sei=`Standard Error`, data = illusion_df)
    meta_df$ID <- seq(nrow(meta_df))
    V <- vcalc(vi, cluster=Study, obs=ID, data=meta_df, rho=0.6)
    #browser()
    #for plotting purposes let's aggregate so there is only one effect size per study
    agg <- aggregate(meta_df, cluster=Study, V=V, addk=TRUE)
    df_aux[as.integer(agg$V1),]
    #browser()
    agg.model <- rma(yi, vi, method="REML",
                     slab = Study,
                     test = "t",
                     data=agg)
    
    #compute 95% confidence intervals for I^2 statistic to be include in forest plots
    conf_I2<-confint(agg.model)
    lower_I2<-round(conf_I2$random[3,2],1)
    upper_I2<-round(conf_I2$random[3,3],1)
    
    ############# PLOTTING ##############
    if (plot_file_ext == 'png') {
      png(paste0('../plots/fornel/',j,'_domain_',jj,'_illusion_',comparison_type,'.png'),
          units = 'in', 
          width = 5.7, 
          height = 6.1, 
          res = 600)
    } else if (plot_file_ext == 'tiff'){
      tiff(paste0('../../Biological Psychiatry Meta-Analysis Submission/',j,
                  '_domain_',jj,'_illusion_',comparison_type,'.tiff'),
           units = 'in', 
           width = 5.7, 
           height = 6.1, 
           res = 500)
    }
    layout(matrix(c(1,2,3), ncol=1),
           heights = c(.05,.011,.11))
    
    ######### TOP PLOT FUNNEL ###########
    par(tck=-.01,
        mgp=c(1.5, .15, 0),
        mar=c(0.3, 8, 0, 8),#bottom, left, top, right
        cex = .7) #can set separate par for each plot within layout
    funnel(agg.model, slab = agg.model$slab, cex = .01, lty = 0)
    text(x = agg.model$yi,y = sqrt(agg.model$vi),
         labels = substr(agg.model$slab, start = 1, stop = 4),
         cex=.85, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8))
    mylims <- par("usr")
    x_coords = c(mylims[1],mylims[2])
    #add regression test info
    reg_res<-regtest(agg.model)
    #browser()
    regtest_text<- paste0('b=',round(reg_res$est,2), 
                          ', t(',reg_res$dfs,')=',
                          round(reg_res$zval,2),
                          ',\np=',round(reg_res$pval,2)
                          )
    if (jj == 'Mueller-Lyer' ){
      regtest_x = -.5
    }else {
      regtest_x = ((max(x_coords)/3) - .25)
    }
    text(x = regtest_x,
         y=0,
         paste0('Asymmetry Test:\n',regtest_text),
         adj = c(0,1),
         cex = .9)

    
    ######### MIDDLE PLOT: Arrows and Labels ##########
    plot(x_coords,c(-.25,1),
         type = "n", xlab = "", ylab = "", axes = F, xaxs = "i",xlim = x_coords)
    #xlim(x_coords)
    middle_point_x = mean(x_coords)
    x_length = diff(x_coords)
    arrow_length = x_length/4 
    text(0, .4, "Hedges' g", cex = .9, xpd = TRUE)
    arrows(0, 0, arrow_length, 0, length = .07, xpd = TRUE)
    arrows(0, 0,-arrow_length, 0, length =.07, xpd = TRUE)
    #vertical line marking 0
    lines(c(-.01,-.01),c(-.1,.1))
    #arrow labels
    text(    -.02,         -.32, paste0(group_name,' = CON'), cex = .77, xpd = TRUE)
    text(    arrow_length, -.32, paste0(group_name,' < CON'), cex = .77, xpd = TRUE)
    text(   -arrow_length, -.32, paste0(group_name,' > CON'), cex = .77, xpd = TRUE)
    
    ######### LOWER PLOT: FOREST ##############
    par(tck=-.01,
        mgp=c(1.3, .1, 0), #first coordinate affects title whereas 2:3 affect axis; default is 3,1,0
        mar=c(1.9, 2, 0, 1) #bottom, left, top, right
    ) 
    #shorten grzeczkowski labels
    agg.model$slab[grepl('Grze',agg.model$slab)]<-paste0(substr(agg.model$slab[grepl('Grze',agg.model$slab)],1,29),c(1,2))
    ilabs<-forest(agg.model, cex=.84, shade = TRUE,
                  mlab = paste0('I^2 = ',
                                round(agg.model$I,1),'% [',lower_I2,',',upper_I2,']'),
                  header = c("Study","Hedges' G [95% CI]"),
                  xlab = "",
                  order = agg$Total.N,
                  ilab = agg[,c('Total.N','Minimum.Acuity.Threshold',
                                'Target.Stimulus.Location..Foveal.or.Peripheral.')],
                  ilab.pos = 2,
                  level = 95
    )$ilab.xpos
    #add Acuity and Location headers
    cex_labels = .84
    font_labels = 2
    #pos_labels = 4
    text(ilabs[1], length(agg.model$slab)+2+.02, substitute(paste(bold('N'))),
         font = font_labels, cex = cex_labels, #pos = pos_labels
         adj = c(2.5,.6) #, offset = .6
         )
    text(ilabs[2], length(agg.model$slab)+2, substitute(paste(bold('Acuity'))),  
         font= font_labels, cex = cex_labels, #pos = pos_labels
         adj = c(1.1,.6) #, offset = .6
         )
    text(ilabs[3], length(agg.model$slab)+2+.02, substitute(paste(bold('Loc.'))), 
         font= font_labels, cex = cex_labels, #pos = pos_labels
         adj = c(1.5,.6) #, offset = .6
         )
    
    lims<- par('usr') # c(x1, x2, y1, y2)
    text(0, lims[3]-(abs(lims[3]-lims[4])*.057),"Hedges' G", xpd = TRUE, cex = .9)
    dev.off()
  }
}
