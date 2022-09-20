## WELCOME TO PIPLINE:
# 1. before running - use the GUI in matlab to create "matlab_to_R_GUI_data.xls"
# 2. this code needs two input files (CumulativeFeedingInExcelFormat.xlsx, DataInExcelFormat.xls)
# both has to be in the same directory of this file


# imports
library("readxl")
library("writexl")
library("stringr")
library("stringi")
library("openxlsx")

library(numbers)
library(ggplot2)
library(reshape2)
library(plyr)
library(lmerTest)
library(car)
library(emmeans)
options(scipen=999)

library(grid)
library("Hmisc")
library("dplyr")
library(svDialogs)

library(extrafont)
library(tidyverse)
library(gridExtra)

# another stuff
options(scipen=999)
setwd("C:/Users/labophir/Desktop/New folder") # NOTE - in another computers, this path need to be change to the right path
graphics.off()
par("mar")
par(mar=c(1,1,1,1))

# create pipeline directory
dir.create(file.path('pipeline'),recursive=TRUE, showWarnings = FALSE)

# ____________________________________________________
# 1. organizer: organize the data.
# need: A. input file 1 - CumulativeFeedingInExcelFormat.xlsx format, in the same directory
#       B. input file 2 - DataInExcelFormat.xls format (with "Electrodes" sheet) in the same directory
#       C. "matlab_to_R_GUI_data.xls" (created by matlab, no need to change location)
# create: A. "colors_hex.csv" 
#         B. organized_file
#         C. organized_file_without_zeros

GUI_data = read_excel("matlab_to_R_GUI_data.xls")

if (GUI_data$to_do_org == TRUE) { # then get into the organizer part
  
  # create output directory + get the data file
  data_file = GUI_data$org_file
  data_file = gsub("\\\\", "/", data_file)
  data_file = basename(data_file)
  
  first_file_name = tools::file_path_sans_ext(data_file)
  first_path = paste0('pipeline/', first_file_name)
  dir.create(file.path(first_path),recursive=TRUE, showWarnings = FALSE)
  
  # electrodes data
  nums_file = GUI_data$elec_file
  nums_file = gsub("\\\\", "/", nums_file)
  nums_file = basename(nums_file)
  
  # handle case where the first sheet is empty
  sheets <- excel_sheets(c(data_file))
  start = 1
  if (all(sheets[1] == "Sheet1")) {
    actual_sheets = sheets[2:length(sheets)]
    start = 2
  } else {
    actual_sheets = sheets[1:length(sheets)]
  }
  
  # create "colors_hex.csv"
  condition = unlist(actual_sheets) # default value
  is_condition = "F"
  
  colors = c("red", "orange", "green", "cyan", "blue", "purple", "pink", "gray") # default value
  if (!is.na(GUI_data$stat_colors)) {
    colors = as.list(strsplit(GUI_data$stat_colors, ",")[[1]])
  }
  colors = colors[1:length(condition)]
  
  df = cbind(condition = unlist(condition), color = unlist(colors))
  df = as.data.frame(df)
  write.csv(df, "colors_hex.csv") # in the working directory
  
  # now creates the other files
  # add time column
  init_file = openxlsx::read.xlsx(data_file, sheet = 2) # add the sheet
  time <- c(seq(from = 10/60, to = 10/60*ncol(init_file), by = 10/60))
  all_files <- list(as.data.frame(time))
  
  num_of_groups = length(sheets) - start + 1
  groups_names = list()
  
  # loop through all the sheets
  # create the df
  for  (i in start:length(sheets)) {
    curr_file = openxlsx::read.xlsx(data_file, sheets[i])
    curr_file = as.data.frame(t(curr_file))
    
    # find the numbers to the row of names
    till <- ncol(curr_file) + 1
    nums = read_excel(nums_file, sheet = "Electrodes", range = anchored("A1", dim = c(till, 1)))
    nums <- t(nums)
    # if there is new names of groups (entered in the GUI), update the data
    if (is_condition == "T") {
      nums <- paste0(as.character(condition[i-1]), "-",(nums+1)/2)
    } else {
      nums <- paste0(sheets[i], "-",(nums+1)/2)
    }
    names(curr_file) <- c(nums)
    
    # wrap up
    all_files <- cbind(all_files, curr_file)
  }
  
  # export with zeros cols and NA
  name = paste0("organized_", first_file_name, ".xlsx")
  write_xlsx(all_files, path = paste0(first_path, "/", name))
  
  # export without NA
  all_files <- all_files[, !grepl('NA', colnames(all_files)) > 0]
  name = paste0("organized_without_zeros_", tools::file_path_sans_ext(data_file), ".xlsx")
  write_xlsx(all_files, path = paste0(first_path, "/", name))
  
  # if there is columns that contain only zero,
  # they need to be removed from all of the groups
  # find indexes
  sums_arr <- colSums(all_files)
  places = c()
  for (i in 1:length(sums_arr)) {
    if (sums_arr[i] == 0) {
      places <- append(places, i)
    }
  }
  # calc which cols should be removed
  items_in_group = till-1
  if (length(places) > 0) {
    div = c(mod(places, items_in_group))
    
    to_remove = c()
    for (num in div) {
      res = num - items_in_group
      while (res + items_in_group <= items_in_group*num_of_groups) {
        res = res + items_in_group
        to_remove <- append(to_remove, res)
      }
      res = 0
    }
    # remove
    all_files <- all_files[,-to_remove]
    
    # export without zeros cols in every group
    write_xlsx(all_files, path = paste0(first_path, "/", name))
  }
  
  print("ORGANIZER - DONE")
}

# ________________________________________________
# 2. Jennifer's statistic: do statistics and plots.
# combination of two programs- led or no led.
# need: A. input file organized_file or organized_file_without_zeros
#       B. "colors_hex.csv"
# create: Jennifer directory with many files inside


if (GUI_data$to_do_org == TRUE) {
  # first, check if input data contain led to know which code to activate
  capital = str_detect(sheets, "LED")
  small = str_detect(sheets, "led")
  both = str_detect(sheets, "Led")
  if (any(capital == "TRUE") || any(small == "TRUE") || any(both == "TRUE")) {
    # there is led - go to code_logistic_regression_no_agar
    
    # parameters:
    #------------
    file = paste0('pipeline/', first_file_name, '/', name) # the output of the last code
    
    # not suppose to happened but anyway
    if (is.na(file)) {
      file = GUI_data$org_file
      file = gsub("\\\\", "/", file)
      file = basename(file)
    }
    
    # remove outliers
    remove.outliers <- T
    
    # load colors (CSV format, hexadecimals or RGB)
    colors <- read.csv('colors_hex.csv')
    if("X" %in% colnames(colors)) {
      colors = select(colors, -X)
    }
    
    if(ncol(colors)>2) #if RGB - convert to hexadecimal values
      colors$color <- rgb(colors$R,colors$G, colors$B, maxColorValue=255)
    
    # load data
    df <- as.data.frame(read_excel(file))
    colnames(df) <- gsub('Naֳ¯ve', 'Naive', colnames(df))
    
    # retrieve file name
    file.name <- substr(file, 1, nchar(file)-5)
    
    # create output directory
    dir.create(file.path(paste0(first_path, '/Jennifer/', '/plots/')),recursive=TRUE, showWarnings = FALSE)
    
    # change to long format
    colnames(df)[1] <- 'time'
    df2 <- melt(df, id.vars = c('time'))
    df2 <- df2[!is.na(df2$value),] # remove missing values
    
    # retrieve fly ID - save in 'rep'
    tmp <- strsplit(as.character(df2$variable), '-')
    df2$rep <- as.factor(sapply(tmp, function(x) x[[2]]))
    
    # retrieve condition and agar
    tmp <- strsplit(sapply(tmp, function(x) x[[1]]),'_')
    df2$condition <- as.factor(sapply(tmp, function(x) x[[1]]))
    df2$agar <- as.factor(sapply(tmp, function(x) x[[2]]))
    df2$sample <- as.factor(paste0(df2$condition, '_', df2$rep))
    
    
    # plot mean values for each group with original data and save plot
    m <- ddply(df2, ~time+condition+agar, summarize, med=median(value), mean=mean(value)) # calculate mean values
    p<-ggplot(data=df2, aes(x=time, y=value, group=variable)) +
      geom_line() +
      geom_line(data=m, aes(x=time, y=mean, group=condition), color='red', size=1.2) +
      facet_grid(agar~condition)
    pdf(paste0(first_path, '/Jennifer', '/plot_all_samp.pdf'), width = 7, height = 4)
    print(p)
    dev.off()
    
    # perform logistic regression for each replicate (fly)  
    samp <- unique(df2$variable)
    res <- data.frame(condition='', agar='', sample=as.character(samp), sample2=as.character(samp), Asym=0, xmid=0, scal=0, stringsAsFactors = F)
    for(i in 1:length(samp)){
      tmp.data <- df2[df2$variable==samp[i],]
      res[i, 'condition'] <- as.character(tmp.data[1,'condition'])
      res[i, 'agar'] <- as.character(tmp.data[1,'agar'])
      res[i, 'sample'] <- as.character(tmp.data[1,'sample'])
      
      tryCatch({
        print(i)
        # regression
        model <-nls(value~SSlogis(time, Asym, xmid, scal), data=tmp.data)
        
        # save model results
        res[i, 'Asym'] <- coef(model)[1]
        res[i, 'xmid'] <- coef(model)[2]
        res[i, 'scal'] <- coef(model)[3]
        
        # plot and save data and regression curves
        tiff(paste0(first_path, '/Jennifer', '/plots/good_plot_', samp[i], '.tiff'), width = 700, height = 550)
        plot(tmp.data$time,tmp.data$value)
        lines(tmp.data$time,predict(model))
        title(paste0(samp[i], ', ID: ', i))
        dev.off()
        
      }, error=function(e){
        # in case model failed to converge
        cat("BAD SAMPLE!!!!\n")
        tiff(paste0(first_path, '/Jennifer', '/plots/bad_plot_', samp[i], '.tiff'), width = 700, height = 550)
        plot(tmp.data$time,tmp.data$value)
        title(paste0(samp[i], ', ID: ', i))
        dev.off()
      })
    }
    
    # remove bad samples (model failed to converge) and mark them as outliers
    res$out <- 0
    res[res$Asym==0,'out'] <- 1
    res2 <- res[res$Asym!=0, ]
    
    # remove outliers (based on IQR criterion)
    ### ONLY IF FLAG IS TRUE!!! ###
    if(remove.outliers){
      tmp <- boxplot(res2$Asym~res2$condition, plot=FALSE)$out
      res2[res2$Asym%in%tmp,'out'] <- 1
      tmp <- boxplot(res2$xmid~res2$condition, plot=FALSE)$out
      res2[res2$xmid%in%tmp,'out'] <- 1
      tmp <- boxplot(res2$scal~res2$condition, plot=FALSE)$out
      res2[res2$scal%in%tmp,'out'] <- 1
    }
    
    # save parameters - with OUTLIERS
    res[res$sample%in%res2[res2$out==1, 'sample'],'out'] <- 1
    write.table(res,paste0(first_path, '/Jennifer', '/param_table.csv'), sep=',', row.names = F)
    res2 <- res2[!res2$out,]
    
    # remove bad samples from original data
    df2 <- df2[which(df2$variable%in%res[res$out==0, 'sample2']),]
    df2 <- df2[which(df2$variable%in%res2[res2$out==0, 'sample2']),]
    
    # plot  mean values
    m <- ddply(df2, ~condition+agar+time, summarize, med=median(value), mean=mean(value), sd=sd(value), n=length(value) )
    m$se <- m$sd/sqrt(m$n)
    m$int <- as.factor(paste0(m$condition, '_', m$agar))
    m$int <- factor(m$int, levels=colors$condition)
    
    # with ribbons (SE)
    p<-ggplot(data=m, aes(x=time, y=mean, color=int)) +
      geom_line(size=1.5)+
      scale_color_manual(values=colors$color)+
      scale_fill_manual(values=colors$color)+
      geom_ribbon(aes(ymin = mean-se, ymax = mean+se,fill = int),linetype=0, alpha=.3)+
      theme_classic()+
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            axis.title.x = element_text(size=20),
            legend.text =element_text(size=15),
            legend.title = element_blank())+
      ylab('Cumulative Number\nof Sips')+
      xlab('Time (min)')
    pdf(paste0(first_path, '/Jennifer', '/plot_mean_samp_with_SE.pdf'), width = 7, height = 4)
    print(p)
    dev.off()
    
    # without ribbons (SE)
    p<-ggplot(data=m, aes(x=time, y=mean, color=int)) +
      geom_line(size=1.5)+
      scale_color_manual(values=colors$color)+
      scale_fill_manual(values=colors$color)+
      theme_classic()+
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            axis.title.x = element_text(size=20),
            legend.text =element_text(size=15),
            legend.title = element_blank())+
      ylab('Cumulative Number\nof Sips')+
      xlab('Time (min)')
    pdf(paste0(first_path, '/Jennifer', '/plot_mean_samp_without_SE.pdf'), width = 7, height = 4)
    print(p)
    dev.off()
    
    ###################################################
    # plot parameter distributions
    #-------------------------------------------------------
    
    res2$int <- as.factor(paste0(res2$condition, '_', res2$agar))
    res2$int <- factor(res2$int, levels=colors$condition)
    
    # Asym
    p<- ggplot(res2, aes(x=int, y=Asym, fill=int))+
      geom_boxplot(width=0.3)+
      xlab('')+  
      theme_bw()+
      scale_fill_manual(values=colors$color)+
      theme(axis.text.x = element_text(size=15, angle=45, hjust=1),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            legend.position = 'none')
    pdf(paste0(first_path, '/Jennifer', '/plot_Asym_distibution.pdf'), width = 5, height = 4)
    print(p)
    dev.off()
    
    # xmid
    p<- ggplot(res2, aes(x=int, y=xmid, fill=int))+
      geom_boxplot(width=0.3)+
      xlab('')+  
      theme_bw()+
      scale_fill_manual(values=colors$color)+
      theme(axis.text.x = element_text(size=15, angle=45, hjust=1),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            legend.position = 'none')
    pdf(paste0(first_path, '/Jennifer', '/plot_xmid_distibution.pdf'), width = 5, height = 4)
    print(p)
    dev.off()
    
    # scal
    p<- ggplot(res2, aes(x=int, y=scal, fill=int))+
      geom_boxplot(width=0.3)+
      xlab('')+  
      theme_bw()+
      scale_fill_manual(values=colors$color)+
      theme(axis.text.x = element_text(size=15, angle=45, hjust=1),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            legend.position = 'none')
    pdf(paste0(first_path, '/Jennifer', '/plot_scal_distibution.pdf'), width = 5, height = 4)
    print(p)
    dev.off()
    
    ###################################################
    # statistical tests
    #-------------------------------------------------------
    # Asym
    # mixed linear model
    model <- lmer(Asym~condition*agar+(1|sample), res2)
    # 2-way ANOVA
    a1<-anova(model)
    a1 <- round(a1, digits=4)
    a1 <- cbind(Asym=rownames(a1), a1)
    write.table(a1, paste0(first_path, '/Jennifer', '/2way_anova.csv'), sep=',', row.names=F)
    # post-hoc - pairwise comparisons
    emm1 <- emmeans(model, ~condition*agar)
    con1 <- as.data.frame(contrast(emm1, "pairwise", simple="each", combine=TRUE, adjust='fdr'))
    # post hoc - test difference of differences
    con1.diff <- as.data.frame(contrast(emm1, interaction = "pairwise", by = NULL))
    colnames(con1.diff)[1] <- 'condition'
    colnames(con1.diff)[2] <- 'agar'
    con1.diff <- cbind(con1.diff[,c(2,1)], contrast='.', con1.diff[,3:ncol(con1.diff)])
    all.con1 <- cbind(param='Asym', rbind(con1, con1.diff))
    
    # xmid
    # mixed linear model
    model <- lmer(xmid~condition*agar+(1|sample), res2)
    summary(model)
    # 2-way ANOVA
    a2<-anova(model)
    a2 <- round(a2, digits=4)
    a2 <- cbind(xmid=rownames(a2), a2)
    write.table(a2, paste0(first_path, '/Jennifer', '/2way_anova.csv'), sep=',', row.names=F, append = T)
    # post-hoc - pairwise comparisons
    emm2 <- emmeans(model, ~condition*agar)
    con2 <- as.data.frame(contrast(emm2, "pairwise", simple="each", combine=TRUE, adjust='fdr'))
    # post hoc - test difference of differences
    con2.diff <- as.data.frame(contrast(emm2, interaction = "pairwise", by = NULL))
    colnames(con2.diff)[1] <- 'condition'
    colnames(con2.diff)[2] <- 'agar'
    con2.diff <- cbind(con2.diff[,c(2,1)], contrast='.', con2.diff[,3:ncol(con2.diff)])
    all.con2 <- cbind(param='xmid', rbind(con2, con2.diff))
    
    # scal
    # mixed linear model
    model <- lmer(scal~condition*agar+(1|sample), res2)
    summary(model)
    # 2-way ANOVA
    a3<-anova(model)
    a3 <- round(a3, digits=4)
    a3 <- cbind(scal=rownames(a3), a3)
    write.table(a3, paste0(first_path, '/Jennifer', '/2way_anova.csv'), sep=',', row.names=F, append = T)
    # post-hoc - pairwise comparisons
    emm3 <- emmeans(model, ~condition*agar)
    con3 <- as.data.frame(contrast(emm3, "pairwise", simple="each", combine=TRUE, adjust='fdr'))
    # post hoc - test difference of differences
    con3.diff <- as.data.frame(contrast(emm3, interaction = "pairwise", by = NULL))
    colnames(con3.diff)[1] <- 'condition'
    colnames(con3.diff)[2] <- 'agar'
    con3.diff <- cbind(con3.diff[,c(2,1)], contrast='.', con3.diff[,3:ncol(con3.diff)])
    all.con3 <- cbind(param='scal', rbind(con3, con3.diff))
    
    # bind all results
    all.con <- rbind(all.con1, all.con2, all.con3)
    all.con[5:ncol(all.con)] <- round(all.con[5:ncol(all.con)], digits=4)
    
    # add significance asterisks
    all.con$sig <- ''
    all.con[all.con$p.value<0.05, 'sig']<- '*'
    all.con[all.con$p.value<0.01, 'sig']<- '**'
    all.con[all.con$p.value<0.001, 'sig']<- '***'
    
    # save post hoc results
    write.table(all.con, paste0(first_path, '/Jennifer', '/post_hoc.csv'), sep=',', row.names=F)
    print("JENNIFER'S CODE DONE")  
    
    
  } else { # there is no led - go to code_logistic_regression
    
    # parameters:
    #------------
    file = paste0('pipeline/', first_file_name, '/', name) # the output of the last code
    
    # remove outliers
    remove.outliers <- T
    
    # load colors (CSV format, hexadecimals or RGB)
    colors <- read.csv('colors_hex.csv')
    if("X" %in% colnames(colors)) {
      colors = select(colors, -X)
    }
    
    if(ncol(colors)>2) #if RGB - convert to hexadecimal values
      colors$color <- rgb(colors$R,colors$G, colors$B, maxColorValue=255)
    
    # load data
    df <- as.data.frame(read_excel(file))
    colnames(df) <- gsub('Naֳ¯ve', 'Naive', colnames(df))
    
    # retrieve file name
    file.name <- substr(file, 1, nchar(file)-5)
    
    # create output directory
    dir.create(file.path(paste0(first_path, '/Jennifer', '/plots/')),recursive=TRUE, showWarnings = FALSE)
    
    # change to long format
    colnames(df)[1] <- 'time'
    df2 <- melt(df, id.vars = c('time'))
    df2 <- df2[!is.na(df2$value),]
    
    # retrieve fly ID - save in 'rep'
    tmp <- strsplit(as.character(df2$variable), '-')
    df2$rep <- as.factor(sapply(tmp, function(x) x[[2]]))
    
    # retrieve conditions
    tmp <- strsplit(sapply(tmp, function(x) x[[1]]),'_')
    df2$condition <- as.factor(sapply(tmp, function(x) x[[1]]))
    
    # plot mean values for each group with original data and save plot
    m <- ddply(df2, ~time+condition, summarize, med=median(value), mean=mean(value)) # calculate mean values
    p<-ggplot(data=df2, aes(x=time, y=value, group=variable)) +
      geom_line() +
      geom_line(data=m, aes(x=time, y=mean, group=condition), color='red', size=1.2) +
      facet_grid(~condition)
    
    pdf(paste0(first_path, '/Jennifer', '/plot_all_samp.pdf'), width = 7, height = 4)
    print(p)
    dev.off()
    
    
    # perform logistic regression for each replicate (fly)  
    samp <- unique(df2$variable)
    res <- data.frame(condition='', sample=as.character(samp), Asym=0, xmid=0, scal=0, stringsAsFactors = F)
    for(i in 1:length(samp)){
      tmp.data <- df2[df2$variable==samp[i],]
      res[i, 'condition'] <- as.character(tmp.data[1,'condition'])
      res[i, 'sample'] <- as.character(tmp.data[1,'variable'])
      
      tryCatch({
        print(i)
        # regression
        model <-nls(value~SSlogis(time, Asym, xmid, scal), data=tmp.data)
        
        # save model results
        res[i, 'Asym'] <- coef(model)[1]
        res[i, 'xmid'] <- coef(model)[2]
        res[i, 'scal'] <- coef(model)[3]
        
        # plot and save data and regression curves
        tiff(paste0(first_path, '/Jennifer', '/plots/good_plot_', samp[i], '.tiff'), width = 700, height = 550)
        plot(tmp.data$time,tmp.data$value)
        lines(tmp.data$time,predict(model))
        title(paste0(samp[i], ', ID: ', i))
        dev.off()
        
      }, error=function(e){
        # in case model failed to converge
        cat("BAD SAMPLE!!!!\n")
        tiff(paste0(first_path, '/Jennifer', '/plots/bad_plot_', samp[i], '.tiff'), width = 700, height = 550)
        plot(tmp.data$time,tmp.data$value)
        title(paste0(samp[i], ', ID: ', i))
        dev.off()
      })
    }
    
    # remove bad samples (model failed to converge) and mark them as outliers
    res$out <- 0
    res[res$Asym==0,'out'] <- 1
    res2 <- res[res$Asym!=0,]
    
    # remove outliers (based on IQR criterion)
    ### ONLY IF FLAG IS TRUE!!! ###
    if(remove.outliers){
      tmp <- boxplot(res2$Asym~res2$condition, plot=FALSE)$out
      res2[res2$Asym%in%tmp,'out'] <- 1
      tmp <- boxplot(res2$xmid~res2$condition, plot=FALSE)$out
      res2[res2$xmid%in%tmp,'out'] <- 1
      tmp <- boxplot(res2$scal~res2$condition, plot=FALSE)$out
      res2[res2$scal%in%tmp,'out'] <- 1
    }
    
    # save parameters - with OUTLIERS
    res[res$sample%in%res2[res2$out==1, 'sample'],'out'] <- 1
    write.table(res,paste0(first_path, '/Jennifer', '/param_table.csv'), sep=',', row.names = F)
    res2 <- res2[!res2$out,]
    
    # remove bad samples from original data
    df2 <- df2[which(df2$variable%in%res[res$out==0, 'sample']),]
    df2 <- df2[which(df2$variable%in%res2[res2$out==0, 'sample']),]
    
    # plot  mean values
    m <- ddply(df2, ~time+condition, summarize, med=median(value), mean=mean(value), sd=sd(value), n=length(value) )
    m$se <- m$sd/sqrt(m$n)
    m$condition <- factor(m$condition, levels=colors$condition)
    
    # with ribbons (SE)
    p<-ggplot(data=m, aes(x=time, y=mean, color=condition)) +
      geom_line(size=1.5)+
      scale_color_manual(values=colors$color)+
      scale_fill_manual(values=colors$color)+
      geom_ribbon(aes(ymin = mean-se, ymax = mean+se,fill = condition),linetype=0, alpha=.3)+
      theme_classic()+
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            axis.title.x = element_text(size=20),
            legend.text =element_text(size=15),
            legend.title = element_blank())+
      ylab('Cumulative Number\nof Sips')+
      xlab('Time (min)')
    pdf(paste0(first_path, '/Jennifer', '/plot_mean_samp_with_SE.pdf'), width = 7, height = 4)
    print(p)
    dev.off()
    
    # without ribbons (SE)
    p<-ggplot(data=m, aes(x=time, y=mean, color=condition)) +
      geom_line(size=1.5)+
      scale_color_manual(values=colors$color)+
      scale_fill_manual(values=colors$color)+
      theme_classic()+
      scale_y_continuous(expand=c(0,0))+
      scale_x_continuous(expand=c(0,0))+
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            axis.title.x = element_text(size=20),
            legend.text =element_text(size=15),
            legend.title = element_blank())+
      ylab('Cumulative Number\nof Sips')+
      xlab('Time (min)')
    pdf(paste0(first_path, '/Jennifer', '/plot_mean_samp_without_SE.pdf'), width = 7, height = 4)
    print(p)
    dev.off()
    
    ###################################################
    # plot parameter distributions
    #-------------------------------------------------------
    
    # Asym
    p<- ggplot(res2, aes(x=condition, y=Asym, fill=condition))+
      geom_boxplot(width=0.3)+
      xlab('')+  
      theme_bw()+
      scale_fill_manual(values=colors$color)+
      theme(axis.text.x = element_text(size=15, angle=45, hjust=1),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            legend.position = 'none')
    pdf(paste0(first_path, '/Jennifer', '/plot_Asym_distibution.pdf'), width = 5, height = 4)
    print(p)
    dev.off()
    
    # xmid
    p<- ggplot(res2, aes(x=condition, y=xmid, fill=condition))+
      geom_boxplot(width=0.3)+
      xlab('')+  
      theme_bw()+
      scale_fill_manual(values=colors$color)+
      theme(axis.text.x = element_text(size=15, angle=45, hjust=1),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            legend.position = 'none')
    pdf(paste0(first_path, '/Jennifer', '/plot_xmid_distibution.pdf'), width = 5, height = 4)
    print(p)
    dev.off()
    
    # scal
    p<- ggplot(res2, aes(x=condition, y=scal, fill=condition))+
      geom_boxplot(width=0.3)+
      xlab('')+  
      theme_bw()+
      scale_fill_manual(values=colors$color)+
      theme(axis.text.x = element_text(size=15, angle=45, hjust=1),
            axis.text.y = element_text(size=15),
            axis.title.y = element_text(size=20),
            legend.position = 'none')
    pdf(paste0(first_path, '/Jennifer', '/plot_scal_distibution.pdf'), width = 5, height = 4)
    print(p)
    dev.off()
    
    ###################################################
    # statistical tests
    #-------------------------------------------------------
    # Asym
    # linear model
    model <- lm(Asym~condition, res2)
    # 1-way ANOVA
    a1 <- anova(model)
    a1 <- round(a1, digits=4)
    a1 <- cbind(Asym=rownames(a1), a1)
    write.table(a1, paste0(first_path, '/Jennifer', '/1way_anova.csv'), sep=',', row.names=F)
    # post-hoc
    emm1 <- emmeans(model, ~condition)
    con1 <- as.data.frame(contrast(emm1, "pairwise", simple="each", combine=TRUE, adjust='fdr'))
    colnames(con1)[1] <- 'condition'
    all.con1 <- cbind(param='Asym',con1)
    
    # xmid
    # linear model
    model <- lm(xmid~condition, res2)
    summary(model)
    # 1-way ANOVA
    a2<-anova(model)
    a2 <- round(a2, digits=4)
    a2 <- cbind(xmid=rownames(a2), a2)
    write.table(a2, paste0(first_path, '/Jennifer', '/1way_anova.csv'), sep=',', row.names=F, append = T)
    # post-hoc
    emm2 <- emmeans(model, ~condition)
    con2 <- as.data.frame(contrast(emm2, "pairwise", simple="each", combine=TRUE, adjust='fdr'))
    colnames(con2)[1] <- 'condition'
    all.con2 <- cbind(param='xmid', con2)
    
    # scal
    # linear model
    model <- lm(scal~condition, res2)
    summary(model)
    # 1-way ANOVA
    a3 <-anova(model)
    a3 <- round(a3, digits=4)
    a3 <- cbind(scal=rownames(a3), a3)
    write.table(a3, paste0(first_path, '/Jennifer', '/1way_anova.csv'), sep=',', row.names=F, append = T)
    # post-hoc
    emm3 <- emmeans(model, ~condition)
    con3 <- as.data.frame(contrast(emm3, "pairwise", simple="each", combine=TRUE, adjust='fdr'))
    colnames(con3)[1] <- 'condition'
    all.con3 <- cbind(param='scal',con3)
    
    # bind all results
    all.con <- rbind(all.con1, all.con2, all.con3)
    all.con[3:ncol(all.con)] <- round(all.con[3:ncol(all.con)], digits=4)
    
    # add significance asterisks
    all.con$sig <- ''
    all.con[all.con$p.value<0.05, 'sig']<- '*'
    all.con[all.con$p.value<0.01, 'sig']<- '**'
    all.con[all.con$p.value<0.001, 'sig']<- '***'
    
    # save post hoc results
    write.table(all.con, paste0(first_path, '/Jennifer', '/post_hoc.csv'), sep=',', row.names=F)
    print("JENNIFER'S CODE DONE")
  }
}



# _______________________________________________
# 3. statistic: do another statistic tests - first sips, then PI.
# need: DataInExcelFormat.xls format (could be the same as the previous or another)
# create: A. sips_file_name.csv
#         B. PI_file_name.csv
# _______________________________________________

# stat for sips

# enter name of file and sheet here
second_file = GUI_data$input_path
second_file = gsub("\\\\", "/", second_file)
second_file = basename(second_file)
GUI_data$input_path = second_file

data = read_excel(second_file, sheet = "NumberOfSips")
num_of_groups = ncol(data)/2 # note - assume there is always two from every group
titles = names(data)

# create path
second_file_name = tools::file_path_sans_ext(second_file)
second_path = paste0('pipeline/', second_file_name)
dir.create(file.path(second_path),recursive=TRUE, showWarnings = FALSE)

df = list()
is_there_not_normal = 0
data = combn(data, 2, simplify=FALSE)

# looping on the cols
# check if there even one not normal - then all not normal
for (loop_i in 1:length(data)) {
  curr = data[loop_i]
  unstacked = curr
  curr = stack(data.frame(curr))
  
  shapiro = shapiro.test(curr$values)
  p_value = shapiro$p.value
  if (shapiro$p.value >= 0.05) {
    is_there_not_normal = 1
  }
}

# NORMAL
if (is_there_not_normal == 0) {
  for (loop_i in 1:length(data)) {
    curr = data[loop_i]
    unstacked = curr
    curr = stack(data.frame(curr))
    groups = paste(names(data[[loop_i]])[1],"-", names(data[[loop_i]])[2])
    
    # two groups
    if (num_of_groups == 2) {
      # shapiro
      shapiro = shapiro.test(curr$values)
      p_value = shapiro$p.value
      name_of_test =shapiro$method
      W = shapiro$statistic[[1]]
      first_test = cbind(name_of_test, p_value, W)
      
      # t test
      is_normal = "normal"
      second_test = t.test(curr$values)
      second_test_name = second_test$method
      t = second_test[["statistic"]][[1]]
      DF_parameter = second_test[["parameter"]][[1]]
      p.value = second_test[["p.value"]]
      conf.int.1 = second_test[["conf.int"]][1]
      conf.int.2 = second_test[["conf.int"]][2]
      mean_of_x = second_test[["estimate"]][[1]]
      mean = second_test[["null.value"]][[1]]
      stderr = second_test[["stderr"]]
      second_test = cbind(second_test_name, t, DF_parameter, p.value, conf.int.1, conf.int.2, mean_of_x, mean, stderr)
      
      # wrap up
      final = data.frame(groups, first_test, is_normal, second_test)
      df = rbind(df, final)
    }
    
    # 3 or more groups
    else {
      # anova
      first_test = "anova"
      anova = aov(formula = curr$values ~ curr$ind)
      raw_anova = anova
      anova = summary(anova)
      shapiro = shapiro.test(curr$values)
      p_value = shapiro$p.value
      
      # TukeyHSD
      is_normal = "normal"
      second_test_name = "TukeyHSD"
      second_test = TukeyHSD(raw_anova)
      confidence_level = capture.output(second_test)[2]
      p_adj = second_test$`curr$ind`[,"p adj"]
      diff = second_test$`curr$ind`[,"diff"]
      lwr = second_test$`curr$ind`[,"lwr"]
      upr = second_test$`curr$ind`[,"upr"]
      second_test_to_write =cbind(confidence_level, p_adj, diff, lwr, upr)
      
      # anova
      DF = anova[[1]][["Df"]][1]
      Sum_Sq = anova[[1]][["Sum Sq"]][1]
      F_value = anova[[1]][["Mean Sq"]][1]
      Mean_Sq = anova[[1]][["F value"]][1]
      Pr_largger_then_F = anova[[1]][["Pr(>F)"]][1]
      
      Residuals_DF = anova[[1]][["Df"]][2]
      Residuals_Sum_Sq = anova[[1]][["Sum Sq"]][2]
      Residuals_F_value = anova[[1]][["Mean Sq"]][2]
      anova_to_write = cbind(Pr_largger_then_F, F_value, Mean_Sq, Sum_Sq, DF, Residuals_F_value, Residuals_Sum_Sq, Residuals_DF)
      
      # wrap up
      final = data.frame(groups, p_value, first_test, anova_to_write, is_normal, second_test_name, second_test_to_write)
      df = rbind(df, final)
    }
    
    name = paste0(second_path, "/sips_normal_", second_file_name, ".csv")
    write.csv(df, name)
    print("STATISTIC SIPS - DONE")
  }
}

# NOT NORMAL
if (is_there_not_normal == 1) {
  for (loop_i in 1:length(data)) {
    curr = data[loop_i]
    unstacked = curr
    curr = stack(data.frame(curr))
    groups = paste(names(data[[loop_i]])[1],"-", names(data[[loop_i]])[2])
    
    # two groups
    if (num_of_groups == 2) {
      
      # shepiro
      shapiro = shapiro.test(curr$values)
      name_of_test =shapiro$method
      p_value = shapiro$p.value
      W = shapiro$statistic[[1]]
      first_test = cbind(name_of_test, p_value, W)
      
      # wilcox
      is_normal = "not normal"
      second_test = wilcox.test(curr$values)
      second_test_name = second_test$method
      V = second_test$statistic[[1]]
      p.value = second_test$p.value
      second_test = cbind(second_test_name, V, p.value)
      
      # wrap up
      final = data.frame(groups, first_test, is_normal, second_test)
      df = rbind(df, final)
    }
    
    # 3 or more groups
    else {
      first_test = "anova"
      anova = aov(formula = curr$values ~ curr$ind)
      raw_anova = anova
      anova = summary(anova)
      shapiro = shapiro.test(curr$values)
      p_value = shapiro$p.value
      
      # kruskal
      is_normal = "not normal"
      kruskal = kruskal.test(curr$values ~ curr$ind)
      second_test_name = kruskal$method
      Kruskal_Wallis_chi_squared = kruskal$statistic[[1]]
      Kruskal_DF = kruskal$parameter[[1]]
      kruskal_p_value = kruskal$p.value
      second_test_to_write =cbind(Kruskal_Wallis_chi_squared, Kruskal_DF, kruskal_p_value)
      
      # anova
      DF = anova[[1]][["Df"]][1]
      Sum_Sq = anova[[1]][["Sum Sq"]][1]
      F_value = anova[[1]][["Mean Sq"]][1]
      Mean_Sq = anova[[1]][["F value"]][1]
      Pr_largger_then_F = anova[[1]][["Pr(>F)"]][1]
      
      Residuals_DF = anova[[1]][["Df"]][2]
      Residuals_Sum_Sq = anova[[1]][["Sum Sq"]][2]
      Residuals_F_value = anova[[1]][["Mean Sq"]][2]
      anova_to_write = cbind(Pr_largger_then_F, F_value, Mean_Sq, Sum_Sq, DF, Residuals_F_value, Residuals_Sum_Sq, Residuals_DF)
      
      # wrap up
      final = data.frame(groups, p_value, first_test, anova_to_write, is_normal, second_test_name, second_test_to_write)
      df = bind_rows(df, final)
    }
    
    name = paste0(second_path, "/sips_not_normal_", second_file_name, ".csv")
    write.csv(df, name)
    print("STATISTIC SIPS - DONE")
  }
}


# stat for PI

data = read_excel(second_file, sheet = "NumberOfSips")
num_of_groups = ncol(data)/2 # note - assume there is always two from every group

# calculate preference
for (i in 1:num_of_groups) {
  preference = (data[i] - data[i+num_of_groups])/(data[i] + data[i+num_of_groups])
  if (i == 1) { # add the first
    mat = data.frame(preference)
  } else { # add the rest
    mat <- cbind(mat, preference)
  }
}
data = mat
mat <- stack(mat)

df = list()
is_there_not_normal = 0
data = combn(data, 2, simplify=FALSE)

# looping on the cols
# check if there even one not normal - then all treated as not normal
for (loop_i in 1:length(data)) {
  curr = data[loop_i]
  unstacked = curr
  curr = stack(data.frame(curr))
  
  shapiro = shapiro.test(curr$values)
  p_value = shapiro$p.value
  if (shapiro$p.value >= 0.05) {
    is_there_not_normal = 1
  }
}

# NORMAL
if (is_there_not_normal == 0) {
  for (loop_i in 1:length(data)) {
    curr = data[loop_i]
    unstacked = curr
    curr = stack(data.frame(curr))
    groups = paste(names(data[[loop_i]])[1],"-", names(data[[loop_i]])[2])
    
    # two groups
    if (num_of_groups == 2) {
      # shapiro
      shapiro = shapiro.test(curr$values)
      p_value = shapiro$p.value
      name_of_test =shapiro$method
      W = shapiro$statistic[[1]]
      first_test = cbind(name_of_test, p_value, W)
      
      # t test
      is_normal = "normal"
      second_test = t.test(curr$values)
      second_test_name = second_test$method
      t = second_test[["statistic"]][[1]]
      DF_parameter = second_test[["parameter"]][[1]]
      p.value = second_test[["p.value"]]
      conf.int.1 = second_test[["conf.int"]][1]
      conf.int.2 = second_test[["conf.int"]][2]
      mean_of_x = second_test[["estimate"]][[1]]
      mean = second_test[["null.value"]][[1]]
      stderr = second_test[["stderr"]]
      second_test = cbind(second_test_name, t, DF_parameter, p.value, conf.int.1, conf.int.2, mean_of_x, mean, stderr)
      
      # wrap up
      final = data.frame(groups, first_test, is_normal, second_test)
      df = rbind(df, final)
    }
    
    # 3 or more groups
    else {
      # anova
      first_test = "anova"
      anova = aov(formula = curr$values ~ curr$ind)
      raw_anova = anova
      anova = summary(anova)
      shapiro = shapiro.test(curr$values)
      p_value = shapiro$p.value
      
      # TukeyHSD
      is_normal = "normal"
      second_test_name = "TukeyHSD"
      second_test = TukeyHSD(raw_anova)
      confidence_level = capture.output(second_test)[2]
      p_adj = second_test$`curr$ind`[,"p adj"]
      diff = second_test$`curr$ind`[,"diff"]
      lwr = second_test$`curr$ind`[,"lwr"]
      upr = second_test$`curr$ind`[,"upr"]
      second_test_to_write =cbind(confidence_level, p_adj, diff, lwr, upr)
      
      # anova
      DF = anova[[1]][["Df"]][1]
      Sum_Sq = anova[[1]][["Sum Sq"]][1]
      F_value = anova[[1]][["Mean Sq"]][1]
      Mean_Sq = anova[[1]][["F value"]][1]
      Pr_largger_then_F = anova[[1]][["Pr(>F)"]][1]
      
      Residuals_DF = anova[[1]][["Df"]][2]
      Residuals_Sum_Sq = anova[[1]][["Sum Sq"]][2]
      Residuals_F_value = anova[[1]][["Mean Sq"]][2]
      anova_to_write = cbind(Pr_largger_then_F, F_value, Mean_Sq, Sum_Sq, DF, Residuals_F_value, Residuals_Sum_Sq, Residuals_DF)
      
      # wrap up
      final = data.frame(groups, p_value, first_test, anova_to_write, is_normal, second_test_name, second_test_to_write)
      df = rbind(df, final)
    }
    
    name = paste0(second_path, "/PI_normal_", second_file_name, ".csv")
    write.csv(df, name)
    print("STATISTIC PI - DONE")
  }
}

# NOT NORMAL
if (is_there_not_normal == 1) {
  for (loop_i in 1:length(data)) {
    curr = data[loop_i]
    unstacked = curr
    curr = stack(data.frame(curr))
    groups = paste(names(data[[loop_i]])[1],"-", names(data[[loop_i]])[2])
    
    # two groups
    if (num_of_groups == 2) {
      
      # shepiro
      shapiro = shapiro.test(curr$values)
      name_of_test =shapiro$method
      p_value = shapiro$p.value
      W = shapiro$statistic[[1]]
      first_test = cbind(name_of_test, p_value, W)
      
      # wilcox
      is_normal = "not normal"
      second_test = wilcox.test(curr$values)
      second_test_name = second_test$method
      V = second_test$statistic[[1]]
      p.value = second_test$p.value
      second_test = cbind(second_test_name, V, p.value)
      
      # wrap up
      final = data.frame(groups, first_test, is_normal, second_test)
      df = rbind(df, final)
    }
    
    # 3 or more groups
    else {
      first_test = "anova"
      anova = aov(formula = curr$values ~ curr$ind)
      raw_anova = anova
      anova = summary(anova)
      shapiro = shapiro.test(curr$values)
      p_value = shapiro$p.value
      
      # kruskal
      is_normal = "not normal"
      kruskal = kruskal.test(curr$values ~ curr$ind)
      second_test_name = kruskal$method
      Kruskal_Wallis_chi_squared = kruskal$statistic[[1]]
      Kruskal_DF = kruskal$parameter[[1]]
      kruskal_p_value = kruskal$p.value
      second_test_to_write =cbind(Kruskal_Wallis_chi_squared, Kruskal_DF, kruskal_p_value)
      
      # anova
      DF = anova[[1]][["Df"]][1]
      Sum_Sq = anova[[1]][["Sum Sq"]][1]
      F_value = anova[[1]][["Mean Sq"]][1]
      Mean_Sq = anova[[1]][["F value"]][1]
      Pr_largger_then_F = anova[[1]][["Pr(>F)"]][1]
      
      Residuals_DF = anova[[1]][["Df"]][2]
      Residuals_Sum_Sq = anova[[1]][["Sum Sq"]][2]
      Residuals_F_value = anova[[1]][["Mean Sq"]][2]
      anova_to_write = cbind(Pr_largger_then_F, F_value, Mean_Sq, Sum_Sq, DF, Residuals_F_value, Residuals_Sum_Sq, Residuals_DF)
      
      # wrap up
      final = data.frame(groups, p_value, first_test, anova_to_write, is_normal, second_test_name, second_test_to_write)
      df = bind_rows(df, final)
    }
    
    name = paste0(second_path, "/PI_not_normal_", second_file_name, ".csv")
    write.csv(df, name)
    print("STATISTIC PI - DONE")
  }
}



# _______________________________________________________________
# 4. GUI: create plots.
# need: A. "matlab_to_R_GUI_data.xls"
#       B. file in DataInExcelFormat.xls format (use the previous)
# create: plots dir -> three plots in pdf or jpg: sips, multi, PI
# _______________________________________________________________

data = GUI_data
raw_data = read_excel(data$input_path, sheet = "NumberOfSips")
dir.create(file.path(paste0(second_path, '/plots/')),recursive=TRUE, showWarnings = FALSE)

# 1. _______ SIPS __________
sips_data = raw_data

# change x cols names if needed
if (!is.na(data$sips_cols_names)) { # check if empty
  names(sips_data) = unlist(strsplit(data$sips_cols_names, ","))
}

# split x columns names if needed
x_titles = names(sips_data)
x_cols_names = list()
for (i in 1:length(x_titles)) {
  if ( ( nchar(x_titles[i]) > 7 && data$sips_x_size > 12) || ( nchar(x_titles[i]) > 10 && data$sips_x_size > 10 ) ) {
    x_titles[i] = gsub('(.{1,10})(\\s|$)', '\\1\n', x_titles[i])
  }
  x_cols_names[[length(x_cols_names) + 1]] = x_titles[i]
}
names(sips_data) = as.character(x_cols_names)

# sort the groups
num_of_groups = ncol(sips_data)/2 # note - assume there is always two from every group
order = c()
for (i in 1:num_of_groups) {
  order = append(order, sips_data[i])
  order = append(order, sips_data[i + num_of_groups])
}

sips_data = order
sips_data = stack(sips_data)

# fill empty cells with zero
for (row in 1:nrow(sips_data)) {
  if (is.na(sips_data$values[row])) {
    sips_data$values[row] = 0  
  }
}

# colors
sips_colors = unlist(strsplit(data$sips_colors, ","))
jitters_colors = paste0(sips_colors, "4")

# check outliers
if (data$outliers == "FALSE") {
  outliers_color = "white"
  outliers_stroke = NA
} else {
  outliers_color = "black"
  outliers_stroke = 2
}

# check bold
if (data$sips_is_x_bold == "TRUE") { sips_x_bold = 2 } else { sips_x_bold = 1 }
if (data$sips_is_y_bold == "TRUE") { sips_y_bold = 2 } else { sips_y_bold = 1 }

# create the plot
sips_plot = qplot(data = sips_data, y = values, x = ind, xlab = "", ylab = "Number of Sips" ,geom = "boxplot", fill = ind) + # basic
  scale_fill_manual(values = sips_colors) +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(color = "white"),
        axis.line = element_line(color = "black")) + # add blank background
  theme(legend.position = "None") + # get rid from legend
  geom_boxplot(lwd=0.5) + stat_boxplot(geom = "errorbar", width = 0.2) + # add error bar
  geom_boxplot(outlier.color = outliers_color ,outlier.size = 0.5, outlier.stroke = outliers_stroke) +
  geom_jitter(aes(color = ind), size = 2) + # add jitters
  scale_color_manual(values = jitters_colors) +
  # text design
  theme(axis.text.x = element_text(size = data$sips_x_size, family = data$sips_x_font, face = sips_x_bold)) +
  theme(axis.text.y = element_text(size = data$sips_y_size, family = data$sips_y_font, face = sips_y_bold)) +
  theme(axis.title.y = element_text(size = data$sips_y_size, family = data$sips_y_font, face = sips_y_bold))
sips_plot

# 2. _____ MULTI SIPS _____

# handle the sheets
sheet <- excel_sheets(c(data$input_path))
start = which(sheet == "Electrodes") + 1
plots_in_row = data$graphs_in_line

list_of_plots <- list()
# loop through the sheets
for (i in start:length(sheet)) {
  curr_sheet = read_excel(data$input_path, sheet = sheet[i])
  names(curr_sheet) = x_titles # take the same as sips, before the split
  
  # 1. sort the groups
  num_of_groups = ncol(curr_sheet)/2 # note - assume there is always two from every group
  order = c()
  for (j in 1:num_of_groups) {
    order = append(order, curr_sheet[j])
    order = append(order, curr_sheet[j + num_of_groups])
  }
  multi_data = stack(order)
  
  # 2. fill empty cells with zero
  for (row in 1:nrow(multi_data)) {
    if (is.na(multi_data$values[row])) {
      multi_data$values[row] = 0  
    }
  }
  
  # 3. split y title if too big
  y_title = sheet[i]
  if (nchar(sheet[i]) > 20 && i - start < plots_in_row) {
    center = round(nchar(sheet[i]) / 2) + 1
    part_one = substr(sheet[i], 0, center)
    part_two = substr(sheet[i], center + 1, nchar(sheet[i]))
    y_title = paste(part_one, part_two, sep="\n")
  }  
  
  # 4. create the plot
  plot = qplot(data = multi_data, y = values, x = ind, xlab = "" , ylab = y_title, geom = "boxplot", fill = ind) +
    theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(color = "white"),
          axis.line = element_line(color = "black")) + # add blank background
    theme(legend.position = "None") + # get rid from legend
    scale_fill_discrete(l = 150) + geom_boxplot(lwd=0.5) + stat_boxplot(geom = "errorbar", width = 0.2) + # add error bar
    geom_boxplot(outlier.color = outliers_color ,outlier.size = 0.5, outlier.stroke = outliers_stroke) +
    geom_jitter(aes(color = ind), size = 0.5) + # add jitters  
    scale_color_manual(values = jitters_colors) +
    # add text design
    theme(axis.text.x = element_text(angle = 90)) +
    theme(axis.text.x = element_text(size = data$sips_x_size-3, family = data$sips_x_font, face = sips_x_bold)) +
    theme(axis.text.y = element_text(size = data$sips_y_size, family = data$sips_y_font, face = sips_y_bold)) +
    theme(axis.title.y = element_text(size = data$sips_y_size, family = data$sips_y_font, face = sips_y_bold)) +
    scale_fill_manual(values = sips_colors,
                      guide = guide_legend(reverse = TRUE))
  
  # add to the final list
  list_of_plots[[i-start+1]] = plot
}

multi_plot = do.call("grid.arrange", c(list_of_plots, ncol = plots_in_row))
multi_plot

# 3. ______ PI ______

# read data
PI_data = raw_data

# fill empty cells
for (row in 1:nrow(PI_data)) {
  for (col in 1:ncol(PI_data)) {
    if (is.na(PI_data[row, col])) {
      PI_data[row, col] = 0.00000000  
    }
  }
}

# calculate preference
num_of_groups = ncol(PI_data)/2 # note - assume there is always two from every group

# loop over columns
for(i in 1:num_of_groups) {
  preference = (PI_data[i] - PI_data[i+num_of_groups])/(PI_data[i] + PI_data[i+num_of_groups])
  if (i == 1) { # add the first
    mat = data.frame(preference)
  }
  else { # add the rest
    mat <- cbind(mat, preference)
  }
}

# take just the first word
names(mat) = word(names(mat)) # sep by space
names(mat) = word(names(mat), sep = fixed('.')) # sep by dot

if (!is.na(data$PI_cols_names)) { # check if empty
  names(mat) = unlist(strsplit(data$PI_cols_names, ","))
}

mat <- stack(mat)

# handle NA values
for (i in which(is.na(mat))) {
  mat$values[i] = 0.00000000  
}

# organize data
PI = mat$values
Groups = mat$ind

# prepare sub titles
sub_title_1 <- grobTree(textGrob(data$upper_subs, x=0.001,  y=0.95, hjust=0,gp=gpar(col="red", fontsize=10, fontface="italic")))
sub_title_2 <- grobTree(textGrob(data$lower_subs, x=0.001,  y=0.05, hjust=0,gp=gpar(col="blue", fontsize=10, fontface="italic")))

# calculations for error bars
mean.worm = tapply(mat$values, mat$ind, mean)
sd.worm = tapply(mat$values, mat$ind, sd)

PI_colors = unlist(strsplit(data$PI_colors, ","))

# bold checkBoxes
if (data$PI_is_x_bold == "TRUE") { PI_x_bold = 2 } else { PI_x_bold = 1 }
if (data$PI_is_y_bold == "TRUE") { PI_y_bold = 2 } else { PI_y_bold = 1 }

# create bar plot
PI_plot = ggplot(data = mat ,mapping = aes(x = Groups, y = PI)) +
  theme_bw() + ylim(-1.1, 1.1) + # basic
  stat_summary(fun.data=mean_sdl, geom="bar", fill = "white", color = "black") + # bars
  geom_hline(yintercept = 0, color = "black") + # zero line
  geom_jitter(aes(color = ind), size = 3, show.legend = FALSE) + # points
  stat_boxplot(geom = "errorbar", ymin = mean.worm-(sd.worm/sqrt(nrow(mat))),
               ymax = mean.worm+(sd.worm/sqrt(nrow(mat))), width = 0.3) + # errorbars
  annotation_custom(sub_title_1) + annotation_custom(sub_title_2) + # subtitles
  theme(axis.title.x=element_blank()) + # remove xlab
  scale_color_manual(values = PI_colors) +
  theme(axis.text.x = element_text(size = data$PI_x_size, family = data$PI_x_font, face = PI_x_bold)) +
  theme(axis.text.y = element_text(size = data$PI_y_size, family = data$PI_y_font, face = PI_y_bold)) +
  theme(axis.title.y = element_text(size = data$PI_y_size, family = data$PI_y_font, face = PI_y_bold))
PI_plot

# export all the 3 plots
plots_path = paste0(second_path, '/plots/')

if (data$output_path == "pdf") {
  name = paste0("sips_", second_file_name, ".pdf")
  ggsave(plot = sips_plot, name,  path = plots_path, width = 7, height = 5)
  name = paste0("multi_", second_file_name, ".pdf")
  ggsave(plot = multi_plot, name, path = plots_path, width = data$multi_width, height = data$multi_heigth)
  name = paste0("PI_", second_file_name, ".pdf")
  ggsave(plot = PI_plot, name, path =  plots_path, width = 5, height = 4)
}
if (data$output_path == "jpg") {
  name = paste0("sips_", second_file_name, ".jpg")
  ggsave(plot = sips_plot, name, path = plots_path, width = 7, height = 5)
  name = paste0("multi_", second_file_name, ".jpg")
  ggsave(plot = multi_plot, name, path = plots_path, width = data$multi_width, height = data$multi_heigth)
  name = paste0("PI_", second_file_name, ".jpg")
  ggsave(plot = PI_plot, name, path = plots_path, width = 5, height = 4)
}
print("PLOTS DONE")
print("_____ DONE ALL _____")

