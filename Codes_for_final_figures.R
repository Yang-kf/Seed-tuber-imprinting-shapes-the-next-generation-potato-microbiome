#R version 3.5.2 (2018-12-20) -- "Eggshell Igloo"
#this code make PCA plots for the final potato manuscript

library(qiime2R)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)


#Reading Metadata

metadata<-read.table(file="C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/metadata_second_generation_bac_withPBS.txt",header=T,sep = "\t")
dim(metadata)
head(metadata)



#define a function to visualize pca plots
visualize_pcoa <- function(id, qza_file, metadata_file, group_factor, output_file){
  
  braycurtis<-read_qza(qza_file)
  
  summary(braycurtis$data)
  braycurtis$data$ProportionExplained
  dim(braycurtis$data$ProportionExplained)
  round(braycurtis$data$ProportionExplained[1,2]*100)
  
  xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
  ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
  
  # Assign a unique ID to each plot
  p1_name <- paste0("p1_", id)
  p1e_name <- paste0("p1e_", id)
  
 pdf(output_file,height=3.5, width=5) 
 p1 <- braycurtis$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata_file) %>%
    ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`seedlot`)) +
    geom_point(alpha=0.9, size =5, stroke = 0.6) + 
    theme_q2r() + 
    scale_shape_manual(values=c(15,16,17,15,16,17)) + 
    #scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
    scale_color_manual(values=c("#ffb395","#fc8d62", "#ce7452")) + 
    theme(text=element_text(size = 15)) +
    xlab( xlab)+ylab( ylab)
 print(p1)
 
 # Assign the modified ellipse object names to the environment
 assign(p1_name, p1, envir = .GlobalEnv)
 
  # p1e <- p1 + 
  #   stat_ellipse(level = 0.68,linetype = 2,aes(group = metadata$group_factor),size=0.5,color="black")
  # print(p1e)
  p1e <- p1 + 
     stat_ellipse(level = 0.68,linetype = 2,aes(group = `seedlot`),size=0.5,color="black")#change group facter here
  print(p1e)

 # Assign the modified ellipse object names to the environment
  assign(p1e_name, p1e, envir = .GlobalEnv)
  
  dev.off()
}

#apply function to different data
id = "Mother_tuber" #only need to change here
group_factor = "variety"
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)

id = "Mother_tuber_Colomba" #only need to change here
group_factor = "seedlot"
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)

id = "Mother_tuber_Innovator" #only need to change here
group_factor = "seedlot"
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)

#group factor doesn't work for variety, change function into clustering by variety
id = "Mother_tuber_after_storage" #only need to change here
group_factor = "`variety`"
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)

#only redo big plots, reuse small plots for each genotype  
# id = "Mother_tuber_Colomba" #only need to change here
# group_factor = "seedlot"
# qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
# output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
# visualize_pcoa(id, qza_file, metadata, group_factor, output_file)
# 
# id = "Mother_tuber_Innovator" #only need to change here
# group_factor = "seedlot"
# qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
# output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
# visualize_pcoa(id, qza_file, metadata, group_factor, output_file)

id = "Daughter_tuber" #only need to change here
group_factor = "`variety`"
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)


id = "Root" #only need to change here
group_factor = "`variety`"
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,".pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)


###Fun data from here
setwd("C:/Postdoc/HZPC_project/Second_generation/Fun/Fun_MP_redo_final/Rvisualization")
load(".RData")
#Reading Metadata

metadata<-read.table(file="../metadata_Second_generation_fun_all.txt",header=T,sep = "\t")
id = "Daughter_tuber" #only need to change here

id = "Root" #only need to change here
id = "Mother_tuber" #only need to change here
id = "Mother_tuber_after_storage" #only need to change here
group_factor = "`variety`"
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Fun/Fun_MP_redo_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,"_fun.pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)

#had to redo small figures as well bc format doesn't match
id = "Daughter_tuber_Colomba"
id = "Daughter_tuber_Innovator"
id = "Mother_tuber_Colomba"
id = "Mother_tuber_Innovator"
id = "Mother_tuber_after_storage_Colomba"
id = "Mother_tuber_after_storage_Innovator"
id = "Root_Colomba"
id = "Root_Innovator"
group_factor = "`seedlot`" #group factor doesn't work, need to manully change in the function
qza_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Fun/Fun_MP_redo_final/Rvisualization/",id,"/bray_curtis_pcoa_results.qza")
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,"_fun.pdf")
visualize_pcoa(id, qza_file, metadata, group_factor, output_file)

print(p1e_Daughter_tuber_Colomba)
print(p1e_Daughter_tuber_Innovator)

#plot together without legend
library(ggpubr)
id = "All"
output_file = paste0("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Plots_4_pub/PCoA_bc_",id,"_fun.pdf")
pdf(output_file,height=13, width=10) 
ggarrange(p1e_Mother_tuber, p1e_Mother_tuber_Colomba,p1e_Mother_tuber_Innovator,
          p1e_Mother_tuber_after_storage, p1e_Mother_tuber_after_storage_Colomba,p1e_Mother_tuber_after_storage_Innovator,
          p1e_Daughter_tuber, p1e_Daughter_tuber_Colomba,p1e_Daughter_tuber_Innovator,
          p1e_Root, p1e_Root_Colomba,p1e_Root_Innovator,ncol = 3, nrow = 4,legend = "none")
dev.off()

#Variety color is wrong, need to change



####STOP here###
#Mother tuber 
{
  ######################
  #####Mother peel before storage######  = 'Mother_tuber' file
  ######################
  {
    setwd("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Mother_tuber")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    round(braycurtis$data$ProportionExplained[1,2]*100)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`seedlot`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    
    
    ggsave("PCoA_bc_MPBS_small.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `variety`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_small_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
  
  ######################
  #####Mother peel before storage Colomba###### = 'Mother_tuber_Colomba' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Mother_tuber/Mother_tuber_Colomba")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Colomba.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Colomba_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
  
  ######################
  #####Mother peel before storage Innovator###### = 'Mother_tuber_Innovator' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Mother_tuber/Mother_tuber_Innovator")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Innovator.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Innovator_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
}

#Mother tuber after storage (3x plot)
{
  ######################
  #####Mother Peel######  = 'Mother_tuber_after_storage' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Mother_tuber_after_storage")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("Second_generation_bac_rep_seqs_filtered2_NoNonsense_rooted-tree_shannon-vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    round(braycurtis$data$ProportionExplained[1,2]*100)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety`),size=0.5,colour = "black"))
    ggsave("PCoA_bc_MPBS_ellipse.pdf", height=6, width=7.5, device="pdf")
  }
  
  ######################
  #####Mother Peel Colomba###### = 'Mother_tuber_after_storage_Colomba' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Mother_tuber_after_storage/Mother_tuber_after_storage_Colomba")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Colomba.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Colomba_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
  
  ######################
  #####Mother Peel Innovator###### = 'Mother_tuber_after_storage_Innovator' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Mother_tuber_after_storage/Mother_tuber_after_storage_Innovator")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Innovator.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Innovator_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
}

#Root (3x plot)
{
  ######################
  #####Daughter Root######  = 'Root' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Root")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("Second_generation_bac_rep_seqs_filtered2_NoNonsense_rooted-tree_shannon-vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    round(braycurtis$data$ProportionExplained[1,2]*100)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety`),size=0.5,colour = "black"))
    ggsave("PCoA_bc_MPBS_ellipse.pdf", height=6, width=7.5, device="pdf")
  }
  
  ######################
  #####Daughter Root Colomba###### = 'Root_Colomba' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Root/Root_Colomba")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Colomba.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Colomba_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
  
  ######################
  #####Daughter Root Innovator###### = 'Root_Innovator' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Root/Root_Innovator")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Innovator.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Innovator_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
}

#Daughter tuber (3x plot)
{
  ######################
  #####Daughter peel before storage######  = 'Daughter_tuber' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Daughter_tuber")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("Second_generation_bac_rep_seqs_filtered2_NoNonsense_rooted-tree_shannon-vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    round(braycurtis$data$ProportionExplained[1,2]*100)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety`),size=0.5,colour = "black"))
    ggsave("PCoA_bc_MPBS_ellipse.pdf", height=6, width=7.5, device="pdf")
  }
  
  ######################
  #####Daughter peel before storage Colomba###### = 'Daughter_tuber_Colomba' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Daughter_tuber/Daughter_tuber_Colomba")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Colomba.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Colomba_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
  
  ######################
  #####Daughter peel before storage Innovator###### = 'Daughter_tuber_Innovator' file
  ######################
  {
    setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/Daughter_tuber/Daughter_tuber_Innovator")
    
    braycurtis<-read_qza("bray_curtis_pcoa_results.qza")
    #shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 
    
    summary(braycurtis$data)
    braycurtis$data$ProportionExplained
    dim(braycurtis$data$ProportionExplained)
    
    xlab <- paste("PC1 (",round(braycurtis$data$ProportionExplained[1,1]*100,1),"% )")
    ylab <- paste("PC2 (",round(braycurtis$data$ProportionExplained[1,2]*100,1),"% )")
    
    (p1<-braycurtis$data$Vectors %>%
        select(SampleID, PC1, PC2) %>%
        left_join(metadata) %>%
        ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`Variety_Field`)) +
        geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
        theme_q2r() + 
        scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
        scale_color_manual(values=c("#ffb395","#fc8d62", "#ce7452")) + 
        theme(text=element_text(size = 15)))
    (p1<-p1+xlab( xlab)+ylab( ylab))
    
    ggsave("PCoA_bc_MPBS_Innovator.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches
    
    (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety_Field`),size=0.5,color="black"))
    ggsave("PCoA_bc_MPBS_Innovator_ellipse.pdf", height=3.5, width=5, device="pdf")
  }
}




