#if (!requireNamespace("devtools", quietly = TRUE)){install.packages("devtools")}
#devtools::install_github("jbisanz/qiime2R") # current version is 0.99.20
library(qiime2R)

setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization")


#Reading Metadata

metadata<-read.table(file="../metadata_second_generation_bac_withPBS.txt",header=T,sep = "\t")
dim(metadata)
head(metadata)
head(metadata)


#Plotting PCoA
library(tidyverse)

# install.packages("tidyverse")
# library(tidyverse)
# devtools::install_github("jbisanz/qiime2R")
# 
# install.packages("remotes")
# remotes::install_github("jbisanz/qiime2R")
# install.packages("qiime2R")
# library(qiime2R)


######################
#####All treatments###
######################
{
setwd("/Users/ellenmanders/Documents/BACT_Potato_Lovers_Tasks/All_treatments")



#####braycurtis######
braycurtis<-read_qza("Second_generation_bac_withPBS_rep_seqs_filtered2_NoNonsense_rooted-tree_bray-curtis-pcoa-results.qza") #19-9-2021 changed name
#shannon<-read_qza("Second_generation_bac_rep_seqs_filtered2_NoNonsense_rooted-tree_shannon-vector.qza")$data %>% rownames_to_column("SampleID") 
#shannon2<-read_qza("10000_Second_generation_bac_shannon-vector.qza")$data %>% rownames_to_column("SampleID")
# shannon3<-read_qza("Second_generation_bac_rep_seqs_filtered2_NoNonsense_rooted-tree_observed-otus-vector.qza")$data %>% rownames_to_column("SampleID")
#size=observed_otus
#shannon4<-read_qza("Second_generation_bac_rep_seqs_filtered2_NoNonsense_rooted-tree_faith-pd-vector.qza")$data %>% rownames_to_column("SampleID")
#size=faith_pd
# summary(shannon)
#summary(shannon3)

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
  geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
  theme_q2r() + 
  scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
  theme(text=element_text(size = 15)))
(p1<-p1+xlab( xlab)+ylab( ylab))
p1

ggsave("PCoA_bc_alltreatment.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `compartment` )))
ggsave("PCoA_bc_alltreatment_ellipse.pdf", height=6, width=7.5, device="pdf")

#All treatments (seedlot = 'Variety_Field')
p1<-braycurtis$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, color=`Variety_Field`, shape=`compartment`)) +
  geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
  theme_q2r() +
  scale_color_brewer(type="seq",palette="Set2",name="Variety_Field of Production") +
  scale_shape_manual(values=c(15,16,17,18), name="Sample Type") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  theme(text=element_text(size = 15))
#+
#scale_color_discrete(name="Seedlot")
p1
(p1<-p1+xlab( xlab)+ylab( ylab))

ggsave("PCoA_bc_alltreatment_seedlot.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches


#All treatments (type)
p1<-braycurtis$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, color=`compartment`, shape=`Variety`)) +
  geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
  theme_q2r() +
  scale_color_brewer(type="seq",palette="Set2",name="Sample Type") +
  scale_shape_manual(values=c(15,17), name="Variety") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  theme(text=element_text(size = 15))

#+
#scale_color_discrete(name="Seedlot")
p1
(p1<-p1+xlab( xlab)+ylab( ylab))

ggsave("PCoA_bc_alltreatment_type.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `compartment` )))
ggsave("PCoA_bc_alltreatment_type_ellipse2.pdf", height=6, width=8.6, device="pdf")

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,colour = "black", aes(group = `compartment` )))
ggsave("PCoA_bc_alltreatment_type_ellipse2_black.pdf", height=6, width=8.6, device="pdf")
}

######################
#####All treatments without Mother_tuber_after_storage###
######################
{
  setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization/All_treatment_without_mother_tuber_after_storage")
  
  
  
  #####braycurtis######
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
      geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
      theme_q2r() + 
      scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
      scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
      theme(text=element_text(size = 15)))
  (p1<-p1+xlab( xlab)+ylab( ylab))
  p1
  
  ggsave("PCoA_bc_alltreatment_without_MTAS.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `compartment` )))
  ggsave("PCoA_bc_alltreatment_without_MTAS_ellipse.pdf", height=6, width=7.5, device="pdf")
  
  #All treatments (seedlot = 'Variety_Field')
  p1<-braycurtis$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`compartment`)) +
    geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    scale_color_brewer(type="seq",palette="Set2",name="Variety_Field of Production") +
    scale_shape_manual(values=c(15,16,17,18), name="Sample Type") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    theme(text=element_text(size = 15))
  #+
  #scale_color_discrete(name="Seedlot")
  p1
  (p1<-p1+xlab( xlab)+ylab( ylab))
  
  ggsave("PCoA_bc_alltreatment_without_MTAS_seedlot.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches
  
  
  #All treatments (type)
  p1<-braycurtis$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    ggplot(aes(x=PC1, y=PC2, color=`compartment`, shape=`variety`)) +
    geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    scale_color_brewer(type="seq",palette="Paired",name="Sample Type") +
    scale_shape_manual(values=c(15,17), name="Variety") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    theme(text=element_text(size = 15))
  
  #+
  #scale_color_discrete(name="Seedlot")
  p1
  (p1<-p1+xlab( xlab)+ylab( ylab))
  
  ggsave("PCoA_bc_alltreatment_without_MTAS_type2.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `compartment` )))
  ggsave("PCoA_bc_alltreatment_without_MTAS_type_ellipse2.pdf", height=6, width=8.6, device="pdf")
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,colour = "black", aes(group = `compartment` )))
  ggsave("PCoA_bc_alltreatment_type_without_MTAS_ellipse2_black2.pdf", height=6, width=8.6, device="pdf")
  
  ###set color for "type"
  #
  library("RColorBrewer")
  display.brewer.all()
  
  brewer.pal(n = 8, name = 'Paired')
  # "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00"
  p1<-braycurtis$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    ggplot(aes(x=PC1, y=PC2, color=`compartment`, shape=`variety`)) +
    geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    #scale_color_manual(values=c("#33A02C","#1F78B4","#B2DF8A")) + 
    scale_color_manual(values=c("#BF9000","#FFC000","#767171")) + 
    scale_shape_manual(values=c(15,17), name="Variety") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    theme(text=element_text(size = 15)) #change legen order doesn't work+
    scale_fill_discrete(breaks = c('Mother_tuber','Daughter_tuber','Root'),labels=c('A','B','C'))
  
  #+
  #scale_color_discrete(name="Seedlot")
  p1
#change legen order doesn't work
   #( p1+scale_fill_discrete(breaks = c('Mother_tuber','Daughter_tuber','Root'),labels=c('A','B','C')))
  (p1<-p1+xlab( xlab)+ylab( ylab))
  
  ggsave("PCoA_bc_MT_vs_MTAS_type_finalcolor.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches
 
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,colour = "black", aes(group = `compartment` )))
  ggsave("PCoA_bc_alltreatment_type_without_MTAS_finalcolor_ellipse_black.pdf", height=6, width=8.6, device="pdf")
  
  
  #change size of the symbols#
  p1<-braycurtis$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    ggplot(aes(x=PC1, y=PC2, color=`compartment`, shape=`variety`)) +
    geom_point(alpha=0.7, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    #scale_color_manual(values=c("#33A02C","#1F78B4","#B2DF8A")) + 
    scale_color_manual(values=c("#FFC000","#BF9000","#767171"),name="Sample type") + 
    scale_shape_manual(values=c(15,17), name="Variety") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    theme(text=element_text(size = 15)) #change legen order doesn't work+
  scale_fill_discrete(breaks = c('Seed tuber','Daughter tuber','Root'),labels=c('A','B','C'))
  
  #+
  #scale_color_discrete(name="Seedlot")
  p1
  #change legen order doesn't work
  #( p1+scale_fill_discrete(breaks = c('Mother_tuber','Daughter_tuber','Root'),labels=c('A','B','C')))
  (p1<-p1+xlab( xlab)+ylab( ylab))
  

  ggsave("PCoA_bc_MT_vs_MTAS_type_finalcolor_small.pdf", height=6, width=8.5, device="pdf")
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,colour = "black", aes(group = `compartment` )))
 
  ggsave("PCoA_bc_alltreatment_type_without_MTAS_finalcolor_ellipse_black_small.pdf", height=6, width=8.5, device="pdf")\
  ggsave("PCoA_bc_alltreatment_type_without_MTAS_finalcolor_ellipse_black_small2.pdf", height=5, width=7.2, device="pdf")
}

######################
#####Mother_tuber_vs_Mother_tuber_after_storage###
######################
{
  setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization/Mother_tuber_vs_mother_tuber_after_storage")
  
  
  
  #####braycurtis######
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
      ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`compartment`)) +
      geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
      theme_q2r() + 
      scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
      scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
      theme(text=element_text(size = 15)))
  (p1<-p1+xlab( xlab)+ylab( ylab))
  p1
  
  ggsave("PCoA_bc_MT_vs_MTAS.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `seedlot` )))
  ggsave("PCoA_bc_MT_vs_MTAS_ellipse_seedlot.pdf", height=6, width=8.5, device="pdf")
  (p1e<-p1+stat_ellipse(level = 0.68,color="black",linetype = 2,aes(group = `seedlot` )))
  ggsave("PCoA_bc_MT_vs_MTAS_ellipse_seedlot_black.pdf", height=6, width=8.5, device="pdf")
  
  #All treatments (seedlot = 'Variety_Field')
  p1<-braycurtis$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`compartment`)) +
    geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    scale_color_brewer(type="seq",palette="Set2",name="Variety_Field of Production") +
    scale_shape_manual(values=c(15,16,17,18), name="Sample Type") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    theme(text=element_text(size = 15))
  #+
  #scale_color_discrete(name="Seedlot")
  p1
  (p1<-p1+xlab( xlab)+ylab( ylab))
  
  ggsave("PCoA_bc_MT_vs_MTAS_seedlot.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches
  
  
  #All treatments (type)
  p1<-braycurtis$data$Vectors %>%
    select(SampleID, PC1, PC2) %>%
    left_join(metadata) %>%
    ggplot(aes(x=PC1, y=PC2, color=`compartment`, shape=`variety`)) +
    geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() +
    scale_color_brewer(type="seq",palette="Paired",name="Sample Type") +
    scale_shape_manual(values=c(15,17), name="Variety") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    theme(text=element_text(size = 15))
  
  #+
  #scale_color_discrete(name="Seedlot")
  p1
  (p1<-p1+xlab( xlab)+ylab( ylab))
  
  ggsave("PCoA_bc_MT_vs_MTAS_type2.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `compartment` )))
  ggsave("PCoA_bc_MT_vs_MTAS_type_ellipse2.pdf", height=6, width=8.6, device="pdf")
  
  (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,colour = "black", aes(group = `compartment` )))
  ggsave("PCoA_bc_MT_vs_MTAS_ellipse2_black2.pdf", height=6, width=8.6, device="pdf")

###set color for "type"
#
library("RColorBrewer")
display.brewer.all()

brewer.pal(n = 8, name = 'Paired')
# "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00"
p1<-braycurtis$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, color=`compartment`, shape=`variety`)) +
  geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
  theme_q2r() +
  scale_color_manual(values=c("#BF9000","#A6CEE3")) + 
  scale_shape_manual(values=c(15,17), name="Variety") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  theme(text=element_text(size = 15))

#+
#scale_color_discrete(name="Seedlot")
p1
(p1<-p1+xlab( xlab)+ylab( ylab))

ggsave("PCoA_bc_MT_vs_MTAS_type_finalcolor.pdf", height=6, width=8.5, device="pdf") # save a PDF 3 inches by 4 inches

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `compartment` )))
ggsave("PCoA_bc_MT_vs_MTAS_type_ellipse2.pdf", height=6, width=8.6, device="pdf")

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,colour = "black", aes(group = `compartment` )))
ggsave("PCoA_bc_MT_vs_MTAS_fianlcolor_ellipse2_black.pdf", height=6, width=8.6, device="pdf")
}
#Mother tuber (3x plot)
{
######################
#####Mother peel before storage######  = 'Mother_tuber' file
######################
{
setwd("C:/Postdoc/HZPC_project/Second_generation/Bac/Qiime2_withPBS_final/Rvisualization/Mother_tuber")

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
    ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`seedlot`)) +
    geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() + 
    scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
    theme(text=element_text(size = 15)))
(p1<-p1+xlab( xlab)+ylab( ylab))

# ggsave("PCoA_bc_MPBS.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches
# 
# (p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `Variety`),size=0.5,colour = "black"))
# ggsave("PCoA_bc_MPBS_ellipse.pdf", height=6, width=7.5, device="pdf")

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


