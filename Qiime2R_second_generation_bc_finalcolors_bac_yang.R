#if (!requireNamespace("devtools", quietly = TRUE)){install.packages("devtools")}
#devtools::install_github("jbisanz/qiime2R") # current version is 0.99.20
library(qiime2R)

setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization")

#Reading Metadata

metadata<-read.table(file="../metadata_second_generation_bac_withPBS.txt",header=T,sep = "\t")
dim(metadata)
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
setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization/All_treatments")


#####braycurtis######
braycurtis<-read_qza("Second_generation_bac_withPBS_rep_seqs_filtered2_NoNonsense_rooted-tree_bray-curtis-pcoa-results.qza")
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
  ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`seedlot`)) +
  geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
  theme_q2r() + 
  scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87" ,"#ffb395","#fc8d62", "#ce7452")) + 
  theme(text=element_text(size = 15)))
(p1<-p1+xlab( xlab)+ylab( ylab))

ggsave("PCoA_bc_alltreatment.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `compartment` )))
ggsave("PCoA_bc_alltreatment_ellipse.pdf", height=6, width=7.5, device="pdf")

#All treatments (seedlot)
p1<-braycurtis$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(metadata) %>%
  ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`compartment`)) +
  geom_point(alpha=0.7, size =4, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
  theme_q2r() +
  scale_color_brewer(type="seq",palette="Set2",name="Field of Production") +
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
  ggplot(aes(x=PC1, y=PC2, color=`compartment`, shape=`variety`)) +
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


###From here see Ellen's code for updated version#####


######################
#####Mother tuber before storage######
######################
setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization/Mother_tuber_before_storage")

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

ggsave("PCoA_bc_MPBS.pdf", height=6, width=7.5, device="pdf") # save a PDF 3 inches by 4 inches

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `variety`),size=0.5,colour = "black"))
ggsave("PCoA_bc_MPBS_ellipse.pdf", height=6, width=7.5, device="pdf")


######################
#####Mother tuber before storage Colomba######
######################
setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization/Mother_tuber_before_storage/Mother_tuber_before_storage_Colomba")

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
    ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`seedlot`)) +
    geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() + 
    scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    scale_color_manual(values=c("#9cd7c2","#66c2a5" ,"#559f87")) + 
    theme(text=element_text(size = 15)))
(p1<-p1+xlab( xlab)+ylab( ylab))

ggsave("PCoA_bc_MPBS_Colomba.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `seedlot`),size=0.5,color="black"))
ggsave("PCoA_bc_MPBS_Colomba_ellipse.pdf", height=3.5, width=5, device="pdf")


######################
#####Mother tuber before storage Innovator######
######################
setwd("C:/Postdoc/HZPC_project/Second_generation/bac/Qiime2_withPBS_final/Rvisualization/Mother_tuber_before_storage/Mother_tuber_before_storage_Innovator")

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
    ggplot(aes(x=PC1, y=PC2, color=`seedlot`, shape=`seedlot`)) +
    geom_point(alpha=0.9, size =5, stroke = 0.6) + #alpha controls transparency and helps when points are overlapping
    theme_q2r() + 
    scale_shape_manual(values=c(15,16,17,15,16,17)) + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
    scale_color_manual(values=c("#ffb395","#fc8d62", "#ce7452")) + 
    theme(text=element_text(size = 15)))
(p1<-p1+xlab( xlab)+ylab( ylab))

ggsave("PCoA_bc_MPBS_Innovator.pdf", height=3.5, width=5, device="pdf") # save a PDF 3 inches by 4 inches

(p1e<-p1+stat_ellipse(level = 0.68,linetype = 2,aes(group = `seedlot`),size=0.5,color="black"))
ggsave("PCoA_bc_MPBS_Innovator_ellipse.pdf", height=3.5, width=5, device="pdf")

