
library(stringi)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(afex)
library(emmeans)
library(lme4)
library(lattice)
library("latticeExtra")
library(dplyr)
library(summarytools)
library(ggfortify)
library(tidyverse)
library(multcomp)
library(ggbeeswarm)
library(phyloseq)
library(qiime2R)
library(car)

sessionInfo()

setwd("~/Desktop/Tim-Lab/FMT_Study_1/new_output")


Performance <- read.table("Performance.txt", header=TRUE, sep="\t")
Performance$Trt <- factor(Performance$Trt, levels = c("Control", "Oral", "Rectal", "In-feed"))
Performance$day <- as.factor(Performance$day)
Performance$Trt <- as.factor(Performance$Trt)

my_colors <- c('#440154FF', '#26818EFF', '#5DC863FF', '#FDE725FF')


#Body weight
#Subset day 2
levels(Performance$day)
Performance_d2 <- subset(Performance, day =="2")

#Compare the mean of each treatment group on d2 to grand mean of d2 
#The dependent variable here was changed for ADG=Average daily gain, BW=Body weight, DI=Diarrhea index 
options(contrasts=c("contr.sum", "contr.poly"))
p2 <- lm(ADG ~ Trt, data = Performance_d2)
shapiro_test(residuals(p2))
summary(p2)

#Subset day 5
Performance_d5 <- subset(Performance, day =="5")
p3 <- lm(ADG ~ Trt, data = Performance_d5)
shapiro_test(residuals(p3))
summary(p3)

p3 <- aov(ADG~Trt, data = Performance_d5)
summary(p3)
TukeyHSD(p3)

#Subset day 7
Performance_d7 <- subset(Performance, day =="7")
p4 <- lm(ADG ~ Trt, data = Performance_d7)
shapiro_test(residuals(p4))
summary(p4)


#Body weight plot
ggplot(Performance, aes(x=day, y=BW, fill=Trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw() +
  ylab("Body Weight") + xlab ("Day") + 
  scale_fill_manual(values = my_colors) +
  labs(fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 32), axis.text.y = element_text(color = "black", size = 32)) 
ggsave(paste0("output/Body_Weight2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches

#Average Daily Gain
Performance_ADG_d2 <- subset(Performance, day =="2")
Performance_ADG_d5 <- subset(Performance, day =="5" )
Performance_ADG_d7 <- subset(Performance, day =="7" )

Performance_ADG <- rbind(Performance_ADG_d2, Performance_ADG_d5, Performance_ADG_d7)
ggplot(Performance_ADG, aes(x=day, y=ADG, fill=Trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw() +
  ylab("Average Daily Gain") + xlab ("Day") + 
  scale_fill_manual(values = my_colors) +
  labs(family="Times New Roman", fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=28, face="bold"), axis.title.y = element_text(color="black", size=28, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 28), axis.text.y = element_text(color = "black", size = 28))
ggsave(paste0("output/Average_Daily_Gain2", ".pdf"), height=5, width=7, device="pdf") # save a PDF 3 inches by 4 inches

#Diarrhea Index
ggplot(Performance, aes(x=day, y=Diarrhea_Index, fill=Trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw() +
  ylab("Diarrhea Index") + xlab ("Route of transplant") + 
  scale_fill_manual(values = my_colors) +
  labs(fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 32), axis.text.y = element_text(color = "black", size = 32)) 
ggsave(paste0("output/Diarrhea_Index2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches



#Feed intake
FI <- lm(Feed_Intake ~ Trt, data = Performance)
summary(FI)
shapiro_test(residuals(FI))

#Feed Intake
ggplot(Performance, aes(x=Trt, y=Feed_Intake, fill=Trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw() +
  ylab("Average Daily Feed Intake") + xlab ("Route of transplant") + 
  scale_fill_manual(values = my_colors) +
  labs(Family="Times New Roman", fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=28, face="bold"), axis.title.y = element_text(color="black", size=28, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 20), axis.text.y = element_text(color = "black", size = 20)) 
ggsave(paste0("output/Feed_Intake2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches

#Gain to feed
ggplot(Performance, aes(x=Trt, y=G_F, fill=Trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw() +
  ylab("Gain:Feed") + xlab ("Route of transplant") + 
  scale_fill_manual(values = my_colors) +
  labs(family="Times New Roman", fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 24), axis.text.y = element_text(color = "black", size = 24)) 
ggsave(paste0("output/G_F", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches

#Model
g_f <-  lm(G_F ~ Trt, data = fmt) 
shapiro_test(residuals(g_f))
summary(g_f)

#Make means
g_f_means <- emmeans(g_f, ~Trt) 
g_f_means_dataframe <- data.frame(g_f_means)

#Non parametric test
kruskal.test(G_Fmod ~ Trt, data=fmt)

#Plot
ggplot(data = g_f_means_dataframe, aes(x = Trt, y = emmean, colour = Trt)) +   
  geom_point(size = 10) + geom_errorbar(aes(ymin =emmean - SE, ymax = emmean + SE), width = 0.2) + 
  theme_bw() + 
  xlab ("Route of transplant") + ylab ("Gain:Feed") +
  scale_color_manual(values = my_colors) +
  labs(family="Times New Roman", color= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 24), axis.text.y = element_text(color = "black", size = 24)) 
ggsave(paste0("output/G_Fnew", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


#########################
#Histology
#Day 4
d4_Ileum <- read.table("d4_Ileum.txt", header=TRUE, sep="\t")

#Dependent variable was changed to vh=villus height, vp=villus perimeter, cd=crypt depth, cp=villus perimeter, vc=villus:crypt
d4_Ile_vh <- lm(VC_Ratio ~ Trt, data = d4_Ileum)
shapiro_test(residuals(d4_Ile_vh))
summary(d4_Ile_vh)
d4_Ile_vh <- aov(VC_Ratio ~ Trt, data = d4_Ileum)
summary(d4_Ile_vh)
TukeyHSD(d4_Ile_vh)

#Day 8
d8_Ileum <- read.table("d8_Ileum.txt", header=TRUE, sep="\t")

d8_Ile_vh <- lm(VC_Ratio ~ Trt, data = d8_Ileum)
shapiro_test(residuals(d8_Ile_vh))
summary(d8_Ile_vh)
d8_Ile_vh <- aov(VC_Ratio ~ Trt, data = d8_Ileum)
summary(d8_Ile_vh)
TukeyHSD(d8_Ile_vh)

#Colon
#Day 4
d4_Colon <- read.table("d4_Colon.txt", header=TRUE, sep="\t")

#Dependent variable was changed to cd=crypt depth, cp=villus perimeter
d4_Col_vh <- lm(Villi_Area ~ Trt, data = d4_Colon)
shapiro_test(residuals(d4_Col_vh))
summary(d4_Col_vh)

d4_Col_vh <- aov(Villi_Area ~ Trt, data = d4_Colon)
summary(d4_Col_vh)
TukeyHSD(d4_Col_vh)

#Day 8
d8_Colon <- read.table("d8_Colon.txt", header=TRUE, sep="\t")

d8_Col_vh <- lm(Villi_Area ~ Trt, data = d8_Colon)
shapiro_test(residuals(d8_Col_vh))
summary(d8_Col_vh)
d8_Col_vh <- aov(Villi_Area ~ Trt, data = d8_Colon)
summary(d8_Col_vh)
TukeyHSD(d8_Col_vh)


#Microbiome Analysis
#Load metadat
metadata <- read.table("metadata.txt", header=TRUE, sep="\t")

#load OTU table
otu_table <- read_qza("observed_features_vector.qza")
#Extract column from OTU table
otu_table <- otu_table$data %>%
  dplyr::select(observed_features)

#Load evenness
evenness <- read_qza("evenness_vector.qza")
#Extract column from evenness
evenness <- evenness$data%>%
  dplyr::select(pielou_evenness)

#Load faith
faith <- read_qza("faith_pd_vector.qza")
#Extract column from faith 
faith <- faith$data%>%
  dplyr::select(V1,V2)
#in faith, the sample ID is named a separate colum and not as column name as in other metrics
colnames(faith) <- c("SampleID","faith")
row.names(faith) <- faith[,1]
faith <- faith[,-1]

#create alpha diversity dataframe
alpha_diversity <- merge(otu_table, shannon, by.x = 0, by.y = 0)
alpha_diversity <- merge(alpha_diversity, evenness, by.x = "Row.names", by.y = 0)
alpha_diversity <- merge(alpha_diversity, faith, by.x = "Row.names", by.y = "SampleID")

meta <- merge(metadata, alpha_diversity, by.x ="SampleID" , by.y = "Row.names")

# Change the sample name Fe_d0_30 to Fe_d0_32
meta$SampleID[meta$SampleID == 'Fe_d0_30'] <- 'Fe_d0_32'

str(meta)
meta$day <- as.factor(meta$day)
meta$trt <- as.factor(meta$trt)
meta$type <- as.factor(meta$type)
meta$type <- as.factor(meta$type)
meta$observed_features <- as.numeric(meta$observed_features)
str(meta)

#order the treatments
meta$trt <- factor(meta$trt, levels = c("Donor", "Control", "Oral", "Rectal", "In-feed"))

levels(meta$trt)


#Subset d0
meta_d0 <- subset(meta, day =="0")

 meta_d0%>%
  group_by(trt) %>%
  get_summary_stats(observed_features, type = "mean_sd")

#Treatment effect on at , this is to see if there is difference between treatment groups before esperiment started
#Compare each treatment mean to the grand mean
 options(contrasts=c("contr.sum", "contr.poly"))
#options(contrasts=c("contr.treatment", "contr.poly"))
 
#Repeat each analysis for observed_features, pielou_evenness, faith
m0 <- lm(observed_features ~ trt, data = meta_d0)
summary(m0)
ggqqplot(residuals(m_overall))
shapiro_test(residuals(m_overall))

#Add donor to d0 to make baseline
meta_donor <- subset(meta, day =="Donor")
meta_baseline <- rbind(meta_d0, meta_donor)

#Difference between treatment group and donor at d0, baseline
#Compare each treatment mean to the donor mean
#options(contrasts=c("contr.sum", "contr.poly"))
options(contrasts=c("contr.treatment", "contr.poly"))
m1 <- lm(observed_features ~ trt, data = meta_baseline)
summary(m1)
m1 <- aov(observed_features~trt, data = meta_baseline)
summary(m1)
TukeyHSD(m1)

#Repeat this for at each time point

#Subset day 2
levels(meta$day)
meta_d2 <- subset(meta, day =="2")

#Compare the mean of each treatment group on d2 to grand mean of d2 
m2 <- lm(observed_features ~ trt, data = meta_d2)
summary(m2)
m2 <- aov(observed_features~trt, data = meta_d2)
summary(m2)
TukeyHSD(m2)

#Subset day 5
meta_d5 <- subset(meta, day =="5")
#Compare the mean of each treatment group on d5 to grand mean of d5
m3 <- lm(observed_features ~ trt, data = meta_d5)
summary(m3)
shapiro_test(residuals(m3_obs))
m3 <- aov(ace~trt, data = meta_d5)
summary(m3)
TukeyHSD(m3)


#Subset day 7
meta_d7 <- subset(meta, day =="7")
#Compare the mean of each treatment group on d7 to grand mean of d7
m4 <- lm(observed_features ~ trt, data = meta_d7)
summary(m4)
m4 <- aov(observed_features ~ trt, data = meta_d7)
summary(m4)
TukeyHSD(m4)



#Subset cecal samples
meta_cecal_alpha <- subset(meta, type == "cecal")
meta_cecal_alpha$trt <- factor(meta_cecal_alpha$trt, levels = c("Donor", "Control", "Oral", "Rectal", "In-feed"))

#Subset d4 cecal samples
meta_cecal_alpha_d4 <- subset(meta_cecal_alpha, day =="4")
meta_cecal_alpha_d4a$trt <- factor(meta_cecal_alpha_d4a$trt, levels = c("Control", "Oral", "Rectal", "In-feed"))
ca1a <- lm(ace ~ trt, data = meta_cecal_alpha_d4a)

#Check assumptions on feca samples
ggqqplot(residuals(ca1a))
shapiro_test(residuals(ca1a))
summary(ca1)

#Subset d8 cecal samples
meta_cecal_alpha_d8 <- subset(meta_cecal_alpha, day =="8")
ca2 <- lm(ace ~ trt, data = meta_cecal_alpha_d8)

#Check assumptions on feca samples
ggqqplot(residuals(ca2))
shapiro_test(residuals(ca2))
summary(ca2)


#Subset colon samples
meta_colon_alpha <- subset(meta, type == "colon")

#Subset d4 colon samples
meta_colon_alpha_d4 <- subset(meta_colon_alpha, day =="4")
co1 <- lm(ace ~ trt, data = meta_colon_alpha_d4)

#Check assumptions on feca samples
ggqqplot(residuals(co1))
shapiro_test(residuals(co1))
summary(co1)

#Subset d8 colon samples
meta_colon_alpha_d8 <- subset(meta_colon_alpha, day =="8")
co2 <- lm(ace ~ trt, data = meta_colon_alpha_d8)

#Check assumptions on feca samples
ggqqplot(residuals(co2))
shapiro_test(residuals(co2))
summary(co2)



library(scales)
show_col(viridis_pal()(60))  
set.seed(596)
my_colors <- c('Red', '#440154FF', '#26818EFF', '#5DC863FF', '#FDE725FF')
meta$trt <- factor(meta$trt, levels = c("Control", "Oral", "Rectal", "In-feed", "Donor"))

#Filter colon and cecum
meta_fecal_alpha <- subset(meta, type == "fecal")
meta_cecal <- subset(meta, type =="cecal")
meta_colon <- subset(meta, type =="colon")
meta_d5 <- subset(meta, day =="5")
meta_d7 <- subset(meta, day =="7")


# fecal alpha diversity figures
ggplot(meta_fecal_alpha, aes(x=day, y=observed_features, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw()+
  ylab("Observed ASVs") + xlab ("Day") + 
  scale_fill_manual(values = my_colors) +
  labs(fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 24), axis.text.y = element_text(color = "black", size = 24))
ggsave(paste0("output/Observed_ASV2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


ggplot(meta_fecal_alpha, aes(x=day, y=faith, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw()+
  labs(
    x = "Day",
    y = expression(atop(bold("Faith's Phylogenetic"), bold("Diversity")))
  ) +
  scale_fill_manual(values = my_colors, name = "Treatment") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.key.size = unit(8, "point"),
    axis.title.x = element_text(color = "black", size = 32, face = "bold"),
    axis.title.y = element_text(color = "black", size = 32, face = "bold"),
    axis.text.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(color = "black", size = 24)
  )
ggsave(paste0("output/Faith2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


ggplot(meta_fecal_alpha, aes(x=day, y=pielou_evenness, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(fill= "Treatment") +
  theme_bw()+
  ylab("Evenness (Pielou)") + xlab ("Day") + 
  scale_fill_manual(values = my_colors) +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 24), axis.text.y = element_text(color = "black", size = 24)) 
ggsave(paste0("output/Evenness2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


# cecal alpha diversity figures
ggplot(meta_cecal, aes(x=day, y=observed_features, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw()+
  ylab("Observed ASVs") + xlab ("Day") + 
  scale_fill_manual(values = my_colors) +
  labs(fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 24), axis.text.y = element_text(color = "black", size = 24))
ggsave(paste0("output/cecal_Observed_ASV2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


ggplot(meta_cecal, aes(x=day, y=faith, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw()+
  labs(
    x = "Day",
    y = expression(atop(bold("Faith's Phylogenetic"), bold("Diversity")))
  ) +
  scale_fill_manual(values = my_colors, name = "Treatment") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.key.size = unit(8, "point"),
    axis.title.x = element_text(color = "black", size = 32, face = "bold"),
    axis.title.y = element_text(color = "black", size = 32, face = "bold"),
    axis.text.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(color = "black", size = 24)
  )
ggsave(paste0("output/cecal_Faith2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches

# colon alpha diversity figures
ggplot(meta_colon, aes(x=day, y=observed_features, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw()+
  ylab("Observed ASVs") + xlab ("Day") + 
  scale_fill_manual(values = my_colors) +
  labs(fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 24), axis.text.y = element_text(color = "black", size = 24))
#theme(text=element_text(family="Times New Roman"))
ggsave(paste0("output/colon_Observed_ASV2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


ggplot(meta_colon, aes(x=day, y=faith, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw()+
  labs(
    x = "Day",
    y = expression(atop(bold("Faith's Phylogenetic"), bold("Diversity")))
  ) +
  scale_fill_manual(values = my_colors, name = "Treatment") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.key.size = unit(8, "point"),
    axis.title.x = element_text(color = "black", size = 32, face = "bold"),
    axis.title.y = element_text(color = "black", size = 32, face = "bold"),
    axis.text.x = element_text(color = "black", size = 24),
    axis.text.y = element_text(color = "black", size = 24)
  )
#theme(text=element_text(family="Times New Roman"))
ggsave(paste0("output/colon_faith2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


ggplot(meta_colon, aes(x=day, y=pielou_evenness, fill=trt)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw()+
  ylab("Pielou's Evenness") + xlab ("Day") + 
  scale_fill_manual(values = my_colors) +
  labs(fill= "Treatment") +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=32, face="bold"), axis.title.y = element_text(color="black", size=32, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 24), axis.text.y = element_text(color = "black", size = 24))
#theme(text=element_text(family="Times New Roman"))
ggsave(paste0("output/colon_Evennes2", ".pdf"), height=6, width=8, device="pdf") # save a PDF 3 inches by 4 inches


#Beta diversity
#Jaccard
metadata <- read.table("metadata.txt", header=TRUE, sep="\t")
ja_PCoA<-read_qza("jaccard_pcoa_results.qza")
ja_meta <- ja_PCoA$data$Vectors %>%
  dplyr::select(SampleID, PC1, PC2) %>%
  inner_join(metadata, by = c("SampleID" = "SampleID"))

ja_meta$trt <- factor(ja_meta$trt, levels = c("Donor", "Control", "Oral", "Rectal", "In-feed"))

beta_feca <- subset(ja_meta, type =="fecal")
beta_cecal <- subset(ja_meta, type =="cecal")
beta_colon <- subset(ja_meta, type =="colon")
beta_donor <- subset(beta_feca, day =="Donor")
beta_d0 <- subset(beta_feca, day =="0")
beta_baseline <- rbind(beta_donor, beta_d0)

beta_d2 <- subset(beta_feca, day =="2")
beta_d5 <- subset(beta_feca, day =="5")
beta_d7 <- subset(beta_feca, day =="7")
beta_cecal_d4 <- subset(beta_cecal, day =="4")
beta_cecal_d8 <- subset(beta_cecal, day =="8")
beta_colon_d4 <- subset(beta_colon, day =="4")
beta_colon_d8 <- subset(beta_colon, day =="8")
beta_feca_d0_d2 <- rbind(beta_baseline, beta_d2)
beta_feca_d5_d7 <- rbind(beta_d5, beta_d7, beta_donor)

my_column <- "trt"
#my_colors <- c('Red', '#440154FF', '#26818EFF', '#5DC863FF', '#FDE725FF')
my_colors <- c('#440154FF', '#26818EFF', '#5DC863FF', '#FDE725FF')

ja_meta_d4 <- subset(ja_meta, day == "4")
ja_meta_d5 <- subset(ja_meta, day == "5")
ja_meta_d7 <- subset(ja_meta, day == "7")
ja_meta_d8 <- subset(ja_meta, day == "8")

ja_meta_d4_5 <- rbind(ja_meta_d4, ja_meta_d5) 
ja_meta_d7_8 <- rbind(ja_meta_d7, ja_meta_d8) 


#Baseline
ggplot(beta_baseline, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlim(-0.24, 0.38)+
  ylim(-0.29,0.21)+
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-Baseline_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_baseline,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_baseline, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 5) +
  theme_bw() +
  #stat_ellipse(level = 0.95, type = "t") +
  xlim(-0.24, 0.38)+
  ylim(-0.29,0.21)+
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-Baseline_ellipse_centroid_", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches


#day2
beta_d2 <- rbind(beta_donor, beta_d2)
ggplot(beta_d2, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlim(-0.24, 0.38)+
  ylim(-0.29,0.21)+
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-d2_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_d2,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_d2, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 5) +
  theme_bw() +
 # stat_ellipse(level = 0.95, type = "t") +
  xlim(-0.24, 0.38)+
  ylim(-0.29,0.21)+
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-d2_ellipse_centroid_", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches


#day 5
beta_d5 <- rbind(beta_donor, beta_d5)
ggplot(beta_d5, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlim(-0.24, 0.13)+
  ylim(-0.19,0.26)+
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-d5_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_d5,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_d5, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 5) +
  theme_bw() +
  #stat_ellipse(level = 0.95, type = "t") +
  xlim(-0.24, 0.13)+
  ylim(-0.19,0.26)+
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-d5_ellipse_centroid_", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

#day7
beta_d7 <- rbind(beta_donor, beta_d7)
ggplot(beta_d7, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlim(-0.24, -0.025)+
  ylim(-0.18,0.24)+
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-d7_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_d7,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_d7, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) +
  geom_point(data=centroids, size = 5) +
  theme_bw() +
  #stat_ellipse(level = 0.95, type = "t") +
  xlim(-0.24, -0.025)+
  ylim(-0.18,0.24)+
  labs(color= "Treatment") +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text (size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-d7_ellipse_centroid", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches


# cecal day 4
ggplot(beta_cecal_d4, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-cecal_d4a_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_cecal_d4,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_cecal_d4, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 5) +
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-cecal_d4_ellipse_centroid_", my_column,".pdf"), height=3, width=6, device="pdf") # save a PDF 3 inches by 4 inches

# cecal day 8
ggplot(beta_cecal_d8, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-cecal_d8a_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_cecal_d8,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_cecal_d8, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 5) +
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-cecal_d8_ellipse_centroid_", my_column,".pdf"), height=3, width=6, device="pdf") # save a PDF 3 inches by 4 inches

# colon day 4
#beta_colon_d4a <- rbind(beta_donor, beta_colon_d4)
ggplot(beta_colon_d4, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-colon_d4a_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_colon_d4,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_colon_d4, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 5) +
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-colon_d4_ellipse_centroid_", my_column,".pdf"), height=3, width=6, device="pdf") # save a PDF 3 inches by 4 inches

# colon day 8
#beta_colon_d8a <- rbind(beta_donor, beta_colon_d8)
ggplot(beta_colon_d8, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point() + #alpha controls transparency and helps when points are overlapping
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-colon_d8a_ellipse", my_column,".pdf"), height=3, width=4.5, device="pdf") # save a PDF 3 inches by 4 inches

centroids <- aggregate(cbind(PC1,PC2)~get(my_column),beta_colon_d8,mean)
colnames(centroids)[1] <- "trt"

ggplot(beta_colon_d8, aes(x=PC1, y=PC2, color=get(my_column))) +
  geom_point(size=1.5) + #alpha controls transparency and helps when points are overlapping
  geom_point(data=centroids, size = 5) +
  theme_bw() +
  xlab(paste0("PC1 (", round(100*ja_PCoA$data$ProportionExplained[1], digits = 2), "%)")) +
  ylab(paste0("PC2 (", round(100*ja_PCoA$data$ProportionExplained[2], digits = 2), "%)")) +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(size =12,color ="black"),
        axis.text.y = element_text (size=12, color ="black"))+
  theme(axis.title.x = element_text(size =16, color = "black", face = "bold"),
        axis.title.y = element_text(size =16, color = "black", face = "bold")) +
  theme(legend.key.size = unit(2, 'cm'), 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values=my_colors, name = my_column)
ggsave(paste0("output/JAC-colon_d8a_ellipse_centroid_", my_column,".pdf"), height=3, width=6, device="pdf") # save a PDF 3 inches by 4 inches


#################################
#Ecological outcomes

setwd("~/Desktop/Tim-Lab/FMT_Study_1/Results/16S/FMT_outcome")

# Useful code
#Load data
metadata <- read.table("metadata_d2.txt", header=TRUE, sep="\t")
ASV_table <- read.table("feature-rarefied_table-l6.tsv", header=TRUE, sep="\t")
ASV_table <- replace(ASV_table, ASV_table ==0, NA) 

#Making sample ID to be on rows instead of column
ASV_table <- as.data.frame(t(as.matrix(ASV_table)))
colnames(ASV_table) <- ASV_table[1,]
ASV_table <- ASV_table[-1,]

#Merging metadata to AASV_table
ASV <- merge(metadata, ASV_table, by.x = 'Sample_ID', by.y = 0)
#subset fecal samples alone
#ASV <- subset(ASV, type == "fecal")
ASV <- ASV[,-4:-6]

#run this line for only metadata_d2, sample Fe_d0_32 was wrongly named as Fe_d0_30 but it is only in day 2
ASV$Sample_ID[ASV$Sample_ID == 'Fe_d0_30'] <- 'Fe_d0_32'

#Extracting the donor ASV_table
Donor_ASV <- subset(ASV, day == "Donor")
#subset lyophilized donor, we will use this as donor for all samples since it is the sample the animals got as transplat
Donor_ASV <- subset(Donor_ASV, Sample_ID == "Donor_lyo")
Donor_ASV <- Donor_ASV[,-1:-3]

#ASV_table present in donor
Donor_ASV <- as.data.frame(t(as.matrix(Donor_ASV)))
Donor_ASV <- na.omit(Donor_ASV)
colnames(Donor_ASV) <- "Donor"
Donor_ASV <- tibble::rownames_to_column(Donor_ASV, "Taxa")

#Loop for extracting outcome features
#Remove the donor from the ASV tables first
ASV <- ASV[-1:-6,]

#Since day 0 and 2 have same length (40), while day 5 and 7 have the same length (20) 
#I will create new dataframe with those combinations

pig_ID <- unique(ASV$ID)
days <- unique(ASV$day)

for(i in 1:length(pig_ID)) {
  print(paste("Pig", pig_ID[i]))
  
  Pig <- subset(ASV, ID == pig_ID[i])
  d0_Pig <- subset(Pig, day == "0")
  d0_Pig <- d0_Pig[,-1:-3]
  
  #AASV_table present in day 0 in pig 1
  d0_Pig <- as.data.frame(t(as.matrix(d0_Pig)))
  d0_Pig <- na.omit(d0_Pig)
  colnames(d0_Pig) <- paste0("Pig_d0")
  d0_Pig <- tibble::rownames_to_column(d0_Pig, "Taxa")
  
  #Extract unique AASV_table to donor and not in day 0, and shared AASV_table between donor and d0 and ASV in d0 alone 
  Unique_donor <- anti_join(Donor_ASV, d0_Pig, by = c("Taxa"="Taxa"))
  shared_ASV <- inner_join(Donor_ASV, d0_Pig, by = c("Taxa"="Taxa"))
  d0_ASV <- anti_join(d0_Pig, Donor_ASV, by = c("Taxa"="Taxa"))
  
  
  for(d in 2:length(days)) {
    print(paste("Day", days[d]))
    
    #subset pig by day
    Pig2 <- Pig[which(Pig$day %in% days[d]),]
    
    #Remove metadata column for sample ID and day
    Pig2 <- Pig2[,-1:-3]
    
    #Remove NAs
    Pig2 <- as.data.frame(t(as.matrix(Pig2)))
    
    Pig2 <- na.omit(Pig2)
    colnames(Pig2) <- "Count"
    Pig2$Day <- days[d]
    Pig2$Pig_ID <- paste0("Pig", pig_ID[i])
    Pig2 <- tibble::rownames_to_column(Pig2, "Taxa")
    
    temp_colonizers <- merge(Unique_donor, Pig2, by="Taxa")
    temp_rejecters <- anti_join(Unique_donor, Pig2, by = c("Taxa"="Taxa"))
    temp_rejecters$Day <- paste0(days[d])
    temp_rejecters$Pig_ID <- paste0("Pig", pig_ID[i])
    temp_persisters <- inner_join(d0_ASV, Pig2, by = c("Taxa"="Taxa"))
    temp_co_existers <- inner_join(shared_ASV, Pig2, by = c("Taxa"="Taxa"))
    temp_novel <- anti_join(Pig2, Unique_donor, by = c("Taxa"="Taxa"))
    temp_novel <- anti_join(temp_novel, d0_ASV, by = c("Taxa"="Taxa"))
    temp_novel <- anti_join(temp_novel, shared_ASV, by = c("Taxa"="Taxa"))
    temp_loss <- inner_join(d0_ASV, Pig2, by = c("Taxa"="Taxa"))
    
    if (exists("colonizers")){
      colonizers <- rbind(colonizers, temp_colonizers)
    } else {
      colonizers <- temp_colonizers
    }
    
    if (exists("rejecters")){
      rejecters <- rbind(rejecters, temp_rejecters)
    } else {
      rejecters <- temp_rejecters
    }
    
    if (exists("persisters")){
      persisters <- rbind(persisters, temp_persisters)
    } else {
      persisters <- temp_persisters
    }
    
    if (exists("co_existers")){
      co_existers <- rbind(co_existers, temp_co_existers)
    } else {
      co_existers <- temp_co_existers
    }
    
    if (exists("novel")){
      novel <- rbind(novel, temp_novel)
    } else {
      novel <- temp_novel
    }
    
    if (exists("loss")){
      loss <- rbind(loss, temp_loss)
    } else {
      loss <- temp_loss
    }
  }
}

#Go back and run from metadat_d5 before running the next line pf codes
# Add a new column for the outcome that they are
colonizers$type <- paste0("colonizers")
persisters$type <- paste0("persisters")
co_existers$type <- paste0("co_existers")
rejecters$type <- paste0("rejecters")
novel$type <- paste0("novel")
loss$type <- paste0("loss")

# make the number of variables in each dataframe the same
colonizers <- colonizers[,-2:-3]
persisters <- persisters[,-2:-3]
co_existers <- co_existers[,-2:-4]
rejecters <- rejecters[,-2]
novel <- novel[,-2]
loss <- loss[,-2:-3]

write.table(colonizers, file = "colonizers.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(persisters, file = "persisters.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(co_existers, file = "co_existers.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(rejecters, file = "rejecters.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(novel, file = "novel.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(loss, file = "loss.txt",quote = F,sep = '\t', row.names = T, col.names = T)


#combine all the outcomes
outcomes <- rbind(colonizers, co_existers, persisters, rejecters, novel, loss)
write.table(outcomes, file = "fmt_outcomes_metrics.txt",quote = F,sep = '\t', row.names = T, col.names = T)

#group the outcomes by day, Pig_ID and type of outcome
outcomes2 <- outcomes %>% group_by(Day, Pig_ID, type) %>% dplyr::summarise(count=n())
outcomes2 <- as.data.frame(outcomes %>% group_by(Day, Pig_ID, type) %>% dplyr::summarise(count=n()))

# split the different outcome
outcomes3 <- split(outcomes2, outcomes2$type)
#make the different outcomes into a dataframe so they can be in different variables 
outcomes4 <- as.data.frame(outcomes3)

outcomes4 <- rowSums(outcomes4)

outcomes4 <- outcomes4[,-3]
outcomes4 <- outcomes4[,-4:-6]
outcomes4 <- outcomes4[,-5:-7]
outcomes4 <- outcomes4[,-6:-8]
outcomes4 <- outcomes4[,-7:-9]
outcomes4 <- outcomes4[,-8:-10]

str(outcomes4)
#rename the columns in the correct name
outcomes5 <- outcomes4 %>% dplyr::rename("Day" = "co_existers.Day",
                                         "Pig_ID" = "co_existers.Pig_ID",
                                         "co_existers" = "co_existers.count",
                                         "colonizers" = "colonizers.count",
                                         "loss" = "loss.count",
                                         "novel" = "novel.count",
                                         "persisters" = "persisters.count",
                                         "rejecters" = "rejecters.count")

#split the Pig_ID column into two
outcomes6 <- outcomes5 %>% separate(Pig_ID,
                                    into = c("Pig", "ID"),
                                    sep = "(?<=[A-Za-z])(?=[0-9])"
)

outcomes6 <- outcomes6[,-2]

outcomes6$Sample_ID <- paste0("Fe_d")
outcomes6$Sample <- paste(outcomes6$Day, outcomes6$ID, sep="_")
outcomes6$Sample_ID <- paste(outcomes6$Sample_ID,outcomes6$Sample, sep="")

outcomes6 <- outcomes6[,-10]

outcomes6$ASV <- rowSums(outcomes6[, c(3,4,5,6,7,8)])

str(outcomes6)
outcomes6$co_existers_norm <- outcomes6$co_existers/outcomes6$ASV
outcomes6$colonizers_norm <- outcomes6$colonizers/outcomes6$ASV
outcomes6$loss_norm <- outcomes6$loss/outcomes6$ASV
outcomes6$novel_norm <- outcomes6$novel/outcomes6$ASV
outcomes6$persisters_norm <- outcomes6$persisters/outcomes6$ASV
outcomes6$rejecters_norm <- outcomes6$rejecters/outcomes6$ASV

write.table(outcomes6, file = "fmt_outcomes.txt",quote = F,sep = '\t', row.names = T, col.names = T)


#Fmt outcomefigure

library("tidyverse", warn.conflicts=F, quietly=T)
library("cowplot", warn.conflicts=F, quietly=T)
library("egg", warn.conflicts=F, quietly=T)

outcome <- read.table("fmt_outcome_comb.txt", header=TRUE, sep="\t")
str(outcome)
head(outcome)

unique(outcome$Sample_ID)

outcome$Sample_ID <- factor(outcome$Sample_ID, levels = c("Fe_2_1", "Fe_2_2","Fe_2_3", "Fe_2_4", "Fe_2_5", "Fe_2_25", "Fe_2_26", "Fe_2_27", "Fe_2_28", "Fe_2_29", "Fe_5_2", "Fe_5_4", "Fe_5_25", "Fe_5_27", "Fe_5_28", "Fe_7_2", "Fe_7_4", "Fe_7_25", "Fe_7_27", "Fe_7_28",
                                                          "Fe_2_8", "Fe_2_9", "Fe_2_10", "Fe_2_11", "Fe_2_12", "Fe_2_32", "Fe_2_33", "Fe_2_34", "Fe_2_35", "Fe_2_36", "Fe_5_8", "Fe_5_11", "Fe_5_12", "Fe_5_33", "Fe_5_34", "Fe_7_8", "Fe_7_11", "Fe_7_12", "Fe_7_33", "Fe_7_34",
                                                          "Fe_2_13", "Fe_2_14", "Fe_2_15", "Fe_2_16", "Fe_2_17", "Fe_2_37", "Fe_2_38", "Fe_2_39", "Fe_2_40", "Fe_2_41", "Fe_5_13", "Fe_5_15", "Fe_5_37", "Fe_5_40", "Fe_5_41", "Fe_7_13", "Fe_7_15", "Fe_7_37", "Fe_7_40", "Fe_7_41",
                                                          "Fe_2_20", "Fe_2_21", "Fe_2_22", "Fe_2_23", "Fe_2_24", "Fe_2_44", "Fe_2_45", "Fe_2_46", "Fe_2_47", "Fe_2_48", "Fe_5_21", "Fe_5_23", "Fe_5_44", "Fe_5_46", "Fe_5_48", "Fe_7_21", "Fe_7_23", "Fe_7_44", "Fe_7_46", "Fe_7_48"))

outcome$type <- factor(outcome$type, levels = c("colonizers", "rejecters", "co_existers", "persisters", "novel", "loss"))

head(outcome)

ggplot(outcome, aes(x = Sample_ID, y = outcomes, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("colonizers" = "#440154FF", "rejecters" = "#5DC863FF", "co_existers" = "#26818EFF", "persisters" = "#fff7bc", "novel" = "#d95f0e", "loss" = "#FDE725FF")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#geom_hline(yintercept = 0, colour = "#FFFF33", linewidth = 0.2)

ggsave(paste0("output/FMT-outcomes4", ".pdf"), height=3, width=10, device="pdf")


# Correlation of colonization with ADG and BW
setwd("~/Desktop/Tim-Lab/FMT_Study_1/new_output")
Performance <- read.table("Performance.txt", header=TRUE, sep="\t")
Performance <- subset(Performance, day == "2")

setwd("~/Desktop/Tim-Lab/FMT_Study_1/Results/16S/FMT_outcome")
outcome <- read.table("fmt_outcome_comb.txt", header=TRUE, sep="\t")  
outcome <- subset(outcome, type == "colonizers")
outcome <- subset(outcome, day == "2")

out_per <- full_join(Performance, outcome, by = c("Sample_ID"="Sample_ID"))
out_per <- out_per[,-9:-15]
out_per <- na.omit(out_per)

out_per_control <- subset(out_per, Trt == "Control")
out_per_oral <- subset(out_per, Trt == "Oral")
out_per_rectal <- subset(out_per, Trt == "Rectal")
out_per_in_feed <- subset(out_per, Trt == "In-feed")

cor(out_per_control$ADG, out_per_control$outcomes, method = c("pearson"))
cor.test(out_per_control$ADG, out_per_control$outcomes, method=c("pearson"))

cor(out_per_control$BW, out_per_control$outcomes, method = c("pearson"))
cor.test(out_per_control$BW, out_per_control$outcomes, method=c("pearson"))


cor(out_per_oral$ADG, out_per_oral$outcomes, method = c("pearson"))
cor.test(out_per_oral$BW, out_per_oral$outcomes, method=c("pearson"))

cor(out_per_rectal$ADG, out_per_rectal$outcomes, method = c("pearson"))
cor.test(out_per_rectal$ADG, out_per_rectal$outcomes, method=c("pearson"))

cor(out_per_in_feed$ADG, out_per_in_feed$outcomes, method = c("pearson"))
cor.test(out_per_in_feed$ADG, out_per_in_feed$outcomes, method=c("pearson"))


ggplot(data = out_per, aes (y = ADG, x = outcomes, group = Trt, colour = Trt)) +  geom_point() + geom_smooth(method = "lm", se = FALSE) + scale_color_brewer(palette = "Dark2") + theme_classic() + xlab ("Number of cases") + ylab ("Number of deaths")
ggplot(data = out_per, aes (y = BW, x = outcomes, group = Trt, colour = Trt)) +  geom_point() + geom_smooth(method = "lm", se = TRUE) + scale_color_brewer(palette = "Dark2") + theme_classic() + xlab ("Number of cases") + ylab ("Number of deaths")


# Elastic net regression

library(dplyr)
library(glmnet)
library(ggplot2)
library(caret)

summary(mtcars)
str(mtcars)

X <- mtcars %>% 
  dplyr::select(disp) %>% 
  scale(center = TRUE, scale = FALSE) %>% 
  as.matrix()

Y <- mtcars %>% 
  dplyr::select(-disp) %>% 
  as.matrix()

# Model Building : Elastic Net Regression
# number = number of cross-validation, repeated = how many times cross-validation will be repeated
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

# Training ELastic Net Regression model
# caret tries different alpha and lamnda value and chooses the best one
# tuneLength = the number of time it will search each value of alpha and lamda
elastic_model <- train(disp ~ .,
                       data = cbind(X, Y),
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 25,
                       trControl = control)

elastic_model

# Model Prediction
x_hat_pre <- predict(elastic_model, Y)
x_hat_pre


# Multiple R-squared
rsq <- cor(X, x_hat_pre)^2
rsq

# Plot
plot(elastic_model, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_model, scale = TRUE))


# modeling the fmt outcomes
#setting up independent variables

setwd("~/Desktop/Tim-Lab/FMT_Study_1/new_output")
metadata <- read.table("metadata.txt", header=TRUE, sep="\t")

#load OTU table
otu_table <- read_qza("observed_features_vector.qza")
#Extract column from OTU table
otu_table <- otu_table$data %>%
  dplyr::select(observed_features)
#otu_table <- tibble::rownames_to_column(otu_table, "SampleID")

#Load evenness
evenness <- read_qza("evenness_vector.qza")
#Extract column from evenness
evenness <- evenness$data%>%
  dplyr::select(pielou_evenness)
#evenness <- tibble::rownames_to_column(evenness, "SampleID")

#Load faith
faith <- read_qza("faith_pd_vector.qza")
#Extract column from faith 
faith <- faith$data%>%
  dplyr::select(V1,V2)
#in faith, the sample ID is named a separate colum and not as column name as in other metrics
colnames(faith) <- c("SampleID","faith")


#Jaccard
metadata <- read.table("metadata.txt", header=TRUE, sep="\t")
ja_PCoA<-read_qza("jaccard_pcoa_results.qza")
ja_meta <- ja_PCoA$data$Vectors %>%
  dplyr::select(SampleID, PC1, PC2) %>%
  inner_join(metadata, by = c("SampleID" = "SampleID"))

#Bray-curtis
metadata <- read.table("metadata.txt", header=TRUE, sep="\t")
bc_PCoA<-read_qza("bray_curtis_pcoa_results.qza")
bc_meta <- bc_PCoA$data$Vectors %>%
  dplyr::select(SampleID, PC1, PC2) %>%
  inner_join(metadata, by = c("SampleID" = "SampleID"))

#Unweighted UniFRAC.   unweighted_unifrac
metadata <- read.table("metadata.txt", header=TRUE, sep="\t")
uw_PCoA<-read_qza("unweighted_unifrac_pcoa_results.qza")
uw_meta <- uw_PCoA$data$Vectors %>%
  dplyr::select(SampleID, PC1, PC2) %>%
  inner_join(metadata, by = c("SampleID" = "SampleID"))

#Unweighted UniFRAC.   unweighted_unifrac
metadata <- read.table("metadata.txt", header=TRUE, sep="\t")
wu_PCoA<-read_qza("weighted_unifrac_pcoa_results.qza")
wu_meta <- uw_PCoA$data$Vectors %>%
  dplyr::select(SampleID, PC1, PC2) %>%
  inner_join(metadata, by = c("SampleID" = "SampleID"))

# merge the alpha diversity to form outcomes
outcomes_p <- merge(otu_table, evenness, by.x = 0, by.y = 0)
outcomes_p <- merge(outcomes_p, faith, by.x = "Row.names", by.y = "SampleID")

# merge the beta diversity to outcomes_p
outcomes_p <- merge(outcomes_p, bc_meta [ , c("SampleID", "PC1")], by.x = "Row.names", by.y = "SampleID")
library(dplyr)
# rename PC1 to the specific beta diversity metrics 
outcomes_p <- outcomes_p %>% 
  rename("BC" = "PC1")

outcomes_p <- merge(outcomes_p, ja_meta [ , c("SampleID", "PC1")], by.x = "Row.names", by.y = "SampleID")
outcomes_p <- outcomes_p %>% 
  rename("JAC" = "PC1")

outcomes_p <- merge(outcomes_p, uw_meta [ , c("SampleID", "PC1")], by.x = "Row.names", by.y = "SampleID")
outcomes_p <- outcomes_p %>% 
  rename("UNWE" = "PC1")

outcomes_p <- merge(outcomes_p, wu_meta [ , c("SampleID", "PC1")], by.x = "Row.names", by.y = "SampleID")
outcomes_p <- outcomes_p %>% 
  rename("WE" = "PC1")

outcomes_p2 <- merge(outcomes_p, metadata [ , c("SampleID", "day", "type", "trt")], by.x = "Row.names", by.y = "SampleID")
outcomes_p3 <- subset(outcomes_p2, type == "fecal")


setwd("~/Desktop/Tim-Lab/FMT_Study_1/Results/16S/FMT_outcome")
# Load the rarefied feature table collapsed to genus level, the same was used to make the outcome metrics
ASV_table <- read.table("feature-rarefied_table-l6.tsv", header=TRUE, sep="\t")
#Remove cecum and colon samples
ASV_table <- ASV_table[,-2:-41]
ASV_table <- ASV_table[,-8:-47]

#To remove either donor and other days except day0
ASV_table <- ASV_table[,-2:-7]
ASV_table <- ASV_table[,-42:-121]

# rename the rowname to be the first column
rownames(ASV_table) <- ASV_table[,1]

# delete the first column
ASV_table <- ASV_table[,-1]

# make the count percentage
ASV_table<-apply(ASV_table, 2, function(x) x/sum(x)*100)
ASV_table <- as.data.frame(ASV_table)
str(ASV_table)

# get the mean abundance of each genus
ASV_table2 <- 
  mutate(data.frame(MeanAbundance=rowMeans(ASV_table)))

# make th rowname to be the first column
ASV_table2 <- rownames_to_column(ASV_table2, var="Feature.ID")

ASV_table <- rownames_to_column(ASV_table, var="Feature.ID")

# merge the mean abundance to the percentage abundance of ASV
ASV_table3 <- full_join(ASV_table,ASV_table2, by = c("Feature.ID"="Feature.ID"))

# arrange the ASVs in descending order and filter the top 50 
ASV_table3 <- arrange(ASV_table3,desc(MeanAbundance))
ASV_table4 <- top_n(ASV_table3, 20, MeanAbundance)

# transpose
ASV_table5 <- as.data.frame(t(as.matrix(ASV_table4))) 
colnames(ASV_table5) <- ASV_table5 [1,]
ASV_table5 <- ASV_table5 [-1,]
ASV_table5 <- rownames_to_column(ASV_table5, var="SampleID")
ASV_table6 <- ASV_table5 [-41,]

#merge 
outcomes_p4 <- merge(outcomes_p3, ASV_table5, by.x = "Row.names", by.y = "SampleID")
#outcomes_p5 <- outcomes_p4[-1:-6,]
write.table(outcomes_p4, file = "fmt_outcomes_predictors_d0.txt",quote = F,sep = '\t', row.names = T, col.names = T)

# FMT outcomes model
setwd("~/Desktop/Tim-Lab/FMT_Study_1/Results/16S/FMT_outcome")

# For colonization
pred_all <- read.table("fmt_model_pred.txt", header=TRUE, sep="\t")
rownames(pred_all) <- pred_all[,1]

# Delete the colum SampleID
pred_all <- pred_all[,-1]
pred_all <- pred_all[,-25:-34]

# Subset for control in d2 full model
pred_all_d2_control_colonizers <- subset(pred_all, trt == 'Control')
# Delete colums for treatment 
pred_all_d2_control_colonizers <- pred_all_d2_control_colonizers[,-1]
# Delete other response variables but retain colonizers which will be used for prediction
pred_all_d2_control_colonizers <- pred_all_d2_control_colonizers[,-1]
pred_all_d2_control_colonizers <- pred_all_d2_control_colonizers[,-2:-5]


library(dplyr)
library(glmnet)
library(ggplot2)
library(caret)

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_colonizer <- train(colonizers_norm ~ .,
                                      data = pred_all_d2_control_colonizers,
                                      method = "glmnet",
                                      preProcess = c("center", "scale"),
                                      tuneLength = 10,
                                      trControl = control)

elastic_d2_control_colonizer

co_control_colonizer <- coef(elastic_d2_control_colonizer$finalModel, elastic_d2_control_colonizer$bestTune$lambda)
#co_control_colonizer = as.data.frame(summary(co_control_colonizer))

co_control_colonizer = as.data.frame(as.matrix(co_control_colonizer))

#predict(elastic_d2_control_colonizer, type = "coef")[1:50,]

mean(elastic_d2_control_colonizer$resample$RMSE)
mean(elastic_d2_control_colonizer$resample$Rsquared)

# Model Prediction
x_hat_pre_control_colonizer <- predict(elastic_d2_control_colonizer, pred_all_d2_control_colonizers[,-1])
x_hat_pre_control_colonizer

# Multiple R-squared
rsq_control_colonizer <- cor(pred_all_d2_control_colonizers[,1], x_hat_pre_control_colonizer)^2
rsq_control_colonizer

# Plot
plot(elastic_d2_control_colonizer, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_colonizer, scale = TRUE))

ggsave(paste0("output/Import_control_colonizer_full", ".pdf"), height=10, width=12, device="pdf")

# d2 control alpha diversity
pred_all_d2_control_colonizers <- subset(pred_all, trt == 'Control')
# Delete colums for day and treatment 
pred_all_d2_control_colonizers_alpha <- pred_all_d2_control_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_control_colonizers_alpha <- pred_all_d2_control_colonizers_alpha[,-2:-5]
pred_all_d2_control_colonizers_alpha <- pred_all_d2_control_colonizers_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_colonizer_alpha <- train(colonizers_norm ~ .,
                                            data = pred_all_d2_control_colonizers_alpha,
                                            method = "glmnet",
                                            preProcess = c("center", "scale"),
                                            tuneLength = 10,
                                            trControl = control)

elastic_d2_control_colonizer_alpha

mean(elastic_d2_control_colonizer_alpha$resample$RMSE)
mean(elastic_d2_control_colonizer_alpha$resample$Rsquared)

# Model Prediction
x_hat_pre_control_colonizer_alpha <- predict(elastic_d2_control_colonizer_alpha, pred_all_d2_control_colonizers_alpha[,-1])
x_hat_pre_control_colonizer_alpha

# Multiple R-squared
rsq_control_colonizer_alpha <- cor(pred_all_d2_control_colonizers_alpha[,1], x_hat_pre_control_colonizer_alpha)^2
rsq_control_colonizer_alpha

# Plot
plot(elastic_d2_control_colonizer_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_colonizer_alpha, scale = TRUE))

ggsave(paste0("output/Import_control_colonizer_alpha", ".pdf"), height=10, width=12, device="pdf")

# d2 control beta diversity
pred_all_d2_control_colonizers <- subset(pred_all, trt == 'Control')
# Delete colums for day and treatment 
pred_all_d2_control_colonizers_beta <- pred_all_d2_control_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_control_colonizers_beta <- pred_all_d2_control_colonizers_beta[,-2:-8]
pred_all_d2_control_colonizers_beta <- pred_all_d2_control_colonizers_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_colonizer_beta <- train(colonizers_norm ~ .,
                                           data = pred_all_d2_control_colonizers_beta,
                                           method = "glmnet",
                                           preProcess = c("center", "scale"),
                                           tuneLength = 10,
                                           trControl = control)

elastic_d2_control_colonizer_beta

mean(elastic_d2_control_colonizer_beta$resample$RMSE)
mean(elastic_d2_control_colonizer_beta$resample$Rsquared)

# Model Prediction
x_hat_pre_control_colonizer_beta <- predict(elastic_d2_control_colonizer_beta, pred_all_d2_control_colonizers_beta[,-1])
x_hat_pre_control_colonizer_beta

# Multiple R-squared
rsq_control_colonizer_beta <- cor(pred_all_d2_control_colonizers_beta[,1], x_hat_pre_control_colonizer_beta)^2
rsq_control_colonizer_beta

# Plot
plot(elastic_d2_control_colonizer_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_colonizer_beta, scale = TRUE))

ggsave(paste0("output/Import_control_colonizer_beta", ".pdf"), height=10, width=12, device="pdf")

# d2 control taxa
pred_all_d2_control_colonizers <- subset(pred_all, trt == 'Control')
# Delete colums for day and treatment 
pred_all_d2_control_colonizers_taxa <- pred_all_d2_control_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_control_colonizers_taxa <- pred_all_d2_control_colonizers_taxa[,-2:-12]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_colonizer_taxa <- train(colonizers_norm ~ .,
                                           data = pred_all_d2_control_colonizers_taxa,
                                           method = "glmnet",
                                           preProcess = c("center", "scale"),
                                           tuneLength = 10,
                                           trControl = control)

elastic_d2_control_colonizer_taxa

mean(elastic_d2_control_colonizer_taxa$resample$RMSE)
mean(elastic_d2_control_colonizer_taxa$resample$Rsquared)

# Model Prediction
x_hat_pre_control_colonizer_taxa <- predict(elastic_d2_control_colonizer_taxa, pred_all_d2_control_colonizers_taxa[,-1])
x_hat_pre_control_colonizer_taxa

# Multiple R-squared
rsq_control_colonizer_taxa <- cor(pred_all_d2_control_colonizers_taxa[,1], x_hat_pre_control_colonizer_taxa)^2
rsq_control_colonizer_taxa

# Plot
plot(elastic_d2_control_colonizer_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_colonizer_taxa, scale = TRUE))

ggsave(paste0("output/Import_control_colonizer_taxa", ".pdf"), height=10, width=12, device="pdf")

# day2 Oral full model
pred_all_d2_Oral_colonizers <- subset(pred_all, trt == 'Oral')
# Delete colums for day and treatment 
pred_all_d2_Oral_colonizers <- pred_all_d2_Oral_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Oral_colonizers <- pred_all_d2_Oral_colonizers[,-2:-5]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_colonizer <- train(colonizers_norm ~ .,
                                   data = pred_all_d2_Oral_colonizers,
                                   method = "glmnet",
                                   preProcess = c("center", "scale"),
                                   tuneLength = 10,
                                   trControl = control)

elastic_d2_oral_colonizer

co_oral_colonizer <- coef(elastic_d2_oral_colonizer$finalModel, elastic_d2_oral_colonizer$bestTune$lambda)
#co_oral_colonizer = as.data.frame(summary(co_orall_colonizer))

co_oral_colonizer = as.data.frame(as.matrix(co_oral_colonizer))

# Model Prediction
x_hat_pre_oral_colonizer <- predict(elastic_d2_oral_colonizer, pred_all_d2_Oral_colonizers[,-1])
x_hat_pre_oral_colonizer

# Multiple R-squared
rsq_oral_colonizer <- cor(pred_all_d2_Oral_colonizers[,1], x_hat_pre_oral_colonizer)^2
rsq_oral_colonizer

# Plot
plot(elastic_d2_oral_colonizer, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_colonizer, scale = TRUE))

ggsave(paste0("output/Import_oral_colonizer_full", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_colonizer$resample$RMSE)
mean(elastic_d2_oral_colonizer$resample$Rsquared)

# day2 Oral alpha diversity
pred_all_d2_Oral_colonizers <- subset(pred_all, trt == 'Oral')
# Delete colums for day and treatment 
pred_all_d2_Oral_colonizers_alpha <- pred_all_d2_Oral_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Oral_colonizers_alpha <- pred_all_d2_Oral_colonizers_alpha[,-2:-5]
pred_all_d2_Oral_colonizers_alpha <- pred_all_d2_Oral_colonizers_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_colonizer_alpha <- train(colonizers_norm ~ .,
                                         data = pred_all_d2_Oral_colonizers_alpha,
                                         method = "glmnet",
                                         preProcess = c("center", "scale"),
                                         tuneLength = 10,
                                         trControl = control)

elastic_d2_oral_colonizer_alpha

# Model Prediction
x_hat_pre_oral_colonizer_alpha <- predict(elastic_d2_oral_colonizer_alpha, pred_all_d2_Oral_colonizers_alpha[,-1])
x_hat_pre_oral_colonizer_alpha

# Multiple R-squared
rsq_oral_colonizer_alpha <- cor(pred_all_d2_Oral_colonizers_alpha[,1], x_hat_pre_oral_colonizer_alpha)^2
rsq_oral_colonizer_alpha

# Plot
plot(elastic_d2_oral_colonizer_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_colonizer_alpha, scale = TRUE))

ggsave(paste0("output/Import_oral_colonizer_alpha", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_colonizer_alpha$resample$RMSE)
mean(elastic_d2_oral_colonizer_alpha$resample$Rsquared)

# day2 Oral beta diversity
pred_all_d2_Oral_colonizers <- subset(pred_all, trt == 'Oral')

# Delete colums for day and treatment 
pred_all_d2_Oral_colonizers_beta <- pred_all_d2_Oral_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Oral_colonizers_beta <- pred_all_d2_Oral_colonizers_beta[,-2:-8]
pred_all_d2_Oral_colonizers_beta <- pred_all_d2_Oral_colonizers_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_colonizer_beta <- train(colonizers_norm ~ .,
                                        data = pred_all_d2_Oral_colonizers_beta,
                                        method = "glmnet",
                                        preProcess = c("center", "scale"),
                                        tuneLength = 10,
                                        trControl = control)

elastic_d2_oral_colonizer_beta

# Model Prediction
x_hat_pre_oral_colonizer_beta <- predict(elastic_d2_oral_colonizer_beta, pred_all_d2_Oral_colonizers_beta[,-1])
x_hat_pre_oral_colonizer_beta

# Multiple R-squared
rsq_oral_colonizer_beta <- cor(pred_all_d2_Oral_colonizers_beta[,1], x_hat_pre_oral_colonizer_beta)^2
rsq_oral_colonizer_beta

# Plot
plot(elastic_d2_oral_colonizer_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_colonizer_beta, scale = TRUE))

ggsave(paste0("output/Import_oral_colonizer_beta", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_colonizer_beta$resample$RMSE)
mean(elastic_d2_oral_colonizer_beta$resample$Rsquared)

# day2 Oral taxa
pred_all_d2_Oral_colonizers <- subset(pred_all, trt == 'Oral')

# Delete colums for day and treatment 
pred_all_d2_Oral_colonizers_taxa <- pred_all_d2_Oral_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Oral_colonizers_taxa <- pred_all_d2_Oral_colonizers_taxa[,-2:-12]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_colonizer_taxa <- train(colonizers_norm ~ .,
                                        data = pred_all_d2_Oral_colonizers_taxa,
                                        method = "glmnet",
                                        preProcess = c("center", "scale"),
                                        tuneLength = 10,
                                        trControl = control)

elastic_d2_oral_colonizer_taxa

# Model Prediction
x_hat_pre_oral_colonizer_taxa <- predict(elastic_d2_oral_colonizer_taxa, pred_all_d2_Oral_colonizers_taxa[,-1])
x_hat_pre_oral_colonizer_taxa

# Multiple R-squared
rsq_oral_colonizer_taxa <- cor(pred_all_d2_Oral_colonizers_taxa[,1], x_hat_pre_oral_colonizer_taxa)^2
rsq_oral_colonizer_taxa

# Plot
plot(elastic_d2_oral_colonizer_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_colonizer_taxa, scale = TRUE))

ggsave(paste0("output/Import_oral_colonizer_taxa", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_colonizer_taxa$resample$RMSE)
mean(elastic_d2_oral_colonizer_taxa$resample$Rsquared)

# day2 Rectal full model
pred_all_d2_Rectal_colonizers <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_colonizers <- pred_all_d2_Rectal_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Rectal_colonizers <- pred_all_d2_Rectal_colonizers[,-2:-5]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_colonizer <- train(colonizers_norm ~ .,
                                     data = pred_all_d2_Rectal_colonizers,
                                     method = "glmnet",
                                     preProcess = c("center", "scale"),
                                     tuneLength = 10,
                                     trControl = control)

elastic_d2_rectal_colonizer

co_rectal_colonizer <- coef(elastic_d2_rectal_colonizer$finalModel, elastic_d2_rectal_colonizer$bestTune$lambda)
#co_rectal_colonizer = as.data.frame(summary(co_rectal_colonizer))

co_rectal_colonizer = as.data.frame(as.matrix(co_rectal_colonizer))

# Model Prediction
x_hat_pre_rectal_colonizer <- predict(elastic_d2_rectal_colonizer, pred_all_d2_Rectal_colonizers[,-1])
x_hat_pre_rectal_colonizer

# Multiple R-squared
rsq_rectal_colonizer <- cor(pred_all_d2_Rectal_colonizers[,1], x_hat_pre_rectal_colonizer)^2
rsq_rectal_colonizer

# Plot
plot(elastic_d2_rectal_colonizer, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_colonizer, scale = TRUE))

ggsave(paste0("output/Import_rectal_colonizer_full", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_colonizer$resample$RMSE)
mean(elastic_d2_rectal_colonizer$resample$Rsquared)

# d2 Rectal alpha diversity
pred_all_d2_Rectal_colonizers <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_colonizers_alpha <- pred_all_d2_Rectal_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Rectal_colonizers_alpha <- pred_all_d2_Rectal_colonizers_alpha[,-2:-5]
pred_all_d2_Rectal_colonizers_alpha <- pred_all_d2_Rectal_colonizers_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_colonizer_alpha <- train(colonizers_norm ~ .,
                                           data = pred_all_d2_Rectal_colonizers_alpha,
                                           method = "glmnet",
                                           preProcess = c("center", "scale"),
                                           tuneLength = 10,
                                           trControl = control)

elastic_d2_rectal_colonizer_alpha

# Model Prediction
x_hat_pre_rectal_colonizer_alpha <- predict(elastic_d2_rectal_colonizer_alpha, pred_all_d2_Rectal_colonizers_alpha[,-1])
x_hat_pre_rectal_colonizer_alpha

# Multiple R-squared
rsq_rectal_colonizer_alpha <- cor(pred_all_d2_Rectal_colonizers_alpha[,1], x_hat_pre_rectal_colonizer_alpha)^2
rsq_rectal_colonizer_alpha

# Plot
plot(elastic_d2_rectal_colonizer_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_colonizer_alpha, scale = TRUE))

ggsave(paste0("output/Import_rectal_colonizer_alpha", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_colonizer_alpha$resample$RMSE)
mean(elastic_d2_rectal_colonizer_alpha$resample$Rsquared)

# d2 Rectal beta diversity
pred_all_d2_Rectal_colonizers <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_colonizers_beta <- pred_all_d2_Rectal_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Rectal_colonizers_beta <- pred_all_d2_Rectal_colonizers_beta[,-2:-8]
pred_all_d2_Rectal_colonizers_beta <- pred_all_d2_Rectal_colonizers_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_colonizer_beta <- train(colonizers_norm ~ .,
                                          data = pred_all_d2_Rectal_colonizers_beta,
                                          method = "glmnet",
                                          preProcess = c("center", "scale"),
                                          tuneLength = 10,
                                          trControl = control)

elastic_d2_rectal_colonizer_beta

# Model Prediction
x_hat_pre_rectal_colonizer_beta <- predict(elastic_d2_rectal_colonizer_beta, pred_all_d2_Rectal_colonizers_beta[,-1])
x_hat_pre_rectal_colonizer_beta

# Multiple R-squared
rsq_rectal_colonizer_beta <- cor(pred_all_d2_Rectal_colonizers_beta[,1], x_hat_pre_rectal_colonizer_beta)^2
rsq_rectal_colonizer_beta

# Plot
plot(elastic_d2_rectal_colonizer_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_colonizer_beta, scale = TRUE))

ggsave(paste0("output/Import_rectal_colonizer_beta", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_colonizer_beta$resample$RMSE)
mean(elastic_d2_rectal_colonizer_beta$resample$Rsquared)

# d2 Rectal taxa
pred_all_d2_Rectal_colonizers <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_colonizers_taxa <- pred_all_d2_Rectal_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Rectal_colonizers_taxa <- pred_all_d2_Rectal_colonizers_taxa[,-2:-12]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_colonizer_taxa <- train(colonizers_norm ~ .,
                                          data = pred_all_d2_Rectal_colonizers_taxa,
                                          method = "glmnet",
                                          preProcess = c("center", "scale"),
                                          tuneLength = 10,
                                          trControl = control)

elastic_d2_rectal_colonizer_taxa

# Model Prediction
x_hat_pre_rectal_colonizer_taxa <- predict(elastic_d2_rectal_colonizer_taxa, pred_all_d2_Rectal_colonizers_taxa[,-1])
x_hat_pre_rectal_colonizer_taxa

# Multiple R-squared
rsq_rectal_colonizer_taxa <- cor(pred_all_d2_Rectal_colonizers_taxa[,1], x_hat_pre_rectal_colonizer_taxa)^2
rsq_rectal_colonizer_taxa

# Plot
plot(elastic_d2_rectal_colonizer_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_colonizer_taxa, scale = TRUE))

ggsave(paste0("output/Import_rectal_colonizer_full", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_colonizer_taxa$resample$RMSE)
mean(elastic_d2_rectal_colonizer_taxa$resample$Rsquared)

# day 2 In-feed full model
pred_all_d2_In_feed_colonizers <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_colonizers <- pred_all_d2_In_feed_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_In_feed_colonizers <- pred_all_d2_In_feed_colonizers[,-2:-5]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_colonizer <- train(colonizers_norm ~ .,
                                      data = pred_all_d2_In_feed_colonizers,
                                      method = "glmnet",
                                      preProcess = c("center", "scale"),
                                      tuneLength = 10,
                                      trControl = control)

elastic_d2_In_feed_colonizer

co_In_feed_colonizer <- coef(elastic_d2_In_feed_colonizer$finalModel, elastic_d2_In_feed_colonizer$bestTune$lambda)
#co_In_feed_colonizer = as.data.frame(summary(co_In_feed_colonizer))

co_In_feed_colonizer = as.data.frame(as.matrix(co_In_feed_colonizer))

# Model Prediction
x_hat_pre_In_feed_colonizer <- predict(elastic_d2_In_feed_colonizer, pred_all_d2_In_feed_colonizers[,-1])
x_hat_pre_In_feed_colonizer

# Multiple R-squared
rsq_In_feed_colonizer <- cor(pred_all_d2_In_feed_colonizers[,1], x_hat_pre_In_feed_colonizer)^2
rsq_In_feed_colonizer

# Plot
plot(elastic_d2_In_feed_colonizer, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_colonizer, scale = TRUE))

ggsave(paste0("output/Import_In-feed_colonizer_taxa", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_colonizer$resample$RMSE)
mean(elastic_d2_In_feed_colonizer$resample$Rsquared)

# day 2 In-feed alpha
pred_all_d2_In_feed_colonizers <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_colonizers_alpha <- pred_all_d2_In_feed_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_In_feed_colonizers_alpha <- pred_all_d2_In_feed_colonizers_alpha[,-2:-5]
pred_all_d2_In_feed_colonizers_alpha <- pred_all_d2_In_feed_colonizers_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_colonizer_alpha <- train(colonizers_norm ~ .,
                                            data = pred_all_d2_In_feed_colonizers_alpha,
                                            method = "glmnet",
                                            preProcess = c("center", "scale"),
                                            tuneLength = 10,
                                            trControl = control)

elastic_d2_In_feed_colonizer_alpha

# Model Prediction
x_hat_pre_In_feed_colonizer_alpha <- predict(elastic_d2_In_feed_colonizer_alpha, pred_all_d2_In_feed_colonizers_alpha[,-1])
x_hat_pre_In_feed_colonizer_alpha

# Multiple R-squared
rsq_In_feed_colonizer_alpha <- cor(pred_all_d2_In_feed_colonizers_alpha[,1], x_hat_pre_In_feed_colonizer_alpha)^2
rsq_In_feed_colonizer_alpha

# Plot
plot(elastic_d2_In_feed_colonizer_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_colonizer_alpha, scale = TRUE))

ggsave(paste0("output/Import_In-feed_colonizer_alpha", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_colonizer_alpha$resample$RMSE)
mean(elastic_d2_In_feed_colonizer_alpha$resample$Rsquared)

# day 2 In-feed beta
pred_all_d2_In_feed_colonizers <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_colonizers_beta <- pred_all_d2_In_feed_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_In_feed_colonizers_beta <- pred_all_d2_In_feed_colonizers_beta[,-2:-8]
pred_all_d2_In_feed_colonizers_beta <- pred_all_d2_In_feed_colonizers_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_colonizer_beta <- train(colonizers_norm ~ .,
                                           data = pred_all_d2_In_feed_colonizers_beta,
                                           method = "glmnet",
                                           preProcess = c("center", "scale"),
                                           tuneLength = 10,
                                           trControl = control)

elastic_d2_In_feed_colonizer_beta

# Model Prediction
x_hat_pre_In_feed_colonizer_beta <- predict(elastic_d2_In_feed_colonizer_beta, pred_all_d2_In_feed_colonizers_beta[,-1])
x_hat_pre_In_feed_colonizer_beta

# Multiple R-squared
rsq_In_feed_colonizer_beta <- cor(pred_all_d2_In_feed_colonizers_beta[,1], x_hat_pre_In_feed_colonizer_beta)^2
rsq_In_feed_colonizer_beta

# Plot
plot(elastic_d2_In_feed_colonizer_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_colonizer_beta, scale = TRUE))

ggsave(paste0("output/Import_In-feed_colonizer_beta", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_colonizer_beta$resample$RMSE)
mean(elastic_d2_In_feed_colonizer_beta$resample$Rsquared)

# day 2 In-feed taxa
pred_all_d2_In_feed_colonizers <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_colonizers_taxa <- pred_all_d2_In_feed_colonizers[,-1:-2]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_In_feed_colonizers_taxa <- pred_all_d2_In_feed_colonizers_taxa[,-2:-12]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_colonizer_taxa <- train(colonizers_norm ~ .,
                                           data = pred_all_d2_In_feed_colonizers_taxa,
                                           method = "glmnet",
                                           preProcess = c("center", "scale"),
                                           tuneLength = 10,
                                           trControl = control)

elastic_d2_In_feed_colonizer_taxa

# Model Prediction
x_hat_pre_In_feed_colonizer_taxa <- predict(elastic_d2_In_feed_colonizer_taxa, pred_all_d2_In_feed_colonizers_taxa[,-1])
x_hat_pre_In_feed_colonizer_taxa

# Multiple R-squared
rsq_In_feed_colonizer_taxa <- cor(pred_all_d2_In_feed_colonizers_taxa[,1], x_hat_pre_In_feed_colonizer_taxa)^2
rsq_In_feed_colonizer_taxa

# Plot
plot(elastic_d2_In_feed_colonizer_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_colonizer_taxa, scale = TRUE))

ggsave(paste0("output/Import_In-feed_colonizer_taxa", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_colonizer_taxa$resample$RMSE)
mean(elastic_d2_In_feed_colonizer_taxa$resample$Rsquared)


# Rejecters
#setwd("~/Desktop/Tim-Lab/FMT_Study_1/Results/16S/FMT_outcome")
# Load data
#pred_all <- read.table("fmt_model_pred_all.txt", header=TRUE, sep="\t")
#pred_all <- read.table("fmt_model_pred_by_d0.txt", header=TRUE, sep="\t")
#rownames(pred_all) <- pred_all[,1]

# Delete the colum SampleID
# pred_all <- pred_all[,-1]

# subset by day and treatment
# pred_all_d2 <- subset(pred_all, Day == '2')
# pred_all_d5 <- subset(pred_all, Day == '5')
# pred_all_d7 <- subset(pred_all, Day == '7')

# Subset for control in d2 full model
pred_all_d2_control_rejecters <- subset(pred_all, trt == 'Control')
# Delete colums for day and treatment 
pred_all_d2_control_rejecters <- pred_all_d2_control_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_control_rejecters <- pred_all_d2_control_rejecters[,-2:-5]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_rejecter <- train(rejecters_norm ~ .,
                                     data = pred_all_d2_control_rejecters,
                                     method = "glmnet",
                                     preProcess = c("center", "scale"),
                                     tuneLength = 5,
                                     trControl = control)

elastic_d2_control_rejecter

co_control_rejecter <- coef(elastic_d2_control_rejecter$finalModel, elastic_d2_control_rejecter$bestTune$lambda)
#co_control_rejecter = as.data.frame(summary(co_control_rejecter))

co_control_rejecter = as.data.frame(as.matrix(co_control_rejecter))

mean(elastic_d2_control_rejecter$resample$RMSE)
mean(elastic_d2_control_rejecter$resample$Rsquared)

# Model Prediction
x_hat_pre_control_rejecter <- predict(elastic_d2_control_rejecter, pred_all_d2_control_rejecters[,-1])
x_hat_pre_control_rejecter

# Multiple R-squared
rsq_control_rejecter <- cor(pred_all_d2_control_rejecters[,1], x_hat_pre_control_rejecter)^2
rsq_control_rejecter

# Plot
plot(elastic_d2_control_rejecter, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_rejecter, scale = TRUE))

ggsave(paste0("output/Import_control_rejecter_full", ".pdf"), height=10, width=12, device="pdf")

# d2 control rejecters alpha diversity
pred_all_d2_control_rejecters <- subset(pred_all, trt == 'Control')
# Delete colums for day and treatment 
pred_all_d2_control_rejecters_alpha <- pred_all_d2_control_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_control_rejecters_alpha <- pred_all_d2_control_rejecters_alpha[,-2:-5]
pred_all_d2_control_rejecters_alpha <- pred_all_d2_control_rejecters_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_rejecter_alpha <- train(rejecters_norm ~ .,
                                           data = pred_all_d2_control_rejecters_alpha,
                                           method = "glmnet",
                                           preProcess = c("center", "scale"),
                                           tuneLength = 10,
                                           trControl = control)

elastic_d2_control_rejecters_alpha

mean(elastic_d2_control_rejecter_alpha$resample$RMSE)
mean(elastic_d2_control_rejecter_alpha$resample$Rsquared)

# Model Prediction
x_hat_pre_control_rejecter_alpha <- predict(elastic_d2_control_rejecter_alpha, pred_all_d2_control_rejecters_alpha[,-1])
x_hat_pre_control_rejecter_alpha

# Multiple R-squared
rsq_control_rejecter_alpha <- cor(pred_all_d2_control_rejecters_alpha[,1], x_hat_pre_control_rejecter_alpha)^2
rsq_control_rejecter_alpha

# Plot
plot(elastic_d2_control_rejecter_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_rejecter_alpha, scale = TRUE))

ggsave(paste0("output/Import_control_rejecter_alpha", ".pdf"), height=10, width=12, device="pdf")

# d2 control rejecters beta diversity
pred_all_d2_control_rejecters <- subset(pred_all, trt == 'Control')
# Delete colums for day and treatment 
pred_all_d2_control_rejecters_beta <- pred_all_d2_control_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_control_rejecters_beta <- pred_all_d2_control_rejecters_beta[,-2:-4]
pred_all_d2_control_rejecters_beta <- pred_all_d2_control_rejecters_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_rejecter_beta <- train(rejecters_norm ~ .,
                                          data = pred_all_d2_control_rejecters_beta,
                                          method = "glmnet",
                                          preProcess = c("center", "scale"),
                                          tuneLength = 10,
                                          trControl = control)

elastic_d2_control_rejecter_beta

mean(elastic_d2_control_rejecter_beta$resample$RMSE)
mean(elastic_d2_control_rejecter_beta$resample$Rsquared)

# Model Prediction
x_hat_pre_control_rejecter_beta <- predict(elastic_d2_control_rejecter_beta, pred_all_d2_control_rejecters_beta[,-1])
x_hat_pre_control_rejecter_beta

# Multiple R-squared
rsq_control_rejecter_beta <- cor(pred_all_d2_control_rejecters_beta[,1], x_hat_pre_control_rejecter_beta)^2
rsq_control_rejecter_beta

# Plot
plot(elastic_d2_control_rejecter_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_rejecter_beta, scale = TRUE))

ggsave(paste0("output/Import_control_rejecter_beta", ".pdf"), height=10, width=12, device="pdf")

# d2 control taxa
pred_all_d2_control_rejecters <- subset(pred_all, trt == 'Control')
# Delete colums for day and treatment 
pred_all_d2_control_rejecters_taxa <- pred_all_d2_control_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_control_rejecters_taxa <- pred_all_d2_control_rejecters_taxa[,-2:-8]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_control_rejecter_taxa <- train(rejecters_norm ~ .,
                                          data = pred_all_d2_control_rejecters_taxa,
                                          method = "glmnet",
                                          preProcess = c("center", "scale"),
                                          tuneLength = 10,
                                          trControl = control)

elastic_d2_control_rejecter_taxa

mean(elastic_d2_control_rejecter_taxa$resample$RMSE)
mean(elastic_d2_control_rejecter_taxa$resample$Rsquared)

# Model Prediction
x_hat_pre_control_rejecter_taxa <- predict(elastic_d2_control_rejecter_taxa, pred_all_d2_control_rejecters_taxa[,-1])
x_hat_pre_control_rejecter_taxa

# Multiple R-squared
rsq_control_rejecter_taxa <- cor(pred_all_d2_control_rejecters_taxa[,1], x_hat_pre_control_rejecter_taxa)^2
rsq_control_rejecter_taxa

# Plot
plot(elastic_d2_control_rejecter_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_control_rejecter_taxa, scale = TRUE))

ggsave(paste0("output/Import_control_rejecter_taxa", ".pdf"), height=10, width=12, device="pdf")

# day2 Oral full model
pred_all_d2_Oral_rejecters <- subset(pred_all, trt == 'Oral')
# Delete colums for day and treatment 
pred_all_d2_Oral_rejecters <- pred_all_d2_Oral_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_Oral_rejecters <- pred_all_d2_Oral_rejecters[,-2:-5]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_rejecter <- train(rejecters_norm ~ .,
                                  data = pred_all_d2_Oral_rejecters,
                                  method = "glmnet",
                                  preProcess = c("center", "scale"),
                                  tuneLength = 10,
                                  trControl = control)

elastic_d2_oral_rejecter

co_oral_rejecter <- coef(elastic_d2_oral_rejecter$finalModel, elastic_d2_oral_rejecter$bestTune$lambda)
#co_oral_rejecter = as.data.frame(summary(co_oral_rejecter))

co_oral_rejecter = as.data.frame(as.matrix(co_oral_rejecter))

# Model Prediction
x_hat_pre_oral_rejecter <- predict(elastic_d2_oral_rejecter, pred_all_d2_Oral_rejecters[,-1])
x_hat_pre_oral_rejecter

# Multiple R-squared
rsq_oral_rejecter <- cor(pred_all_d2_Oral_rejecters[,1], x_hat_pre_oral_rejecter)^2
rsq_oral_rejecter

# Plot
plot(elastic_d2_oral_rejecter, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_rejecter, scale = TRUE))

ggsave(paste0("output/Import_oral_rejecter_full", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_rejecter$resample$RMSE)
mean(elastic_d2_oral_rejecter$resample$Rsquared)

# day2 Oral alpha diversity
pred_all_d2_Oral_rejecters <- subset(pred_all, trt == 'Oral')
# Delete colums for day and treatment 
pred_all_d2_Oral_rejecters_alpha <- pred_all_d2_Oral_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_Oral_rejecters_alpha <- pred_all_d2_Oral_rejecters_alpha[,-2:-5]
pred_all_d2_Oral_rejecters_alpha <- pred_all_d2_Oral_rejecters_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_rejecter_alpha <- train(rejecters_norm ~ .,
                                        data = pred_all_d2_Oral_rejecters_alpha,
                                        method = "glmnet",
                                        preProcess = c("center", "scale"),
                                        tuneLength = 10,
                                        trControl = control)

elastic_d2_oral_rejecter_alpha

# Model Prediction
x_hat_pre_oral_rejecter_alpha <- predict(elastic_d2_oral_rejecter_alpha, pred_all_d2_Oral_rejecters_alpha[,-1])
x_hat_pre_oral_rejecter_alpha

# Multiple R-squared
rsq_oral_rejecter_alpha <- cor(pred_all_d2_Oral_rejecters_alpha[,1], x_hat_pre_oral_rejecter_alpha)^2
rsq_oral_rejecter_alpha

# Plot
plot(elastic_d2_oral_rejecter_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_rejecter_alpha, scale = TRUE))

ggsave(paste0("output/Import_oral_rejecter_alpha", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_rejecter_alpha$resample$RMSE)
mean(elastic_d2_oral_rejecter_alpha$resample$Rsquared)

# day2 Oral beta diversity
pred_all_d2_Oral_rejecters <- subset(pred_all, trt == 'Oral')

# Delete colums for day and treatment 
pred_all_d2_Oral_rejecters_beta <- pred_all_d2_Oral_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Oral_rejecters_beta <- pred_all_d2_Oral_rejecters_beta[,-2:-4]
pred_all_d2_Oral_rejecters_beta <- pred_all_d2_Oral_rejecters_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_rejecter_beta <- train(rejecters_norm ~ .,
                                       data = pred_all_d2_Oral_rejecters_beta,
                                       method = "glmnet",
                                       preProcess = c("center", "scale"),
                                       tuneLength = 10,
                                       trControl = control)

elastic_d2_oral_rejecter_beta

# Model Prediction
x_hat_pre_oral_rejecter_beta <- predict(elastic_d2_oral_rejecter_beta, pred_all_d2_Oral_rejecters_beta[,-1])
x_hat_pre_oral_rejecter_beta

# Multiple R-squared
rsq_oral_rejecter_beta <- cor(pred_all_d2_Oral_rejecters_beta[,1], x_hat_pre_oral_rejecter_beta)^2
rsq_oral_rejecter_beta

# Plot
plot(elastic_d2_oral_rejecter_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_rejecter_beta, scale = TRUE))

ggsave(paste0("output/Import_oral_rejecter_beta", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_rejecter_beta$resample$RMSE)
mean(elastic_d2_oral_rejecter_beta$resample$Rsquared)

# day2 Oral taxa
pred_all_d2_Oral_rejecters <- subset(pred_all, trt == 'Oral')
# Delete colums for day and treatment 
pred_all_d2_Oral_rejecters_taxa <- pred_all_d2_Oral_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Oral_rejecters_taxa <- pred_all_d2_Oral_rejecters_taxa[,-2:-8]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_oral_rejecter_taxa <- train(rejecters_norm ~ .,
                                       data = pred_all_d2_Oral_rejecters_taxa,
                                       method = "glmnet",
                                       preProcess = c("center", "scale"),
                                       tuneLength = 10,
                                       trControl = control)

elastic_d2_oral_rejecter_taxa

# Model Prediction
x_hat_pre_oral_rejecter_taxa <- predict(elastic_d2_oral_rejecter_taxa, pred_all_d2_Oral_rejecters_taxa)
x_hat_pre_oral_rejecter_taxa

# Multiple R-squared
rsq_oral_rejecter_taxa <- cor(pred_all_d2_Oral_rejecters_taxa[,1], x_hat_pre_oral_rejecter_taxa)^2
rsq_oral_rejecter_taxa

# Plot
plot(elastic_d2_oral_rejecter_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_oral_rejecter_taxa, scale = TRUE))

ggsave(paste0("output/Import_oral_rejecter_taxa", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_oral_rejecter_taxa$resample$RMSE)
mean(elastic_d2_oral_rejecter_taxa$resample$Rsquared)

# day2 Rectal full model
pred_all_d2_Rectal_rejecters <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_rejecters <- pred_all_d2_Rectal_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_Rectal_rejecters <- pred_all_d2_Rectal_rejecters[,-2:-5]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_rejecter <- train(rejecters_norm ~ .,
                                    data = pred_all_d2_Rectal_rejecters,
                                    method = "glmnet",
                                    preProcess = c("center", "scale"),
                                    tuneLength = 10,
                                    trControl = control)

elastic_d2_rectal_rejecter

co_rectal_rejecter <- coef(elastic_d2_rectal_rejecter$finalModel, elastic_d2_rectal_rejecter$bestTune$lambda)
#co_rectal_rejecter = as.data.frame(summary(co_rectal_rejecter))

co_rectal_rejecter = as.data.frame(as.matrix(co_rectal_rejecter))

# Model Prediction
x_hat_pre_rectal_rejecter <- predict(elastic_d2_rectal_rejecter, pred_all_d2_Rectal_rejecters[,-1])
x_hat_pre_rectal_rejecter

# Multiple R-squared
rsq_rectal_rejecter <- cor(pred_all_d2_Rectal_rejecters[,1], x_hat_pre_rectal_rejecter)^2
rsq_rectal_rejecter

# Plot
plot(elastic_d2_rectal_rejecter, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_rejecter, scale = TRUE))

ggsave(paste0("output/Import_rectal_rejecter_full", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_rejecter$resample$RMSE)
mean(elastic_d2_rectal_rejecter$resample$Rsquared)

# d2 Rectal alpha diversity
pred_all_d2_Rectal_rejecters <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_rejecters_alpha <- pred_all_d2_Rectal_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_Rectal_rejecters_alpha <- pred_all_d2_Rectal_rejecters_alpha[,-2:-5]
pred_all_d2_Rectal_rejecters_alpha <- pred_all_d2_Rectal_rejecters_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_rejecter_alpha <- train(rejecters_norm ~ .,
                                          data = pred_all_d2_Rectal_rejecters_alpha,
                                          method = "glmnet",
                                          preProcess = c("center", "scale"),
                                          tuneLength = 10,
                                          trControl = control)

elastic_d2_rectal_rejecter_alpha

# Model Prediction
x_hat_pre_rectal_rejecter_alpha <- predict(elastic_d2_rectal_rejecter_alpha, pred_all_d2_Rectal_rejecters_alpha[,-1])
x_hat_pre_rectal_rejecter_alpha

# Multiple R-squared
rsq_rectal_rejecter_alpha <- cor(pred_all_d2_Rectal_rejecters_alpha[,1], x_hat_pre_rectal_rejecter_alpha)^2
rsq_rectal_rejecter_alpha

# Plot
plot(elastic_d2_rectal_rejecter_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_rejecter_alpha, scale = TRUE))

ggsave(paste0("output/Import_rectal_rejecter_alpha", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_rejecter_alpha$resample$RMSE)
mean(elastic_d2_rectal_rejecter_alpha$resample$Rsquared)

# d2 Rectal beta diversity
pred_all_d2_Rectal_rejecters <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_rejecters_beta <- pred_all_d2_Rectal_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Rectal_rejecters_beta <- pred_all_d2_Rectal_rejecters_beta[,-2:-4]
pred_all_d2_Rectal_rejecters_beta <- pred_all_d2_Rectal_rejecters_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_rejecter_beta <- train(rejecters_norm ~ .,
                                         data = pred_all_d2_Rectal_rejecters_beta,
                                         method = "glmnet",
                                         preProcess = c("center", "scale"),
                                         tuneLength = 10,
                                         trControl = control)

elastic_d2_rectal_rejecter_beta

# Model Prediction
x_hat_pre_rectal_rejecter_beta <- predict(elastic_d2_rectal_rejecter_beta, pred_all_d2_Rectal_rejecters_beta[,-1])
x_hat_pre_rectal_rejecter_beta

# Multiple R-squared
rsq_rectal_rejecter_beta <- cor(pred_all_d2_Rectal_rejecters_beta[,1], x_hat_pre_rectal_rejecter_beta)^2
rsq_rectal_rejecter_beta

# Plot
plot(elastic_d2_rectal_rejecter_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_rejecter_beta, scale = TRUE))

ggsave(paste0("output/Import_rectal_rejecter_beta", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_rejecter_beta$resample$RMSE)
mean(elastic_d2_rectal_rejecter_beta$resample$Rsquared)

# d2 Rectal taxa
pred_all_d2_Rectal_rejecters <- subset(pred_all, trt == 'Rectal')
# Delete colums for day and treatment 
pred_all_d2_Rectal_rejecters_taxa <- pred_all_d2_Rectal_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_Rectal_rejecters_taxa <- pred_all_d2_Rectal_rejecters_taxa[,-2:-8]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_rectal_rejecter_taxa <- train(rejecters_norm ~ .,
                                         data = pred_all_d2_Rectal_rejecters_taxa,
                                         method = "glmnet",
                                         preProcess = c("center", "scale"),
                                         tuneLength = 10,
                                         trControl = control)

elastic_d2_rectal_rejecter_taxa

# Model Prediction
x_hat_pre_rectal_rejecter_taxa <- predict(elastic_d2_rectal_rejecter_taxa, pred_all_d2_Rectal_rejecters_taxa[,-1])
x_hat_pre_rectal_rejecter_taxa

# Multiple R-squared
rsq_rectal_rejecter_taxa <- cor(pred_all_d2_Rectal_rejecters_taxa[,1], x_hat_pre_rectal_rejecter_taxa)^2
rsq_rectal_rejecter_taxa

# Plot
plot(elastic_d2_rectal_rejecter_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_rectal_rejecter_taxa, scale = TRUE))

ggsave(paste0("output/Import_rectal_rejecter_taxa", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_rectal_rejecter_taxa$resample$RMSE)
mean(elastic_d2_rectal_rejecter_taxa$resample$Rsquared)

# day 2 In-feed full model
pred_all_d2_In_feed_rejecters <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_rejecters <- pred_all_d2_In_feed_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_In_feed_rejecters <- pred_all_d2_In_feed_rejecters[,-2:-5]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_rejecter <- train(rejecters_norm ~ .,
                                     data = pred_all_d2_In_feed_rejecters,
                                     method = "glmnet",
                                     preProcess = c("center", "scale"),
                                     tuneLength = 10,
                                     trControl = control)

elastic_d2_In_feed_rejecter

co_In_feed_rejecter <- coef(elastic_d2_In_feed_rejecter$finalModel, elastic_d2_In_feed_rejecter$bestTune$lambda)
#co_In_feed_rejecter = as.data.frame(summary(co_In_feed_rejecter))

co_In_feed_rejecter = as.data.frame(as.matrix(co_In_feed_rejecter))

# Model Prediction
x_hat_pre_In_feed_rejecter <- predict(elastic_d2_In_feed_rejecter, pred_all_d2_In_feed_rejecters[,-1])
x_hat_pre_In_feed_rejecter

# Multiple R-squared
rsq_In_feed_rejecter <- cor(pred_all_d2_In_feed_rejecters[,1], x_hat_pre_In_feed_rejecter)^2
rsq_In_feed_rejecter

# Plot
plot(elastic_d2_In_feed_rejecter, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_rejecter, scale = TRUE))

ggsave(paste0("output/Import_In-feed_rejecter_full", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_rejecter$resample$RMSE)
mean(elastic_d2_In_feed_rejecter$resample$Rsquared)

# day 2 In-feed alpha
pred_all_d2_In_feed_rejecters <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_rejecters_alpha <- pred_all_d2_In_feed_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
#pred_all_d2_In_feed_rejecters_alpha <- pred_all_d2_In_feed_rejecters_alpha[,-2:-5]
pred_all_d2_In_feed_rejecters_alpha <- pred_all_d2_In_feed_rejecters_alpha[,-5:-18]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_rejecter_alpha <- train(rejecters_norm ~ .,
                                           data = pred_all_d2_In_feed_rejecters_alpha,
                                           method = "glmnet",
                                           preProcess = c("center", "scale"),
                                           tuneLength = 10,
                                           trControl = control)

elastic_d2_In_feed_rejecter_alpha

# Model Prediction
x_hat_pre_In_feed_rejecter_alpha <- predict(elastic_d2_In_feed_rejecter_alpha, pred_all_d2_In_feed_rejecters_alpha[,-1])
x_hat_pre_In_feed_rejecter_alpha

# Multiple R-squared
rsq_In_feed_rejecter_alpha <- cor(pred_all_d2_In_feed_rejecters_alpha[,1], x_hat_pre_In_feed_rejecter_alpha)^2
rsq_In_feed_rejecter_alpha

# Plot
plot(elastic_d2_In_feed_rejecter_alpha, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_rejecter_alpha, scale = TRUE))

ggsave(paste0("output/Import_In-feed_rejecter_alpha", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_rejecter_alpha$resample$RMSE)
mean(elastic_d2_In_feed_rejecter_alpha$resample$Rsquared)

# day 2 In-feed beta
pred_all_d2_In_feed_rejecters <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_rejecters_beta <- pred_all_d2_In_feed_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_In_feed_rejecters_beta <- pred_all_d2_In_feed_rejecters_beta[,-2:-4]
pred_all_d2_In_feed_rejecters_beta <- pred_all_d2_In_feed_rejecters_beta[,-6:-15]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_rejecter_beta <- train(rejecters_norm ~ .,
                                          data = pred_all_d2_In_feed_rejecters_beta,
                                          method = "glmnet",
                                          preProcess = c("center", "scale"),
                                          tuneLength = 10,
                                          trControl = control)

elastic_d2_In_feed_rejecter_beta

# Model Prediction
x_hat_pre_In_feed_rejecter_beta <- predict(elastic_d2_In_feed_rejecter_beta, pred_all_d2_In_feed_rejecters_beta[,-1])
x_hat_pre_In_feed_rejecter_beta

# Multiple R-squared
rsq_In_feed_rejecter_beta <- cor(pred_all_d2_In_feed_rejecters_beta[,1], x_hat_pre_In_feed_rejecter_beta)^2
rsq_In_feed_rejecter_beta

# Plot
plot(elastic_d2_In_feed_rejecter_beta, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_rejecter_beta, scale = TRUE))

ggsave(paste0("output/Import_In-feed_rejecter_beta", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_rejecter_beta$resample$RMSE)
mean(elastic_d2_In_feed_rejecter_beta$resample$Rsquared)

# day 2 In-feed taxa
pred_all_d2_In_feed_rejecters <- subset(pred_all, trt == "In-feed")
# Delete colums for day and treatment 
pred_all_d2_In_feed_rejecters_taxa <- pred_all_d2_In_feed_rejecters[,-1:-6]
# Delete other response variables but retain colonizers which will be used for prediction 
pred_all_d2_In_feed_rejecters_taxa <- pred_all_d2_In_feed_rejecters_taxa[,-2:-8]
#pred_all_d2_In_feed_rejecters_taxa <- pred_all_d2_In_feed_rejecters_taxa[,-22:-52]

set.seed(222)
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        #search = "random",
                        verboseIter = TRUE)

elastic_d2_In_feed_rejecter_taxa <- train(rejecters_norm ~ .,
                                          data = pred_all_d2_In_feed_rejecters_taxa,
                                          method = "glmnet",
                                          preProcess = c("center", "scale"),
                                          tuneLength = 10,
                                          trControl = control)

elastic_d2_In_feed_rejecter_taxa

# Model Prediction
x_hat_pre_In_feed_rejecter_taxa <- predict(elastic_d2_In_feed_rejecter_taxa, pred_all_d2_In_feed_rejecters_taxa[,-1])
x_hat_pre_In_feed_rejecter_taxa

# Multiple R-squared
rsq_In_feed_rejecter_taxa <- cor(pred_all_d2_In_feed_rejecters_taxa[,1], x_hat_pre_In_feed_rejecter_taxa)^2
rsq_In_feed_rejecter_taxa

# Plot
plot(elastic_d2_In_feed_rejecter_taxa, main = "Elastic Net Regression")

# plotting important variables
plot(varImp(elastic_d2_In_feed_rejecter_taxa, scale = TRUE))

ggsave(paste0("output/Import_In-feed_rejecter_taxa", ".pdf"), height=10, width=12, device="pdf")

mean(elastic_d2_In_feed_rejecter_taxa$resample$RMSE)
mean(elastic_d2_In_feed_rejecter_taxa$resample$Rsquared)

# Combining the Rsqrs 
# Control
#Colonizer
name <- c('full_model', 'alpha', 'beta', 'taxa')
Rsquare <- c(rsq_control_colonizer,rsq_control_colonizer_alpha,rsq_control_colonizer_beta,rsq_control_colonizer_taxa)
Control_colonizers <- data.frame(name, Rsquare)
Control_colonizers$trt <- paste0("Control")
Control_colonizers$type <- paste0("Colonizer")

# Rejecters
name <- c('full_model', 'alpha', 'beta', 'taxa')
Rsquare <- c(rsq_control_rejecter,rsq_control_rejecter_alpha,rsq_control_rejecter_beta,rsq_control_rejecter_taxa)
Control_rejecters <- data.frame(name, Rsquare)
Control_rejecters$trt <- paste0("Control")
Control_rejecters$type <- paste0("Rejecter")

Control_model_out <- rbind(Control_colonizers, Control_rejecters)

write.table(Control_model_out, file = "fmt_control_Rsqr_new.txt",quote = F,sep = '\t', row.names = T, col.names = T)

# Oral
#Colonizer
Rsquare <- c(rsq_oral_colonizer,rsq_oral_colonizer_alpha,rsq_oral_colonizer_beta,rsq_oral_colonizer_taxa)
Oral_colonizers <- data.frame(name, Rsquare)
Oral_colonizers$trt <- paste0("Oral") 
Oral_colonizers$type <- paste0("Colonizer") 

# Rejecter
Rsquare <- c(rsq_oral_rejecter,rsq_oral_rejecter_alpha,rsq_oral_rejecter_beta,rsq_oral_rejecter_taxa)
Oral_rejecters <- data.frame(name, Rsquare)
Oral_rejecters$trt <- paste0("Oral")
Oral_rejecters$type <- paste0("Rejecter")

Oral_model_out <- rbind(Oral_colonizers, Oral_rejecters)

write.table(Oral_model_out, file = "fmt_oral_Rsqr.txt_new",quote = F,sep = '\t', row.names = T, col.names = T)

# Rectal
# Colonizer
Rsquare <- c(rsq_rectal_colonizer,rsq_rectal_colonizer_alpha,rsq_rectal_colonizer_beta,rsq_rectal_colonizer_taxa)
Rectal_colonizers <- data.frame(name, Rsquare)
Rectal_colonizers$trt <- paste0("Rectal") 
Rectal_colonizers$type <- paste0("Colonizer") 

# Rejecter
Rsquare <- c(rsq_rectal_rejecter,rsq_rectal_rejecter_alpha,rsq_rectal_rejecter_beta,rsq_rectal_rejecter_taxa)
Rectal_rejecters <- data.frame(name, Rsquare)
Rectal_rejecters$trt <- paste0("Rectal")
Rectal_rejecters$type <- paste0("Rejecter")

Rectal_model_out <- rbind(Rectal_colonizers, Rectal_rejecters)

write.table(Rectal_model_out, file = "fmt_rectal_Rsqr_new.txt",quote = F,sep = '\t', row.names = T, col.names = T)

#In-feed
# Colonizer
Rsquare <- c(rsq_In_feed_colonizer,rsq_In_feed_colonizer_alpha,rsq_In_feed_colonizer_beta,rsq_In_feed_colonizer_taxa)
In_feed_colonizers <- data.frame(name, Rsquare)
In_feed_colonizers$trt <- paste0("In_feed") 
In_feed_colonizers$type <- paste0("Colonizer") 

# Rejecter
Rsquare <- c(rsq_In_feed_rejecter,rsq_In_feed_rejecter_alpha,rsq_In_feed_rejecter_beta,rsq_In_feed_rejecter_taxa)
In_feed_rejecters <- data.frame(name, Rsquare)
In_feed_rejecters$trt <- paste0("In_feed")
In_feed_rejecters$type <- paste0("Rejecter")

In_feed_model_out <- rbind(In_feed_colonizers, In_feed_rejecters)

write.table(In_feed_model_out, file = "fmt_In-feed_Rsqr_new.txt",quote = F,sep = '\t', row.names = T, col.names = T)

# Lollipop plot of outcome models
# Control
Control_model_out$name[Control_model_out$name == 'full_model'] <- 'Full model' 
Control_model_out$name[Control_model_out$name == 'alpha'] <- 'Alpha diversity'
Control_model_out$name[Control_model_out$name == 'beta'] <- 'Beta diversity'
Control_model_out$name[Control_model_out$name == 'taxa'] <- 'To 10 Genus'

Control_model_out$name <- factor(Control_model_out$name, levels = c("To 10 Genus", "Beta diversity", "Alpha diversity","Full model"))

p <- ggplot(Control_model_out, aes(x = name, y = Rsquare, color = type, size = Rsquare, fill = type)) +
  ylim(0,1.0)+
  geom_linerange(aes(ymin = 0, ymax = Rsquare), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), shape = 21, stroke = 2) +
  labs(x = "Name of predictor", y = "Elastic Net R-square") +
  #theme_minimal() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))

p + coord_flip()
ggsave(paste0("output/Control-Rsquare_new", ".pdf"), height=4, width=6, device="pdf")


# Oral
Oral_model_out$name[Oral_model_out$name == 'full_model'] <- 'Full model' 
Oral_model_out$name[Oral_model_out$name == 'alpha'] <- 'Alpha diversity'
Oral_model_out$name[Oral_model_out$name == 'beta'] <- 'Beta diversity'
Oral_model_out$name[Oral_model_out$name == 'taxa'] <- 'To 10 Genus'

Oral_model_out$name <- factor(Oral_model_out$name, levels = c("To 10 Genus", "Beta diversity", "Alpha diversity","Full model"))

p2 <- ggplot(Oral_model_out, aes(x = name, y = Rsquare, color = type, size = Rsquare, fill = type)) +
  ylim(0,1.0)+
  geom_linerange(aes(ymin = 0, ymax = Rsquare), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), shape = 21, stroke = 2) +
  labs(x = "Name of predictor", y = "Elastic Net R-square") +
  #theme_minimal() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))

p2 + coord_flip()
ggsave(paste0("output/Oral-Rsquare_new", ".pdf"), height=4, width=6, device="pdf")

# Rectal
Rectal_model_out$name[Rectal_model_out$name == 'full_model'] <- 'Full model' 
Rectal_model_out$name[Rectal_model_out$name == 'alpha'] <- 'Alpha diversity'
Rectal_model_out$name[Rectal_model_out$name == 'beta'] <- 'Beta diversity'
Rectal_model_out$name[Rectal_model_out$name == 'taxa'] <- 'To 10 Genus'

Rectal_model_out$name <- factor(Rectal_model_out$name, levels = c("To 10 Genus", "Beta diversity", "Alpha diversity","Full model"))

p3 <- ggplot(Rectal_model_out, aes(x = name, y = Rsquare, color = type, size = Rsquare, fill = type)) +
  ylim(0,1.0)+
  geom_linerange(aes(ymin = 0, ymax = Rsquare), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), shape = 21, stroke = 2) +
  labs(x = "Name of predictor", y = "Elastic Net R-square") +
  #theme_minimal() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))

p3 + coord_flip()
ggsave(paste0("output/Rectal-Rsquare_new", ".pdf"), height=4, width=6, device="pdf")

# In-feed
In_feed_model_out$name[In_feed_model_out$name == 'full_model'] <- 'Full model' 
In_feed_model_out$name[In_feed_model_out$name == 'alpha'] <- 'Alpha diversity'
In_feed_model_out$name[In_feed_model_out$name == 'beta'] <- 'Beta diversity'
In_feed_model_out$name[In_feed_model_out$name == 'taxa'] <- 'To 10 Genus'

In_feed_model_out$name <- factor(In_feed_model_out$name, levels = c("To 10 Genus", "Beta diversity", "Alpha diversity","Full model"))

p4 <- ggplot(In_feed_model_out, aes(x = name, y = Rsquare, color = type, size = Rsquare, fill = type)) +
  ylim(0,1.0)+
  geom_linerange(aes(ymin = 0, ymax = Rsquare), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), shape = 21, stroke = 2) +
  labs(x = "Name of predictor", y = "Elastic Net R-square") +
  #theme_minimal() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))

p4 + coord_flip()
ggsave(paste0("output/In_feed-Rsquare_new", ".pdf"), height=4, width=6, device="pdf")



#Making plot for the coeficients
# colonizer
co_control_colonizer$s1 <- replace(co_control_colonizer$s1, co_control_colonizer$s1 == 0, NA)
co_control_colonizer <- na.omit(co_control_colonizer)
co_control_colonizer <- tibble::rownames_to_column(co_control_colonizer, "Predictors")
co_control_colonizer <- co_control_colonizer[-1,] 

co_oral_colonizer$s1 <- replace(co_oral_colonizer$s1, co_oral_colonizer$s1 == 0, NA)
co_oral_colonizer <- na.omit(co_oral_colonizer)
co_oral_colonizer <- tibble::rownames_to_column(co_oral_colonizer, "Predictors")
co_oral_colonizer <- co_oral_colonizer[-1,] 

co_rectal_colonizer$s1 <- replace(co_rectal_colonizer$s1, co_rectal_colonizer$s1 == 0, NA)
co_rectal_colonizer <- na.omit(co_rectal_colonizer)
co_rectal_colonizer <- tibble::rownames_to_column(co_rectal_colonizer, "Predictors")
co_rectal_colonizer <- co_rectal_colonizer[-1,] 

co_In_feed_colonizer$s1 <- replace(co_In_feed_colonizer$s1, co_In_feed_colonizer$s1 == 0, NA)
co_In_feed_colonizer <- na.omit(co_In_feed_colonizer)
co_In_feed_colonizer <- tibble::rownames_to_column(co_In_feed_colonizer, "Predictors")
co_In_feed_colonizer <- co_In_feed_colonizer[-1,] 

# rejecter
co_control_rejecter$s1 <- replace(co_control_rejecter$s1, co_control_rejecter$s1 == 0, NA)
co_control_rejecter <- na.omit(co_control_rejecter)
co_control_rejecter <- tibble::rownames_to_column(co_control_rejecter, "Predictors")
co_control_rejecter <- co_control_rejecter[-1,] 

co_oral_rejecter$s1 <- replace(co_oral_rejecter$s1, co_oral_rejecter$s1 == 0, NA)
co_oral_rejecter <- na.omit(co_oral_rejecter)
co_oral_rejecter <- tibble::rownames_to_column(co_oral_rejecter, "Predictors")
co_oral_rejecter <- co_oral_rejecter[-1,] 

co_rectal_rejecter$s1 <- replace(co_rectal_rejecter$s1, co_rectal_rejecter$s1 == 0, NA)
co_rectal_rejecter <- na.omit(co_rectal_rejecter)
co_rectal_rejecter <- tibble::rownames_to_column(co_rectal_rejecter, "Predictors")
co_rectal_rejecter <- co_rectal_rejecter[-1,] 

co_In_feed_rejecter$s1 <- replace(co_In_feed_rejecter$s1, co_In_feed_rejecter$s1 == 0, NA)
co_In_feed_rejecter <- na.omit(co_In_feed_rejecter)
co_In_feed_rejecter <- tibble::rownames_to_column(co_In_feed_rejecter, "Predictors")
co_In_feed_rejecter <- co_In_feed_rejecter[-1,] 

# Lollipop plot for coefficients
# Colonizer

#Control
#co_control_colonizer$Predictors[co_control_colonizer$Predictors == 'd__Bacteria.p__Synergistota.c__Synergistia.o__Synergistales.f__Synergistaceae.g__Cloacibacillus'] <- 'Cloacibacillus'
#co_control_colonizer$Predictors[co_control_colonizer$Predictors == 'd__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae.g__Methanobrevibacter'] <- 'Methanobrevibacter'

write.table(co_control_colonizer, file = "Coeffeicient_control_colonizer.txt",quote = F,sep = '\t', row.names = T, col.names = T)

c1 <- ggplot(co_control_colonizer, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
c1
c1 + coord_flip()
ggsave(paste0("output/Coeff_Colonizer_Control_", ".pdf"), height=4, width=4, device="pdf")

# Oral
co_oral_colonizer$Predictors[co_oral_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.g__Lachnoclostridium'] <- 'Lachnoclostridium'
co_oral_colonizer$Predictors[co_oral_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Negativicutes.o__Acidaminococcales.f__Acidaminococcaceae.g__Phascolarctobacterium'] <- 'Phascolarctobacterium'
co_oral_colonizer$Predictors[co_oral_colonizer$Predictors == 'd__Bacteria.p__Synergistota.c__Synergistia.o__Synergistales.f__Synergistaceae.g__Cloacibacillus'] <- 'Cloacibacillus'
co_oral_colonizer$Predictors[co_oral_colonizer$Predictors == 'd__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae.g__Methanobrevibacter'] <- 'Methanobrevibacter'
co_oral_colonizer$Predictors[co_oral_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Bacilli.o__Lactobacillales.f__Lactobacillaceae.g__Lactobacillus'] <- 'Lactobacillus'
co_oral_colonizer$Predictors[co_oral_colonizer$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Alloprevotella'] <- 'Alloprevotella'

write.table(co_oral_colonizer, file = "Coeffeicient_oral_colonizer.txt",quote = F,sep = '\t', row.names = T, col.names = T)

co_oral_colonizer$Predictors <- factor(co_oral_colonizer$Predictors, levels = c("observed_features", "pielou_evenness", "JAC", "Alloprevotella", "Cloacibacillus","Lachnoclostridium", "Lactobacillus", "Methanobrevibacter", "Phascolarctobacterium"))

c2 <- ggplot(co_oral_colonizer, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
c2
c2 + coord_flip()
ggsave(paste0("output/Coeff_Colonizer_Oral_", ".pdf"), height=4, width=4, device="pdf")

# Rectal
co_rectal_colonizer$Predictors[co_rectal_colonizer$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides'] <- 'Bacteroides'
co_rectal_colonizer$Predictors[co_rectal_colonizer$Predictors == 'd__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacterales.f__Enterobacteriaceae.g__Escherichia.Shigella'] <- 'Escherichia.Shigella'
co_rectal_colonizer$Predictors[co_rectal_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Negativicutes.o__Acidaminococcales.f__Acidaminococcaceae.g__Phascolarctobacterium'] <- 'Phascolarctobacterium'
co_rectal_colonizer$Predictors[co_rectal_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.__'] <- 'f_Lachnospiraceae'

write.table(co_rectal_colonizer, file = "Coeffeicient_rectal_colonizer.txt",quote = F,sep = '\t', row.names = T, col.names = T)

co_rectal_colonizer$Predictors <- factor(co_rectal_colonizer$Predictors, levels = c("observed_features", "Bacteroides", "Escherichia.Shigella","Phascolarctobacterium", "f_Lachnospiraceae"))

c3 <- ggplot(co_rectal_colonizer, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
c3
c3 + coord_flip()
ggsave(paste0("output/Coeff_Colonizer_rectal_", ".pdf"), height=4, width=4, device="pdf")

# In-feed
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides'] <- 'Bacteroides'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.g__Lachnoclostridium'] <- 'Lachnoclostridium'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacterales.f__Enterobacteriaceae.g__Escherichia.Shigella'] <- 'Escherichia.Shigella'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Prevotella'] <- 'Prevotella'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Negativicutes.o__Acidaminococcales.f__Acidaminococcaceae.g__Phascolarctobacterium'] <- 'Phascolarctobacterium'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Synergistota.c__Synergistia.o__Synergistales.f__Synergistaceae.g__Cloacibacillus'] <- 'Cloacibacillus'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae.g__Methanobrevibacter'] <- 'Methanobrevibacter'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.__'] <- 'f_Lachnospiraceae'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Firmicutes.c__Bacilli.o__Lactobacillales.f__Lactobacillaceae.g__Lactobacillus'] <- 'Lactobacillus'
co_In_feed_colonizer$Predictors[co_In_feed_colonizer$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Alloprevotella'] <- 'Alloprevotella'

write.table(co_In_feed_colonizer, file = "Coeffeicient_In_feed_colonizer.txt",quote = F,sep = '\t', row.names = T, col.names = T)

co_In_feed_colonizer$Predictors <- factor(co_In_feed_colonizer$Predictors, levels = c("observed_features", "pielou_evenness", "faith", "BC", "JAC", "UNWE", "WE", "Alloprevotella", "Bacteroides", "Cloacibacillus", "Escherichia.Shigella", "Lachnoclostridium", "Lactobacillus",  "Methanobrevibacter", "Phascolarctobacterium", "Prevotella", "f_Lachnospiraceae"))

c4 <- ggplot(co_In_feed_colonizer, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
c4
c4 + coord_flip()
ggsave(paste0("output/Coeff_Colonizer_In_feed_", ".pdf"), height=4, width=4, device="pdf")


# Rejecters

write.table(co_control_rejecter, file = "Coeffeicient_control_rejecter.txt",quote = F,sep = '\t', row.names = T, col.names = T)

# Control
r1 <- ggplot(co_control_rejecter, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
r1
r1 + coord_flip()
ggsave(paste0("output/Coeff_Rejecter_Control_", ".pdf"), height=4, width=4, device="pdf")

# Oral
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides'] <- 'Bacteroides'
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Prevotella'] <- 'Prevotella'
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Negativicutes.o__Acidaminococcales.f__Acidaminococcaceae.g__Phascolarctobacterium'] <- 'Phascolarctobacterium'
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Bacteria.p__Synergistota.c__Synergistia.o__Synergistales.f__Synergistaceae.g__Cloacibacillus'] <- 'Cloacibacillus'
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae.g__Methanobrevibacter'] <- 'Methanobrevibacter'
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.__'] <- 'f_Lachnospiraceae'
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Bacilli.o__Lactobacillales.f__Lactobacillaceae.g__Lactobacillus'] <- 'Lactobacillus'
co_oral_rejecter$Predictors[co_oral_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Alloprevotella'] <- 'Alloprevotella'

write.table(co_oral_rejecter, file = "Coeffeicient_oral_rejecter.txt",quote = F,sep = '\t', row.names = T, col.names = T)

co_oral_rejecter$Predictors <- factor(co_oral_rejecter$Predictors, levels = c("observed_features", "pielou_evenness", "faith", "JAC", "UNWE", "WE", "Alloprevotella", "Bacteroides", "Cloacibacillus", "Lactobacillus", "Methanobrevibacter", "Phascolarctobacterium", "Prevotella", "f_Lachnospiraceae"))

r2 <- ggplot(co_oral_rejecter, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
r2
r2 + coord_flip()
ggsave(paste0("output/Coeff_Rejecter_Oral_", ".pdf"), height=4, width=4, device="pdf")

# Rectal
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides'] <- 'Bacteroides'
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacterales.f__Enterobacteriaceae.g__Escherichia.Shigella'] <- 'Escherichia.Shigella'
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Prevotella'] <- 'Prevotella'
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Negativicutes.o__Acidaminococcales.f__Acidaminococcaceae.g__Phascolarctobacterium'] <- 'Phascolarctobacterium'
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae.g__Methanobrevibacter'] <- 'Methanobrevibacter'
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.__'] <- 'f_Lachnospiraceae'
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Bacilli.o__Lactobacillales.f__Lactobacillaceae.g__Lactobacillus'] <- 'Lactobacillus'
co_rectal_rejecter$Predictors[co_rectal_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Alloprevotella'] <- 'Alloprevotella'

write.table(co_rectal_rejecter, file = "Coeffeicient_rectal_rejecter.txt",quote = F,sep = '\t', row.names = T, col.names = T)

co_rectal_rejecter$Predictors <- factor(co_rectal_rejecter$Predictors, levels = c("observed_features", "pielou_evenness", "faith", "BC", "JAC", "UNWE", "WE", "Alloprevotella", "Bacteroides", "Escherichia.Shigella", "Lactobacillus", "Methanobrevibacter", "Prevotella", "Phascolarctobacterium", "f_Lachnospiraceae"))

r3 <- ggplot(co_rectal_rejecter, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
r3
r3 + coord_flip()
ggsave(paste0("output/Coeff_Rejecter_rectal_", ".pdf"), height=4, width=4, device="pdf")

# In-feed
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides'] <- 'Bacteroides'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.g__Lachnoclostridium'] <- 'Lachnoclostridium'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacterales.f__Enterobacteriaceae.g__Escherichia.Shigella'] <- 'Escherichia.Shigella'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Prevotella'] <- 'Prevotella'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Negativicutes.o__Acidaminococcales.f__Acidaminococcaceae.g__Phascolarctobacterium'] <- 'Phascolarctobacterium'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Synergistota.c__Synergistia.o__Synergistales.f__Synergistaceae.g__Cloacibacillus'] <- 'Cloacibacillus'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae.g__Methanobrevibacter'] <- 'Methanobrevibacter'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Clostridia.o__Lachnospirales.f__Lachnospiraceae.__'] <- 'f_Lachnospiraceae'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Firmicutes.c__Bacilli.o__Lactobacillales.f__Lactobacillaceae.g__Lactobacillus'] <- 'Lactobacillus'
co_In_feed_rejecter$Predictors[co_In_feed_rejecter$Predictors == 'd__Bacteria.p__Bacteroidota.c__Bacteroidia.o__Bacteroidales.f__Prevotellaceae.g__Alloprevotella'] <- 'Alloprevotella'

write.table(co_In_feed_rejecter, file = "Coeffeicient_In_feed_rejecter.txt",quote = F,sep = '\t', row.names = T, col.names = T)

co_In_feed_rejecter$Predictors <- factor(co_In_feed_rejecter$Predictors, levels = c("observed_features", "pielou_evenness", "faith", "BC", "JAC", "UNWE", "WE", "Alloprevotella", "Bacteroides", "Cloacibacillus", "Escherichia.Shigella", "Lachnoclostridium", "Lactobacillus",  "Methanobrevibacter", "Phascolarctobacterium", "Prevotella", "f_Lachnospiraceae"))

r4 <- ggplot(co_In_feed_rejecter, aes(x = Predictors, y = s1)) +
  ylim(-0.03,0.03)+
  geom_segment(aes(xend = Predictors, yend = 0), color = "orange") +  # Lollipop sticks
  geom_point(color = "orange", size = 3) +  # Points at the end of sticks
  labs( x = "Name of predictor", y = "Elastic Net coefficient") +
  theme_bw() +
  geom_hline(yintercept = 0, color="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text = element_text(size = 9, face = "bold")) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.title = element_text(size = 12, face= "bold")) +
  theme(legend.key.size = unit(8, "point")) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold"), axis.title.y = element_text(color="black", size=12, face="bold")) + 
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))
r4
r4 + coord_flip()
ggsave(paste0("output/Coeff_Rejecter_In_feed_", ".pdf"), height=4, width=4, device="pdf")




# Making tree
library(ggrepel) 
library(BiocManager)
BiocManager::install("ggtree")
library(ggtree)
library(ape)

setwd("~/Desktop/Tim-Lab/FMT_Study_1/Results/16S/FMT_outcome/moving_pictures")
ASV_table<-read_qza("table.qza")$data
results<-read_qza("gut-table.qza")$data
taxonomy<-read_qza("taxonomy.qza")$data
tree<-read_qza("rooted-tree.qza")$data

tree<-drop.tip(tree, tree$tip.label[!tree$tip.label%in% results$Feature.ID]) # remove all the features from the tree we do not have data for
ggtree(tree, layout="circular") %<+% results +
  geom_tippoint(aes(fill=diff.btw), shape=21, color="grey50")  +
  geom_tiplab2(aes(label=Significant), size=10) +
  scale_fill_gradient2(low="darkblue",high="darkred", midpoint = 0, mid="white", name="log2(fold-change") +
  theme(legend.position="right")
ggsave("tree.pdf", height=10, width=10, device="pdf", useDingbats=F)


#To count the different taxa colonizing in the treatment group
colonizers <- read.table("colonizers.txt", header=TRUE, sep="\t")
colonizers2 <- separate(data = colonizers, col = Pig_ID, into = c("Pig", "ID"), sep = "g", remove = F)
metadata <- read.table("metadata_d2.txt", header=TRUE, sep="\t")
metadata <- metadata[,-3:-4]
metadata <- metadata[,-4]
metadata <- metadata[,-1]
colonizers3 <- merge(colonizers2, metadata, by.x = 'ID', by.y = 'ID')
colonizers3 <- colonizers3 %>% distinct()

colonizer_d2 <- subset(colonizers3, Day =="2")
colonizer_d5 <- subset(colonizers3, Day =="5")
colonizer_d7 <- subset(colonizers3, Day =="7")

col_d2 <- colonizer_d2 %>% count(trt, Taxa)
col_d2_control <- subset(col_d2, trt == "Control")
col_d2_oral <- subset(col_d2, trt == "Oral")
col_d2_rectal <- subset(col_d2, trt == "Rectal")
col_d2_infeed <- subset(col_d2, trt == "In-feed")

write.table(col_d2, file = "output_taxa/d2_colonizer.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d2_control, file = "output_taxa/d2_colonizer_control.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d2_oral, file = "output_taxa/d2_colonizer_oral.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d2_rectal, file = "output_taxa/d2_colonizer_rectal.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d2_infeed, file = "output_taxa/d2_colonizer_In-feed.txt",quote = F,sep = '\t', row.names = T, col.names = T)


col_d5 <- colonizer_d5 %>% count(trt, Taxa)
col_d5_control <- subset(col_d5, trt == "Control")
col_d5_oral <- subset(col_d5, trt == "Oral")
col_d5_rectal <- subset(col_d5, trt == "Rectal")
col_d5_infeed <- subset(col_d5, trt == "In-feed")

write.table(col_d5, file = "output_taxa/d5_colonizer.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d5_control, file = "output_taxa/d5_colonizer_control.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d5_oral, file = "output_taxa/d5_colonizer_oral.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d5_rectal, file = "output_taxa/d5_colonizer_rectal.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d5_infeed, file = "output_taxa/d5_colonizer_In-feed.txt",quote = F,sep = '\t', row.names = T, col.names = T)

col_d7 <- colonizer_d7 %>% count(trt, Taxa)
col_d7_control <- subset(col_d7, trt == "Control")
col_d7_oral <- subset(col_d7, trt == "Oral")
col_d7_rectal <- subset(col_d7, trt == "Rectal")
col_d7_infeed <- subset(col_d7, trt == "In-feed")

write.table(col_d7, file = "output_taxa/d7_colonizer.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d7_control, file = "output_taxa/d7_colonizer_control.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d7_oral, file = "output_taxa/d7_colonizer_oral.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d7_rectal, file = "output_taxa/d7_colonizer_rectal.txt",quote = F,sep = '\t', row.names = T, col.names = T)
write.table(col_d7_infeed, file = "output_taxa/d7_colonizer_In-feed.txt",quote = F,sep = '\t', row.names = T, col.names = T)

col_d2_merge <- full_join(col_d2_control, col_d2_oral, by = c("Taxa"="Taxa"))
col_d2_merge <- col_d2_merge %>% 
  rename("Control" = "n.x")
col_d2_merge <- col_d2_merge %>% 
  rename("Oral" = "n.y")
col_d2_merge <- col_d2_merge[,-1]
col_d2_merge <- col_d2_merge[,-3]

col_d2_merge <- full_join(col_d2_merge, col_d2_rectal, by = c("Taxa"="Taxa"))
col_d2_merge <- col_d2_merge %>% 
  rename("Rectal" = "n")
col_d2_merge <- col_d2_merge[,-4]

col_d2_merge <- full_join(col_d2_merge, col_d2_infeed, by = c("Taxa"="Taxa"))
col_d2_merge <- col_d2_merge %>% 
  rename("In-feed" = "n")
col_d2_merge <- col_d2_merge[,-5]

col_d2_merge[is.na(col_d2_merge)] <- 0

write.table(col_d2_merge, file = "output_taxa/col_d2_merge.txt",quote = F,sep = '\t', row.names = T, col.names = T)



col_d5_merge <- full_join(col_d5_control, col_d5_oral, by = c("Taxa"="Taxa"))
col_d5_merge <- col_d5_merge %>% 
  rename("Control" = "n.x")
col_d5_merge <- col_d5_merge %>% 
  rename("Oral" = "n.y")
col_d5_merge <- col_d5_merge[,-1]
col_d5_merge <- col_d5_merge[,-3]

col_d5_merge <- full_join(col_d5_merge, col_d5_rectal, by = c("Taxa"="Taxa"))
col_d5_merge <- col_d5_merge %>% 
  rename("Rectal" = "n")
col_d5_merge <- col_d5_merge[,-4]

col_d5_merge <- full_join(col_d5_merge, col_d5_infeed, by = c("Taxa"="Taxa"))
col_d5_merge <- col_d5_merge %>% 
  rename("In-feed" = "n")
col_d5_merge <- col_d5_merge[,-5]

col_d5_merge[is.na(col_d5_merge)] <- 0

write.table(col_d5_merge, file = "output_taxa/col_d5_merge.txt",quote = F,sep = '\t', row.names = T, col.names = T)


col_d7_merge <- full_join(col_d7_control, col_d7_oral, by = c("Taxa"="Taxa"))
col_d7_merge <- col_d7_merge %>% 
  rename("Control" = "n.x")
col_d7_merge <- col_d7_merge %>% 
  rename("Oral" = "n.y")
col_d7_merge <- col_d7_merge[,-1]
col_d7_merge <- col_d7_merge[,-3]

col_d7_merge <- full_join(col_d7_merge, col_d7_rectal, by = c("Taxa"="Taxa"))
col_d7_merge <- col_d7_merge %>% 
  rename("Rectal" = "n")
col_d7_merge <- col_d7_merge[,-4]

col_d7_merge <- full_join(col_d7_merge, col_d7_infeed, by = c("Taxa"="Taxa"))
col_d7_merge <- col_d7_merge %>% 
  rename("In-feed" = "n")
col_d7_merge <- col_d7_merge[,-5]

col_d7_merge[is.na(col_d7_merge)] <- 0

write.table(col_d7_merge, file = "output_taxa/col_d7_merge.txt",quote = F,sep = '\t', row.names = T, col.names = T)

