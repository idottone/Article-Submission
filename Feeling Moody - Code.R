### Packages ##################################################################################################################################
library(StanHeaders)
library(cmdstanr)
library(inline)
library(Rcpp)
library(brms)
library(dplyr)
library(tidyr)
library(Matrix)
library(SciViews)
library(lme4)
library(stats)
library(e1071)
library(ggplot2)
library(tidybayes)
library(posterior)
library(gtsummary)
library(ggpubr)
library(dynlm)
library(sjPlot)
library(bayestestR)
library(mediation)
library(patchwork)
library(extrafont)
library(EMAeval)
library(wesanderson)
library(loo)
library(rstan)
library(bayesplot)

### Load Data
MoodAndDenseEmo <- read.csv(
  # Insert File Path
)

# Raw Data Graphs ##################################################################################################################################################################################################################################################
##### ** Mood x Dense ####
MoodAndDenseEmo$DaysSinceDenseBin <- cut(MoodAndDenseEmo$DaysSinceDense,
                                         ordered_result = T,
                                         breaks = c(0, 2, 4, 6, 8, 10, 12, 14))

RD_MoodGraph <- data.frame(DaysSinceDenseBin = c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]"),
                              DenseEmo_PA_char = c("High", "High","High","High","High","High", "High",
                                                   "Middle", "Middle","Middle", "Middle","Middle","Middle", "Middle",
                                                   "Low", "Low","Low","Low","Low","Low", "Low"),
                              DenseEmo_NA_char = c("High", "High","High","High","High","High", "High",
                                                   "Middle", "Middle","Middle", "Middle","Middle","Middle", "Middle",
                                                   "Low", "Low","Low","Low","Low","Low", "Low"))

RD_MoodGraph$DeltaMoodPA <- NA
RD_MoodGraph$DeltaMoodPA.se <- NA
RD_MoodGraph$DeltaMoodNA <- NA
RD_MoodGraph$DeltaMoodNA.se <- NA
for (i in c("High", "Middle", "Low")) {
  for (j in c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]")) {
    RD_MoodGraph$DeltaMoodPA[which(RD_MoodGraph$DaysSinceDenseBin == j & RD_MoodGraph$DenseEmo_PA_char == i)] <- mean(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$DenseEmo_PA_char == i)], na.rm = TRUE)
    RD_MoodGraph$DeltaMoodPA.se[which(RD_MoodGraph$DaysSinceDenseBin == j & RD_MoodGraph$DenseEmo_PA_char == i)] <- sd(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$DenseEmo_PA_char == i)], na.rm = TRUE)/
      sqrt(nrow(subset(MoodAndDenseEmo, MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$DenseEmo_PA_char == i & !is.na(MoodAndDenseEmo$DeltaMood_PA))))
    RD_MoodGraph$DeltaMoodNA[which((RD_MoodGraph$DaysSinceDenseBin == j & RD_MoodGraph$DenseEmo_NA_char == i))] <-  mean(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin ==j & MoodAndDenseEmo$DenseEmo_NA_char == i)], na.rm = TRUE)
    RD_MoodGraph$DeltaMoodNA.se[which(RD_MoodGraph$DaysSinceDenseBin == j & RD_MoodGraph$DenseEmo_NA_char == i)] <- sd(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$DenseEmo_NA_char == i)], na.rm = TRUE)/
      sqrt(nrow(subset(MoodAndDenseEmo, MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$DenseEmo_NA_char == i & !is.na(MoodAndDenseEmo$DeltaMood_NA))))
  }
}


RD_MoodGraph$DaysSinceDense <- NA
DSD_Bins <- c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]")
DSD_Range <- c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "13-14")
for (i in 1:nrow(RD_MoodGraph)) {
  for (j in 1:7) {
    if (RD_MoodGraph$DaysSinceDenseBin[i] == DSD_Bins[j]) {
      RD_MoodGraph$DaysSinceDense[i] <-  DSD_Range[j]
    }
  }
}

RD_MoodGraph$DaysSinceDense <- factor(RD_MoodGraph$DaysSinceDense, levels = c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "13-14"))
RD_MoodGraph$DenseEmo_PA_char <- factor(RD_MoodGraph$DenseEmo_PA_char,levels = c("High", "Middle", "Low"))
RD_MoodGraph$DenseEmo_NA_char <- factor(RD_MoodGraph$DenseEmo_NA_char,levels = c("High", "Middle", "Low"))


ggplot(data = RD_MoodGraph, aes(x = DaysSinceDense, y = DeltaMoodPA, color = DenseEmo_PA_char)) +
  geom_point(data = RD_MoodGraph, aes(y = DeltaMoodPA), size = 3, shape = 19) +
  geom_errorbar(data = RD_MoodGraph, aes(y = DeltaMoodPA, ymin = DeltaMoodPA - DeltaMoodPA.se, ymax = DeltaMoodPA + DeltaMoodPA.se), width = .25) +
  geom_line(aes(group = DenseEmo_PA_char), linewidth = 1) +
  scale_color_manual(values = c("High" = "chartreuse3", "Low" = "red3"), name = "Positive Affect After Grade Reveal") +
  labs(x = "Days Since Grade Reveal",
       y = "Change in Positive Affect") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "Bahnschrift")) +
  scale_fill_discrete(labels = c("Low", "Middle", "High")) +
  ylim(-10,14)


ggplot(data = RD_MoodGraph, aes(x = DaysSinceDense, y = DeltaMoodNA, color = DenseEmo_NA_char)) +
  geom_point(data = RD_MoodGraph, aes(y = DeltaMoodNA), size = 3, shape = 19) +
  geom_errorbar(data = RD_MoodGraph, aes(y = DeltaMoodNA, ymin = DeltaMoodNA - DeltaMoodNA.se, ymax = DeltaMoodNA + DeltaMoodNA.se), width = .25) +
  geom_line(aes(group = DenseEmo_NA_char), linewidth = 1) +
  scale_color_manual(values = c("High" = "red3", "Low" = "chartreuse3"), name = "Negative Affect After Grade Reveal") +
  labs(x = "Days Since Grade Reveal",
       y = "Change in Negative Affect") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "Bahnschrift")) +
  scale_fill_discrete(labels = c("Low", "Middle", "High")) +
  ylim(-10, 14)


##### ** Grade x Dense ####
RD_GradeGraph <- data.frame(DaysSinceDenseBin = c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]"),
                               Grade_char = c("High", "High","High","High","High","High", "High",
                                              "Low", "Low","Low","Low","Low","Low", "Low"))

RD_GradeGraph$DeltaMoodPA <- NA
RD_GradeGraph$DeltaMoodPA.se <- NA
RD_GradeGraph$DeltaMoodNA <- NA
RD_GradeGraph$DeltaMoodNA.se <- NA
for (i in c("High", "Low")) {
  for (j in c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]")) {
    if (length(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i)]) > 0) {
      RD_GradeGraph$DeltaMoodPA[which(RD_GradeGraph$DaysSinceDenseBin == j & RD_GradeGraph$Grade_char == i)] <- mean(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i)], na.rm = TRUE)
      RD_GradeGraph$DeltaMoodPA.se[which(RD_GradeGraph$DaysSinceDenseBin == j & RD_GradeGraph$Grade_char == i)] <- sd(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i)], na.rm = TRUE)/
        sqrt(nrow(subset(MoodAndDenseEmo, MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i & !is.na(MoodAndDenseEmo$DeltaMood_PA)))) 
    }
    if (length(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i)]) > 0) {
      RD_GradeGraph$DeltaMoodNA[which(RD_GradeGraph$DaysSinceDenseBin == j & RD_GradeGraph$Grade_char == i)] <- mean(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i)], na.rm = TRUE)
      RD_GradeGraph$DeltaMoodNA.se[which(RD_GradeGraph$DaysSinceDenseBin == j & RD_GradeGraph$Grade_char == i)] <- sd(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i)], na.rm = TRUE)/
        sqrt(nrow(subset(MoodAndDenseEmo, MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$Grade_char == i & !is.na(MoodAndDenseEmo$DeltaMood_PA))))
    }
  }}

RD_GradeGraph$DaysSinceDense <- NA
DSD_Bins <- c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]")
DSD_Range <- c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "13-14")
for (i in 1:nrow(RD_GradeGraph)) {
  for (j in 1:7) {
    if (RD_GradeGraph$DaysSinceDenseBin[i] == DSD_Bins[j]) {
      RD_GradeGraph$DaysSinceDense[i] <-  DSD_Range[j]
    }
  }
}

RD_GradeGraph$DaysSinceDense <- factor(RD_GradeGraph$DaysSinceDense, levels = c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "13-14"))


# Plotting
ggplot(data = RD_GradeGraph, aes(x = DaysSinceDense, y = DeltaMoodPA, color = Grade_char)) +
  geom_point(data = RD_GradeGraph, aes(y = DeltaMoodPA), size = 3, shape = 19) +
  geom_errorbar(data = RD_GradeGraph, aes(y = DeltaMoodPA, ymin = DeltaMoodPA - DeltaMoodPA.se, ymax = DeltaMoodPA + DeltaMoodPA.se), width = .25) +
  geom_line(aes(group = Grade_char), linewidth = 1) +
  scale_color_manual(values = c("High" = "chartreuse3", "Low" = "red3"), name = "Grade (Binned)") +
  labs(x = "Days Since Grade Reveal",
       y = "Change in Positive Affective") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "Bahnschrift")) +
  ylim(-8,8) 


ggplot(data = RD_GradeGraph, aes(x = DaysSinceDense, y = DeltaMoodNA, color = Grade_char)) +
  geom_point(data = RD_GradeGraph, aes(y = DeltaMoodNA), size = 3, shape = 19) +
  geom_errorbar(data = RD_GradeGraph, aes(y = DeltaMoodNA, ymin = DeltaMoodNA - DeltaMoodNA.se, ymax = DeltaMoodNA + DeltaMoodNA.se), width = .25) +
  geom_line(aes(group = Grade_char), linewidth = 1) +
  scale_color_manual(values = c("High" = "chartreuse3", "Low" = "red3"), name = "Grade (Binned)") +
  labs(x = "Days Since Grade Reveal",
       y = "Change in Negative Affect") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "Bahnschrift")) +
  ylim(-9,8)



##### ** PE x Dense ####
RD_PEGrade <- data.frame(DaysSinceDenseBin = c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]"),
                            PE_char = c("Negative", "Negative","Negative","Negative","Negative","Negative", "Negative",
                                        "Positive", "Positive","Positive", "Positive","Positive","Positive", "Positive"))

RD_PEGrade$DeltaMoodPA <- NA
RD_PEGrade$DeltaMoodPA.se <- NA
RD_PEGrade$DeltaMoodNA <- NA
RD_PEGrade$DeltaMoodNA.se <- NA
for (i in c("Negative", "Positive")) {
  for (j in c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]")) {
    if (length(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i)]) > 0) {
      RD_PEGrade$DeltaMoodPA[which(RD_PEGrade$DaysSinceDenseBin == j & RD_PEGrade$PE_char == i)] <- mean(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i)], na.rm = TRUE)
      RD_PEGrade$DeltaMoodPA.se[which(RD_PEGrade$DaysSinceDenseBin == j & RD_PEGrade$PE_char == i)] <- sd(MoodAndDenseEmo$DeltaMood_PA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i)], na.rm = TRUE)/
        sqrt(nrow(subset(MoodAndDenseEmo, MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i & !is.na(MoodAndDenseEmo$DeltaMood_PA)))) 
    }
    if (length(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i)]) > 0) {
      RD_PEGrade$DeltaMoodNA[which(RD_PEGrade$DaysSinceDenseBin == j & RD_PEGrade$PE_char == i)] <- mean(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i)], na.rm = TRUE)
      RD_PEGrade$DeltaMoodNA.se[which(RD_PEGrade$DaysSinceDenseBin == j & RD_PEGrade$PE_char == i)] <- sd(MoodAndDenseEmo$DeltaMood_NA[which(MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i)], na.rm = TRUE)/
        sqrt(nrow(subset(MoodAndDenseEmo, MoodAndDenseEmo$DaysSinceDenseBin == j & MoodAndDenseEmo$PE_1sign == i & !is.na(MoodAndDenseEmo$DeltaMood_PA))))
    }
  }}

RD_PEGrade$DaysSinceDense <- NA
DSD_Bins <- c("(0,2]", "(2,4]", "(4,6]","(6,8]", "(8,10]", "(10,12]", "(12,14]")
DSD_Range <- c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "13-14")
for (i in 1:nrow(RD_PEGrade)) {
  for (j in 1:7) {
    if (RD_PEGrade$DaysSinceDenseBin[i] == DSD_Bins[j]) {
      RD_PEGrade$DaysSinceDense[i] <-  DSD_Range[j]
    }
  }
}

RD_PEGrade$DaysSinceDense <- factor(RD_PEGrade$DaysSinceDense, levels = c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "13-14"))


# Plotting
ggplot(data = RD_PEGrade, aes(x = DaysSinceDense, y = DeltaMoodPA, color = PE_char)) +
  geom_point(data = RD_PEGrade, aes(y = DeltaMoodPA), size = 3, shape = 19) +
  geom_errorbar(data = RD_PEGrade, aes(y = DeltaMoodPA, ymin = DeltaMoodPA - DeltaMoodPA.se, ymax = DeltaMoodPA + DeltaMoodPA.se), width = .25) +
  geom_line(aes(group = PE_char), linewidth = 1) +
  scale_color_manual(values = c("Positive" = "chartreuse3", "Negative" = "red3"), name = "Prediction Error Sign") +
  labs(title = "Change in Positive Mood based on PE",
    x = "Days Since Grade Reveal",
       y = "Change in Positive Affective") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "Bahnschrift")) +
  ylim(-7.5,8) +
  scale_fill_discrete(labels = c("Positive", "Negative")) 

ggplot(data = RD_PEGrade, aes(x = DaysSinceDense, y = DeltaMoodNA, color = PE_char)) +
  geom_point(data = RD_PEGrade, aes(y = DeltaMoodNA), size = 3, shape = 19) +
  geom_errorbar(data = RD_PEGrade, aes(y = DeltaMoodNA, ymin = DeltaMoodNA - DeltaMoodNA.se, ymax = DeltaMoodNA + DeltaMoodNA.se), width = .25) +
  geom_line(aes(group = PE_char), linewidth = 1) +
  scale_color_manual(values = c("Positive" = "chartreuse3", "Negative" = "red3"), name = "Prediction Error Sign") +
  labs(title = "Change in Negative Mood based on PE", x = "Days Since Grade Reveal",
       y = "Change in Negative Affect") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "Bahnschrift")) +
  ylim(-7.5,8) +
  scale_fill_discrete(labels = c("Positive", "Negative")) 


# Brms ####
#### ** Mood ~ Emo * DSD ####
NAMood_DenseNA <- brm(DeltaMood_NA ~ DenseEmo_NA * DaysSinceDense + exam_num + N_Responses + (1 + DenseEmo_NA |cohort_class/Repeat_ID),
                      data = MoodAndDenseEmo, 
                      backend = "cmdstanr", cores = 5,
                      iter = 4000, control = list(adapt_delta = 0.95),
                      family = gaussian(),
                      file = "Brms_results/NAMood_DenseNA.brm", file_refit = "on_change")
NAMood_DenseNA <- readRDS("Brms_results/NAMood_DenseNA.brm.rds")
summary(NAMood_DenseNA)
plot(NAMood_DenseNA)
pp_check(NAMood_DenseNA)

PAMood_DensePA <- brm(DeltaMood_PA ~ DenseEmo_PA * DaysSinceDense + exam_num + N_Responses + (1 + DenseEmo_PA |cohort_class/Repeat_ID),
                                       data = MoodAndDenseEmo, 
                                       backend = "cmdstanr", cores = 5,
                                       iter = 4000, control = list(adapt_delta = 0.95),
                                       family = gaussian(),
                                       file = "Brms_results/PAMood_DensePA.brm", file_refit = "on_change")
PAMood_DensePA <- readRDS("Brms_results/PAMood_DensePA.brm.rds")
summary(PAMood_DensePA)
plot(PAMood_DensePA)
pp_check(PAMood_DensePA)
conditional_effects(PAMood_DensePA)


#### ** Mood ~ Grade * DSD ####
NAMood_Grade <- brm(DeltaMood_NA ~ grade_100 * DaysSinceDense + exam_num + N_Responses + (1 + grade_100 |cohort_class/Repeat_ID),
                    data = MoodAndDenseEmo, 
                    backend = "cmdstanr", cores = 5,
                    iter = 4000, control = list(adapt_delta = 0.95),
                    family = gaussian(),
                    file = "Brms_results/NAMood_Grade.brm", file_refit = "on_change")
NAMood_Grade <- readRDS("Brms_results/NAMood_Grade.brm.rds")
summary(NAMood_Grade)
plot(NAMood_Grade)
pp_check(NAMood_Grade)
conditional_effects(NAMood_Grade)


PAMood_Grade <- brm(DeltaMood_PA ~ grade_100 * DaysSinceDense + exam_num + N_Responses + (1 + grade_100 |cohort_class/Repeat_ID),
                                     data = MoodAndDenseEmo, 
                                     backend = "cmdstanr", cores = 5,
                                     iter = 4000, control = list(adapt_delta = 0.95),
                                     family = gaussian(),
                                     file = "Brms_results/PAMood_Grade.brm", file_refit = "on_change")
PAMood_Grade <- readRDS("Brms_results/PAMood_Grade.brm.rds")
summary(PAMood_Grade)
plot(PAMood_Grade)
pp_check(PAMood_Grade)
conditional_effects(PAMood_Grade)


#### ** Mood ~ PE * DSD ####
NAMood_PE <- brm(DeltaMood_NA ~ PE_1 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1 |cohort_class/Repeat_ID),
                 data = MoodAndDenseEmo, 
                 backend = "cmdstanr", cores = 5,
                 iter = 4000, control = list(adapt_delta = 0.95),
                 family = gaussian(),
                 file = "Brms_results/NAMood_PE.brm", file_refit = "on_change")
NAMood_PE <- readRDS("Brms_results/NAMood_PE.brm.rds")
summary(NAMood_PE)
plot(NAMood_PE)
pp_check(NAMood_PE)
conditional_effects(NAMood_PE)


PAMood_PE <- brm(DeltaMood_PA ~ PE_1 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1 |cohort_class/Repeat_ID),
                                  data = MoodAndDenseEmo, 
                                  backend = "cmdstanr", cores = 5,
                                  iter = 4000, control = list(adapt_delta = 0.95),
                                  family = gaussian(),
                                  file = "Brms_results/PAMood_PE.brm", file_refit = "on_change")
PAMood_PE <- readRDS("Brms_results/PAMood_PE.brm.rds")
summary(PAMood_PE)
plot(PAMood_PE)
pp_check(PAMood_PE)
conditional_effects(PAMood_PE)

#### ** Mood & PE Med by Dense ####
Med.EmoNA_PE_1_DSD_2wks_DD1 <- bf(DenseEmo_NA ~ PE_1 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1|cohort_class/Repeat_ID))
Out.EmoNA_PE_1_DSD_2wks_DD1 <- bf(DeltaMood_NA ~ PE_1 * DaysSinceDense + DenseEmo_NA + exam_num + N_Responses + (1 + PE_1|cohort_class/Repeat_ID))
MedMod_NAMood_PE_DenseNA <- brm(Med.EmoNA_PE_1_DSD_2wks_DD1 + Out.EmoNA_PE_1_DSD_2wks_DD1 + set_rescor(FALSE),
                                            data = MoodAndDenseEmo,
                                            backend = "cmdstanr", cores = 5, refresh = 0,
                                            iter = 4000, control = list(adapt_delta = 0.95),
                                            family = gaussian(),
                                            file = "Brms_results/MedMod_NAMood_PE_DenseNA.brm", file_refit = "on_change")
MedMod_NAMood_PE_DenseNA <- readRDS("Brms_results/MedMod_NAMood_PE_DenseNA.brm.rds")
mediation(MedMod_NAMood_PE_DenseNA)
summary(MedMod_NAMood_PE_DenseNA)
plot(MedMod_NAMood_PE_DenseNA)
pp_check(MedMod_NAMood_PE_DenseNA, resp = "DeltaMoodNA")
pp_check(MedMod_NAMood_PE_DenseNA, resp = "DenseEmoNA")

plot(MedMod_NAMood_PE_DenseNA)

#### ** Mood & Grade Med by Dense ####
Med.EmoNA_grade_100_DSD_2wks_DD1 <- bf(DenseEmo_NA ~ grade_100 * DaysSinceDense + exam_num + N_Responses + (1 + grade_100|cohort_class/Repeat_ID))
Out.EmoNA_grade_100_DSD_2wks_DD1 <- bf(DeltaMood_NA ~ grade_100 * DaysSinceDense + DenseEmo_NA + exam_num + N_Responses + (1 + grade_100|cohort_class/Repeat_ID))
MedMod_NAMood_Grade_DenseNA <- brm(Med.EmoNA_grade_100_DSD_2wks_DD1 + Out.EmoNA_grade_100_DSD_2wks_DD1 + set_rescor(FALSE),
                                               data = MoodAndDenseEmo,
                                               backend = "cmdstanr", cores = 4, refresh = 0,
                                               iter = 5000, control = list(adapt_delta = 0.95),
                                               family = gaussian(),
                                               file = "Brms_results/MedMod_NAMood_Grade_DenseNA.brm", file_refit = "on_change")
MedMod_NAMood_Grade_DenseNA <- readRDS("Brms_results/MedMod_NAMood_Grade_DenseNA.brm.rds")
mediation(MedMod_NAMood_Grade_DenseNA)
summary(MedMod_NAMood_Grade_DenseNA)
plot(MedMod_NAMood_Grade_DenseNA)
pp_check(MedMod_NAMood_Grade_DenseNA, resp = "DeltaMoodNA")
pp_check(MedMod_NAMood_Grade_DenseNA, resp = "DenseEmoNA")

### Model Graphs ##################################################################################################################################################################
#### Dense Emo ####
DenseEmo_PA_plot_SD <- c((mean(MoodAndDenseEmo$DenseEmo_PA) - sd(MoodAndDenseEmo$DenseEmo_PA)), (mean(MoodAndDenseEmo$DenseEmo_PA) + sd(MoodAndDenseEmo$DenseEmo_PA)))
plot_PAMood_PAEmo <- conditional_effects(PAMood_DensePA, 
                                         effects = "DaysSinceDense:DenseEmo_PA",
                                         int_conditions = list(DenseEmo_PA = DenseEmo_PA_plot_SD))
plot_PAMood_PAEmo_df <- as.data.frame(plot_PAMood_PAEmo$`DaysSinceDense:DenseEmo_PA`)
plot_PAMood_PAEmo_df$DenseEmo_PA_char <- NA
for (i in 1:nrow(plot_PAMood_PAEmo_df)) {
  if (isTRUE(plot_PAMood_PAEmo_df[i, "DenseEmo_PA"] <= 27)) {
    plot_PAMood_PAEmo_df$DenseEmo_PA_char[i] <- "Low Positive Emotion" 
  }
  if (isTRUE(plot_PAMood_PAEmo_df[i, "DenseEmo_PA"] >= 62)) {
    plot_PAMood_PAEmo_df$DenseEmo_PA_char[i] <- "High Positive Emotion" 
  }
}


DenseEmo_NA_plot_SD <- c((mean(MoodAndDenseEmo$DenseEmo_NA) - sd(MoodAndDenseEmo$DenseEmo_NA)), (mean(MoodAndDenseEmo$DenseEmo_NA) + sd(MoodAndDenseEmo$DenseEmo_NA)))
plot_NAMood_NAEmo <- conditional_effects(NAMood_DenseNA, 
                                         effects = "DaysSinceDense:DenseEmo_NA",
                                         int_conditions = list(DenseEmo_NA =DenseEmo_NA_plot_SD))
plot_NAMood_NAEmo_df <- as.data.frame(plot_NAMood_NAEmo$`DaysSinceDense:DenseEmo_NA`)
plot_NAMood_NAEmo_df$DenseEmo_NA <- as.character(plot_NAMood_NAEmo_df$DenseEmo_NA)
plot_NAMood_NAEmo_df$DenseEmo_NA_char <- NA
for (i in 1:nrow(plot_NAMood_NAEmo_df)) {
  if (isTRUE(plot_NAMood_NAEmo_df[i, "DenseEmo_NA"] == "23.0120736727898")) {
    plot_NAMood_NAEmo_df$DenseEmo_NA_char[i] <- "Low Negative Emotion" 
  }
  if (isTRUE(plot_NAMood_NAEmo_df[i, "DenseEmo_NA"] == "71.7117813640349")) {
    plot_NAMood_NAEmo_df$DenseEmo_NA_char[i] <- "High Negative Emotion" 
  }
}

##### ** NA ####
BRM_NA_Emo <- ggplot(plot_NAMood_NAEmo_df, aes(x = DaysSinceDense, y = estimate__)) +
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Negative Mood") +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = DenseEmo_NA_char), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=DenseEmo_NA_char)) +
  scale_fill_manual(values = c("Low Negative Emotion" = "#2957FF", "High Negative Emotion" = "#F72836"), name = "Dense Emotion") +
  scale_color_manual(values = c("Low Negative Emotion" = "#2957FF", "High Negative Emotion" = "#F72836"), name = "Dense Emotion") +
  theme(legend.position = "bottom",
        # legend.text = element_text(size = 16, family = "Bahnschrift"),
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16)) +
  ylim(-10,14)

##### ** PA ####
BRM_PA_Emo <- ggplot(plot_PAMood_PAEmo_df, aes(x = DaysSinceDense, y = estimate__)) + 
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Positive Mood") + 
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = DenseEmo_PA_char), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=DenseEmo_PA_char)) +
  scale_fill_manual(values = c("High Positive Emotion" = "#2957FF", "Low Positive Emotion" = "#F72836"), name = "Dense Emotion") +
  scale_color_manual(values = c("High Positive Emotion" = "#2957FF", "Low Positive Emotion" = "#F72836"), name = "Dense Emotion") +
  theme(legend.position = "bottom",
        # legend.text = element_text(size = 16, family = "Bahnschrift"),
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")
        ,
        text = element_text(size = 16)
  ) +
  ylim(-10,14)


BRM_NA_Emo
BRM_PA_Emo

#### Grade ####
Grade_plot_SD <- c((mean(MoodAndDenseEmo$grade_100) - sd(MoodAndDenseEmo$grade_100)), (mean(MoodAndDenseEmo$grade_100) + sd(MoodAndDenseEmo$grade_100)))

plot_NAMood_Grade <- conditional_effects(NAMood_Grade, 
                                         effects = "DaysSinceDense:grade_100",
                                         int_conditions = list(grade_100 = Grade_plot_SD))
plot_NAMood_Grade_df <- as.data.frame(plot_NAMood_Grade$`DaysSinceDense:grade_100`)
plot_NAMood_Grade_df$grade_100 <- as.character(plot_NAMood_Grade_df$grade_100)
plot_NAMood_Grade_df$grade_100[which(plot_NAMood_Grade_df$grade_100 == "56.3514656277529")] <- "56.35%"
plot_NAMood_Grade_df$grade_100[which(plot_NAMood_Grade_df$grade_100 == "91.4510669949173")] <- "91.45%"


plot_PAMood_Grade <- conditional_effects(PAMood_Grade, 
                                         effects = "DaysSinceDense:grade_100",
                                         int_conditions = list(grade_100 = Grade_plot_SD))
plot_PAMood_Grade_df <- as.data.frame(plot_PAMood_Grade$`DaysSinceDense:grade_100`)
plot_PAMood_Grade_df$grade_100 <- as.character(plot_PAMood_Grade_df$grade_100)
plot_PAMood_Grade_df$grade_100[which(plot_PAMood_Grade_df$grade_100 == "56.3514656277529")] <- "56.35%"
plot_PAMood_Grade_df$grade_100[which(plot_PAMood_Grade_df$grade_100 == "91.4510669949173")] <- "91.45%"


##### ** NA ####
BRM_NA_Grade <- ggplot(plot_NAMood_Grade_df, aes(x = DaysSinceDense, y = estimate__)) +
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Negative Mood") +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = grade_100), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=grade_100)) +
  scale_fill_manual(values = c("91.45%" = "#2957FF", "56.35%" = "#F72836"), name = "Grade") +
  scale_color_manual(values = c("91.45%" = "#2957FF", "56.35%" = "#F72836"), name = "Grade") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16)) +
  ylim(-9,8)

##### ** PA ####
BRM_PA_Grade <- ggplot(plot_PAMood_Grade_df, aes(x = DaysSinceDense, y = estimate__)) +
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Positive Mood") +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = grade_100), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=grade_100)) +
  scale_fill_manual(values = c("91.45%" = "#2957FF", "56.35%" = "#F72836"), name = "Grade") +
  scale_color_manual(values = c("91.45%" = "#2957FF", "56.35%" = "#F72836"), name = "Grade") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16)) +
  ylim(-9,8)


BRM_NA_Grade
BRM_PA_Grade

##### ** Med: Grade ####
mean(MoodAndDenseEmo$grade_100) - sd(MoodAndDenseEmo$grade_100)
mean(MoodAndDenseEmo$grade_100) + sd(MoodAndDenseEmo$grade_100)
DenseEmo_NA_SD <- c((mean(MoodAndDenseEmo$DenseEmo_NA) - sd(MoodAndDenseEmo$DenseEmo_NA)), (mean(MoodAndDenseEmo$DenseEmo_NA) + sd(MoodAndDenseEmo$DenseEmo_NA)))

condition_grademedgraph <- data.frame(grade_100 = c(56.35, 91.45))
cond_grademedgraph <- conditional_effects(MedMod_NAMood_Grade_DenseNA,
                                          effects = "DaysSinceDense:DenseEmo_NA",
                                          int_conditions = list(DenseEmo_NA = DenseEmo_NA_SD),
                                          conditions = condition_grademedgraph)
plot_MoodG_MedEmo_new_df <- as.data.frame(cond_grademedgraph$`DeltaMoodNA.DeltaMoodNA_DaysSinceDense:DenseEmo_NA`)
plot_MoodG_MedEmo_new_df$DenseEmo_NA

plot_MoodG_MedEmo_new_df$DenseEmo_NA <- as.character(plot_MoodG_MedEmo_new_df$DenseEmo_NA)
plot_MoodG_MedEmo_new_df$DenseEmo_NA[which(plot_MoodG_MedEmo_new_df$DenseEmo_NA == "71.7117813640349")] <- "High Negative Emotion"
plot_MoodG_MedEmo_new_df$DenseEmo_NA[which(plot_MoodG_MedEmo_new_df$DenseEmo_NA == "23.0120736727898")] <- "Low Negative Emotion"


BRM_NA_Grade_Med <- ggplot(plot_MoodG_MedEmo_new_df, aes(x = DaysSinceDense, y = estimate__)) +
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Negative Mood") +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = DenseEmo_NA), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=DenseEmo_NA)) +
  scale_fill_manual(values = c("Low Negative Emotion" = "#2957FF", "High Negative Emotion" = "#F72836"), name = "Negative Emotion") +
  scale_color_manual(values = c("Low Negative Emotion" = "#2957FF", "High Negative Emotion" = "#F72836"), name = "Negative Emotion") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16)) +
  facet_wrap(~grade_100)

BRM_NA_Grade_Med


#### PE ####
plot_NAMood_PE_1 <- conditional_effects(NAMood_PE, 
                                        effects = "DaysSinceDense:PE_1",
                                        int_conditions = list(PE_1 = c((mean(MoodAndDenseEmo$PE_1) + sd(MoodAndDenseEmo$PE_1)), (mean(MoodAndDenseEmo$PE_1) - sd(MoodAndDenseEmo$PE_1)))))
plot_NAMood_PE_1_df <- as.data.frame(plot_NAMood_PE_1$`DaysSinceDense:PE_1`)
plot_NAMood_PE_1_df$PE_1 <- as.character(plot_NAMood_PE_1_df$PE_1)
plot_NAMood_PE_1_df$PE_1[which(plot_NAMood_PE_1_df$PE_1 == "11.7795352865165")] <- "Positive PE"
plot_NAMood_PE_1_df$PE_1[which(plot_NAMood_PE_1_df$PE_1 == "-12.8483415435724")] <- "Negative PE"

plot_PAMood_PE_1 <- conditional_effects(PAMood_PE,
                                        effects = "DaysSinceDense:PE_1",
                                        int_conditions = list(PE_1 = c((mean(MoodAndDenseEmo$PE_1) + sd(MoodAndDenseEmo$PE_1)), (mean(MoodAndDenseEmo$PE_1) - sd(MoodAndDenseEmo$PE_1)))))
plot_PAMood_PE_1_df <- as.data.frame(plot_PAMood_PE_1$`DaysSinceDense:PE_1`)
plot_PAMood_PE_1_df$PE_1 <- as.character(plot_PAMood_PE_1_df$PE_1)
plot_PAMood_PE_1_df$PE_1[which(plot_PAMood_PE_1_df$PE_1 == "11.7795352865165")] <- "Positive PE"
plot_PAMood_PE_1_df$PE_1[which(plot_PAMood_PE_1_df$PE_1 == "-12.8483415435724")] <- "Negative PE"


##### ** NA ####
BRM_NA_PE <- ggplot(plot_NAMood_PE_1_df, aes(x = DaysSinceDense, y = estimate__)) +
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Negative Mood") +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = PE_1), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=PE_1)) +
  scale_fill_manual(values = c("Positive PE" = "#2957FF", "Negative PE" = "#F72836"), name = "Prediction Error") +
  scale_color_manual(values = c("Positive PE" = "#2957FF", "Negative PE" = "#F72836"), name = "Prediction Error") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16)) +
  ylim(-8,8)


##### ** PA ####
BRM_PA_PE <- ggplot(plot_PAMood_PE_1_df, aes(x = DaysSinceDense, y = estimate__)) +
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Positive Mood") +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = PE_1), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=PE_1)) +
  scale_fill_manual(values = c("Positive PE" = "#2957FF", "Negative PE" = "#F72836"), name = "Prediction Error") +
  scale_color_manual(values = c("Positive PE" = "#2957FF", "Negative PE" = "#F72836"), name = "Prediction Error") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16)) +
  ylim(-8, 8)


BRM_NA_PE
BRM_PA_PE

##### ** Med: PE ####
mean(MoodAndDenseEmo$PE_1) - sd(MoodAndDenseEmo$PE_1)
mean(MoodAndDenseEmo$PE_1) + sd(MoodAndDenseEmo$PE_1)

condition4 <- data.frame(PE_1 = c(-12.84, 11.77))
cond4 <- conditional_effects(MedMod_NAMood_PE_DenseNA,
                             effects = "DaysSinceDense:DenseEmo_NA",
                             int_conditions = list(DenseEmo_NA = DenseEmo_NA_SD),
                             conditions = condition4)
plot_MoodPe_MedEmo_new_df <- as.data.frame(cond4$`DeltaMoodNA.DeltaMoodNA_DaysSinceDense:DenseEmo_NA`)
plot_MoodPe_MedEmo_new_df$DenseEmo_NA

plot_MoodPe_MedEmo_new_df$DenseEmo_NA <- as.character(plot_MoodPe_MedEmo_new_df$DenseEmo_NA)
plot_MoodPe_MedEmo_new_df$DenseEmo_NA[which(plot_MoodPe_MedEmo_new_df$DenseEmo_NA == "71.7117813640349")] <- "High Negative Emotion"
plot_MoodPe_MedEmo_new_df$DenseEmo_NA[which(plot_MoodPe_MedEmo_new_df$DenseEmo_NA == "23.0120736727898")] <- "Low Negative Emotion"


BRM_NA_PE_Med <- ggplot(plot_MoodPe_MedEmo_new_df, aes(x = DaysSinceDense, y = estimate__)) +
  labs(x = "Days Since Grade Reveal",
       y = "Mean Change in Negative Mood") +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = DenseEmo_NA), alpha=0.2) +
  geom_line(linewidth=1, position=position_dodge(0.05), aes(color=DenseEmo_NA)) +
  scale_fill_manual(values = c("Low Negative Emotion" = "#2957FF", "High Negative Emotion" = "#F72836"), name = "Negative Emotion") +
  scale_color_manual(values = c("Low Negative Emotion" = "#2957FF", "High Negative Emotion" = "#F72836"), name = "Negative Emotion") +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_line(color = "grey70"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        text = element_text(size = 16)) +
  facet_wrap(~PE_1)

BRM_NA_PE_Med


# Models for comparison ####
#### Negative Mood ####
#### ** Emotion ####
DenseNA_pls_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                       data = MoodAndDenseEmo, 
                       backend = "cmdstanr", cores = 5,
                       iter = 4000, control = list(adapt_delta = 0.95),
                       family = gaussian(),
                       file = "Brms_results/DenseNA_pls_DSD.brm", file_refit = "on_change")
DenseNA_pls_DSD<- readRDS("Brms_results/DenseNA_pls_DSD.brm.rds")
summary(DenseNA_pls_DSD)
pp_check(DenseNA_pls_DSD)

DenseNA_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                   data = MoodAndDenseEmo, 
                   backend = "cmdstanr", cores = 5,
                   iter = 4000, control = list(adapt_delta = 0.95),
                   family = gaussian(),
                   file = "Brms_results/DenseNA_DSD.brm", file_refit = "on_change")
DenseNA_DSD<- readRDS("Brms_results/DenseNA_DSD.brm.rds")
summary(DenseNA_DSD)
pp_check(DenseNA_DSD)

#### ** Grade ####
NAgrade_pls_DSD <- brm(DeltaMood_NA ~ grade_100 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                       data = MoodAndDenseEmo, 
                       backend = "cmdstanr", cores = 5,
                       iter = 4000, control = list(adapt_delta = 0.95),
                       family = gaussian(),
                       file = "Brms_results/NAgrade_pls_DSD.brm", file_refit = "on_change")
NAgrade_pls_DSD<- readRDS("Brms_results/NAgrade_pls_DSD.brm.rds")
summary(NAgrade_pls_DSD)

NAgrade_DSD <- brm(DeltaMood_NA ~ grade_100 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                   data = MoodAndDenseEmo, 
                   backend = "cmdstanr", cores = 5,
                   iter = 4000, control = list(adapt_delta = 0.95),
                   family = gaussian(),
                   file = "Brms_results/NAgrade_DSD.brm", file_refit = "on_change")
NAgrade_DSD<- readRDS("Brms_results/NAgrade_DSD.brm.rds")
summary(NAgrade_DSD)

#### ** PE ####
NApe_pls_DSD <- brm(DeltaMood_NA ~ PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                    data = MoodAndDenseEmo, 
                    backend = "cmdstanr", cores = 5,
                    iter = 4000, control = list(adapt_delta = 0.95),
                    family = gaussian(),
                    file = "Brms_results/NApe_pls_DSD.brm", file_refit = "on_change")
NApe_pls_DSD<- readRDS("Brms_results/NApe_pls_DSD.brm.rds")
summary(NApe_pls_DSD)

NApe_DSD <- brm(DeltaMood_NA ~ PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                data = MoodAndDenseEmo, 
                backend = "cmdstanr", cores = 5,
                iter = 4000, control = list(adapt_delta = 0.95),
                family = gaussian(),
                file = "Brms_results/NApe_DSD.brm", file_refit = "on_change")
NApe_DSD<- readRDS("Brms_results/NApe_DSD.brm.rds")
summary(NApe_DSD)

#### ** Emo and Grade ####
DenseNA_pls_grade_pls_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA + grade_100 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                 data = MoodAndDenseEmo, 
                                 backend = "cmdstanr", cores = 5,
                                 iter = 4000, control = list(adapt_delta = 0.95),
                                 family = gaussian(),
                                 file = "Brms_results/DenseNA_pls_grade_pls_DSD.brm", file_refit = "on_change")
DenseNA_pls_grade_pls_DSD<- readRDS("Brms_results/DenseNA_pls_grade_pls_DSD.brm.rds")
summary(DenseNA_pls_grade_pls_DSD)

DenseNA_grade_pls_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * grade_100 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                             data = MoodAndDenseEmo, 
                             backend = "cmdstanr", cores = 5,
                             iter = 4000, control = list(adapt_delta = 0.95),
                             family = gaussian(),
                             file = "Brms_results/DenseNA_grade_pls_DSD.brm", file_refit = "on_change")
DenseNA_grade_pls_DSD<- readRDS("Brms_results/DenseNA_grade_pls_DSD.brm.rds")
summary(DenseNA_grade_pls_DSD)

DenseNA_grade_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * grade_100 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                         data = MoodAndDenseEmo, 
                         backend = "cmdstanr", cores = 5,
                         iter = 4000, control = list(adapt_delta = 0.95),
                         family = gaussian(),
                         file = "Brms_results/DenseNA_grade_DSD.brm", file_refit = "on_change")
DenseNA_grade_DSD<- readRDS("Brms_results/DenseNA_grade_DSD.brm.rds")
summary(DenseNA_grade_DSD)

DenseNA_DSD_pls_grade_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * DaysSinceDense + grade_100 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                 data = MoodAndDenseEmo, 
                                 backend = "cmdstanr", cores = 5,
                                 iter = 4000, control = list(adapt_delta = 0.95),
                                 family = gaussian(),
                                 file = "Brms_results/DenseNA_DSD_pls_grade_DSD.brm", file_refit = "on_change")
DenseNA_DSD_pls_grade_DSD<- readRDS("Brms_results/DenseNA_DSD_pls_grade_DSD.brm.rds")
summary(DenseNA_DSD_pls_grade_DSD)

#### ** Emo and PE ####
DenseNA_pls_PE_pls_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA + PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/DenseNA_pls_PE_pls_DSD.brm", file_refit = "on_change")
DenseNA_pls_PE_pls_DSD<- readRDS("Brms_results/DenseNA_pls_PE_pls_DSD.brm.rds")
summary(DenseNA_pls_PE_pls_DSD)

DenseNA_PE_pls_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                          data = MoodAndDenseEmo, 
                          backend = "cmdstanr", cores = 5,
                          iter = 4000, control = list(adapt_delta = 0.95),
                          family = gaussian(),
                          file = "Brms_results/DenseNA_PE_pls_DSD.brm", file_refit = "on_change")
DenseNA_PE_pls_DSD<- readRDS("Brms_results/DenseNA_PE_pls_DSD.brm.rds")
summary(DenseNA_PE_pls_DSD)

DenseNA_PE_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                      data = MoodAndDenseEmo, 
                      backend = "cmdstanr", cores = 5,
                      iter = 4000, control = list(adapt_delta = 0.95),
                      family = gaussian(),
                      file = "Brms_results/DenseNA_PE_DSD.brm", file_refit = "on_change")
DenseNA_PE_DSD<- readRDS("Brms_results/DenseNA_PE_DSD.brm.rds")
summary(DenseNA_PE_DSD)

DenseNA_DSD_pls_PE_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * DaysSinceDense + PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/DenseNA_DSD_pls_PE_DSD.brm", file_refit = "on_change")
DenseNA_DSD_pls_PE_DSD<- readRDS("Brms_results/DenseNA_DSD_pls_PE_DSD.brm.rds")
summary(DenseNA_DSD_pls_PE_DSD)

#### ** Grade and PE ####
NAgrade_pls_PE_pls_DSD <- brm(DeltaMood_NA ~ grade_100 + PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/NAgrade_pls_PE_pls_DSD.brm", file_refit = "on_change")
NAgrade_pls_PE_pls_DSD<- readRDS("Brms_results/NAgrade_pls_PE_pls_DSD.brm.rds")
summary(NAgrade_pls_PE_pls_DSD)

NAgrade_PE_pls_DSD <- brm(DeltaMood_NA ~ grade_100 * PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                          data = MoodAndDenseEmo, 
                          backend = "cmdstanr", cores = 5,
                          iter = 4000, control = list(adapt_delta = 0.95),
                          family = gaussian(),
                          file = "Brms_results/NAgrade_PE_pls_DSD.brm", file_refit = "on_change")
NAgrade_PE_pls_DSD<- readRDS("Brms_results/NAgrade_PE_pls_DSD.brm.rds")
summary(NAgrade_PE_pls_DSD)

NAgrade_PE_DSD <- brm(DeltaMood_NA ~ grade_100 * PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                      data = MoodAndDenseEmo, 
                      backend = "cmdstanr", cores = 5,
                      iter = 4000, control = list(adapt_delta = 0.95),
                      family = gaussian(),
                      file = "Brms_results/NAgrade_PE_DSD.brm", file_refit = "on_change")
NAgrade_PE_DSD<- readRDS("Brms_results/NAgrade_PE_DSD.brm.rds")
summary(NAgrade_PE_DSD)

NAgrade_DSD_pls_PE_DSD <- brm(DeltaMood_NA ~ grade_100 * DaysSinceDense + PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/NAgrade_DSD_pls_PE_DSD.brm", file_refit = "on_change")
NAgrade_DSD_pls_PE_DSD <- readRDS("Brms_results/NAgrade_DSD_pls_PE_DSD.brm.rds")
summary(NAgrade_DSD_pls_PE_DSD)

#### ** Emo, Grade, and PE ####
DenseNA_pls_grade_pls_PE_pls_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA + grade_100 + PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                        data = MoodAndDenseEmo, 
                                        backend = "cmdstanr", cores = 5,
                                        iter = 4000, control = list(adapt_delta = 0.95),
                                        family = gaussian(),
                                        file = "Brms_results/DenseNA_pls_grade_pls_PE_pls_DSD.brm", file_refit = "on_change")
DenseNA_pls_grade_pls_PE_pls_DSD<- readRDS("Brms_results/DenseNA_pls_grade_pls_PE_pls_DSD.brm.rds")
summary(DenseNA_pls_grade_pls_PE_pls_DSD)

DenseNA_grade_PE_pls_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * grade_100 * PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                data = MoodAndDenseEmo, 
                                backend = "cmdstanr", cores = 5,
                                iter = 4000, control = list(adapt_delta = 0.95),
                                family = gaussian(),
                                file = "Brms_results/DenseNA_grade_PE_pls_DSD.brm", file_refit = "on_change")
DenseNA_grade_PE_pls_DSD<- readRDS("Brms_results/DenseNA_grade_PE_pls_DSD.brm.rds")
summary(DenseNA_grade_PE_pls_DSD)

DenseNA_grade_PE_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * grade_100 * PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                            data = MoodAndDenseEmo, 
                            backend = "cmdstanr", cores = 5,
                            iter = 4000, control = list(adapt_delta = 0.95),
                            family = gaussian(),
                            file = "Brms_results/DenseNA_grade_PE_DSD.brm", file_refit = "on_change")
DenseNA_grade_PE_DSD<- readRDS("Brms_results/DenseNA_grade_PE_DSD.brm.rds")
summary(DenseNA_grade_PE_DSD)

DenseNA_DSD_pls_grade_DSD_pls_PE_DSD <- brm(DeltaMood_NA ~ DenseEmo_NA * DaysSinceDense + grade_100 * DaysSinceDense + PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                            data = MoodAndDenseEmo, 
                                            backend = "cmdstanr", cores = 5,
                                            iter = 4000, control = list(adapt_delta = 0.95),
                                            family = gaussian(),
                                            file = "Brms_results/DenseNA_DSD_pls_grade_DSD_pls_PE_DSD.brm", file_refit = "on_change")
DenseNA_DSD_pls_grade_DSD_pls_PE_DSD<- readRDS("Brms_results/DenseNA_DSD_pls_grade_DSD_pls_PE_DSD.brm.rds")
summary(DenseNA_DSD_pls_grade_DSD_pls_PE_DSD)

#### Neg: Loos ####
loo_DenseNA_pls_DSD <- loo(DenseNA_pls_DSD, save_psis = TRUE)
loo_DenseNA_DSD <- loo(DenseNA_DSD, save_psis = TRUE)

loo_NAgrade_pls_DSD <- loo(NAgrade_pls_DSD, save_psis = TRUE)
loo_NAgrade_DSD <- loo(NAgrade_DSD, save_psis = TRUE)

loo_NApe_pls_DSD <- loo(NApe_pls_DSD, save_psis = TRUE)
loo_NApe_DSD <- loo(NApe_DSD, save_psis = TRUE)

loo_DenseNA_pls_grade_pls_DSD <- loo(DenseNA_pls_grade_pls_DSD, save_psis = TRUE)
loo_DenseNA_grade_pls_DSD <- loo(DenseNA_grade_pls_DSD, save_psis = TRUE)
loo_DenseNA_grade_DSD <- loo(DenseNA_grade_DSD, save_psis = TRUE)
loo_DenseNA_DSD_pls_grade_DSD <- loo(DenseNA_DSD_pls_grade_DSD, save_psis = TRUE)

loo_DenseNA_pls_PE_pls_DSD <- loo(DenseNA_pls_PE_pls_DSD, save_psis = TRUE)
loo_DenseNA_PE_pls_DSD <- loo(DenseNA_PE_pls_DSD, save_psis = TRUE)
loo_DenseNA_PE_DSD <- loo(DenseNA_PE_DSD, save_psis = TRUE)
loo_DenseNA_DSD_pls_PE_DSD <- loo(DenseNA_DSD_pls_PE_DSD, save_psis = TRUE)

loo_NAgrade_pls_PE_pls_DSD <- loo(NAgrade_pls_PE_pls_DSD, save_psis = TRUE)
loo_NAgrade_PE_pls_DSD <- loo(NAgrade_PE_pls_DSD, save_psis = TRUE)
loo_NAgrade_PE_DSD <- loo(NAgrade_PE_DSD, save_psis = TRUE)
loo_NAgrade_DSD_pls_PE_DSD <- loo(NAgrade_DSD_pls_PE_DSD, save_psis = TRUE)

loo_DenseNA_pls_grade_pls_PE_pls_DSD <- loo(DenseNA_pls_grade_pls_PE_pls_DSD, save_psis = TRUE)
loo_DenseNA_grade_PE_pls_DSD <- loo(DenseNA_grade_PE_pls_DSD, save_psis = TRUE)
loo_DenseNA_grade_PE_DSD <- loo(DenseNA_grade_PE_DSD, save_psis = TRUE)
loo_DenseNA_DSD_pls_grade_DSD_pls_PE_DSD <- loo(DenseNA_DSD_pls_grade_DSD_pls_PE_DSD, save_psis = TRUE)


loo_compare(loo_DenseNA_pls_DSD, loo_DenseNA_DSD,
            loo_NAgrade_pls_DSD, loo_NApe_DSD,
            loo_DenseNA_pls_grade_pls_DSD, loo_DenseNA_grade_pls_DSD, loo_DenseNA_grade_DSD, loo_DenseNA_DSD_pls_grade_DSD,
            loo_DenseNA_pls_PE_pls_DSD, loo_DenseNA_PE_pls_DSD, loo_DenseNA_PE_DSD, loo_DenseNA_DSD_pls_PE_DSD,
            loo_NAgrade_pls_PE_pls_DSD, loo_NAgrade_PE_pls_DSD, loo_NAgrade_PE_DSD, loo_NAgrade_DSD_pls_PE_DSD,
            loo_DenseNA_pls_grade_pls_PE_pls_DSD, loo_DenseNA_grade_PE_pls_DSD,loo_DenseNA_grade_PE_DSD,loo_DenseNA_DSD_pls_grade_DSD_pls_PE_DSD)



#### Positive Mood ####
#### ** Emotion ####
DensePA_pls_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                       data = MoodAndDenseEmo, 
                       backend = "cmdstanr", cores = 5,
                       iter = 4000, control = list(adapt_delta = 0.95),
                       family = gaussian(),
                       file = "Brms_results/DensePA_pls_DSD.brm", file_refit = "on_change")
DensePA_pls_DSD<- readRDS("Brms_results/DensePA_pls_DSD.brm.rds")
summary(DensePA_pls_DSD)
pp_check(DensePA_pls_DSD)

DensePA_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                   data = MoodAndDenseEmo, 
                   backend = "cmdstanr", cores = 5,
                   iter = 4000, control = list(adapt_delta = 0.95),
                   family = gaussian(),
                   file = "Brms_results/DensePA_DSD.brm", file_refit = "on_change")
DensePA_DSD<- readRDS("Brms_results/DensePA_DSD.brm.rds")
summary(DensePA_DSD)
pp_check(DensePA_DSD)

#### ** Grade ####
PAgrade_pls_DSD <- brm(DeltaMood_PA ~ grade_100 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                       data = MoodAndDenseEmo, 
                       backend = "cmdstanr", cores = 5,
                       iter = 4000, control = list(adapt_delta = 0.95),
                       family = gaussian(),
                       file = "Brms_results/PAgrade_pls_DSD.brm", file_refit = "on_change")
PAgrade_pls_DSD<- readRDS("Brms_results/PAgrade_pls_DSD.brm.rds")
summary(PAgrade_pls_DSD)

PAgrade_DSD <- brm(DeltaMood_PA ~ grade_100 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                   data = MoodAndDenseEmo, 
                   backend = "cmdstanr", cores = 5,
                   iter = 4000, control = list(adapt_delta = 0.95),
                   family = gaussian(),
                   file = "Brms_results/PAgrade_DSD.brm", file_refit = "on_change")
PAgrade_DSD<- readRDS("Brms_results/PAgrade_DSD.brm.rds")
summary(PAgrade_DSD)

#### ** PE ####
PApe_pls_DSD <- brm(DeltaMood_PA ~ PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                    data = MoodAndDenseEmo,
                    backend = "cmdstanr", cores = 5,
                    iter = 4000, control = list(adapt_delta = 0.95),
                    family = gaussian(),
                    file = "Brms_results/PApe_pls_DSD.brm", file_refit = "on_change")
PApe_pls_DSD<- readRDS("Brms_results/PApe_pls_DSD.brm.rds")
summary(PApe_pls_DSD)

PApe_DSD <- brm(DeltaMood_PA ~ PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                data = MoodAndDenseEmo,
                backend = "cmdstanr", cores = 5,
                iter = 4000, control = list(adapt_delta = 0.95),
                family = gaussian(),
                file = "Brms_results/PApe_DSD.brm", file_refit = "on_change")
PApe_DSD<- readRDS("Brms_results/PApe_DSD.brm.rds")
summary(PApe_DSD)

#### ** Emo and Grade ####
DensePA_pls_grade_pls_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA + grade_100 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                 data = MoodAndDenseEmo, 
                                 backend = "cmdstanr", cores = 5,
                                 iter = 4000, control = list(adapt_delta = 0.95),
                                 family = gaussian(),
                                 file = "Brms_results/DensePA_pls_grade_pls_DSD.brm", file_refit = "on_change")
DensePA_pls_grade_pls_DSD<- readRDS("Brms_results/DensePA_pls_grade_pls_DSD.brm.rds")
summary(DensePA_pls_grade_pls_DSD)

DensePA_grade_pls_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * grade_100 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                             data = MoodAndDenseEmo, 
                             backend = "cmdstanr", cores = 5,
                             iter = 4000, control = list(adapt_delta = 0.95),
                             family = gaussian(),
                             file = "Brms_results/DensePA_grade_pls_DSD.brm", file_refit = "on_change")
DensePA_grade_pls_DSD<- readRDS("Brms_results/DensePA_grade_pls_DSD.brm.rds")
summary(DensePA_grade_pls_DSD)

DensePA_grade_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * grade_100 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                         data = MoodAndDenseEmo, 
                         backend = "cmdstanr", cores = 5,
                         iter = 4000, control = list(adapt_delta = 0.95),
                         family = gaussian(),
                         file = "Brms_results/DensePA_grade_DSD.brm", file_refit = "on_change")
DensePA_grade_DSD<- readRDS("Brms_results/DensePA_grade_DSD.brm.rds")
summary(DensePA_grade_DSD)

DensePA_DSD_pls_grade_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * DaysSinceDense + grade_100 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                 data = MoodAndDenseEmo, 
                                 backend = "cmdstanr", cores = 5,
                                 iter = 4000, control = list(adapt_delta = 0.95),
                                 family = gaussian(),
                                 file = "Brms_results/DensePA_DSD_pls_grade_DSD.brm", file_refit = "on_change")
DensePA_DSD_pls_grade_DSD<- readRDS("Brms_results/DensePA_DSD_pls_grade_DSD.brm.rds")
summary(DensePA_DSD_pls_grade_DSD)

#### ** Emo and PE ####
DensePA_pls_PE_pls_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA + PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/DensePA_pls_PE_pls_DSD.brm", file_refit = "on_change")
DensePA_pls_PE_pls_DSD<- readRDS("Brms_results/DensePA_pls_PE_pls_DSD.brm.rds")
summary(DensePA_pls_PE_pls_DSD)

DensePA_PE_pls_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                          data = MoodAndDenseEmo, 
                          backend = "cmdstanr", cores = 5,
                          iter = 4000, control = list(adapt_delta = 0.95),
                          family = gaussian(),
                          file = "Brms_results/DensePA_PE_pls_DSD.brm", file_refit = "on_change")
DensePA_PE_pls_DSD<- readRDS("Brms_results/DensePA_PE_pls_DSD.brm.rds")
summary(DensePA_PE_pls_DSD)

DensePA_PE_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                      data = MoodAndDenseEmo, 
                      backend = "cmdstanr", cores = 5,
                      iter = 4000, control = list(adapt_delta = 0.95),
                      family = gaussian(),
                      file = "Brms_results/DensePA_PE_DSD.brm", file_refit = "on_change")
DensePA_PE_DSD<- readRDS("Brms_results/DensePA_PE_DSD.brm.rds")
summary(DensePA_PE_DSD)

DensePA_DSD_pls_PE_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * DaysSinceDense + PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/DensePA_DSD_pls_PE_DSD.brm", file_refit = "on_change")
DensePA_DSD_pls_PE_DSD<- readRDS("Brms_results/DensePA_DSD_pls_PE_DSD.brm.rds")
summary(DensePA_DSD_pls_PE_DSD)

#### ** Grade and PE ####
PAgrade_pls_PE_pls_DSD <- brm(DeltaMood_PA ~ grade_100 + PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/PAgrade_pls_PE_pls_DSD.brm", file_refit = "on_change")
PAgrade_pls_PE_pls_DSD<- readRDS("Brms_results/PAgrade_pls_PE_pls_DSD.brm.rds")
summary(PAgrade_pls_PE_pls_DSD)

PAgrade_PE_pls_DSD <- brm(DeltaMood_PA ~ grade_100 * PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                          data = MoodAndDenseEmo, 
                          backend = "cmdstanr", cores = 5,
                          iter = 4000, control = list(adapt_delta = 0.95),
                          family = gaussian(),
                          file = "Brms_results/PAgrade_PE_pls_DSD.brm", file_refit = "on_change")
PAgrade_PE_pls_DSD<- readRDS("Brms_results/PAgrade_PE_pls_DSD.brm.rds")
summary(PAgrade_PE_pls_DSD)

PAgrade_PE_DSD <- brm(DeltaMood_PA ~ grade_100 * PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                      data = MoodAndDenseEmo, 
                      backend = "cmdstanr", cores = 5,
                      iter = 4000, control = list(adapt_delta = 0.95),
                      family = gaussian(),
                      file = "Brms_results/PAgrade_PE_DSD.brm", file_refit = "on_change")
PAgrade_PE_DSD<- readRDS("Brms_results/PAgrade_PE_DSD.brm.rds")
summary(PAgrade_PE_DSD)

PAgrade_DSD_pls_PE_DSD <- brm(DeltaMood_PA ~ grade_100 * DaysSinceDense + PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/PAgrade_DSD_pls_PE_DSD.brm", file_refit = "on_change")
PAgrade_DSD_pls_PE_DSD<- readRDS("Brms_results/PAgrade_DSD_pls_PE_DSD.brm.rds")
summary(PAgrade_DSD_pls_PE_DSD)

#### ** Emo, Grade, and PE ####
DensePA_pls_grade_pls_PE_pls_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA + grade_100 + PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                        data = MoodAndDenseEmo, 
                                        backend = "cmdstanr", cores = 5,
                                        iter = 4000, control = list(adapt_delta = 0.95),
                                        family = gaussian(),
                                        file = "Brms_results/DensePA_pls_grade_pls_PE_pls_DSD.brm", file_refit = "on_change")
DensePA_pls_grade_pls_PE_pls_DSD<- readRDS("Brms_results/DensePA_pls_grade_pls_PE_pls_DSD.brm.rds")
summary(DensePA_pls_grade_pls_PE_pls_DSD)

DensePA_grade_PE_pls_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * grade_100 * PE_1 + DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                data = MoodAndDenseEmo, 
                                backend = "cmdstanr", cores = 5,
                                iter = 4000, control = list(adapt_delta = 0.95),
                                family = gaussian(),
                                file = "Brms_results/DensePA_grade_PE_pls_DSD.brm", file_refit = "on_change")
DensePA_grade_PE_pls_DSD<- readRDS("Brms_results/DensePA_grade_PE_pls_DSD.brm.rds")
summary(DensePA_grade_PE_pls_DSD)

DensePA_grade_PE_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * grade_100 * PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                            data = MoodAndDenseEmo, 
                            backend = "cmdstanr", cores = 5,
                            iter = 4000, control = list(adapt_delta = 0.95),
                            family = gaussian(),
                            file = "Brms_results/DensePA_grade_PE_DSD.brm", file_refit = "on_change")
DensePA_grade_PE_DSD<- readRDS("Brms_results/DensePA_grade_PE_DSD.brm.rds")
summary(DensePA_grade_PE_DSD)

DensePA_DSD_pls_grade_DSD_pls_PE_DSD <- brm(DeltaMood_PA ~ DenseEmo_PA * DaysSinceDense + grade_100 * DaysSinceDense + PE_1 * DaysSinceDense + (1 |cohort_class/Repeat_ID),
                                            data = MoodAndDenseEmo, 
                                            backend = "cmdstanr", cores = 5,
                                            iter = 4000, control = list(adapt_delta = 0.95),
                                            family = gaussian(),
                                            file = "Brms_results/DensePA_DSD_pls_grade_DSD_pls_PE_DSD.brm", file_refit = "on_change")
DensePA_DSD_pls_grade_DSD_pls_PE_DSD<- readRDS("Brms_results/DensePA_DSD_pls_grade_DSD_pls_PE_DSD.brm.rds")
summary(DensePA_DSD_pls_grade_DSD_pls_PE_DSD)

#### Pos: Loos ####
loo_DensePA_pls_DSD <- loo(DensePA_pls_DSD, save_psis = TRUE)
loo_DensePA_DSD <- loo(DensePA_DSD, save_psis = TRUE)

loo_PAgrade_pls_DSD <- loo(PAgrade_pls_DSD, save_psis = TRUE)
loo_PAgrade_DSD <- loo(PAgrade_DSD, save_psis = TRUE)

loo_PApe_pls_DSD <- loo(PApe_pls_DSD, save_psis = TRUE)
loo_PApe_DSD <- loo(PApe_DSD, save_psis = TRUE)

loo_DensePA_pls_grade_pls_DSD <- loo(DensePA_pls_grade_pls_DSD, save_psis = TRUE)
loo_DensePA_grade_pls_DSD <- loo(DensePA_grade_pls_DSD, save_psis = TRUE)
loo_DensePA_grade_DSD <- loo(DensePA_grade_DSD, save_psis = TRUE)
loo_DensePA_DSD_pls_grade_DSD <- loo(DensePA_DSD_pls_grade_DSD, save_psis = TRUE)

loo_DensePA_pls_PE_pls_DSD <- loo(DensePA_pls_PE_pls_DSD, save_psis = TRUE)
loo_DensePA_PE_pls_DSD <- loo(DensePA_PE_pls_DSD, save_psis = TRUE)
loo_DensePA_PE_DSD <- loo(DensePA_PE_DSD, save_psis = TRUE)
loo_DensePA_DSD_pls_PE_DSD <- loo(DensePA_DSD_pls_PE_DSD, save_psis = TRUE)

loo_PAgrade_pls_PE_pls_DSD <- loo(PAgrade_pls_PE_pls_DSD, save_psis = TRUE)
loo_PAgrade_PE_pls_DSD <- loo(PAgrade_PE_pls_DSD, save_psis = TRUE)
loo_PAgrade_PE_DSD <- loo(PAgrade_PE_DSD, save_psis = TRUE)
loo_PAgrade_DSD_pls_PE_DSD <- loo(PAgrade_DSD_pls_PE_DSD, save_psis = TRUE)

loo_DensePA_pls_grade_pls_PE_pls_DSD <- loo(DensePA_pls_grade_pls_PE_pls_DSD, save_psis = TRUE)
loo_DensePA_grade_PE_pls_DSD <- loo(DensePA_grade_PE_pls_DSD, save_psis = TRUE)
loo_DensePA_grade_PE_DSD <- loo(DensePA_grade_PE_DSD, save_psis = TRUE)
loo_DensePA_DSD_pls_grade_DSD_pls_PE_DSD <- loo(DensePA_DSD_pls_grade_DSD_pls_PE_DSD, save_psis = TRUE)


loo_compare(loo_DensePA_pls_DSD, loo_DensePA_DSD,
            loo_PAgrade_pls_DSD, loo_PApe_DSD,
            loo_DensePA_pls_grade_pls_DSD, loo_DensePA_grade_pls_DSD, loo_DensePA_grade_DSD, loo_DensePA_DSD_pls_grade_DSD,
            loo_DensePA_pls_PE_pls_DSD, loo_DensePA_PE_pls_DSD, loo_DensePA_PE_DSD, loo_DensePA_DSD_pls_PE_DSD,
            loo_PAgrade_pls_PE_pls_DSD, loo_PAgrade_PE_pls_DSD, loo_PAgrade_PE_DSD, loo_PAgrade_DSD_pls_PE_DSD,
            loo_DensePA_pls_grade_pls_PE_pls_DSD, loo_DensePA_grade_PE_pls_DSD,loo_DensePA_grade_PE_DSD,loo_DensePA_DSD_pls_grade_DSD_pls_PE_DSD)


### PE_1 EWMA ################################################################################################################################################################################################################
PE_1EWMA <- arrange(unique(MoodAndDenseEmo[,c("ID", "exam_num", "PE_1")]), ID)
PE_1EWMA <- PE_1EWMA[which(!is.na(PE_1EWMA$PE_1)),]

for (j in unique(PE_1EWMA$ID)) {
  participant_data <- PE_1EWMA %>% filter(ID == j)
  for (i in 1:nrow(participant_data)) {
    if (i == 1) {
      PE_1EWMA$EWMA.25[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- participant_data$PE_1[i]
    } else {
      prev_EWMA.25 <- PE_1EWMA$EWMA.25[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i - 1]]
      PE_1EWMA$EWMA.25[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- EWMA(0.25, participant_data$PE_1[i], prev_EWMA.25)
    }
    if (i == 1) {
      PE_1EWMA$EWMA.5[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- participant_data$PE_1[i]
    } else {
      prev_EWMA.5 <- PE_1EWMA$EWMA.5[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i - 1]]
      PE_1EWMA$EWMA.5[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- EWMA(0.5, participant_data$PE_1[i], prev_EWMA.5)
    }
    if (i == 1) {
      PE_1EWMA$EWMA.75[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- participant_data$PE_1[i]
    } else {
      prev_EWMA.75 <- PE_1EWMA$EWMA.75[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i - 1]]
      PE_1EWMA$EWMA.75[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- EWMA(0.75, participant_data$PE_1[i], prev_EWMA.75)
    }
    if (i == 1) {
      PE_1EWMA$EWMA.95[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- participant_data$PE_1[i]
    } else {
      prev_EWMA.95 <- PE_1EWMA$EWMA.95[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i - 1]]
      PE_1EWMA$EWMA.95[PE_1EWMA$ID == j & PE_1EWMA$exam_num == participant_data$exam_num[i]] <- EWMA(0.95, participant_data$PE_1[i], prev_EWMA.95)
    }
  }
}



MoodAndDenseEmo$PE_1EWMA.25 <- NA
MoodAndDenseEmo$PE_1EWMA.5 <- NA
MoodAndDenseEmo$PE_1EWMA.75 <- NA
MoodAndDenseEmo$PE_1EWMA.95 <- NA
for (i in unique(MoodAndDenseEmo$ID)) {
  for (j in unique(MoodAndDenseEmo$exam_num)) {
    MoodAndDenseEmo$PE_1EWMA.25[which(MoodAndDenseEmo$ID == i & MoodAndDenseEmo$exam_num == j)] <- PE_1EWMA$EWMA.25[which(PE_1EWMA$ID == i & PE_1EWMA$exam_num == j)]
    MoodAndDenseEmo$PE_1EWMA.5[which(MoodAndDenseEmo$ID == i & MoodAndDenseEmo$exam_num == j)] <- PE_1EWMA$EWMA.5[which(PE_1EWMA$ID == i & PE_1EWMA$exam_num == j)]
    MoodAndDenseEmo$PE_1EWMA.75[which(MoodAndDenseEmo$ID == i & MoodAndDenseEmo$exam_num == j)] <- PE_1EWMA$EWMA.75[which(PE_1EWMA$ID == i & PE_1EWMA$exam_num == j)]
    MoodAndDenseEmo$PE_1EWMA.95[which(MoodAndDenseEmo$ID == i & MoodAndDenseEmo$exam_num == j)] <- PE_1EWMA$EWMA.95[which(PE_1EWMA$ID == i & PE_1EWMA$exam_num == j)]
  }
}

#### Raw NA PE ####
RawNA_PE_1 <- brm(NA_score ~ PE_1 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1 |cohort_class/Repeat_ID),
                       data = MoodAndDenseEmo, 
                       backend = "cmdstanr", cores = 5,
                       iter = 4000, control = list(adapt_delta = 0.95),
                       family = gaussian(),
                       file = "Brms_results/RawNA_PE_1.brm", file_refit = "on_change")
RawNA_PE_1 <- readRDS("Brms_results/RawNA_PE_1.brm.rds")
summary(RawNA_PE_1)
plot(RawNA_PE_1)
pp_check(RawNA_PE_1)
conditional_effects(RawNA_PE_1)

RawNA_PE_1EWMA.25 <- brm(NA_score ~ PE_1EWMA.25 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.25 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/RawNA_PE_1EWMA.25.brm", file_refit = "on_change")
RawNA_PE_1EWMA.25 <- readRDS("Brms_results/RawNA_PE_1EWMA.25.brm.rds")
summary(RawNA_PE_1EWMA.25)
plot(RawNA_PE_1EWMA.25)
pp_check(RawNA_PE_1EWMA.25)

RawNA_PE_1EWMA.5 <- brm(NA_score ~ PE_1EWMA.5 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.5 |cohort_class/Repeat_ID),
                             data = MoodAndDenseEmo, 
                             backend = "cmdstanr", cores = 5,
                             iter = 4000, control = list(adapt_delta = 0.95),
                             family = gaussian(),
                             file = "Brms_results/RawNA_PE_1EWMA.5.brm", file_refit = "on_change")
RawNA_PE_1EWMA.5 <- readRDS("Brms_results/RawNA_PE_1EWMA.5.brm.rds")
summary(RawNA_PE_1EWMA.5)
plot(RawNA_PE_1EWMA.5)
pp_check(RawNA_PE_1EWMA.5)

RawNA_PE_1EWMA.75 <- brm(NA_score ~ PE_1EWMA.75 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.75 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/RawNA_PE_1EWMA.75.brm", file_refit = "on_change")
RawNA_PE_1EWMA.75 <- readRDS("Brms_results/RawNA_PE_1EWMA.75.brm.rds")
summary(RawNA_PE_1EWMA.75)
plot(RawNA_PE_1EWMA.75)
pp_check(RawNA_PE_1EWMA.75)

RawNA_PE_1EWMA.95 <- brm(NA_score ~ PE_1EWMA.95 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.95 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/RawNA_PE_1EWMA.95.brm", file_refit = "on_change")
RawNA_PE_1EWMA.95 <- readRDS("Brms_results/RawNA_PE_1EWMA.95.brm.rds")
summary(RawNA_PE_1EWMA.95)
plot(RawNA_PE_1EWMA.95)
pp_check(RawNA_PE_1EWMA.95)

#### Loo: Raw NA PE ####
RawNA_PE_1 <- readRDS("Brms_results/RawNA_PE_1.brm.rds")
RawNA_PE_1EWMA.25 <- readRDS("Brms_results/RawNA_PE_1EWMA.25.brm.rds")
RawNA_PE_1EWMA.5 <- readRDS("Brms_results/RawNA_PE_1EWMA.5.brm.rds")
RawNA_PE_1EWMA.75 <- readRDS("Brms_results/RawNA_PE_1EWMA.75.brm.rds")
RawNA_PE_1EWMA.95 <- readRDS("Brms_results/RawNA_PE_1EWMA.95.brm.rds")

RawNA_PE_1_loo <- loo(RawNA_PE_1)
RawNA_PE_1EWMA.25_loo <- loo(RawNA_PE_1EWMA.25)
RawNA_PE_1EWMA.5_loo <- loo(RawNA_PE_1EWMA.5)
RawNA_PE_1EWMA.75_loo <- loo(RawNA_PE_1EWMA.75)
RawNA_PE_1EWMA.95_loo <- loo(RawNA_PE_1EWMA.95)
RawNA_PE_LOO <- loo_compare(RawNA_PE_1_loo, RawNA_PE_1EWMA.25_loo, RawNA_PE_1EWMA.5_loo,
                            RawNA_PE_1EWMA.75_loo, RawNA_PE_1EWMA.95_loo)
RawNA_PE_LOO


#### Raw PA PE ####
RawPA_PE_1 <- brm(PA_score ~ PE_1 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1 |cohort_class/Repeat_ID),
                       data = MoodAndDenseEmo, 
                       backend = "cmdstanr", cores = 5,
                       iter = 4000, control = list(adapt_delta = 0.95),
                       family = gaussian(),
                       file = "Brms_results/RawPA_PE_1.brm", file_refit = "on_change")
RawPA_PE_1 <- readRDS("Brms_results/RawPA_PE_1.brm.rds")
summary(RawPA_PE_1)
plot(RawPA_PE_1)
pp_check(RawPA_PE_1)
conditional_effects(RawPA_PE_1)

RawPA_PE_1EWMA.25 <- brm(PA_score ~ PE_1EWMA.25 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.25 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/RawPA_PE_1EWMA.25.brm", file_refit = "on_change")
RawPA_PE_1EWMA.25 <- readRDS("Brms_results/RawPA_PE_1EWMA.25.brm.rds")
summary(RawPA_PE_1EWMA.25)
plot(RawPA_PE_1EWMA.25)
pp_check(RawPA_PE_1EWMA.25)

RawPA_PE_1EWMA.5 <- brm(PA_score ~ PE_1EWMA.5 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.5 |cohort_class/Repeat_ID),
                             data = MoodAndDenseEmo, 
                             backend = "cmdstanr", cores = 5,
                             iter = 4000, control = list(adapt_delta = 0.95),
                             family = gaussian(),
                             file = "Brms_results/RawPA_PE_1EWMA.5.brm", file_refit = "on_change")
RawPA_PE_1EWMA.5 <- readRDS("Brms_results/RawPA_PE_1EWMA.5.brm.rds")
summary(RawPA_PE_1EWMA.5)
plot(RawPA_PE_1EWMA.5)
pp_check(RawPA_PE_1EWMA.5)

RawPA_PE_1EWMA.75 <- brm(PA_score ~ PE_1EWMA.75 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.75 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/RawPA_PE_1EWMA.75.brm", file_refit = "on_change")
RawPA_PE_1EWMA.75 <- readRDS("Brms_results/RawPA_PE_1EWMA.75.brm.rds")
summary(RawPA_PE_1EWMA.75)
plot(RawPA_PE_1EWMA.75)
pp_check(RawPA_PE_1EWMA.75)

RawPA_PE_1EWMA.95 <- brm(PA_score ~ PE_1EWMA.95 * DaysSinceDense + exam_num + N_Responses + (1 + PE_1EWMA.95 |cohort_class/Repeat_ID),
                              data = MoodAndDenseEmo, 
                              backend = "cmdstanr", cores = 5,
                              iter = 4000, control = list(adapt_delta = 0.95),
                              family = gaussian(),
                              file = "Brms_results/RawPA_PE_1EWMA.95.brm", file_refit = "on_change")
RawPA_PE_1EWMA.95 <- readRDS("Brms_results/RawPA_PE_1EWMA.95.brm.rds")
summary(RawPA_PE_1EWMA.95)
plot(RawPA_PE_1EWMA.95)
pp_check(RawPA_PE_1EWMA.95)

#### Loo: Raw PA PE ####
RawPA_PE_1 <- readRDS("Brms_results/RawPA_PE_1.brm.rds")
RawPA_PE_1EWMA.25 <- readRDS("Brms_results/RawPA_PE_1EWMA.25.brm.rds")
RawPA_PE_1EWMA.5 <- readRDS("Brms_results/RawPA_PE_1EWMA.5.brm.rds")
RawPA_PE_1EWMA.75 <- readRDS("Brms_results/RawPA_PE_1EWMA.75.brm.rds")
RawPA_PE_1EWMA.95 <- readRDS("Brms_results/RawPA_PE_1EWMA.95.brm.rds")

RawPA_PE_1_loo <- loo(RawPA_PE_1)
RawPA_PE_1EWMA.25_loo <- loo(RawPA_PE_1EWMA.25)
RawPA_PE_1EWMA.5_loo <- loo(RawPA_PE_1EWMA.5)
RawPA_PE_1EWMA.75_loo <- loo(RawPA_PE_1EWMA.75)
RawPA_PE_1EWMA.95_loo <- loo(RawPA_PE_1EWMA.95)
RawPA_PE_LOO <- loo_compare(RawPA_PE_1_loo, RawPA_PE_1EWMA.25_loo, RawPA_PE_1EWMA.5_loo,
                            RawPA_PE_1EWMA.75_loo, RawPA_PE_1EWMA.95_loo)
RawPA_PE_LOO

