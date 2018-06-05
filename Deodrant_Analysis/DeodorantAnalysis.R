# Clear Workspace
rm(list = ls())

# Loading Libraries
library(tidyr)
library("MASS")
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)

# Load Data Set
deodrant_b <- read.csv('Deodorant B.csv')
deodrant_f <- read.csv('Deodorant F.csv')
deodrant_g <- read.csv('Deodorant G.csv')
deodrant_h <- read.csv('Deodorant H.csv')
deodrant_j <- read.csv('Deodorant J.csv')


#------------------------------------------------------------------------------------#
#               CLEANING deodrant_b DATA                                             #
#------------------------------------------------------------------------------------#

# Adding q8.3, q8.4, q8.7, q8.9, q8.10, q8.14, q8.15, q8.16, q8.17, q8.18
deodrant_b$q8.3 <- 0
deodrant_b$q8.4 <- 0
deodrant_b$q8.7 <- 0
deodrant_b$q8.9 <- 0
deodrant_b$q8.10 <- 0
deodrant_b$q8.14 <- 0
deodrant_b$q8.15 <- 0
deodrant_b$q8.16 <- 0
deodrant_b$q8.17 <- 0
deodrant_b$q8.18 <- 0
# Move all q8 to one place (at the end)
deodrant_b <- deodrant_b%>%select(-q8.1:-q8.20,q8.1:q8.20)
deodrant_b <- deodrant_b%>%select(-s13.2,s13.2)
deodrant_b$s13.6 <- 0
deodrant_b$s13.7 <- 0
deodrant_b$s13.8 <- 0
deodrant_b$s13.10 <- 0


# q8 is in a wide format. Need to convert it into long format
# deodrant_b : q8.3 - q8.20
deodrant_b_q8_long <- gather(deodrant_b, key = q8, value = q8_val, q8.3:q8.20)
deodrant_b_q8_long <- deodrant_b_q8_long[-which(deodrant_b_q8_long$q8_val==0), ]
deodrant_b_q8_long$q8 <- lapply(deodrant_b_q8_long$q8, function(x) strsplit(x, "[.]")[[1]][2])
deodrant_b_q8_long$q8_val <- NULL

# deodrant_b : There are 50 posible values for s13 questions out of which s13.2 is already there :
deodrant_b_s13_long <- gather(deodrant_b_q8_long, key = s13, value = s13_val, s13.2:s13.10)
deodrant_b_s13_long$s13 <- lapply(deodrant_b_s13_long$s13, function(x) strsplit(x, "[.]")[[1]][2])


# deodrant_b : s13a  :
deodrant_final_b <- deodrant_b_s13_long
deodrant_final_b$s13a.f.most.often <- NA
deodrant_final_b$s13a.g.most.often <- NA
deodrant_final_b$s13a.h.most.often <- NA
deodrant_final_b$s13a.j.most.often <- NA

#------------------------------------------------------------------------------------#
#               CLEANING deodrant_f DATA                                             #
#------------------------------------------------------------------------------------#

# Adding q8.2, q8.3, q8.4, q8.8, q8.10, q8.12, q8.14, q8.15, q8.16, q8.17
deodrant_f$q8.2 <- 0
deodrant_f$q8.3 <- 0
deodrant_f$q8.4 <- 0
deodrant_f$q8.8 <- 0
deodrant_f$q8.10 <- 0
deodrant_f$q8.12 <- 0
deodrant_f$q8.14 <- 0
deodrant_f$q8.15 <- 0
deodrant_f$q8.16 <- 0
deodrant_f$q8.17 <- 0
# Move all q8 to one place (at the end)
deodrant_f <- deodrant_f%>%select(-q8.1:-q8.20,q8.1:q8.20)
deodrant_f <- deodrant_f%>%select(-s13.6,s13.6)
deodrant_f$s13.2 <- 0
deodrant_f$s13.7 <- 0
deodrant_f$s13.8 <- 0
deodrant_f$s13.10 <- 0


# q8 is in a wide format. Need to convert it into long format
# deodrant_f : q8.1 - q8.20
deodrant_f_q8_long <- gather(deodrant_f, key = q8, value = q8_val, q8.2:q8.20)
deodrant_f_q8_long <- deodrant_f_q8_long[-which(deodrant_f_q8_long$q8_val==0), ]
deodrant_f_q8_long$q8 <- lapply(deodrant_f_q8_long$q8, function(x) strsplit(x, "[.]")[[1]][2])
deodrant_f_q8_long$q8_val <- NULL

# deodrant_f : There are 50 posible values for s13 questions out of which s13.6 is already there :
deodrant_f_s13_long <- gather(deodrant_f_q8_long, key = s13, value = s13_val, s13.6:s13.10)
deodrant_f_s13_long$s13 <- lapply(deodrant_f_s13_long$s13, function(x) strsplit(x, "[.]")[[1]][2])


# deodrant_f : s13a  :
deodrant_final_f <- deodrant_f_s13_long
deodrant_final_f$s13a.b.most.often <- NA
deodrant_final_f$s13a.g.most.often <- NA
deodrant_final_f$s13a.h.most.often <- NA
deodrant_final_f$s13a.j.most.often <- NA


#------------------------------------------------------------------------------------#
#               CLEANING deodrant_g DATA                                             #
#------------------------------------------------------------------------------------#

# Adding q8.3, q8.4, q8.8, q8.9, q8.10, q8.14, q8.15, q8.16, q8.18, q8.20
deodrant_g$q8.3 <- 0
deodrant_g$q8.4 <- 0
deodrant_g$q8.8 <- 0
deodrant_g$q8.9 <- 0
deodrant_g$q8.10 <- 0
deodrant_g$q8.14 <- 0
deodrant_g$q8.15 <- 0
deodrant_g$q8.16 <- 0
deodrant_g$q8.18 <- 0
deodrant_g$q8.20 <- 0
# Move all q8 to one place (at the end)
deodrant_g <- deodrant_g%>%select(-q8.1:-q8.19,q8.1:q8.19)
deodrant_g <- deodrant_g%>%select(-s13.7,s13.7)
deodrant_g$s13.2 <- 0
deodrant_g$s13.6 <- 0
deodrant_g$s13.8 <- 0
deodrant_g$s13.10 <- 0

# q8 is in a wide format. Need to convert it into long format
# deodrant_g : q8.1 - q8.19
deodrant_g_q8_long <- gather(deodrant_g, key = q8, value = q8_val, q8.3:q8.19)
deodrant_g_q8_long <- deodrant_g_q8_long[-which(deodrant_g_q8_long$q8_val==0), ]
deodrant_g_q8_long$q8 <- lapply(deodrant_g_q8_long$q8, function(x) strsplit(x, "[.]")[[1]][2])
deodrant_g_q8_long$q8_val <- NULL

# deodrant_g : There are 50 posible values for s13 questions out of which s13.7 is already there :
deodrant_g_s13_long <- gather(deodrant_g_q8_long, key = s13, value = s13_val, s13.7:s13.10)
deodrant_g_s13_long$s13 <- lapply(deodrant_g_s13_long$s13, function(x) strsplit(x, "[.]")[[1]][2])


# deodrant_g : s13a  :
deodrant_final_g <- deodrant_g_s13_long
deodrant_final_g$s13a.b.most.often <- NA
deodrant_final_g$s13a.f.most.often <- NA
deodrant_final_g$s13a.h.most.often <- NA
deodrant_final_g$s13a.j.most.often <- NA



#------------------------------------------------------------------------------------#
#               CLEANING deodrant_h DATA                                             #
#------------------------------------------------------------------------------------#

# Adding q8.3, q8.4, q8.8, q8.9, q8.10, q8.14, q8.15, q8.16, q8.17, q8.18
deodrant_h$q8.3 <- 0
deodrant_h$q8.4 <- 0
deodrant_h$q8.8 <- 0
deodrant_h$q8.9 <- 0
deodrant_h$q8.10 <- 0
deodrant_h$q8.14 <- 0
deodrant_h$q8.15 <- 0
deodrant_h$q8.16 <- 0
deodrant_h$q8.17 <- 0
deodrant_h$q8.18 <- 0
# Move all q8 to one place (at the end)
deodrant_h <- deodrant_h%>%select(-q8.1:-q8.20,q8.1:q8.20)
deodrant_h <- deodrant_h%>%select(-s13.8,s13.8)
deodrant_h$s13.2 <- 0
deodrant_h$s13.6 <- 0
deodrant_h$s13.7 <- 0
deodrant_h$s13.10 <- 0

# q8 is in a wide format. Need to convert it into long format
# deodrant_h : q8.1 - q8.20
deodrant_h_q8_long <- gather(deodrant_h, key = q8, value = q8_val, q8.3:q8.20)
deodrant_h_q8_long <- deodrant_h_q8_long[-which(deodrant_h_q8_long$q8_val==0), ]
deodrant_h_q8_long$q8 <- lapply(deodrant_h_q8_long$q8, function(x) strsplit(x, "[.]")[[1]][2])
deodrant_h_q8_long$q8_val <- NULL

# deodrant_h : There are 50 posible values for s13 questions out of which s13.8 is already there :
deodrant_h_s13_long <- gather(deodrant_h_q8_long, key = s13, value = s13_val, s13.8:s13.10)
deodrant_h_s13_long$s13 <- lapply(deodrant_h_s13_long$s13, function(x) strsplit(x, "[.]")[[1]][2])


# deodrant_h : s13a  :
deodrant_final_h <- deodrant_h_s13_long
deodrant_final_h$s13a.b.most.often <- NA
deodrant_final_h$s13a.g.most.often <- NA
deodrant_final_h$s13a.f.most.often <- NA
deodrant_final_h$s13a.j.most.often <- NA


#------------------------------------------------------------------------------------#
#               CLEANING deodrant_j DATA                                             #
#------------------------------------------------------------------------------------#

# Adding q8.2, q8.3, q8.4, q8.8, q8.9, q8.14, q8.15, q8.16, q8.17, q8.20
deodrant_j$q8.2 <- 0
deodrant_j$q8.3 <- 0
deodrant_j$q8.4 <- 0
deodrant_j$q8.8 <- 0
deodrant_j$q8.9 <- 0
deodrant_j$q8.14 <- 0
deodrant_j$q8.15 <- 0
deodrant_j$q8.16 <- 0
deodrant_j$q8.17 <- 0
deodrant_j$q8.20 <- 0
# Move all q8 to one place (at the end)
deodrant_j <- deodrant_j%>%select(-q8.1:-q8.19,q8.1:q8.19)
deodrant_j <- deodrant_j%>%select(-s13.10,s13.10)
deodrant_j$s13.2 <- 0
deodrant_j$s13.6 <- 0
deodrant_j$s13.7 <- 0
deodrant_j$s13.8 <- 0

# q8 is in a wide format. Need to convert it into long format
# deodrant_j : q8.1 - q8.20
deodrant_j_q8_long <- gather(deodrant_j, key = q8, value = q8_val, q8.2:q8.19)
deodrant_j_q8_long <- deodrant_j_q8_long[-which(deodrant_j_q8_long$q8_val==0), ]
deodrant_j_q8_long$q8 <- lapply(deodrant_j_q8_long$q8, function(x) strsplit(x, "[.]")[[1]][2])
deodrant_j_q8_long$q8_val <- NULL

# deodrant_j : There are 50 posible values for s13 questions out of which s13.10 is already there :
deodrant_j_s13_long <- gather(deodrant_j_q8_long, key = s13, value = s13_val, s13.10:s13.8)
deodrant_j_s13_long$s13 <- lapply(deodrant_j_s13_long$s13, function(x) strsplit(x, "[.]")[[1]][2])


# deodrant_j : s13a  :
deodrant_final_j <- deodrant_j_s13_long
deodrant_final_j$s13a.b.most.often <- NA
deodrant_final_j$s13a.g.most.often <- NA
deodrant_final_j$s13a.f.most.often <- NA
deodrant_final_j$s13a.h.most.often <- NA


# Creating a master deodrant data set combining all 5 dataset
deodrant <- rbind(deodrant_final_b, deodrant_final_f)
deodrant <- rbind(deodrant, deodrant_final_g)
deodrant <- rbind(deodrant, deodrant_final_h)
deodrant <- rbind(deodrant, deodrant_final_j)


#------------------------------------------------------------------------------------#
#                              EDA PLOTS                                             #
#------------------------------------------------------------------------------------#

# q1_labels <- c("[1] DISLIKE EXTREMELY", "[2] DISLIKE A LOT", "[3] DISLIKE MODERATELY", "[4] NEITHER LIKE NOR DISLIKE",
#                "[5] LIKE MODERATELY", "[6] LIKE A LOT", "[7] LIKE EXTREMELY")
# deodrant_b$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_b$q1_1.personal.opinion.of.this.Deodorant)
# 
# db <- ggplot(data = deodrant_b, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant B') +
#   scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# deodrant_f$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_f$q1_1.personal.opinion.of.this.Deodorant)
# 
# df <- ggplot(data = deodrant_f, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant F') +
#   scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# deodrant_g$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_g$q1_1.personal.opinion.of.this.Deodorant)
# 
# dg <- ggplot(data = deodrant_g, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant G') +
#   scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# deodrant_h$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_h$q1_1.personal.opinion.of.this.Deodorant)
# 
# dh <- ggplot(data = deodrant_h, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant H') +
#   scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# deodrant_j$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_j$q1_1.personal.opinion.of.this.Deodorant)
# 
# dj <- ggplot(data = deodrant_j, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant J') +
#   scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# ggarrange(db, df, dg, dh, dj, nrow = 1, ncol = 5, hjust = 5)
# 
# q3_labels <- c("[1] MUCH TOO WEAK", "[2] SOMEWHAT TOO WEAK", "[3] JUST ABOUT RIGHT", "[4] SOMEWHAT TOO STRONG",
#                "[5] MUCH TOO STRONG")
# deodrant_b$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_b$q3_1.strength.of.the.Deodorant)
# deodrant_f$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_f$q3_1.strength.of.the.Deodorant)
# deodrant_g$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_g$q3_1.strength.of.the.Deodorant)
# deodrant_h$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_h$q3_1.strength.of.the.Deodorant)
# deodrant_j$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_j$q3_1.strength.of.the.Deodorant)
# 
# db_q3 <- ggplot(data = deodrant_b, aes(x = q3_1.strength.of.the.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant B') +
#   scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# df_q3 <- ggplot(data = deodrant_f, aes(x = q3_1.strength.of.the.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant F') +
#   scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# dg_q3 <- ggplot(data = deodrant_g, aes(x = q3_1.strength.of.the.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant G') +
#   scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# dh_q3 <- ggplot(data = deodrant_h, aes(x = q3_1.strength.of.the.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant H') +
#   scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# dj_q3 <- ggplot(data = deodrant_j, aes(x = q3_1.strength.of.the.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant J') +
#   scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# ggarrange(db_q3, df_q3, dg_q3, dh_q3, dj_q3, nrow = 1, ncol = 5, hjust = 5)
# 
# q9_labels <- c("[1] Definitely would not purchase", "[2] Probably would not purchase", "[3] Not sure I would purchase or not",
#                "[4] Probably would purchase", "[5] Definitely would purchase")
# deodrant_b$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_b$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
# deodrant_f$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_f$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
# deodrant_g$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_g$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
# deodrant_h$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_h$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
# deodrant_j$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_j$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
# 
# 
# db_q9 <- ggplot(data = deodrant_b, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant B') +
#   scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# df_q9 <- ggplot(data = deodrant_f, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant F') +
#   scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# dg_q9 <- ggplot(data = deodrant_g, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant G') +
#   scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# dh_q9 <- ggplot(data = deodrant_h, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant H') +
#   scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# dj_q9 <- ggplot(data = deodrant_j, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
#   geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
#   xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant J') +
#   scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# ggarrange(db_q9, df_q9, dg_q9, dh_q9, dj_q9, nrow = 1, ncol = 5, hjust = 5)

#------------------------------------------------------------------------------------#
#               CLEANING deodrant DATA                                               #
#------------------------------------------------------------------------------------#

deodrant$s13a.b.most.often[which(is.na(deodrant$s13a.b.most.often))] <- 0
deodrant$s13a.f.most.often[which(is.na(deodrant$s13a.f.most.often))] <- 0
deodrant$s13a.g.most.often[which(is.na(deodrant$s13a.g.most.often))] <- 0
deodrant$s13a.h.most.often[which(is.na(deodrant$s13a.h.most.often))] <- 0
deodrant$s13a.j.most.often[which(is.na(deodrant$s13a.j.most.often))] <- 0

# Converting q8 and s13 from list to numeric
deodrant$q8 <- as.numeric(deodrant$q8)
deodrant$s13 <- as.numeric(deodrant$s13)

# Converting variables to dummy 
# 1. q1_1.personal.opinion.of.this.Deodorant
deodrant$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant$q1_1.personal.opinion.of.this.Deodorant)
dummy_1 <- data.frame(model.matrix(~q1_1.personal.opinion.of.this.Deodorant, deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,-5], dummy_1)

# 2. q2_all.words
deodrant$q2_all.words <- as.factor(deodrant$q2_all.words)
dummy_1 <- data.frame(model.matrix(~q2_all.words, deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,-5], dummy_1)

# 3. q3_1.strength.of.the.Deodorant
deodrant$q3_1.strength.of.the.Deodorant <- as.factor(deodrant$q3_1.strength.of.the.Deodorant)
dummy_1 <- data.frame(model.matrix(~q3_1.strength.of.the.Deodorant, deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,-5], dummy_1)

# 4. q4_1.artificial.chemical
deodrant$q4_1.artificial.chemical <- as.factor(deodrant$q4_1.artificial.chemical)
dummy_1 <- data.frame(model.matrix(~q4_1.artificial.chemical, deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,-5], dummy_1)

# 5. q4_2.attractive
deodrant$q4_2.attractive <- as.factor(deodrant$q4_2.attractive)
dummy_1 <- data.frame(model.matrix(~q4_2.attractive, deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,-5], dummy_1)

# 6. q4_3.bold
deodrant$q4_3.bold <- as.factor(deodrant$q4_3.bold)
dummy_1 <- data.frame(model.matrix(~q4_3.bold, deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,-5], dummy_1)

# 7. q4_4.boring + q4_5.casual
deodrant$q4_4.boring <- as.factor(deodrant$q4_4.boring)
deodrant$q4_5.casual <- as.factor(deodrant$q4_5.casual)
dummy_1 <- data.frame(model.matrix(~q4_4.boring + q4_5.casual, deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,c(-5,-6)], dummy_1)

# 8. q4_6.cheap + q4_7.clean + q4_8.easy.to.wear + q4_9.elegant + q4_10.feminine + 
# q4_11.for.someone.like.me + q4_12.heavy + q4_13.high.quality + q4_14.long.lasting + 
# q4_15.masculine + q4_16.memorable + q4_17.natural + q4_18.old.fashioned + q4_19.ordinary +
# q4_20.overpowering + q4_21.sharp + q4_22.sophisticated + q4_23.upscale + q4_24.well.rounded 
deodrant$q4_6.cheap <- as.factor(deodrant$q4_6.cheap)
deodrant$q4_7.clean <- as.factor(deodrant$q4_7.clean)
deodrant$q4_8.easy.to.wear <- as.factor(deodrant$q4_8.easy.to.wear)
deodrant$q4_9.elegant <- as.factor(deodrant$q4_9.elegant)
deodrant$q4_10.feminine <- as.factor(deodrant$q4_10.feminine)
deodrant$q4_11.for.someone.like.me <- as.factor(deodrant$q4_11.for.someone.like.me)
deodrant$q4_12.heavy <- as.factor(deodrant$q4_12.heavy)
deodrant$q4_13.high.quality <- as.factor(deodrant$q4_13.high.quality)
deodrant$q4_14.long.lasting <- as.factor(deodrant$q4_14.long.lasting)
deodrant$q4_15.masculine <- as.factor(deodrant$q4_15.masculine)
deodrant$q4_16.memorable <- as.factor(deodrant$q4_16.memorable)
deodrant$q4_17.natural <- as.factor(deodrant$q4_17.natural)
deodrant$q4_18.old.fashioned <- as.factor(deodrant$q4_18.old.fashioned)
deodrant$q4_19.ordinary <- as.factor(deodrant$q4_19.ordinary)
deodrant$q4_20.overpowering <- as.factor(deodrant$q4_20.overpowering)
deodrant$q4_21.sharp <- as.factor(deodrant$q4_21.sharp)
deodrant$q4_22.sophisticated <- as.factor(deodrant$q4_22.sophisticated)
deodrant$q4_23.upscale <- as.factor(deodrant$q4_23.upscale)
deodrant$q4_24.well.rounded <- as.factor(deodrant$q4_24.well.rounded)

dummy_1 <- data.frame(model.matrix(~q4_6.cheap + q4_7.clean + q4_8.easy.to.wear + q4_9.elegant + q4_10.feminine + 
                                          q4_11.for.someone.like.me + q4_12.heavy + q4_13.high.quality + q4_14.long.lasting + 
                                          q4_15.masculine + q4_16.memorable + q4_17.natural + q4_18.old.fashioned + q4_19.ordinary +
                                          q4_20.overpowering + q4_21.sharp + q4_22.sophisticated + q4_23.upscale + q4_24.well.rounded
                                         , deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,c(-5:-23)], dummy_1)


# 8. q5_1.Deodorant.is.addictive + q7 + q9.how.likely.would.you.be.to.purchase.this.Deodorant + q10.prefer.this.Deodorant.or.your.usual.Deodorant + q11.time.of.day.would.this.Deodorant.be.appropriate + 
# q12.which.occasions.would.this.Deodorant.be.appropriate + Q13_Liking.after.30.minutes + q14.Deodorant.overall.on.a.scale.from.1.to.10 + ValSegb + 
# s7.involved.in.the.selection.of.the.cosmetic.products + s8.ethnic.background + s9.education + s10.income + s11.marital.status +
# s12.working.status + s13a.b.most.often + s13b.bottles.of.Deodorant.do.you.currently.own + q8 + s13 
deodrant$q5_1.Deodorant.is.addictive <- as.factor(deodrant$q5_1.Deodorant.is.addictive)
deodrant$q7 <- as.factor(deodrant$q7)
deodrant$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
deodrant$q10.prefer.this.Deodorant.or.your.usual.Deodorant <- as.factor(deodrant$q10.prefer.this.Deodorant.or.your.usual.Deodorant)
deodrant$q11.time.of.day.would.this.Deodorant.be.appropriate <- as.factor(deodrant$q11.time.of.day.would.this.Deodorant.be.appropriate)
deodrant$q12.which.occasions.would.this.Deodorant.be.appropriate <- as.factor(deodrant$q12.which.occasions.would.this.Deodorant.be.appropriate)
deodrant$Q13_Liking.after.30.minutes <- as.factor(deodrant$Q13_Liking.after.30.minutes)
deodrant$q14.Deodorant.overall.on.a.scale.from.1.to.10 <- as.factor(deodrant$q14.Deodorant.overall.on.a.scale.from.1.to.10)
deodrant$ValSegb <- as.factor(deodrant$ValSegb)
deodrant$s8.ethnic.background <- as.factor(deodrant$s8.ethnic.background)
deodrant$s9.education <- as.factor(deodrant$s9.education)
deodrant$s10.income <- as.factor(deodrant$s10.income)
deodrant$s11.marital.status <- as.factor(deodrant$s11.marital.status)
deodrant$s12.working.status <- as.factor(deodrant$s12.working.status)
deodrant$s13b.bottles.of.Deodorant.do.you.currently.own <- as.factor(deodrant$s13b.bottles.of.Deodorant.do.you.currently.own)
deodrant$q8 <- as.factor(deodrant$q8)
deodrant$s13 <- as.factor(deodrant$s13)

dummy_1 <- data.frame(model.matrix(~q5_1.Deodorant.is.addictive + q7 + q9.how.likely.would.you.be.to.purchase.this.Deodorant + 
                                          q10.prefer.this.Deodorant.or.your.usual.Deodorant + q11.time.of.day.would.this.Deodorant.be.appropriate + 
                                          q12.which.occasions.would.this.Deodorant.be.appropriate + Q13_Liking.after.30.minutes + 
                                          q14.Deodorant.overall.on.a.scale.from.1.to.10 + ValSegb  
                                          , deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,c(-5:-13)], dummy_1)

dummy_1 <- data.frame(model.matrix(~s8.ethnic.background + 
                                          s9.education + s10.income + s11.marital.status +
                                          s12.working.status 
                                        , deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,c(-6:-10)], dummy_1)

dummy_1 <- data.frame(model.matrix(~s13b.bottles.of.Deodorant.do.you.currently.own + 
                                          q8 +
                                          s13 
                                        , deodrant))
dummy_1 <- dummy_1[-1]
deodrant <- cbind(deodrant[,c(-7:-9)], dummy_1)




# Checking for NA values in deodrant data set 
sum(is.na(deodrant))      # No NAs found in the data set

# Divide the data set into train and test data set
trainindices_deodrant <- sample(1:nrow(deodrant), 0.7*nrow(deodrant))
train_deodrant <- deodrant[trainindices_deodrant,]
test_deodrant <- deodrant[-trainindices_deodrant,]


# Building model using decision tree
rfit <- rpart(Instant.Liking ~ ., train_deodrant)
summary(rfit)
rpart.plot(rfit)

plot(rfit)
text(rfit)

test_predict <- predict(rfit, test_deodrant)
test_deodrant <- cbind(test_deodrant,as.data.frame(test_predict))
test_result <- test_deodrant[,c("Instant.Liking","test_predict")]
cor(test_result$Instant.Liking, test_result$test_predict)

cm <- table(test_result$Instant.Liking, test_result$test_predict)
cm

# Removing unwanted data frames
rm(deodrant_b_q8_long, deodrant_b_s13_long, deodrant_f_q8_long, deodrant_f_s13_long, deodrant_g_q8_long, 
   deodrant_g_s13_long, deodrant_h_q8_long, deodrant_h_s13_long, deodrant_j_q8_long, deodrant_j_s13_long, 
   deodrant_final_b, deodrant_final_f, deodrant_final_g, deodrant_final_h, deodrant_final_j)

#------------------------------------------------------------------------------------#
#               Loading External Deodrant Test DATA                                  #
#------------------------------------------------------------------------------------#

deodrant_test_data <- read.csv('test_data.csv')

#------------------------------------------------------------------------------------#
#               CLEANING External Deodrant Test DATA                                 #
#------------------------------------------------------------------------------------#

# Adding q8.3, q8.4, q8.7, q8.9, q8.10, q8.14, q8.15, q8.16, q8.17, q8.18
deodrant_test_data$q8.3 <- 0
deodrant_test_data$q8.4 <- 0
deodrant_test_data$q8.7 <- 0
deodrant_test_data$q8.9 <- 0
deodrant_test_data$q8.10 <- 0
deodrant_test_data$q8.14 <- 0
deodrant_test_data$q8.15 <- 0
deodrant_test_data$q8.16 <- 0
deodrant_test_data$q8.17 <- 0
deodrant_test_data$q8.18 <- 0
# Move all q8 to one place (at the end)
deodrant_test_data <- deodrant_test_data%>%select(-q8.1:-q8.20,q8.1:q8.20)
deodrant_test_data <- deodrant_test_data%>%select(-s13.2,s13.2)
deodrant_test_data$s13.10 <- 0
deodrant_test_data$s13.6 <- 0
deodrant_test_data$s13.7 <- 0
deodrant_test_data$s13.8 <- 0

# q8 is in a wide format. Need to convert it into long format
# deodrant_test : q8.1 - q8.20
deodrant_test_q8_long <- gather(deodrant_test_data, key = q8, value = q8_val, q8.3:q8.20)
#deodrant_test_q8_long <- deodrant_test_q8_long[-which(deodrant_test_q8_long$q8_val==0), ]
deodrant_test_q8_long$q8 <- lapply(deodrant_test_q8_long$q8, function(x) strsplit(x, "[.]")[[1]][2])
deodrant_test_q8_long$q8_val <- NULL

# deodrant_test : There are 50 posible values for s13 questions out of which s13.2 is already there :
deodrant_test_s13_long <- gather(deodrant_test_q8_long, key = s13, value = s13_val, s13.2:s13.8)
deodrant_test_s13_long$s13 <- lapply(deodrant_test_s13_long$s13, function(x) strsplit(x, "[.]")[[1]][2])


# deodrant_test : s13a  :
deodrant_final_test <- deodrant_test_s13_long
deodrant_final_test$s13a.j.most.often <- 0
deodrant_final_test$s13a.g.most.often <- 0
deodrant_final_test$s13a.f.most.often <- 0
deodrant_final_test$s13a.h.most.often <- 0


# Converting q8 and s13 from list to numeric
deodrant_final_test$q8 <- as.numeric(deodrant_final_test$q8)
deodrant_final_test$s13 <- as.numeric(deodrant_final_test$s13)

# Converting variables to dummy 
# 1. q1_1.personal.opinion.of.this.Deodorant
deodrant_final_test$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_final_test$q1_1.personal.opinion.of.this.Deodorant)
dummy_1 <- data.frame(model.matrix(~q1_1.personal.opinion.of.this.Deodorant, deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,-4], dummy_1)

# 2. q2_all.words
deodrant_final_test$q2_all.words <- as.factor(deodrant_final_test$q2_all.words)
dummy_1 <- data.frame(model.matrix(~q2_all.words, deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,-4], dummy_1)

# 3. q3_1.strength.of.the.Deodorant
deodrant_final_test$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_final_test$q3_1.strength.of.the.Deodorant)
dummy_1 <- data.frame(model.matrix(~q3_1.strength.of.the.Deodorant, deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,-4], dummy_1)

# 4. q4_1.artificial.chemical
deodrant_final_test$q4_1.artificial.chemical <- as.factor(deodrant_final_test$q4_1.artificial.chemical)
dummy_1 <- data.frame(model.matrix(~q4_1.artificial.chemical, deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,-4], dummy_1)

# 5. q4_2.attractive
deodrant_final_test$q4_2.attractive <- as.factor(deodrant_final_test$q4_2.attractive)
dummy_1 <- data.frame(model.matrix(~q4_2.attractive, deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,-4], dummy_1)

# 6. q4_3.bold
deodrant_final_test$q4_3.bold <- as.factor(deodrant_final_test$q4_3.bold)
dummy_1 <- data.frame(model.matrix(~q4_3.bold, deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,-4], dummy_1)

# 7. q4_4.boring + q4_5.casual
deodrant_final_test$q4_4.boring <- as.factor(deodrant_final_test$q4_4.boring)
deodrant_final_test$q4_5.casual <- as.factor(deodrant_final_test$q4_5.casual)
dummy_1 <- data.frame(model.matrix(~q4_4.boring + q4_5.casual, deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,c(-4,-5)], dummy_1)

# 8. q4_6.cheap + q4_7.clean + q4_8.easy.to.wear + q4_9.elegant + q4_10.feminine + 
# q4_11.for.someone.like.me + q4_12.heavy + q4_13.high.quality + q4_14.long.lasting + 
# q4_15.masculine + q4_16.memorable + q4_17.natural + q4_18.old.fashioned + q4_19.ordinary +
# q4_20.overpowering + q4_21.sharp + q4_22.sophisticated + q4_23.upscale + q4_24.well.rounded 
deodrant_final_test$q4_6.cheap <- as.factor(deodrant_final_test$q4_6.cheap)
deodrant_final_test$q4_7.clean <- as.factor(deodrant_final_test$q4_7.clean)
deodrant_final_test$q4_8.easy.to.wear <- as.factor(deodrant_final_test$q4_8.easy.to.wear)
deodrant_final_test$q4_9.elegant <- as.factor(deodrant_final_test$q4_9.elegant)
deodrant_final_test$q4_10.feminine <- as.factor(deodrant_final_test$q4_10.feminine)
deodrant_final_test$q4_11.for.someone.like.me <- as.factor(deodrant_final_test$q4_11.for.someone.like.me)
deodrant_final_test$q4_12.heavy <- as.factor(deodrant_final_test$q4_12.heavy)
deodrant_final_test$q4_13.high.quality <- as.factor(deodrant_final_test$q4_13.high.quality)
deodrant_final_test$q4_14.long.lasting <- as.factor(deodrant_final_test$q4_14.long.lasting)
deodrant_final_test$q4_15.masculine <- as.factor(deodrant_final_test$q4_15.masculine)
deodrant_final_test$q4_16.memorable <- as.factor(deodrant_final_test$q4_16.memorable)
deodrant_final_test$q4_17.natural <- as.factor(deodrant_final_test$q4_17.natural)
deodrant_final_test$q4_18.old.fashioned <- as.factor(deodrant_final_test$q4_18.old.fashioned)
deodrant_final_test$q4_19.ordinary <- as.factor(deodrant_final_test$q4_19.ordinary)
deodrant_final_test$q4_20.overpowering <- as.factor(deodrant_final_test$q4_20.overpowering)
deodrant_final_test$q4_21.sharp <- as.factor(deodrant_final_test$q4_21.sharp)
deodrant_final_test$q4_22.sophisticated <- as.factor(deodrant_final_test$q4_22.sophisticated)
deodrant_final_test$q4_23.upscale <- as.factor(deodrant_final_test$q4_23.upscale)
deodrant_final_test$q4_24.well.rounded <- as.factor(deodrant_final_test$q4_24.well.rounded)

dummy_1 <- data.frame(model.matrix(~q4_6.cheap + q4_7.clean + q4_8.easy.to.wear + q4_9.elegant + q4_10.feminine + 
                                     q4_11.for.someone.like.me + q4_12.heavy + q4_13.high.quality + q4_14.long.lasting + 
                                     q4_15.masculine + q4_16.memorable + q4_17.natural + q4_18.old.fashioned + q4_19.ordinary +
                                     q4_20.overpowering + q4_21.sharp + q4_22.sophisticated + q4_23.upscale + q4_24.well.rounded
                                   , deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,c(-4:-22)], dummy_1)


# 8. q5_1.Deodorant.is.addictive + q7 + q9.how.likely.would.you.be.to.purchase.this.Deodorant + q10.prefer.this.Deodorant.or.your.usual.Deodorant + q11.time.of.day.would.this.Deodorant.be.appropriate + 
# q12.which.occasions.would.this.Deodorant.be.appropriate + Q13_Liking.after.30.minutes + q14.Deodorant.overall.on.a.scale.from.1.to.10 + ValSegb + 
# s7.involved.in.the.selection.of.the.cosmetic.products + s8.ethnic.background + s9.education + s10.income + s11.marital.status +
# s12.working.status + s13a.b.most.often + s13b.bottles.of.Deodorant.do.you.currently.own + q8 + s13 
deodrant_final_test$q5_1.Deodorant.is.addictive <- as.factor(deodrant_final_test$q5_1.Deodorant.is.addictive)
deodrant_final_test$q7 <- as.factor(deodrant_final_test$q7)
deodrant_final_test$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_final_test$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
deodrant_final_test$q10.prefer.this.Deodorant.or.your.usual.Deodorant <- as.factor(deodrant_final_test$q10.prefer.this.Deodorant.or.your.usual.Deodorant)
deodrant_final_test$q11.time.of.day.would.this.Deodorant.be.appropriate <- as.factor(deodrant_final_test$q11.time.of.day.would.this.Deodorant.be.appropriate)
deodrant_final_test$q12.which.occasions.would.this.Deodorant.be.appropriate <- as.factor(deodrant_final_test$q12.which.occasions.would.this.Deodorant.be.appropriate)
deodrant_final_test$Q13_Liking.after.30.minutes <- as.factor(deodrant_final_test$Q13_Liking.after.30.minutes)
deodrant_final_test$q14.Deodorant.overall.on.a.scale.from.1.to.10 <- as.factor(deodrant_final_test$q14.Deodorant.overall.on.a.scale.from.1.to.10)
deodrant_final_test$ValSegb <- as.factor(deodrant_final_test$ValSegb)
deodrant_final_test$s8.ethnic.background <- as.factor(deodrant_final_test$s8.ethnic.background)
deodrant_final_test$s9.education <- as.factor(deodrant_final_test$s9.education)
deodrant_final_test$s10.income <- as.factor(deodrant_final_test$s10.income)
deodrant_final_test$s11.marital.status <- as.factor(deodrant_final_test$s11.marital.status)
deodrant_final_test$s12.working.status <- as.factor(deodrant_final_test$s12.working.status)
deodrant_final_test$s13b.bottles.of.Deodorant.do.you.currently.own <- as.factor(deodrant_final_test$s13b.bottles.of.Deodorant.do.you.currently.own)
deodrant_final_test$q8 <- as.factor(deodrant_final_test$q8)
deodrant_final_test$s13 <- as.factor(deodrant_final_test$s13)

dummy_1 <- data.frame(model.matrix(~q5_1.Deodorant.is.addictive + q7 + q9.how.likely.would.you.be.to.purchase.this.Deodorant + 
                                     q10.prefer.this.Deodorant.or.your.usual.Deodorant + q11.time.of.day.would.this.Deodorant.be.appropriate + 
                                     q12.which.occasions.would.this.Deodorant.be.appropriate + Q13_Liking.after.30.minutes + 
                                     q14.Deodorant.overall.on.a.scale.from.1.to.10 + ValSegb  
                                   , deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,c(-4:-12)], dummy_1)

dummy_1 <- data.frame(model.matrix(~s8.ethnic.background + 
                                     s9.education + s10.income + s11.marital.status +
                                     s12.working.status 
                                   , deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,c(-5:-9)], dummy_1)

dummy_1 <- data.frame(model.matrix(~s13b.bottles.of.Deodorant.do.you.currently.own + 
                                     q8 + s13
                                   , deodrant_final_test))
dummy_1 <- dummy_1[-1]
deodrant_final_test <- cbind(deodrant_final_test[,c(-6:-8)], dummy_1)

# Predicting for the external deodrant_final_test
Instant.Liking <- predict(rfit, deodrant_final_test)
deodrant_final_test <- cbind(deodrant_final_test,as.data.frame(Instant.Liking))
predicted_deodant_data <- deodrant_final_test[, c("Respondent.ID", "Product","Instant.Liking")]
unique_predicted_deodant_data <- unique(predicted_deodant_data)

write.csv(unique_predicted_deodant_data, 'predicted_deodrant_data.csv', row.names = FALSE)
