library(ggplot2)
library(reshape2)
library(ggpubr)

q1_labels <- c("[1] DISLIKE EXTREMELY", "[2] DISLIKE A LOT", "[3] DISLIKE MODERATELY", "[4] NEITHER LIKE NOR DISLIKE",
               "[5] LIKE MODERATELY", "[6] LIKE A LOT", "[7] LIKE EXTREMELY")
deodrant_b$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_b$q1_1.personal.opinion.of.this.Deodorant)

db <- ggplot(data = deodrant_b, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant B') +
  scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

deodrant_f$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_f$q1_1.personal.opinion.of.this.Deodorant)

df <- ggplot(data = deodrant_f, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant F') +
  scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


deodrant_g$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_g$q1_1.personal.opinion.of.this.Deodorant)

dg <- ggplot(data = deodrant_g, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant G') +
  scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


deodrant_h$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_h$q1_1.personal.opinion.of.this.Deodorant)

dh <- ggplot(data = deodrant_h, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant H') +
  scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


deodrant_j$q1_1.personal.opinion.of.this.Deodorant <- as.factor(deodrant_j$q1_1.personal.opinion.of.this.Deodorant)

dj <- ggplot(data = deodrant_j, aes(x = q1_1.personal.opinion.of.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Personal Opinion of Deodorant') + ylab('count of opinions') + ggtitle('Deodorant J') +
  scale_x_discrete(labels = q1_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggarrange(db, df, dg, dh, dj, nrow = 1, ncol = 5, hjust = 5)


q3_labels <- c("[1] MUCH TOO WEAK", "[2] SOMEWHAT TOO WEAK", "[3] JUST ABOUT RIGHT", "[4] SOMEWHAT TOO STRONG",
               "[5] MUCH TOO STRONG")
deodrant_b$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_b$q3_1.strength.of.the.Deodorant)
deodrant_f$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_f$q3_1.strength.of.the.Deodorant)
deodrant_g$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_g$q3_1.strength.of.the.Deodorant)
deodrant_h$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_h$q3_1.strength.of.the.Deodorant)
deodrant_j$q3_1.strength.of.the.Deodorant <- as.factor(deodrant_j$q3_1.strength.of.the.Deodorant)

db_q3 <- ggplot(data = deodrant_b, aes(x = q3_1.strength.of.the.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant B') +
  scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

df_q3 <- ggplot(data = deodrant_f, aes(x = q3_1.strength.of.the.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant F') +
  scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dg_q3 <- ggplot(data = deodrant_g, aes(x = q3_1.strength.of.the.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant G') +
  scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dh_q3 <- ggplot(data = deodrant_h, aes(x = q3_1.strength.of.the.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant H') +
  scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dj_q3 <- ggplot(data = deodrant_j, aes(x = q3_1.strength.of.the.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Strength of the Deodrant') + ylab('count of opinions') + ggtitle('Deodorant J') +
  scale_x_discrete(labels = q3_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggarrange(db_q3, df_q3, dg_q3, dh_q3, dj_q3, nrow = 1, ncol = 5, hjust = 5)


q9_labels <- c("[1] Definitely would not purchase", "[2] Probably would not purchase", "[3] Not sure I would purchase or not",
               "[4] Probably would purchase", "[5] Definitely would purchase")
deodrant_b$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_b$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
deodrant_f$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_f$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
deodrant_g$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_g$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
deodrant_h$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_h$q9.how.likely.would.you.be.to.purchase.this.Deodorant)
deodrant_j$q9.how.likely.would.you.be.to.purchase.this.Deodorant <- as.factor(deodrant_j$q9.how.likely.would.you.be.to.purchase.this.Deodorant)


db_q9 <- ggplot(data = deodrant_b, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant B') +
  scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

df_q9 <- ggplot(data = deodrant_f, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant F') +
  scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dg_q9 <- ggplot(data = deodrant_g, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant G') +
  scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dh_q9 <- ggplot(data = deodrant_h, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant H') +
  scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dj_q9 <- ggplot(data = deodrant_j, aes(x = q9.how.likely.would.you.be.to.purchase.this.Deodorant)) +
  geom_bar(stat = 'count', colour = "green", aes(fill="#0072B2")) + guides(fill = FALSE) + 
  xlab('Likelihood of purchase') + ylab('count of opinions') + ggtitle('Deodorant J') +
  scale_x_discrete(labels = q9_labels) + geom_text(stat = 'count', aes(label = ..count..), vjust=-1, colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggarrange(db_q9, df_q9, dg_q9, dh_q9, dj_q9, nrow = 1, ncol = 5, hjust = 5)