library(ggplot2)
library(ggrepel)
library(dplyr)

df <- read.csv("Data/write.out.1.csv")
df <- group_by(df, Kommission) %>% 
  mutate(label = ifelse(Mittelwert == max(Mittelwert) | Mittelwert == min(Mittelwert),
                        as.character(Name), ""))

ggplot(df, aes(x = Mittelwert, y = Mittelwert, ymax = CI..unten., ymin = CI..oben., colour = Fraktion, label = label)) + 
  geom_point() + facet_wrap(~Kommission) + geom_pointrange() +
  theme_bw() + geom_text_repel()

df1 <- read.csv("Data/write.out.votes.csv")
df1 <- group_by(df1, Parameter, Kommission) %>% 
  mutate(label = ifelse(Mittelwert == max(Mittelwert) | Mittelwert == min(Mittelwert),
                        as.character(Vote.ID), ""))

  
ggplot(df1[df1$Parameter == "difficulty", ], aes(x = Mittelwert, y = Mittelwert, ymax = CI..unten., ymin = CI..oben., label = label)) + geom_point() + 
  facet_wrap(~Kommission) + 
  geom_pointrange() + geom_text_repel() +
  theme_classic() + coord_flip()
ggsave("Plots/difficulty.pdf", width = 10, height = 10)
ggplot(df1[df1$Parameter == "discrimination", ], aes(x = Mittelwert, y = Mittelwert, ymax = CI..unten., ymin = CI..oben., label = label)) + geom_point() + 
  facet_wrap(~Kommission) + geom_text_repel() +
  geom_pointrange() + 
  theme_classic() + coord_flip()
ggsave("Plots/discrimination.pdf", width = 10, height = 10)

apk_votes <- filter(df1, Kommission == "APK" & Vote.ID %in% c(13440, 13487, 12939))
theta <- seq(-3, 3, 0.001)
for (i in unique(apk_votes$Vote.ID)){
  p <- pnorm(theta * apk_votes$Mittelwert[apk_votes$Vote.ID == i & apk_votes$Parameter == "discrimination"] -
               apk_votes$Mittelwert[apk_votes$Vote.ID == i & apk_votes$Parameter == "difficulty"])
  ggplot() + geom_line(aes(x = theta, y = p)) + theme_classic() + xlab("Idealpunkt") + ylab("Pr(y = 1)") + ylim(0,1) +
    ggsave(paste0("Plots/irf_", i, ".pdf"), width = 5, height = 5)
}
