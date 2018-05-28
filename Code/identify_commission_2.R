id_com <- select(data1, VoteRegistrationNumber, Kommission) %>% distinct() %>% 
  mutate(Kommission = as.character(Kommission), 
         nKommission = gsub("*-NR", "", Kommission),
         nKommission = gsub("*-SR", "", nKommission))

l <- lapply(id_com$nKommission, function(x) unlist(strsplit(x, ",")))
l <- lapply(l, function(x) trimws(x))
l <- lapply(l, function(x) unique(x))



match <- matrix(nrow = nrow(id_com), ncol = 1 + length(unique(unlist(l))))

for (i in 1:nrow(id_com)){
  match[i, 1] <- id_com$VoteRegistrationNumber[i]
  for (j in 1:length(unique(unlist(l)))){
    match[i, 1 + j] <- ifelse(unique(unlist(l))[j] %in% l[[i]], 1, 0)
  }
}

match_df <- data.frame(match)

names(match_df) <- c("VoteRegistrationNumber", unique(unlist(l)))

match_df$unique <- apply(match_df[,2:ncol(match_df)], 1, sum)
match_df$unique <- ifelse(match_df$unique == 1, 1, 0)

# df <- filter(match_df, unique == 1) %>% select(-unique, -sumL) %>% 
#   gather(key, value, -VoteRegistrationNumber) %>% 
#   filter(value == 1) %>% group_by(key) %>% 
#   summarize(count = length(unique(VoteRegistrationNumber)))

# # head(match_df)
# df <- filter(match_df, unique == 1)
# for (i in names(match_df)[-c(1, 15:16)]){
#   temp <- df[df[,i] == 1, ]
#   print(paste(i, nrow(temp)))
# }




# merge match_dfL to vote data
dim(data1)
data2 <- merge(data1, match_df,by = "VoteRegistrationNumber", all.y=FALSE, all.x=FALSE)
dim(data2)
data2 <- subset(data2, unique == 1)
# really only kept the right observations?
table(data2$sumL, useNA = "ifany")

# naming variables
data3 <- data2[,c("VoteRegistrationNumber","CouncillorBioId","CouncillorName", "Geburtsdatum", "Kanton", "Fraktion", "Vote",
                  topics)]


com_id <- lapply(l, function(x) paste0(x, collapse = ","))
id_com <- cbind(id_com, unlist(com_id))
names(id_com)[4] <- "Kom_unique"

df <- data3 %>%merge(id_com) %>% 
  mutate(Vote = ifelse(Vote == "Ja", 1,
                       ifelse(Vote == "Nein", 0, NA))) %>% 
  group_by(Kom_unique, VoteRegistrationNumber, Fraktion) %>% 
  summarize(aye = sum(Vote == 1, na.rm = T), 
            nay = sum(Vote == 0, na.rm = T), 
            total = sum(aye + nay),
            share = aye / total) %>% filter(Kom_unique %in% topics)

unity_by_com_fraction <- df %>% group_by(Kom_unique, Fraktion) %>% summarize(aye = sum(aye), 
                                                                             nay = sum(nay), 
                                                                             total = aye + nay,
                                                                             share = aye / total)
unity_by_fraction <- df %>% group_by(Fraktion) %>% summarize(aye = sum(aye), 
                                                             nay = sum(nay), 
                                                             total = aye + nay,
                                                             share = aye / total)
save(unity_by_com_fraction, unity_by_fraction, file = paste0("Data/unity.", session, ".Rda"))
