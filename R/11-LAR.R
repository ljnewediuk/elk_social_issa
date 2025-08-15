

## Cleaned Locs - generate lagged association rates ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data + NN
# Outputs: LAR

### Packages ----
libs <- c('data.table', 'ggplot2', 'asnipe', 'spatsoc', 'plotrix', 'gridExtra')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
vita <- readRDS("output/ZOE/7-locs-mod.RDS")
vita$lagYear <- as.factor(vita$lagYear)

## remove 
vita <- vita[!is.na(Season)]


vita$IDmemb <- as.factor(paste(vita$animal_ID, vita$membership, sep = "_"))
b1 <- vita[, .N, by = c("animal_ID", "membership", "lagYear")]
b1[, c("N") := NULL]
write.csv(b1, "output/ZOE/community_membership_by_lag_year.csv")

######################## 
######## Year 1 ########
########################

fo19 <- get_gbi(vita[lagYear == "Year1"], group = 'group', id = 'animal_ID')
times19 <- data.table(time = vita[lagYear == "Year1"]$JDate,
                      group = vita[lagYear == "Year1"]$group)
days <- data.table(group = as.numeric(rownames(fo19)))
qq <- merge(times19, days, by = "group")
times19 <- unique(qq)
times19 <- times19$time

lra19 <- data.table(LRA(fo19, times = times19, timejump = 1, output_style = 2))
LRA19avg <- lra19[, meanLRA := mean(RATE), by = .(TIME)]
LRA19avg <- lra19[, seLRA := std.error(RATE), by = .(TIME)]
avg19 <- LRA19avg[, unique(meanLRA), by = c("TIME")]
se19 <- LRA19avg[, unique(seLRA), by = c("TIME")][,c("TIME") := NULL]
LRA19 <- cbind(avg19, se19)
colnames(LRA19) <- c("TIME", "meanLRA", "seLRA")
LRA19$lagYear <- "Year1"

######################## 
######## Year2 ######### 
########################

fo20 <- get_gbi(vita[lagYear == "Year2"], group = 'group', id = 'animal_ID')
times20 <- data.table(time = vita[lagYear == "Year2"]$JDate,
                      group = vita[lagYear == "Year2"]$group)
days <- data.table(group = as.numeric(rownames(fo20)))
qq <- merge(times20, days, by = "group")
times20 <- unique(qq)
times20 <- times20$time

lra20 <- data.table(LRA(fo20, times = times20, timejump = 1, output_style = 2))
LRA20avg <- lra20[, meanLRA := mean(RATE), by = .(TIME)]
LRA20avg <- lra20[, seLRA := std.error(RATE), by = .(TIME)]
avg20 <- LRA20avg[, unique(meanLRA), by = c("TIME")]
se20 <- LRA20avg[, unique(seLRA), by = c("TIME")][,c("TIME") := NULL]
LRA20 <- cbind(avg20, se20)
colnames(LRA20) <- c("TIME", "meanLRA", "seLRA")
LRA20$lagYear <- "Year2"


LAR <- rbind(LRA19, LRA20)




png("graphics/ZOE/Fig2_bylagYear.png", width = 3000, height = 3000,
    units = "px", res = 600)
ggplot(LAR, aes(TIME, meanLRA)) +
  geom_line(aes(color = as.factor(lagYear)), alpha = 0.5, size = 1) + 
  ylab("Lagged association rate") +
  xlab("Days") +
  ylim(0, 0.7) +
  geom_errorbar(aes(ymin = meanLRA - seLRA, 
                    ymax = meanLRA + seLRA, 
                color = as.factor(lagYear)),
                width=0.04, alpha = 0.5) + 
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  labs(color = "lagYear") +
  theme(legend.position = c(0.15,0.85),
        legend.title = element_text(size = 12),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) 
dev.off()
######################## 
######## Year1 ######### 
########################

fo19 <- get_gbi(vita[lagYear == "Year1"], group = 'group', id = 'IDmemb')
times19 <- data.table(time = vita[lagYear == "Year1"]$JDate,
                      group = vita[lagYear == "Year1"]$group)
days <- data.table(group = as.numeric(rownames(fo19)))
qq <- merge(times19, days, by = "group")
times19 <- unique(qq)
times19 <- times19$time

lra19 <- data.table(LRA(fo19, times = times19, timejump = 1, output_style = 2))

lra19$ID <- gsub("ER_E_", "VE", lra19$ID)
lra19$ASSOCIATE <- gsub("ER_E_", "VE", lra19$ASSOCIATE)
lra19[, c("ID", "membership1") := tstrsplit(ID, "_", fixed=TRUE)][, c("ASSOCIATE", "membership2") := tstrsplit(ASSOCIATE, "_", fixed=TRUE)]
lra19$memb <- paste(lra19$membership1, lra19$membership2, sep = "_")
LRA19avg <- lra19[, meanLRA := mean(RATE), by = .(TIME, memb)]
LRA19avg <- lra19[, seLRA := std.error(RATE), by = .(TIME, memb)]
avg19 <- LRA19avg[, unique(meanLRA), by = c("TIME", "memb")]
se19 <- LRA19avg[, unique(seLRA), by = c("TIME", "memb")][,c("TIME", "memb") := NULL]
LRA19tot <- cbind(avg19, se19)
colnames(LRA19tot) <- c("TIME", "memb", "meanLRA", "seLRA")

LRA19avg$memb[LRA19avg$memb == "2_1"] <- "Different community"
LRA19avg$memb[LRA19avg$memb == "1_2"] <- "Different community"
LRA19avg$memb[LRA19avg$memb == "1_1"] <- "Same community"
LRA19avg$memb[LRA19avg$memb == "2_2"] <- "Same community"


LRA19avg1 <- LRA19avg[, mean(meanLRA), by = c("TIME", "memb")]
LRA19se <- LRA19avg[, mean(seLRA), by = c("TIME", "memb")]
LRA19 <- cbind(LRA19avg1, LRA19se$V1)
colnames(LRA19) <- c("TIME", "memb", "avg", "se")

######################## 
######## Year2 ######### 
######################## 

fo20 <- get_gbi(vita[lagYear == "Year2"], group = 'group', id = 'IDmemb')
times20 <- data.table(time = vita[lagYear == "Year2"]$JDate,
                      group = vita[lagYear == "Year2"]$group)
days <- data.table(group = as.numeric(rownames(fo20)))
qq <- merge(times20, days, by = "group")
times20 <- unique(qq)
times20 <- times20$time

lra20 <- data.table(LRA(fo20, times = times20, timejump = 1, output_style = 2))

lra20$ID <- gsub("ER_E_", "VE", lra20$ID)
lra20$ASSOCIATE <- gsub("ER_E_", "VE", lra20$ASSOCIATE)
lra20[, c("ID", "membership1") := tstrsplit(ID, "_", fixed=TRUE)][, c("ASSOCIATE", "membership2") := tstrsplit(ASSOCIATE, "_", fixed=TRUE)]
lra20$memb <- paste(lra20$membership1, lra20$membership2, sep = "_")
LRA20avg <- lra20[, meanLRA := mean(RATE), by = .(TIME, memb)]
LRA20se <- lra20[, seLRA := std.error(RATE), by = .(TIME, memb)]
avg20 <- LRA20avg[, mean(meanLRA), by = c("TIME", "memb")]
se20 <- LRA20se[, mean(seLRA), by = c("TIME", "memb")][,c("TIME", "memb") := NULL]
LRA20tot <- cbind(avg20, se20)
colnames(LRA20tot) <- c("TIME", "memb", "meanLRA", "seLRA")

LRA20avg$memb[LRA20avg$memb == "2_1"] <- "Different community"
LRA20avg$memb[LRA20avg$memb == "3_1"] <- "Different community"
LRA20avg$memb[LRA20avg$memb == "3_2"] <- "Different community"
LRA20avg$memb[LRA20avg$memb == "1_2"] <- "Different community"
LRA20avg$memb[LRA20avg$memb == "1_3"] <- "Different community"
LRA20avg$memb[LRA20avg$memb == "2_3"] <- "Different community"
LRA20avg$memb[LRA20avg$memb == "1_1"] <- "Same community"
LRA20avg$memb[LRA20avg$memb == "2_2"] <- "Same community"
LRA20avg$memb[LRA20avg$memb == "3_3"] <- "Same community"


LRA20avg1 <- LRA20avg[, mean(meanLRA, na.rm = T), by = c("TIME", "memb")]
LRA20se <- LRA20avg[, mean(seLRA, na.rm = T), by = c("TIME", "memb")]
LRA20 <- cbind(LRA20avg1, LRA20se$V1)
colnames(LRA20) <- c("TIME", "memb", "avg", "se")


LRA19$lagYear <- "Year 1"
LRA20$lagYear <- "Year 2"
LAR_both <- rbind(LRA19, LRA20)

color = c("orange", "dodgerblue")

png("graphics/ZOE/FigS4.png", width = 6000, height = 3000,
    units = "px", res = 600)
ggplot(LAR_both, aes(TIME, avg, fill = factor(memb))) +
  geom_line(alpha = 0.5, aes(color = factor(memb)), size = 1) + 
  ylab("Lagged association rate") +
  xlab("Days") +
  ylim(0, 0.7) +
  geom_errorbar(aes(ymin = avg - se, 
                    ymax = avg + se, 
                    color = factor(memb)), 
                 width=0.04, alpha = 0.5) + 
  scale_fill_manual(values = color) +
  scale_color_manual(values = color) +
  labs(color = "Annual communities") +
  theme(legend.position = c(0.15,0.85),
        legend.title = element_text(size = 12),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = NA),
        strip.text = element_text(size = 14))+
  facet_wrap(~lagYear)
dev.off()
