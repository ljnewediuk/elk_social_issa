## Genetics exploration##
libs <- c('tidyverse', 'dplyr', 'PopGenReport', 'adegenet', 'genetics','poppr')
lapply(libs, require, character.only = TRUE)

#Read in the Coancestry output & add headings
CoAnc <- read.table("data/Vita-elk-1/RelatednessEstimates.Txt", sep = ",")
colnames(CoAnc) <- c("dyad", "indv1", "ind2", "pop1pop2", "trioML", "wang","lynchli", "lynchRd", "Ritland", "QGT", "DyadMl")

#Remove the last empty column and the population column(only 1 population)
CoAnc <- dplyr::select(CoAnc, -last_col())
CoAnc <- dplyr::select (CoAnc, -pop1pop2)

#Arrange the data by the Wang estimate
CoAnc <- arrange(CoAnc, wang)

#Transform the wide dataframe to long to create an estimator column
CoAnc_long <- gather(CoAnc, Estimator, Relatedness, trioML: DyadMl)

#Filter for classical relatedness estimators and plot these against ascending relatedness
CoAnc_moment <- filter (CoAnc_long, Estimator %in% c("wang", "lynchli", "lynchRd", "QGT", "Ritland"))
CoAnc_moment$dyad <- as.character(CoAnc_moment$dyad)
g <- ggplot(data = CoAnc_moment, aes(x = reorder(dyad, Relatedness), y = Relatedness, group = Estimator))+
  geom_line(aes(color = Estimator))
g


#Filter for maximum likelihood estimators and plot these against accending relatedness

CoAnc_ML <- filter (CoAnc_long, Estimator %in% c("trioML", "DyadMl"))
CoAnc_ML$dyad <- as.character(CoAnc_ML$dyad)
g1 <- ggplot(data = CoAnc_ML, aes(x = reorder(dyad, Relatedness), y = Relatedness, group = Estimator))+
  geom_line(aes(color = Estimator))
g1


## CI 95

#Load in the 95% confidence intervals from Coancestry

CI95 <- read.table("data/Vita-elk-1/RelatednessCI95.Txt", sep = ",")
CI95 <- dplyr::select(CI95, -last_col())
CI95 <- dplyr::select(CI95, -last_col())

#Create dataframes with corresponding 2.5 or 97.5 confidence intervals

CI2.5 <- CI95 %>% dplyr::select (1:4,5,7,9,11,13,15,17)
CI97.5 <- CI95 %>% dplyr::select (1:4,6,8,10,12,14,16,18)

#Give the dataframes headers
colnames(CI2.5) <- c("dyad", "indv1", "ind2", "pop1pop2", "trioML", "wang","lynchli", "lynchRd", "Ritland", "QGT", "DyadMl")
CI2.5 <- dplyr::select (CI2.5, -pop1pop2)
colnames(CI97.5) <- c("dyad", "indv1", "ind2", "pop1pop2", "trioML", "wang","lynchli", "lynchRd", "Ritland", "QGT", "DyadMl")
CI97.5 <- dplyr::select (CI97.5, -pop1pop2)

#Transform wide to long format
CI2.5_long<- gather(CI2.5, Estimator, CI2.5, trioML: DyadMl)
CI97.5_long<- gather(CI97.5, Estimator, CI97.5, trioML: DyadMl)


#Join the confidence interval to the original dataframe of relatedness
CI_long <- left_join(CoAnc_long, CI2.5_long, by = c("dyad", "Estimator", "indv1", "ind2"))
CI_long <- left_join(CI_long, CI97.5_long, by = c("dyad", "Estimator", "indv1", "ind2"))


#Filter for the Wang estimator and plot relatedness with CI
wang <- filter (CI_long, Estimator %in% c("wang"))
wang$dyad <- as.character(wang$dyad)
g2 <- ggplot(data = wang, aes(x = reorder(dyad, Relatedness), y = Relatedness, group = Estimator))+
  geom_point(aes(color = Estimator))+
  geom_errorbar(aes(ymin = CI2.5, ymax = CI97.5))
g2

#Export file
write.csv(CI_long, "output/ZOE/Vita_Elk_Wang_Relatedness_18ind.csv")

### Check for correlation between SRI and relatedness using Wang 2002
# Read in the datasets
DT <- readRDS("output/ZOE/4-sri-N10.RDS")
R <- read.csv ("data/Vita_Elk_Wang_Relatedness_18ind.csv")

#Focus on Wang 2002 estimator of relatedness
Wang <- filter(R, Estimator == "wang")

#Re-format the ID columns
DT$ID1 <- as.character(DT$ID1)
DT$ID2 <- as.character(DT$ID2)

DT$ID1 <- substr(DT$ID1,1,nchar(DT$ID1)-5)
DT$ID2 <- substr(DT$ID2,1,nchar(DT$ID2)-5)

DT$ID1 <- gsub("ER_E", "VE", DT$ID1)
DT$ID2 <- gsub("ER_E", "VE", DT$ID2)

Wang <- Wang %>% 
  rename(ID1 = indv1, ID2 = ind2)

#Calculate mean SRI per dyad (SRI calculated weekly in the summer dataset)
Mean_sri <- DT %>%
  group_by(ID1, ID2) %>%
  summarise(mean_sri = mean(sri))

#Join mean SRI with relatedness estimates
DT_Wang <- left_join(Mean_sri, Wang, by = c("ID1", "ID2"))

#Remove SRIs of 0
DT_Wang <- filter (DT_Wang, mean_sri != 0)


g<- ggplot(DT_Wang, aes(Relatedness,mean_sri))+
  geom_point()+
  geom_smooth(method = "lm")
g

#Remove extreme high SRI value (I don't know if this is an error or they were spending a lot of time together)
DT_Wang_high_point_removed <- filter (DT_Wang, mean_sri < 0.1)

g<- ggplot(DT_Wang_high_point_removed, aes(Relatedness,mean_sri))+
  geom_point()+
  geom_smooth(method = "lm")
g

cor.test(DT_Wang$mean_sri, DT_Wang$Relatedness, method = "pearson")
cor.test(DT_Wang_high_point_removed$mean_sri, DT_Wang_high_point_removed$Relatedness, method = "pearson")



### Investigate genetic differentiation between the two social communities we detected
elk.groups <- read.genetable(paste("data/Vita-elk-1/vita_elk_Year_1_groups.csv"), ind = 1, pop = 2, oneColPerAll = TRUE)
heir <- read.table("data/Vita-elk-1/vita_elk_heir.txt", header = T)
heir$com <- as.character(heir$com); heir$pop <- as.character(heir$pop)
heir <- dplyr::select(heir, com, Pop = pop)
strata(elk.groups) <- heir
table(strata(elk.groups, ~Pop/com, combine = FALSE))

# The poppr package output is explained much better by grunwaldlab
# within = FALSE because we only have one strata (i.e. just compare within and between communities)

res.elk <- poppr.amova(elk.groups, ~com, within = FALSE)
res.elk
elksignif   <- randtest(res.elk, nrepet = 999)
plot(elksignif)
elksignif

write.table(res.elk$componentsofcovariance, sep = ",", file = "output/AMOVA.csv")

