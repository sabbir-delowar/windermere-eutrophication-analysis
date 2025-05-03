par(mfrow=c(3,3))
#initial setup####
pacman::p_load(pgirmess,boot, bootstrap,sp, rgdal, tidyr, raster,
               dplyr, DescTools, EnvStats, ggplot2, ggpubr, 
               lattice,PMCMR, PMCMRplus, rcompanion,openxlsx,jsonlite,
               httr,tidyverse,streamgraph,rgl, ggpmisc,ggplot2,
               viridis,hrbrthemes,plotly,devtools, factoextra, rstatix,
               )
#set working directory
wd <- ("C:/Users/SLL883/OneDrive - Brunel University London/Dissertation/gis_BASIC DATA/Lake_data/NDCI_/")
setwd(wd)

#create a workbook
Stats <- createWorkbook()

#preparing the data section wise and summary stat and regression ####

##section_1####

###data preparation####
NDCI_1_path <- paste0(wd,"NDCI_1")
all_NDCI_1 <- list.files(NDCI_1_path,
                          full.names = TRUE,
                          pattern = ".tif$")
all_NDCI_1
NDCI_1 <- stack(all_NDCI_1)

# Change the data into a dataframe
NDCI_1_df <- as.data.frame(NDCI_1, xy=TRUE)

#delete coordinates
NDCI_1_df <- NDCI_1_df[ -c(1,2) ]
head(NDCI_1_df)

#change band names
names(NDCI_1_df)[1]<-paste("2017")
names(NDCI_1_df)[2]<-paste("2018")
names(NDCI_1_df)[3]<-paste("2019")
names(NDCI_1_df)[4]<-paste("2020")
names(NDCI_1_df)[5]<-paste("2021")
names(NDCI_1_df)[6]<-paste("2022")
head(NDCI_1_df)

#horizonal  to verticle
NDCI_1_df <- NDCI_1_df %>% pivot_longer(cols=c('2017','2018', '2019', '2020', '2021', '2022'),
                                          names_to='Year',
                                          values_to='NDCI_1')
#convert '0' values to NA
NDCI_1_df[NDCI_1_df==0] <- NA
NDCI_1_df

#delete NA values
NDCI_1_df <- na.omit(NDCI_1_df)
head(NDCI_1_df)

#year into factor
NDCI_1_df$Year <- factor(NDCI_1_df$Year)
str(NDCI_1_df)

###summary stat####
SumStat_1<-summaryStats(NDCI_1_df$NDCI_1 ~ NDCI_1_df$Year, digits =2, se = T, ci = T)
SumStat_1 <- data.frame  (unclass(SumStat_1))
SumFull_1<-summaryFull(NDCI_1_df$NDCI_1 ~ NDCI_1_df$Year)
SumFull_1 <- data.frame (unclass(SumFull_1))
# addWorksheet(Stats,"SumStat_1")
# addWorksheet(Stats, "SumFull_1")
# writeData(Stats,"SumStat_1",SumStat_1, rowNames = TRUE)
# writeData(Stats, "SumFull_1",SumFull_1, rowNames = TRUE)
# openxlsx::readWorkbook(Stats, sheet=1)

###regression####
#method ="pearson", "kendall", or "spearman".
SumFull_1_1 <- SumFull_1[2,]
SumFull_1_1
SumFull_1_1 <- SumFull_1_1 %>% pivot_longer(cols=c('X2017','X2018', 'X2019', 'X2020', 'X2021', 'X2022'),
                                              names_to='Year',
                                              values_to='Mean')
SumFull_1_1$Year <- c(2017,2018,2019,2020,2021,2022)

#trend tests
mymk_1 <- kendallTrendTest(Mean~Year, data=SumFull_1_1)
tau_mk_1 <- round(mymk_1$estimate[1],3)
p_mk_1 <- round(mymk_1$p.value,3)

pearson_1 <- cor.test(SumFull_1_1$Year, SumFull_1_1$Mean, method=c("pearson"))
r_pearson_1 <- round(pearson_1$estimate,3)
p_pearson_1 <- round(pearson_1$p.value,3)

spearman_1 <- cor.test(SumFull_1_1$Year, SumFull_1_1$Mean, method=c("spearman"))
r_spearman_1 <- round(spearman_1$estimate,3)
p_spearman_1 <- round(spearman_1$p.value,3)


lm_1 <- lm(Mean~Year,SumFull_1_1)
summary(lm_1)
slope_1 <- coef(lm_1)[2]


#plot
ggplot(data = SumFull_1_1, aes(x = Year, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("Section 1")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##section_2####

###data preparation####
NDCI_2_path <- paste0(wd,"NDCI_2")
all_NDCI_2 <- list.files(NDCI_2_path,
                         full.names = TRUE,
                         pattern = ".tif$")
all_NDCI_2
NDCI_2 <- stack(all_NDCI_2)

# Change the data into a dataframe
NDCI_2_df <- as.data.frame(NDCI_2, xy=TRUE)

#delete coordinates
NDCI_2_df <- NDCI_2_df[ -c(1,2) ]
head(NDCI_2_df)

#change band names
names(NDCI_2_df)[1]<-paste("2017")
names(NDCI_2_df)[2]<-paste("2018")
names(NDCI_2_df)[3]<-paste("2019")
names(NDCI_2_df)[4]<-paste("2020")
names(NDCI_2_df)[5]<-paste("2021")
names(NDCI_2_df)[6]<-paste("2022")
head(NDCI_2_df)

#horizonal  to verticle
NDCI_2_df <- NDCI_2_df %>% pivot_longer(cols=c('2017','2018', '2019', '2020', '2021', '2022'),
                                        names_to='Year',
                                        values_to='NDCI_2')
#convert '0' values to NA
NDCI_2_df[NDCI_2_df==0] <- NA
NDCI_2_df

#delete NA values
NDCI_2_df <- na.omit(NDCI_2_df)
head(NDCI_2_df)

#year into factor
NDCI_2_df$Year <- factor(NDCI_2_df$Year)
str(NDCI_2_df)

###summary stat####
SumStat_2<-summaryStats(NDCI_2_df$NDCI_2 ~ NDCI_2_df$Year, digits =2, se = T, ci = T)
SumStat_2 <- data.frame  (unclass(SumStat_2))
SumFull_2<-summaryFull(NDCI_2_df$NDCI_2 ~ NDCI_2_df$Year)
SumFull_2 <- data.frame (unclass(SumFull_2))
# addWorksheet(Stats,"SumStat_2")
# addWorksheet(Stats, "SumFull_2")
# writeData(Stats,"SumStat_2",SumStat_2, rowNames = TRUE)
# writeData(Stats, "SumFull_2",SumFull_2, rowNames = TRUE)
# openxlsx::readWorkbook(Stats, sheet=1)

###regression####
#method ="pearson", "kendall", or "spearman".
SumFull_2_2 <- SumFull_2[2,]
SumFull_2_2
SumFull_2_2 <- SumFull_2_2 %>% pivot_longer(cols=c('X2017','X2018', 'X2019', 'X2020', 'X2021', 'X2022'),
                                            names_to='Year',
                                            values_to='Mean')
SumFull_2_2$Year <- c(2017,2018,2019,2020,2021,2022)
SumFull_2_2

#trend tests
mymk_2 <- kendallTrendTest(Mean~Year, data=SumFull_2_2)
tau_mk_2 <- round(mymk_2$estimate[1],3)
p_mk_2 <- round(mymk_2$p.value,3)

pearson_2 <- cor.test(SumFull_2_2$Year, SumFull_2_2$Mean, method=c("pearson"))
r_pearson_2 <- round(pearson_2$estimate,3)
p_pearson_2 <- round(pearson_2$p.value,3)

spearman_2 <- cor.test(SumFull_2_2$Year, SumFull_2_2$Mean, method=c("spearman"))
r_spearman_2 <- round(spearman_2$estimate,3)
p_spearman_2 <- round(spearman_2$p.value,3)


lm_2 <- lm(Mean~Year,SumFull_2_2)
summary(lm_2)
slope_2 <- coef(lm_2)[2]


#plot
ggplot(data = SumFull_2_2, aes(x = Year, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("Section 2")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##section_3####

###data preparation####
NDCI_3_path <- paste0(wd,"NDCI_3")
all_NDCI_3 <- list.files(NDCI_3_path,
                         full.names = TRUE,
                         pattern = ".tif$")
all_NDCI_3
NDCI_3 <- stack(all_NDCI_3)

# Change the data into a dataframe
NDCI_3_df <- as.data.frame(NDCI_3, xy=TRUE)

#delete coordinates
NDCI_3_df <- NDCI_3_df[ -c(1,2) ]
head(NDCI_3_df)

#change band names
names(NDCI_3_df)[1]<-paste("2017")
names(NDCI_3_df)[2]<-paste("2018")
names(NDCI_3_df)[3]<-paste("2019")
names(NDCI_3_df)[4]<-paste("2020")
names(NDCI_3_df)[5]<-paste("2021")
names(NDCI_3_df)[6]<-paste("2022")
head(NDCI_3_df)

#horizonal  to verticle
NDCI_3_df <- NDCI_3_df %>% pivot_longer(cols=c('2017','2018', '2019', '2020', '2021', '2022'),
                                        names_to='Year',
                                        values_to='NDCI_3')
#convert '0' values to NA
NDCI_3_df[NDCI_3_df==0] <- NA
NDCI_3_df

#delete NA values
NDCI_3_df <- na.omit(NDCI_3_df)
head(NDCI_3_df)

#year into factor
NDCI_3_df$Year <- factor(NDCI_3_df$Year)
str(NDCI_3_df)

###summary stat####
SumStat_3<-summaryStats(NDCI_3_df$NDCI_3 ~ NDCI_3_df$Year, digits =2, se = T, ci = T)
SumStat_3 <- data.frame  (unclass(SumStat_3))
SumFull_3<-summaryFull(NDCI_3_df$NDCI_3 ~ NDCI_3_df$Year)
SumFull_3 <- data.frame (unclass(SumFull_3))
# addWorksheet(Stats,"SumStat_3")
# addWorksheet(Stats, "SumFull_3")
# writeData(Stats,"SumStat_3",SumStat_3, rowNames = TRUE)
# writeData(Stats, "SumFull_3",SumFull_3, rowNames = TRUE)
# openxlsx::readWorkbook(Stats, sheet=1)

###regression####
#method ="pearson", "kendall", or "spearman".
SumFull_3_3 <- SumFull_3[2,]
SumFull_3_3
SumFull_3_3 <- SumFull_3_3 %>% pivot_longer(cols=c('X2017','X2018', 'X2019', 'X2020', 'X2021', 'X2022'),
                                            names_to='Year',
                                            values_to='Mean')
SumFull_3_3$Year <- c(2017,2018,2019,2020,2021,2022)
SumFull_3_3

#trend tests
mymk_3 <- kendallTrendTest(Mean~Year, data=SumFull_3_3)
tau_mk_3 <- round(mymk_3$estimate[1],3)
p_mk_3 <- round(mymk_3$p.value,3)

pearson_3 <- cor.test(SumFull_3_3$Year, SumFull_3_3$Mean, method=c("pearson"))
r_pearson_3 <- round(pearson_3$estimate,3)
p_pearson_3 <- round(pearson_3$p.value,3)

spearman_3 <- cor.test(SumFull_3_3$Year, SumFull_3_3$Mean, method=c("spearman"))
r_spearman_3 <- round(spearman_3$estimate,3)
p_spearman_3 <- round(spearman_3$p.value,3)


lm_3 <- lm(Mean~Year,SumFull_3_3)
summary(lm_3)
slope_3 <- coef(lm_3)[2]


#plot
ggplot(data = SumFull_3_3, aes(x = Year, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("Section 3")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##section_4####

###data preparation####
NDCI_4_path <- paste0(wd,"NDCI_4")
all_NDCI_4 <- list.files(NDCI_4_path,
                         full.names = TRUE,
                         pattern = ".tif$")
all_NDCI_4
NDCI_4 <- stack(all_NDCI_4)

# Change the data into a dataframe
NDCI_4_df <- as.data.frame(NDCI_4, xy=TRUE)

#delete coordinates
NDCI_4_df <- NDCI_4_df[ -c(1,2) ]
head(NDCI_4_df)

#change band names
names(NDCI_4_df)[1]<-paste("2017")
names(NDCI_4_df)[2]<-paste("2018")
names(NDCI_4_df)[3]<-paste("2019")
names(NDCI_4_df)[4]<-paste("2020")
names(NDCI_4_df)[5]<-paste("2021")
names(NDCI_4_df)[6]<-paste("2022")
head(NDCI_4_df)

#horizonal  to verticle
NDCI_4_df <- NDCI_4_df %>% pivot_longer(cols=c('2017','2018', '2019', '2020', '2021', '2022'),
                                        names_to='Year',
                                        values_to='NDCI_4')
#convert '0' values to NA
NDCI_4_df[NDCI_4_df==0] <- NA
NDCI_4_df

#delete NA values
NDCI_4_df <- na.omit(NDCI_4_df)
head(NDCI_4_df)

#year into factor
NDCI_4_df$Year <- factor(NDCI_4_df$Year)
str(NDCI_4_df)

###summary stat####
SumStat_4<-summaryStats(NDCI_4_df$NDCI_4 ~ NDCI_4_df$Year, digits =2, se = T, ci = T)
SumStat_4 <- data.frame  (unclass(SumStat_4))
SumFull_4<-summaryFull(NDCI_4_df$NDCI_4 ~ NDCI_4_df$Year)
SumFull_4 <- data.frame (unclass(SumFull_4))
# addWorksheet(Stats,"SumStat_4")
# addWorksheet(Stats, "SumFull_4")
# writeData(Stats,"SumStat_4",SumStat_4, rowNames = TRUE)
# writeData(Stats, "SumFull_4",SumFull_4, rowNames = TRUE)
# openxlsx::readWorkbook(Stats, sheet=1)

###regression####
#method ="pearson", "kendall", or "spearman".
SumFull_4_4 <- SumFull_4[2,]
SumFull_4_4
SumFull_4_4 <- SumFull_4_4 %>% pivot_longer(cols=c('X2017','X2018', 'X2019', 'X2020', 'X2021', 'X2022'),
                                            names_to='Year',
                                            values_to='Mean')
SumFull_4_4$Year <- c(2017,2018,2019,2020,2021,2022)
str(SumFull_4_4)

#trend tests
#trend tests
mymk_4 <- kendallTrendTest(Mean~Year, data=SumFull_4_4)
tau_mk_4 <- round(mymk_4$estimate[1],3)
p_mk_4 <- round(mymk_4$p.value,3)

pearson_4 <- cor.test(SumFull_4_4$Year, SumFull_4_4$Mean, method=c("pearson"))
r_pearson_4 <- round(pearson_4$estimate,3)
p_pearson_4 <- round(pearson_4$p.value,3)

spearman_4 <- cor.test(SumFull_4_4$Year, SumFull_4_4$Mean, method=c("spearman"))
r_spearman_4 <- round(spearman_4$estimate,3)
p_spearman_4 <- round(spearman_4$p.value,3)


lm_4 <- lm(Mean~Year,SumFull_4_4)
summary(lm_4)
slope_4 <- coef(lm_4)[2]


#plot
ggplot(data = SumFull_4_4, aes(x = Year, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("Section 4")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##section_5####

###data preparation####
NDCI_5_path <- paste0(wd,"NDCI_5")
all_NDCI_5 <- list.files(NDCI_5_path,
                         full.names = TRUE,
                         pattern = ".tif$")
all_NDCI_5
NDCI_5 <- stack(all_NDCI_5)

# Change the data into a dataframe
NDCI_5_df <- as.data.frame(NDCI_5, xy=TRUE)

#delete coordinates
NDCI_5_df <- NDCI_5_df[ -c(1,2) ]
head(NDCI_5_df)

#change band names
names(NDCI_5_df)[1]<-paste("2017")
names(NDCI_5_df)[2]<-paste("2018")
names(NDCI_5_df)[3]<-paste("2019")
names(NDCI_5_df)[4]<-paste("2020")
names(NDCI_5_df)[5]<-paste("2021")
names(NDCI_5_df)[6]<-paste("2022")
head(NDCI_5_df)

#horizonal  to verticle
NDCI_5_df <- NDCI_5_df %>% pivot_longer(cols=c('2017','2018', '2019', '2020', '2021', '2022'),
                                        names_to='Year',
                                        values_to='NDCI_5')
#convert '0' values to NA
NDCI_5_df[NDCI_5_df==0] <- NA
NDCI_5_df

#delete NA values
NDCI_5_df <- na.omit(NDCI_5_df)
head(NDCI_5_df)

#year into factor
NDCI_5_df$Year <- factor(NDCI_5_df$Year)
str(NDCI_5_df)

###summary stat####
SumStat_5<-summaryStats(NDCI_5_df$NDCI_5 ~ NDCI_5_df$Year, digits =2, se = T, ci = T)
SumStat_5 <- data.frame  (unclass(SumStat_5))
SumFull_5<-summaryFull(NDCI_5_df$NDCI_5 ~ NDCI_5_df$Year)
SumFull_5 <- data.frame (unclass(SumFull_5))
# addWorksheet(Stats,"SumStat_5")
# addWorksheet(Stats, "SumFull_5")
# writeData(Stats,"SumStat_5",SumStat_5, rowNames = TRUE)
# writeData(Stats, "SumFull_5",SumFull_5, rowNames = TRUE)
# openxlsx::readWorkbook(Stats, sheet=1)

###regression####
#method ="pearson", "kendall", or "spearman".
SumFull_5_5 <- SumFull_5[2,]
SumFull_5_5
SumFull_5_5 <- SumFull_5_5 %>% pivot_longer(cols=c('X2017','X2018', 'X2019', 'X2020', 'X2021', 'X2022'),
                                            names_to='Year',
                                            values_to='Mean')
SumFull_5_5$Year <- c(2017,2018,2019,2020,2021,2022)
SumFull_5_5

#trend tests
mymk_5 <- kendallTrendTest(Mean~Year, data=SumFull_5_5)
tau_mk_5 <- round(mymk_5$estimate[1],3)
p_mk_5 <- round(mymk_5$p.value,3)

pearson_5 <- cor.test(SumFull_5_5$Year, SumFull_5_5$Mean, method=c("pearson"))
r_pearson_5 <- round(pearson_5$estimate,3)
p_pearson_5 <- round(pearson_5$p.value,3)

spearman_5 <- cor.test(SumFull_5_5$Year, SumFull_5_5$Mean, method=c("spearman"))
r_spearman_5 <- round(spearman_5$estimate,3)
p_spearman_5 <- round(spearman_5$p.value,3)


lm_5 <- lm(Mean~Year,SumFull_5_5)
summary(lm_5)
slope_5 <- coef(lm_5)[2]


#plot
ggplot(data = SumFull_5_5, aes(x = Year, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("Section 5")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)




#create data frame for the whole dataset (mean)####

fulldata <- as.data.frame(SumFull_1_1)
names(fulldata)[2]<-paste("1")
fulldata$'2' <- SumFull_2_2$Mean
fulldata$'3' <- SumFull_3_3$Mean
fulldata$'4' <- SumFull_4_4$Mean
fulldata$'5' <- SumFull_5_5$Mean
fulldata

addWorksheet(Stats, "Full data")
writeData(Stats,"Full data",fulldata, rowNames = TRUE)
openxlsx::readWorkbook(Stats, sheet=1)
# saveWorkbook(Stats,"C://Users//SLL883//OneDrive - Brunel University London//Dissertation//Stat_data//Stat_r.xlsx", overwrite = TRUE )

#horizonal  to verticle
fulldata_1 <- fulldata %>% pivot_longer(cols=c('1','2', '3', '4', '5'),
                                          names_to='Section',
                                          values_to='Mean')

fulldata_1$Section <- factor(fulldata_1$Section)
fulldata_1$Year <- factor(fulldata_1$Year)
str(fulldata_1)
attach(fulldata_1)

#Yearly full data analysis####

##stacked bar chart####

#can use position="stack/dodge"
ggplot(fulldata_1, aes(fill=Section, y=Mean, x=Year)) + 
  geom_bar(position="dodge", stat="identity") #can use position="stack/dodge"

ggplot(fulldata_1, aes(x=Year, y=Mean, group=Section, color=Section)) +
  geom_point(aes(color=Section), size = 3) + 
  geom_line(aes(color=Section), size = 1.2) +
  theme(text = element_text(size = 14)) +
  ylab ("Mean")
##summary stat####

Sumfull_y <-summaryFull(Mean ~ Year)
Sumfull_y <- data.frame (unclass(Sumfull_y))

##regression####
Sumfull_y_1 <- Sumfull_y[2,]
Sumfull_y_1
Sumfull_y_1 <- Sumfull_y_1 %>% pivot_longer(cols=c('X2017','X2018', 'X2019', 'X2020', 'X2021', 'X2022'),
                                        names_to='Year',
                                        values_to='Mean')
Sumfull_y_1$Year <- c(2017,2018,2019,2020,2021,2022)

Sumfull_y_1
str(Sumfull_y_1)

#trend tests
mymk_y_1 <- kendallTrendTest(Mean~Year, data=Sumfull_y_1)
tau_mk_y_1 <- round(mymk_y_1$estimate[1],3)
p_mk_y_1 <- round(mymk_y_1$p.value,3)

pearson_y_1 <- cor.test(Sumfull_y_1$Year, Sumfull_y_1$Mean, method=c("pearson"))
r_pearson_y_1 <- round(pearson_y_1$estimate,3)
p_pearson_y_1 <- round(pearson_y_1$p.value,3)

spearman_y_1 <- cor.test(Sumfull_y_1$Year, Sumfull_y_1$Mean, method=c("spearman"))
r_spearman_y_1 <- round(spearman_y_1$estimate,3)
p_spearman_y_1 <- round(spearman_y_1$p.value,3)


lm_y_1 <- lm(Mean~Year,Sumfull_y_1)
summary(lm_y_1)
slope_y_1 <- coef(lm_y_1)[2]


#plot
ggplot(data = Sumfull_y_1, aes(x = Year, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("All sections ccombined")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)


#all sections trend data combined####

tau_mk <- c(tau_mk_1,tau_mk_2, tau_mk_3, tau_mk_4, tau_mk_5, tau_mk_y_1)
p_mk <- c(p_mk_1,p_mk_2, p_mk_3, p_mk_4, p_mk_5, p_mk_y_1)
r_pearson <- c(r_pearson_1,r_pearson_2, r_pearson_3, r_pearson_4, r_pearson_5, r_pearson_y_1)
p_pearpson <- c(p_pearson_1,p_pearson_2, p_pearson_3, p_pearson_4, p_pearson_5, p_pearson_y_1)
r_spearman <- c(r_spearman_1,r_spearman_2, r_spearman_3, r_spearman_4, r_spearman_5, r_spearman_y_1)
p_spearman <- c(p_spearman_1,p_spearman_2, p_spearman_3, p_spearman_4, p_spearman_5, p_spearman_y_1)
section <- c("Section 1", "Section 2", "Section 3", "Section 4", "Section 5", "combined")
slope <- c(slope_1,slope_2, slope_3, slope_4, slope_5, slope_y_1)
trend_y <- data.frame(section, slope, tau_mk,p_mk,r_pearson, p_pearpson, r_spearman, p_spearman)
trend_y
Trend <- createWorkbook()
addWorksheet(Trend, "Yearly_trend")
writeData(Trend,"Yearly_trend",trend_y, rowNames = TRUE)
openxlsx::readWorkbook(Trend, sheet=1)


#hypothesis testing(difference between years####

#boxplot
boxplot(Mean ~ Year, col = c(2:7), cex = 1.5, pch = 16,
        xlab = "Year",
        ylab = "NDCI Mean (2017-2022)")

# Histograms - based on well and month - Original data
histogram(~ Mean | Year, 
          data = fulldata_1, 
          layout = c(2,3),
          main = "Histogram - Arsenic data")

#normality test
NDCI_gof <- gofGroupTest(Mean ~ Year)
NDCI_gof

##outlier
rosnerTest(Mean , k=4)

#variance test
LeveneTest(Mean ~ Year, center = mean)

# Serial Correlation Coefficient
EnvStats::print(serialCorrelationTest(Mean))

fulldata_1 %>%
  group_by(Year) %>%
  get_summary_stats(Mean, type = "common")

ggboxplot(fulldata_1, x = "Year", y = "Mean", add = "jitter")

res.fried <- fulldata_1 %>% friedman_test (Mean ~ Year|Section)
str(fulldata_1)
fulldata_1 %>% friedman_effsize(Mean ~ Year|Section)
pwc <- fulldata_1 %>%
  wilcox_test(Mean ~ Year, paired = TRUE, p.adjust.method = "bonferroni")
pwc
pwc2 <- fulldata_1 %>%
  sign_test(Mean ~ Year, p.adjust.method = "bonferroni")
pwc2

pwc <- pwc %>% add_xy_position(x = "Year")
ggboxplot(fulldata_1, x = "Year", y = "Mean", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

res.aov <- anova_test(data = fulldata_1, dv = Mean, wid = Section, within = Year)
get_anova_table(res.aov)
pwc <- fulldata_1 %>%
  pairwise_t_test(
    Mean ~ Year, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
pwc <- pwc %>% add_xy_position(x = "Year")
ggboxplot(fulldata_1, x = "Year", y = "Mean", add = "point") + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Nonparametric test - Kruskal Wallis
kruskal.test(Mean ~ Year)

# Multiple comparisons - Pairwise compariso
kruskalmc(Mean ~ Year)

# One-way ANOVA
av <- aov((Mean) ~ Year)
summary(av)


# Post hoc test - Tukey's test (pairwise comparisons)
tk <- PostHocTest(av, method = "hsd")
tk
plot(tk)
# Strip chart - Method 2 
EnvStats::stripChart(Mean ~ Year, pch = 16, points.cex = 2, 
                     col = c(1:6), p.value = TRUE, 
                     ylab = "NDCI")

