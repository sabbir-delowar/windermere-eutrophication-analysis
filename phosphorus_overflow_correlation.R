
#Calling overflow data from websiite####
##2017-2019####
url1 <- 'https://environment.data.gov.uk/water-quality/id/sampling-point/NW-88004250/measurements.json?determinand=0348&startDate=2016-01-01&endDate=2019-12-31'
res = GET(url1)
res = rawToChar(res$content)
data = fromJSON(res)
Phosphorus_w <-(data$items)
result <- Phosphorus_w$"result"
date <- Phosphorus_w$"sample"$"sampleDateTime"
Phosphorus_w$date <-date
Phosphorus_w <- Phosphorus_w[,c("date","result")]
Phosphorus_w$date <- substr(Phosphorus_w$date, 1, 10)
Phosphorus_w$date <- mutate(Phosphorus_w, date=as.Date(date, format = "%Y-%m-%d"))
Phosphorus_w$date <- Phosphorus_w$date$date
Phosphorus_w1 <- Phosphorus_w %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year)
Phosphorus_w1 <- Phosphorus_w1[,c("month","year","result")]
box_year <- boxplot(Phosphorus_w1$result ~ Phosphorus_w1$year , col = c(2:7), cex = 1.5, pch = 16,
                    xlab = "Year",
                    ylab = "Field Phosphorus_w concentration (ug/L)",
                    main = "(a) Boxplot of Phosphorus_w\nfield data")
par(mfrow=c(1,1))
plot(Phosphorus_w$date,Phosphorus_w$result)
lines(Phosphorus_w$date[order(Phosphorus_w$date)], Phosphorus_w$result[order(Phosphorus_w$date)], xlim=range(Phosphorus_w$date), ylim=range(Phosphorus_w$result), pch=16)
Phosphorus_w <- createWorkbook()
addWorksheet(Phosphorus_w, "Phosphorus_w_17-18")
writeData(Phosphorus_w,"Phosphorus_w_17-18",Phosphorus_w1, rowNames = TRUE)
openxlsx::readWorkbook(Phosphorus_w, sheet=1)

##2020-2022####
url1 <- 'https://environment.data.gov.uk/water-quality/id/sampling-point/NW-88004250/measurements.json?determinand=0348&startDate=2019-12-01&endDate=2022-12-31'
res = GET(url1)
res = rawToChar(res$content)
data = fromJSON(res)
Phosphorus_w_ <-(data$items)
result <- Phosphorus_w_$"result"
date <- Phosphorus_w_$"sample"$"sampleDateTime"
Phosphorus_w_$date <-date
str(Phosphorus_w_)
Phosphorus_w_ <- Phosphorus_w_[,c("date","result")]
Phosphorus_w_$date <- substr(Phosphorus_w_$date, 1, 10)
Phosphorus_w_$date <- mutate(Phosphorus_w_, date=as.Date(date, format = "%Y-%m-%d"))
Phosphorus_w_$date <- Phosphorus_w_$date$date
Phosphorus_w_1 <- Phosphorus_w_ %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year)
Phosphorus_w_1 <- Phosphorus_w_1[,c("month","year","result")]
box_year <- boxplot(Phosphorus_w_1$result ~ Phosphorus_w_1$year , col = c(2:7), cex = 1.5, pch = 16,
                    xlab = "Year",
                    ylab = "Field Phosphorus_w_ concentration (ug/L)",
                    main = "(a) Boxplot of Phosphorus_w_\nfield data")
str(Phosphorus_w_1)
par(mfrow=c(1,1))
plot(Phosphorus_w_$date,Phosphorus_w_$result)
lines(Phosphorus_w_$date[order(Phosphorus_w_$date)], Phosphorus_w_$result[order(Phosphorus_w_$date)], xlim=range(Phosphorus_w_$date), ylim=range(Phosphorus_w_$result), pch=16)
addWorksheet(Phosphorus_w,"Phosphorus_w__20-22")
writeData(Phosphorus_w,"Phosphorus_w__20-22",Phosphorus_w_1, rowNames = TRUE)
openxlsx::readWorkbook(Phosphorus_w, sheet=2)
 saveWorkbook(Phosphorus_w,"C://Users//SLL883//OneDrive - Brunel University London//Dissertation//Stat_data//Phosphorus_w.xlsx", overwrite = TRUE )


 
#call P data from pc####
 setwd("C:/Users/SLL883/OneDrive - Brunel University London/Dissertation/Stat_data")
 P_ambleside <- read.table("P_ambleside.csv", header = T, sep = ",",
                     stringsAsFactors = T) 
 str(P_ambleside)
 P_ambleside <- as.data.frame(P_ambleside)

###summary stat#### 
 SumStat_p <-summaryStats(P_ambleside$result ~ P_ambleside$year, digits =2, se = T, ci = T)
 SumStat_p <- data.frame  (unclass(SumStat_p))
 SumFull_p <-summaryFull(P_ambleside$result ~ P_ambleside$year)
 SumFull_p <- data.frame (unclass(SumFull_p))
 # addWorksheet(Stats,"SumStat_p")
 # addWorksheet(Stats, "SumFull_p")
 # writeData(Stats,"SumStat_p",SumStat_p, rowNames = TRUE)
 # writeData(Stats, "SumFull_p",SumFull_p, rowNames = TRUE)
 # openxlsx::readWorkbook(Stats, sheet=1)
 
#adding P data to dataframe####
 # SumFull_1_1$Phosphorus <- as.numeric(+geom_text(     label=rownames(data),      nudge_x = 0.25, nudge_y = 0.25,      check_overlap = T   ))
 # SumFull_2_2$Phosphorus <- as.numeric(paste0(SumFull_p[2,]))
 # SumFull_3_3$Phosphorus <- as.numeric(paste0(SumFull_p[2,]))
 # SumFull_4_4$Phosphorus <- as.numeric(paste0(SumFull_p[2,]))
 # SumFull_5_5$Phosphorus <- as.numeric(paste0(SumFull_p[2,])) 
 
 # SumFull_1_1$Year <- as.factor(SumFull_1_1$Year)
 # SumFull_2_2$Year <- as.factor(SumFull_2_2$Year)
 # SumFull_3_3$Year <- as.factor(SumFull_3_3$Year)
 # SumFull_4_4$Year <- as.factor(SumFull_4_4$Year)
 # SumFull_5_5$Year <- as.factor(SumFull_5_5$Year)


# #sction 1####
# #trend tests
# mymk_p_1 <- kendallTrendTest(Mean~Phosphorus, data=SumFull_1_1)
# tau_mk_p_1 <- round(mymk_p_1$estimate[1],3)
# p_mk_p_1 <- round(mymk_p_1$p.value,3)
# 
# pearson_p_1 <- cor.test(SumFull_1_1$Phosphorus, SumFull_1_1$Mean, method=c("pearson"))
# r_pearson_p_1 <- round(pearson_p_1$estimate,3)
# p_pearson_p_1 <- round(pearson_p_1$p.value,3)
# 
# spearman_p_1 <- cor.test(SumFull_1_1$Phosphorus, SumFull_1_1$Mean, method=c("spearman"))
# r_spearman_p_1 <- round(spearman_p_1$estimate,3)
# p_spearman_p_1 <- round(spearman_p_1$p.value,3)
# 
# 
# lm_p_1 <- lm(Mean~Phosphorus,SumFull_1_1)
# summary(lm_p_1)
# slope_p_1 <- coef(lm_p_1)[2]
# 
# 
# #plot
# ggplot(data = SumFull_1_1, aes(x = Phosphorus, y = Mean)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
#   geom_point(size = 2)+
#   ggtitle("Section 1")+
#   ylab("Mean NDCI")+
#   theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
#         panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(colour = "black")) +
#   stat_poly_eq(aes(label = after_stat(eq.label))) +
#   stat_poly_eq(label.y = 0.9)+
#   ylim(-.2,.3)
# 
# #sction 2####
# #trend tests
# mymk_p_2 <- kendallTrendTest(Mean~Phosphorus, data=SumFull_2_2)
# tau_mk_p_2 <- round(mymk_p_2$estimate[1],3)
# p_mk_p_2 <- round(mymk_p_2$p.value,3)
# 
# pearson_p_2 <- cor.test(SumFull_2_2$Phosphorus, SumFull_2_2$Mean, method=c("pearson"))
# r_pearson_p_2 <- round(pearson_p_2$estimate,3)
# p_pearson_p_2 <- round(pearson_p_2$p.value,3)
# 
# spearman_p_2 <- cor.test(SumFull_2_2$Phosphorus, SumFull_2_2$Mean, method=c("spearman"))
# r_spearman_p_2 <- round(spearman_p_2$estimate,3)
# p_spearman_p_2 <- round(spearman_p_2$p.value,3)
# 
# 
# lm_p_2 <- lm(Mean~Phosphorus,SumFull_2_2)
# summary(lm_p_2)
# slope_p_2 <- coef(lm_p_2)[2]
# 
# 
# #plot
# ggplot(data = SumFull_2_2, aes(x = Phosphorus, y = Mean)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
#   geom_point(size = 2)+
#   ggtitle("Section 2")+
#   ylab("Mean NDCI")+
#   theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
#         panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(colour = "black")) +
#   stat_poly_eq(aes(label = after_stat(eq.label))) +
#   stat_poly_eq(label.y = 0.9)+
#   ylim(-.2,.3)
# 
# #sction 3####
# #trend tests
# mymk_p_3 <- kendallTrendTest(Mean~Phosphorus, data=SumFull_3_3)
# tau_mk_p_3 <- round(mymk_p_3$estimate[1],3)
# p_mk_p_3 <- round(mymk_p_3$p.value,3)
# 
# pearson_p_3 <- cor.test(SumFull_3_3$Phosphorus, SumFull_3_3$Mean, method=c("pearson"))
# r_pearson_p_3 <- round(pearson_p_3$estimate,3)
# p_pearson_p_3 <- round(pearson_p_3$p.value,3)
# 
# spearman_p_3 <- cor.test(SumFull_3_3$Phosphorus, SumFull_3_3$Mean, method=c("spearman"))
# r_spearman_p_3 <- round(spearman_p_3$estimate,3)
# p_spearman_p_3 <- round(spearman_p_3$p.value,3)
# 
# 
# lm_p_3 <- lm(Mean~Phosphorus,SumFull_3_3)
# summary(lm_p_3)
# slope_p_3 <- coef(lm_p_3)[2]
# 
# 
# #plot
# ggplot(data = SumFull_3_3, aes(x = Phosphorus, y = Mean)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
#   geom_point(size = 2)+
#   ggtitle("Section 3")+
#   ylab("Mean NDCI")+
#   theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
#         panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(colour = "black")) +
#   stat_poly_eq(aes(label = after_stat(eq.label))) +
#   stat_poly_eq(label.y = 0.9)+
#   ylim(-.2,.3)
# 
# 
# #sction 4####
# #trend tests
# mymk_p_4 <- kendallTrendTest(Mean~Phosphorus, data=SumFull_4_4)
# tau_mk_p_4 <- round(mymk_p_4$estimate[1],3)
# p_mk_p_4 <- round(mymk_p_4$p.value,3)
# 
# pearson_p_4 <- cor.test(SumFull_4_4$Phosphorus, SumFull_4_4$Mean, method=c("pearson"))
# r_pearson_p_4 <- round(pearson_p_4$estimate,3)
# p_pearson_p_4 <- round(pearson_p_4$p.value,3)
# 
# spearman_p_4 <- cor.test(SumFull_4_4$Phosphorus, SumFull_4_4$Mean, method=c("spearman"))
# r_spearman_p_4 <- round(spearman_p_4$estimate,3)
# p_spearman_p_4 <- round(spearman_p_4$p.value,3)
# 
# 
# lm_p_4 <- lm(Mean~Phosphorus,SumFull_4_4)
# summary(lm_p_4)
# slope_p_4 <- coef(lm_p_4)[2]
# 
# 
# #plot
# ggplot(data = SumFull_4_4, aes(x = Phosphorus, y = Mean)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
#   geom_point(size = 2)+
#   ggtitle("Section 4")+
#   ylab("Mean NDCI")+
#   theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
#         panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(colour = "black")) +
#   stat_poly_eq(aes(label = after_stat(eq.label))) +
#   stat_poly_eq(label.y = 0.9)+
#   ylim(-.2,.3)
# 
# #sction 5####
# #trend tests
# mymk_p_5 <- kendallTrendTest(Mean~Phosphorus, data=SumFull_5_5)
# tau_mk_p_5 <- round(mymk_p_5$estimate[1],3)
# p_mk_p_5 <- round(mymk_p_5$p.value,3)
# 
# pearson_p_5 <- cor.test(SumFull_5_5$Phosphorus, SumFull_5_5$Mean, method=c("pearson"))
# r_pearson_p_5 <- round(pearson_p_5$estimate,3)
# p_pearson_p_5 <- round(pearson_p_5$p.value,3)
# 
# spearman_p_5 <- cor.test(SumFull_5_5$Phosphorus, SumFull_5_5$Mean, method=c("spearman"))
# r_spearman_p_5 <- round(spearman_p_5$estimate,3)
# p_spearman_p_5 <- round(spearman_p_5$p.value,3)
# 
# 
# lm_p_5 <- lm(Mean~Phosphorus,SumFull_5_5)
# summary(lm_p_5)
# slope_p_5 <- coef(lm_p_5)[2]
# 
# 
# #plot
# ggplot(data = SumFull_5_5, aes(x = Phosphorus, y = Mean)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
#   geom_point(size = 2)+
#   ggtitle("Section 5")+
#   ylab("Mean NDCI")+
#   theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
#         panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
#         panel.background = element_rect(colour = "black")) +
#   stat_poly_eq(aes(label = after_stat(eq.label))) +
#   stat_poly_eq(label.y = 0.9)+
#   ylim(-.2,.3)

#correlation with Phosphorus and combined mean NDCI####

 
Sumfull_y_1$Phosphorus <- c(0.474, 0.357, 0.346, 0.260, 0.371, 0.348)
Sumfull_y_1$Year <- as.factor(Sumfull_y_1$Year)
Sumfull_p_m <- Sumfull_y_1
Sumfull_p_m$Overflow <- c(423.68, 1201.17, 1175.65, 1719.03, 1043.95,0)


#trend tests
mymk_p_m <- kendallTrendTest(Mean~Phosphorus, data=Sumfull_p_m)
tau_mk_p_m <- round(mymk_p_m$estimate[1],3)
p_mk_p_m <- round(mymk_p_m$p.value,3)

pearson_p_m <- cor.test(Sumfull_p_m$Phosphorus, Sumfull_p_m$Mean, method=c("pearson"))
r_pearson_p_m <- round(pearson_p_m$estimate,3)
p_pearson_p_m <- round(pearson_p_m$p.value,3)

spearman_p_m <- cor.test(Sumfull_p_m$Phosphorus, Sumfull_p_m$Mean, method=c("spearman"))
r_spearman_p_m <- round(spearman_p_m$estimate,3)
p_spearman_p_m <- round(spearman_p_m$p.value,3)


lm_p_m <- lm(Mean~Phosphorus,Sumfull_p_m)
summary(lm_p_m)
slope_p_m <- coef(lm_p_m)[2]




#plot
ggplot(data = Sumfull_p_m, aes(x = Phosphorus, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+ 
  ylab("Mean NDCI")+ 
  xlab("Phosphorus (mg/l)")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.1,.1)


#all sections trend data combined####
# 
# tau_mk <- c(tau_mk_p_1,tau_mk_p_2, tau_mk_p_3, tau_mk_p_4, tau_mk_p_5, tau_mk_p_m)
# p_mk <- c(p_mk_p_1,p_mk_p_2, p_mk_p_3, p_mk_p_4, p_mk_p_5, p_mk_p_m)
# r_pearson <- c(r_pearson_p_1,r_pearson_p_2, r_pearson_p_3, r_pearson_p_4, r_pearson_p_5, r_pearson_p_m)
# p_pearpson <- c(p_pearson_p_1,p_pearson_p_2, p_pearson_p_3, p_pearson_p_4, p_pearson_p_5, p_pearson_p_m)
# r_spearman <- c(r_spearman_p_1,r_spearman_p_2, r_spearman_p_3, r_spearman_p_4, r_spearman_p_5, r_spearman_p_m)
# p_spearman <- c(p_spearman_p_1,p_spearman_p_2, p_spearman_p_3, p_spearman_p_4, p_spearman_p_5, p_spearman_p_m)
# section <- c("Section 1", "Section 2", "Section 3", "Section 4", "Section 5", "combined")
# slope <- c(slope_p_1,slope_p_2, slope_p_3, slope_p_4, slope_p_5, slope_p_m)
# trend_p <- data.frame(section, slope, tau_mk,p_mk,r_pearson, p_pearpson, r_spearman, p_spearman)
# 
# trend_y <- data.frame(slope_p_m, tau_mk_p_m, p_mk_p_m, r_pearson_p_m, 
#                       p_pearson_p_m, r_spearman_p_m, p_spearman_p_m)
# 
# addWorksheet(Trend, "Phosphorus_correlation")
# writeData(Trend,"Phosphorus_correlation",trend_p, rowNames = TRUE)
# openxlsx::readWorkbook(Trend, sheet=1)
# # saveWorkbook(Trend,"C://Users//SLL883//OneDrive - Brunel University London//Dissertation//Stat_data//trend.xlsx", overwrite = TRUE )

#Overflow vs NDCI####
Sumfull_p_m<-Sumfull_p_m[-6,]
#trend tests
mymk_o_m <- kendallTrendTest(Mean~Overflow, data=Sumfull_p_m)
tau_mk_o_m <- round(mymk_o_m$estimate[1],3)
p_mk_o_m <- round(mymk_o_m$p.value,3)

pearson_o_m <- cor.test(Sumfull_p_m$Overflow, Sumfull_p_m$Mean, method=c("pearson"))
r_pearson_o_m <- round(pearson_o_m$estimate,3)
p_pearson_o_m <- round(pearson_o_m$p.value,3)

spearman_o_m <- cor.test(Sumfull_p_m$Overflow, Sumfull_p_m$Mean, method=c("spearman"))
r_spearman_o_m <- round(spearman_o_m$estimate,3)
p_spearman_o_m <- round(spearman_o_m$p.value,3)


lm_o_m <- lm(Mean~Overflow,Sumfull_p_m)
summary(lm_o_m)
slope_o_m <- coef(lm_o_m)[2]

#plot
ggplot(data = Sumfull_p_m, aes(x = Overflow, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+ 
  ylab("Mean NDCI")+
  xlab("Overflow (hours)")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.1,.1)

#Overflow vs P####
#trend tests
mymk_po_m <- kendallTrendTest(Phosphorus~Overflow, data=Sumfull_p_m)
tau_mk_po_m <- round(mymk_po_m$estimate[1],3)
p_mk_po_m <- round(mymk_po_m$p.value,3)

pearson_po_m <- cor.test(Sumfull_p_m$Overflow, Sumfull_p_m$Phosphorus, method=c("pearson"))
r_pearson_po_m <- round(pearson_po_m$estimate,3)
p_pearson_po_m <- round(pearson_po_m$p.value,3)

spearman_po_m <- cor.test(Sumfull_p_m$Overflow, Sumfull_p_m$Phosphorus, method=c("spearman"))
r_spearman_po_m <- round(spearman_po_m$estimate,3)
p_spearman_po_m <- round(spearman_po_m$p.value,3)


lm_po_m <- lm(Phosphorus~Overflow,Sumfull_p_m)
summary(lm_po_m)
slope_po_m <- coef(lm_po_m)[2]

#plot
ggplot(data = Sumfull_p_m, aes(x = Overflow, y = Phosphorus)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+ 
  ylab("Phosphorus (mg/l)")+
  xlab("Overflow (hours)")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(.2,.6)
