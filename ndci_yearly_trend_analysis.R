#year wise full data analysis####

##2017####

###data preparation####
NDCI_17_df <- fulldata_1[fulldata_1$Year == '2017',]
NDCI_17_df$Section <- as.numeric (NDCI_17_df$Section)
str(NDCI_17_df)
###regression####
#method ="pearson", "kendall", or "spearman".

#trend tests
mymk_17 <- kendallTrendTest(Mean~Section, data=NDCI_17_df)
tau_mk_17 <- round(mymk_17$estimate[1],3)
p_mk_17 <- round(mymk_17$p.value,3)

pearson_17 <- cor.test(NDCI_17_df$Section, NDCI_17_df$Mean, method=c("pearson"))
r_pearson_17 <- round(pearson_17$estimate,3)
p_pearson_17 <- round(pearson_17$p.value,3)

spearman_17 <- cor.test(NDCI_17_df$Section, NDCI_17_df$Mean, method=c("spearman"))
r_spearman_17 <- round(spearman_17$estimate,3)
p_spearman_17 <- round(spearman_17$p.value,3)


lm_17 <- lm(Mean~Section,NDCI_17_df)
summary(lm_17)
slope_17 <- coef(lm_17)[2]


#plot
ggplot(data = NDCI_17_df, aes(x = Section, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("2017")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)



##2018####

###data preparation####
NDCI_18_df <- fulldata_1[fulldata_1$Year == '2018',]
NDCI_18_df$Section <- as.numeric (NDCI_18_df$Section)
str(NDCI_18_df)
###regression####
#method ="pearson", "kendall", or "spearman".

#trend tests
mymk_18 <- kendallTrendTest(Mean~Section, data=NDCI_18_df)
tau_mk_18 <- round(mymk_18$estimate[1],3)
p_mk_18 <- round(mymk_18$p.value,3)

pearson_18 <- cor.test(NDCI_18_df$Section, NDCI_18_df$Mean, method=c("pearson"))
r_pearson_18 <- round(pearson_18$estimate,3)
p_pearson_18 <- round(pearson_18$p.value,3)

spearman_18 <- cor.test(NDCI_18_df$Section, NDCI_18_df$Mean, method=c("spearman"))
r_spearman_18 <- round(spearman_18$estimate,3)
p_spearman_18 <- round(spearman_18$p.value,3)


lm_18 <- lm(Mean~Section,NDCI_18_df)
summary(lm_18)
slope_18 <- coef(lm_18)[2]


#plot
ggplot(data = NDCI_18_df, aes(x = Section, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("2018")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##2019####

###data preparation####
NDCI_19_df <- fulldata_1[fulldata_1$Year == '2019',]
NDCI_19_df$Section <- as.numeric (NDCI_19_df$Section)
str(NDCI_19_df)
###regression####
#method ="pearson", "kendall", or "spearman".

#trend tests
mymk_19 <- kendallTrendTest(Mean~Section, data=NDCI_19_df)
tau_mk_19 <- round(mymk_19$estimate[1],3)
p_mk_19 <- round(mymk_19$p.value,3)

pearson_19 <- cor.test(NDCI_19_df$Section, NDCI_19_df$Mean, method=c("pearson"))
r_pearson_19 <- round(pearson_19$estimate,3)
p_pearson_19 <- round(pearson_19$p.value,3)

spearman_19 <- cor.test(NDCI_19_df$Section, NDCI_19_df$Mean, method=c("spearman"))
r_spearman_19 <- round(spearman_19$estimate,3)
p_spearman_19 <- round(spearman_19$p.value,3)


lm_19 <- lm(Mean~Section,NDCI_19_df)
summary(lm_19)
slope_19 <- coef(lm_19)[2]


#plot
ggplot(data = NDCI_19_df, aes(x = Section, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("2019")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##2020####

###data preparation####
NDCI_20_df <- fulldata_1[fulldata_1$Year == '2020',]
NDCI_20_df$Section <- as.numeric (NDCI_20_df$Section)
str(NDCI_20_df)
###regression####
#method ="pearson", "kendall", or "spearman".

#trend tests
mymk_20 <- kendallTrendTest(Mean~Section, data=NDCI_20_df)
tau_mk_20 <- round(mymk_20$estimate[1],3)
p_mk_20 <- round(mymk_20$p.value,3)

pearson_20 <- cor.test(NDCI_20_df$Section, NDCI_20_df$Mean, method=c("pearson"))
r_pearson_20 <- round(pearson_20$estimate,3)
p_pearson_20 <- round(pearson_20$p.value,3)

spearman_20 <- cor.test(NDCI_20_df$Section, NDCI_20_df$Mean, method=c("spearman"))
r_spearman_20 <- round(spearman_20$estimate,3)
p_spearman_20 <- round(spearman_20$p.value,3)


lm_20 <- lm(Mean~Section,NDCI_20_df)
summary(lm_20)
slope_20 <- coef(lm_20)[2]


#plot
ggplot(data = NDCI_20_df, aes(x = Section, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("2020")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##2021####

###data preparation####
NDCI_21_df <- fulldata_1[fulldata_1$Year == '2021',]
NDCI_21_df$Section <- as.numeric (NDCI_21_df$Section)
str(NDCI_21_df)
###regression####
#method ="pearson", "kendall", or "spearman".

#trend tests
mymk_21 <- kendallTrendTest(Mean~Section, data=NDCI_21_df)
tau_mk_21 <- round(mymk_21$estimate[1],3)
p_mk_21 <- round(mymk_21$p.value,3)

pearson_21 <- cor.test(NDCI_21_df$Section, NDCI_21_df$Mean, method=c("pearson"))
r_pearson_21 <- round(pearson_21$estimate,3)
p_pearson_21 <- round(pearson_21$p.value,3)

spearman_21 <- cor.test(NDCI_21_df$Section, NDCI_21_df$Mean, method=c("spearman"))
r_spearman_21 <- round(spearman_21$estimate,3)
p_spearman_21 <- round(spearman_21$p.value,3)


lm_21 <- lm(Mean~Section,NDCI_21_df)
summary(lm_21)
slope_21 <- coef(lm_21)[2]


#plot
ggplot(data = NDCI_21_df, aes(x = Section, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("2021")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)

##2022####

###data preparation####
NDCI_22_df <- fulldata_1[fulldata_1$Year == '2022',]
NDCI_22_df$Section <- as.numeric (NDCI_22_df$Section)
str(NDCI_22_df)
###regression####
#method ="pearson", "kendall", or "spearman".

#trend tests
mymk_22 <- kendallTrendTest(Mean~Section, data=NDCI_22_df)
tau_mk_22 <- round(mymk_22$estimate[1],3)
p_mk_22 <- round(mymk_22$p.value,3)

pearson_22 <- cor.test(NDCI_22_df$Section, NDCI_22_df$Mean, method=c("pearson"))
r_pearson_22 <- round(pearson_22$estimate,3)
p_pearson_22 <- round(pearson_22$p.value,3)

spearman_22 <- cor.test(NDCI_22_df$Section, NDCI_22_df$Mean, method=c("spearman"))
r_spearman_22 <- round(spearman_22$estimate,3)
p_spearman_22 <- round(spearman_22$p.value,3)


lm_22 <- lm(Mean~Section,NDCI_22_df)
summary(lm_22)
slope_22 <- coef(lm_22)[2]


#plot
ggplot(data = NDCI_22_df, aes(x = Section, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("2022")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)


#combined####
##stacked bar chart####

ggplot(fulldata_1, aes(fill=Year, y=Mean, x=Section)) + 
  geom_bar(position="fill", stat="identity") #can use position="stack/dodge/fill"

ggplot(fulldata_1, aes(x=Section, y=Mean, group=Year, color=Year)) +
  geom_point(aes(color=Year), size = 3) + 
  geom_line(aes(color=Year), size = 1.2) +
  theme(text = element_text(size = 14)) +
  ylab ("Mean")
##summary stat####
attach(fulldata_1)

Sumfull_s <-summaryFull(Mean ~ Section)
Sumfull_s <- data.frame (unclass(Sumfull_s))

##regression####
Sumfull_s_1 <- Sumfull_s[2,]
Sumfull_s_1
Sumfull_s_1 <- Sumfull_s_1 %>% pivot_longer(cols=c('X1','X2', 'X3', 'X4', 'X5'),
                                            names_to='Section',
                                            values_to='Mean')
Sumfull_s_1$Section <- c(1,2,3,4,5)
Sumfull_s_1
#trend tests
mymk_s_1 <- kendallTrendTest(Mean~Mean, data=Sumfull_s_1)
tau_mk_s_1 <- round(mymk_s_1$estimate[1],3)
p_mk_s_1 <- round(mymk_s_1$p.value,3)

pearson_s_1 <- cor.test(Sumfull_s_1$Mean, Sumfull_s_1$Mean, method=c("pearson"))
r_pearson_s_1 <- round(pearson_s_1$estimate,3)
p_pearson_s_1 <- round(pearson_s_1$p.value,3)

spearman_s_1 <- cor.test(Sumfull_s_1$Mean, Sumfull_s_1$Mean, method=c("spearman"))
r_spearman_s_1 <- round(spearman_s_1$estimate,3)
p_spearman_s_1 <- round(spearman_s_1$p.value,3)


lm_s_1 <- lm(Mean~Section,Sumfull_s_1)
summary(lm_s_1)
slope_s_1 <- coef(lm_s_1)[2]


#plot
ggplot(data = Sumfull_s_1, aes(x = Section, y = Mean)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 2)+
  ggtitle("All sections combined")+
  theme(panel.grid.major = element_line(colour = "grey70"), text = element_text(size = 14),
        panel.ontop = FALSE, plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(colour = "black")) +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9)+
  ylim(-.2,.3)


#all year trend data combined####

tau_mk <- c(tau_mk_17,tau_mk_18, tau_mk_19, tau_mk_20, tau_mk_21,tau_mk_22, tau_mk_s_1)
p_mk <- c(p_mk_17,p_mk_18, p_mk_19, p_mk_20, p_mk_21,p_mk_22, p_mk_s_1)
r_pearson <- c(r_pearson_17,r_pearson_18, r_pearson_19, r_pearson_20, r_pearson_21,r_pearson_22, r_pearson_s_1)
p_pearpson <- c(p_pearson_17,p_pearson_18, p_pearson_19, p_pearson_20, p_pearson_21, p_pearson_22, p_pearson_s_1)
r_spearman <- c(r_spearman_17,r_spearman_18, r_spearman_19, r_spearman_20, r_spearman_21, r_spearman_22, r_spearman_s_1)
p_spearman <- c(p_spearman_17,p_spearman_18, p_spearman_19, p_spearman_20, p_spearman_21, p_spearman_22, p_spearman_s_1)
Year <- c("2017", "2018", "2019", "2020", "2021", "2022", "combined")
slope <- c(slope_17,slope_18, slope_19, slope_20, slope_21, slope_22, slope_s_1)
trend_s <- data.frame(Year, slope, tau_mk,p_mk,r_pearson, p_pearpson, r_spearman, p_spearman)
trend_s
addWorksheet(Trend, "Section_trend")
writeData(Trend,"Section_trend",trend_s, rowNames = TRUE)
openxlsx::readWorkbook(Trend, sheet=2)



#hypothesis testing(difference between sections####

#boxplot
boxplot(Mean ~ Section, col = c(2:7), cex = 1.5, pch = 16,
        xlab = "Section",
        ylab = "NDCI Mean (2017-2022)")

# Histograms - based on well and month - Original data
histogram(~ Mean | Section, 
          data = fulldata_1, 
          layout = c(2,3),
          main = "Histogram - Arsenic data")

#normality test
NDCI_gof <- gofGroupTest(Mean ~ Section)
NDCI_gof

##outlier
rosnerTest(Mean , k=4)

#variance test
LeveneTest(Mean ~ Section, center = mean)

# Serial Correlation Coefficient
EnvStats::print(serialCorrelationTest(Mean))

# Nonparametric test - Kruskal Wallis

kruskal.test(Mean ~ Section)

# Multiple comparisons - Pairwise comparison
kruskalmc(Mean ~ Section)

# One-way ANOVA
av <- aov((Mean) ~ Section)
plot(av)
summary(av)

# Post hoc test - Tukey's test (pairwise comparisons)
tk <- PostHocTest(av, method = "hsd")
tk
plot(tk)

DunnettTest(Mean, Section, control = "1", conf.level = 0.95)

dn <- DunnettTest(Mean ~ Section)
dn
plot(dn, col = c(1:5))
