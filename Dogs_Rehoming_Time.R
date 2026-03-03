Codes 
#------------------------------------------------------------------------------- 
# Used Libraries 
if (!require(e1071)) install.packages("e1071") 
if (!require(MASS)) install.packages("MASS") 
if (!require(ggplot2)) install.packages("ggplot2") 
if (!require(dplyr)) install.packages("dplyr") 
if (!require(knitr)) install.packages("knitr") 
library(e1071) 
library(MASS) 
library(ggplot2) 
library(dplyr) 
library(knitr) 
#------------------------------------------------------------------------------- 
# 1. LOADING DATA AND QUICK INSPECTION 
createsample(202007327) 
save(mysample, file = "mysample.RData") 
head(mysample) 
str(mysample) 
summary(mysample) 
table(mysample$Breed) 
#------------------------------------------------------------------------------- 
# DATA CLEANING 
df <- mysample 
total_rows <- nrow(df) 
total_rows 
# Removing duplicates 
df$Rehomed[df$Rehomed == 99999] <- NA 
duplicates_removed <- sum(duplicated(df)) 
df <- df[!duplicated(df), ] 
# Remove missing Rehomed times 
missing_rehomed <- sum(is.na(df$Rehomed)) 
df <- df[!is.na(df$Rehomed), ] 
# Removing missing Breed 
missing_breed <- sum(is.na(df$Breed)) 
df <- df[!is.na(df$Breed), ] 
# Removing negative time values 
negative_count <- sum(df$Rehomed < 0 | df$Visited < 0) 
df <- df[df$Rehomed >= 0 & df$Visited >= 0, ] 
df_clean <- df 
cleaned_rows <- nrow(df_clean) 
total_removed <- total_rows - cleaned_rows 
cat("Total removed:", total_removed, "\n") 
#------------------------------------------------------------------------------- 
# SPLITING DATA BY BREED 
mixed    <- df_clean[df_clean$Breed == "Mixed Breed", ] 
shihtzu  <- df_clean[df_clean$Breed == "Shih Tzu", ] 
stafford <- df_clean[df_clean$Breed == "Staffordshire Bull Terrier", ] 
x1 <- mixed$Rehomed[mixed$Rehomed > 0] 
x2 <- shihtzu$Rehomed[shihtzu$Rehomed > 0] 
x3 <- stafford$Rehomed[stafford$Rehomed > 0] 
#------------------------------------------------------------------------------- 
# Summary tables 
summary_table <- data.frame( 
  Breed = c("Mixed Breed", "Shih Tzu", "Staffordshire Bull Terrier"), 
  n     = c(length(x1), length(x2), length(x3)), 
  Mean  = c(mean(x1), mean(x2), mean(x3)), 
  SD    = c(sd(x1), sd(x2), sd(x3)), 
  Skewness = c(skewness(x1), skewness(x2), skewness(x3)) 
) 
summary_table 
#------------------------------------------------------------------------------- 
# GRAPHICAL SUMMARIES 
# Boxplot for each breed 
boxplot(Rehomed ~ Breed, data = df_clean, 
        main = "Rehoming Time by Breed", 
        xlab = "Breed", ylab = "Weeks", 
        col = c("skyblue","lightpink","lightgreen")) 
# Histograms for each breed 
hist(mixed$Rehomed, main="Mixed Breed Rehoming Time", 
     xlab="Weeks", col="lightgrey", border="white") 
hist(shihtzu$Rehomed, main="Shih Tzu Rehoming Time", 
     xlab="Weeks", col="lightgrey", border="white") 
hist(stafford$Rehomed, main="Staffordshire Bull Terrier Rehoming Time", 
     xlab="Weeks", col="lightgrey", border="white") 
# Age distribution  graph 
df_age <- df_clean %>% 
  count(Breed, Age) %>% 
  group_by(Breed) %>% 
  mutate(Percentage = n/sum(n)*100) 
ggplot(df_age, aes(x=Breed, y=n, fill=Age)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=paste0(round(Percentage,1),"%")), 
            position=position_dodge(0.9), vjust=-0.5) + 
  labs(title="Age Group Distribution by Breed", x="Breed", y="Count") + 
  theme_minimal() 
# Health conditions graph 
df_clean$Health_Group <- cut(df_clean$Health, 
                             breaks=c(0,40,70,100), 
                             labels=c("Poor (<40)","Moderate (<70)","Good (>=70)"), 
                             include.lowest=TRUE) 
df_health <- df_clean %>% 
  count(Breed, Health_Group) %>% 
  group_by(Breed) %>% 
  mutate(Percentage = n/sum(n)*100) 
ggplot(df_health, aes(x=Breed, y=n, fill=Health_Group)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=paste0(round(Percentage,1),"%")), 
            position=position_dodge(0.9), vjust=-0.5) + 
  labs(title="Health Group Distribution by Breed", 
       x="Breed", y="Count", fill="Health Group") + 
  theme_minimal() 
# Return status graph 
ggplot(df_clean %>% mutate(Returned_clean = ifelse(is.na(Returned),"Unknown",Returned)), 
       aes(x=Returned_clean, y=Rehomed, fill=Returned_clean)) + 
  stat_summary(fun="mean", geom="bar", width=0.5) + 
  stat_summary(fun="mean", geom="text", 
               aes(label=round(..y..,1)), vjust=-0.5) + 
  labs(title="Average Rehoming Time by Return Status", 
       x="Return Status", y="Weeks") + 
  theme_minimal() + theme(legend.position="none") 
#------------------------------------------------------------------------------- 
# FITING MODELS 
fit_lognorm_mixed    <- fitdistr(x1, "lognormal") 
fit_gamma_mixed      <- fitdistr(x1, "gamma") 
fit_lognorm_shihtzu  <- fitdistr(x2, "lognormal") 
fit_gamma_shihtzu    <- fitdistr(x2, "gamma") 
fit_lognorm_stafford <- fitdistr(x3, "lognormal") 
fit_gamma_stafford   <- fitdistr(x3, "gamma") 
# ------------------------------------------------------------------------------ 
# MODEL CHECKING (HISTOGRAMS + FITTED CURVES + QQ PLOTS) 
plot_fitted <- function(data, fit_lnorm, fit_gamma, title){ 
  hist(data, probability=TRUE, main=title, xlab="Weeks", 
       13 
       col="grey85", border="white", ylim=c(0, 0.06)) 
  # Lognormal curve 
  curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), 
        add=TRUE, col="red", lwd=2) 
  # Gamma curve 
  curve(dgamma(x, fit_gamma$estimate[1], fit_gamma$estimate[2]), 
        add=TRUE, col="blue", lwd=2, lty=2) 
  legend("topright", 
         legend = c("Lognormal", "Gamma"), 
         col    = c("red", "blue"), 
         lwd    = 2, 
         lty    = c(1,2), 
         bty    = "n", 
         cex    = 0.9) 
} 
par(mfrow = c(1, 3), mar=c(4,4,3,1), oma=c(0,0,2,0)) 
plot_fitted(x1, fit_lognorm_mixed, fit_gamma_mixed, "Mixed Breed") 
plot_fitted(x2, fit_lognorm_shihtzu, fit_gamma_shihtzu, "Shih Tzu") 
plot_fitted(x3, fit_lognorm_stafford, fit_gamma_stafford, "Staffordshire") 
par(mfrow = c(1,1)) 
# Normal Q-Q plots 
par(mfrow=c(1,3)) 
qqnorm(x1, main="Normal Q-Q: Mixed Breed"); qqline(x1, col="red") 
qqnorm(x2, main="Normal Q-Q: Shih Tzu"); qqline(x2, col="red") 
qqnorm(x3, main="Normal Q-Q: Staffordshire"); qqline(x3, col="red") 
par(mfrow=c(1,1))   
# Gamma Q-Q plots  
par(mfrow=c(1,3)) 
qqplot(qgamma(ppoints(length(x1)), 
              shape=fit_gamma_mixed$estimate["shape"], 
              rate =fit_gamma_mixed$estimate["rate"]), 
       x1, main="Gamma Q-Q: Mixed Breed", 
       xlab="Theoretical", ylab="Sample"); abline(0,1,col="red") 
qqplot(qgamma(ppoints(length(x2)), 
              shape=fit_gamma_shihtzu$estimate["shape"], 
              rate =fit_gamma_shihtzu$estimate["rate"]), 
       x2, main="Gamma Q-Q: Shih Tzu", 
       xlab="Theoretical", ylab="Sample"); abline(0,1,col="red") 
qqplot(qgamma(ppoints(length(x3)), 
              shape=fit_gamma_stafford$estimate["shape"], 
              rate =fit_gamma_stafford$estimate["rate"]), 
       x3, main="Gamma Q-Q: Staffordshire", 
       xlab="Theoretical", ylab="Sample"); abline(0,1,col="red") 
par(mfrow=c(1,1)) 
# Lognormal Q-Q plots 
par(mfrow=c(1,3)) 
qqnorm(log(x1), main="Lognormal Q-Q: Mixed Breed"); qqline(log(x1), col="red") 
qqnorm(log(x2), main="Lognormal Q-Q: Shih Tzu"); qqline(log(x2), col="red") 
qqnorm(log(x3), main="Lognormal Q-Q: Staffordshire"); qqline(log(x3), col="red") 
par(mfrow=c(1,1)) 
#------------------------------------------------------------------------------- 
# Model Testing 
# KS tests 
ks1_gamma   <- ks.test(x1, "pgamma", shape=fit_gamma_mixed$estimate[1], rate=fit_gamma_mixed$estimate[2]) 
ks1_lognorm <- ks.test(x1, "plnorm", meanlog=fit_lognorm_mixed$estimate[1], 
                       sdlog=fit_lognorm_mixed$estimate[2]) 
ks2_gamma   <- ks.test(x2, "pgamma", shape=fit_gamma_shihtzu$estimate[1], rate=fit_gamma_shihtzu$estimate[2]) 
ks2_lognorm <- ks.test(x2, "plnorm", meanlog=fit_lognorm_shihtzu$estimate[1], 
                       sdlog=fit_lognorm_shihtzu$estimate[2]) 
ks3_gamma   <- ks.test(x3, "pgamma", shape=fit_gamma_stafford$estimate[1], rate=fit_gamma_stafford$estimate[2]) 
ks3_lognorm <- ks.test(x3, "plnorm", meanlog=fit_lognorm_stafford$estimate[1], 
                       sdlog=fit_lognorm_stafford$estimate[2]) 
# Chi-square tests  
chisq_test <- function(x, fit_gamma, fit_lnorm){ 
  h <- hist(x, breaks=10, plot=FALSE) 
  p_gamma <- pgamma(h$breaks[-1], shape=fit_gamma$estimate[1], rate=fit_gamma$estimate[2]) - 
    pgamma(h$breaks[-length(h$breaks)], shape=fit_gamma$estimate[1], rate=fit_gamma$estimate[2]) 
  p_gamma <- p_gamma / sum(p_gamma) 
  p_lognorm <- plnorm(h$breaks[-1], meanlog=fit_lnorm$estimate[1], sdlog=fit_lnorm$estimate[2]) - 
    plnorm(h$breaks[-length(h$breaks)], meanlog=fit_lnorm$estimate[1], sdlog=fit_lnorm$estimate[2]) 
  p_lognorm <- p_lognorm / sum(p_lognorm) 
  list( 
    gamma = chisq.test(h$counts, p=p_gamma), 
    lognorm = chisq.test(h$counts, p=p_lognorm) 
  ) 
} 
chisq1 <- chisq_test(x1, fit_gamma_mixed, fit_lognorm_mixed) 
chisq2 <- chisq_test(x2, fit_gamma_shihtzu, fit_lognorm_shihtzu) 
chisq3 <- chisq_test(x3, fit_gamma_stafford, fit_lognorm_stafford) 
#------------------------------------------------------------------------------- 
# CONFIDENCE INTERVALS  
t1 <- t.test(x1, mu=27) 
t2 <- t.test(x2, mu=27) 
t3 <- t.test(x3, mu=27) 
t1 
t2 
t3 
#-------------------------------------------------------------------------------- 
# CI Plot for Dog Rehoming Times 
breeds <- c("Mixed Breed", "Shih Tzu", "Staffordshire") 
means  <- c(19.11, 19.50, 19.46) 
lower  <- c(18.41, 15.77, 18.01) 
upper  <- c(19.82, 23.23, 20.91) 
verticalpos <- 1:3 
cex <- 1.2          
par(oma=c(0,0,0,0), mar=c(6,2,4,4)) 
plot(x=1, y=0, type="n", xaxt='n', yaxt='n',  
     xlab="", ylab="", bty="n", 
     xlim=c(10, 28), ylim=c(0.5, 3.5)) 
mtext("95% Confidence Intervals for Mean Rehoming Time by Breed", 
      side = 3, line = 1.5, cex = 1.4, font = 1) 
# line for 27 weeks 
abline(v = 27, col="red", lty=2, lwd=1.5) 
text(27, 3.35, "27 weeks", col="red", cex=0.9, pos=3) 
points(means, verticalpos, pch=16, cex=1.3) 
for (i in 1:3) { 
  lines(c(lower[i], upper[i]), c(verticalpos[i], verticalpos[i]), lwd=2) 
  lines(c(lower[i], lower[i]), c(verticalpos[i] - 0.15, verticalpos[i] + 0.15), lwd=2) 
  lines(c(upper[i], upper[i]), c(verticalpos[i] - 0.15, verticalpos[i] + 0.15), lwd=2) 
} 
axis(side=1, at=seq(10, 28, 2), cex.axis=1) 
mtext("Mean Rehoming Time (weeks)", side=1, line=4, cex=1.2) 
mtext(text = breeds, side=2, line=2, at=verticalpos, las=1, cex=1.1, adj=0) 
ci_labels <- paste0("(", lower, ", ", upper, ")") 
mtext(text = ci_labels, side=4, line=-2, at=verticalpos, las=1, cex=1) 
#------------------------------------------------------------------------------- 
# BREED COMPARISONS 
tt12 <- t.test(x1, x2, var.equal=FALSE) 
tt13 <- t.test(x1, x3, var.equal=FALSE) 
tt23 <- t.test(x2, x3, var.equal=FALSE) 
tt12 
tt13 
tt23