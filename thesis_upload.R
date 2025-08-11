library(haven)      
library(dplyr)      # for data manipulation
library(ggplot2)    # for visualization
library(summarytools) 
library(gtsummary)  

# Load the data
hbsc <- read_sav("/Users/aries/Downloads/RESEARCH PROJECT MENTAL/PROBLEMATIC GAMING/HBSC DATA FILE/HBSC Scotland 2022 data for Luqyana 290525.sav")
View(hbsc)
summary(hbsc)
summary(is.na(hbsc))
colSums(is.na(hbsc))
names(hbsc)


# cleaned data
hbsc_clean <- hbsc[complete.cases(hbsc[, vars_needed]), ]
nrow(hbsc_clean)
nrow(hbsc)

cat("Dropped", nrow(hbsc_clean) - nrow(hbsc), "cases due to missing data.")

# check per variable
colSums(is.na(hbsc[, vars_needed]))

  
cont_vars <- c("CohenPSS4", "gadscore", "Family_support_scale", "Peer_support_scale")
cat_vars_trial <- c("problematicgame", "gad3groups", "grade", "FAS_quintGP", "leftout", "lonely", "sleepdificulty", "genderid")

# Combine all variables
vars_needed_2 <- c(cont_vars, cat_vars)


vars_in_hbsc <- vars_needed[vars_needed %in% names(hbsc)]
colSums(is.na(hbsc[, vars_in_hbsc]))


missing_counts <- colSums(is.na(hbsc[, vars_needed]))
print(missing_counts)

# prior descriptive analysis

# group gender into 3

#check levels
levels(hbsc_clean$genderid)
# check levels and values
table(hbsc_clean$genderid, useNA = "ifany")

# recode into 3 groups
hbsc_clean$gender3grouped <- case_when(
  hbsc_clean$genderid == "I identify myself as a boy"  ~ "Boy",
  hbsc_clean$genderid == "I identify myself as a girl" ~ "Girl",
  hbsc_clean$genderid %in% c("Neither boy nor girl", "Other/s") ~ "Other"
)

# Convert to factor and specify the order of levels:
hbsc_clean$gender3grouped <- factor(hbsc_clean$gender3grouped, 
                                    levels = c("Boy", "Girl", "Other"))

summary(hbsc_clean[, c("problematicgame", 
                       "leftout", "lonely", 
                       "CohenPSS4", "gadscore", "gad3groups", "sleepdificulty", 
                       "genderid", "Family_support_scale", "Peer_support_scale", 
                       "grade", "FAS_quintGP", "leftout_oftenalways", "lonely_mostalways", "sleepdiff_binary")])
sapply(hbsc_clean[, c("problematicgame", 
                      "leftout", "lonely", 
                      "CohenPSS4", "gadscore", "gad3groups", "sleepdificulty", 
                      "genderid", "Family_support_scale", "Peer_support_scale", 
                      "grade", "FAS_quintGP", "leftout_oftenalways", "lonely_mostalways", "sleepdiff_binary")], class)


hbsc$Family_support_scale_binary
class(hbsc_clean$Family_support_scale_binary)

# change double into factors
library(dplyr)

hbsc_clean <- hbsc_clean %>%
  mutate(across(c(problematicgame, FAS_quintGP, leftout, lonely, gad3groups, sleepdificulty, genderid, grade, Family_support_scale_binary, leftout_oftenalways, lonely_mostalways, sleepdiff_binary, gender3grouped), ~ as_factor(.)))


sum(is.na(hbsc_clean$FAS_quintGP))  
hbsc_clean %>%
  select(fasfamcar, fasbedroom, fascomputers, fasholidays, genderid, grade) %>%  # adjust based on FAS calc
  summarise(across(everything(), ~ sum(is.na(.))))


# desc stat

library(gtsummary)
library(dplyr)

# List  continuous and categorical variables
cont_vars <- c("CohenPSS4", "gadscore", "Family_support_scale", "Peer_support_scale")
cat_vars <- c("problematicgame", "gad3groups", "grade", "FAS_quintGP", "leftout_oftenalways", "lonely_mostalways", "sleepdiff_binary", "gender3grouped")


# Combine all variables
vars_needed <- c(cont_vars, cat_vars)

# Create descriptive table
hbsc_clean %>%
  select(all_of(vars_needed)) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  bold_labels()

hbsc_clean %>%
  select(all_of(vars_needed)) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_n() %>% 
  bold_labels()


names(hbsc_clean)
str(hbsc_clean$sleepdiff_binary)
str(hbsc_clean$leftout_oftenalways)
vars_needed

# desc analysis for categorical variables

# Cross-tab: anxiety group (gad3groups) by FAS and grade
with(hbsc_clean, table(gad3groups, FAS_quintGP, grade))

library(ggplot2)
library(dplyr)


unique(hbsc_clean$grade)
# For S2
s2 <- hbsc_clean %>% filter(grade == "Secondary 2")
table_s2 <- table(s2$gad3groups, s2$FAS_quintGP)
chisq.test(table_s2)

# For S4
s4 <- hbsc_clean %>% filter(grade == "Secondary 4")
table_s4 <- table(s4$gad3groups, s4$FAS_quintGP)
chisq.test(table_s4)


# Cross-tab: sleep difficulties by FAS and grade
with(hbsc_clean, table(sleepdificulty, FAS_quintGP, grade))

labelled::val_labels(hbsc$sleepdificulty)

# recode sleep difficulties into binary
# Sleep difficulty: weekly or more (1) vs. less (<1) (0)
hbsc_clean$sleepdiff_binary <- ifelse(hbsc_clean$sleepdificulty %in% c("About every day", "More than once a week", "About every week"), 1, 0)
hbsc_clean$sleepdiff_binary <- factor(
  hbsc_clean$sleepdiff_binary,
  levels = c("0", "1"),
  labels = c("Less than weekly", "Weekly or more")
)
hbsc_clean$sleepdiff_binary
class(hbsc_clean$sleepdiff_binary)

#check assumption
hbsc_clean$sleepdificulty
hbsc_clean$sleepdiff_binary
hbsc_clean$FAS_quintGP
hbsc_clean$grade
unique(hbsc_clean$sleepdiff_binary)

table(hbsc_clean$sleepdificulty, hbsc_clean$FAS_quintGP, useNA = 'ifany')

#assumption check
chisq.test(table_s2_sleep)$expected
chisq.test(table_s4_sleep)$expected

# For S2
table_s2_sleep <- table(s2$sleepdificulty, s2$FAS_quintGP)
table_s2_sleep
chisq.test(table_s2_sleep)

# Make sure "table_s2_sleep" is a matrix
table_s2_sleep_pct <- prop.table(table_s2_sleep, margin = 2) * 100
round(table_s2_sleep_pct, 1)  # Round for reporting

# S2 binary variables
table_s2_sleep_binary <- table(s2$sleepdiff_binary, s2$FAS_quintGP)
table_s2_sleep_binary
chisq.test(table_s2_sleep_binary)


table_s2_sleep_binary_pct <- prop.table(table_s2_sleep_binary, margin = 2) * 100
round(table_s2_sleep_binary_pct, 1)  # Round for reporting


# For S4
table_s4_sleep <- table(s4$sleepdificulty, s4$FAS_quintGP)
chisq.test(table_s4_sleep)

table_s4_sleep_pct <- prop.table(table_s4_sleep, margin = 2) * 100
round(table_s4_sleep_pct, 1)  # Round for reporting

# S4 binary variables
table_s4_sleep_binary <- table(s4$sleepdiff_binary, s4$FAS_quintGP)
table_s4_sleep_binary
chisq.test(table_s4_sleep_binary)

table_s4_sleep_binary_pct <- prop.table(table_s4_sleep_binary, margin = 2) * 100
round(table_s4_sleep_binary_pct, 1)  # Round for reporting



# Cross-tab: feeling left out by FAS and grade
with(hbsc_clean, table(leftout, FAS_quintGP, grade))

labelled::val_labels(hbsc$leftout)



#assumption check
chisq.test(table_s2_leftout)$expected
chisq.test(table_s4_leftout)$expected

# For S2
table_s2_leftout <- table(s2$leftout, s2$FAS_quintGP)
table_s2_leftout
chisq.test(table_s2_leftout)

# For S2 BINARY
table_s2_leftout_binary <- table(s2$leftout_oftenalways, s2$FAS_quintGP)
table_s2_leftout_binary
chisq.test(table_s2_leftout_binary)

# Make sure "table_s2_sleep" is a matrix
table_s2_leftout_binary_pct <- prop.table(table_s2_leftout_binary, margin = 2) * 100
round(table_s2_leftout_binary_pct, 1)  # Round for reporting

# For S4
table_s4_leftout_binary <- table(s4$leftout_oftenalways, s4$FAS_quintGP)
chisq.test(table_s4_leftout_binary)

table_s4_leftout_binary_pct <- prop.table(table_s4_leftout_binary, margin = 2) * 100
round(table_s4_leftout_binary_pct, 1)  # Round for reporting


# Cross-tab: feeling lonely by FAS and grade
with(hbsc_clean, table(lonely, FAS_quintGP, grade))

# For S2
table_s2_lonely <- table(s2$lonely, s2$FAS_quintGP)
table_s2_lonely
chisq.test(table_s2_lonely)

# For S2 LONELY BINARY
table_s2_lonely_binary <- table(s2$lonely_mostalways, s2$FAS_quintGP)
table_s2_lonely_binary
chisq.test(table_s2_lonely_binary)

# Make sure "table_s2_sleep" is a matrix
table_s2_lonely_pct_binary <- prop.table(table_s2_lonely_binary, margin = 2) * 100
round(table_s2_lonely_pct_binary, 1)  # Round for reporting


#assumption check
chisq.test(table_s2_lonely)$expected
chisq.test(table_s4_lonely)$expected

# For S4
table_s4_lonely_binary <- table(s4$lonely_mostalways, s4$FAS_quintGP)
chisq.test(table_s4_lonely_binary)

table_s4_lonely_pct_binary <- prop.table(table_s4_lonely_binary, margin = 2) * 100
round(table_s4_lonely_pct_binary, 1)  # Round for reporting


# data analysis for continuous var

# perceived stress
# assumptions check
# Load required libraries
library(car)   # for leveneTest
library(dplyr)

#Summary stats

summary_grade_fas <- hbsc_clean %>% group_by(grade, FAS_quintGP) %>% 
  summarize(n = sum(!is.na(CohenPSS4)),
            mean = mean(CohenPSS4, na.rm = TRUE),
            sd = sd(CohenPSS4, na.rm = TRUE),
            median = median(CohenPSS4, na.rm = TRUE),
            IQR = IQR(CohenPSS4, na.rm = TRUE),
            min = min(CohenPSS4, na.rm = TRUE),
            max = max(CohenPSS4, na.rm = TRUE))
summary_grade_fas
view(summary_grade_fas)

# overall anova (not stratified)
anova_fas <- aov(CohenPSS4 ~ FAS_quintGP, data = hbsc_clean)
summary(anova_fas)

# post hoc for FAS
tukey_stress <- TukeyHSD(anova_fas)
print(tukey_stress)


install.packages("emmeans")
library(emmeans)
emmeans(anova_fas, pairwise ~ FAS_quintGP)


# Comparing Stress by Grade (2 groups) Within Each FAS Level

# For each FAS, compare grades (stratified)
for (fas in unique(hbsc_clean$FAS_quintGP)) {
  print(fas)
  subset <- hbsc_clean %>% filter(FAS_quintGP == fas)
  print(t.test(CohenPSS4 ~ grade, data = subset))
}

#overall t test
t.test(CohenPSS4 ~ grade, data = hbsc_clean)

# Family support by FAS and grade

#Summary stats

summary_grade_fam <- hbsc_clean %>% group_by(grade, FAS_quintGP) %>% 
  summarize(n = sum(!is.na(Family_support_scale)),
            mean = mean(Family_support_scale, na.rm = TRUE),
            sd = sd(Family_support_scale, na.rm = TRUE),
            median = median(Family_support_scale, na.rm = TRUE),
            IQR = IQR(Family_support_scale, na.rm = TRUE),
            min = min(Family_support_scale, na.rm = TRUE),
            max = max(Family_support_scale, na.rm = TRUE))
summary_grade_fam
view(summary_grade_fam)

# overall anova famsupport (not stratified)
anova_fam <- aov(Family_support_scale ~ FAS_quintGP, data = hbsc_clean)
summary(anova_fam)

# post hoc for FAS
tukey_fam <- TukeyHSD(anova_fam)
print(tukey_fam)

emmeans(anova_fam, pairwise ~ FAS_quintGP)

#overall t test
t.test(Family_support_scale ~ grade, data = hbsc_clean)

# Peer support by FAS and grade

#Summary stats

summary_grade_peer <- hbsc_clean %>% group_by(grade, FAS_quintGP) %>% 
  summarize(n = sum(!is.na(Peer_support_scale)),
            mean = mean(Peer_support_scale, na.rm = TRUE),
            sd = sd(Peer_support_scale, na.rm = TRUE),
            median = median(Peer_support_scale, na.rm = TRUE),
            IQR = IQR(Peer_support_scale, na.rm = TRUE),
            min = min(Peer_support_scale, na.rm = TRUE),
            max = max(Peer_support_scale, na.rm = TRUE))
summary_grade_peer

# overall anova peer support (not stratified)
anova_peer <- aov(Peer_support_scale ~ FAS_quintGP, data = hbsc_clean)
summary(anova_peer)

# post hoc for FAS
tukey_peer <- TukeyHSD(anova_peer)
print(tukey_peer)

emmeans(anova_peer, pairwise ~ FAS_quintGP)

#overall t test
t.test(Peer_support_scale ~ grade, data = hbsc_clean)

# Anxiety by FAS and grade

#Summary stats

summary_anxiety_peer <- hbsc_clean %>% group_by(grade, FAS_quintGP) %>% 
  summarize(n = sum(!is.na(gadscore)),
            mean = mean(gadscore, na.rm = TRUE),
            sd = sd(gadscore, na.rm = TRUE),
            median = median(gadscore, na.rm = TRUE),
            IQR = IQR(gadscore, na.rm = TRUE),
            min = min(gadscore, na.rm = TRUE),
            max = max(gadscore, na.rm = TRUE))
summary_anxiety_peer

# overall anova peer support (not stratified)
anova_anxiety <- aov(gadscore ~ FAS_quintGP, data = hbsc_clean)
summary(anova_anxiety)

# post hoc for FAS
tukey_anxiety <- TukeyHSD(anova_anxiety)
print(tukey_anxiety)

emmeans(anova_anxiety, pairwise ~ FAS_quintGP)

#overall t test
t.test(gadscore ~ grade, data = hbsc_clean)

# assumption checks
# 1. PERCEIVED STRESS
# Extract residuals from ANOVA model
residuals_anova <- residuals(anova_fas)

# Histogram
hist(residuals_anova, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot
qqnorm(residuals_anova)
qqline(residuals_anova)

# Shapiro-Wilk test for normality (note: sensitive for large samples)
shapiro.test(residuals_anova)

# levene test
leveneTest(CohenPSS4 ~ FAS_quintGP, data = hbsc_clean)
# > 0.05, equal variances.

# t test check

# Check normality per grade group
by(hbsc_clean$CohenPSS4, hbsc_clean$grade, function(x) {
  shapiro.test(x)
})

# Visual check per group
library(ggplot2)
ggplot(hbsc_clean, aes(sample = CohenPSS4)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~grade)

# Use var.test() to test equal variances
var.test(CohenPSS4 ~ grade, data = hbsc_clean)


# 2. FAMILY SUPPORT
# Extract residuals from ANOVA model
residuals_anova_fam <- residuals(anova_fam)

# Histogram
hist(residuals_anova_fam, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot
qqnorm(residuals_anova_fam)
qqline(residuals_anova_fam)

# Shapiro-Wilk test for normality (note: sensitive for large samples)
shapiro.test(residuals_anova_fam)

# levene test
leveneTest(residuals_anova_fam ~ FAS_quintGP, data = hbsc_clean)
# > 0.05, equal variances.

# t test check
# Check normality per grade group
by(hbsc_clean$Family_support_scale, hbsc_clean$grade, function(x) {
  shapiro.test(x)
})

# Visual check per group
library(ggplot2)
ggplot(hbsc_clean, aes(sample = Family_support_scale)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~grade)

# Use var.test() to test equal variances
var.test(Family_support_scale ~ grade, data = hbsc_clean)

# normality in fam support is clearly violated, so consider non parametric and see
# if it gives the same result

wilcox.test(Family_support_scale ~ grade, data = hbsc_clean)


# 3. PEER SUPPORT
# Extract residuals from ANOVA model
residuals_anova_peer <- residuals(anova_peer)

# Histogram
hist(residuals_anova_peer, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot
qqnorm(residuals_anova_peer)
qqline(residuals_anova_peer)

# Shapiro-Wilk test for normality (note: sensitive for large samples)
shapiro.test(residuals_anova_peer)

# levene test for variances anova
leveneTest(residuals_anova_peer ~ FAS_quintGP, data = hbsc_clean)
# > 0.05, equal variances.

# t test check
# Check normality per grade group
by(hbsc_clean$Peer_support_scale, hbsc_clean$grade, function(x) {
  shapiro.test(x)
})

# Visual check per group
library(ggplot2)
ggplot(hbsc_clean, aes(sample = Peer_support_scale)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~grade)

# Use var.test() to test equal variances
var.test(Peer_support_scale ~ grade, data = hbsc_clean)

# normality in peer support is clearly violated, so consider non parametric and see
# if it gives the same result

wilcox.test(Peer_support_scale ~ grade, data = hbsc_clean)

# 4. ANXIETY
# Extract residuals from ANOVA model
residuals_anova_peer <- residuals(anova_peer)

# Histogram
hist(residuals_anova_peer, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot
qqnorm(residuals_anova_peer)
qqline(residuals_anova_peer)

# Shapiro-Wilk test for normality (note: sensitive for large samples)
shapiro.test(residuals_anova_peer)

# levene test for variances
leveneTest(residuals_anova_peer ~ FAS_quintGP, data = hbsc_clean)
# > 0.05, equal variances.

# t test check
# Check normality per grade group
by(hbsc_clean$gadscore, hbsc_clean$grade, function(x) {
  shapiro.test(x)
})

# Visual check per group 
library(ggplot2)
ggplot(hbsc_clean, aes(sample = gadscore)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~grade)

# Use var.test() to test equal variances
var.test(gadscore ~ grade, data = hbsc_clean)


wilcox.test(gadscore ~ grade, data = hbsc_clean)


# correlation matrix

library(psych) # For the corr.test function

vars_cor <- hbsc_clean[, c("CohenPSS4", "gadscore", "Family_support_scale", "Peer_support_scale")]

# Pearson correlation matrix
correlation_matrix <- corr.test(vars_cor, use = "pairwise.complete.obs", method = "pearson")
print(correlation_matrix, short=FALSE)

# correlation plot
pairs(vars_cor) # basic
install.packages("GGally")
library(GGally)
ggpairs(vars_cor, 
        lower = list(continuous=wrap("points", size=1))) # for a more refined plot

ggpairs(vars_cor,lower = list(continuous = wrap("points", position=position_dodge(width=0.1), alpha=0.1, color="blue")))

View(vars_cor)
ggpairs(vars_cor[, c(1:4)],
        columnLabels = c("Stress","Anxiety","Family Support",
                         "Peer Support"),
        upper = list(continuous = wrap('cor', size = 4)),
        title = "Scatterplot matrix of `mtcars`")


ggpairs(vars_cor,                 # Data frame
        columns = 1:4,        # Columns
        aes(size = 0.1,  # Color by group (cat. variable)
            alpha = 0.5))     # Transparency

#heatmap
library(corrplot)
cor_mat <- cor(vars_cor, use = "pairwise.complete.obs")
corrplot(cor_mat, method = "color", addCoef.col = "black", order = "hclust", type = "full")


# UNIVARIATE MULTILEVEL LOGISTIC REGRESSION
# class_ID is the cluster variable

install.packages("Matrix")
library(Matrix)
library(lme4)

hbsc_clean$ID3

# ICC model for cluster variable ID3


names(hbsc_clean)

null_model <- glmer(problematicgame ~ 1 + (1 | ID3), data = hbsc_clean, family = binomial)
summary(null_model)

# Perceived stress
model_pss <- glmer(problematicgame ~ CohenPSS4 + (1 | ID3), 
                   data = hbsc_clean, family = binomial)
summary(model_pss)
confint(model_pss)
#OR
# Get coefficients (log odds)
fixef(model_pss)
# Convert to odds ratios
exp(fixef(model_pss))

# Anxiety
model_gad <- glmer(problematicgame ~ gadscore + (1 | ID3), 
                   data = hbsc_clean, family = binomial)
summary(model_gad)
confint(model_gad)

exp(fixef(model_gad))

# Feeling left out
model_leftout <- glmer(problematicgame ~ leftout_oftenalways + (1 | ID3), 
                       data = hbsc_clean, family = binomial)
summary(model_leftout)
confint(model_leftout)

#OR
# Get coefficients (log odds)
fixef(model_leftout)
# Convert to odds ratios
exp(fixef(model_leftout))

# Feeling lonely
model_lonely <- glmer(problematicgame ~ lonely_mostalways + (1 | ID3), 
                       data = hbsc_clean, family = binomial)
summary(model_lonely)
confint(model_lonely)
exp(fixef(model_lonely))

# Sleep difficulty
model_sleep <- glmer(problematicgame ~ sleepdiff_binary + (1 | ID3), 
                      data = hbsc_clean, family = binomial)
summary(model_sleep)
confint(model_sleep)
exp(fixef(model_sleep))


# Family Support
model_fam <- glmer(problematicgame ~ Family_support_scale + (1 | ID3), 
                     data = hbsc_clean, family = binomial)
summary(model_fam)
confint(model_fam)
exp(fixef(model_fam))

# Peer Support
model_peer <- glmer(problematicgame ~ Peer_support_scale + (1 | ID3), 
                   data = hbsc_clean, family = binomial)
summary(model_peer)
confint(model_peer)
exp(fixef(model_peer))

# gender
model_gender <- glmer(problematicgame ~ gender3grouped + (1 | ID3), 
                    data = hbsc_clean, family = binomial)
summary(model_gender)
confint(model_gender, method = "Wald")
exp(fixef(model_gender))

# FAS
model_FAS <- glmer(problematicgame ~ FAS_quintGP + (1 | ID3), 
                      data = hbsc_clean, family = binomial)
summary(model_FAS)
confint(model_FAS)
exp(fixef(model_FAS))

# Grade
model_grade <- glmer(problematicgame ~ grade + (1 | ID3), 
                   data = hbsc_clean, family = binomial)
summary(model_grade)
confint(model_grade)
exp(fixef(model_grade))


# final adjusted multivariate logistic regression

install.packages("car")
library(car)          # For VIF
install.packages("broom.mixed")
library(broom.mixed)  # For tidy model summaries
install.packages("sjPlot")
library(sjPlot)       # For model tables/plots
library(ggplot2)  

model_full <- glmer(
  problematicgame ~ CohenPSS4 + gadscore + lonely_mostalways + leftout_oftenalways +
    sleepdiff_binary + gender3grouped + Family_support_scale + Peer_support_scale +
    grade + FAS_quintGP + (1 | ID3),   # Random intercept for class/school
  data = hbsc_clean,
  family = binomial
)

summary(model_full)

coefs <- summary(model_full)$coefficients
est <- coefs[,1]
se <- coefs[,2]

# Odds ratios
or <- exp(est)

# 95% Confidence Intervals
lower <- exp(est - 1.96 * se)
upper <- exp(est + 1.96 * se)

# p-values
pval <- coefs[,4]

# Create a data.frame for results table
results <- data.frame(
  Variable = rownames(coefs),
  Estimate = round(est, 2),
  OR = round(or, 2),
  CI_lower = round(lower, 2),
  CI_upper = round(upper, 2),
  p_value = signif(pval, 2)
)

results

# model fit

base_model <- glmer(
  problematicgame ~ 1 + (1 | ID3),
  data = hbsc_clean,
  family = binomial
)

base_model

# model interaction term

# recode gender to only be binary: 0 = boy, 1 = girl; others are set to NA
hbsc_clean$gender_bin <- NA
hbsc_clean$gender_bin[hbsc_clean$genderid == "I identify myself as a boy"] <- 0
hbsc_clean$gender_bin[hbsc_clean$genderid == "I identify myself as a girl"] <- 1

# Filter to keep only boys and girls
hbsc_bg <- subset(hbsc_clean, !is.na(gender_bin))
View(hbsc_bg)
View(hbsc_clean)

unique(hbsc_clean)
levels(hbsc_bg$FAS_quintGP)
levels(hbsc_bg$grade)

# rescale/standardize continuous predictors

hbsc_bg$gadscore_z <- scale(hbsc_bg$gadscore)
hbsc_bg$Family_support_scale_z <- scale(hbsc_bg$Family_support_scale)
hbsc_bg$CohenPSS4_z <- scale(hbsc_bg$CohenPSS4)
hbsc_bg$Peer_support_scale_z <- scale(hbsc_bg$Peer_support_scale)


# 1. gender as a moderator

# gender x stress

model_int_gender_stress <- glmer(
  problematicgame ~ CohenPSS4_z * gender_bin + gadscore_z + lonely_mostalways + leftout_oftenalways + sleepdiff_binary +
    Family_support_scale_z + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)

summary(model_int_gender_stress)

# gender x anxiety
model_int_gender_anxiety <- glmer(
  problematicgame ~ gadscore_z * gender_bin + CohenPSS4_z + leftout_oftenalways + lonely_mostalways + sleepdiff_binary +
    Family_support_scale_z + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_gender_anxiety)

# gender x lonely
model_int_gender_lonely <- glmer(
  problematicgame ~ lonely_mostalways * gender_bin + CohenPSS4_z + leftout_oftenalways + Family_support_scale_z + sleepdiff_binary +
    gadscore_z + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_gender_lonely)

# gender x leftout
model_int_gender_leftout <- glmer(
  problematicgame ~ leftout_oftenalways * gender_bin + CohenPSS4_z + lonely_mostalways + Family_support_scale_z + sleepdiff_binary +
    gadscore_z + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_gender_leftout)

# gender x sleep
model_int_gender_sleep <- glmer(
  problematicgame ~ sleepdiff_binary * gender_bin + CohenPSS4_z + lonely_mostalways + Family_support_scale_z + leftout_oftenalways +
    gadscore_z + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_gender_sleep)

unique(hbsc_clean$sleepdificulty)

# stratify by gender

# Subset for boys
hbsc_boys <- subset(hbsc_bg, gender_bin == 0)

model_boys <- glmer(
  problematicgame ~ CohenPSS4_z + gadscore_z + lonely_mostalways + leftout_oftenalways + sleepdiff_binary +
    Family_support_scale_z + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_boys, family = binomial
)

# Subset for girls
hbsc_girls <- subset(hbsc_bg, gender_bin == 1)

model_girls <- glmer(
  problematicgame ~ CohenPSS4_z + gadscore_z + lonely_mostalways + leftout_oftenalways + sleepdiff_binary +
    Family_support_scale_z + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_girls, family = binomial
)

summary(model_boys)
summary(model_girls)


# recode from factors to numerical
hbsc_bg$problematicgame_bin <- as.numeric(hbsc_bg$problematicgame == "Problematic gamer")
table(hbsc_bg$problematicgame, hbsc_bg$problematicgame_bin, useNA="ifany")

table(hbsc_bg$problematicgame, useNA="ifany")
levels(hbsc_bg$problematicgame)
table(hbsc_bg$problematicgame_bin, useNA="ifany")

# 2. family support as a moderator

# family x stress

model_int_family_stress <- glmer(
  problematicgame_bin ~ CohenPSS4_z * Family_support_scale_z + gadscore_z + leftout_oftenalways + lonely_mostalways + sleepdiff_binary +
    gender_bin + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_family_stress)


install.packages("interactions")
library(interactions)
interact_plot(model_int_family_stress,
              pred = CohenPSS4_z,
              modx = Family_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)
table(hbsc_bg$problematicgame_bin, useNA = "ifany")
str(hbsc_bg$problematicgame_bin)

levels(hbsc_bg$CohenPSS4_z)


# check plots due to non significancy

library(ggplot2)

hbsc_bg$stress_group <- cut(hbsc_bg$CohenPSS4_z, breaks = quantile(hbsc_bg$CohenPSS4_z, probs = seq(0, 1, 0.33), na.rm = TRUE), include.lowest = TRUE)
hbsc_bg$family_group <- cut(hbsc_bg$Family_support_scale_z, breaks = quantile(hbsc_bg$Family_support_scale_z, probs = seq(0, 1, 0.33), na.rm = TRUE), include.lowest = TRUE)

ggplot(hbsc_bg, aes(x = stress_group, fill = as.factor(problematicgame_bin))) +
  geom_bar(position = "fill") +
  facet_wrap(~ family_group) +
  labs(y = "Proportion with IGD", fill = "Problematic Gaming") +
  theme_minimal()

library(sjPlot)
plot_model(model_int_family_stress, type = "int", terms = c("CohenPSS4_z", "Family_support_scale_z"))

# Create stress and family support tertiles
hbsc_bg$stress_group <- cut(hbsc_bg$CohenPSS4_z,
                            breaks = quantile(hbsc_bg$CohenPSS4_z, probs = seq(0, 1, 0.33), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("Low", "Medium", "High"))

hbsc_bg$family_group <- cut(hbsc_bg$Family_support_scale_z,
                            breaks = quantile(hbsc_bg$Family_support_scale_z, probs = seq(0, 1, 0.33), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("Low", "Medium", "High"))

# Cross-tab
table(hbsc_bg$stress_group, hbsc_bg$family_group)

# family x anxiety

model_int_family_anxiety <- glmer(
  problematicgame_bin ~ gadscore_z * Family_support_scale_z + CohenPSS4_z + leftout_oftenalways + lonely_mostalways + sleepdiff_binary +
    gender_bin + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_family_anxiety)

interact_plot(model_int_family_anxiety,
              pred = gadscore_z,
              modx = Family_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)

library(sjPlot)
plot_model(model_int_family_anxiety, type = "int", terms = c("gadscore_z", "Family_support_scale_z"))

hbsc_bg$anxiety_group <- cut(hbsc_bg$gadscore_z,
                             breaks = c(-Inf, -0.5, 0.5, Inf),
                             labels = c("Low", "Medium", "High"))

hbsc_bg$family_group <- cut(hbsc_bg$Family_support_scale_z,
                            breaks = c(-Inf, -0.5, 0.5, Inf),
                            labels = c("Low", "Medium", "High"))

table(hbsc_bg$anxiety_group, hbsc_bg$family_group)


# family x lonely

model_int_family_lonely <- glmer(
  problematicgame_bin ~ lonely_mostalways * Family_support_scale_z + CohenPSS4_z + leftout_oftenalways + gadscore_z + sleepdiff_binary +
    gender_bin + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_family_lonely)

interact_plot(model_int_family_lonely,
              pred = lonely_mostalways,
              modx = Family_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)

# family x leftout

model_int_family_leftout <- glmer(
  problematicgame_bin ~ leftout_oftenalways * Family_support_scale_z + CohenPSS4_z + lonely_mostalways + gadscore_z + sleepdiff_binary +
    gender_bin + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_family_leftout)

interact_plot(model_int_family_leftout,
              pred = leftout_oftenalways,
              modx = Family_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)

# family x sleep

model_int_family_sleep <- glmer(
  problematicgame_bin ~ sleepdiff_binary * Family_support_scale_z + CohenPSS4_z + lonely_mostalways + gadscore_z + leftout_oftenalways +
    gender_bin + Peer_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_family_sleep)

interact_plot(model_int_family_sleep,
              pred = sleepdiff_binary,
              modx = Family_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)

library(emmeans)
# For sleepdificulty as a factor, get estimated marginal means at +/-1SD family support
emtrends(model_int_family_sleep, var = "Family_support_scale", 
         at = list(sleepdificulty = "More than once a week")) 

library(interactions)
sim_slopes(model_int_family_sleep, 
           pred = sleepdificulty, 
           modx = Family_support_scale,
           johnson_neyman = TRUE)

# 3. peer support as a moderator

# peer x stress

model_int_peer_stress <- glmer(
  problematicgame_bin ~ CohenPSS4_z * Peer_support_scale_z + gadscore_z + leftout_oftenalways + lonely_mostalways + sleepdiff_binary +
    gender_bin + Family_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_peer_stress)

install.packages("interactions")
library(interactions)
interact_plot(model_int_peer_stress,
              pred = CohenPSS4_z,
              modx = Peer_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)


# peer x anxiety

model_int_peer_anxiety <- glmer(
  problematicgame_bin ~ gadscore_z * Peer_support_scale_z + CohenPSS4_z + leftout_oftenalways + lonely_mostalways + sleepdiff_binary +
    gender_bin + Family_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_peer_anxiety)

interact_plot(model_int_peer_anxiety,
              pred = gadscore_z,
              modx = Peer_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)

# family x lonely

model_int_peer_lonely <- glmer(
  problematicgame_bin ~ lonely_mostalways * Peer_support_scale_z + CohenPSS4_z + leftout_oftenalways + gadscore_z + sleepdiff_binary +
    gender_bin + Family_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_peer_lonely)

interact_plot(model_int_peer_lonely,
              pred = lonely_mostalways,
              modx = Peer_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)

# family x leftout

model_int_peer_leftout <- glmer(
  problematicgame_bin ~ leftout_oftenalways * Peer_support_scale_z + CohenPSS4_z + lonely_mostalways + gadscore_z + sleepdiff_binary +
    gender_bin + Family_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_peer_leftout)

interact_plot(model_int_peer_leftout,
              pred = leftout_oftenalways,
              modx = Peer_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)

# family x sleep

model_int_peer_sleep <- glmer(
  problematicgame_bin ~ sleepdiff_binary * Peer_support_scale_z + CohenPSS4_z + lonely_mostalways + gadscore_z + leftout_oftenalways +
    gender_bin + Family_support_scale_z + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_bg, family = binomial
)
summary(model_int_peer_sleep)

interact_plot(model_int_peer_sleep,
              pred = sleepdiff_binary,
              modx = Peer_support_scale_z,
              plot.points = TRUE,
              interval = TRUE)


unique(hbsc_bg$problematicgame)
unique(hbsc_bg$problematicgame)
View(hbsc)

# peer support as a moderator

model_int_peer <- glmer(
  problematicgame ~ CohenPSS4 * Peer_support_scale + gadscore + leftout + lonely + sleepdificulty +
    genderid + Family_support_scale + grade + FAS_quintGP + (1 | ID3),
  data = hbsc_clean, family = binomial
)
summary(model_int_peer)



# VIF Check 
model_vif <- glm(
  problematicgame ~ CohenPSS4 + gadscore + lonely_mostalways + leftout_oftenalways +
    sleepdiff_binary + gender3grouped + Family_support_scale + Peer_support_scale +
    grade + FAS_quintGP,
  data = hbsc_clean, family = binomial
)
vif(model_vif)

hbsc_clean$sex

# VIF check for interaction
model_vif_interaction <- glm(
  problematicgame_bin ~ CohenPSS4_z * Family_support_scale_z + gadscore_z + lonely_mostalways + leftout_oftenalways +
    sleepdiff_binary + gender_bin + Peer_support_scale_z + grade + FAS_quintGP,
  data = hbsc_bg, family = binomial
)

vif(model_vif_interaction)



# assumption checks TRIAL

library(ggplot2)

# PERCEIVED STRESS

# Visual: Histogram by group
ggplot(s2, aes(x = CohenPSS4, fill = FAS_quintGP)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~ FAS_quintGP)

# Visual: Q-Q plot for each FAS group
by(s2$CohenPSS4, s2$FAS_quintGP, function(x) {qqnorm(x); qqline(x)})

# Statistical: Shapiro-Wilk in each group
by(s2$CohenPSS4, s2$FAS_quintGP, shapiro.test)

library(car)
leveneTest(CohenPSS4 ~ FAS_quintGP, data = s2)  # For S2/FAS
leveneTest(CohenPSS4 ~ FAS_quintGP, data = s4)  # For S4/FAS

# For t-test between grades, if comparing by FAS group:
leveneTest(CohenPSS4 ~ grade, data = hbsc_clean)

kruskal.test(CohenPSS4 ~ FAS_quintGP, data = s2)

# Family support

# Visual: Histogram by group
ggplot(s2, aes(x = Family_support_scale, fill = FAS_quintGP)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~ FAS_quintGP)

# Visual: Q-Q plot for each FAS group
by(s2$Family_support_scale, s2$FAS_quintGP, function(x) {qqnorm(x); qqline(x)})

# Statistical: Shapiro-Wilk in each group
by(s2$Family_support_scale, s2$FAS_quintGP, shapiro.test)

library(car)
leveneTest(CohenPSS4 ~ FAS_quintGP, data = s2)  # For S2/FAS
leveneTest(CohenPSS4 ~ FAS_quintGP, data = s4)  # For S4/FAS

# For t-test between grades, if comparing by FAS group:
leveneTest(CohenPSS4 ~ grade, data = hbsc_clean)

kruskal.test(CohenPSS4 ~ FAS_quintGP, data = s2)

