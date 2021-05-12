# LMM_analysis.R

#library(lme4)
#library(lmerTest)
#library(emmeans)

## 1ST  REPORTED ANALYSIS IN THE STUDY
m1 <- lmer(Rating ~ as.numeric(Mode) + as.numeric(Instrument) + as.numeric(Type) + as.numeric(Background) + as.numeric(Mode):as.numeric(Background) + as.numeric(Instrument):as.numeric(Background) + as.numeric(Type):as.numeric(Background) + (1|PARTICIPANTID), data=m,REML = TRUE)
s <- summary(m1,cor=FALSE)

rownames(s$coefficients)<-c('Intercept','Chord','Timbre','Type','Culture','Chord x Culture','Timbre x Culture','Type x Culture')
knitr::kable(s$coefficients,digits = 3,caption = 'LMM analysis with main effects and first order interactions with Culture')

S <- m %>%
  dplyr::group_by(Instrument) %>%
  dplyr::summarise(n=n(),m=mean(Rating,na.rm = TRUE),sd=sd(Rating,na.rm = TRUE),.groups = ) %>%
  dplyr::mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se) 

##### 2ND REPORTED ANALYSIS

mf <- dplyr::filter(m,Mode=='Major' | Mode=='Minor')
mf$Mode<-factor(mf$Mode)

m1 <- lmer(Rating ~ Mode * Background + (1|PARTICIPANTID), data=mf)
#summary(m1,correlation=FALSE)

em2<-emmeans(m1, specs = ~ Mode | Background)
print(knitr::kable(em2,digits = 3,caption = 'Comparison of Major vs Minor Across Culture.'))

print(knitr::kable(pairs(em2,digits = 3,caption = 'Comparison of Major vs Minor Across Culture ( statistics).')))

#########################
# for the figure

# first p vals
m1 <- lmer(Rating ~ Type + Mode + Background + (1|PARTICIPANTID), data=m)
ph1 <- pairs(emmeans(m1, c('Mode')))
print(knitr::kable(ph1,digits = 3, caption = 'Contrasts (for Figure 1).'))

# second set of pvals, contrasts within chords between Background
m1 <- lmer(Rating ~ Mode * Background + (1|PARTICIPANTID), data=m)
outgroup_means = emmeans(m1, ~ Background | Mode)
ph2 <- contrast(outgroup_means, method = "pairwise")
print(knitr::kable(ph2,digits = 3, caption = 'Contrasts (for Figure 1).'))

# third set of pvals, contrasts within chords and Background between Type
m1 <- lmer(Rating ~ Mode * Background * Type + (1|PARTICIPANTID), data=m)
outgroup_means = emmeans(m1, ~ Type | Mode | Background)
ph3 <- contrast(outgroup_means, method = "pairwise")
print(knitr::kable(ph3,digits = 3, caption = 'Contrasts (for Figure 1).'))
