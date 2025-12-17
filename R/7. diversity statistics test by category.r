library(lmtest)

div.df <- read.csv("Data/for plot/ArcGis csv/3. diversity index.csv")

type.df <- read.csv("Data/for plot/ArcGis csv/1. fishery type.csv")
div.df <- merge(div.df, type.df)
div.df <- div.df[order(div.df$code),]

# ---- one-way ANOVA by region ---------------- 

## Margalef index D ----
region.D <- aov(D ~ region, data = div.df)
summary(region.D) # p = 0.801
bptest(region.D) # 0.1923

## Shannon-Wiener index H ----
region.H <- aov(H ~ region, data = div.df)
summary(region.H) # p = 0.877
bptest(region.H) # 0.6566

## Shannon-Wiener index J ----
region.J <- aov(J ~ region, data = div.df)
summary(region.J) # p = 0.453
bptest(region.J) # 0.5305

# ---- one-way ANOVA by type ---------------- 

## Margalef index D ----
type.D <- aov(D ~ type, data = div.df)
summary(type.D) # p = 0.168
bptest(type.D) # 0.3757

## Shannon-Wiener index H ----
type.H <- aov(H ~ type, data = div.df)
summary(type.H) # p = 0.0188
bptest(type.H) # 0.3655

## Shannon-Wiener index J ----
type.J <- aov(J ~ type, data = div.df)
summary(type.J) # p = 0.49
bptest(type.J) # 0.1522

# ---- two-way ANOVA no interaction ---------------- 

## Margalef index D ----
tw.D <- aov(D ~ type + region, data = div.df)
summary(tw.D) 
bptest(tw.D) 

## Shannon-Wiener index H ----
tw.H <- aov(H ~ type + region, data = div.df)
summary(tw.H) 
bptest(tw.H)

## Shannon-Wiener index J ----
tw.J <- aov(J ~ type+ region, data = div.df)
summary(tw.J)
bptest(tw.J)

# ---- two-way ANOVA with interaction ---------------- 

## Margalef index D ----
tw.int.D <- aov(D ~ type*region, data = div.df)
summary(tw.int.D) 
bptest(tw.int.D) # 0.03954

## Shannon-Wiener index H ----
tw.int.H <- aov(H ~ type*region, data = div.df)
summary(tw.int.H) 
bptest(tw.int.H) # 0.5969

## Shannon-Wiener index J ----
tw.int.J <- aov(J ~ type*region, data = div.df)
summary(tw.int.J) 
bptest(tw.int.J) # 0.6715

# ---- best fit model with AIC ---------------- 

library(AICcmodavg)

## Margalef index D ----
moded.set <- list(region.D, type.D, tw.D, tw.int.D)
lapply(moded.set, AIC)

## Shannon-Wiener index H ----
moded.set <- list(region.H, type.H, tw.H, tw.int.H)
lapply(moded.set, AIC)

## Shannon-Wiener index J ----
moded.set <- list(region.J, type.J, tw.J, tw.int.J)
lapply(moded.set, AIC)

