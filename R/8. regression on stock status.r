library(car)
library(lmtest)

### ------------------------------------------------ ###
# -------------------- heat map ------------------ 
### ------------------------------------------------ ###

var.df <- read.csv("Data/for plot/R csv/10. regression variables.csv")

str(var.df)
var.df$type <- as.factor(var.df$type)
var.df$region <- as.factor(var.df$region)
var.df$pre.assess.mf <- as.factor(var.df$pre.assess.mf)
var.df$assess.mf <- as.factor(var.df$assess.mf)
var.df$cover.species <- as.factor(var.df$cover.species)
var.df$tool <- as.factor(var.df$tool)
var.df$pre.manage.mf <- as.factor(var.df$pre.manage.mf)
var.df$manage.mf <- as.factor(var.df$manage.mf)

# biomass proportion larger than BMSY ----

## for all fisheries----
model.B <- lm(B.msy ~ type + region + log.size + latitude + gear + D + H + J + p.assessed + p.drm.assessed + assess.mf +
                cover.species + tool + manage.mf + pub.freq + kws.freq,
             data = var.df)

model.B <- lm(B.msy ~ gear + J + p.assessed + assess.mf +
                cover.species + tool + manage.mf + pub.freq,
              data = var.df)


Anova(model.B)


## exclude the two data-limited fisheries----
var.df.short <- var.df[-c(19,20),]

### mixed fisheries considerations with y/n ----

model.B <- lm(B.msy ~ gear + J + p.assessed + pre.assess.mf +
                cover.species + tool + pre.manage.mf + pub.freq,
              data = var.df.short)

Anova(model.B)
summary(model.B)

### mixed fisheries considerations with multiple levels ----

model.B <- lm(B.msy ~ gear + J + p.assessed + assess.mf +
                cover.species + tool + manage.mf + pub.freq,
              data = var.df.short)

Anova(model.B)
summary(model.B)

#### model selection routine to remove insignificant terms ----
step(model.B)
model.B <- lm(B.msy ~ gear + J + p.assessed + assess.mf +
                  tool + manage.mf,
              data = var.df.short)
Anova(model.B)
summary(model.B)

# fishing mortality proportion smaller than FMSY ----

## for all fisheries----
model.F <- lm(F.msy ~ type + region + log.size + latitude + gear + D + H + J + p.assessed + p.drm.assessed + assess.mf +
                cover.species + tool + manage.mf + pub.freq + kws.freq,
              data = var.df)

model.F <- lm(F.msy ~ gear + J + p.assessed + assess.mf +
                cover.species + tool + manage.mf + pub.freq,
              data = var.df)


Anova(model.F)
summary(model.F)

## exclude the two data-limited fisheries----
var.df.short <- var.df[-c(19,20),]

### mixed fisheries considerations with y/n ----

model.F <- lm(F.msy ~ gear + J + p.assessed + pre.assess.mf +
                cover.species + tool + pre.manage.mf + pub.freq,
              data = var.df.short)

Anova(model.F)
summary(model.F)

### mixed fisheries considerations with multiple levels ----

model.F <- lm(F.msy ~ gear + J + p.assessed + assess.mf +
                cover.species + tool + manage.mf + pub.freq,
              data = var.df.short)

Anova(model.F)
summary(model.F)

#### model selection routine to remove insignificant terms ----
step(model.F)
model.F <- lm(B.msy ~ gear + J + p.assessed + assess.mf +
                cover.species + tool + manage.mf + pub.freq,
              data = var.df.short)
Anova(model.F)
summary(model.F)
