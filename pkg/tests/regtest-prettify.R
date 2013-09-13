#####
## Tests for prettify interface

require("papeR")

set.seed(1234)

################################################################################
## Test computation of CIs when data is part of the call
## (i.e. not only a link is passed but really the data)
model_fit <- function(my_data, model_class) {
    do.call(model_class, list(y ~ x, data = my_data))
}

for (model_class in c("lm", "glm")) {  #, "coxph", "lme", "lmer")) {
    cat(model_class, "\n")
    x <- rnorm(100)
    y <- rnorm(100, mean = 2 * x)
    data <- data.frame(y = y, x = x)

    ## fit model with data argument
    mod <- do.call(model_class, list(y ~ x, data = data))
    psm1 <- prettify(summary(mod))
    rm(data)
    psm1a <- prettify(summary(mod))

    ## fit model without data argument
    mod2 <- do.call(model_class, list(y ~ x))
    psm2 <- prettify(summary(mod2))

    ## fit model in different environment
    mod3 <- model_fit(data.frame(y = y, x = x), model_class)
    psm3 <- prettify(summary(mod3))

    ## change data and compute summary
    x <- rnorm(100)
    y <- rnorm(100, mean = 2 * x)
    data <- data.frame(y = y, x = x)

    psm4 <- prettify(summary(mod))

    stopifnot(all(all.equal(psm1, psm1a),
                  all.equal(psm1, psm2),
                  all.equal(psm1, psm3),
                  all.equal(psm1, psm4)))
}

################################################################################
## Test computation of CIs when data is *not* part of the call
rm(list = ls())
model_fit2 <- function(my_data, model_class) {
    switch(model_class,
           lm = lm(y ~ x, data = my_data),
           glm = glm(y ~ x, data = my_data),
           coxph = coxph(Surv(y, event) ~ x, data = my_data),
           lme = lme(y ~ x, random = ~ 1 | group, data = my_data),
           lmer = lmer(y ~ x + (1 | group), data = my_data))
}

fit_model <- function(model_class =  c("lm", "glm", "coxph", "lme", "lmer")) {
    x <- rnorm(100)
    y <- rnorm(100, mean = 2 * x)
    data <- data.frame(y = y, x = x)

    if (model_class %in% c("lme", "lmer")) {
        group <- sample(1:2, 100, replace = TRUE)
        data$group <- group
    }

    if (model_class %in% "coxph") {
        event <- as.logical(sample(0:1, 100, replace = TRUE))
        data$event <- event
        data$y <- exp(y)
    }

    ## fit model with data argument
    mod <- switch(model_class,
                  lm = lm(y ~ x, data = data),
                  glm = glm(y ~ x, data = data),
                  coxph = coxph(Surv(y, event) ~ x, data = data),
                  lme = lme(y ~ x, random = ~ 1 | group, data = data),
                  lmer = lmer(y ~ x + (1 | group), data = data))
    psm1 <- prettify(summary(mod))
    data_tmp <- data
    rm(data)
    psm1a <- try(prettify(summary(mod)), silent = TRUE)

    ## fit model without data argument
    mod2 <- switch(model_class,
                   lm = lm(y ~ x),
                   glm = glm(y ~ x),
                   coxph = coxph(Surv(y, event) ~ x),
                   lme = lme(y ~ x, random = ~ 1 | group),
                   lmer = lmer(y ~ x + (1 | group)))
    psm2 <- prettify(summary(mod2))

    ## fit model in different environment
    mod3 <- model_fit2(data_tmp, model_class)
    psm3 <- try(prettify(summary(mod3)), silent = TRUE)

    ## change data and compute summary
    x <- rnorm(100)
    y <- rnorm(100, mean = 2 * x)
    data <- data.frame(y = y, x = x)

    if (model_class %in% c("lme", "lmer")) {
        group <- sample(1:2, 100, replace = TRUE)
        data$group <- group
    }

    if (model_class %in% "coxph") {
        event <- as.logical(sample(0:1, 100, replace = TRUE))
        data$event <- event
        data$y <- exp(y)
    }

    psm4 <- prettify(summary(mod))

    ret <- list(psm1, psm1a, psm2, psm3, psm4)
    names(ret) <- c("standard", "data removed from global environment",
                    "no data argument in call", "local environment",
                    "data in global environment changed")
    return(ret)
}

### check lm interface
(res <- fit_model("lm"))
stopifnot(all.equal(res[[1]], res[[3]], check.attributes = FALSE))
stopifnot(all.equal(res[[1]], res[[4]], check.attributes = FALSE))
## differences in CIs as different data is used
all.equal(res[[1]], res[[5]], check.attributes = FALSE)
## CI dropped. Other values equal
stopifnot(all.equal(res[[1]][, -(3:4)], res[[2]], check.attributes = FALSE))

### check glm interface
(res <- fit_model("glm"))
stopifnot(all.equal(res[[1]], res[[3]], check.attributes = FALSE))
stopifnot(all.equal(res[[1]], res[[4]], check.attributes = FALSE))
## differences in CIs as different data is used
all.equal(res[[1]], res[[5]], check.attributes = FALSE)
## CI dropped. Other values equal
stopifnot(all.equal(res[[1]][, -(3:4)], res[[2]], check.attributes = FALSE))

### check lme interface
(res <- fit_model("lme"))
stopifnot(all.equal(res[[1]], res[[3]], check.attributes = FALSE))
stopifnot(all.equal(res[[1]], res[[4]], check.attributes = FALSE))
## differences in CIs as different data is used
all.equal(res[[1]], res[[5]], check.attributes = FALSE)
## CI dropped. Other values equal
stopifnot(all.equal(res[[1]][, -(3:4)], res[[2]], check.attributes = FALSE))

### check coxph interfaces
(res <- fit_model("coxph"))
stopifnot(all.equal(res[[1]], res[[3]], check.attributes = FALSE))
stopifnot(inherits(res[[2]], "try-error"))
stopifnot(inherits(res[[4]], "try-error"))
## differences in CIs as different data is used
all.equal(res[[1]], res[[5]], check.attributes = FALSE)

### check lmer interface
(res <- fit_model("lmer"))
if (packageDescription("lme4")$Version >= 1) {
    stopifnot(all.equal(res[[1]], res[[3]], check.attributes = FALSE))
    stopifnot(inherits(res[[2]], "try-error"))
    stopifnot(inherits(res[[4]], "try-error"))
    ## differences in CIs as different data is used

    ## CURRENTLY NOT USE
    # all.equal(res[[1]], res[[5]], check.attributes = FALSE)
}
