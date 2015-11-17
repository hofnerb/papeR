library("papeR")
context("label functions")

set.seed(1234)

################################################################################
## Test computation of CIs when data is part of the call
## (i.e. not only a link is passed but really the data)
################################################################################

test_that("computation of CIs when data is part of the call", {
    model_fit <- function(my_data, model_class) {
        do.call(model_class, list(y ~ x, data = my_data))
    }

    for (model_class in c("lm", "glm")) {
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

        expect_equal(psm1, psm1a)
        expect_equal(psm1, psm2)
        expect_equal(psm1, psm3)
        expect_equal(psm1, psm4)
    }
})


test_that("CIs can be hand specified", {
    model_fit <- function(my_data, model_class) {
        do.call(model_class, list(y ~ x, data = my_data))
    }

    for (model_class in c("lm", "glm")) {
        cat(model_class, "\n")
        x <- rnorm(100)
        y <- rnorm(100, mean = 2 * x)
        data <- data.frame(y = y, x = x)

        ## fit model
        mod <- do.call(model_class, list(y ~ x, data = data))
        ps <- prettify(summary(mod), confint = matrix(c(1, 2, 3, 4), ncol = 2))

        res <- data.frame(1:2, 3:4)
        colnames(res) <- c("CI (lower)", "CI (upper)")
        expect_equal(ps[,3:4], res)
    }
})

test_that("OR are included", {
    x <- rnorm(100)
    y <- rbinom(100, 1, make.link("logit")$linkinv(x * 2))
    data <- data.frame(x, y)
    mod <- glm(y ~ x, data = data, family = binomial)
    ps <- prettify(summary(mod))
    expect_true("Odds Ratio" %in% names(ps))
})
