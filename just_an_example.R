

#region [Muti-cursor editing and cmd/ctrl+d] ####
#************************************************************
# Say, I want to compare coefficients from 3 different models. 
# Each model contains 7 parameters named m1,..,m7, v1,...,v7, p1,...,p7
# They're retrieved from a function named `getCoefs()`
# The final code looks like below. How would you create it?
model_1 <- "m1 + (m2 - m7 * t) * ((1 / (1 + exp((m3 - t) / m4))) - (1 / (1 + exp((m5 - t) / m6))))"
model_2 <- "v1 + (v2 - v7 * log(t)) * ((1 / (1 + exp((v3 - t) / v4))) - (1 / (1 + exp((v5 - t) / v6))))"
model_3 <- "p1 + (p2 - p7 * sin(t)) * ((1 / (1 + exp((p3 - t) / p4))) - (1 / (1 + exp((p5 - t) / p6))))"

m1 <- getCoefs("m1", "model_1")
m2 <- getCoefs("m2", "model_1")
m3 <- getCoefs("m3", "model_1")
m4 <- getCoefs("m4", "model_1")
m5 <- getCoefs("m5", "model_1")
m6 <- getCoefs("m6", "model_1")
m7 <- getCoefs("m7", "model_1")

v1 <- getCoefs("v1", "model_2")
v2 <- getCoefs("v2", "model_2")
v3 <- getCoefs("v3", "model_2")
v4 <- getCoefs("v4", "model_2")
v5 <- getCoefs("v5", "model_2")
v6 <- getCoefs("v6", "model_2")
v7 <- getCoefs("v7", "model_2")

p1 <- getCoefs("p1", "model_3")
p2 <- getCoefs("p2", "model_3")
p3 <- getCoefs("p3", "model_3")
p4 <- getCoefs("p4", "model_3")
p5 <- getCoefs("p5", "model_3")
p6 <- getCoefs("p6", "model_3")
p7 <- getCoefs("p7", "model_3")

#endregion [Muti-cursor editing]



#region [Quick selection] ####
#************************************************************
# Say, you wanna select the body of this function, but it is too long
FitBayesianMixed <- function(model_str, landsat, landsat_prior = NULL, ifplot = FALSE) {
    # convert data to jags format
    y <- landsat$all_evi
    t <- as.numeric(landsat$date - as.Date(paste0(year(landsat$date), "-01-01"))) + 1
    n <- length(y) # total num of observations
    yr <- year(landsat$date) - year(landsat$date)[1] + 1 # year id vector
    numYears <- length(unique(yr))

    wgt <- rep(1, n)
    wgt[landsat$snow == TRUE] <- 1


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Format data, inits, and model
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    model_string <- "model {
        # Likelihood
        for (i in 1:n) {
            Y[i] ~ dnorm(mu[i], tau_y)
            mu[i] <- weights[i] * (m1[yr[i]] + (m2[yr[i]] - m7[yr[i]] * t[i]) * ((1 / (1 + exp((m3[yr[i]] - t[i]) / m4[yr[i]]))) - (1 / (1 + exp((m5[yr[i]] - t[i]) / m6[yr[i]])))))
        }
    
        # Priors
        for (j in 1:N) {
            M1[j] ~ dnorm(mu_m1, tau[1])
            logit(m1[j]) <- M1[j]
            m2[j] ~ dnorm(mu_m2, tau[2])
            m3[j] ~ dnorm(mu_m3, tau[3])
            m4[j] ~ dnorm(mu_m4, tau[4])
            m5[j] ~ dnorm(mu_m5, tau[5])
            m6[j] ~ dnorm(mu_m6, tau[6])
            m7[j] ~ dgamma(0.1, 0.1 / mu_m7)
        }
    
        mu_m1 ~ dunif(0, 0.3)
        mu_m2 ~ dunif(0.5, 2)
        mu_m3 ~ dunif(0, 185)
        mu_m4 ~ dunif(1, 15)
        mu_m5 ~ dunif(185, 366)
        mu_m6 ~ dunif(1, 15)
        mu_m7 ~ dunif(0, 0.001)
    
        for (k in 1:7) {
            tau[k] ~ dgamma(0.1, 0.1)
        }
        tau_y ~ dgamma(0.1, 0.1)
    }"

    if (!is.null(landsat_prior)) {
        p_m1 <- coef(landsat5_prior$fit)["m1"]
        p_m2 <- coef(landsat5_prior$fit)["m2"]
        p_m3 <- coef(landsat5_prior$fit)["m3"]
        p_m4 <- coef(landsat5_prior$fit)["m4"]
        p_m5 <- coef(landsat5_prior$fit)["m5"]
        p_m6 <- coef(landsat5_prior$fit)["m6"]
        p_m7 <- coef(landsat5_prior$fit)["m7"]
    } else {
        p_m1 <- 0.05
        p_m2 <- 1
        p_m3 <- 120
        p_m4 <- 8
        p_m5 <- 290
        p_m6 <- 8
        p_m7 <- 0.001
    }

    data <- list(Y = y, t = t, n = n, yr = yr, N = numYears, weights = wgt)

    inits <- list(
        m2 = rep(p_m2, numYears), m3 = rep(p_m3, numYears),
        m4 = rep(p_m4, numYears), m5 = rep(p_m5, numYears), m6 = rep(p_m6, numYears)
    )

    model <- jags.model(textConnection(model_string), data = data, inits = inits, n.chains = 2, quiet = TRUE)
    update(model, 2000, progress.bar = "none")
    samp <- coda.samples(model,
        variable.names = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
        n.iter = 2000,
        thin = 10,
        progress.bar = "none"
    )

    # plot(samp)

    m1 <- m2 <- m3 <- m4 <- m5 <- m6 <- m7 <- NULL
    for (i in 1:numYears) {
        m1 <- cbind(m1, c(samp[[1]][, paste0("m1", "[", i, "]")], samp[[2]][, paste0("m1", "[", i, "]")]))
        m2 <- cbind(m2, c(samp[[1]][, paste0("m2", "[", i, "]")], samp[[2]][, paste0("m2", "[", i, "]")]))
        m3 <- cbind(m3, c(samp[[1]][, paste0("m3", "[", i, "]")], samp[[2]][, paste0("m3", "[", i, "]")]))
        m4 <- cbind(m4, c(samp[[1]][, paste0("m4", "[", i, "]")], samp[[2]][, paste0("m4", "[", i, "]")]))
        m5 <- cbind(m5, c(samp[[1]][, paste0("m5", "[", i, "]")], samp[[2]][, paste0("m5", "[", i, "]")]))
        m6 <- cbind(m6, c(samp[[1]][, paste0("m6", "[", i, "]")], samp[[2]][, paste0("m6", "[", i, "]")]))
        m7 <- cbind(m7, c(samp[[1]][, paste0("m7", "[", i, "]")], samp[[2]][, paste0("m7", "[", i, "]")]))
    }

    m1_quan <- data.table(apply(m1, 2, quantile, c(0.05, 0.5, 0.95)))
    m2_quan <- data.table(apply(m2, 2, quantile, c(0.05, 0.5, 0.95)))
    m3_quan <- data.table(apply(m3, 2, quantile, c(0.05, 0.5, 0.95)))
    m5_quan <- data.table(apply(m5, 2, quantile, c(0.05, 0.5, 0.95)))
    # browser()
    years <- sort(unique(year(landsat$date)))
    bf_phenos <- NULL
    for (i in 1:numYears) {
        if (m2_quan[2, ][[i]] > 0.4) {
            bf_phenos <- rbind(bf_phenos, list(
                Id = NA, Year = years[i],
                midgup_lower = m3_quan[1, ][[i]], midgup = m3_quan[2, ][[i]], midgup_upper = m3_quan[3, ][[i]],
                midgdown_lower = m5_quan[1, ][[i]], midgdown = m5_quan[2, ][[i]], midgdown_upper = m5_quan[3, ][[i]]
            ))
        } else {
            bf_phenos <- rbind(bf_phenos, list(
                Id = NA, Year = years[i],
                midgup_lower = NA, midgup = NA, midgup_upper = NA,
                midgdown_lower = NA, midgdown = NA, midgdown_upper = NA
            ))
        }
    }

    if (ifplot == TRUE) { # fig: Bayesian Mixed model fit
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Predict fitted value for full dates
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        years <- sort(unique(year(landsat$date)))
        bf_pred <- NULL
        for (i in 1:numYears) { # i = 1
            date <- seq(as.Date(paste0(years[i], "-01-01")), as.Date(paste0(years[i], "-12-31")), by = "day")
            bf_params <- data.table(m1 = m1[, i], m2 = m2[, i], m3 = m3[, i], m4 = m4[, i], m5 = m5[, i], m6 = m6[, i], m7 = m7[, i])
            phenos_idx <- data.table(midgup = numeric(nrow(bf_params)), midgdown = numeric(nrow(bf_params)))

            predCI <- NULL
            for (j in 1:nrow(bf_params)) { # j = 1
                # pred based on current parameter samples
                pred <- eval(str2expression(model_str), envir = list(
                    m1 = as.numeric(bf_params[j, 1]), m2 = as.numeric(bf_params[j, 2]), m3 = as.numeric(bf_params[j, 3]), m4 = as.numeric(bf_params[j, 4]),
                    m5 = as.numeric(bf_params[j, 5]), m6 = as.numeric(bf_params[j, 6]), m7 = as.numeric(bf_params[j, 7]),
                    t = 1:length(date)
                ))
                predCI <- cbind(predCI, pred)
            }

            predCI <- t(data.table(apply(predCI, 1, function(x) quantile(x, c(0.025, 0.975)))))

            pred <- data.table(apply(predCI, 1, function(x) quantile(x, 0.5)))
            cur_year_pred <- cbind(date, pred)
            bf_pred <- rbind(bf_pred, cbind(cur_year_pred, predCI))
        }

        bf_pred <- as.data.table(bf_pred)
        colnames(bf_pred) <- c("Date", "Fitted", "Fitted_lower", "Fitted_upper")
        bf_pred$Date <- as.Date(bf_pred$Date, origin = "1970-01-01")

        # bf_phenos <- apply(bf_phenos, 2, unlist)
        bf_phenos <- data.table(bf_phenos)
        plot(bf_pred[year(Date) == 1984]$Date, bf_pred[year(Date) == 1984]$Fitted, cex = 0, ylim = c(-0.1, 1.2), xlab = "Date", ylab = "EVI2")
        polygon(c(bf_pred$Date, rev(bf_pred$Date)), c(bf_pred$Fitted_upper, rev(bf_pred$Fitted_lower)),
            col = Transparent("red", 0.2),
            border = NA
        )
        points(landsat[, .(date, all_evi)], pch = 16, cex = 0.5)
        points(landsat[snow == TRUE, .(date, all_evi)], pch = 16, col = "grey", cex = 0.5)

        lines(bf_pred$Date, bf_pred$Fitted, type = "l", ylim = c(0, 1), col = "red", lwd = 2)

        pheno_names <- c("midgup", "midgdown")
        pheno_colors <- rev(viridis(9))
        for (k in 1:length(pheno_names)) { # k = 1
            pheno <- pheno_names[k]
            phn_dates <- bf_phenos[!is.na(get(pheno)), ][[pheno]]
            phn_dates <- as.Date(paste0(years, "-01-01")) + unlist(phn_dates)

            phn_val <- bf_pred[Date %in% as.Date(as.character(phn_dates)), Fitted]

            points(phn_dates, phn_val, pch = 16, col = pheno_colors[k])
            phn_dates_lower <- as.Date(paste0(years, "-01-01")) + unlist(bf_phenos[!is.na(get(pheno)), ][[paste0(pheno, "_lower")]])
            phn_dates_upper <- as.Date(paste0(years, "-01-01")) + unlist(bf_phenos[!is.na(get(pheno)), ][[paste0(pheno, "_upper")]])
            segments(phn_dates_lower, phn_val, phn_dates_upper, phn_val)
        }
        legend("top",
            ncol = 6, lty = c(NA, NA, 1, rep(NA, 8), 1), pch = c(16, 16, NA, 15, rep(16, 7), NA),
            # fill = c(NA, NA, NA, Transparent("red", 0.2), rep(NA, 8)), border = NA,
            col = c("black", "grey", "red", Transparent("red", 0.2), pheno_colors[1:7], "black"),
            bty = "n",
            legend = c("Landsat", "snow filled", "Median Fit", "95% C.I. of fit", "Greenup", "Mid-Greenup", "Maturity", "Peak", "Senescence", "Mid-Greendown", "Dormancy", "95% C.I. of phenometrics")
        )
    }

    # convert data to jags format
    y <- landsat$all_evi
    t <- as.numeric(landsat$date - as.Date(paste0(year(landsat$date), "-01-01"))) + 1
    n <- length(y) # total num of observations
    yr <- year(landsat$date) - year(landsat$date)[1] + 1 # year id vector
    numYears <- length(unique(yr))
    
    wgt <- rep(1, n)
    wgt[landsat$snow == TRUE] <- 1
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Format data, inits, and model
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    model_string <- "model {
            # Likelihood
            for (i in 1:n) {
                Y[i] ~ dnorm(mu[i], tau_y)
                mu[i] <- weights[i] * (m1[yr[i]] + (m2[yr[i]] - m7[yr[i]] * t[i]) * ((1 / (1 + exp((m3[yr[i]] - t[i]) / m4[yr[i]]))) - (1 / (1 + exp((m5[yr[i]] - t[i]) / m6[yr[i]])))))
            }
        
            # Priors
            for (j in 1:N) {
                M1[j] ~ dnorm(mu_m1, tau[1])
                logit(m1[j]) <- M1[j]
                m2[j] ~ dnorm(mu_m2, tau[2])
                m3[j] ~ dnorm(mu_m3, tau[3])
                m4[j] ~ dnorm(mu_m4, tau[4])
                m5[j] ~ dnorm(mu_m5, tau[5])
                m6[j] ~ dnorm(mu_m6, tau[6])
                m7[j] ~ dgamma(0.1, 0.1 / mu_m7)
            }
        
            mu_m1 ~ dunif(0, 0.3)
            mu_m2 ~ dunif(0.5, 2)
            mu_m3 ~ dunif(0, 185)
            mu_m4 ~ dunif(1, 15)
            mu_m5 ~ dunif(185, 366)
            mu_m6 ~ dunif(1, 15)
            mu_m7 ~ dunif(0, 0.001)
        
            for (k in 1:7) {
                tau[k] ~ dgamma(0.1, 0.1)
            }
            tau_y ~ dgamma(0.1, 0.1)
        }"
    
    if (!is.null(landsat_prior)) {
        p_m1 <- coef(landsat5_prior$fit)["m1"]
        p_m2 <- coef(landsat5_prior$fit)["m2"]
        p_m3 <- coef(landsat5_prior$fit)["m3"]
        p_m4 <- coef(landsat5_prior$fit)["m4"]
        p_m5 <- coef(landsat5_prior$fit)["m5"]
        p_m6 <- coef(landsat5_prior$fit)["m6"]
        p_m7 <- coef(landsat5_prior$fit)["m7"]
    } else {
        p_m1 <- 0.05
        p_m2 <- 1
        p_m3 <- 120
        p_m4 <- 8
        p_m5 <- 290
        p_m6 <- 8
        p_m7 <- 0.001
    }
    
    data <- list(Y = y, t = t, n = n, yr = yr, N = numYears, weights = wgt)
    
    inits <- list(
        m2 = rep(p_m2, numYears), m3 = rep(p_m3, numYears),
        m4 = rep(p_m4, numYears), m5 = rep(p_m5, numYears), m6 = rep(p_m6, numYears)
    )
    
    model <- jags.model(textConnection(model_string), data = data, inits = inits, n.chains = 2, quiet = TRUE)
    update(model, 2000, progress.bar = "none")
    samp <- coda.samples(model,
        variable.names = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
        n.iter = 2000,
        thin = 10,
        progress.bar = "none"
    )
    
    # plot(samp)
    
    m1 <- m2 <- m3 <- m4 <- m5 <- m6 <- m7 <- NULL
    for (i in 1:numYears) {
        m1 <- cbind(m1, c(samp[[1]][, paste0("m1", "[", i, "]")], samp[[2]][, paste0("m1", "[", i, "]")]))
        m2 <- cbind(m2, c(samp[[1]][, paste0("m2", "[", i, "]")], samp[[2]][, paste0("m2", "[", i, "]")]))
        m3 <- cbind(m3, c(samp[[1]][, paste0("m3", "[", i, "]")], samp[[2]][, paste0("m3", "[", i, "]")]))
        m4 <- cbind(m4, c(samp[[1]][, paste0("m4", "[", i, "]")], samp[[2]][, paste0("m4", "[", i, "]")]))
        m5 <- cbind(m5, c(samp[[1]][, paste0("m5", "[", i, "]")], samp[[2]][, paste0("m5", "[", i, "]")]))
        m6 <- cbind(m6, c(samp[[1]][, paste0("m6", "[", i, "]")], samp[[2]][, paste0("m6", "[", i, "]")]))
        m7 <- cbind(m7, c(samp[[1]][, paste0("m7", "[", i, "]")], samp[[2]][, paste0("m7", "[", i, "]")]))
    }
    
    m1_quan <- data.table(apply(m1, 2, quantile, c(0.05, 0.5, 0.95)))
    m2_quan <- data.table(apply(m2, 2, quantile, c(0.05, 0.5, 0.95)))
    m3_quan <- data.table(apply(m3, 2, quantile, c(0.05, 0.5, 0.95)))
    m5_quan <- data.table(apply(m5, 2, quantile, c(0.05, 0.5, 0.95)))
    # browser()
    years <- sort(unique(year(landsat$date)))
    bf_phenos <- NULL
    for (i in 1:numYears) {
        if (m2_quan[2, ][[i]] > 0.4) {
            bf_phenos <- rbind(bf_phenos, list(
                Id = NA, Year = years[i],
                midgup_lower = m3_quan[1, ][[i]], midgup = m3_quan[2, ][[i]], midgup_upper = m3_quan[3, ][[i]],
                midgdown_lower = m5_quan[1, ][[i]], midgdown = m5_quan[2, ][[i]], midgdown_upper = m5_quan[3, ][[i]]
            ))
        } else {
            bf_phenos <- rbind(bf_phenos, list(
                Id = NA, Year = years[i],
                midgup_lower = NA, midgup = NA, midgup_upper = NA,
                midgdown_lower = NA, midgdown = NA, midgdown_upper = NA
            ))
        }
    }
    
    if (ifplot == TRUE) { # fig: Bayesian Mixed model fit
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Predict fitted value for full dates
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        years <- sort(unique(year(landsat$date)))
        bf_pred <- NULL
        for (i in 1:numYears) { # i = 1
            date <- seq(as.Date(paste0(years[i], "-01-01")), as.Date(paste0(years[i], "-12-31")), by = "day")
            bf_params <- data.table(m1 = m1[, i], m2 = m2[, i], m3 = m3[, i], m4 = m4[, i], m5 = m5[, i], m6 = m6[, i], m7 = m7[, i])
            phenos_idx <- data.table(midgup = numeric(nrow(bf_params)), midgdown = numeric(nrow(bf_params)))
    
            predCI <- NULL
            for (j in 1:nrow(bf_params)) { # j = 1
                # pred based on current parameter samples
                pred <- eval(str2expression(model_str), envir = list(
                    m1 = as.numeric(bf_params[j, 1]), m2 = as.numeric(bf_params[j, 2]), m3 = as.numeric(bf_params[j, 3]), m4 = as.numeric(bf_params[j, 4]),
                    m5 = as.numeric(bf_params[j, 5]), m6 = as.numeric(bf_params[j, 6]), m7 = as.numeric(bf_params[j, 7]),
                    t = 1:length(date)
                ))
                predCI <- cbind(predCI, pred)
            }
    
            predCI <- t(data.table(apply(predCI, 1, function(x) quantile(x, c(0.025, 0.975)))))
    
            pred <- data.table(apply(predCI, 1, function(x) quantile(x, 0.5)))
            cur_year_pred <- cbind(date, pred)
            bf_pred <- rbind(bf_pred, cbind(cur_year_pred, predCI))
        }
    
        bf_pred <- as.data.table(bf_pred)
        colnames(bf_pred) <- c("Date", "Fitted", "Fitted_lower", "Fitted_upper")
        bf_pred$Date <- as.Date(bf_pred$Date, origin = "1970-01-01")
    
        # bf_phenos <- apply(bf_phenos, 2, unlist)
        bf_phenos <- data.table(bf_phenos)
        plot(bf_pred[year(Date) == 1984]$Date, bf_pred[year(Date) == 1984]$Fitted, cex = 0, ylim = c(-0.1, 1.2), xlab = "Date", ylab = "EVI2")
        polygon(c(bf_pred$Date, rev(bf_pred$Date)), c(bf_pred$Fitted_upper, rev(bf_pred$Fitted_lower)),
            col = Transparent("red", 0.2),
            border = NA
        )
        points(landsat[, .(date, all_evi)], pch = 16, cex = 0.5)
        points(landsat[snow == TRUE, .(date, all_evi)], pch = 16, col = "grey", cex = 0.5)
    
        lines(bf_pred$Date, bf_pred$Fitted, type = "l", ylim = c(0, 1), col = "red", lwd = 2)
    
        pheno_names <- c("midgup", "midgdown")
        pheno_colors <- rev(viridis(9))
        for (k in 1:length(pheno_names)) { # k = 1
            pheno <- pheno_names[k]
            phn_dates <- bf_phenos[!is.na(get(pheno)), ][[pheno]]
            phn_dates <- as.Date(paste0(years, "-01-01")) + unlist(phn_dates)
    
            phn_val <- bf_pred[Date %in% as.Date(as.character(phn_dates)), Fitted]
    
            points(phn_dates, phn_val, pch = 16, col = pheno_colors[k])
            phn_dates_lower <- as.Date(paste0(years, "-01-01")) + unlist(bf_phenos[!is.na(get(pheno)), ][[paste0(pheno, "_lower")]])
            phn_dates_upper <- as.Date(paste0(years, "-01-01")) + unlist(bf_phenos[!is.na(get(pheno)), ][[paste0(pheno, "_upper")]])
            segments(phn_dates_lower, phn_val, phn_dates_upper, phn_val)
        }
        legend("top",
            ncol = 6, lty = c(NA, NA, 1, rep(NA, 8), 1), pch = c(16, 16, NA, 15, rep(16, 7), NA),
            # fill = c(NA, NA, NA, Transparent("red", 0.2), rep(NA, 8)), border = NA,
            col = c("black", "grey", "red", Transparent("red", 0.2), pheno_colors[1:7], "black"),
            bty = "n",
            legend = c("Landsat", "snow filled", "Median Fit", "95% C.I. of fit", "Greenup", "Mid-Greenup", "Maturity", "Peak", "Senescence", "Mid-Greendown", "Dormancy", "95% C.I. of phenometrics")
        )
    }

    return(list(fitted = NA, phenos = bf_phenos))
}


# Another example. What about selecting some content in between a paticular pair of parentheses
mu[i] <- weights[i] * (m1[yr[i]] + (m2[yr[i]] - m7[yr[i]] * t[i]) * ((1 / 
    (1 + exp((m3[yr[i]] - t[i]) / m4[yr[i]]))) - (1 / (1 + exp((m5[yr[i]] - t[i]) / m6[yr[i]])))))


#endregion [Quick selection]



# Color pick ####
#************************************************************
# How do you choose colors? Color names? Hex code?
color <- ""



# Look up documentation ####
#************************************************************
# What would you do if you want to check the doc of a function?
boxplot()



# Quick jump ####
#************************************************************
# 1. Go to definition
# 2. Peek definition
# 3. Back & Forth

FitBayesianMixed()



# Keybindings ####
#************************************************************
# 1. Open Keyboard Shortcuts
# 2. Open Keyboard Shortcuts (JSON)
#    - define your own R command shortcuts



# Snippets & gists ####
#************************************************************
source('https://gist.github.com/mrjgao/7bd6f978771746fd27a999c9a5c808c6/raw')



# Terminal (cmd/ctrl+j or cmd/ctrl+`)####
#************************************************************
# You can have mutiple terminals
# Customize keyboard shortcuts can allow you jump through editor and terminal



# Remote SSH
#************************************************************
# 1. HPC
# login
ssh -Y xgao26@login.hpc.ncsu.edu
# request a debug node
bsub -Is -n 8 -x -W 60 tcsh

# run code
R
library(car)
mtcars

# However, never do intensive computing on debug node, submit jobs to computing nodes instead.
# Read HPC documentation


# 2. Easy to connect servers
# The Remote SSH extension













