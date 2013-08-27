library(kernlab)
library(mvtnorm)
library(plyr)

log_sum <- function(x) {
    m <- max(x)
    r <- sum(exp(x - m))
    m + log(r)
}

# obs_x:   matrix where rows are observations, columns are dimensions
# obs_y:   matrix where rows are observations, should only be a single column
# train_x: matrix where rows are observations, columns are dimensions
# train_y: m x 1 matrix where rows are observations, should only be a single column
gp_ll <- function(obs_x, obs_y, train_x, train_y) {
    rbfkernel <- rbfdot(sigma = 0.1)
    
    sigma <- kernelMatrix(rbfkernel, train_x)@.Data
    sigma <- sigma + diag(rep(1, nrow(sigma)))
    isigma <- solve(sigma)

    k_star <- kernelMatrix(rbfkernel, obs_x, train_x)
    k_double_star <- kernelMatrix(rbfkernel, obs_x)

    obs_mean <- k_star %*% isigma %*% train_y
    obs_var <- k_double_star - k_star %*% isigma %*% t(k_star)

    dmvnorm(as.numeric(ox), obs_mean, obs_var, log = TRUE)
}

resample_group <- function(pid, data) {
    groups <- sort(unique(data$group))
    
    ox <- as.matrix(subset(data, PtID == pid)[, "year"])
    oy <- as.matrix(subset(data, PtID == pid)[, "fvc"])
    message("Prepared observed data.")

    tx <- subset(data, PtID != pid)[, c("year", "group")]
    ty <- subset(data, PtID != pid)[, c("fvc", "group")]
    message("Prepared training data.")

    lls <- vector("numeric", length(groups))

    for (i in seq_along(groups)) {
        k <- groups[i]
        message(sprintf("Computing group %d LL.", k))
        
        k_tx <- as.matrix(subset(tx, group == k)[, "year"])
        k_ty <- as.matrix(subset(ty, group == k)[, "fvc"])
        
        lls[i] <- gp_ll(ox, ox, k_tx, k_ty)
    }

    s <- log_sum(lls)
    ps <- exp(lls - s)

    list(lls=lls, ps=ps)
#    sample(groups, 1, prob = ps)
}

data <- arrange(read.csv("train.csv"), PtID, year)
data$PtID <- as.factor(data$PtID)

norm_data <- ddply(data, .(PtID), transform, fvc = fvc - mean(fvc))

k <- 2
norm_data <- ddply(norm_data, .(PtID), transform, group = sample(k, 1))

patient_ids <- sort(unique(data$PtID))
niter <- 10

for (i in seq(niter)) {
    new_data <- data
    for (id in patient_ids) {
        
    }
}
