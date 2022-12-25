
#---------- Simulation
#===============================================================================

genDataMeta <- function(N = 50, n_min = 100, n_max = 2000, RR = 1.5, countries) {
    study      <- paste0("Study", 1:N)
    year       <- sample(1950:2021, N, replace = T)
    country    <- sample(countries, N, replace = T)
    
    N_obse        <- c()  # individuals who experienced childhood obesity
    N_norm        <- c()  # individuals who NEVER experienced childhood obesity
    N_hbp_obse    <- c()  # adult HBP with obesity during their childhood
    N_hbp_norm    <- c()  # adult HBP with NO obesity during their childhood
    for (i in 1:N) {
        RR            <- abs(rnorm(1, RR, RR/10))
        N_obse[i]     <- round(runif(1, n_min, n_max))
        N_norm[i]     <- round(runif(1, n_min, n_max))
        N_hbp_obse[i] <- round(runif(1, N_obse[i]*1.5/5, N_obse[i]*3.5/5))
        N_hbp_norm[i] <- round((N_hbp_obse[i]/N_obse[i]/RR)*N_norm[i])
        N_hbp_norm[i] <- ifelse(N_hbp_norm[i] < N_norm[i], N_hbp_norm[i],
                                round(runif(1, N_norm[i]*1.5/5, N_norm[i]*3.5/5)))
    }
    df <- data.frame(study, year, country, N_obse, N_hbp_obse, N_norm, N_hbp_norm)
    return(df)
}

# Apply the function to generate data
set.seed(12345)
df <- genDataMeta(N = 50, n_min = 100, n_max = 2000, RR = 1.5, 
                  countries = c("Austria", "Belgium", "France", "Germany", "Netherlands"))

glimpse(df)

saveRDS(df, "Meta_simData.RDS")


# Test

library(tidyverse)
df <- df %>% mutate(R1 = N_hbp_obse/N_obse,
              R2 = N_hbp_norm/N_norm,
              RR = R1/R2)
summary(df$R1)
summary(df$R2)
summary(df$RR)
