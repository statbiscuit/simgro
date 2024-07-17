#' Calculates the rate of node initiation
#' per plant (number per plant) "dN/dt" Eq 1 Jones 1999
#' fn reduces the rade of node initiation when T is outside
#' the optimum range, and to reduce sink strength of growing
#' organs, Table 3, Jones 1991
#' @param T current temperature (degrees C), default 15
#' @param nmax maximum rate of node initiation, defaut 0.5
node_init_rate <- function(T = 15, nmax = 0.5){
    ## reduce rate of node initiation when T is outside the optimum range
    fn <- numeric(length(T))
    for(i in 1:length(fn)){
        if (T[i] > 12 & T[i] <= 28) {
            fn[i] <- 1.0 + 0.0281 * (T[i] - 28)
        } else if (T[i] > 28 & T[i] < 50) {
            fn[i] <- 1.0 - 0.0455 * (T[i] - 28)
        } else {
            fn[i] <- 0
        }
    }
    ## rate of node initiation per plant
    return(nmax*fn)
}
#' Calculates the Leaf Area Index (LAI)
#' @param n number of nodes on the mainstem, default 3
#' @param plant_density number of plants per meter sq, default 3.2
#' @param parmas a vector of parameters/coefficiens in
#' Jones' 1999 Eq 4, default c("sigma" = 0.038, "beta" = 0.169, "Nb" = 16),
#' where \code{LAI_max} is the maximum leaf area index,
#' \code{sigma} is the maximum leaf area expansion per node (coef in eq 2 of Jones 1999),
#' \code{beta} is a coeficient in Eq 2 of Jones 1999, and
#' \code{Nb} modifies the number of nodes n.
leaf_area_index <- function(plant_density = 3.2, n = 3,
                            params = c( "sigma" = 0.038, "beta" = 0.169, "Nb" = 16)){
    p1 <-  plant_density*(params[["sigma"]]/params[["beta"]])
    p2 <- log(1 + exp(params[["beta"]]*(n - params[["Nb"]])))
    return(p1*p2)
}
leaf_area_index()
#' Calculates the rate of leaf area development per node
#' Eq 4 in Jones' 1999 "dLAI/dt"
#' @param LAI Leaf Area Index, default leaf_area_index() (m2[leaf]/m2[ground])
#' @param LAI_max  is the maximum leaf area index, default 4
#' @inheritParams leaf_area_index
#' @inheritParams node_init_rate
leaf_area_development_rate <- function(LAI = leaf_area_index(), n = 3, plant_density = 3.2, LAI_max = 4,
                                  params = c( "sigma" = 0.038, "beta" = 0.169, "Nb" = 16),
                                  nir = node_init_rate()){
    res <- ifelse(LAI <= LAI_max,{
        p1 <- plant_density*params[["sigma"]]
        ## note temp func (lambda) to reduce leaf area expansion is assumed to be 1
        p2 <- (exp(params[["beta"]]*(n - params[["Nb"]])))/(1 + exp(params[["beta"]]*(n - params[["Nb"]])))
        res <- p1*p2*nir}, 0)
   
    return(res)
}
#' Calculates the net aboveground biomass growth rate "GRnet"
#' @param params a vector of parameters/coefficiens in
#' Jones' 1999 Eq 5, default
#' c("E" = 0.1717, "Q10" = 1.4, "rm" = 0.016, "D" = 2.593, "K" = 0.58, "m" = 0.1, "Qe" = 0.0645, "tau" = 0.063)
#' where \code{E} is the growth efficiency (ratio of biomass to photosynthate available),
#' \code{Q10}, and  \code{rm} are cofficients in maintinence respiration Eq,
#' \code{D}, are  \code{K} are cofficients in Eq 16 Jones 1991,
#' \code{m} is the leaf light transmission coefficient, and
#' \code{tau} os the C02 use efficiency Eq 17 Jones 1991.
#' @param W aboveground biomass accumulation, default 21.8
#' @param Wm mature fruit biomass accumulation, default = 0
#' @param C02 carbon dioxide (ppm), default 350
#' @param PPFD photosynthetic photon flux density (~light intensity), default 180
#' @inheritParams leaf_area_development_rate
biomass_growth_rate <- function(params =  c("E" = 0.717, "Q10" = 1.4, "rm" = 0.016,
                                            "D" = 2.593, "K" = 0.58, "m" = 0.1, "Qe" = 0.0645, "tau" = 0.0693),
                                W = 21.8, Wm = 0, n = 3, T = 15, C02 = 350, LAI = 0.2, PPFD  = 180){
    ##  fraction carbs partitioned to roots 
    proot <- ifelse(n >= 30, 0.07, -0.0046 * n + 0.2034)
    ## maximum leaf photosyntehstic rate, EQ 17 Jones 1991
    lfmax <- params[["tau"]]*C02
    ## reduce photosynthetic rate for temps outside optimal range
    ## Table 3, Jones 1991
    pgred <- ifelse(T < 12 | T >= 35, 0, ifelse(T < 12, 1/12*T, 1))
    ## daily integral of gross photosynthesis
    ## Eq 16 Jones 1991
    pg1 <- (params[["D"]] * lfmax * pgred / params[["K"]])
    pg21 <- (1 - params[["m"]])*lfmax + params[["Qe"]]*params[["K"]]*PPFD
    pg22 <- (1 - params[["m"]])*lfmax + params[["Qe"]]*params[["K"]]*PPFD*exp(-params[["K"]]*LAI)
    pg <- pg1*log(pg21/pg22)
    ## daily integral of maintenance respiration
    ## Eq 6 Jones 1999
    Rm <- params[["Q10"]]^((T - 20)/10) * params[["rm"]] * (W - Wm)
    ## net above ground biomass growth rate
    ## Eq 5 Jones 1999
    grnet <- params[["E"]]*(pg - Rm) *(1 - proot)
    return(grnet)
}
#' Calculates the rate of fruit dry matter
#' Eq 7 Jones 1999 "DWf/dt"
#' @param nodes_pp the number of nodes per plant when first fruit appears, default 22
#' @param params_02 a vector of parameters/coefficiens in
#' Jones' 1999 Eq 7, default c("alpha" = 0.8, "nu" = 0.35)
#' @param tcrit  mean daytime temperature above which fruit abortion start, default 24.4
#' @param tday average daytime temperature, default = 20
#' @inheritParams biomass_growth_rate
fruit_dry_matter_rate <- function( T = 15, nodes_pp = 22, bgr = biomass_growth_rate(),
                                  tcrit = 24.4, tday = 20, n = 30,
                                  params_02 = c("alpha" = 0.8, "nu" = 0.135)){
    ## net aboveground biomas growthrate (g/(m2 d)) Eq 5 Jones 1999
    grnet <- bgr
    ## overall rate of development (per day) of fruit under current T
    ## Table 3, Jones 1991
    rf <-  ifelse(T > 8 | T <= 28, 0.0017 * T - 0.0147, ifelse(T > 28, 0.032, 0))
    ## function to modify fruit under hot daytime conditions
    ## Eq 8 Jones 1999
    gtday <- ifelse(tday < tcrit, 1 - 0.154*(tday - tcrit), 0)
    ## Eq 7 Jones 1999
    res <- ifelse(n > nodes_pp, grnet*params_02[["alpha"]]*rf *(1 - exp(-params_02[["nu"]]*(n - nodes_pp)))*gtday,
                  0)
    return(res)
} 
#' Calculates the net rate of change of aboveground biomass accumulation
#' Eqs 9 and 10 Jones 1999 "dW/dt"
#' @param Vmax default 8, Jones 1999
#' @inheritParams biomass_growth_rate
#' @inheritParams node_init_rate
#' @inheritParams fruit_dry_matter_rate
above_biomass_accumulation <- function(nir = node_init_rate(),
                                       params_03 = c("p1" = 2, "rho" = 3.1),
                                       Vmax = 8, LAI_max = 4, LAI = 0.2,
                                       bgr = biomass_growth_rate(),
                                       fdr = fruit_dry_matter_rate()){
    ## net aboveground biomas growthrate (g/(m2 d)) Eq 5 Jones 1999
    p1 <- ifelse(LAI < LAI_max, 0, params_03[["p1"]])
    ## Eq 9 Jones 1999
    res01 <- bgr - (p1*params_03[["rho"]]*nir)
    ## max rate of vegetative growth per node Eq 10 Jones 1999
    res02 <- fdr + (Vmax -  p1)*params_03[["rho"]]*nir
    res <- apply(cbind(res01,res02), 1, min)
    return(res)
}
#' Calculates mature fruit biomass accumilation
#' Eq 11 Jones 1999 "dWm/dt"
#' @inheritParams above_biomass_accumulation
#' @param kappa estimated parameter as Jones 1999, default 5
#' @param Wm mature fruit, default 0
mature_fruit_accumulation <- function(n = 3, T = 15, nodes_pp = 22, kappa = 5, Wf = 0 , Wm = 0){
    ## overall rate of development (per day) of fruit under current T
    ## Table 3, Jones 1991
    rf <-  ifelse(T > 9 | T <= 28, 0.0017 * T - 0.015, ifelse(T > 28 & T < 35, 0.032, 0))
    res <- ifelse(n > nodes_pp + kappa, rf *(Wf - Wm), 0)
    return(res)
}



#' Function to calculate final tomato plant Fruit Dry Weight (g/m^2), Total Plant Weight (g/m^2),
#' and Mature Fruit Dry Weight (g/m^2) for 1) environmental variables daily temperature (degrees C), &
#' PPFD (photosynthetic photon flux density) values, and 2) student chosen values tomato species, &
#' fertilizer manure proportion.
#' @param T average daily temperature
#' @param PPFD average daily PPFD value
#' @param manure_effect A scalar multiplier for additive effect of manure (%) on
#' biomass growth rate of plant
#' @param N Initial number of nodes, default 6
#' @param LAI  Initial Leaf Area Index, default 0.006
grow <- function(T, PPFD, manure_effect, N = 6, LAI = 0.006){
    ## initial starting values
    Ws <- Wfs <- Wms <- numeric(length(T))
    W <- Wf <- Wm  <- 0
    for(j in 1:length(T)){
        dndt <-  node_init_rate(T[j])
        dlaidt <- leaf_area_development_rate(LAI = LAI,
                                             plant_density = 3.1,
                                             n = N, nir = dndt)
        bgr <- biomass_growth_rate(T = T[j], PPFD = PPFD[j],
                                   LAI = LAI , n = N, W = W, Wm = Wm)
        bgr <- manure_effect*bgr + bgr
        dWfdt <- fruit_dry_matter_rate(T = T[j], bgr = bgr, n = N)
        dWdt <- above_biomass_accumulation(LAI = LAI, bgr = bgr,
                                           nir = dndt, fdr = dWfdt)
        dWmdt <- mature_fruit_accumulation(n = N, T = T[j],
                                           Wf = Wf, Wm = Wm)
        ## Update
        N <- N + dndt
        LAI <- LAI +  dlaidt
        W <- W + dWdt
        Wf <- Wf + dWfdt
        Wm <- Wm + dWmdt
        ## output
        Ws[j] <- W
        Wfs[j] <- Wf
        Wms[j] <- Wm
    }
    n <- length(T) ## last day
    return(cbind(Ws[n], Wfs[n], Wms[n]))
}
#' Main function to simulate the tomato growth in the greenhouse
#' @param plant_type  A 7x7 character array specifying the tomato plant type, either
#' \code{"cherry"}, \code{"heirloom"}, or the "path"
#' @param percent_manure A 7x7 numeric array [0,100] specifying the percentage of manure
#' in the fertilizer recipie including the control (0).
#' @param recipes A named vector that maps the numeric percent_manure values to the
#' given recipe names for example \code{c("Control" = 0, "Recipe 1" = 10, "Recipe 2" = 30, "Recipe 3" = 50, "Recipe 4" = 80)}.
#' @param n_days the number of days to "grow" the tomatoes (default 90, ~3 months)
#' @param prob_die A vector of length two specifying the probabilities that eithe
#' one or two plants die respectively. The probability that all plants survive is
#' the complement of the sum. The default that there is a 10% chance of one plant dying and a 5% chance
#' of two plants dying.
tomgro <- function(plant_type, percent_manure, recipes, n_days = 90, prob_die = c(0.1, 0.05)){
    days <- 1:n_days
    ## manure effect function
    manure <- function(x, type = "cherry"){
        if(type == "cherry"){
            res = (3/5)*x - (1/175)*x^2 
        }else{
            if(type == "heirloom"){
                res = 4*sqrt(x) - (1/3)*x
            }
        }
        return(res)
    }
    ## hardcoded horrible nonsense
    ## Note these are the path indecies
    ## (5, 1), (5, 2), (5, 3), (4, 5), (4, 6), (4, 7)
    ## from left to right PPFD gets stronger
    PPFD_gradient <- matrix(rep(seq(140, 200, length.out = 7), each = 7), nrow = 7)
    ## from top to bottom temp gets cooler
    temp_gradient <- matrix(rep(seq(28, 21, length.out = 7), each = 7), nrow = 7, byrow = TRUE)
    ## path index
    idx <- cbind(c(5, 5, 5, 4, 4, 4,1:7), c(1:3, 5:7, rep(4,7)))
    temp_gradient[idx] <- PPFD_gradient[idx] <-  NA
    ## initiate output matricies
    outputW <- outputWf <- outputWm <- matrix(NA, nrow = 7, ncol = 7)
    for(i in 1:7){ ## rows
        for(j in 1:7){ ## cols
            if(!is.na(temp_gradient[i, j]) & !is.na(PPFD_gradient[i, j]) & !is.na(percent_manure[i, j])){
                temp_days <- rnorm(n_days, temp_gradient[i, j], 1) +
                    (cos(days)/(n_days/5))
                ppfd_days <- rnorm(n_days, PPFD_gradient[i, j], 20)
                ## plant type changes N and LAI intial values
                ## manure percentage changes biomas growth rate
                ## by modifying the C02 use efficiency
                ## which is different per plant type
                if(plant_type[i, j] == "cherry"){
                    N <- 15
                    LAI <- 0.06
                    manure_effect <- manure(percent_manure[i, j])
                }else{
                    if(plant_type[i, j] == "heirloom"){
                        N <- 5
                        LAI <- 0.09
                        manure_effect <- manure(percent_manure[i, j], "heirloom")
                    }
                }
                gr <- grow(T = temp_days, PPFD = ppfd_days, manure_effect = manure_effect, N = N, LAI = LAI)
                outputW[i, j] <- gr[1]
                outputWf[i, j] <- gr[2]
                outputWm[i, j] <- gr[3]
            }
        }
    }
    ## Output dataframe 
    output <- data.frame(row = rep(1:7, times = 7),
                         col = rep(1:7, each = 7),
                         plant = c(plant_type),
                         recipe = numeric(49), ## initalise
                         percent_manure = c(percent_manure),
                         survived = numeric(49), ## initialise
                         total_plant_weight = c(outputW),
                         fruit_dry_weight = c(outputWf),
                         mature_fruit_weight = c(outputWm))
    output <- subset(output, !is.na(output$total_plant_weight) | !is.na(output$percent_manure))
    ## randomly kill (a) plant(s) (this has absolutely nothing to do with the environmental factors)
    ## maximum of two plants can die
    ## probability of  a single plant dying is prob_die[]
    ## probability of two plants dying is prob_die[2]
    kill_plant <- sample(c("survived", "single_plant_died", "two_plants_died"),
                         size = 1, prob = c((1 - sum(prob_die)), prob_die[1], prob_die[2]))
    if(kill_plant == "single_plant_died") idx <- sample(1:36, size = 1)
    if(kill_plant == "two_plants_died") idx <- sample(1:36, size = 2)
    if(kill_plant != "survived") output[idx, 7:9] <- NA
    output$survived <- ifelse(is.na(output$total_plant_weight), "no", "yes")
    output$recipe <- names(recipes)[match(output$percent_manure, recipes)]
    return(output)
}
    
    
