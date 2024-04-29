#' Calculates the rate of node initiation
#' per plant (number per plant) "dN/dt" Eq 1 Jones 1999
#' fn reduces the rade of node initiation when T is outside
#' the optimum range, and to reduce sink strength of growing
#' organs, Table 3, Jones 1991
#' @param T current temperature (degrees C), default 15
#' @param nmax maximum rate of node initiation, defaut 0.5
node_init_rate <- function(T = 15, nmax = 0.5){
    ## reduce rate of node initiation when T is outside the optimum range
    ## Table 3, Jones 1991
    fn = ifelse(T < 12 | T >=50, 0, ifelse(T < 28, 0.55, 1))
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
biomass_growth_rate <- function(params =  c("E" = 0.1717, "Q10" = 1.4, "rm" = 0.016,
                                            "D" = 2.593, "K" = 0.58, "m" = 0.1, "Qe" = 0.0645, "tau" = 0.063),
                                W = 21.8, Wm = 0, n = 3, T = 15, C02 = 350, LAI = 0.2, PPFD  = 180){
    ##  fraction carbs partitioned to roots 
    proot <- ifelse(n >= 30, 0.07, ifelse(n < 1, 0,
                                   ifelse(n < 12, 0.2, ifelse(n <- 21, 0.15, ifelse(n < 30, 0.1,0)))))
   
    ## maximum leaf photosyntehstic rate, EQ 17 Jones 1991
    lfmax <- params[["tau"]]*C02
    ## reduce photosynthetic rate for temps outside optimal range
    ## Table 3, Jones 1991
    pgred <- ifelse(T < 9 | T >= 35, 0, ifelse(T < 12, 0.67, 1))
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
#' @param nodes_pp the number of nodes per plant when first fruit appears, default 12
#' @param params_02 a vector of parameters/coefficiens in
#' Jones' 1999 Eq 7, default c("alpha" = 0.8, "nu" = 0.35)
#' @param tcrit  mean daytime temperature above which fruit abortion start, default 24.4
#' @param tday average daytime temperature, default = 20
#' @inheritParams biomass_growth_rate
fruit_dry_matter_rate <- function( T = 15, nodes_pp = 12, bgr = biomass_growth_rate(),
                                  tcrit = 24.4, tday = 20, n = 30,
                                  params_02 = c("alpha" = 0.8, "nu" = 0.135)){
    ## net aboveground biomas growthrate (g/(m2 d)) Eq 5 Jones 1999
    grnet <- bgr
    ## overall rate of development (per day) of fruit under current T
    ## Table 3, Jones 1991
    rf <- ifelse(T < 9 | T >=50, 0, ifelse(T <15 , 0.0053, ifelse(T < 21, 0.0103, ifelse(T < 28, 0.0203, 0.032))))
    ## function to modify fruit under hot daytime conditions
    ## Eq 8 Jones 1999
    gtday <- ifelse(tday > tcrit, 1 - 0.154*(tday - tcrit), 1)
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
    rf <- ifelse(T < 9 | T >=50, 0, ifelse(T <15 , 0.0053, ifelse(T < 21, 0.0103, ifelse(T < 28, 0.0203, 0.032))))
    res <- ifelse(n > nodes_pp + kappa, rf *(Wf - Wm), 0)
    return(res)
}



#' Function to calculate final tomato plant Fruit Dry Weight (g/m^2), Total Plant Weight (g/m^2),
#' and Mature Fruit Dry Weight (g/m^2) for 1) environmental variables daily temperature (degrees C), &
#' PPFD (photosynthetic photon flux density) values, and 2) student chosen values tomato species, &
#' fertilizer manure proportion.
#' @param T average daily temperature
#' @param PPFD average daily PPFD value
grow <- function(T, PPFD){
    nodes <- node_init_rate(T)
    lai <- leaf_area_index(n = cumsum(nodes))
    ##dLAI/dt
    dlai <- leaf_area_development_rate(LAI = lai, n = cumsum(nodes), nir = nodes)
    ## Grnet
    bgr <- biomass_growth_rate(T = T, PPFD = PPFD, LAI = cumsum(lai) , n = cumsum(nodes))
    ## Dwf/dt
    fdr <- fruit_dry_matter_rate(T = T, bgr = bgr, n = cumsum(nodes))
    ## DWdt
    aba <- above_biomass_accumulation(LAI = cumsum(lai),
                                      nir = nodes, bgr = bgr,
                                      fdr = fdr)
    ## DWmdt
    mfa <- mature_fruit_accumulation(T = T, n = cumsum(nodes), Wf = cumsum(fdr))
    n <- length(T) ## last day
    return(cbind(aba[n], fdr[n], mfa[n]))
}
