library(tidyverse)

pops <- read_csv(
  "https://raw.githubusercontent.com/ModelingTaskForce/covidApp/master/data/misc/states_population.csv"
)

read_compartimentos_online <- function(model = "sir"){
  dir_loc <- paste0(
    "https://raw.githubusercontent.com/ModelingTaskForce/covidApp/master/data/model_comp/compartimentos_", 
    model,
    "_estados.csv")
  return(
    read_csv(dir_loc, col_types = cols()) %>% 
      left_join( pops, by = c('state') ) %>% 
      mutate_at(vars(suscetivel:recuperado), ~ .*pop) %>% 
      mutate_if(is.numeric, round)
  )
}

estados_sir <- read_compartimentos_online("sir")
estados_sir_bv <- read_compartimentos_online("sir_bv")
estados_seir <- read_compartimentos_online("seir")
estados_seir_bv <- read_compartimentos_online("seir_bv")
estados_seiir <- read_compartimentos_online("seiir")
estados_seiir_bv <- read_compartimentos_online("seiir_bv") 

join_data_susc <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = suscetivel) %>% 
  left_join(
    estados_sir %>% dplyr::transmute(day, state, SIR = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir %>% dplyr::transmute(day, state, SEIR = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir_bv %>% dplyr::transmute(day, state, SEIR_beta_variante = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir %>% dplyr::transmute(day, state, SEIIR = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_beta_variante = suscetivel),
    by = c("day", "state")
  )

join_data_susc <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = suscetivel) %>% 
  left_join(
    estados_sir %>% dplyr::transmute(day, state, SIR = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir %>% dplyr::transmute(day, state, SEIR = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir_bv %>% dplyr::transmute(day, state, SEIR_beta_variante = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir %>% dplyr::transmute(day, state, SEIIR = suscetivel),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_beta_variante = suscetivel),
    by = c("day", "state")
  )

join_data_rec <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = recuperado) %>% 
  left_join(
    estados_sir %>% dplyr::transmute(day, state, SIR = recuperado),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir %>% dplyr::transmute(day, state, SEIR = recuperado),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir_bv %>% dplyr::transmute(day, state, SEIR_beta_variante = recuperado),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir %>% dplyr::transmute(day, state, SEIIR = recuperado),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_beta_variante = recuperado),
    by = c("day", "state")
  ) 

join_data_inf <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = infectado) %>% 
  left_join(
    estados_sir %>% dplyr::transmute(day, state, SIR = infectado),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir %>% dplyr::transmute(day, state, SEIR = infectado),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seir_bv %>% dplyr::transmute(day, state, SEIR_beta_variante = infectado),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir %>% dplyr::transmute(day, state, SEIIR_sin = infectadoS),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_sin_beta_variante = infectadoS),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir %>% dplyr::transmute(day, state, SEIIR_asin = infectadoA),
    by = c("day", "state")
  ) %>% 
  left_join(
    estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_asin_beta_variante = infectadoA),
    by = c("day", "state")
  )

write_csv(x = join_data_susc,file = "estados/data/model_comp/estados_full_join_comp_susc.csv")
write_csv(x = join_data_rec, file = "estados/data/model_comp/estados_full_join_comp_rec.csv")
write_csv(x = join_data_inf, file = "estados/data/model_comp/estados_full_join_comp_inf.csv")