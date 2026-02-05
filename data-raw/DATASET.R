## code to prepare `DATASET` dataset goes here
compartment_defn <- tibble::tribble(~state, ~desc, ~color, ~milk_production, ~cow_state, ~cow_color,
                            'S', 'Susceptible', '#0072B2', 'full', 'Healthy', '#009E73',
                            'A', 'Asymptomatic', '#F272B2', 'full', 'Infected', '#009E73',
                            'I', 'Infectious', '#E69F00', 'reduced', 'Sick', '#E69F00',
                            'B', 'Symptomatic, not infectious', '#D55E00', 'reduced', 'Sick', '#E69F00',
                            'R', 'Recovered', '#009E73', 'full', 'Healthy', '#009E73',
                            'Ra', 'Asymptomatic Recovered', '#009E73', 'full', 'Healthy', '#009E73',
                            'D', 'Dead from disease', '#999999', 'none', 'Dead from Disease', '#999999',
                            'C', 'To Market', '#CC79A7', 'none', 'To Market', '#CC79A7',
                            'Z', 'Infected counter variable', as.character(NA), as.character(NA), as.character(NA), as.character(NA), #BB added to count all inf. for KPI
                            'M', 'Milk', as.character(NA), as.character(NA), as.character(NA), as.character(NA) ) |>
  dplyr::mutate(cow_state = forcats::fct_inorder(cow_state))
states_full <- compartment_defn |> dplyr::filter(milk_production=='full') |> dplyr::pull(state)
states_reduced <- compartment_defn |> dplyr::filter(milk_production=='reduced') |> dplyr::pull(state)
cow_states <- compartment_defn |> dplyr::pull(cow_state) |> na.omit() |> unique()




usethis::use_data(compartment_defn, overwrite = TRUE)
