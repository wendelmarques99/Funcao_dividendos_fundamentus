pegar_dividendos <- function(ticker){
  
  link <- glue::glue("https://www.fundamentus.com.br/proventos.php?papel={ticker}&tipo=2")
  
  tabela <- rvest::read_html(link) %>% 
    rvest::html_table() %>% 
    purrr::pluck(1) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(data_de_pagamento = ifelse(data_de_pagamento == "-", NA, data_de_pagamento)) %>% 
    dplyr::mutate(data = lubridate::dmy(data), 
                  data_de_pagamento = lubridate::dmy(data_de_pagamento), 
                  valor = gsub(pattern = ",",  replacement = ".", x = valor) %>% as.numeric())
  
  return(tabela)
}


# testando funcao
pegar_dividendos("PETR4")
