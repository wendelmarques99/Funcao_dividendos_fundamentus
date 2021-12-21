library(magrittr)

pegar_dividendos <- function(ticker){
  # link do site do fundamentus  
  link <- glue::glue("https://www.fundamentus.com.br/proventos.php?papel={ticker}&tipo=2")
  # extaindo os dados
  tabela <- rvest::read_html(link) %>% 
    rvest::html_table() %>% 
    purrr::pluck(1)
  # se o papel nao tiver dividendos ficara NULL
  if (is.null(tabela)){
    tibble::tibble(
      Data  = NA, 
      valor = NA, 
      tipo = NA, 
      data_de_pagamento = NA, 
      por_quantas_acoes = NA, 
      Codigo = ticker) 
  }
  
  else{
    # se nao for NULL, trata os dados normalmente
    dividendos <- tabela %>% 
      janitor::clean_names() %>% 
      dplyr::mutate(data_de_pagamento = ifelse(data_de_pagamento == "-", NA, data_de_pagamento)) %>% 
      dplyr::mutate(Data = lubridate::dmy(data), 
                    data_de_pagamento = lubridate::dmy(data_de_pagamento), 
                    valor = gsub(pattern = ",",  replacement = ".", x = valor) %>% as.numeric(), 
                    Codigo = ticker, 
                    # o dado de JCP no fundamentus nao tira os 15% de IR. Fzemos por aqui.
                    valor = ifelse(tipo == "JRS CAP PROPRIO", valor*.85, valor)) %>% 
      dplyr::select(-c(data))
    
    return(dividendos)
  }
}

# testando
pegar_dividendos("PETR4")
