# Add labels to categorical variables of household datasets
#' @keywords internal
add_labels_households <- function(arrw,
                                  year = parent.frame()$year,
                                  lang = 'pt'){

  # check input
  checkmate::assert_string(lang, pattern = 'pt', na.ok = TRUE)
  if (!(year %in% c(2000, 2010))) {
    cli::cli_abort("Labels for this data are only available for the years c(2000, 2010)")
  }

  # names of columns present in the data
  cols <- names(arrw) # nocov start


  # ALL YEARS ------------------------------------------------------------------

  if (year == 2010 & lang == "pt" & "V1006" %in% cols) {
  if ('V1006' %in% cols) {
    arrw <- mutate(arrw, V1006 = case_when(
      V1006 == '1' ~'Urbana',
      V1006 == '2' ~'Rural'))
  }

  if (year == 2000 & lang == "pt") {
    config <- load_label_config(dataset = "households", year = year, lang = lang)
    arrw <- apply_label_config(arrw, config)
  }

  # YEAR 2010 ------------------------------------------------------------------
    if (year == 2010 & lang == 'pt') {

      # Private vs collective household
      if ('V4001' %in% cols) {
        arrw <- mutate(arrw, V4001 = case_when(
          V4001 == '1' ~'Domic\u00edlio particular permanente ocupado',
          V4001 == '2' ~'Domic\u00edlio particular permanente ocupado sem entrevista realizada',
          V4001 == '5' ~'Domic\u00edlio particular improvisado ocupado',
          V4001 == '6' ~'Domic\u00edlio coletivo com morador'))
      }

      # household type
      if ('V4002' %in% cols) {
        arrw <- mutate(arrw, V4002 = case_when(
          V4002 == '11' ~ 'Casa',
          V4002 == '12' ~ 'Casa de vila ou em condom\u00ednio',
          V4002 == '13' ~ 'Apartamento',
          V4002 == '14' ~ 'Habita\u00e7\u00e3o em: casa de c\u00f4modos, corti\u00e7o ou cabe\u00e7a de porco',
          V4002 == '15' ~ 'Oca ou maloca ',
          V4002 == '51' ~ 'Tenda ou barraca',
          V4002 == '52' ~ 'Dentro de estabelecimento',
          V4002 == '53' ~ 'Outro (vag\u00e3o, trailer, gruta, etc)',
          V4002 == '61' ~ 'Asilo, orfanato e similares  com morador',
          V4002 == '62' ~ 'Hotel, pens\u00e3o e similares com morador',
          V4002 == '63' ~ 'Alojamento de trabalhadores com morador',
          V4002 == '64' ~ 'Penitenci\u00e1ria, pres\u00eddio ou casa de deten\u00e7\u00e3o com morador'))
      }

      # household tenure / occupancy status
      if ('V0201' %in% cols) {
        arrw <- mutate(arrw, V0201 = case_when(
          V0201 == '1' ~ 'Pr\u00f3prio de algum morador - j\u00e1 pago',
          V0201 == '2' ~ 'Pr\u00f3prio de algum morador - ainda pagando',
          V0201 == '3' ~ 'Alugado',
          V0201 == '4' ~ 'Cedido por empregador',
          V0201 == '5' ~ 'Cedido de outra forma',
          V0201 == '6' ~ 'Outra condi\u00e7\u00e3o'))
      }

      # material used to build household wall
      if ('V0202' %in% cols) {
        arrw <- mutate(arrw, V0202 = case_when(
          V0202 == '1' ~ 'Alvenaria com revestimento',
          V0202 == '2' ~ 'Alvenaria sem revestimento',
          V0202 == '3' ~ 'Madeira apropriada para constru\u00e7\u00e3o (aparelhada)',
          V0202 == '4' ~ 'Taipa revestida',
          V0202 == '5' ~ 'Taipa n\u00e3o revestida',
          V0202 == '6' ~ 'Madeira aproveitada',
          V0202 == '7' ~ 'Palha',
          V0202 == '8' ~ 'Outro material',
          V0202 == '9' ~ 'Sem parede'))
        }


      # type of sanitation connection
      if ('V0207' %in% cols) {
        arrw <- mutate(arrw, V0207 = case_when(
          V0207 == '1' ~ 'Rede geral de esgoto ou pluvial',
          V0207 == '2' ~ 'Fossa s\u00e9ptica',
          V0207 == '3' ~ 'Fossa rudimentar',
          V0207 == '4' ~ 'Vala',
          V0207 == '5' ~ 'Rio, lago ou mar',
          V0207 == '6' ~ 'Outro'))
      }

      # access to water
        if ('V0208' %in% cols) {
          arrw <- mutate(arrw, V0208 = case_when(
            V0208 == '01' ~ 'Rede geral de distribui\u00e7\u00e3o',
            V0208 == '02' ~ 'Po\u00e7o ou nascente na propriedade',
            V0208 == '03' ~ 'Po\u00e7o ou nascente fora da propriedade',
            V0208 == '04' ~ 'Carro-pipa',
            V0208 == '05' ~ '\u00c1gua da chuva armazenada em cisterna',
            V0208 == '06' ~ '\u00c1gua da chuva armazenada de outra forma',
            V0208 == '07' ~ 'Rios, a\u00e7udes, lagos e igarap\u00e9s',
            V0208 == '08' ~ 'Outra',
            V0208 == '09' ~ 'Po\u00e7o ou nascente na aldeia',
            V0208 == '10' ~ 'Po\u00e7o ou nascente fora da aldeia'))
          }

      # water connection
      if ('V0209' %in% cols) {
        arrw <- mutate(arrw, V0209 = case_when(
          V0209 == '1' ~ 'Sim, em pelo menos um c\u00f4modo',
          V0209 == '2' ~ 'Sim, s\u00f3 na propriedade ou terreno',
          V0209 == '3' ~ 'N\u00e3o'))
        }

      # waste treatment
      if ('V0210' %in% cols) {
        arrw <- mutate(arrw, V0210 = case_when(
          V0210 == '1' ~ 'Coletado diretamente por servi\u00e7o de limpeza',
          V0210 == '2' ~ 'Colocado em ca\u00e7amba de servi\u00e7o de limpeza',
          V0210 == '3' ~ 'Queimado (na propriedade)',
          V0210 == '4' ~ 'Enterrado (na propriedade)',
          V0210 == '5' ~ 'Jogado em terreno baldio ou logradouro',
          V0210 == '6' ~ 'Jogado em rio, lago ou mar',
          V0210 == '7' ~ 'Tem outro destino'))
      }

      # eletricity
      if ('V0211' %in% cols) {
        arrw <- mutate(arrw, V0211 = case_when(
          V0211 == '1' ~ 'Sim, de companhia distribuidora',
          V0211 == '2' ~ 'Sim, de outras fontes',
          V0211 == '3' ~ 'N\u00e3o existe energia el\u00e9trica'))
      }

      # eletricity meter
      if ('V0212' %in% cols) {
        arrw <- mutate(arrw, V0212 = case_when(
          V0212 == '1' ~'Sim, de uso exclusivo',
          V0212 == '2' ~'Sim, de uso comum ',
          V0212 == '3' ~'N\u00e3o tem medidor ou rel\u00f3gio'))
      }

      # shared household head
      if ('V0402' %in% cols) {
        arrw <- mutate(arrw, V0402 = case_when(
          V0402 == '1' ~ 'Apenas um morador',
          V0402 == '2' ~ 'Mais de um morador',
          V0402 == '9' ~ 'Ignorado'))
        }

      # type of domestic / family
      if ('V6600' %in% cols) {
        arrw <- mutate(arrw, V6600 = case_when(
          V6600 == '1' ~ 'Unipessoal',
          V6600 == '2' ~ 'Nuclear',
          V6600 == '3' ~ 'Estendida',
          V6600 == '4' ~ 'Composta'))
        }

      # adequate housing
      if ('V6210' %in% cols) {
        arrw <- mutate(arrw, V6210 = case_when(
          V6210 == '1' ~ 'Adequada',
          V6210 == '2' ~ 'Semi-adequada',
          V6210 == '3' ~ 'Inadequada'))
      }

      # census tract type
      if ('V1005' %in% cols) {
        arrw <- mutate(arrw, V1005 = case_when(
          V1005 == '1' ~ '\u00c1rea urbanizada',
          V1005 == '2' ~ '\u00c1rea n\u00e3o urbanizada',
          V1005 == '3' ~ '\u00c1rea urbanizada isolada',
          V1005 == '4' ~ '\u00c1rea rural de extens\u00e3o urbana',
          V1005 == '5' ~ 'Aglomerado rural (povoado)',
          V1005 == '6' ~ 'Aglomerado rural (n\u00facleo)',
          V1005 == '7' ~ 'Aglomerado rural (outros)',
          V1005 == '8' ~ '\u00c1rea rural exclusive aglomerado rural'))
      }

      ### Yes (1) or No (2) columns
      vars_sim_nao <- c('V0206', 'V0213', 'V0214', 'V0215', 'V0216', 'V0217', 'V0218',
                        'V0219', 'V0220', 'V0221', 'V0222', 'V0301', 'V0701')

      # mutate only colnames present
      vars_sim_nao_present <- vars_sim_nao[vars_sim_nao %in% cols]
      arrw <- dplyr::mutate(arrw, dplyr::across(all_of(vars_sim_nao_present),
                                                ~ if_else(.x == '1', 'Sim', 'N\u00e3o')
                                                ))
      # arrw <- mutate_at(arrw,
      #                   .vars = vars_sim_nao_present,
      #                   .funs = add_sim_nao_labels)
      ## mutate(mtcars, across(all_of(cols_to_change), fchange))

      # arrw <- add_sim_nao_labels2(arrw, column_names = vars_sim_nao)
    }

  } # nocov end

  return(arrw)
}
