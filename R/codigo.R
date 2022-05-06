#' Numeros por extenso
#'
#' Converte os valores numericos em seu equivalente por escrito
#' @param numero_pre e o numero a ser inserido, que pode ser decimal, separado por "."
#' @param moeda, boolean, em caso de verdaderio o valor sera considerado monetario
#' @return O numero por extenso
#' @examples
#' extenso1 <- extenso(50);
#' extenso2 <- extenso(50, TRUE);
#' @export



extenso <- function(numero_pre, moeda){

  excessoes <- data.frame(
    num = c(0,11:19),
    nome = c("zero","onze", "doze", "treze", "catorze", "quinze", "dezesseis", "dezessete", "dezoito", "dezenove"),
    stringsAsFactors = F
  )

  numeros <- data.frame(
    num = c(0,1:9),
    unidade = c(NA,"um", "dois", "tres", "quatro", "cinco", "seis", "sete", "oito", "nove"),
    dezena = c(NA,"dez", "vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"),
    centena = c(NA,"cento", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos"),
    stringsAsFactors = F
  )


    if(stringr::str_detect(numero_pre, "[.]")){


      decimal <- stringr::str_extract(numero_pre, "[^.]*$")

        if(nchar(decimal) < 2){
        decimal <- paste0(decimal, "0")
      }

      numero <- stringr::str_extract(numero_pre, ".*(?=[.])")

       }else{
      decimal <- NA
      numero <- numero_pre
    }

  tamanho_numero <- stringr::str_length(numero)

  posicao_vetor <- unlist(stringr::str_split(numero, ""))

  nomes_numeros_pre <- c()
  nome_numero<- c()
  nomes_numeros <- c()
  rodada <- tamanho_numero

nome_numeros_interno <- function(vetor_atual, excessoes, numeros){


         tamanho_numero_atual <-  length(vetor_atual)


       for (coluna in 1:tamanho_numero_atual) {

         teste_vetor <- as.numeric(paste(rev(vetor_atual), collapse = ""))
         vetor_exc <- ifelse(nchar(teste_vetor) < 3 , teste_vetor,substr(teste_vetor,2,3))
         vetor_resto <- ifelse(nchar(teste_vetor) < 3 , NA, substr(teste_vetor, 1,1))

         if(vetor_exc %in% excessoes$num){

           nome_numero[1]<- excessoes$nome[excessoes$num == vetor_exc]

           nome_numero[2]<- ifelse(is.na(vetor_resto), NA,  numeros[,c(1,4)][numeros$num == vetor_resto,2])
         }else{


           numero_vez <- vetor_atual[coluna]

           print_vez <- numeros[,c(1,coluna + 1)]

           nome_numero[coluna] <- print_vez[print_vez$num == numero_vez,2]


         }

       }
       return(nome_numero)

}

decima_functionl <- function(decimal, excessoes, numeros){

    if(is.na(decimal)){
    nome_decimal<-NA
  }else{


  tamanho_numero <- stringr::str_length(decimal)

  posicao_vetor <- unlist(stringr::str_split(stringi::stri_reverse(decimal), ""))

  nome_decimal<- nome_numeros_interno(posicao_vetor, excessoes, numeros)

  }

  return(nome_decimal)

}

nomes_numeros_pre[[1]] <- decima_functionl(decimal, excessoes, numeros)


     for (i in 1:ceiling(tamanho_numero/3)) {

       inicio <-rodada

       final <- ifelse(inicio-(2) < 1, 1, inicio-(2))

     vetor_atual <-  posicao_vetor[inicio:final]
     teste_vetor <- vetor_atual

     rodada <- final - 1

     nomes_numeros_pre[[i+1]] <- nome_numeros_interno(vetor_atual, excessoes, numeros)
     }


nomeros_concat <- function(nomes_numeros_pre){

  numero_1 <- nomes_numeros_pre[3]
  numero_2 <- nomes_numeros_pre[2]
  numero_3 <- nomes_numeros_pre[1]


      if(is.na(numero_1)){

       if(is.na(numero_2)){

         nomes_numeros <-   paste0(numero_3)

       }else if(is.na(numero_3)){

         nomes_numeros <-   paste0(numero_2)
         }else{

         nomes_numeros <- paste0(numero_2, " e ", numero_3)
       }

       }else if(is.na(numero_2)) {

         if(is.na(numero_1)){

           nomes_numeros <- paste0(numero_3)

         }else{

           nomes_numeros <- paste0(numero_1, " e ", numero_3)

         }
       }else if(is.na(numero_3)){
         if(is.na(numero_1)){

           nomes_numeros <-paste0(numero_2)

         }else{

           nomes_numeros <- paste0(numero_1, " e ", numero_2)

            }
         }else{

           nomes_numeros <- paste0(numero_1, " e ", numero_2, " e ", numero_3)

         }

  return(nomes_numeros)
}

tamanho_concatenado <- length(nomes_numeros_pre)

  for (i in (tamanho_concatenado-1):1) {

    nomes_numeros[[i]] <- nomeros_concat(nomes_numeros_pre[[i+1]])

  }

if (!is.na(decimal)){
  nomes_decimal <- nomeros_concat(nomes_numeros_pre[[1]])
}

     tabela_nomes_completos <- as.data.frame(nomes_numeros) |>
       tidyr::pivot_longer(cols = everything(), names_to = "numeros")|>
       dplyr::rename("nomes" = 1) |>
       dplyr::mutate("ID" = dplyr::row_number()) |>
       dplyr::mutate("qualificador" = dplyr::case_when(ID == 1 ~ "",
                                                       ID == 2 ~ "mil",
                                                       ID == 3 & value != "um" ~ "milhões",
                                                       ID == 3 & value == "um" ~ "milhão",
                                                       ID == 4 & value != "um" ~ "bilhões",
                                                       ID == 4 & value == "um" ~ "bilhão",
                                                       ID == 5 & value != "um" ~ "trilhões",
                                                       ID == 5 & value == "um" ~ "trilhão",
                                                       ID == 6 & value != "um" ~ "quatrilhões",
                                                       ID == 6 & value == "um" ~ "quatrilhão"))|>
       dplyr::mutate(nome_completo = paste0(value, sep = " ", qualificador))|>
       dplyr::select(nome_completo)

     resposta_final<- paste(rev(tabela_nomes_completos$nome_completo), collapse = " ")


     if(moeda == TRUE){
       if(is.na(decimal)){

       resposta_final <- paste0(resposta_final, " reais ")
       }else{

         resposta_final <- paste0(resposta_final, " reais e ", nomes_decimal, " centavos")
       }

     }else{
    if(is.na(decimal)){
      resposta_final <- paste0(resposta_final)
    }else{

  resposta_final <- paste0(resposta_final, " vírgula ", nomes_decimal)
  }

  }

  return(resposta_final)



}

extenso(1598798.45, TRUE)
