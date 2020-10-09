#' @title teste hipotese utilizando a distribuição normal
#' @name normalidade
#'
#' @description utilizamos calculos para definir se uma hipotese nula deve ou não ser rejeitada,sabendo a media e variancia populacional.
#'
#' @param a amostra
#' @param mp media populacional
#' @param var variancial populacional
#' @param conf.level intervalo de confiança
#' @param tipo qual o tipo de teste que será realizado :
#' "diferente" testa se a media amostral é diferente da media populacional (realiza um teste bilateral)
#' "menor" testa se a  media amostral é menor que a media populacional (realiza um teste unilateral esquerdo)
#' "maior" testa se a  media amostral é maior que a media populacional (realiza um teste unilateral direito)
#'
#' @return retorna a medida a ser tomada de acordo com a analise
#'
#' @author Victor Prates
#'
#' @seealso \url{https://r-pkgs.org/whole-game.html#whole-game-document}
#'
#' @importFrom stats qnorm
#'
#' @export
normal<-function(a,mp,var,conf.level=0.95,tipo){
  ma<-mean(a)
  z<-(ma-mp)/sqrt(var)
  if(tipo=="diferente") {
    if((z>=qnorm((1-conf.level)/2))&(z<=qnorm((1-((1-conf.level)/2))))){print("As medias sao iguais")}
    else{print("As medias sao diferentes")}
  }
  if(tipo=="menor"){
    if(z>=qnorm((1-conf.level))){print("A media amostral nao e menor que a media populacional")}
    else{print("A media amostral e menor que a media populacional")}
  }
  if(tipo=="maior"){
    if(z<=qnorm(conf.level)){print("A media amostral nao e maior que a media populacional")}
    else{print("A media amostral e maior que a media populacional")}
  }
  if((tipo!="diferente")&(tipo!="menor")&(tipo!="maior")){errorCondition("tipo deve ser algum desses: diferente,menor,maior")}
}
