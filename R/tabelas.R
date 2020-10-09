#' @title tabela de mortalidade
#' @name life.table
#'
#' @description criamos uma tabela de mortalidade a partir da variaveis x,nDx e nKx.
#'
#' @param x faixas etarias
#' @param nDx pessoas que morreram nos periodos estudados por faixa etaria
#' @param nKx pessoas vivas nos periodos estudados por faixa etaria
#'
#' @return retorna uma tabela de mortalidade
#'
#' @author Victor Prates
#'
#' @seealso \url{https://r-pkgs.org/whole-game.html#whole-game-document}
#'
#' @export
life.table <- function(x, nDx, nKx){
  b0=0.07
  b1=1.7
  nxx=0


  nmax <- length(x)


  nMx <- nDx/nKx

  nax <- NULL
  nax[1] <- b0 + b1 *nMx[1]
  nax[2] <- 1.5
  nax[3:(nmax-1)] <- 2.5
  if(nxx == 0)  nax[nmax] <- 1/nMx[nmax]
  else nax[nmax] <- nxx

  n <- c(1,4, rep(5, nmax - 3),999) #width of the intervals
  nqx <- (n*nMx) / (1 + (n-nax)*nMx)
  nqx[nmax] <- 1.0



  lx <- cumprod(c(1,1-nqx))

  lxpn <- lx[-1]
  ndx <- -diff(lx)

  nLx <- n*lxpn + ndx*nax
  Tx <- rev(cumsum(rev(nLx)))

  ex <- Tx/lx[1:nmax]


  lt <- cbind(x, "nax" = round(nax,4),
              "nMx" = round(nMx,4),
              "nqx" = round(nqx[1:nmax],4),
              "lx" = round(lx[1:nmax],4),
              "ndx" = round(ndx,4),
              "nLx" = round(nLx,4),
              "Tx" = round(Tx,2),
              "ex" = round(ex,2) )

  print(lt)
}
