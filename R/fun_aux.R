## Funçoes para calcular a curva, dado uma idade e os parâmetros dos modelos
hp_curve_9 = function(x, p){
  a = p[1] ; b = p[2] ; c = p[3] ; d = p[4] ; e = p[5] ; f = p[6] ; g = p[7] ; h = p[8] ; k = p[9]
  a^((x+b)^c) + d*exp(-e*log(x/f)^2) + g*h^x / (1 + k*g*h^x)
}

hp_curve <- function(x, p){
  a = p[1]; b = p[2]; c = p[3]; d = p[4]; e = p[5]; f = p[6]; g = p[7]; h = p[8]
  a ^ ((x + b) ^ c) + d * exp(-e * (log(x) - log(f))^2) + g * h ^ x
}

## Pequena função para verificar o número de decimais necessários na escala y
decimal <- function(n){
  cont <- 0
  while(n%%10 < 1){
    n <- 10*n; cont <- cont + 1
  }
  return(cont)
}
