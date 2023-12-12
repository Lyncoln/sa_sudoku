set.seed(041123)

replot <- function(z, originais) {
  par(mar = c(0, 0, 0, 0), bg = "white")
  plot(0.5:9.5, 0.5:9.5, type = "n", axes = FALSE, xlab = "", ylab = "")
  segments(0.5:9.5, rep(0.5, 10), 0.5:9.5, rep(9.5, 10), col = "grey")
  segments(rep(0.5, 10), 0.5:9.5, rep(9.5, 10), 0.5:9.5, col = "grey")
  segments(c(0, 3, 6, 9) + 0.5, rep(0.5, 4), c(0, 3, 6, 9) + 0.5, rep(9.5, 4), lwd = 3)
  segments(rep(0.5, 4), c(0, 3, 6, 9) + 0.5, rep(9.5, 4), c(0, 3, 6, 9) + 0.5, lwd = 3)
  
  
  for (i in 1:9) {
    for (j in 1:9) {
      if (z[i, j]) {
        if(originais[i,j]){
          text(j, 10 - i, z[i, j], col = "#00613c", font = 2, cex = 1.8)
        }
        else{
          text(j, 10 - i, z[i, j], col = "#870a28", font = 2, cex = 1.8)
        }
      }
    }
  }
}
retorna_originais = function(z){
  coordenadas = matrix(FALSE, ncol = 9, nrow = 9)
  coordenadas = z>0
  return(coordenadas)
}
divide_matrizes <- function(matrix_9x9) {
  submatrizes <- list()
  
  for (i in 1:3) {
    for (j in 1:3) {
      submatriz <- matrix_9x9[((i - 1) * 3 + 1):(i * 3), ((j - 1) * 3 + 1):(j * 3)]
      submatrizes[[paste("Submatrix", i, j, sep = "_")]] <- submatriz
    }
  }
  
  return(submatrizes)
}
junta_matrizes <- function(submatrizes) {
  matriz_9x9 <- matrix(0, nrow = 9, ncol = 9)
  
  for (i in 1:3) {
    for (j in 1:3) {
      submatriz <- submatrizes[[paste("Submatrix", i, j, sep = "_")]]
      start_row <- ((i - 1) * 3) + 1
      start_col <- ((j - 1) * 3) + 1
      end_row <- start_row + 2
      end_col <- start_col + 2
      
      matriz_9x9[start_row:end_row, start_col:end_col] <- submatriz
    }
  }
  
  return(matriz_9x9)
}
funcao_avaliacao <- function(z){
  linha <- 0
  coluna <- 0
  submatrizes <- 0
  for(i in 1:9){
    linha <- linha + sum(unique(z[i,])>0)
    coluna <- coluna + sum(unique(z[,i])>0)
  }
  
  return(sum(linha,coluna))
}
preenche_zeros <- function(z){
  submatrizes <- divide_matrizes(z)
  numeros_possiveis = 1:9
  for(k in 1:9){
    submatriz <- submatrizes[[k]]
    for(i in 1:3){
      for(j in 1:3){
        if(submatriz[i,j]==0){
          numeros_possiveis_sub <- numeros_possiveis[!(numeros_possiveis %in% submatriz[c(submatriz)>0])]
          if(length(numeros_possiveis_sub)>1){
            submatriz[i,j] <- sample(numeros_possiveis_sub,1)
          }else{
            submatriz[i,j] <- numeros_possiveis_sub
          }
        }
      }
    }
    submatrizes[[k]] <- submatriz
  }
return(junta_matrizes(submatrizes))
}

troca <- function(z,originais){
  sub_matrizes <- divide_matrizes(z)
  sub_originais <- divide_matrizes(originais)
  
  while(TRUE){
    index <- sample(1:9,1)
    if(sum(sub_originais[[index]])<9){
      break
    }
  }
  
  novo_numero <- sample(1:9,1)
  linha_1 <- sample(1:3,1)
  coluna_1 <- sample(1:3,1)
  linha_2 <- sample(1:3,1)
  coluna_2 <- sample(1:3,1)
  while(sub_originais[[index]][linha_1,coluna_1] || sub_originais[[index]][linha_2,coluna_2]){
    linha_1 <- sample(1:3,1)
    coluna_1 <- sample(1:3,1)
    linha_2 <- sample(1:3,1)
    coluna_2 <- sample(1:3,1)
  } 
  temp <- sub_matrizes[[index]][linha_1,coluna_1] 
  sub_matrizes[[index]][linha_1,coluna_1] <- sub_matrizes[[index]][linha_2,coluna_2]
  sub_matrizes[[index]][linha_2,coluna_2] <- temp
  return(junta_matrizes(sub_matrizes))
}

aceitar <- function(estado_atual, estado_candidato, temperatura){
  f_atual <- funcao_avaliacao(estado_atual)
  f_candidato <- funcao_avaliacao(estado_candidato)
  dif <- f_candidato - f_atual

  if(dif > 0){  # Se a função candidata for melhor
    estado <- estado_candidato
  } else {
    P <- exp(dif/temperatura)  # Boltzmann
    if(runif(1) < P){
      estado <- estado_candidato
    } else {
      estado <- estado_atual
    }
  }
  return(estado)
}
 
resolve_sudoku <- function(sudoku_matrix, temperatura, max_iter){
   originais <- retorna_originais(sudoku_matrix)
   sudoku_matrix <- preenche_zeros(sudoku_matrix)
   estado <- sudoku_matrix
   metrica_melhor <- funcao_avaliacao(estado)
   iter_cont <- 0
   
   
   while(iter_cont < max_iter){
     candidato <- troca(estado, originais)
     estado <- aceitar(estado,candidato,temperatura)
     metrica_estado <- funcao_avaliacao(estado)
     iter_cont <- iter_cont + 1
     
     temperatura <- temperatura*0.99999
     if(metrica_estado >= metrica_melhor){
       metrica_melhor <- metrica_estado
       melhor_estado <- estado
       if(metrica_melhor==81*2){
         print(paste0("Sudoku resolvido em ",iter_cont," passos!"))
         break
       }
     }
     
     if(iter_cont%%1000 == 0){
       print(paste0("Melhor métrica: ",metrica_melhor," Metrica atual: ",metrica_estado," Iteração: ",iter_cont, " Temperatura ", temperatura))
     }
   }
   
   if(metrica_melhor!=81*2){
     print(paste0("Não foi possível resolver o sudoku em ", max_iter, "passos, melhor resultado para métrica obtido foi ",metrica_melhor))
   }
   
   replot(melhor_estado,originais)
}

sudoku_matrix <- sudoku::generateSudoku(51)
#vetor <- c(5,3,0,0,7,0,0,0,0,
# 6,0,0,1,9,5,0,0,0,
# 0,9,8,0,0,0,0,6,0,
# 8,0,0,0,6,0,0,0,3,
# 4,0,0,8,0,3,0,0,1,
# 7,0,0,0,2,0,0,0,6,
# 0,6,0,0,0,0,2,8,0,
# 0,0,0,4,1,9,0,0,5,
# 0,0,0,0,8,0,0,7,9)
#sudoku_matrix <- matrix(vetor, nrow = 9, byrow = TRUE)


{
  replot(sudoku_matrix,retorna_originais(sudoku_matrix))
  resolve_sudoku(sudoku_matrix, 0.5, 100000)
}
