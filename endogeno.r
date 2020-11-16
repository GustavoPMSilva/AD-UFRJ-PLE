K <- 2
N <- 5
L0 <- 0
L1 <- 0
F01mi0 <- 1
F02mi0 <- 2
F11mi1 <- 1
F12mi1 <- 2
GOOD <- 0
FAKE <- 1


gerarEndogenosIniciais <- function(){
    for(i in 1:length(Usuarios)) {
	    gerarEventosEndogenos(i)
    }
}

atualizarTempos <- function(tempo){
        estado <- c(0,0,0,0)
    	for(usuario in Usuarios){
            if(usuario==0){estado[1] <- estado[1] + 1}
            else if(usuario==1){estado[2] <- estado[2] + 1}
            else if(usuario==2){estado[3] <- estado[3] + 1}
            else if(usuario==3){estado[4] <- estado[4] + 1}
        }
        estadoStr <- paste(estado,collapse='')
	    Tempos[[estadoStr]] <<- Tempos[[estadoStr]] + tempo
}

modificarTimelinesFIFO <- function(e){
    if(e[2] == GOOD){
        if(Usuarios[e[4]] == 0){Usuarios[e[4]] <<- 0}
        else if(Usuarios[e[4]] == 1){Usuarios[e[4]] <<- 2}
        else if(Usuarios[e[4]] == 2){Usuarios[e[4]] <<- 0}
        else if(Usuarios[e[4]] == 3){Usuarios[e[4]] <<- 2}
    }                                                            
    else {
        if(Usuarios[e[4]] == 0){Usuarios[e[4]] <<- 1}
        else if(Usuarios[e[4]] == 1){Usuarios[e[4]] <<- 3}
        else if(Usuarios[e[4]] == 2){Usuarios[e[4]] <<- 1}
        else if(Usuarios[e[4]] == 3){Usuarios[e[4]] <<- 3}    
        }                                              
}

modificarTimelinesRND <- function(e){
    if(e[2] == GOOD){
        if(Usuarios[e[4]]==0){Usuarios[e[4]] <<- 0}
        else if(Usuarios[e[4]]==1){Usuarios[e[4]] <<- sample(c(0,1),1,rep=FALSE)} 
        else if(Usuarios[e[4]]==2){Usuarios[e[4]] <<- sample(c(0,2),1,rep=FALSE)}  
        else if(Usuarios[e[4]]==3){Usuarios[e[4]] <<- sample(c(1,2),1,rep=FALSE)}        
    }
    else {
        if(Usuarios[e[4]]==0){Usuarios[e[4]] <<- sample(c(1,2),1,rep=FALSE)}
        else if(Usuarios[e[4]]==1){Usuarios[e[4]] <<- sample(c(1,3),1,rep=FALSE)}
        else if(Usuarios[e[4]]==2){Usuarios[e[4]] <<- sample(c(2,3),1,rep=FALSE)}
        else if(Usuarios[e[4]]==3){Usuarios[e[4]] <<- 3}    
    }
}

arrumarFilasEventos <- function(){
    FilaDeEventos <<- FilaDeEventos[order(sapply(FilaDeEventos, function(x) x[[3]]))]
}

gerarEventosEndogenos <- function(quem){
    
    ###criarEventosNovos
    if(Usuarios[quem]==0){
        evento <- c(quem, GOOD, rexp(1,F02mi0), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento

    }
    if(Usuarios[quem]==1){
        evento <- c(quem, GOOD, rexp(1,F01mi0), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento
        evento <- c(quem, FAKE, rexp(1,F11mi1), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento   
    }
    if(Usuarios[quem]==2){
        evento <- c(quem, GOOD, rexp(1,F01mi0), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento    
        evento <- c(quem, FAKE, rexp(1,F11mi1), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento       
    }
    if(Usuarios[quem]==3){
        evento <- c(quem, FAKE, rexp(1,F12mi1), gerarSample(quem))
        FilaDeEventos[[length(FilaDeEventos)+1]] <<- evento
       
    }            
}

gerarSample <- function(nquero){
    while(TRUE) {
        num <- sample(1:5,1,rep=FALSE)
        if (num != nquero) break
    }
    return(num)
}

FIM <- 1000
FIFO <- FALSE

contadorEstadoInicial <- numeric(FIM)
contadorEstadoFinal <- numeric(FIM)
guardador <- matrix(0L, ncol = FIM, nrow = 2)

for(i in 1:FIM){
    #evento = (de quem veio, good/fake, tempo, pra onde vai)
    T <- 0 
    Usuarios <- c(1,1,1,1,1)
    Tempos <- numeric(56)
    names(Tempos) <- c('5000','4100','4010','4001','1400','0410','0401',
                    '1040','0140','0041','1004','0104','0014','3200',
                    '3020','3002','2300','0320','0302','2030','0230',
                    '0032','2003','0203','0023','3110','3101','3011',
                    '1310','1301','0311','1130','1031','0131','1103',
                    '1013','0113','2210','2201','2120','2021','2102',
                    '2012','1220','0221','1202','0212','1022','0122',
                    '2111','1211','1121','1112','0500','0050','0005')

    while(Tempos[['5000']] == 0 & Tempos[['0005']] == 0){
        
        FilaDeEventos <- list()
        gerarEndogenosIniciais()
        arrumarFilasEventos()
        atualizarTempos(evento[3])

        evento <- FilaDeEventos[[1]]
        T <- T + evento[3]

        if(FIFO){modificarTimelinesFIFO(evento)}else{modificarTimelinesRND(evento)}
        
    }

    if(Tempos[['5000']] > Tempos[['0005']]){
        contadorEstadoInicial[i] <- 1
        guardador[1,i] <- T
        guardador[2,i] <- 1
        }
    else if(Tempos[['0005']] > Tempos[['5000']]){
        contadorEstadoFinal[i] <- 1
        guardador[1,i] <- T
        guardador[2,i] <- 2 
        }
}

print(mean(contadorEstadoInicial) - (1.96 * sd(contadorEstadoInicial) / sqrt(FIM)))
print(mean(contadorEstadoInicial))
print(mean(contadorEstadoInicial) + (1.96 * sd(contadorEstadoInicial) / sqrt(FIM)))
print(mean(contadorEstadoFinal) - (1.96 * sd(contadorEstadoFinal) / sqrt(FIM)))
print(mean(contadorEstadoFinal))
print(mean(contadorEstadoFinal) + (1.96 * sd(contadorEstadoFinal) / sqrt(FIM)))

guardador <- t(apply(guardador,1,sort))
somaInicial <- numeric(FIM)
somaFinal <- numeric(FIM)
for(i in 1:FIM){
    for(j in 1:i){
        if(guardador[2,j]==1){somaInicial[i] <- somaInicial[i] + 1}
        else {somaFinal[i] <- somaFinal[i] + 1}
    } 
}

plot(guardador[1,], somaFinal/FIM, type="l", xlim=c(1,30), ylim=c(0,1), main="Estado Inicial = 0500", xlab="Tempo", ylab="Probabilidade", col="blue")
lines(guardador[1,],somaInicial/FIM, col="red")
abline(h=seq(0,1,by=0.1), lwd=1, lty="dotted", col="lightgray")
abline(v=c(1:30), lwd=1, lty="dotted", col="lightgray") 
legend("topleft",
c("Estado 0005 (ALL FAKE)","Estado 5000 (ALL NON FAKE)"),
fill=c("blue","red")
)

print('fim')