
dados <- read.csv(file = '/Users/conta/OneDrive/Documentos/Analise_Exploratoria_de-_Dados_2022-1/dados.csv')

dados

xx <- split(dados, f <- dados$xx)
xx

split(dados, f = dados$yy)

jpeg("plot_xx_yy.jpeg")
plot(dados$xx,dados$yy)
#lines(0:70, 0:70, lwd = 3, col = "gray")
dev.off()