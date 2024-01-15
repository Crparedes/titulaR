if (FALSE) {
(Out10 <- ggplot(data = data.frame(x = c(rep(3.6, 20), seedNorm(500, 6, 0.5)), y = c(rep('o', 20), rep('i', 500))),
                 aes(x = x, fill = y)) + 
  geom_histogram(binwidth = 0.2, colour = "white") + 
  scale_fill_manual(values=c('#146175', '#c44f4f')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"), legend.position = "none") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 90), breaks = NULL) + labs(x = '', y = ''))

(Out01 <- ggplot(data = data.frame(x = c(rep(8.4, 20), seedNorm(500, 6, 0.5)), y = c(rep('o', 20), rep('i', 500))), 
                aes(x = x, fill = y)) + 
  geom_histogram(binwidth = 0.2, colour = "white") + 
  scale_fill_manual(values=c('#146175', '#c44f4f')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"), legend.position = "none") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 90), breaks = NULL) + labs(x = '', y = ''))

(Out11 <- ggplot(data = data.frame(x = c(rep(4, 20), rep(8, 20), seedNorm(500, 6, 0.5, 2)), y = c(rep('o', 40), rep('i', 500))),
                aes(x = x, fill = y)) + 
    geom_histogram(binwidth = 0.2, colour = "white") + 
    scale_fill_manual(values=c('#146175', '#c44f4f')) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 79), breaks = NULL) + labs(x = '', y = ''))

(Out02 <- ggplot(data = data.frame(x = c(rep(8.7, 20), rep(8.3, 20), seedNorm(500, 6, 0.5, 4)), y = c(rep('o', 40), rep('i', 500))), 
                 aes(x = x, fill = y)) + 
    geom_histogram(binwidth = 0.2, colour = "white") + 
    scale_fill_manual(values=c('#146175', '#c44f4f')) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 90), breaks = NULL) + labs(x = '', y = ''))

(Out20 <- ggplot(data = data.frame(x = c(rep(4, 20), rep(3.6, 20), seedNorm(500, 6, 0.5, 4)), y = c(rep('o', 40), rep('i', 500))), 
                 aes(x = x, fill = y)) + 
    geom_histogram(binwidth = 0.2, colour = "white") + 
    scale_fill_manual(values=c('#146175', '#c44f4f')) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 90), breaks = NULL) + labs(x = '', y = ''))

png(filename = './www/Out10.png', width = 400, height = 200)
print(Out10)
dev.off()

png(filename = './www/Out01.png', width = 400, height = 200)
print(Out01)
dev.off()

png(filename = './www/Out11.png', width = 400, height = 200)
print(Out11)
dev.off()

png(filename = './www/Out20.png', width = 400, height = 200)
print(Out20)
dev.off()

png(filename = './www/Out02.png', width = 400, height = 200)
print(Out02)
dev.off()
}
