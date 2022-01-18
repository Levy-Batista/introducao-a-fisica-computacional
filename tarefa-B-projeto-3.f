      program tarefaB

      real*8 valor, y, exato, trapezio, simpson, boole, pi
c     definindo pi como uma variável comum a ser usada nas demais funções
      common /cte/pi
      open(1, file='entrada-B-11212550')
      open(2, file='saida-B-11212550')

      pi = dacos(-1.d0)
c     guarda o valor exato da integral na variável valor
      valor = exato()
      write(2, 700)"N|", "h=(b-a)/N|", "Trapézio|", "Simpson|", 
     +             "Boole|"
      do l = 1, 10
c       lê os valores de N diretamente do arquivo de entrada
        read(1,*)y
c       o módulo das diferenças para cada método vai sendo escrito na saída
        write(2, 900) int(y), "|", 1/y, "|", abs(trapezio(y) - valor),
     +                "|", abs(simpson(y) - valor), "|",
     +                abs(boole(y) - valor), "|"
      end do
      
c     escreve o valor exato da integral no arquivo de saída
      write(2, 800)"exato = ", valor

 700  format(A5, A12, A13, A12, A12)
 800  format(A, F13.11)
 900  format(I4, A, D11.4, A, D11.4, A, D11.4, A, D11.4, A)

      close(1)
      close(2)

      end

c     função que calcula a integral pelo método do trapézio
      real*8 function trapezio (x)
        real*8 h, x, pi
        common /cte/pi
        h = 1/x
        trapezio = 0.d0
      	do i = 1, int(x)-1
          trapezio = trapezio + h*dexp(i/(2*x))*dsin(pi*i/x)
        end do
        return
      end

c     função que calcula a integral pelo método de Simpson
      real*8 function simpson (x)
        real*8 h, x, pi
        common /cte/pi
        h = 1/x
        simpson = 0.d0
        do j = 1, int(x)/2
          simpson = simpson + (2*h/3)*dexp(j/x)*dsin(pi*2*j/x) +
     +              (4*h/3)*dexp((2*j-1)/(2*x))*dsin(pi*(2*j-1)/x)
        end do
        return
      end

c     função que calcula a integral pelo método de Boole
      real*8 function boole (x)
        real*8 h, x, pi
        common /cte/pi
        h = 1/x
        boole = 0.d0
        do k = 1, int(x)/4
          boole = boole + (64*h/45)*dexp((4*k-3)/(2*x))*
     +            dsin(pi*(4*k-3)/x) + (8*h/15)*dexp((4*k-2)/(2*x))*
     +            dsin(pi*(4*k-2)/x) + (64*h/45)*dexp((4*k-1)/(2*x))*
     +            dsin(pi*(4*k-1)/x) + (28*h/45)*dexp(2*k/x)*
     +            dsin(pi*4*k/x)
        end do
        return 
      end 
      
c     função que calcula a integral de forma mais exata
      real*8 function exato ()
        real*8 pi
        common /cte/pi
        exato = 4*pi*(1 + dexp(0.5d0))/(4*pi**2 + 1)
        return
      end
