      program tarefaA

      real*8 f1, f2, f3, d1, d2, d3, ds3, df2, dp2, ds5, dss5, dtas5, x
     +       , h
      open(1, file='entrada-A-11212550')
      open(2, file='saida-A-11212550')
      
      x = 0.25d0
      f1 = d1(x)
      f2 = d2(x)
      f3 = d3(x)
      
      write(2, 700)"h|", "ds3|", "df2|", "dp2|", "ds5|", "dss5|", 
     +             "dtas5|"
      do i = 1, 14
c       lê os valores de h que estão em um arquivo de entrada
        read(1,*)h
c       o módulo das diferenças são escritos na tabela no arquivo de saída
        write(2, 900)h, "|", abs(ds3(h, x)-f1), "|", abs(df2(h, x)-f1),
     +               "|", abs(dp2(h, x)-f1), "|", abs(ds5(h, x)-f1),
     +               "|", abs(dss5(h, x)-f2), "|", abs(dtas5(h, x)-f3), 
     +               "|"
      end do

c     os valores exatos das derivadas primeira, segunda e terceira também são escritos
      write(2,*)"exatos:"
      write(2,800)"f'(0,25) = ", f1
      write(2,800)"f''(0,25) = ", f2
      write(2,800)"f'''(0,25) = ", f3
      
      close(1)
      close(2)
 
c     as diferentes formatações utilizadas na saída
 700  format(A11, A8, A8, A8, A8, A8, A13)
 800  format(A, F14.11)
 900  format(F10.8, A, F7.4, A, F7.4, A, F7.4, A, F7.4, A, F7.4, A,
     +       F12.4, A)
      
      end

c     função que calcula a derivada simétrica de 3 pontos
      real*8 function ds3 (h, x)
        real*8 h, x
      	ds3 = (dcosh(4*(x+h))*dsin((x+h)/4) - dcosh(4*(x-h))
     +         *dsin((x-h)/4))/(2*h)
        return
      end

c     função que calcula a derivada para frente de 2 pontos
      real*8 function df2 (h, x)
        real*8 h, x
        df2 = (dcosh(4*(x+h))*dsin((x+h)/4) - dcosh(4*x)*dsin(x/4))/h
        return
      end

c     função que calcula a derivada para trás de 2 pontos
      real*8 function dp2 (h, x)
        real*8 h, x
        dp2 = (dcosh(4*x)*dsin(x/4) - dcosh(4*(x-h))*dsin((x-h)/4))/h
        return
      end

c     função que calcula a derivada simétrica de 5 pontos
      real*8 function ds5 (h, x)
        real*8 h, x
        ds5 = (dcosh(4*(x-2*h))*dsin((x-2*h)/4) - 8*dcosh(4*(x-h))
     +        *dsin((x-h)/4) + 8*dcosh(4*(x+h))*dsin((x+h)/4)
     +        - dcosh(4*(x+2*h))*dsin((x+2*h)/4))/(12*h) 
        return
      end

c     função que calcula a derivada segunda simétrica de 3 pontos        
      real*8 function dss5 (h, x)
        real*8 h, x
        dss5 = (-dcosh(4*(x-2*h))*dsin((x-2*h)/4) + 16*dcosh(4*(x-h))
     +         *dsin((x-h)/4) + 16*dcosh(4*(x+h))*dsin((x+h)/4) 
     +         - dcosh(4*(x+2*h))*dsin((x+2*h)/4) - 30*dcosh(4*x)
     +         *dsin(x/4))/(12*h**2) 
        return
      end

c     função que calcula a derivada terceira anti-simétrica de 5 pontos
      real*8 function dtas5 (h, x)
        real*8 h, x
        dtas5 = (-dcosh(4*(x-2*h))*dsin((x-2*h)/4) + 2*dcosh(4*(x-h))
     +          *dsin((x-h)/4) - 2*dcosh(4*(x+h))*dsin((x+h)/4)
     +          + dcosh(4*(x+2*h))*dsin((x+2*h)/4))/(2*h**3) 
        return
      end

c     função que calcula o valor exato da derivada primeira
      real*8 function d1 (x)
        real*8 x
      	d1 = 4*dsinh(4*x)*dsin(x/4) + (1/4)*dcosh(4*x)*dcos(x/4)
        return
      end

c     função que calcula o valor exato da derivada segunda
      real*8 function d2 (x)
        real*8 x
      	d2 = (255/16)*dcosh(4*x)*dsin(x/4) + 2*dsinh(4*x)*dcos(x/4)
        return
      end

c     função que calcula o valor exato da derivada terceira
      real*8 function d3 (x)
        real*8 x
      	d3 = (253/4)*dsinh(4*x)*dsin(x/4) + (767/64)*dcosh(4*x)
     +       *dcos(x/4) 
        return
      end
