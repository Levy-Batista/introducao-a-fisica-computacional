      program tarefaA

      implicit real*8 (a-h, o-z)

c     obtenção do delta t que permite órbita circular
      alpha = 4.d0*dacos(-1.d0)**2
c     condições iniciais (planeta Terra)
      vx0 = 0.d0
      vy0 = dsqrt(alpha)
      x = 1.d0
      y = 0.d0
c     varia-se o valor de deltat até encontrar o ideal
      deltat = 0.01d0
c     tempo de movimento
      T = 0.d0
      open(1, file='orbitas')
      open(2, file='1saida-A-11212550')
      open(3, file='2saida-A-11212550')
      write(1, 100)x, y      

c     primeiro passo do método de Verlet
      xaux = x
      yaux = y
      x = x + vx0*deltat
      y = y + vy0*deltat
      T = T + deltat
      write(1, 100)x, y

 10   if (T.le.100) then
c       distância do planeta ao Sol
        r = dsqrt(x**2 + y**2)
c       método de Verlet
        xaux2 = x
        yaux2 = y
        x = 2*x - xaux - alpha*x*deltat**2/r**3
        y = 2*y - yaux - alpha*y*deltat**2/r**3
        xaux = xaux2
        yaux = yaux2
        T = T + deltat
        write(1, 100)x, y
        goto 10
      end if
      close(1)

c     item a1
c     condições iniciais, verificando que velocidade corresponde à órbita
c     circular para cada planeta, com deltat usado anteriormente
      raio = 39.53d0
      vx0 = 0.d0
      vy0 = dsqrt(alpha/raio) - 0.001d0
      x = raio
      y = 0.d0
      T = 0.d0

c     aplicação do método de Verlet
      xaux = x
      yaux = y
      x = x + vx0*deltat
      y = y + vy0*deltat
      T = T + deltat
      write(2, 100)x, y

 20   if (T.le.300) then
        r = dsqrt(x**2 + y**2)
        xaux2 = x
        yaux2 = y
        x = 2*x - xaux - alpha*x*deltat**2/r**3
        y = 2*y - yaux - alpha*y*deltat**2/r**3
        xaux = xaux2
        yaux = yaux2
        T = T + deltat
        write(2, 100)x, y
        goto 20
      end if
      close(2)

c     item a2
c     condições inicias, agora variadas para obter-se os diferentes tipos
c     possíveis de órbitas
      vx0 = 0.d0
      vy0 = 8.d0
      x = 1.d0
      y = 0.d0
      T = 0.d0

c     novamente, aplicação do método de Verlet
      xaux = x
      yaux = y
      x = x + vx0*deltat
      y = y + vy0*deltat
      T = T + deltat
      write(3, 100)x, y

 30   if (T.le.100) then
        r = dsqrt(x**2 + y**2)
        xaux2 = x
        yaux2 = y
        x = 2*x - xaux - alpha*x*deltat**2/r**3
        y = 2*y - yaux - alpha*y*deltat**2/r**3
        xaux = xaux2
        yaux = yaux2
        T = T + deltat
        write(3, 100)x, y
        goto 30
      end if
      close(3)

 100  format(F7.2, F7.2)

      end
