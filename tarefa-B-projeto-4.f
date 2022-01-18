      program tarefaB

      implicit real*8 (a-h,o-z)
c     pi foi definido dessa forma pois será usado nas subrotinas
      common /cte/pi
      pi = dacos(-1.d0)
      
c     as três subrotinas são chamadas em sequência
      call B1()
      theta = pi/18
      omega = 0.d0
      call B3(theta, omega)
      theta = pi/18
      omega = 0.d0
      call B4(theta, omega)

      end

      subroutine B1 ()
      
        implicit real*8 (a-h,o-z)
        common /cte/pi
c       intervalo utilizado na discretização do tempo
        deltat = 0.01d0
c       ângulo inicial, que vai sendo alterado até pi/3
        ang = pi/36
c       epsilon utilizado no cálculo da integral elíptica
        e = 0.001d0
c       número de divisões do intervalo de integração por Boole
        n = 384
        open(1,file='1saida-B-11212550')
        write(1, 100) 'theta0', '|', 'T/Euler-Cromer', '|', 
     +                'T/integral', '|', 'T/fórmula'

        do i = 1, 12
c         método de Euler-Cromer para encontrar o período
          angaux = ang
          vel = 0.d0
          k = 0
          T = 0.d0
 10       if (T.le.3*pi) then
            angaux2 = angaux
            vel = vel - dsin(angaux)*deltat
            angaux = angaux + vel*deltat
            T = T + deltat
c           marca a primeira vez que o ângulo mda de sinal
            if (angaux2*angaux.lt.0.and.k.eq.1) then
              Tf = T
              k = 2
            end if
c           marca a segunda vez que o ângulo muda de sinal
            if (angaux2*angaux.lt.0.and.k.eq.0) then
              Ti = T
              k = 1
            end if
            goto 10
          end if
c         cálculo da integral elíptica
          valor = 4*dsqrt(2*e/dsin(ang))
          thint = 0
          h = (ang - e)/n
c         cálculo da parte numérica pelo método de Boole
          do j = 0, (n/4 - 1)
            thint=thint + (2*h/45)*(7/dsqrt(dcos(4*j*h)-dcos(ang))+
     +            32/dsqrt(dcos((4*j+1)*h)-dcos(ang))+ 12/dsqrt(dcos
     +            ((4*j+2)*h)-dcos(ang))+ 32/dsqrt(dcos((4*j+3)*h)-
     +            dcos(ang))+ 7/dsqrt(dcos((4*j+4)*h)-dcos(ang)))
          end do
          thint = 2*dsqrt(2.d0)*thint
c         soma das partes analítica e numérica
          valor = valor + thint
          write(1, 200) ang*180/pi, '|', 2*(Tf - Ti), '|', valor,
     +                  '|', 2*pi*(1 + ang**2/16)
c         incremento do ângulo inicial
          ang = ang + pi/36
        end do
        close(1)  

 100    format(A8, A, A14, A, A10, A, A10)
 200    format(F8.1, A, F14.2, A, F10.2, A, F9.2)
      end

      subroutine B3 (ang, vel)
      
        implicit real*8 (a-h,o-z)
        common /cte/pi
c       intervalo utilizado na discretização do tempo
        deltat = 0.01d0
c       tempo total de movimento
        T = 0.d0
c       coeficiente de amortecimento
        q = 0.5d0

        open(1, file='2saida-B-11212550')
        write(1, 100) 't(s)', '|', 'theta(rad)'
 10     if (T.le.10*pi) then
c         aplicação do método de Euler-Cromer
          write(1, 200)T, '|', ang
          vel = vel - dsin(ang)*deltat - q*vel*deltat
          ang = ang + vel*deltat
          T = T + deltat
          goto 10
        end if
        write(1, 200)T, '|', ang
        close(1)

 100    format(A9, A, A10)
 200    format(F9.2, A, F8.5)  

      end

      subroutine B4 (ang, vel)
      
        implicit real*8 (a-h,o-z)
        common /cte/pi
c       intervalo utilizado na discretização do tempo
        deltat = 0.03d0
c       tempo total de movimento
        T = 0.d0
c       coeficiente de amortecimento
        q = 0.5d0
c       amplitude da força externa
        F0 = 1.2d0
c       frequência da força externa
        omegaF = 2.d0/3

        open(1, file='3saida-B-11212550')
        open(2, file='4saida-B-11212550')
        write(1, 100) 't(s)', '|', 'theta(rad)'
        write(2, 100) 't(s)', '|', 'vel(rad/s)'
 10     if (T.le.20*pi) then
c         aplicação do método de Euler-Cromer
          write(1, 200)T, '|', ang
          write(2, 200)T, '|', vel
          vel = vel - dsin(ang)*deltat - q*vel*deltat
     +          + F0*dsin(omegaF*T)*deltat
          ang = ang + vel*deltat
          T = T + deltat
          goto 10
        end if
        write(1, 200)T, '|', ang
        write(2, 200)T, '|', vel
        close(1)
        close(2)

 100    format(A9, A, A10)
 200    format(F9.2, A, F8.5)  

      end
