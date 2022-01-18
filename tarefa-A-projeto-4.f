      program tarefaA

      implicit real*8 (a-h,o-z)
c     pi, comp e g serão utilizadas nas subrotinas implementadas
      common /cte/pi, comp, g
      pi = dacos(-1.d0)
      comp = 9.8d0
      g = 9.8d0

c     subrotina que calcula theta e E pelo método de Euler
      theta = pi/12
      omega = 0.d0
      
      call euler(theta, omega)

c     subrotina que calcula theta e E pelo método de Euler-Cromer
      theta = pi/12
      omega = 0.d0
      
      call euler_cromer(theta, omega)

      end

      subroutine euler (ang, vel)
      
        implicit real*8 (a-h,o-z)
        common /cte/pi, comp, g
c       intervalo utilizado na discretização do tempo
        deltat = 0.01d0
c       tempo total de simulação
        T = 0.d0

        open(1, file='1saida-A-11212550')
        write(1, 100) 't(s)', '|', 'theta(rad)', '|', 'E(J)'
        E = (comp*vel)**2/2 + g*comp*(1.d0 - dcos(ang))
        write(1, 200) T, '|', ang, '|', E
 10     if (T.le.20*pi) then
          velaux = vel
c         itera os valores de theta e omega
          vel = vel - ang*deltat
          ang = ang + velaux*deltat
c         calcula o novo valor de energia do sistema
          E = (comp*vel)**2/2 + g*comp*(1.d0 - dcos(ang))
c         registra a passagem do tempo
          T = T + deltat
          write(1, 200) T, '|', ang, '|', E
          goto 10
        end if
        close(1)

 100    format(A5, A, A10, A, A4)
 200    format(F5.2, A, F10.5, A, F9.6)  

      end

      subroutine euler_cromer (ang, vel)
      
        implicit real*8 (a-h,o-z)
        common /cte/pi, comp, g
c       intervalo utilizado na discretização do tempo
        deltat = 0.01d0
c       tempo total de simulação
        T = 0.d0

        open(1, file='2saida-A-11212550')
        write(1, 100) 't(s)', '|', 'theta(rad)', '|', 'E(J)'
        E = (comp*vel)**2/2 + g*comp*(1.d0 - dcos(ang))
        write(1, 200) T, '|', ang, '|', E
 10     if (T.le.20*pi) then
c         itera os valores de theta e omega
          vel = vel - ang*deltat
          ang = ang + vel*deltat
c         calcula o novo valor de energia do sistema
          E = (comp*vel)**2/2 + g*comp*(1.d0 - dcos(ang))
c         registra a passagem do tempo          
          T = T + deltat
          write(1, 200) T, '|', ang, '|', E
          goto 10
        end if
        close(1)

 100    format(A5, A, A10, A, A4)
 200    format(F5.2, A, F10.5, A, F9.6)  

      end
