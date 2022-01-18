      program tarefaC

        implicit real*8 (a-h,o-z)
        pi = dacos(-1.d0)
c       ângulos e velocidades iniciais para os dois osciladores
        ang1 = pi/18
        ang2 = ang1 + 0.001d0
        vel1 = 0.d0
        vel2 = 0.d0
c       intervalo utilizado na discretização do tempo
        deltat = 0.03d0
c       tempo total de movimento
        T = 0.d0
c       termo de amortecimento
        q = 0.5d0
c       amplitude da força externa
        F0 = 1.2d0
c       frequência da força externa
        omegaF = 2.d0/3

        open(1, file='saida-C-11212550')
        write(1, 100) 't(s)', '|', 'theta(rad)'
 10     if (T.le.200*pi) then
          write(1, 200)T, '|', ang2-ang1
c         algoritmo de Euler-Cromer para os dois osciladores
          vel1 = vel1 - dsin(ang1)*deltat - q*vel1*deltat+
     +           F0*dsin(omegaF*T)*deltat
          ang1 = ang1 + vel1*deltat
          vel2 = vel2 - dsin(ang2)*deltat - q*vel2*deltat+
     +           F0*dsin(omegaF*T)*deltat
          ang2 = ang2 + vel2*deltat
          T = T + deltat
          goto 10
        end if
        write(1, 200)T, '|', ang2-ang1
        close(1)

 100    format(A6, A, A10)
 200    format(F6.2, A, F11.7)

        end
