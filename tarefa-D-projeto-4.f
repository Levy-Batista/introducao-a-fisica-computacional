      program tarefaD

        implicit real*8 (a-h,o-z)
        pi = dacos(-1.d0)
c       angulo inicial que o pêndulo faz com a vertical
        ang = pi/12
c       velocidade angular inicial
        vel = 0.d0
c       intervalo utilizado na discretização do tempo
        deltat = 0.03d0
c       tempo total de movimento
        T = 0.d0
c       coeficiente de amortecimento
        q = 0.5d0
c       amplitude de força externa
        F0 = 1.2d0
c       frequência da força externa
        omegaF = 2.d0/3

        open(1, file='saida-D-11212550')
        write(1, 100) 'theta(rad)', '|', 'omega(rad/s)'
 10     if (T.le.200*pi) then
c         algoritmo de Euler-Cromer
          vel = vel - dsin(ang)*deltat - q*vel*deltat+
     +           F0*dsin(omegaF*T)*deltat
          ang = ang + vel*deltat
          T = T + deltat
c         mantém o ângulo sempre entre -pi e +pi
          if (abs(ang).gt.pi) then
            angaux = ang*180/pi
            if (angaux.gt.0) then
              angaux = mod((angaux+180.d0), 360.d0) - 180.d0
            else
              angaux = mod((angaux+180.d0), 360.d0) + 180.d0
            end if
            angaux = angaux*pi/180
            write(1, 200)angaux, '|', vel
          else
            write(1, 200)ang, '|', vel
          end if
          goto 10
        end if
        close(1)

 100    format(A10, A, A12)
 200    format(F10.7, A, F11.7)

        end
