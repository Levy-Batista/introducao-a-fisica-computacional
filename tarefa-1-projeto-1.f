      program tarefa1

      parameter (pi = acos(-1.0))
      print*,'Digite os raios interno e externo do torus, nessa ordem:'
      read(*,*) r1, r2
      area = pi**2*(r2**2 - r1**2)
      volume = pi**2*(r2 - r1)**2*(r1 + r2)/4
      write(*,*)'A área lateral é:', area, 'e o volume é:', volume

      end
