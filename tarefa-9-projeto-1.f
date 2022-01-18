      program tarefa9

      parameter (pi = acos(-1.0))
      Vd = pi
      tmp = 1.0
      open(1,file='dimensões-esferas')

      do id = 2, 20
      	Vd = Vd*tmp
c       escreve a dimensão e o respectivo volume da esfera de raio unitário
      	write(1,*)id, Vd
      	tmp = (sqrt(pi)*id/(id+1))*gamma(id/2.0)/gamma((id+1)/2.0)
      end do

      close(1)
      
      end
