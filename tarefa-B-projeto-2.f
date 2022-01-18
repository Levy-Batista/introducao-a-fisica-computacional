      program tarefaB

c     M -> número de andarilhos, N -> número de passos
c     p -> probabilidade de dar um passo à direita 
      parameter (M = 1000000, N = 1000, p = 1.e0/2)
      integer*8 l, ipos
c     array que guarda a quantidade de andarilhos em cada posição
      dimension ipos(-N/2:N/2)
      open(1, file='1saida-B-11212550')
      xmed = 0.e0
      xqmed = 0.e0
      do k = -N/2, N/2
        ipos(k) = 0
      end do

      do i = 1, M
        ix = 0
c       a seed muda a cada iteração para gerar um conjunto diferente
        valor = rand(i)
        do j = 1, N
c         verifica para qual direção será o passo do andarilho
          if (valor.lt.p) then
            ix = ix + 1
          else
            ix = ix - 1
          end if
          valor = rand()
        end do
c       atualiza a quantidade de andarilhos na posição que o atual parou
        ipos(ix/2) = ipos(ix/2) + 1
      end do

      do l = -N/2, N/2
c       calcula <x> e <x²> 
        xmed = xmed + 2*l*ipos(l)
        xqmed = xqmed + (2*l)**2*ipos(l)
        write(1,*)2*l, ipos(l)
      end do

      write(*,*)'O valor medio de x eh:', xmed/M,
     +          'e o de x² eh:', xqmed/M 

      close(1)
      end      
