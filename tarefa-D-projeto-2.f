      program tarefaD

c     até o cálculo da entropia de fato, o código é o mesmo da tarefa-C
      parameter (M = 10000, N = 10, p = 1.e0/4)
      integer*8 icont, jcont, k, l, ipos, ix, iy
      dimension ipos(-N:N, -N:N)

      s = 0.e0
      do k = -N, N
        do l = -N, N
          ipos(k, l) = 0
        end do
      end do

      do i = 1, M
        ix = 0
        iy = 0
        valor = rand(i)
        do j = 1, N
          if (valor.lt.p) then
            ix = ix + 1
          else if (valor.lt.2*p) then
            ix = ix - 1
          else if(valor.lt.3*p) then
            iy = iy + 1
          else
            iy = iy - 1
          end if
          valor = rand()
        end do
        ipos(ix, iy) = ipos(ix, iy) + 1
      end do

      do icont = -N, N
        do jcont = -N, N
c         verifica se a posição está vazia 
          if (ipos(icont, jcont).ne.0) then
c           caso não esteja, computa a contribuição dela na entropia 
            s = s - ipos(icont, jcont)*log(1.0*ipos(icont, jcont)/M)
          end if
        end do
      end do

      write(*,*)'O valor da entropia nessa configuracao eh:', s/M 

      end      
