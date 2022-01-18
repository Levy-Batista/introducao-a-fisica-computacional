      program tarefaC

c     no caso bidimensional, p assume o valor 1/4
      parameter (M = 10000, N = 10, p = 1.0/4)
      integer*8 k, l, tmpx, tmpy
c     agora, a quantidade em cada posição é armazenada numa matriz 
      dimension ipos(-N:N, -N:N)
      open(1, file='1saida-C-11212550')
      xmed = 0
      xqmed = 0
      ymed = 0
      yqmed = 0
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
c         verifica para qual das 4 direções foi o passo dado
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
c       adiciona 1 na posição onde o anadarilho parou
        ipos(ix, iy) = ipos(ix, iy) + 1
        write(1,*)ix, iy
      end do

      do icont = -N, N
c       armazena quantos andarilhos estão na reta x = icont 
        tmpx = 0
c       armazena quantos andarilhos estão na reta y = icont
        tmpy = 0
        do jcont = -N, N
          tmpx = tmpx + ipos(icont, jcont)
          tmpy = tmpy + ipos(jcont, icont)
        end do
c       calcula <x>, <y>, <x²> e <y²>
        xmed = xmed + icont*tmpx
        xqmed = xqmed + icont**2*tmpx
        ymed = ymed + icont*tmpy
        yqmed = yqmed + icont**2*tmpy
      end do

      write(*,*)'O valor medio de x eh:', xmed/M,
     +          'e o de x² eh:', xqmed/M
      write(*,*)'O valor medio de y eh:', ymed/M,
     +          'e o de y² eh:', yqmed/M 

      close(1)
      end      
