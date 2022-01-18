      program tarefaA

c     o valor da seed foi definido como parâmetro e pode ser alterado abaixo
      parameter (m = 1000000, iseed = 5)
      write(*,*)'Digite o expoente n:'
      read(*,*)n
      valor = rand(iseed)

      xmedia = 0
      do i = 1, m
c       o valor de n é informado no terminal pelo usuário
        xmedia = xmedia + valor**n
        valor = rand()
      end do

      write(*,*)'A media de x elevado a', n, 'eh', xmedia/m

      end
