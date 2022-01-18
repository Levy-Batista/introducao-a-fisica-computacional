      program tarefa3

      parameter (idmax = 20)
      real lista(idmax)
      rmax = 0.0
      open(unit=1, file='entrada-3-11212550')
      open(unit=2, file='saida-3-11212550')
      write(*,*)'Quantos números você quer ordenar? menos que', idmax
      read(*,*)n
c     vetor lista recebe todos os números que estão no arquivo de entrada
      do i = 1, idmax
      	read(1,*)lista(i)
        if (lista(i).gt.rmax) rmax = lista(i) !rmax recebe o maior número do vetor
      end do
      
      
      do k = 1, n
      	tmp = lista(1)
      	do j = 2, idmax
c         tmp recebe o atual menor número e ipos armazena a posição dele
      	  if (tmp.gt.lista(j)) then
            tmp = lista(j)
            ipos = j
          endif
        end do
c       caso o menor número seja o primeiro da lista, ipos recebe 1
        if (tmp.eq.lista(1)) ipos = 1
        write(2,*) tmp
c       as posições ordenadas tem o valor alterado para não serem contadas de novo
        lista(ipos) = rmax+1
      end do

      write(2,*)'Você quis ordenar', n, 'números'
      close(1)
      close(2)

      END
