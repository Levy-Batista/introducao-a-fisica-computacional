      program tarefa6

      parameter (idmax = 4)
      dimension ivet(idmax+1)
      real mat(idmax, idmax)
      det = 0
      prod = 1
      open(1, file='1entrada-6-11212550')
      open(2, file='2entrada-6-11212550')

c     a matriz que queremos calcular o det é lida de um arquivo de entrada
      do i = 1, idmax
        read(2,*)(mat(i,l), l = 1, idmax)
      end do

      do j = 1, 24
      	read(1,*) (ivet(k), k = 1, idmax+1)
c       prod calcula os termos do somatório que dá o det em termo das permutações
        do m = 1, idmax
          prod = prod*mat(m, ivet(m))
        end do
c       ivet(idmax+1) armazena a paridade da permutação utilizada
        prod = prod*ivet(idmax+1)
        det = det + prod
c       antes de utilizar uma nova permutação, prod retorna ao seu valor inicial 
        prod = 1
      end do

      write(*,*)'Determinante da matriz real', idmax, 'x', idmax, ':',
     +            det
      close(1)
      close(2)

      end
