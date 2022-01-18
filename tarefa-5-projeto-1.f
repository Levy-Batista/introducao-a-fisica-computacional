      program tarefa5

      parameter (idmax = 4)
      dimension ient(idmax), isai(idmax+1)
      open(1, file='entrada-5-11212550')
      open(2, file='1saida-5-11212550')

c     o arquivo de entrada tem (idmax-1)! linhas
      do i = 1, 6
      	read(1,*) (ient(j), j = 1, idmax)
c       ipar armazena a paridade da nova permutação
        ipar = ient(idmax)
c       em cada loop,  uma permutação nova é formada a partir de uma linha da entrada
      	do m = idmax, 1, -1
c         é montada a parte anterior à inserção do novo termo 
          do k = 1, m-1
            isai(k) = ient(k)
          end do
c         novo termo inserido
          isai(m) = idmax
c         é montada a parte posterior à inserção do novo termo
          do l = m+1, idmax
            isai(l) = ient(l-1)
          end do
          isai(idmax+1) = ipar
          ipar = -ipar ! o sinal da próxima permutação é invertido
          write(2,*)(isai(n), n = 1, idmax+1)
        end do
      end do

      close(1)
      close(2)

      end
