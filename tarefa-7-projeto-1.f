      program tarefa7
      
      dimension y(6)
      real mat_den(6, 6), mat_num(6,6)
      write(*,*)'Qual a dimensão da matriz quadrada A?(4, 5 ou 6)'
      read(*,*)i    

c     verifica quais arquivos de matrizes A e Y serão usados   
      if (i.eq.4) then
      	open(1, file='1entrada-7-11212550')
      	open(2, file='2entrada-7-11212550')
      elseif (i.eq.5) then
      	open(1, file='3entrada-7-11212550')
      	open(2, file='4entrada-7-11212550')
      else 
        open(1, file='5entrada-7-11212550')
      	open(2, file='6entrada-7-11212550')
      endif

      do l = 1, i
      	read(1,*) (mat_den(l, m), m = 1, i)
        read(2,*) y(l)
      end do

      mat_num = mat_den
      write(*,*)'O vetor coluna X que resolve o sistema é:'

c     o determinante da matriz do denominador é calculado
      call calc_det(mat_den, i, det_den)

c     cada matriz do numerador é montada e tem seu determinante calculado
      do icont = 1, i
c       a cada iteração a matriz do numerador é alterada
      	if (icont.eq.1) then
          do jcont = 1, i
            mat_num(jcont, icont) = y(jcont)
          end do
        else
      	  do kcont = 1, i
      	    mat_num(kcont, icont - 1) = mat_den(kcont, icont - 1)
      	    mat_num(kcont, icont) = y(kcont)
          end do
      	end if
c       o determinante da matriz do numerador é calculado
      	call calc_det(mat_num, i, det_num)
        write(*,*) det_num/det_den
      end do

      close(1)
      close(2)

      END

      subroutine calc_det(real_m, n, det)
      	dimension ivet(n+1)
      	dimension real_m(6, 6)
      	det = 0
      	prod = 1

c       verifica qual arquivo de permutações será utilizado
        if (n.eq.4) then 
      	  open(12, file='1saida-5-11212550')
      	  do kcont = 1, 24
      	    read(12,*) (ivet(lcont), lcont = 1, n+1)
c           prod calcula os termos do somatório que dá o det em termo das permutações
            do mcont = 1, n
              prod = prod*real_m(mcont, ivet(mcont))
            end do
c           ivet(n+1) armazena a paridade da permutação utilizada
            prod = prod*ivet(n+1)
            det = det + prod
c           antes de utilizar uma nova permutação, prod retorna ao seu valor inicial 
            prod = 1
          end do
          close(12)
        elseif (n.eq.5) then
c         mesmo procedimento de n = 4 
      	  open(13, file='2saida-5-11212550')
          do kcont = 1, 120
      	    read(13,*) (ivet(lcont), lcont = 1, n+1)
            do mcont = 1, n
              prod = prod*real_m(mcont, ivet(mcont))
            end do
            prod = prod*ivet(n+1)
            det = det + prod 
            prod = 1
          end do
          close(13)
        else 
c         mesmo procedimento de n = 4
          open(14, file='3saida-5-11212550')
          do kcont = 1, 720
      	    read(14,*) (ivet(lcont), lcont = 1, n+1)
            do mcont = 1, n
              prod = prod*real_m(mcont, ivet(mcont))
            end do
            prod = prod*ivet(n+1)
            det = det + prod 
            prod = 1
          end do
          close(14)
      	endif

      return 
      end
