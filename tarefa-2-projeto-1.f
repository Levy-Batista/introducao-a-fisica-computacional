      program tarefa2
 
      real v1(3), v2(3), v3(3), v4(3)
      print*,'Digite as coordenadas do vetor v1:'
      read(*,*) v1(1), v1(2), v1(3)
      print*,'Digite as coordenadas do vetor v2:'
      read(*,*) v2(1), v2(2), v2(3)
      print*,'Digite as coordenadas do vetor v3:'
      read(*,*) v3(1), v3(2), v3(3)
c     v4 é implementado como o vetor v3 - v2 
      do i=1, 3
      	v4(i)=v3(i)-v2(i)
      end do
      
      area=2*(areaface(v1(1), v1(2), v1(3), v2(1), v2(2), v2(3))+
     +     areaface(v1(1), v1(2), v1(3), v4(1), v4(2), v4(3))+
     +     areaface(v2(1), v2(2), v2(3), v4(1), v4(2), v4(3)))
      volume=prodmisto(v4(1), v4(2), v4(3), v1(1), v1(2), v1(3),
     +       v2(1), v2(2), v2(3)) 
      
      write(*,*)'A área lateral do prisma formado é:', area,
     +       'e o volume é:', volume

      end
c     função que retorna a área de uma face do prisma
      real function areaface(x1, y1, z1, x2, y2, z2)

      areaface=sqrt((y1*z2-z1*y2)**2+(z1*x2-x1*z2)**2+ 
     +         (x1*y2-y1*x2)**2)
      return

      end
c     função que retorna o volume do prisma a partir do produto misto de 3 vetores 
      real function prodmisto(x4, y4, z4, x1, y1, z1, x2, y2, z2)

      prodmisto=x4*(y1*z2-z1*y2)+y4*(z1*x2-x1*z2)+z4*(x1*y2-y1*x2)
      return

      end
      
