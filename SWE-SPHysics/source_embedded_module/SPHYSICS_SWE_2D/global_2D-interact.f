
#define GET_VARIABLE(type,name)







      subroutine copy_to_fortran(xpos, nparts)
      use global_2D
	  
	  real xpos(npar)
	  integer nparts
	  
	  integer i
	  
	  do i=1,nparts
		xp(i)=xpos(i)
	  enddo
	  
	end
		
	  
	  
	  