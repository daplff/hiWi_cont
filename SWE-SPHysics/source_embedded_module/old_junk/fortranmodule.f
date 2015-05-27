c
c fortranfn.f
c
c Created on: May 20, 2015
c     Author: wannerbe
c
        module fortranmodule
        use iso_c_binding

        integer (c_int), bind(c, name = "LALALA") :: changedvar2

        contains
      subroutine fortranfn2(changedvar) bind(c, name = "sb")
        use global_2D

        integer (c_int)changedvar

        np = 5;
        changedvar = 55;
        xp(1)= 1;

        end subroutine
        end module
