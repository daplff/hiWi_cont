

#define PASTEHELP(s) s
#define concat(A,B) PASTEHELP(A)B
#define concatdef(B,inputname) concat(fortran_get_variable_,B)(inputname)

#define PUTIN_QUOTES(A) "A"

#define CREATE_GETVAR_FUNCTION(varname, cvartype, cfunname) \
      subroutine concatdef(varname,c_input) & NEWL\
      bind(c, name = PUTIN_QUOTES(cfunname) ) NEWL\
      cvartype c_input NEWL\
      real varname = 5 NEWL\
      c_input = varname NEWL\
      end subroutine

#define CREATE_GETVAR(varname, cvartype, cfunname) \
      subroutine  NEWL\
      cvartype c_input NEWL\
      real varname NEWL\
      varname = 5 NEWL\
      c_input = varname NEWL\
      end subroutine




      module name
          implicit none

      contains

        subroutine get_fortran_variable_xp (ctype_variable) &
        bind (c, name = "FORTRAN_GETTER_xp")
        use global_2d

        <c_double>::VALUE ctype_variable

        ctype_variable = xp

        end subroutine
        contains

        subroutine get_fortran_variable_xp (ctype_variable, length)
     1   bind (c, name = "FORTRAN_GETTER_xp")
        use global_2d

        real(c_double), dimension(*)::ctype_variable
        integer (c_int), value::length

        int i

        do i=1,length
            ctype_variable(i) = xp(i)
        enddo

        end subroutine

     1   subroutine set_fortran_variable_xp (ctype_variable, length)
        bind (c, name = "FORTRAN_SETTER_xp")
        use global_2d

        real(c_double), dimension(*)::ctype_variable
        integer (c_int), value::length
        int i

        do i=1,length
            xp(i) = ctype_variable(i)
        enddo

        end subroutine











      end module name
