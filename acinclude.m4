dnl @synopsis AC_PROG_PERL_VERSION(VERSION, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
dnl
dnl Makes sure that perl supports the version indicated. If true the
dnl shell commands in ACTION-IF-TRUE are executed. If not the shell
dnl commands in ACTION-IF-FALSE are run. Note if $PERL is not set (for
dnl example by running AC_CHECK_PROG or AC_PATH_PROG),
dnl AC_CHECK_PROG(PERL, perl, perl) will be run.
dnl
dnl Example:
dnl
dnl   AC_PROG_PERL_VERSION(5.6.0)
dnl
dnl This will check to make sure that the perl you have supports at
dnl least version 5.6.0.
dnl
dnl @category InstalledPackages
dnl @author Dean Povey <povey@wedgetail.com>
dnl @version 2002-09-25
dnl @license AllPermissive

AC_DEFUN([AC_PROG_PERL_VERSION],[dnl
# Make sure we have perl
if test -z "$PERL"; then
AC_CHECK_PROG(PERL,perl,perl)
fi

# Check if version of Perl is sufficient
ac_perl_version="$1"

if test "x$PERL" != "x"; then
  AC_MSG_CHECKING(for perl version greater than or equal to $ac_perl_version)
  # NB: It would be nice to log the error if there is one, but we cannot rely
  # on autoconf internals
  $PERL -e "use $ac_perl_version;" > /dev/null 2>&1
  if test $? -ne 0; then
    AC_MSG_RESULT(no);
    $3
  else
    AC_MSG_RESULT(ok);
    $2
  fi
else
  AC_MSG_WARN(could not find perl)
fi
])dnl


dnl @synopsis AC_PROG_PERL_MODULES([MODULES], [ACTION-IF-TRUE], [ACTION-IF-FALSE])
dnl
dnl Checks to see if the the given perl modules are available. If true
dnl the shell commands in ACTION-IF-TRUE are executed. If not the shell
dnl commands in ACTION-IF-FALSE are run. Note if $PERL is not set (for
dnl example by calling AC_CHECK_PROG, or AC_PATH_PROG),
dnl AC_CHECK_PROG(PERL, perl, perl) will be run.
dnl
dnl Example:
dnl
dnl   AC_CHECK_PERL_MODULES(Text::Wrap Net::LDAP, ,
dnl                         AC_MSG_WARN(Need some Perl modules)
dnl
dnl @category InstalledPackages
dnl @author Dean Povey <povey@wedgetail.com>
dnl @version 2002-09-25
dnl @license AllPermissive

AC_DEFUN([AC_PROG_PERL_MODULES],[dnl
ac_perl_modules="$1"
# Make sure we have perl
if test -z "$PERL"; then
AC_CHECK_PROG(PERL,perl,perl)
fi

if test "x$PERL" != x; then
  ac_perl_modules_failed=0
  for ac_perl_module in $ac_perl_modules; do
    AC_MSG_CHECKING(for perl module $ac_perl_module)

    # Would be nice to log result here, but can't rely on autoconf internals
    $PERL "-M$ac_perl_module" -e exit > /dev/null 2>&1
    if test $? -ne 0; then
      AC_MSG_RESULT(no);
      ac_perl_modules_failed=1
   else
      AC_MSG_RESULT(ok);
    fi
  done

  # Run optional shell commands
  if test "$ac_perl_modules_failed" = 0; then
    :
    $2
  else
    :
    $3
  fi
else
  AC_MSG_WARN(could not find perl)
fi])dnl


dnl @synopsis AX_SYS_PERLSHARPBANG
dnl
dnl Determine how the perl interpreter is located by the OS kernel and
dnl make substitution variable PERL_SHEBANG available. Does
dnl AC_PATH_PROG to find the path to perl. As a side-effect, that sets
dnl PERLINTERP and makes it available as a substitution variable.
dnl
dnl Note: The macro allows for the possibility (expected to be seldom
dnl used) of an explicit user override (the "user" being the operator
dnl executing the final 'configure' script, in this context) by making
dnl the option argument like:
dnl
dnl    --with-perl-shebang='#! /my/funky/perlpath' # OR
dnl    --with-perl-shebang='/my/funky/perlpath'  # we just throw away the #! anyway
dnl                                              # bec it must be absent in Makefile
dnl
dnl Rationale: The are various ways of starting an interpreter on
dnl different *nix-like systems. Many use the simple
dnl
dnl   #!/usr/bin/perl
dnl
dnl but it could be instead
dnl
dnl   #!/usr/local/bin/perl
dnl
dnl and there is even the possibility that the user wants
dnl
dnl   #!/usr/bin/env perl
dnl
dnl to find whichever perl comes first in the current $PATH. This is
dnl preferred by some of us because we run multiple perl installations
dnl on the same box. Adjusting our $PATH then allows us to set
dnl precedence over other perls, even whatever the "house" version is.
dnl
dnl Users on very non-unix systems like MS Windows do not have a kernel
dnl that does this kind of thing from the first line of script files,
dnl but instead the perl on their machine is started and merely notices
dnl whatever comes after the interpreter path on this first line of the
dnl script (options like "-w").
dnl
dnl Acknowledgement: this macro was inspired in part by
dnl <ac_prog_perl_version> authored by Dean Povey, see the AC-Archive
dnl (ac-archive.sf.net).
dnl
dnl @category InstalledPackages
dnl @author Soren Andersen <somian *AT* pobox |DOT| com>
dnl @version 2004-02-21
dnl @license AllPermissive

AC_DEFUN([AX_SYS_PERLSHARPBANG],[dnl

   AC_PATH_PROG(PERLINTERP,perl,perl)
   ac_cv_path_perlinterp="$PERLINTERP"
   _sHpB='#!'

   AC_ARG_WITH(perl-shebang,
                AC_HELP_STRING([--with-perl-shebang],
           [override what perl thinks is the way for the kernel to start it (seldom needed)]dnl
		           ),
		[opt_perl_shebang="$withval"]dnl
		            ,dnl
		[opt_perl_shebang="not_set"]dnl
    )dnl

   AC_CACHE_CHECK([whether explicit instead of detected sharpbang is to be used],
		   ax_cv_opt_perl_shebang,
		  [ case "$opt_perl_shebang" in
		      not_set  ) ax_cv_opt_perl_shebang=''
		               ;;
		         *     )
	ax_cv_opt_perl_shebang=`echo "$opt_perl_shebang" | sed -e's|^#!\s*\(.*\)$|\1|'`
		    esac
		  ]dnl
    )dnl

   if test "A$ax_cv_opt_perl_shebang" != "A"
     then
       ac_cv_sys_kernshrpbang_perl="$ax_cv_opt_perl_shebang"
       PERL_SHEBANG="$ac_cv_sys_kernshrpbang_perl"
       AC_SUBST(PERL_SHEBANG)dnl
       AC_MSG_NOTICE([OK - PERL_SHEBANG is $_sHpB$PERL_SHEBANG.])

# Automatic detection of sharpbang formula starts here
     else dnl

   _somian_shbangperl=`$PERLINTERP -V:startperl`
   negclass="[[^']]"; dnl
# must leave this comment:  m4 will remove the outer brackets for us, heheh
   AC_CACHE_CHECK([for kernel sharpbang invocation to start perl],
	          ac_cv_sys_kernshrpbang_perl,
	[_somian_kspb_perl=`echo "$_somian_shbangperl" | sed -ne"s|.*='\($negclass*\)';$|\1|p"`
	if test "x$_somian_kspb_perl" == x
	  then _somian_ksbp_warn_empty='durnit'
	  else
	  case "A$_somian_kspb_perl" in
	         A#!*perl* )
           ac_cv_sys_kernshrpbang_perl=`echo "$_somian_kspb_perl" | sed -e's|#!\(.*\)$|\1|'`
			;;
	             A*    )  _somian_ksbp_warn_defau='trouble'
		              ac_cv_sys_kernshrpbang_perl="$PERLINTERP"
	  esac
	fi
])dnl Done with testing sharpbang

# The above prints Checking ... result message to user.
   PERL_SHEBANG="$ac_cv_sys_kernshrpbang_perl"
   AC_SUBST(PERL_SHEBANG)
    if test A${_somian_ksbp_warn_empty+set} == Aset
      then   AC_MSG_WARN([dnl
In last check, doing $PERLINTERP -V:startperl yielded empty result! That should not happen.])
    fi
# Inform user after printing result value
    if test A${_somian_ksbp_warn_defau+set} == Aset
      then AC_MSG_NOTICE([Maybe Not good -])
	   AC_MSG_WARN([dnl
In last check perl's Config query did not work so we bunted: $_sHpB$PERLINTERP])
      else AC_MSG_NOTICE([OK Good result - ])
	   AC_MSG_NOTICE([dnl
In last check we got a proper-looking answer from perl's Config: $_somian_shbangperl])
dnl Done with user info messages
    fi
dnl Outer loop checked for user override term here
  fi dnl

])dnl EOMACRO DEF
