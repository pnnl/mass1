# Cross-section Management Tools

This directory contains several scripts for the manipulation of
*general* cross section data.  

## Requirements


* [Perl](https://www.perl.org)

  Standard Perl 5 can be used for all the Perl scripts, except one.

* [Perl Data Language (PDL)](http://pdl.perl.org) with
  [PGPLOT](http://search.cpan.org/~kgb/PGPLOT-2.18/PGPLOT.pm)
  
  Specifically, the`PDL::Graphics::PGPLOT::Window` package is required
  for `CHARIMAview.pl`.

* [Awk](https://en.wikipedia.org/wiki/AWK) or
  (gawk)[https://www.gnu.org/software/gawk] 
  
  The scripts to convert between MASS1 and CHARIMA format are in
  [Awk](https://en.wikipedia.org/wiki/AWK). 

## Cross Section Format

These scripts manipulate cross sections in one or more files.  Many
cross sections can be contained in a file.  

These scripts understand the cross section format used by the CHARIMA
code.  MASS1's format is slightly different.  Only the general cross
section type is understood (type 50). MASS1's prismatic section types
are not handled.

To convert from MASS1 to CHARIMA format:

```
awk -f section-unfix.awk < mass1_section.txt > charima_section.txt
```

To convert from CHARIMA to MASS1 format:

```
awk -f section-fix.awk < charima_section.txt > mass1_section.txt
```

## Utilities

These utilities are provided as-is. There is little or no
documentation (except the code itself).  Some are still used often by the
authors; others are quite old and in probably in need of maintenance.  

* `CHARIMAarea.pl`
* `CHARIMAextract.pl`
* `CHARIMAfill.pl`
* `CHARIMAinsert.pl`
* `CHARIMAnotch.pl`
* `CHARIMArenum.pl`
* `CHARIMAthalweg.pl`
* `CHARIMAview.pl`
* `HEC2area.pl`
* `HEC2CHARIMA.pl`
* `HECRASCHARIMA.pl`
* `README.md`
* `section-fix.awk`
* `section-unfix.awk`
* `XSection.pm`
* `XSECTprops.pl`
